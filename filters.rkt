#lang racket/base

(require racket/contract
         racket/provide-transform
         racket/require-transform
         racket/sequence

         syntax/parse/define
         (for-syntax racket/base))

(provide transform/filter transform/when
         transform/remove transform/unless
         transform/filter-raw
         transform/remove-raw
         transform/map transform/for
         transform/map-raw
         transform/append-map transform/for*
         transform/append-map-raw
         transform/matches
         transform/replace
         transform/disambiguate

         im/export/c
         im/export-name
         im/export-map-name
         filter/c
         reducing-function/c
         transform/compose)

(define im/export/c (or/c import? export?))

(define (reducing-function/c in/c acc/c)
  (rename-contract
   (case->
    (in/c acc/c . -> . acc/c)
    (acc/c . -> . acc/c))
   (list 'reducing-function/c
         (contract-name in/c)
         (contract-name acc/c))))

(define filter/c
  (rename-contract
   (parametric->/c
    (Acc)
    (-> (reducing-function/c im/export/c Acc)
        (reducing-function/c im/export/c Acc)))
   'filter/c))

(define/contract transform/compose
  (filter/c ... . -> . filter/c)
  compose1)

(define/contract ((transform/filter-raw accept?) rf)
  ((-> im/export/c any/c) . -> . filter/c)
  (case-lambda
   [(im/ex acc) (if (accept? im/ex) (rf im/ex acc) acc)]
   [(acc) acc]))

(define/contract (transform/remove-raw reject?)
  ((-> im/export/c any/c) . -> . filter/c)
  (transform/filter-raw (λ (it) (not (reject? it)))))

(define/contract (transform/filter accept?)
  ((-> string? any/c) . -> . filter/c)
  (transform/filter-raw (λ (it) (accept? (im/export-name it)))))

(define-syntax-parse-rule (transform/when name:id expr)
  ;; using a function contract gives better errors than just any/c on
  ;; expr does, in the case that expr returns multiple values
  #:with fun #'(λ (name) expr)
  #:declare fun (expr/c #'(-> string? any/c) #:name "when")
  (transform/filter fun.c))

(define/contract (transform/remove reject?)
  ((-> string? any/c) . -> . filter/c)
  (transform/filter-raw (λ (it) (not (reject? (im/export-name it))))))

(define-syntax-parse-rule (transform/unless name:id expr)
  #:with fun #'(λ (name) expr)
  #:declare fun (expr/c #'(-> string? any/c) #:name "unless")
  (transform/remove fun.c))

(define/contract ((transform/map-raw func) rf)
  ((-> im/export/c im/export/c) . -> . filter/c)
  (case-lambda
    [(im/ex acc) (rf (func im/ex) acc)]
    [(acc) acc]))

(define/contract (transform/map func)
  ((-> string? string?) . -> . filter/c)
  (transform/map-raw (λ (it) (im/export-map-name it func))))

(define-syntax-parse-rule (transform/for name:id expr)
  #:with fun #'(λ (name) expr)
  #:declare fun (expr/c #'(-> string? string?) #:name "for")
  (transform/map fun.c))

(define/contract ((transform/append-map-raw func) rf)
  ((-> im/export/c (sequence/c im/export/c)) . -> . filter/c)
  (case-lambda
    [(im/ex acc) (for/fold ([acc acc]) ([v (func im/ex)]) (rf v acc))]
    [(acc) acc]))

(define/contract (transform/append-map func)
  ((-> string? (sequence/c string?)) . -> . filter/c)
  (transform/append-map-raw
   (λ (im/ex)
     (for*/list ([name (func (im/export-name im/ex))])
       (im/export-map-name im/ex (λ (_prev) name))))))

(define-syntax-parse-rule (transform/for* name:id expr)
  #:with fun #'(λ (name) expr)
  #:declare fun (expr/c #'(-> string? (sequence/c string?)) #:name "for*")
  (transform/append-map fun.c))

(define/contract (transform/matches pat)
  ((or/c regexp? string?) . -> . filter/c)
  (transform/filter (λ (it) (regexp-match? pat it))))

(define/contract (transform/replace pat replacement)
  ((or/c regexp? string?) (or/c string? (unconstrained-domain-> string?)) . -> . filter/c)
  (transform/map (λ (it) (regexp-replace* pat it replacement))))

(define/contract ((transform/disambiguate [n->str (λ (orig i) (format "~a-~a" orig i))]) rf)
  (() ((-> string? natural-number/c string?)) . ->* . filter/c)
  (define seen (make-hasheq))
  (case-lambda
    [(im/ex acc)
     (let loop ([im/ex im/ex])
       (define sym-name (string->symbol (im/export-name im/ex)))
       (hash-update! seen sym-name add1 0)
       (define conflicts (hash-ref seen sym-name))
       (cond
         [(= 1 conflicts) (rf im/ex acc)]
         [else
          (define (nth-name orig-name) (n->str orig-name conflicts))
          (loop (im/export-map-name im/ex nth-name))]))]
    [(acc) acc]))

(define (im/export-name x)
  (cond
    [(string? x) x]
    [else
     (im/export-name
      (cond
        [(import? x) (import-local-id x)]
        [(export? x) (export-out-id x)]
        [(identifier? x) (syntax-e x)]
        [(symbol? x) (symbol->string x)]
        [else (raise-argument-error 'im/export-name "(or/c symbol? identifier? import? export?)" x)]))]))

(define (im/export-map-name x string-func)
  (cond
    [(string? x)
     (define transformed (string-func x))
     (if (equal? x transformed) x transformed)]
    [else
     (define-values (x-> ->x)
       (cond
         [(symbol? x) (values symbol->string string->symbol)]
         [(identifier? x) (values syntax-e (λ (it) (datum->syntax x it x)))]
         [(import? x) (values import-local-id (λ (it) (struct-copy import x [local-id it])))]
         [(export? x) (values export-out-id (λ (it) (struct-copy export x [out-id it])))]
         [else (raise-argument-error 'transform "(or/c symbol? identifier? import? export?)" x)]))
     (define unwrapped (x-> x))
     (define transformed (im/export-map-name unwrapped string-func))
     (if (eq? transformed unwrapped) x (->x transformed))]))
