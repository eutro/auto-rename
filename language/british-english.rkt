#lang racket/base

(require (for-syntax racket/base racket/string)
         "../main.rkt")
(provide for-british for-american)

(begin-for-syntax
  (require racket/runtime-path)

  ;; british_spellings.tsv was taken from
  ;; https://github.com/hyperreality/American-British-English-Translator/blob/f8806121e58530f601b84ebb7302afc81b8b1461/data/british_spellings.json
  (define-runtime-path british-spellings-path "british_spellings.tsv")

  (define tsv-data
    (call-with-input-file* british-spellings-path
      #:mode 'text
      (λ (ic)
        (for/list ([line (in-lines ic)])
          (apply cons (string-split line "\t"))))))

  (define (flip-pair x) (cons (cdr x) (car x)))
  (define british->american-map (make-immutable-hash tsv-data))
  (define american->british-map (make-immutable-hash (map flip-pair tsv-data)))

  ;; TODO: introduce rule-based matcher rather than just consulting a table,
  ;; to catch things like `dingor-frombilizer` -> `dingour-frombiliser`
  (define ((map-words table) name)
    (regexp-replace*
     #rx"[a-zA-Z]+" name
     (λ (word) (hash-ref table word word)))))

(define-syntax for-british
  (make-auto-rename-transformer
   (tx/map (map-words american->british-map))))
(define-syntax for-american
  (make-auto-rename-transformer
   (tx/map (map-words british->american-map))))

(module+ test
  (module american racket/base
    (provide (all-defined-out))
    (define-values (color-red
                    500-eons-ago
                    emergency-breathalyzer
                    galvanized-square-steel)
      (values 1 2 3 4)))

  (module british racket/base
    (provide (all-defined-out))
    (define-values (cosy-defenceless-cottage
                    fibre-glass-fishing-rod
                    watercolour-fertiliser)
      (values 'a 'b 'c)))

  (require (for-british 'american)
           (for-american 'british)
           rackunit)

  (check-equal?
   (list colour-red 500-aeons-ago emergency-breathalyser galvanised-square-steel)
   (list 1 2 3 4))
  (check-equal?
   (list cozy-defenseless-cottage fiber-glass-fishing-rod watercolor-fertilizer)
   (list 'a 'b 'c)))
