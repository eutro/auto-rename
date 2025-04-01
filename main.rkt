#lang racket/base

(require (for-syntax racket/base
                     "reqprov-transformer.rkt"
                     "filters.rkt")
         racket/require-syntax
         racket/provide-syntax)

(provide auto-rename-in/out auto-rename-in auto-rename-out
         transform-in/out   transform-in   transform-out

         (for-syntax (all-from-out "reqprov-transformer.rkt"
                                   "filters.rkt")
                     (auto-rename-out
                      (all-from-out "filters.rkt")
                      #:tx (transform/replace #rx"transform/" "tx/"))))

(define-syntax auto-rename-in/out (new-auto-rename-transformer values))
(define-require-syntax (auto-rename-in stx)
  (syntax-case stx [] [(_ . tail) #`(auto-rename-in/out #:derived #,stx . tail)]))
(define-provide-syntax (auto-rename-out stx)
  (syntax-case stx [] [(_ . tail) #`(auto-rename-in/out #:derived #,stx . tail)]))

(define-syntax transform-in/out (make-rename-transformer #'auto-rename-in/out))
(define-syntax transform-in (make-rename-transformer #'auto-rename-in))
(define-syntax transform-out (make-rename-transformer #'auto-rename-out))

(module+ test
  (define-syntax rust-in
    (make-auto-rename-transformer
     (transform/replace #rx"-" "🚀")
     (transform/replace #rx"\\?" "🤔")
     (transform/replace #rx"!" "💥")))

  (require (rust-in rackunit racket/base))

  (module mod racket/base
    (require (submod ".." "..") (for-syntax racket/base racket/string))
    (provide (transform-out abc #:tx (transform/map string-upcase))
             (transform-out ABC #:tx (transform/for x (string-downcase x)))
             (transform-out |def ghi| #:tx (transform/append-map string-split))
             (transform-out jKl #:tx (transform/for* it (list (string-downcase it) (string-upcase it))))
             (rename-out [pqr-1 pqr])
             (transform-out mno1 pqr #:tx (transform/filter (λ (it) (equal? it "mno1"))))
             (transform-out mno2 pqr #:tx (transform/when it (equal? it "mno2")))
             (transform-out mno3 pqr #:tx (transform/remove (λ (it) (equal? it "pqr"))))
             (transform-out mno4 pqr #:tx (transform/unless it (equal? it "pqr")))
             (transform-out stu1 stu2 stu3 pqr #:tx (transform/matches #rx"stu.")))
    (define-values   (abc ABC |def ghi| jKl mno1 mno2 mno3 mno4 pqr pqr-1 stu1 stu2 stu3)
      (apply values '(abc ABC d/g       jKl mno1 mno2 mno3 mno4 pqr pqr-1 stu1 stu2 stu3))))
  (require 'mod)

  (check🚀eq🤔 ABC 'abc)
  (check🚀eq🤔 abc 'ABC)
  (check🚀eq🤔 def 'd/g)
  (check🚀eq🤔 ghi 'd/g)
  (check🚀eq🤔 jkl 'jKl)
  (check🚀eq🤔 JKL 'jKl)
  (check🚀equal🤔 (list mno1 mno2 mno3 mno4 pqr)
                  '(    mno1 mno2 mno3 mno4 pqr-1))
  (check🚀equal🤔 (list stu1 stu2 stu3)
                  '(    stu1 stu2 stu3))

  (check🚀equal🤔
   (let ([x 5]) (set💥 x 8) x)
   8)

  (module english racket/base
    (require (for-syntax "reqprov-transformer.rkt"
                         "filters.rkt"
                         racket/base))
    (provide for-british)
    (define-syntax for-british
      (make-auto-rename-transformer
       (transform/replace #rx"([yi])ze" "\\1se")
       (transform/replace #rx"center" "centre")
       (transform/replace #rx"defense" "defence")
       (transform/replace #rx"or$" "our"))))

  (module american🇺🇸🦅 racket/base
    (provide (all-defined-out))
    (define-values (analyze center defense labor organize)
      (values 1 2 3 4 5)))
  (module british🇬🇧🫖 racket/base
    (require (submod ".." english) (submod ".." american🇺🇸🦅))
    (provide (for-british (all-from-out (submod ".." american🇺🇸🦅)))))

  (require 'british🇬🇧🫖)
  (check🚀not🚀exn (λ () (list analyse centre defence labour)))

  ;
  )
