#lang racket/base

(require (for-syntax racket/base racket/string)
         "../main.rkt")

(provide en-français
         auf-deutsch
         magyarul)

(begin-for-syntax
  (require racket/runtime-path)
  (define-logger polyglot)

  (define-runtime-path table-path "stdlib-words.tsv")
  (define tsv-data
    (call-with-input-file* table-path
      #:mode 'text
      (λ (ic)
        (for/hash ([line (in-lines ic)])
          (define vals (string-split line "\t"))
          (values (car vals)
                  (apply vector-immutable (cdr vals)))))))

  (define ((map-words lang-code column) name)
    (cond
      [(string-prefix? name "#%") name]
      [else
       (define translated
         (regexp-replace*
          #rx"[a-zA-Z]+" name
          (λ (word)
            (define translated (hash-ref tsv-data word #f))
            (cond
              [translated
               (vector-ref translated column)]
              [else word]))))
       (log-polyglot-debug "~a -[~a]> ~a" name lang-code translated)
       translated])))

(define-syntax en-français
  (make-auto-rename-transformer
   (tx/map (map-words 'fr 0))
   (tx/disambiguate)))

(define-syntax auf-deutsch
  (make-auto-rename-transformer
   (tx/map (map-words 'de 1))
   (tx/disambiguate)))

(define-syntax magyarul
  (make-auto-rename-transformer
   (tx/map (map-words 'hu 2))
   (tx/disambiguate)))
