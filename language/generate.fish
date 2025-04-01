#!/usr/bin/env fish

: '
  ;; Run these in a REPL
  (require auto-rename)
  (require (transform-in                                                                                                                
            racket #:tx 
            (lambda (rf)
              (define coll (mutable-set))
              (case-lambda               
                [(im/ex acc)
                 (for ([name (in-list (regexp-match* #rx"[A-Za-z]+" (im/export-name im/ex)))])
                   (set-add! coll name))                                                      
                 (rf im/ex acc)]        
                [(acc)
                 (with-output-to-file "stdlib-words.tsv"
                   (lambda ()
                     (displayln (string-join (sort (set->list coll) string<?) "\n"))))
                 acc]))))
'

set LANGS fr de hu

function atr_transl
    for lang in $LANGS
        awk '{print $1}' stdlib-words.tsv \
            | parallel --bar -N 5 -k -- trans -brief en:$lang \
            | string lower \
            | sed 's/ /-/g' \
            > stdlib-words.$lang.tsv
    end
end

function atr_collate
    awk '{print $1}' stdlib-words.tsv > stdlib-words.en.tsv
    paste -d '\t' stdlib-words.en.tsv stdlib-words.$LANGS.tsv > stdlib-words.tsv
    rm stdlib-words.en.tsv
end
