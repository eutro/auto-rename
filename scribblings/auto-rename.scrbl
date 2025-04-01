#lang scribble/manual

@(require auto-rename/language/british-english
          auto-rename/language/polyglot)
@(require (for-label auto-rename
                     auto-rename/language/british-english
                     auto-rename/language/polyglot
                     racket/base
                     racket/sequence
                     racket/contract
                     racket/provide-transform
                     racket/require-transform
                     (except-in
                      (combine-in
                       (for-british racket)
                       (en-français racket racket/generator)
                       (auf-deutsch racket)
                       (magyarul racket racket/generator))
                      import export rest tag))
          scribble/example)

@(define make-evaluator
   (make-eval-factory (list 'racket 'auto-rename)))

@title{Auto Rename Transformers}
@author{eutro}

@defmodule[auto-rename]

A package for programmatic @racket[require] and @racket[provide] filters and renamings.

@(examples
  #:eval (make-evaluator)
  #:once
  #:label "Example:"
  
  (eval:no-prompt
   (module fancy-lib racket
     (provide (all-defined-out))
     (struct fancy (x y z) #:transparent))
   code:blank)

  (require (transform-in
            'fancy-lib
            #:transform (transform/replace "fancy" "not-so-fancy")))
  (not-so-fancy 1 2 3)
  struct:not-so-fancy)

@deftogether[[(defform (auto-rename-in require-spec ...+ #:transform transform ...))
              (defform (auto-rename-out provide-spec ...+ #:transform transform ...))
              (defform (auto-rename-in/out spec ...+ #:transform transform ...)
                #:contracts ([transform filter/c]))]]{
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key "require transformer"]{Require}
and @tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key "provide transformer"]{provide}
transformers which apply the given @racket[transform]s to all the imports and exports from the @racket[spec]s.
}

@deftogether[[(defform (transform-in require-spec ...+ #:transform transforms ...))
              (defform (transform-out provide-spec ...+ #:transform transforms ...))
              (defform (transform-in/out spec ...+ #:transform transforms ...))]]{
Aliases for @racket[auto-rename-in], @racket[auto-rename-out] and @racket[auto-rename-in/out].
}

@section{Transformers}

@defmodule[auto-rename/filters]

These bindings are provided by both @racket[auto-rename] and @racket[auto-rename/filters].
@racket[auto-rename] additionally provides these bindings with @racket[tx/] replacing @racket[transformer/].

@defproc[(transform/filter [accept? (-> string? any/c)]) filter/c]{
Include only imports/exports for which @racket[accept?] produces a true value.
}
@defform[(transform/when x accept?-expr)]{
Equivalent to @racket[(transform/filter (λ (x) accept?-expr))].
}

@defproc[(transform/remove [reject? (-> string? any/c)]) filter/c]{
Exclude any imports/exports for which @racket[reject?] produces a true value.
}
@defform[(transform/unless x reject?-expr)]{
Equivalent to @racket[(transform/remove (λ (x) reject?-expr))].
}

@defproc[(transform/map [renamer (-> string? string?)]) filter/c]{
Change the name of any imports/exports to the result of applying @racket[renamer] to the original name.
}
@defform[(transform/for x name-expr)]{
Equivalent to @racket[(transform/map (λ (x) name-expr))].
}

@defproc[(transform/append-map [renamer (-> string? (sequence/c string?))]) filter/c]{
Change the name of any imports/exports to the results of applying @racket[renamer] to the original name.
That is, the binding is imported/exported under every name that @racket[renamer] returns, or no names at all
if the returned sequence is empty.
}
@defform[(transform/for* x names-expr)]{
Equivalent to @racket[(transform/append-map (λ (x) names-expr))].
}

@defproc[(transform/matches [pattern (or/c string? regexp?)]) filter/c]{
Include only imports/exports whose name matches @racket[pattern].
Equivalent to @racket[(transform/when name (regexp-match? pattern name))].
}

@defproc[(transform/replace [pattern (or/c string? regexp?)]
                            [replacement (or/c string? (string? string? ... . -> . string?))])
         filter/c]{
Replace all occurrences of @racket[pattern] in import/export names with @racket[replacement],
as if by @racket[regexp-replace*].

Equivalent to @racket[(transform/for name (regexp-replace* pattern name replacement))].
}

@section{Miscellaneous Definitions}

@deftogether[(@defthing[filter/c
                        contract?
                        #:value (-> (reducing-function/c im/export/c A)
                                    (reducing-function/c im/export/c A))]
              @defproc[(reducing-function/c [input/c contract?] [accumulator/c contract?])
                       contract?
                       #:value (case->
                                (input/c accumulator/c . -> . accumulator/c)
                                (accumulator/c . -> . accumulator/c))])]{
A @deftech{filter} is a composable function which transforms streams of
imports/exports. @racketmodname[auto-rename] uses filters to modify
the sequence of imports/exports that @racket[auto-rename-in/out] produce.
Typically one would use the @racket[transform/xyz] functions available in
@racketmodname[auto-rename/filters] to construct these, rather than
writing them by hand.

A @deftech{reducing function} is a type of procedure which accumulates inputs
into an accumulator. @racketmodname[auto-rename] uses reducing functions
to transform a sequence of imports/exports into the list of imports/exports 
produced by @racket[auto-rename-in/out]. Typically, one does not see reducing
functions unless they are writing a @tech{filter} by hand.

Specifically @racket[(reducing-function/c in/c acc/c)] is a contract
for a procedure @racket[rf] suitable for the expression
@racket[(rf (foldl rf acc ins))], where @racket[acc] and @racket[ins] conform to 
@racket[acc/c] and @racket[(listof in/c)] respectively, and a @tech{filter}
is a function which maps an import/export @tech{reducing function} to a new import/export
reducing function, which may rename, remove, duplicate, or otherwise modify the 
stream of imports/exports that the original reducing function receives, but 
may not observe or modify the accumulator.

@(examples
  #:eval (make-evaluator)
  #:once #:no-prompt
  #:label "Some illustrative, but not terribly useful, examples:"

  (code:line
   (code:comment "A transformer which drops all imports/exports.")
   (define (transform/none rf)
     (case-lambda
       [(im/ex acc) acc]
       [(acc) acc])))

  (code:line
   code:blank
   (code:comment "A transformer which drops imports randomly.")
   (define (transform/drop-randomly rf)
     (case-lambda
       [(im/ex acc)
        (if (zero? (random 2))
            acc
            (rf im/ex acc))]
       [(acc) acc])))
  )
}

@defthing[im/export/c contract? #:value (or/c import? export?)]{
A import or an export.
}

@defproc[(im/export-name [im/ex im/export/c]) string?]{
Get the name of an import/export.
}

@defproc[(im/export-map-name [im/ex im/export/c] [proc (-> string? string?)]) im/export/c]{
Return a copy of @racket[im/ex] with its name transformed by @racket[proc].
}

@defproc[(transform/compose [transform filter/c] ...) filter/c]{
Compose the given @racket[transform]s.
The effects of the transformations are performed left to right.
}

@section{Internationalisation Support}

Bindings detailed in this section are experimental, and may not be
up to the same quality as those exported by @racketmodname[auto-rename].
I make no backwards-compatibility guarantees.

@subsection{British and American English}

@defmodule[auto-rename/language/british-english]

This module provides require/provide transformers for
importing and exporting bindings with British English spelling.

@(examples
  #:eval (make-evaluator)
  #:once
  (require auto-rename/language/british-english
           (for-british racket))
  (normalise-arity 1)
  (rationalise 1/4 1/10)
  (eval:error
   (parameterise ([error-print-width 5])
     (car (expt 10 1024))))
  (string-normalise-spaces "  foo bar  baz \r\n\t"))

@defform[(for-british spec ...+)]{
A @tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key "require transformer"]{require}
and @tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key "provide transformer"]{provide}
transformer which renames identifiers using American English spellings to use British English spellings instead.
}

@defform[(for-american spec ...+)]{
A @tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key "require transformer"]{require}
and @tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key "provide transformer"]{provide}
transformer which renames identifiers using British English spellings to use American English spellings instead.
}

@subsection{Other languages}

@defmodule[auto-rename/language/polyglot]

This module provides require/provide transformers
for importing and exporting bindings in languages like French, German
and Hungarian. Due to the automated and word-based nature of these
translations they may not be very good, but they are predictable.
Sometimes.

@(examples
  #:eval (make-evaluator)
  #:once

  (require auto-rename/language/polyglot)
  (require (en-français racket racket/generator))
  (se-dresser inconvénients nul (liste 1 2 3))
  (|jeter-un-coup-d'œil-octets| 4 0 (ouvrir-saisir-chaîne "abcd"))

  (exiger (auf-deutsch racket))
  (sogar? (länge (nachteile 0 (nachteile 1 null))))
  (lassen-werte ([(rohr-ein rohr-aus) (machen-rohr)])
    (schreiben-saite (saite-oben "efgh\nijkl") rohr-aus)
    (schließen-ausgabe-hafen rohr-aus)
    (beginnen0
      (hafen->linien rohr-ein)
      (schließen-eingang-hafen rohr-ein)))

  (erfordern (magyarul racket racket/generator))
  (meghatároz funkció
    (ügy-lambda
     [() (hozam 1) (hozam 2) (hozam 3)]
     [(x y) (hányados/maradék x y)]))
  (funkció 100 7)

  (definieren l\'ensemble (veränderlich-satz))
  (für ([w (dans-générateur
             (dynamique-vent
              (λ () (kijelző "Be "))
              funkció
              (λ () (anzeige "Aus "))))]
        [x (in-reichweite 5)]
        [y (-ben-természetes 3)]
        [z (dans-faire-du-vélo (dans-valeur 2))])
    (satz-hinzufügen!
     l\'ensemble
     (dans-le-monde-ior
      (arithmetik-schicht w x)
      (rationalisieren (/ y) (/ z)))))
  l\'ensemble)

@deftogether[[(defform (en-français spec ...+))
              (defform (auf-deutsch spec ...+))
              (defform (magyarul spec ...+))]]{
@tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key "require transformer"]{Require}
and @tech[#:doc '(lib "scribblings/reference/reference.scrbl") #:key "provide transformer"]{provide}
transformers which renames identifiers to be in the corresponding language.

This works by splitting each identifier into tokens of (English) alphabet
characters, and applying pre-defined translations (if any) to each token
individually.
}
