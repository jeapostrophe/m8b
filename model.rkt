#lang racket
(require (prefix-in 19: srfi/19)
         (planet jaymccarthy/mongodb)
         (for-syntax unstable/syntax
                     syntax/stx
                     racket/syntax))

(define (call-with-model thunk)
  (define mongo-server #f)
  (define root-db #f)
  (dynamic-wind
      (λ ()
        (set! mongo-server (create-mongo))
        (if root-db
          (set-mongo-db-mongo! root-db mongo-server)
          (set! root-db (make-mongo-db mongo-server "m8b"))))
      (λ ()
        (parameterize ([current-mongo-db root-db])
          (thunk)))
      (λ ()
        (close-mongo! mongo-server))))

(define-mongo-struct
  applicant "applicants"
  ([last-name #:required]
   [first-name #:required]
   [citizenship]
   [lds?]
   [financial-aid?]
   [gre-date]
   [gre-verbal-score]
   [gre-verbal-percentile]
   [gre-quant-score]
   [gre-quant-percentile]
   [gre-analytic-score]
   [gre-analytic-percentile]
   [prior-school]
   [cumulative-gpa]
   [major-gpa]
   [degree]
   [degree-sought #:required]
   [toefl]
   [research-area]
   [advisor]
                                        ; Files
   [pdf-application] ; apple-script / button_delete_green
   [pdf-letters] ; document_blank / button_delete_blue
   [pdf-transcript] ; curriculum_vitae / button_delete_red
                                        ; Metadata
   [comments #:push #:required]
   [decisions #:push #:pull #:required]
   [tags #:set-add #:pull #:required]))

(define (null+ l r)
  (if (or (bson-null? l) (bson-null? r))
    bson-null
    (+ l r)))
(define (null< l r)
  (if (or (bson-null? l) (bson-null? r))
    #f
    (< l r)))
(define (null-time<? l r)
  (if (or (bson-null? l) (bson-null? r))
    #f
    (19:time<? l r)))

(define (applicant-raw-gre a)
  (null+ (applicant-gre-verbal-score a)
         (applicant-gre-quant-score a)))

(define (string->time x)
  (with-handlers ([exn:fail? (lambda (x) bson-null)])
    (19:date->time-utc
     (19:string->date (string-append x "-01") "~Y-~m-~d"))))
(define (time->string x)
  (19:date->string (19:time-utc->date x) "~Y-~m"))

(define (bson-default x def)
  (if (bson-null? x)
    def
    x))
(define (applicant-research-area* a)
  (bson-default (applicant-research-area a) ""))
(define (applicant-advisor* a)
  (bson-default (applicant-advisor a) ""))
(define (applicant-degree-sought* a)
  (bson-default (applicant-degree-sought a) 'missing))

(define-syntax (id-list stx)
  (syntax-case stx ()
    [(_ base suf ...)
     (quasisyntax/loc stx
       (list #,@(stx-map (lambda (i)
                           (format-id #'base "~a~a" #'base i))
                         #'(suf ...))))]))

(define (applicant-complete? a)
  (empty?
   (filter (compose bson-null? (lambda (f) (f a)))
           (id-list applicant-
                    citizenship lds? financial-aid?
                    gre-date
                    gre-verbal-score gre-verbal-percentile
                    gre-quant-score gre-quant-percentile
                    gre-analytic-score gre-analytic-percentile
                    prior-school cumulative-gpa degree degree-sought
                    pdf-application pdf-letters pdf-transcript))))

                                        ; XXX [toefl-okay? (symbols 'waived 'cleared 'not-cleared)]

(define (applicants)
  (mongo-dict-query "applicants" empty))

(define-mongo-struct
  faculty "faculty"
  ([name #:required]
   [netid #:required]
   [email #:required]))
(define (faculty)
  (mongo-dict-query "faculty" empty))

(define-mongo-struct
  file "files"
  ([uploaded #:required]
   [bytes #:required]))

(provide (all-defined-out))
