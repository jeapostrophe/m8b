#lang scheme
(require (prefix-in 19: srfi/19)
         (planet jaymccarthy/mongodb)
         (for-syntax unstable/syntax))

(define mongo-server (create-mongo))
(define root-db
  (make-mongo-db mongo-server "m8b"))
(current-mongo-db root-db)

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
   [degree-sought]
   [toefl]
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

(define-syntax (id-list stx)
  (syntax-case stx ()
    [(_ base suf ...)
     (quasisyntax/loc stx
       (list #,@(syntax-map (lambda (i)
                              (format-id #'base "~a~a" #'base i))
                            #'(suf ...))))]))

(define (applicant-complete? a)
  (empty? 
   (ormap (compose bson-null? (lambda (f) (f a)))
          (id-list applicant-
                   citizenship lds? financial-aid? 
                   gre-date 
                   gre-verbal-score gre-verbal-percentile
                   gre-quant-score gre-quant-percentile
                   gre-analytic-score gre-analytic-percentile
                   prior-school cumulative-gpa major-gpa degree degree-sought toefl
                   pdf-application pdf-letters pdf-transcript))))

; XXX [toefl-okay? (symbols 'waived 'cleared 'not-cleared)]

(define (applicants)
  (mongo-dict-query "applicants" empty))

(define-mongo-struct
  faculty "faculty"
  ([name #:required]))
(define (faculty)
  (mongo-dict-query "faculty" empty))

(define-mongo-struct
  file "files"
  ([uploaded #:required]
   [bytes #:required]))

(provide (all-defined-out))