#lang racket
(require racket/date
         racket/runtime-path
         (prefix-in 19: srfi/19)
         (planet neil/csv)
         (planet jaymccarthy/mongodb))

;; When you run:
(require "model.rkt")
;; When you test parsing the file:
#;(begin
  (define (call-with-model t) (t))
  (define-syntax (kw-quote stx)
    (syntax-case stx ()
      [(_ e)
       (if (keyword? (syntax->datum #'e))
           #''e
           #'e)]))
  (define-syntax-rule (make-file e ...)
    (pretty-print `(make-file ,(kw-quote e) ...)))
  (define-syntax-rule (make-applicant e ...)
    (pretty-print `(make-applicant ,(kw-quote e) ...))))

(define parse%
  (match-lambda
   [(regexp #rx"^(.*)%$" (list _ ns))
    (string->number* ns)]
   [""
    bson-null]))
(define (string->number* s)
  (define v (string->number s))
  (or v
      bson-null))
(define (string-empty? s)
  (zero? (string-length s)))
(define (bad-string? s)
  (or (string-empty? s)
      (string=? "NA" s)))
(define (19:string->date* str fmt)
  (19:string->date str fmt))
(define (gre->time x)
  (with-handlers ([exn:fail? (lambda (x) bson-null)])
                 (19:date->time-utc
                  (19:string->date* (string-append x "/01") "~Y/~m/~d"))))

(define-runtime-path csv-path "/home/gradadmin/winter-2012-init-data/applicants.csv")
(define-runtime-path pdf-path "/home/gradadmin/winter-2012-init-data/applicants")

(call-with-model
 (λ ()
    (with-input-from-file csv-path
      (lambda ()
        (csv-for-each
         (match-lambda
          [(list-rest "Student" _)
           (void)]
          [(and x (list Student App LOI Trans LOR GREDate GRE-Verb GRE-Quan GRE-Anal TOEFL-Kind TOEFL-Date TOEFL-Read TOEFL-Listen TOEFL-Speaking TOEFL-Writing PriorSchool Degree CumGPA MajGPA LDS-Status ApplyForFin DegreeSought Citizenship Prfrd-Advsr Research-Area))

           ;; Spreadsheets are column oriented, so naturally squeeze multiple columns into one!
           (match-define (regexp #rx"^([^,]+), (.+)"
                                 (list _ LastName FirstName))
                         Student)
           (define parse-GRE
             (match-lambda
              [(regexp #rx"^(.+)/(.+)$"
                       (list _ Score %))
               (values Score %)]
              [(regexp #rx"^([0-9]+)$"
                       (list _ Score))
               (values Score "")]))
           (define-values (GRE_Q GRE_QP) (parse-GRE GRE-Quan))
           (define-values (GRE_V GRE_VP) (parse-GRE GRE-Verb))
           (define-values (GRE_A GRE_AP) (parse-GRE GRE-Anal))
           (define TOEFL-Date-to-parse (string-append TOEFL-Date "/01"))

           (printf "Adding ~a ~a~n" FirstName LastName)

           ;; TODO: Add the check for an apostrophe to the regexp on Lastname such as O'Neill
           (define (get-pdf-type type)
             (define this-pdf
               (build-path pdf-path (format "~a~a~a.pdf" (regexp-replace* #rx" " LastName "")  (regexp-replace* #rx" " FirstName "") type)))
             (cond
              [(file-exists? this-pdf)
               (define this-pdf-bytes
                 (file->bytes this-pdf))
               (mongo-dict-id
                (make-file #:uploaded (19:current-time)
                           #:bytes this-pdf-bytes))]
              [else
               (eprintf "Didn't find PDF: ~a\n" this-pdf)
               bson-null]))

           (make-applicant
            #:first-name FirstName
            #:last-name LastName
            #:lds? (match LDS-Status
                          [(or "Y" "TRUE") #t]
                          [(or "N" "FALSE") #f]
                          [else
                           (eprintf "Invalid LDS-Status: ~e\n" else)
                           bson-null])
            #:major-gpa (string->number* MajGPA)
            #:prior-school PriorSchool
            #:citizenship Citizenship
            #:cumulative-gpa (string->number* CumGPA)
            #:degree-sought
            (match DegreeSought
                   ["PhD" 'phd]
                   ["MS" 'ms]
                   [else
                    (eprintf "Invalid DegreeSought: ~e\n" else)
                    bson-null])
            #:degree Degree
            #:financial-aid? (match ApplyForFin
                                    [(or "Yes" "Y") #t]
                                    [(or "No"  "N") #f]
                                    [else
                                     (eprintf "Invalid ApplyForFin: ~e\n" else)
                                     bson-null])
            #:gre-analytic-percentile (parse% GRE_AP)
            #:gre-analytic-score (string->number* GRE_A)
            #:gre-quant-percentile (parse% GRE_QP)
            #:gre-quant-score (string->number* GRE_Q)
            #:gre-verbal-percentile (parse% GRE_VP)
            #:gre-verbal-score (string->number* GRE_V)
            #:gre-date (gre->time GREDate)
            #:toefl
            (if (ormap bad-string?
                       (list TOEFL-Kind TOEFL-Date TOEFL-Read TOEFL-Listen TOEFL-Speaking TOEFL-Writing))
                bson-null
                (list (cons 'kind (string->symbol TOEFL-Kind))
                      (cons 'date (19:date->time-utc
                                   (19:string->date* TOEFL-Date-to-parse "~m/~Y/~d")))
                      (cons 'read (string->number TOEFL-Read))
                      (cons 'listen (string->number TOEFL-Listen))
                      (cons 'speak (string->number TOEFL-Speaking))
                      (cons 'write (string->number TOEFL-Writing))))

            #:pdf-application
            (get-pdf-type "App")
            #:pdf-letters
            (get-pdf-type "Letters")
            #:pdf-transcript
            (get-pdf-type "Transcript")

            #:comments (vector)
            #:tags (vector)
            #:decisions (vector)
            )])
         (current-input-port))))))
