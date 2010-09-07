#lang racket
(require "model.rkt"
         racket/date
         racket/runtime-path
         (prefix-in 19: srfi/19)
         (planet neil/csv)
         (planet jaymccarthy/mongodb))

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

(define-runtime-path csv-path "admissions-winter-2011.csv")
(define-runtime-path example-pdf "example.pdf")

(define example-pdf-bytes
  (file->bytes example-pdf))
(define example-pdf-obj
  (make-file #:uploaded (19:current-time)
             #:bytes example-pdf-bytes))

(with-input-from-file csv-path
  (lambda ()
    (csv-for-each 
     (match-lambda
       [(list DV DZ EM ER JM Rec TempAdvisor LetterPrinted IntentToEnroll
              LastName FirstName Citizenship LDS-Status ApplyForFin
              RawGRE GREDate GRE_V GRE_VP GRE_Q GRE_QP GRE_A GRE_AP
              PriorSchool CumGPA MajGPA Degree DegreeSought SemesterYear
              Area DesiredAdvisor TOEFL-Cleared TOEFL-Kind TOEFL-Date TOEFL-Overall
              TOEFL-Read TOEFL-Listen TOEFL-Speaking TOEFL-Writing)
        (printf "Adding ~a ~a~n" FirstName LastName)
        (make-applicant 
         #:first-name FirstName
         #:last-name LastName
         #:lds? (match LDS-Status
                  ["TRUE" #t]
                  ["FALSE" #f])
         #:major-gpa (string->number* MajGPA)
         #:prior-school PriorSchool
         #:citizenship Citizenship
         #:cumulative-gpa (string->number* CumGPA)
         #:degree-sought
         (match DegreeSought
           ["PhD" 'phd]
           ["MS" 'ms])
         #:degree Degree
         #:financial-aid? (match ApplyForFin
                            [(or "Yes" "Y") #t]
                            [(or "No"  "N") #f])
         #:gre-analytic-percentile (parse% GRE_AP)
         #:gre-analytic-score (string->number* GRE_A)
         #:gre-quant-percentile (parse% GRE_QP)
         #:gre-quant-score (string->number* GRE_Q)
         #:gre-verbal-percentile (parse% GRE_VP)
         #:gre-verbal-score (string->number* GRE_V)
         #:gre-date 
         (with-handlers ([exn:fail? (lambda (x) bson-null)])
           (19:date->time-utc
            (19:string->date (string-append GREDate "-01") "~b-~y-~d")))
         #:toefl 
         (if (ormap string-empty? (list TOEFL-Kind TOEFL-Date TOEFL-Read TOEFL-Listen TOEFL-Speaking TOEFL-Writing))
             bson-null
             (list (cons 'kind (string->symbol TOEFL-Kind))
                   (cons 'date (19:date->time-utc
                                (19:string->date (string-append TOEFL-Date "-01") "~b-~y-~d")))
                   (cons 'read (string->number TOEFL-Read))
                   (cons 'listen (string->number TOEFL-Listen))
                   (cons 'speak (string->number TOEFL-Speaking))
                   (cons 'write (string->number TOEFL-Writing))))
         
         #:pdf-application 
         bson-null
         #;(if (zero? (random 10))
             bson-null
             (mongo-dict-id example-pdf-obj))
         #:pdf-letters
         bson-null
         #;(if (zero? (random 10))
             bson-null
             (mongo-dict-id example-pdf-obj))
         #:pdf-transcript
         bson-null
         #;(if (zero? (random 10))
             bson-null
             (mongo-dict-id example-pdf-obj))
         
         #:comments (vector)
         #:tags (vector)
         #:decisions (vector)
         )])
     (current-input-port))))

(for ([n (in-list
          '("ADMIN"
            "Cory Barker" "Bill Barrett" "Robert Burton" "Mark Clement"
            "Parris Egbert" "David Embley" "J. Kelly Flanagan" "Christophe Giraud-Carrier"
            "Michal A. Goodrich" "Michael Jones" "Charles Knutson" "Tony Martinez"
            "Jay McCarthy" "Eric Mercer" "Bryan Morse" "Dennis Ng"
            "Dan Olsen" "Eric Ringger" "Ken Rodham" "Paul Roper"
            "Kent Seamons" "Tom Sederberg" "Kevin Seppi" "Quinn Snell"
            "Dan Ventura" "Sean Warnick" "Philip Windley" "Scott Woodfield"
            "Daniel Zappala"))])
  (make-faculty #:name n))

(for ([a (applicants)])
  (displayln (applicant-first-name a)))