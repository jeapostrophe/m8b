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
(define-runtime-path pdf-path "admissions-winter-2011")

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
        
        (define (get-pdf-type type)
          (define this-pdf
            (build-path pdf-path (format "~a~a~a.pdf" LastName FirstName type)))
          (define this-pdf-bytes
            (file->bytes this-pdf))
          (mongo-dict-id 
           (make-file #:uploaded (19:current-time)
                      #:bytes this-pdf-bytes)))
        
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
         (get-pdf-type "App")
         #:pdf-letters
         (get-pdf-type "Letters")
         #:pdf-transcript
         (get-pdf-type "Transcript")
         
         #:comments (vector)
         #:tags (vector)
         #:decisions (vector)
         )])
     (current-input-port))))

(for ([n*netid*email (in-list
          '(("ADMIN" #f #f)
            ("COMMITTEE" #f #f)
            ("Cory Barker" #f "corybarker@byu.edu")
            ("Bill Barrett" #f "barrett@cs.byu.edu")
            ("Robert Burton" #f "rpburton@cs.byu.edu")
            ("Mark Clement" #f "clement@cs.byu.edu")
            ("Parris Egbert" #f "egbert@cs.byu.edu")
            ("David Embley" #f "embley@cs.byu.edu")
            ("J. Kelly Flanagan" #f "kelly_flanagan@byu.edu")
            ("Christophe Giraud-Carrier" #f "cgc@cs.byu.edu")
            ("Michael A. Goodrich" #f "mike@cs.byu.edu")
            ("Michael Jones" #f "jones@cs.byu.edu")
            ("Charles Knutson" #f "knutson@cs.byu.edu")
            ("Tony Martinez" #f "martinez@cs.byu.edu")
            ("Jay McCarthy" "jaymcc" "jay@cs.byu.edu")
            ("Eric Mercer" #f "egm@cs.byu.edu")
            ("Bryan Morse" #f "morse@cs.byu.edu")
            ("Dennis Ng" #f "ng@cs.byu.edu")
            ("Dan Olsen" #f "olsen@cs.byu.edu")
            ("Eric Ringger" #f "ringger@cs.byu.edu")
            ("Ken Rodham" #f "rodham@cs.byu.edu")
            ("Paul Roper" #f "proper@cs.byu.edu")
            ("Kent Seamons" #f "seamons@cs.byu.edu")
            ("Tom Sederberg" #f "tom@cs.byu.edu")
            ("Kevin Seppi" #f "kseppi@byu.edu")
            ("Quinn Snell" #f "snell@cs.byu.edu")
            ("Dan Ventura" #f "ventura@cs.byu.edu")
            ("Sean Warnick" #f "sean@cs.byu.edu")
            ("Philip Windley" #f "windley@cs.byu.edu")
            ("Scott Woodfield" #f "woodfiel@cs.byu.edu")
            ("Daniel Zappala" #f "zappala@cs.byu.edu")))])
  (match-define (list n netid email) n*netid*email)
  (make-faculty #:name n))

(for ([a (applicants)])
  (displayln (applicant-first-name a)))