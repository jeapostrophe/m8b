#lang racket
(require racket/date
         net/url
         web-server/http
         (for-syntax unstable/syntax
                     racket/function)
         (prefix-in 19: srfi/19)
         (planet jaymccarthy/mongodb)
         (planet jaymccarthy/mongodb/dispatch)
         "id-cookie.rkt"
         "model.rkt")

; View
(define (footer)
  `(div ([id "footer"])
        "Powered by " (a ([href "http://racket-lang.org/"]) "Racket") ". "
        "Written by " (a ([href "http://faculty.cs.byu.edu/~jay"]) "Jay McCarthy") ". "))

(define (template #:breadcrumb bc
                  . bodies)
  (response/xexpr
   `(html (head (title ,@(add-between (map car bc) " / "))
                (script ([src "/sorttable.js"]) " ")
                (link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
          (body 
           (div ([class "breadcrumb"])
                ,@(for/list ([b (in-list bc)])
                    (match-define (cons name url) b)
                    (if url
                        `(span (a ([href ,url]) ,name) " / ")
                        `(span ([class "this"]) ,name)))
                ,(if (current-user)
                     `(span ([id "logout"]) 
                            ,(current-user) " | "
                            ,@(if (next-applicant?)
                                  (list `(a ([href ,(top-url next-app)]) "next") " | ")
                                  empty)
                            (a ([href ,(top-url archive)]) "archive") " | "
                            (a ([href ,(top-url logout)]) "logout"))
                     ""))
           (div ([class "content"])
                ,@bodies
                ,(footer))))))

(define (tabs header . the-tabs)
  (define found-selected? #f)
  (define tab-seq
    (build-vector (/ (length the-tabs) 2)
                  (lambda (i)
                    (define id (symbol->string (gensym)))
                    (define label (list-ref the-tabs (* 2 i)))
                    (define body (list-ref the-tabs (add1 (* 2 i))))
                    (define no-content? 
                      (and (string? body)
                           (string=? "" body)))
                    (define selected?
                      (and (not found-selected?)
                           (not no-content?)))
                    (when selected?
                      (set! found-selected? #t))
                    (vector id selected? no-content? label body))))
  `(div ([class "tabbed"])
        (div ([class "tab-header"])
             (div ([class "tab-uheader"]) ,header)
             (ul
              ,@(for/list ([v (in-vector tab-seq)])
                  (match-define (vector id selected? no-content? label body) v)
                  (define direct-link
                    (match body
                      [(cons #f url) url]
                      [_ #f]))
                  `(li ([id ,(format "li~a" id)] ,@(if selected? `([class "tab-selected"]) empty))
                       ,(cond
                          [no-content?
                           label]
                          [direct-link
                           `(a ([href ,direct-link]) ,label)]
                          [else
                           `(a ([href ,(format "javascript:~a~a;"
                                               (for/fold ([s ""]) ([v (in-vector tab-seq)])
                                                 (match-define (vector id selected? no-content? label _) v)
                                                 (format "ToggleOff(~S);~a" id s))
                                               (format "ToggleOn(~S)" id))])
                               ,label)])))))
        ,@(for/list ([v (in-vector tab-seq)])
            (match-define (vector id selected? no-content? _ body) v)
            (define direct-link
              (match body
                [(cons #f url) url]
                [_ #f]))
            `(div ([id ,id]
                   [style ,(if selected?
                               "display: block"
                               "display: none")]
                   [class "tab-content"])
                  ,(if direct-link "" body)))))

(define (data-table . rows)
  (define max-cols
    (apply max 0 (for/list ([row (in-list rows)]
                            #:when row)
                   (length row))))
  `(table ([class "data"])
          ,@(for/list ([row (in-list rows)]
                       #:when row)
              (define len (length row))
              `(tr
                ,@(for/list ([col (in-list row)]
                             [i (in-naturals)])
                    (define label?
                      (even? i))
                    (define last?
                      (= i (sub1 len)))
                    `(td ([class ,(if label? "label" "value")]
                          [colspan ,(number->string (if last? (- max-cols i) 1))])
                         ,col))))))

(define (19:time-subtract t1 t2)
  (19:subtract-duration t1 (19:time-difference t1 t2)))

; Codes
(define-struct code (key str href))
(define (code-set->xexpr-forest cs)
  (for/list ([c (in-list cs)])
    (match-define (struct code (key str href)) c)
    `(a ([class "code"]
         [title ,str]
         ,@(if href
               `([href ,(format href str)])
               empty))
        ,key)))
(define-syntax-rule (list-cond [q code] ...)
  (append (if q (list code) empty)
          ...))  
(define (applicant-codes a)
  (list-cond
   [(symbol=? 'phd (applicant-degree-sought a))
    (make-code "P" "PhD applicant" #f)]
   [(symbol=? 'ms (applicant-degree-sought a)) 
    (make-code "M" "MS applicant" #f)]
   [(not (applicant-complete? a))
    (make-code "I" "Incomplete Application" #f)]
   [(not (string=? "BYU" (applicant-prior-school a)))
    (make-code
     "Y" (applicant-prior-school a)
     "http://www.google.com/search?q=~a")]
   [(not (regexp-match #rx"BS-CS" (applicant-degree a)))
    (make-code "D" (applicant-degree a) #f)]
   [(null-time<? (applicant-gre-date a)
                 (19:time-subtract
                  (19:current-time 19:time-utc)
                  (19:date->time-utc
                   (19:make-date 0 0 0 0 0 12 0 0))))
    (make-code "G" 
               (format "Old GRE scores: ~a"
                       (date->xexpr (applicant-gre-date a)))
               #f)]
   [(not (string=? "USA" (applicant-citizenship a)))
    (make-code "C" (applicant-citizenship a) #f)]
   [(not (applicant-lds? a))
    (make-code "L" "Not LDS" #f)]
   [(not (applicant-financial-aid? a))
    (make-code "F" "No financial aid" #f)]))

(define (applicant-vote->who a)
  (define vote->who (make-hasheq empty))
  (for ([d (applicant-decisions a)])
    (define who (hash-ref d 'who))
    (define vote (hash-ref d 'vote))
    (hash-update! vote->who vote (curry list* who) empty))
  vote->who)

(define (boolean->xexpr b)
  (if b "Y" "N"))
(define (number->xexpr n)
  (if (bson-null? n)
      ""
      (number->string n)))
(define (percentage->xexpr n)
  (if (bson-null? n)
      ""
      (string-append (number->string n) "%")))
(define (date->xexpr s)
  (if (bson-null? s)
      ""
      (19:date->string (19:time-utc->date s) "~B ~Y")))

(define (number->xexpr-forest n)
  (list (number->xexpr n)))
(define (date->xexpr-forest s)
  (list (date->xexpr s)))

(define (string->xexpr-forest s)
  (list s))
(define (degree-sought->xexpr-forest d)
  (list
   (case d
     [(phd) "PhD"]
     [(ms) "MS"])))
(define (number-field/limits->xexpr-forest f phd ms a)
  (define v (f a))
  (define limit
    (case (applicant-degree-sought a)
      [(phd) phd]
      [(ms) ms]
      [else
       (error 'number-field/limits->xexpr-forest 
              "Applicant ~a has crazy degree sought: ~a"
              a (applicant-degree-sought a))]))
  (define vs (number->xexpr-forest v))
  (cond
    [(or (bson-null? v) (v . > . limit))
     vs]
    [(v . > . (* .9 limit))
     `(([class "low"]) ,@vs)]
    [else
     `(([class "verylow"]) ,@vs)]))

(define (applicant-final-decision a)
  (for/or ([d (applicant-decisions a)])
    (and (string=? "COMMITTEE" (hash-ref d 'who))
         (not (eq? (hash-ref d 'vote) 'Undecided))
         `((strong ,(symbol->string (hash-ref d 'vote)))))))

(define possible-votes '(PhD MS Provisional Reject))
(define (vote->who->xexpr-forest vote->who)
  (for/list ([v (in-list possible-votes)])
    `(span ([class ,(format "dec-~a" v)]
            [title ,(symbol->string v)])
           ,(with-handlers ([exn:fail? (lambda (x) 'nbsp)])
              (number->string
               (length
                (hash-ref vote->who v)))))))

(define top-field-ids
  '(decision last-name first-name raw-gre gre-verbal gre-verbal% gre-quant gre-quant% gre-anal gre-anal% cum-gpa major-gpa codes))
(define field->field-name
  #hasheq([decision . "Decisions"]
          [last-name . "Last Name"]
          [first-name . "First Name"]
          [raw-gre . "Raw GRE"]
          [gre-date . "GRE Date"]
          [gre-verbal . "GRE_V"]
          [gre-verbal% . "%"]
          [gre-quant . "GRE_Q"]
          [gre-quant% . "%"]
          [gre-anal . "GRE_A"]
          [gre-anal% . "%"]
          [cum-gpa . "Cum. GPA"]
          [major-gpa . "Major GPA"]
          [codes . ""]))
(define gre-verbal->xexpr-forest
  (curry number-field/limits->xexpr-forest applicant-gre-verbal-score 575 530))
(define gre-quant->xexpr-forest
  (curry number-field/limits->xexpr-forest applicant-gre-quant-score 750 700))
(define gre-anal->xexpr-forest
  (curry number-field/limits->xexpr-forest applicant-gre-analytic-score 5 4.5))
(define major-gpa->xexpr-forest
  (curry number-field/limits->xexpr-forest applicant-major-gpa 3.5 3.25))
(define field->applicant-field-xexpr
  (make-hasheq
   (list (cons 'decision 
               (λ (a)
                 (or (applicant-final-decision a)
                     (vote->who->xexpr-forest 
                      (if (current-user-has-decided? a)
                          (applicant-vote->who a)
                          (hasheq))))))
         (cons 'last-name (compose string->xexpr-forest applicant-last-name))
         (cons 'first-name (compose string->xexpr-forest applicant-first-name))
         (cons 'raw-gre (compose number->xexpr-forest applicant-raw-gre))
         (cons 'gre-date (compose date->xexpr-forest applicant-gre-date))
         (cons 'gre-verbal gre-verbal->xexpr-forest)
         (cons 'gre-verbal% (compose number->xexpr-forest applicant-gre-verbal-percentile))
         (cons 'gre-quant gre-quant->xexpr-forest)
         (cons 'gre-quant% (compose number->xexpr-forest applicant-gre-quant-percentile))
         (cons 'gre-anal gre-anal->xexpr-forest)
         (cons 'gre-anal% (compose number->xexpr-forest applicant-gre-analytic-percentile))
         (cons 'cum-gpa (compose number->xexpr-forest applicant-cumulative-gpa))
         (cons 'major-gpa major-gpa->xexpr-forest)
         (cons 'codes (compose code-set->xexpr-forest applicant-codes)))))

(define (render-applicant-table applicants-seq #:editing? [editing? #f])
  (define rows
    (for/list ([a applicants-seq])
      `(tr ([onclick ,(format "document.location = ~S" (top-url view-app a))])
           ,@(for/list ([id (in-list top-field-ids)])
               (define f (hash-ref field->applicant-field-xexpr id))
               `(td ,@(f a)))
           (td nbsp 
               ,(if editing?
                    `(a ([href ,(top-url edit-app a)]) "Edit")
                    `(a ([href ,(top-url view-app a)]) "View"))))))
  (if (empty? rows)
      ""
      `(table ([class "applicants sortable"])
              (thead (tr ,@(for/list ([id (in-list top-field-ids)])
                             (define name (hash-ref field->field-name id 'nbsp))
                             `(th ,name))
                         (th nbsp)))
              (tbody
               ,@rows))))

(define (applicants-tagged-for user #:tagged? [tagged? #t] #:decided? decided?)
  (define ans
    (mongo-dict-query 
     "applicants" 
     (if tagged?
         (list (cons 'tags (list (cons '$in (vector user)))))
         empty)))
  (for/list ([a ans]
             #:when
             (let* ([non-undecided?
                     (for/or ([d (in-vector (applicant-decisions a))]
                              #:when (equal? user (hash-ref d 'who)))
                       (not (eq? 'Undecided (hash-ref d 'vote))))])
               (eq? decided? non-undecided?)))
    a))

(define (applicants/decided? user #:decided? decided?)
  (applicants-tagged-for user #:tagged? #f #:decided? decided?))

(define (applicant-faculty-decision a f)
  (define n (faculty-name f))
  (define ds (applicant-decisions a))
  (or (for/or ([d (in-vector ds)]
               #:when (equal? n (hash-ref d 'who)))
        (hash-ref d 'vote))
      'Undecided))

(define (render-apps)
  (template
   #:breadcrumb (list (cons "Applicants" #f))
   
   (tabs
    ""
    "Your Applicants"
    (render-applicant-table 
     (applicants-tagged-for (current-user) #:decided? #f))
    "Your Applicants (decided)"
    (render-applicant-table 
     (applicants-tagged-for (current-user) #:decided? #t))
    "All Applicants"
    (render-applicant-table 
     (applicants/decided? (current-user) #:decided? #f))
    "All Applicants (decided)"
    (render-applicant-table 
     (applicants/decided? (current-user) #:decided? #t))
    "Next Undecided Applicant"
    (if (next-applicant?)
        (cons #f (top-url next-app))
        ""))))

(define (next-applicant?)
  (for/or ([a (applicants-tagged-for (current-user) #:decided? #f)])
    a))

(require web-server/formlets/lib)

(define (required-string def)
  (to-string
   (required
    (text-input 
     #:value (if (bson-null? def)
                 #f (string->bytes/utf-8 def))))))
(define (optional-string def)
  (cross (pure (λ (b)
                 (if (binding:form? b)
                     (bytes->string/utf-8 (binding:form-value b))
                     bson-null)))                 
         (text-input #:value (if (bson-null? def)
                                 #f
                                 (string->bytes/utf-8 def)))))
(define (optional-boolean def)
  (cross (pure (λ (x) 
                 (and (binding:form? x)
                      (not (bytes=? #"off" (binding:form-value x))))))
         (checkbox #"" 
                   (if (bson-null? def)
                       #f def))))
(define (optional-date def)
  (cross (pure (λ (x) 
                 (if (binding:form? x)
                     (string->time
                      (bytes->string/utf-8 (binding:form-value x)))
                     def)))
         (text-input #:value (if (bson-null? def)
                                 #"YYYY-MM"
                                 (string->bytes/utf-8 (time->string def))))))
(define (optional-number-in-range def min max)
  (cross 
   (pure (λ (x)
           (or (and (bytes? x) 
                    (let ([n (string->number (bytes->string/utf-8 x))])
                      (and (number? n)
                           (<= min n max)
                           n)))
               bson-null)))
   (required
    (text-input #:value (if (bson-null? def)
                            #f
                            (string->bytes/utf-8 (number->string def)))))))
(define (sym-from def . opts)
  (select-input opts
                #:selected? (λ (x) (eq? def x))
                #:display symbol->string))
(define (optional-file def-id accepted)
  (define def
    (if (bson-null? def-id)
        bson-null
        (make-mongo-dict "files" def-id)))
  (cross (pure (λ (x) 
                 (if (binding:file? x)
                     (mongo-dict-id 
                      (make-file #:uploaded (19:current-time)
                                 #:bytes (binding:file-content x)))
                     def-id)))
         (make-input (λ (n) `(input ([type "file"] [name ,n] [accept ,accepted])
                                    (span ([class "version"])
                                     ,(if (bson-null? def)
                                          "No existing version"
                                          (format "Last version uploaded on ~a"
                                                  (19:date->string 
                                                   (19:time-utc->date (file-uploaded def)))))))))))

(define (applicant/default f v)
  (if v
      (f v)
      bson-null))

(define-syntax (applicant-set! stx)
  (syntax-case stx ()
    [(_ a f ...)
     (with-syntax ([(set-applicant-f! ...)
                    (map (curry format-id #'applicant? "set-applicant-~a!")
                         (syntax->list #'(f ...)))])
       (syntax/loc stx
         (begin (begin 
                  (printf "~v\n" `(set-applicant-f! a ,f))
                  (set-applicant-f! a f))
                ...)))]))

(define (edit-application-form k-url embed/url a)
  (define toefl:t 
    (if a (applicant-toefl a)
        bson-null))
  (define (hash-ref** h k)
    (if (bson-null? h)
        bson-null
        (hash-ref h k bson-null)))
  (define toefl:kind (hash-ref** toefl:t 'kind))
  (define toefl:date (hash-ref** toefl:t 'date))
  (define toefl:reading 
    (match toefl:kind
      [(? bson-null?) bson-null]
      ['None bson-null]
      ['IBT (hash-ref** toefl:t 'read)]
      ['PBT (hash-ref** toefl:t 'reading)]))
  (define toefl:listening 
    (hash-ref** toefl:t 'listen))
  (define toefl:writing
    (match toefl:kind
      [(? bson-null?) bson-null]
      ['None bson-null]
      ['IBT (hash-ref** toefl:t 'write)]
      ['PBT (hash-ref** toefl:t 'writing)]))
  (define toefl:speaking/structure 
    (match toefl:kind
      [(? bson-null?) bson-null]
      ['None bson-null]
      ['IBT (hash-ref** toefl:t 'speak)]
      ['PBT (hash-ref** toefl:t 'structure)]))
  
  (define the-formlet
    (formlet
     (table ([class "appform"])
            (tr (th "First Name")
                (td ,{(required-string (applicant/default applicant-first-name a)) . => . first-name})
                (th "Last Name")
                (td ,{(required-string (applicant/default applicant-last-name a)) . => . last-name}))
            (tr (th "Prior School")
                (td ,{(optional-string (applicant/default applicant-prior-school a)) . => . prior-school})
                (th "Prior Degree")
                (td ,{(optional-string (applicant/default applicant-degree a)) . => . degree}))
            (tr (th "Cumulative GPA")
                (td ,{(optional-number-in-range (applicant/default applicant-cumulative-gpa a) 0 4) . => . cumulative-gpa})
                (th "Major GPA")
                (td ,{(optional-number-in-range (applicant/default applicant-major-gpa a) 0 4) . => . major-gpa}))
            (tr (th ([colspan "3"]) "Is the student LDS?")
                (td ,{(optional-boolean (applicant/default applicant-lds? a)) . => . lds?}))
            (tr (th ([colspan "3"]) "Does the student need financial aid?")
                (td ,{(optional-boolean (applicant/default applicant-financial-aid? a)) . => . financial-aid?}))
            (tr (th ([colspan "3"]) "What degree is the applicant seeking?")      
                (td ,{(sym-from (applicant/default applicant-degree-sought a) 'phd 'ms) . => . degree-sought}))
            (tr (th ([colspan "2"]) "Citizenship")
                (td ([colspan "2"]) ,{(optional-string (applicant/default applicant-citizenship a)) . => . citizenship}))
            
            (tr (td ([colspan "4"]) nbsp))
            
            (tr (th "GRE") (td nbsp)
                (th "Date") (td ,{(optional-date (applicant/default applicant-gre-date a)) . => . gre-date}))
            (tr (th "Verbal")
                (td ,{(optional-number-in-range (applicant/default applicant-gre-verbal-score a) 0 800) . => . gre-verbal-score})
                (th "Percentile")
                (td ,{(optional-number-in-range (applicant/default applicant-gre-verbal-percentile a) 0 99) . => . gre-verbal-percentile}))
            (tr (th "Quant")
                (td ,{(optional-number-in-range (applicant/default applicant-gre-quant-score a) 0 800) . => . gre-quant-score})
                (th "Percentile")
                (td ,{(optional-number-in-range (applicant/default applicant-gre-quant-percentile a) 0 99) . => . gre-quant-percentile}))
            (tr (th "Analytic")
                (td ,{(optional-number-in-range (applicant/default applicant-gre-analytic-score a) 0 6) . => . gre-analytic-score})
                (th "Percentile")
                (td ,{(optional-number-in-range (applicant/default applicant-gre-analytic-percentile a) 0 99) . => . gre-analytic-percentile}))
            
            (tr (td ([colspan "4"]) nbsp))
           
            (tr (th "TOEFL") (td ,{(sym-from toefl:kind 'None 'IBT 'PBT) . => . toefl:kind})
                (th "Date") (td ,{(optional-date toefl:date) . => . toefl:date}))
            (tr (th "Reading") (td ,{(optional-number-in-range toefl:reading 0 100) . => . toefl:reading})
                (th "Listening") (td ,{(optional-number-in-range toefl:listening 0 100) . => . toefl:listening}))
            (tr (th "Writing") (td ,{(optional-number-in-range toefl:writing 0 100) . => . toefl:writing})
                (th "Speaking/Structure") (td ,{(optional-number-in-range toefl:speaking/structure 0 100) . => . toefl:speaking/structure}))
            
            (tr (td ([colspan "4"]) nbsp))

            (tr (th ([colspan "2"]) "Application")
                (td ([colspan "2"]) ,{(optional-file (applicant/default applicant-pdf-application a) "application/pdf") . => . pdf-application}))
            (tr (th ([colspan "2"]) "Reference Letters")
                (td ([colspan "2"]) ,{(optional-file (applicant/default applicant-pdf-letters a) "application/pdf") . => . pdf-letters}))
            (tr (th ([colspan "2"]) "Transcript")
                (td ([colspan "2"]) ,{(optional-file (applicant/default applicant-pdf-transcript a) "application/pdf") . => . pdf-transcript}))
            (tr (td ([colspan "4"]) nbsp))
            (tr (td ([colspan "4"] [align "center"]) (input ([type "submit"])))))
     (let ([a
            (if a
                a
                (make-applicant #:first-name first-name
                                #:last-name last-name
                                #:degree-sought degree-sought
                                #:comments (vector)
                                #:tags (vector)
                                #:decisions (vector)))])
       (define toefl
         (match toefl:kind
           [(? bson-null?) bson-null]
           ['None bson-null]
           ['IBT 
            (hasheq 'kind 'IBT
                    'date toefl:date
                    'read toefl:reading
                    'write toefl:writing
                    'listen toefl:listening
                    'speak toefl:speaking/structure)]
           ['PBT 
            (hasheq 'kind 'PBT
                    'date toefl:date
                    'listen toefl:listening
                    'structure toefl:speaking/structure
                    'reading toefl:reading
                    'writing toefl:writing)]))
       
       (applicant-set! a 
                       first-name last-name prior-school
                       degree cumulative-gpa major-gpa
                       lds? financial-aid? degree-sought
                       citizenship gre-date
                       gre-verbal-percentile gre-verbal-score
                       gre-quant-percentile gre-quant-score
                       gre-analytic-percentile gre-analytic-score
                       toefl
                       pdf-application pdf-letters pdf-transcript)
       a)))
  (define (submit-handler req)
    (formlet-process the-formlet req)
    (redirect-to k-url))
  
  `(form ([action ,(embed/url submit-handler)] [method "post"])
         ,@(formlet-display the-formlet)))

(define (render-admin)
  (send/suspend/dispatch
   (λ (embed/url)
     (template
      #:breadcrumb (list (cons "Applicants (admin)" #f))
      (tabs 
       ""
       "New Applicant"
       `(div ([id "add"])
             ,(edit-application-form (top-url show-root) embed/url #f))
       "All Applicants"
       (render-applicant-table (applicants)
                               #:editing? #t)
       "Manage Faculty Accounts"
       "XXX Incomplete, soweee :)")))))

(define (edit-app req a)
  (define name
    (format "~a ~a"
            (applicant-first-name a)
            (applicant-last-name a)))
  (send/suspend/dispatch
   (lambda (embed/url)
     (template
      #:breadcrumb
      (list (cons "Applicants" (top-url show-root))
            (cons name #f))
      (edit-application-form (top-url edit-app a) embed/url a)))))

; Controller 
(require scheme/runtime-path
         web-server/servlet-env
         web-server/servlet
         web-server/http
         web-server/formlets
         web-server/dispatch)

(define (admin-mode?)
  (string=? "ADMIN" (current-user)))
(define (fake-account? u)
  (or (string=? "ADMIN" u)
      (string=? "COMMITTEE" u)))

(define (show-root req)
  (if (admin-mode?)
      (render-admin)
      (render-apps)))

(define (seq-member? e v)
  (for/or ([t v]
           #:when (equal? t e))
    #t))

(define (sequence-partition ? s)
  (for/fold ([yes empty]
             [no empty])
    ([e s])
    (if (? e)
        (values (list* e yes) no)
        (values yes (list* e no)))))

(define (time->xexpr t)
  `(span ([class "time"])
         ,(19:date->string (19:time-utc->date t) "~B ~e, ~Y ~I:~M~p")))

(define (maybe-add-parens v)
  (if (string=? v "")
      v
      (format "(~a)" v)))

(define (show-app-file req a name)
  (define selector
    (match name
      ["Application.pdf" applicant-pdf-application]
      ["Letters.pdf" applicant-pdf-letters]
      ["Transcripts.pdf" applicant-pdf-transcript]))
  (define file-obj-id
    (selector a))
  (define file
    (make-mongo-dict "files" file-obj-id))
  (response/full
   200 #"Okay"
   (19:time-second (file-uploaded file)) #"application/pdf"
   empty
   (list (file-bytes file))))

(define (current-user-has-decided? a)
  (for/or ([(vote who) (in-hash (applicant-vote->who a))])
    (and (not (symbol=? 'Undecided vote))
         (ormap (curry string=? (current-user)) who))))

(define (applicant-name a)
  (format "~a ~a"
          (applicant-first-name a)
          (applicant-last-name a)))

(define (view-app req a)
  (send/suspend/dispatch
   (lambda (embed/url)
     (apply 
      template
      #:breadcrumb
      (list (cons "Applicants" (top-url show-root))
            (cons (applicant-name a) #f))
      
      (view-app-body a embed/url)))))

(define (view-app-body a embed/url)
  (define name (applicant-name a))
  (define original-tags (applicant-tags a))
  (define (applicant-tagged-with? f)
    (define n (faculty-name f))
    (seq-member? n original-tags))
  (define-values (tagged not-tagged)
    (partition applicant-tagged-with? 
               (filter-not (compose fake-account? faculty-name)
                           (for/list ([f (faculty)]) f))))
  (define comment-formlet
    (formlet 
     ,{(to-string (required (textarea-input))) . => . comment}
     comment))
  (define (tag-formlet facs)
    (formlet
     (#%#
      ,{(select-input facs #:display faculty-name)
        . => .
        tag}
      (br)
      ,{comment-formlet . => . comment})
     (vector tag comment)))
  (define ((handle-tag formlet ! comment-type) req)
    (match-define (vector new-fac comment) (formlet-process formlet req))
    (define n (faculty-name new-fac))
    (push-applicant-comments!
     a (list (cons 'who (current-user))
             (cons 'when (19:current-time))
             (cons 'what comment)
             (cons 'type (vector comment-type n))))
    (! a n)
    (redirect-to (top-url view-app a)))
  
  (define add-tag-formlet (tag-formlet not-tagged))
  (define add-tag-handler (handle-tag add-tag-formlet set-add-applicant-tags! 'add-tag))
  (define remove-tag-formlet (tag-formlet tagged))
  (define remove-tag-handler (handle-tag remove-tag-formlet pull-applicant-tags! 'remove-tag))
  
  (define dec-formlet
    (formlet
     (#%#
      ,{(select-input (list* 'Undecided possible-votes)
                      #:selected? (curry eq? (applicant-faculty-decision a (current-user-obj)))
                      #:display symbol->string)
        . => .
        decision}
      (br)
      ,{comment-formlet . => . comment})
     (vector decision comment)))
  (define (handle-dec req)
    (match-define (vector decision comment) (formlet-process dec-formlet req))
    (push-applicant-comments!
     a (list (cons 'who (current-user))
             (cons 'when (19:current-time))
             (cons 'what comment)
             (cons 'type (vector 'decision decision))))
    (pull-applicant-decisions!
     a (list (cons 'who (current-user))))
    (push-applicant-decisions! 
     a (list (cons 'who (current-user))
             (cons 'vote decision)))
    (redirect-to (top-url view-app a)))
  (define has-decided?
    (current-user-has-decided? a))
  
  (list
   (local [(define (icon-url f)
             (format "/icons/flavour-extended-png/~a.png" f))
           (define (show-link applicant-file-contents name yes-icon no-icon-color)
             (define exists? (not (bson-null? (applicant-file-contents a))))
             (define no-icon (format "button_delete_~a" no-icon-color))
             `(td
               ,(if exists?
                    `(a ([href ,(top-url show-app-file a (format "~a.pdf" name))]) (img ([src ,(icon-url yes-icon)])))
                    `(img ([src ,(icon-url no-icon)]))) (br)
                                                        ,name))]
     `(table ([class "pdfs"])
             (tr
              ,(show-link applicant-pdf-application "Application" "apple-script" "green")
              ,(show-link applicant-pdf-letters "Letters" "document_blank" "blue")
              ,(show-link applicant-pdf-transcript "Transcripts" "curriculum_vitae" "red"))))
   
   (data-table
    (list "Name" name
          "Degree Sought" `(span ,@(degree-sought->xexpr-forest (applicant-degree-sought a))))
    
    (list
     "Citizenship" (applicant-citizenship a)
     "LDS?" (boolean->xexpr (applicant-lds? a))
     "Financial Aid?" (boolean->xexpr (applicant-financial-aid? a)))
    
    (list
     "Prior School" (applicant-prior-school a))
    (list
     "Degree" (applicant-degree a))
    
    
    (list
     "Cumulative GPA" (number->xexpr (applicant-cumulative-gpa a))
     "Major GPA" `(span ,@(major-gpa->xexpr-forest a)))
    
    (list
     "GRE" (date->xexpr (applicant-gre-date a))
     "Verbal" `(span ,@(gre-verbal->xexpr-forest a) nbsp
                     ,(maybe-add-parens (percentage->xexpr (applicant-gre-verbal-percentile a))))
     "Quantative" `(span ,@(gre-quant->xexpr-forest a) nbsp
                         ,(maybe-add-parens (percentage->xexpr (applicant-gre-quant-percentile a))))
     "Analytic" `(span ,@(gre-anal->xexpr-forest a) nbsp
                       ,(maybe-add-parens (percentage->xexpr (applicant-gre-analytic-percentile a)))))
    
    (if (bson-null? (applicant-toefl a))
        #f
        (local [(define toefl (applicant-toefl a))
                (define kind (hash-ref toefl 'kind))]
          (list*
           "TOEFL" (date->xexpr (hash-ref toefl 'date))
           (match kind
             ['IBT
              (define read (hash-ref toefl 'read))
              (define write (hash-ref toefl 'write))
              (define listen (hash-ref toefl 'listen))
              (define speak (hash-ref toefl 'speak))
              (define total (+ read write listen speak))
              (list "Total" (number->string total)
                    "Reading" (number->string read)
                    "Listening" (number->string listen)
                    "Speaking" (number->string speak)
                    "Writing" (number->string write))]
             ['PBT
              (define listen (hash-ref toefl 'listen))
              (define structure (hash-ref toefl 'structure))
              (define reading (hash-ref toefl 'reading))
              (define writing (hash-ref toefl 'writing))
              (define total (+ listen structure reading))
              (list "Total" (number->string total)
                    "Listening" (number->string listen)
                    "Structure" (number->string structure)
                    "Reading" (number->string reading)
                    "Writing" (number->string writing))]))))
    
    (local [(define tags (applicant-tags a))]
      (if (zero? (vector-length tags))
          #f
          (list "Faculty"
                `(ul ([class "horiz"])
                     ,@(for/list ([t tags])
                         `(li ,t))))))) 
   
   (if embed/url
       `(table
         ([class "edit"])
         (thead
          (tr (th "Add Tag")
              (th "Remove Tag")
              (th "Decide")))
         (tbody
          (tr
           (td ,(if (empty? not-tagged)
                    ""
                    `(form ([action ,(embed/url add-tag-handler)] [method "post"])
                           ,@(formlet-display add-tag-formlet) (br)
                           (input ([type "submit"] [value "Add Tag"])))))
           (td ,(if (empty? tagged)
                    ""
                    `(form ([action ,(embed/url remove-tag-handler)] [method "post"])
                           ,@(formlet-display remove-tag-formlet) (br)
                           (input ([type "submit"] [value "Remove Tag"])))))
           (td (form ([action ,(embed/url handle-dec)] [method "post"])
                     ,@(formlet-display dec-formlet) (br)
                     (input ([type "submit"] [value "Decide"])))))))
       'nbsp)
   
   `(h3 "Decisions")
   (local [(define votes (applicant-vote->who a))]
     (if (or has-decided?
             (fake-account? (current-user)))
         (apply data-table
                (for/list ([(vote who) (in-hash votes)])
                  (list (symbol->string vote)
                        `(ul ([class "horiz"])
                             ,@(for/list ([t who])
                                 `(li ,t))))))
         "Redacted"))
   
   `(h3 "Comments")
   `(div ,@(for/list ([c (sort (vector->list (applicant-comments a))
                               19:time>=?
                               #:key (lambda (c)
                                       (hash-ref c 'when)))])
             (define who (hash-ref c 'who))
             (define when (hash-ref c 'when))
             (define what (hash-ref c 'what))
             (define type (hash-ref c 'type))
             (match type
               [(vector 'add-tag tag)
                `(p ([class "comment"])
                    (span ([class "who"]) ,who) " added the tag " (span ([class "tag"]) ,tag) "." (br)
                    ,what ,(time->xexpr when))]
               [(vector 'remove-tag tag)
                `(p ([class "comment"])
                    (span ([class "who"]) ,who) " removed the tag " (span ([class "tag"]) ,tag) "." (br)
                    ,what ,(time->xexpr when))]
               [(vector 'decision decision)
                (if (or (equal? who (current-user)) has-decided?
                        (fake-account? (current-user)))
                    `(p ([class "comment"])
                        (span ([class "who"]) ,who) " made the decision " (span ([class "decision"]) ,(symbol->string decision)) "." (br)
                        ,what ,(time->xexpr when))
                    `(p ([class "comment"])
                        "Redacted"))])))))

(define (logout req)
  (redirect-to 
   (top-url show-root)
   #:headers
   (list
    (cookie->header
     logout-id-cookie))))

(define (next-app req)
  (redirect-to
   (top-url view-app (next-applicant?))))

(define (archive req)
  (apply 
   template
   #:breadcrumb
   (list (cons "Applicants" (top-url show-root))
         (cons "Archive" #f))
   
   (apply append
          (for/list ([a (mongo-dict-query "applicants" (hasheq))])
            (list*
             '(hr)
             `(h1 ,(applicant-name a))
             (view-app-body a #f))))))

(define-values (top-dispatch top-url top-applies?)
  (dispatch-rules+applies
   [("") show-root]
   [("logout") logout]
   [("archive") archive]
   [("next") next-app]
   [("edit" (mongo-dict-arg "applicants")) edit-app]
   [("app" (mongo-dict-arg "applicants")) view-app]
   [("pdf" (mongo-dict-arg "applicants") (string-arg)) show-app-file]))

(define-runtime-path secret-key "secret.key")
(define m8b-key (file->bytes secret-key))

(define current-user (make-parameter #f))
(define (current-user-obj)
  (for/or ([e (mongo-dict-query "faculty" (list (cons 'name (current-user))))])
    e))

(require (planet jaymccarthy/ldap))
(define (authenticate-netid u p)
  (ldap-authenticate "ldap.byu.edu" 389 (format "uid=~a,ou=People,o=BYU.edu" u) p))

(define (login req [last-error #f])
  ; XXX look nice
  (define login-formlet
    (formlet
     (table
      (tr (td "NetID:")
          (td  ,{(to-string (required (text-input))) . => . netid}))
      (tr (td "NetID Password:")
          (td ,{(to-string (required (password-input))) . => . passwd})))
     (values netid passwd)))
  (define log-req
    (send/suspend
     (λ (k-url)
       (template
        #:breadcrumb (list (cons "Login" #f))
        `(div ([id "login"])
              (form ([action ,k-url] [method "post"])
                    ,@(formlet-display login-formlet)
                    (input ([type "submit"] [value "Log in"])))
              ,@(if last-error
                    `((h1 ([class "error"]) ,last-error))
                    '()))))))
  (define-values 
    (netid passwd)
    (formlet-process login-formlet log-req))
  
  (define who
    (or (for/or ([fac (mongo-dict-query "faculty" (hasheq 'netid netid))])
          fac)
        (for/or ([fac (mongo-dict-query "faculty" (hasheq 'name netid))])
          fac)))
  
  (if (not who)
      (login req (format "Invalid username (~S)" netid))
      (let ([authenticated?
             ; If there is no netid, then use the secret key
             (if (faculty-netid who)
                 (authenticate-netid netid passwd)
                 (bytes=? m8b-key (string->bytes/utf-8 passwd)))])
        (if authenticated?
            (redirect-to (top-url show-root)
                         #:headers
                         (list
                          (cookie->header
                           ; XXX It is a bit wrong to use the name rather than the objectid
                           (make-id-cookie m8b-key (faculty-name who)))))
            (login req (format "Invalid password for user (~S)" netid))))))

(define (call-with-custodian-shutdown thunk)
  (define cust (make-custodian))
  (dynamic-wind void
                thunk
                (λ ()
                  (custodian-shutdown-all cust))))
  
(define (login-then-top-dispatch req)
  (printf "[~a] ~a\n" (current-seconds) (url->string (request-uri req)))
  (call-with-custodian-shutdown
   (λ ()
     (call-with-model
      (λ ()
        (if (top-applies? req)
            (let ()
              (define maybe-id (request-valid-id-cookie m8b-key req))
              (match maybe-id
                [#f
                 (login req)]
                [id
                 (parameterize ([current-user id])
                   (top-dispatch req))]))
            (top-dispatch req)))))))

; Configuration
(define-runtime-path static-path "static")
(define-runtime-path private-key "private-key.pem")
(define-runtime-path server-cert "server-cert.pem")

(serve/servlet login-then-top-dispatch
               #:port 9000
               #:listen-ip #f
               #:quit? #f
               #:launch-browser? #t
               #:servlet-regexp #rx""
               #:servlet-path "/"
               #:ssl-cert server-cert
               #:ssl-key private-key
               #:extra-files-paths (list static-path))