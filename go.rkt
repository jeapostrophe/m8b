#lang scheme
(require scheme/date
         (prefix-in 19: srfi/19)
         (planet jaymccarthy/mongodb)
         (planet jaymccarthy/mongodb/dispatch)
         "id-cookie.ss"
         "model.ss")

; View
(define (footer)
  `(div ([id "footer"])
        "Powered by " (a ([href "http://plt-scheme.org/"]) "PLT Scheme") ". "
        "Written by " (a ([href "http://faculty.cs.byu.edu/~jay"]) "Jay McCarthy") ". "))

(define (template #:breadcrumb bc
                  . bodies)
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
                           (a ([href ,(top-url logout)]) "logout"))
                    ""))
          (div ([class "content"])
               ,@bodies
               ,(footer)))))

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
      [(ms) ms]))
  (define vs (number->xexpr-forest v))
  (cond
    [(or (bson-null? v) (v . > . limit))
     vs]
    [(v . > . (* .9 limit))
     `(([class "low"]) ,@vs)]
    [else
     `(([class "verylow"]) ,@vs)]))

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
  #hasheq([decision . "Votes"]
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
   (list (cons 'decision (compose vote->who->xexpr-forest applicant-vote->who))
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

(define (render-applicant-table applicants-seq)
  (define rows
    (for/list ([a applicants-seq])
      `(tr ([onclick ,(format "document.location = ~S" (top-url edit-app a))])
           ,@(for/list ([id (in-list top-field-ids)])
               (define f (hash-ref field->applicant-field-xexpr id))
               `(td ,@(f a)))
           (td nbsp (a ([href ,(top-url edit-app a)]) "View")))))
  (if (empty? rows)
      ""
      `(table ([class "applicants sortable"])
              (thead (tr ,@(for/list ([id (in-list top-field-ids)])
                             (define name (hash-ref field->field-name id 'nbsp))
                             `(th ,name))
                         (th nbsp)))
              (tbody
               ,@rows))))

(define (applicants-tagged-for user #:tagged? [tagged? #t] #:decided? [decided? bson-null])
  (mongo-dict-query 
   "applicants" 
   (append 
    (if (bson-null? decided?)
        empty
        (list (cons 'decisions 
                    (let ([m (list (cons '$elemMatch
                                         (list (cons 'who user))))])
                      (if decided?
                          m
                          (list (cons '$not m)))))))
    (if tagged?
        (list (cons 'tags (list (cons '$in (vector user)))))
        empty))))

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
(define (new-app req)
  ; XXX have this make sense
  (define name (formlet-process applicant-formlet req))
  (make-applicant name)
  ; XXX
  (redirect-to (top-url show-root)))

(define required-string input-string)
; XXX
(define optional-string input-string)
; XXX
(define optional-boolean
  (cross (pure (λ (x) 
                 (if (binding:form? x)
                     (bytes=? #"on" (binding:form-value x))
                     bson-null)))
         (checkbox #"" #f)))
; XXX
(define optional-date required-string)
; XXX
(define (optional-number-in-range min max)
  input-int)
; XXX
(define (optional-from . opts)
  (select-input opts #:display symbol->string))
; XXX
(define (optional-file suffix)
  input-string)

; XXX Use this to edit as well.
(define applicant-formlet
  (formlet
   (#%#
    ,{required-string . => . first-name}
    ,{required-string . => . last-name}
    ,{optional-string . => . citizenship}
    ,{optional-boolean . => . lds?}
    ,{optional-boolean . => . financial-aid?}
    ,{optional-date . => . gre-date}
    ,{(optional-number-in-range 0 800) . => . gre-verbal-score}
    ,{(optional-number-in-range 0 99) . => . gre-verbal-percentile}
    ,{(optional-number-in-range 0 800) . => . gre-quant-score}
    ,{(optional-number-in-range 0 99) . => . gre-quant-percentile}
    ,{(optional-number-in-range 0 6) . => . gre-analytic-score}
    ,{(optional-number-in-range 0 99) . => . gre-analytic-percentile}
    ,{optional-string . => . prior-school}
    ,{(optional-number-in-range 0 4) . => . cumulative-gpa}
    ,{(optional-number-in-range 0 4) . => . major-gpa}
    ,{optional-string . => . degree}
    ,{(optional-from 'PhD 'MS) . => . degree-sought}
    ; XXX TOEFL
    ,{(optional-file ".pdf") . => . pdf-application}
    ,{(optional-file ".pdf") . => . pdf-letters}
    ,{(optional-file ".pdf") . => . pdf-transcript}
    )
   first-name))

(define (render-admin)
  (template
   #:breadcrumb (list (cons "Applicants (admin)" #f))
   (tabs 
    ""
    "New Applicant"
    `(div ([id "add"])
          (form ([action ,(top-url new-app)] [method "post"])
                ,@(formlet-display applicant-formlet)))
    "All Applicants"
    ; XXX Have edit rather than view links
    (render-applicant-table (applicants)))))

; Controller 
(require scheme/runtime-path
         web-server/servlet-env
         web-server/servlet
         web-server/http
         web-server/formlets
         web-server/dispatch)

(define (admin-mode?)
  (string=? "ADMIN" (current-user)))

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
  (make-response/full
   200 #"Okay"
   (19:time-second (file-uploaded file)) #"application/pdf"
   empty
   (list (file-bytes file))))

(define (edit-app req a)
  (define original-tags (applicant-tags a))
  (define (applicant-tagged-with? f)
    (define n (faculty-name f))
    (seq-member? n original-tags))
  (define-values (tagged not-tagged)
    (sequence-partition applicant-tagged-with? (faculty)))
  (define comment-formlet
    (formlet 
     ,{(textarea-input) . => . comment}
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
    (redirect-to (top-url edit-app a)))
  
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
    (unless (symbol=? 'Undecided decision)
      (push-applicant-comments!
       a (list (cons 'who (current-user))
               (cons 'when (19:current-time))
               (cons 'what comment)
               (cons 'type (vector 'decision decision))))
      (pull-applicant-decisions!
       a (list (cons 'who (current-user))))
      (push-applicant-decisions! 
       a (list (cons 'who (current-user))
               (cons 'vote decision))))
    (redirect-to (top-url edit-app a)))
  
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
      
      `(h3 "Decisions")
      (apply data-table
             (for/list ([(vote who) (in-hash (applicant-vote->who a))])
               (list (symbol->string vote)
                     `(p ,@who))))
      
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
                   `(p ([class "comment"])
                       (span ([class "who"]) ,who) " made the decision " (span ([class "decision"]) ,(symbol->string decision)) "." (br)
                       ,what ,(time->xexpr when))])))))))

(define (logout req)
  (redirect-to 
   (top-url show-root)
   #:headers
   (list
    (cookie->header
     logout-id-cookie))))

(define (next-app req)
  (redirect-to
   (top-url edit-app (next-applicant?))))

(define-values (top-dispatch top-url top-applies?)
  (dispatch-rules+applies
   [("") show-root]
   [("logout") logout]
   [("new") new-app]
   [("next") next-app]
   [("app" (mongo-dict-arg "applicants")) edit-app]
   [("pdf" (mongo-dict-arg "applicants") (string-arg)) show-app-file]))

(define-runtime-path secret-key "secret.key")
(define m8b-key (file->bytes secret-key))

(define current-user (make-parameter #f))
(define (current-user-obj)
  (for/or ([e (mongo-dict-query "faculty" (list (cons 'name (current-user))))])
    e))

; XXX look nice
(define login-formlet
  (formlet
   (span "Name:" 
         ,{(select-input
            (faculty)
            #:display faculty-name)
           . => . who})
   who))
(define (login req)
  (define log-req
    (send/suspend
     (λ (k-url)
       (template
        #:breadcrumb (list (cons "Login" #f))
        `(div ([id "login"])
              (form ([action ,k-url] [method "post"])
                    ,@(formlet-display login-formlet)
                    (input ([type "submit"] [value "Log in"]))))))))
  (define who (formlet-process login-formlet log-req))
  (redirect-to (top-url show-root)
               #:headers
               (list
                (cookie->header
                 ; XXX It is a bit wrong to use the name
                 (make-id-cookie m8b-key (faculty-name who))))))

(define (login-then-top-dispatch req)
  (if (top-applies? req)
      (match (request-valid-id-cookie m8b-key req)
        [#f
         (login req)]
        [id
         (parameterize ([current-user id])
           (top-dispatch req))])
      (top-dispatch req)))

; Configuration
(define-runtime-path static-path "static")

(serve/servlet login-then-top-dispatch
               #:port 9000
               #:listen-ip #f
               #:quit? #f
               #:launch-browser? #t
               #:servlet-regexp #rx""
               #:servlet-path "/"
               #:extra-files-paths (list static-path))