#lang scheme
(require unstable/bytes
         net/base64
         net/cookie
         web-server/http
         web-server/stuffers/hmac-sha1)

(define (substring* s st en)
  (substring s st (+ (string-length s) en)))

(define (mac key v)
  (substring*
   (bytes->string/utf-8
    (base64-encode (HMAC-SHA1 key (write/bytes v))))
   0 -3))

(define-signature id-cookie^
  ((contracted
    [logout-id-cookie cookie?]
    [request-valid-id-cookie (bytes? request? . -> . (or/c false/c cookie-value?))]
    [make-id-cookie (bytes? cookie-value? . -> . cookie?)])))
(define (id-cookie@ name)
  (unit (import)
        (export id-cookie^)
        
        (define (id-cookie? c)
          (and (client-cookie? c)
               (string=? (client-cookie-name c) name)))
        
        (define (make-id-cookie key data)
          (define authored (current-seconds))
          (define digest
            (mac key (list authored data)))
          (make-cookie name
                       (format "~a&~a&~a"
                               digest authored data)))
        
        (define (valid-id-cookie? key c)
          (and (id-cookie? c)
               (with-handlers ([exn:fail? (lambda (x) #f)])
                 (match (client-cookie-value c)
                   [(regexp #rx"^(.+)&(.+)&(.*)$" (list _ digest authored-s data))
                    (define authored (string->number authored-s))
                    (define re-digest (mac key (list authored data)))
                    (and (string=? digest re-digest)
                         ; XXX Checked authored for timeout
                         data)]
                   [cv
                    #f]))))  
        
        (define (request-valid-id-cookie key req)
          (define cookies (request-cookies req))
          (for/or ([c (in-list cookies)])
            (valid-id-cookie? key c)))
        
        (define logout-id-cookie
          (make-cookie name "invalid format"))))

(define-values/invoke-unit (id-cookie@ "id")
  (import)
  (export id-cookie^))

(provide
 id-cookie^)
(provide/contract
 [id-cookie@ (cookie-name? . -> . (unit/c (import) (export id-cookie^)))]
 [logout-id-cookie cookie?]
 [request-valid-id-cookie (bytes? request? . -> . (or/c false/c cookie-value?))]
 [make-id-cookie (bytes? cookie-value? . -> . cookie?)])