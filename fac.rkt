#lang racket
(require "model.rkt"
         (planet jaymccarthy/mongodb))

(call-with-model
 (λ ()
   (mongo-collection-remove!
    (mongo-collection (current-mongo-db) "faculty")
    (hasheq))
   
   (for ([n*netid*email (in-list
                         '(("ADMIN" #f #f)
                           ("COMMITTEE" #f #f)
                           ("Cory Barker" "jcb67" "corybarker@byu.edu")
                           ("Bill Barrett" "wab2" "barrett@cs.byu.edu")
                           ("Robert Burton" "rpb2" "rpburton@cs.byu.edu")
                           ("Mark Clement" "mjc22" "clement@cs.byu.edu")
                           ("Parris Egbert" "pe2" "egbert@cs.byu.edu")
                           ("David Embley" "dwe" "embley@cs.byu.edu")
                           ("Ryan Farrell" "rmf43" "farrell@cs.byu.edu")
                           ("J. Kelly Flanagan" "jkf2" "kelly_flanagan@byu.edu")
                           ("Christophe Giraud-Carrier" "cg299" "cgc@cs.byu.edu")
                           ("Michael A. Goodrich" "mag27" "mike@cs.byu.edu")
                           ("Michael Jones" "mdj" "jones@cs.byu.edu")
                           ("Charles Knutson" "cdk23" "knutson@cs.byu.edu")
                           ("Tony Martinez" "trm3" "martinez@cs.byu.edu")
                           ("Jay McCarthy" "jaymcc" "jay@cs.byu.edu")
                           ("Eric Mercer" "egm8" "egm@cs.byu.edu")
                           ("Bryan Morse" "bsm" "morse@cs.byu.edu")
                           ("Dennis Ng" "yn" "ng@cs.byu.edu")
                           ("Dan Olsen" "dro5" "olsen@cs.byu.edu")
                           ("Eric Ringger" "ekr8" "ringger@cs.byu.edu")
                           ("Ken Rodham" "kjr52" "rodham@cs.byu.edu")
                           ("Paul Roper" "prr5" "proper@cs.byu.edu")
                           ("Kent Seamons" "kes69" "seamons@cs.byu.edu")
                           ("Tom Sederberg" "tws" "tom@cs.byu.edu")
                           ("Kevin Seppi" "kds69" "kseppi@byu.edu")
                           ("Quinn Snell" "qos" "snell@cs.byu.edu")
                           ("Dan Ventura" "dav" "ventura@cs.byu.edu")
                           ("Sean Warnick" "seancw" "sean@cs.byu.edu")
                           ("Philip Windley" "pjw" "windley@cs.byu.edu")
                           ("Scott Woodfield" "snw2" "woodfiel@cs.byu.edu")
                           ("Daniel Zappala" "dz33" "zappala@cs.byu.edu")))])
     (match-define (list n netid email) n*netid*email)
     (make-faculty #:name n
                   #:netid netid
                   #:email email))))
