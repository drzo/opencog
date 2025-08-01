;; File: timeless_way.scm
;; A minimal Scheme-based encoding of a hypergraph for Timeless Way patterns
;; demonstrating patterns, relationships, and sample transformations.

;; Each pattern is stored as:
;; (pattern
;;   name
;;   description
;;   (list of sub-pattern references)
;;   (list of super-pattern references))

(define *patterns* '())

(define (make-pattern name description sub-patterns super-patterns)
  (list 'pattern name description sub-patterns super-patterns))

(define (pattern-name p)        (cadr p))
(define (pattern-description p) (caddr p))
(define (pattern-subs p)        (cadddr p))
(define (pattern-supers p)      (car (cddddr p)))

;; Helper to add a newly defined pattern into the global list.
(define (register-pattern! pat)
  (set! *patterns* (cons pat *patterns*))
  pat)

;; Example: A simplified pattern for "Entrance Transition"
;; sub-patterns might be "Threshold Marker", "Visual Privacy Shift"
;; super-pattern might be "Layout of House"
(register-pattern!
  (make-pattern
   "Entrance-Transition"
   "A transitional space that mediates public and private domains."
   '("Threshold-Marker" "Visual-Privacy-Shift")
   '("House-Layout")))

;; Another example: a smaller sub-pattern
(register-pattern!
  (make-pattern
   "Threshold-Marker"
   "A small architectural detail marking the shift from outside to inside."
   '()
   '("Entrance-Transition")))

;; Query function to retrieve a pattern by name
(define (get-pattern-by-name pat-name)
  (let loop ((remaining *patterns*))
    (cond
      ((null? remaining) #f)
      ((string=? pat-name (pattern-name (car remaining))) (car remaining))
      (else (loop (cdr remaining))))))

;; Evaluate structural integrity:
;; Check that all sub-patterns exist and link back to the parent for synergy
(define (evaluate-pattern pat)
  (let ((subs (pattern-subs pat))
        (supers (pattern-supers pat))
        (missing-sub-patterns '())
        (missing-super-patterns '()))
    (for-each
      (lambda (sp)
        (if (not (get-pattern-by-name sp))
            (set! missing-sub-patterns (cons sp missing-sub-patterns))))
      subs)
    (for-each
      (lambda (sup)
        (if (not (get-pattern-by-name sup))
            (set! missing-super-patterns (cons sup missing-super-patterns))))
      supers)
    (list
     (cons 'pattern (pattern-name pat))
     (cons 'missing-sub-patterns (reverse missing-sub-patterns))
     (cons 'missing-super-patterns (reverse missing-super-patterns)))))

;; Sample function applying a pattern (dummy transform)
;; In practice, you'd integrate with a larger environment representation
(define (apply-pattern pat env)
  (append env (list (string-append "Applied pattern: " (pattern-name pat)))))

;; Simple test harness
(define (run-tests)
  (let ((entrance (get-pattern-by-name "Entrance-Transition"))
        (threshold (get-pattern-by-name "Threshold-Marker"))
        (bogus (get-pattern-by-name "Bogus-Pattern")))
    (display "Test 1: Retrieving valid patterns... ")
    (display (if (and entrance threshold (not bogus))
                  "Pass" "Fail"))
    (newline)

    (display "Test 2: Evaluating structural integrity... ")
    (let ((res1 (evaluate-pattern entrance))
          (res2 (evaluate-pattern threshold)))
      (if (and (null? (assoc 'missing-sub-patterns res1))
               (null? (assoc 'missing-super-patterns res2)))
          (display "Pass")
          (display "Fail")))
    (newline)

    (display "Test 3: Applying a pattern to environment... ")
    (let ((transformed (apply-pattern entrance '("RoomA" "RoomB"))))
      (display (if (member "Applied pattern: Entrance-Transition" transformed)
                    "Pass" "Fail")))
    (newline)))

;; Uncomment to run tests directly:
;; (run-tests)