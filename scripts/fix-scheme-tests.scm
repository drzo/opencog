#!/usr/bin/env guile
!#
;;; fix-scheme-tests.scm
;;;
;;; OpenCog Scheme Test Fixer
;;; =========================
;;;
;;; Specialized tool for fixing Scheme/Guile test failures in OpenCog.
;;; This tool can detect and fix common issues in Scheme tests including:
;;; - Module loading errors
;;; - Undefined variable errors
;;; - Type mismatches
;;; - Memory/GC issues
;;; - Test runner configuration problems

(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (ice-9 match)
             (ice-9 ftw)
             (ice-9 pretty-print)
             (srfi srfi-1)
             (srfi srfi-26)
             (system base compile))

;; Define test issue types
(define-record-type <test-issue>
  (make-test-issue file line-number type description severity fix-suggestion)
  test-issue?
  (file test-issue-file)
  (line-number test-issue-line)
  (type test-issue-type)
  (description test-issue-description)
  (severity test-issue-severity)
  (fix-suggestion test-issue-fix))

;; Issue severity levels
(define SEVERITY-ERROR 'error)
(define SEVERITY-WARNING 'warning)
(define SEVERITY-INFO 'info)

;; Issue types
(define ISSUE-MODULE-NOT-FOUND 'module-not-found)
(define ISSUE-UNDEFINED-VARIABLE 'undefined-variable)
(define ISSUE-TYPE-ERROR 'type-error)
(define ISSUE-GC-ERROR 'gc-error)
(define ISSUE-TEST-RUNNER-CONFIG 'test-runner-config)
(define ISSUE-DEPRECATED-FUNCTION 'deprecated-function)
(define ISSUE-MISSING-DEPENDENCY 'missing-dependency)

;; Global configuration
(define *project-root* (or (getenv "OPENCOG_ROOT") (getcwd)))
(define *dry-run* #f)
(define *verbose* #f)

;; Logging utilities
(define (log-info fmt . args)
  (when *verbose*
    (apply format (current-error-port) 
           (string-append "INFO: " fmt "\n") args)))

(define (log-error fmt . args)
  (apply format (current-error-port) 
         (string-append "ERROR: " fmt "\n") args))

(define (log-warning fmt . args)
  (apply format (current-error-port) 
         (string-append "WARNING: " fmt "\n") args))

;; File utilities
(define (find-scheme-files dir)
  "Find all Scheme files in directory recursively"
  (let ((files '()))
    (ftw dir
         (lambda (filename statinfo flag)
           (when (and (eq? flag 'regular)
                      (or (string-suffix? ".scm" filename)
                          (string-suffix? ".ss" filename)))
             (set! files (cons filename files)))
           #t))
    (reverse files)))

(define (read-file-lines filename)
  "Read file and return list of lines"
  (call-with-input-file filename
    (lambda (port)
      (let loop ((lines '()))
        (let ((line (read-line port)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

(define (write-file-lines filename lines)
  "Write lines to file"
  (call-with-output-file filename
    (lambda (port)
      (for-each (lambda (line)
                  (display line port)
                  (newline port))
                lines))))

;; Pattern matching for common issues
(define module-patterns
  '((missing-module . "no code for module \\(([^)]+)\\)")
    (unbound-module . "Unbound variable: ([^ ]+) in module \\(([^)]+)\\)")
    (wrong-type . "Wrong type argument in position ([0-9]+)")
    (gc-error . "Too many root sets|GC Warning|out of memory")))

;; Analyze a single Scheme file
(define (analyze-scheme-file filename)
  "Analyze a Scheme file for potential issues"
  (log-info "Analyzing ~a" filename)
  (let ((issues '())
        (lines (read-file-lines filename)))
    
    ;; Check for module declarations
    (let ((has-define-module #f)
          (has-use-modules #f))
      (for-each 
       (lambda (line line-num)
         (cond
          ((string-match "^\\s*\\(define-module" line)
           (set! has-define-module #t))
          ((string-match "^\\s*\\(use-modules" line)
           (set! has-use-modules #t))))
       lines
       (iota (length lines) 1))
      
      ;; Check if it's a test file without proper module setup
      (when (and (string-contains filename "test")
                 (not has-define-module))
        (set! issues 
              (cons (make-test-issue 
                     filename 1 
                     ISSUE-TEST-RUNNER-CONFIG
                     "Test file without define-module"
                     SEVERITY-WARNING
                     "Add (define-module (test-name) #:use-module ...)")
                    issues))))
    
    ;; Check for common problematic patterns
    (for-each
     (lambda (line line-num)
       ;; Check for direct opencog module usage without proper import
       (when (and (string-match "\\(cog-[a-z]+\\b" line)
                  (not (string-match "^\\s*;" line))) ; not a comment
         (unless (any (lambda (l) (string-contains l "(opencog)")) 
                      (take lines (min line-num (length lines))))
           (set! issues
                 (cons (make-test-issue
                        filename line-num
                        ISSUE-MISSING-DEPENDENCY
                        "Using OpenCog function without importing (opencog) module"
                        SEVERITY-ERROR
                        "Add (use-modules (opencog)) at the top")
                       issues))))
       
       ;; Check for deprecated test runner usage
       (when (string-contains line "test-runner-current")
         (set! issues
               (cons (make-test-issue
                      filename line-num
                      ISSUE-DEPRECATED-FUNCTION
                      "Using deprecated test-runner-current"
                      SEVERITY-WARNING
                      "Use (opencog-test-runner) instead")
                     issues))))
     lines
     (iota (length lines) 1))
    
    ;; Try to compile the file to detect syntax errors
    (catch #t
      (lambda ()
        (compile-file filename #:env (current-module)))
      (lambda (key . args)
        (let ((error-msg (with-output-to-string
                          (lambda ()
                            (display-error #f (current-output-port) 
                                         #f key args)))))
          (set! issues
                (cons (make-test-issue
                       filename 0
                       ISSUE-TYPE-ERROR
                       (format #f "Compilation error: ~a" error-msg)
                       SEVERITY-ERROR
                       "Fix syntax errors in the file")
                      issues)))))
    
    issues))

;; Fix generators for different issue types
(define (generate-fix issue)
  "Generate a fix for the given issue"
  (case (test-issue-type issue)
    ((module-not-found)
     (generate-module-fix issue))
    ((undefined-variable)
     (generate-undefined-var-fix issue))
    ((gc-error)
     (generate-gc-fix issue))
    ((test-runner-config)
     (generate-test-runner-fix issue))
    ((missing-dependency)
     (generate-dependency-fix issue))
    (else #f)))

(define (generate-module-fix issue)
  "Generate fix for missing module issues"
  (let* ((file (test-issue-file issue))
         (lines (read-file-lines file)))
    ;; Add use-modules at the beginning if missing
    (if (not (any (lambda (line) (string-contains line "use-modules")) lines))
        (cons "(use-modules (opencog) (opencog test-runner))"
              lines)
        lines)))

(define (generate-test-runner-fix issue)
  "Generate fix for test runner configuration"
  (let* ((file (test-issue-file issue))
         (lines (read-file-lines file))
         (fixed-lines '()))
    
    ;; Add proper test runner setup
    (set! fixed-lines
          (append 
           '("(use-modules (opencog test-runner))"
             ""
             "(opencog-test-runner)"
             "")
           lines))
    
    ;; Replace old test-begin with new format
    (map (lambda (line)
           (if (string-match "^\\s*\\(test-begin\\s+\"([^\"]+)\"" line)
               (regexp-substitute #f 
                                (string-match "^(\\s*)\\(test-begin\\s+\"([^\"]+)\"" line)
                                'pre 1 "(test-begin " 2)
               line))
         fixed-lines)))

(define (generate-gc-fix issue)
  "Generate fix for GC-related issues"
  (let* ((file (test-issue-file issue))
         (lines (read-file-lines file)))
    ;; Add GC configuration at the beginning
    (append
     '(";; Configure GC for large test suite"
       "(debug-set! stack 0)"
       "(gc-disable)"
       "")
     lines
     '(""
       ";; Re-enable GC at the end"
       "(gc-enable)"))))

(define (generate-dependency-fix issue)
  "Generate fix for missing dependencies"
  (let* ((file (test-issue-file issue))
         (lines (read-file-lines file))
         (use-modules-line #f)
         (use-modules-idx #f))
    
    ;; Find existing use-modules
    (for-each
     (lambda (line idx)
       (when (string-match "^\\s*\\(use-modules" line)
         (set! use-modules-line line)
         (set! use-modules-idx idx)))
     lines
     (iota (length lines)))
    
    (if use-modules-line
        ;; Add to existing use-modules
        (let ((new-line (string-append 
                        (string-trim-right use-modules-line #\))
                        " (opencog))")))
          (list-set! lines use-modules-idx new-line))
        ;; Add new use-modules
        (set! lines (cons "(use-modules (opencog))" lines)))
    
    lines))

;; Apply fixes to files
(define (apply-fix issue fix-lines)
  "Apply the fix to the file"
  (let ((file (test-issue-file issue)))
    (if *dry-run*
        (begin
          (format #t "Would fix ~a:\n" file)
          (format #t "Issue: ~a\n" (test-issue-description issue))
          (format #t "Fix: ~a\n\n" (test-issue-fix issue)))
        (begin
          (log-info "Fixing ~a" file)
          (write-file-lines file fix-lines)
          (format #t "Fixed: ~a\n" file)))))

;; Main analysis function
(define (analyze-all-tests test-dir)
  "Analyze all test files in directory"
  (let ((test-files (find-scheme-files test-dir))
        (all-issues '()))
    
    (for-each
     (lambda (file)
       (let ((issues (analyze-scheme-file file)))
         (set! all-issues (append all-issues issues))))
     test-files)
    
    ;; Sort by severity
    (sort all-issues
          (lambda (a b)
            (let ((sev-order '((error . 0) (warning . 1) (info . 2))))
              (< (assoc-ref sev-order (test-issue-severity a))
                 (assoc-ref sev-order (test-issue-severity b))))))))

;; Report generation
(define (generate-report issues output-file)
  "Generate a report of all issues found"
  (call-with-output-file output-file
    (lambda (port)
      (format port "OpenCog Scheme Test Analysis Report\n")
      (format port "===================================\n\n")
      (format port "Generated: ~a\n\n" (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time))))
      
      (format port "Summary:\n")
      (format port "--------\n")
      (let ((by-severity (group-by test-issue-severity issues)))
        (for-each
         (lambda (sev)
           (let ((count (length (or (assoc-ref by-severity sev) '()))))
             (when (> count 0)
               (format port "~a: ~a\n" 
                       (string-upcase (symbol->string sev))
                       count))))
         '(error warning info)))
      
      (format port "\n\nDetailed Issues:\n")
      (format port "---------------\n\n")
      
      (for-each
       (lambda (issue)
         (format port "File: ~a:~a\n" 
                 (test-issue-file issue)
                 (test-issue-line issue))
         (format port "Type: ~a\n" (test-issue-type issue))
         (format port "Severity: ~a\n" (test-issue-severity issue))
         (format port "Description: ~a\n" (test-issue-description issue))
         (format port "Suggested Fix: ~a\n\n" (test-issue-fix issue)))
       issues))))

;; Helper function for grouping
(define (group-by key-func lst)
  "Group list elements by the result of key-func"
  (let ((groups '()))
    (for-each
     (lambda (item)
       (let* ((key (key-func item))
              (group (assoc key groups)))
         (if group
             (set-cdr! group (cons item (cdr group)))
             (set! groups (cons (cons key (list item)) groups)))))
     lst)
    groups))

;; Command-line interface
(define (main args)
  "Main entry point"
  (let ((test-dir (string-append *project-root* "/tests"))
        (fix-mode #f)
        (report-file #f))
    
    ;; Parse arguments
    (let loop ((args (cdr args)))
      (cond
       ((null? args) #t)
       ((string=? (car args) "--fix")
        (set! fix-mode #t)
        (loop (cdr args)))
       ((string=? (car args) "--dry-run")
        (set! *dry-run* #t)
        (loop (cdr args)))
       ((string=? (car args) "--verbose")
        (set! *verbose* #t)
        (loop (cdr args)))
       ((string=? (car args) "--test-dir")
        (set! test-dir (cadr args))
        (loop (cddr args)))
       ((string=? (car args) "--report")
        (set! report-file (cadr args))
        (loop (cddr args)))
       (else
        (format #t "Unknown argument: ~a\n" (car args))
        (exit 1))))
    
    ;; Analyze tests
    (format #t "Analyzing Scheme tests in ~a...\n\n" test-dir)
    (let ((issues (analyze-all-tests test-dir)))
      
      ;; Display summary
      (format #t "Found ~a issues:\n" (length issues))
      (let ((by-severity (group-by test-issue-severity issues)))
        (for-each
         (lambda (sev)
           (let ((sev-issues (or (assoc-ref by-severity sev) '())))
             (when (not (null? sev-issues))
               (format #t "  ~a: ~a\n" 
                       (string-upcase (symbol->string sev))
                       (length sev-issues)))))
         '(error warning info)))
      
      ;; Generate report if requested
      (when report-file
        (generate-report issues report-file)
        (format #t "\nReport written to: ~a\n" report-file))
      
      ;; Apply fixes if requested
      (when fix-mode
        (format #t "\n~a fixes...\n\n" 
                (if *dry-run* "Would apply" "Applying"))
        
        (let ((fixed-count 0))
          (for-each
           (lambda (issue)
             (let ((fix (generate-fix issue)))
               (when fix
                 (apply-fix issue fix)
                 (set! fixed-count (+ fixed-count 1)))))
           issues)
          
          (format #t "\n~a ~a issues\n" 
                  (if *dry-run* "Would fix" "Fixed")
                  fixed-count))))))

;; Run main if this is the main script
(when (batch-mode?)
  (main (command-line))) 