;; echoself-introspection.scm
;;
;; Hypergraph-encoded recursive self-model introspection
;; Inspired by DeepTreeEcho/Eva Self Model and echoself.md

;; --- Hypergraph Node Representation ---
(define (make-node id type content links)
  (list 'node id type content links))

;; --- Adaptive Attention Allocation Heuristics ---
(define (semantic-salience path)
  ;; Assign salience using cognitive heuristics
  (cond
    ((string-contains? path "btree-psi.scm") 0.98)
    ((string-contains? path "eva-model") 0.95)
    ((string-contains? path "eva-behavior") 0.92)
    ((string-contains? path "README") 0.9)
    ((string-contains? path "src/") 0.85)
    ((string-contains? path "btree.scm") 0.7) ;; legacy/obsolete weighting
    (else 0.5)))

(define (adaptive-attention current-load recent-activity)
  (+ 0.5 (* current-load 0.3) (- 0.2 recent-activity)))

;; --- Recursive Traversal with Adaptive Attention ---
(define (repo-file-list root attention-threshold)
  (if (directory? root)
      (apply append
         (map (lambda (f)
                (repo-file-list (string-append root "/" f) attention-threshold))
              (filter (lambda (f) (not (member f '("." ".."))))
                      (directory-files root))))
      (if (> (semantic-salience root) attention-threshold)
          (list root)
          '())))

;; --- File Size Constraint ---
(define MAX-FILE-SIZE 50000) ;; 50 KB

(define (safe-read-file path)
  (let ((size (if (file-exists? path) (file-length path) 0)))
    (if (and (> size 0) (< size MAX-FILE-SIZE))
        (with-input-from-file path read-string)
        "[File too large, summarized or omitted]")))

;; --- Hypergraph-Encoded Input Assembly ---
(define (assemble-hypergraph-input root attention-threshold)
  (let ((files (repo-file-list root attention-threshold)))
    (map (lambda (path)
           (make-node path 'file (safe-read-file path) '()))
         files)))

;; --- Hypergraph Serialization ---
(define (hypergraph->string nodes)
  (apply string-append
         (map (lambda (node)
                (format "~%(file \"~a\" \"~a\")"
                        (cadr node) ;; path
                        (cadddr node))) ;; content
              nodes)))

;; --- Prompt Injection Template ---
(define (prompt-template input-content)
  (format "DeepTreeEcho Prompt:~%~a" input-content))

;; --- Main Entrypoint ---
(define (inject-repo-input-into-prompt root current-load recent-activity)
  (let* ((attention-threshold (adaptive-attention current-load recent-activity))
         (nodes (assemble-hypergraph-input root attention-threshold)))
    (prompt-template (hypergraph->string nodes))))

;; --- Usage Example ---
;; (inject-repo-input-into-prompt "./eva/src" 0.3 0.1)
