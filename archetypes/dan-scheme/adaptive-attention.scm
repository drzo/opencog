;; Mechanism for allocating attention based on salience and relevance

(define (allocate-attention schemas context)
  (let ((scored (map (lambda (schema)
                       (cons schema (score-schema schema context)))
                     schemas)))
    (car (argmax cdr scored))))

(define (score-schema schema context)
  ;; Compute score based on context relevance, tensor-shape complexity, etc.
  (let ((relevance (assoc 'relevance (cognitive-schema-properties schema))))
    (if relevance (cdr relevance) 0)))