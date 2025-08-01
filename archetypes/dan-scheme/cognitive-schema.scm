;; Cognitive schemas as hypergraph nodes with dynamic tensor shapes

(define-record-type cognitive-schema
  (fields id label tensor-shape children properties))

;; Example: A flow-state schema
(define flow-schema
  (make-cognitive-schema
    'flow
    "Flow State"
    '(batch 1 latent-dim 128 time-steps 32) ; sample tensor shape
    '()
    '((modality . attention)
      (feedback . tightly-coupled)
      (error-sensitivity . high))))