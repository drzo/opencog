
;; Generated by OpenCog Cognitive Platform
;; Timestamp: 2025-06-14T17:13:39.061Z
;; Template: Basic Transformer
;; Parameters: {"modelName":"TransformerModel","layers":6,"heads":8,"hiddenSize":512,"vocabSize":50000}

;; Basic Transformer Model in AtomSpace
(use-modules (opencog) (opencog atom-types))

(define (create-transformer-model layers heads hidden-size)
  (EvaluationLink
    (PredicateNode "TransformerModel")
    (ListLink
      (NumberNode layers)
      (NumberNode heads)
      (NumberNode hidden-size))))

(define transformer (create-transformer-model 6 8 512))