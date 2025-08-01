;; ≡≡≡ HGNN Self-Debiasing Extensions ≡≡≡

;; 1. Overlay nodes -------------------------------------------------------
(define (BiasLens id entropy-threshold contrast-factor)
  (Node id 'BIAS_LENS
        `((entropy_threshold . ,entropy-threshold)
          (contrast_factor   . ,contrast-factor))
        '() ;; dynamic will be appended
        '())) ;; embedding placeholder

(define (AnomalyDetector id sensitivity)
  (Node id 'ANOMALY_DETECTOR
        `((sensitivity . ,sensitivity))
        '()
        '()))

;; 2. Hyperedge: REFLECTION_LOOP -----------------------------------------
(define (ReflectionLoop⊗ id engineer spec lens detector attrs)
  (Hyperedge id 'REFLECTION_LOOP
    attrs
    (list (list :node engineer :role 'engineer)
          (list :node spec      :role 'spec)
          (list :node lens      :role 'bias_lens)
          (list :node detector  :role 'anomaly_detector))
    '())) ;; embedding filled during training

;; 3. Adaptive Attention Allocation (AAA) ---------------------------------
(define (update-attention Σ entropy-score contrast-factor)
  ;; Σ: base attention weights vector
  (let* ((penalty   (* contrast-factor (- entropy-score 0.5)))
         (new-Σ     (map (λ (w) (max 0 (- w penalty))) Σ)))
    ;; renormalize
    (let ((s (apply + new-Σ)))
      (map (λ (w) (/ w s)) new-Σ))))

;; 4. Injection into message-passing loop ---------------------------------
(define (message-pass node incoming-embeds edge-weights attention-Σ)
  ;; simplified demonstration
  (let* ((avg-in  (apply vector-avg incoming-embeds))
         (att-Σ   (update-attention attention-Σ
                                    (graph-entropy node) ; custom metric
                                    (node-get node 'contrast_factor 1.0)))
         (weighted (vector-scale avg-in att-Σ)))
    (vector-add (node-embed node) weighted)))