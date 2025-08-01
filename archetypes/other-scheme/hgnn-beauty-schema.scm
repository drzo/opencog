;; Core hyper-constructors ────────────────────────────────────────────────
(define (Node id type static dynamic embed)
  (list :id id :type type
        :static static :dynamic dynamic
        :embed embed))

(define (Edge from to type props weight temporal)
  (list :from from :to to :type type
        :props props :weight weight :temporal temporal))

(define (Hyperedge id htype attrs connections embed)
  (list :id id :htype htype
        :attrs attrs :connect connections :embed embed))

;; Example: PRODUCT_FORMULATION hyperedge ────────────────────────────────
(define product-formulation⊗
  (Hyperedge "pf-98765" 'PRODUCT_FORMULATION
    '((formulation_date . "2025-05-18")
      (batch_number     . "PF-05-18-B")
      (quality_score    . 0.93)
      (sustainability_score . 0.81))
    (list
     ;; finished product
     (list :node "prod-78901"  :role 'product)
     ;; brand creator
     (list :node "brand-567"   :role 'brand)
     ;; ingredients
     (list :node "ing-12345"   :role 'ingredient :conc "2%")
     (list :node "ing-67890"   :role 'ingredient :conc "0.5%")
     ;; supplier
     (list :node "supplier-345" :role 'supplier :quality_grade "A+"))
    '(0.11 0.34 -0.07 0.63 0.19)))