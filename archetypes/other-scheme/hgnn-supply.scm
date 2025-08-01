;; hyperedge definition
(define-record-type SUPPLIER-DIVERSITY
  (fields suppliers ingredient product
          event-dates quality-scores sustainability-scores))

;; Attention: normalize sustainability
(define (normalize-scores scores)
  (let ((sum (apply + scores)))
    (map (cut / <> sum) scores)))

;; Build hyperedge instance
(define (make-supplier-diversity suppliers ingredient product meta)
  (make-SUPPLIER-DIVERSITY
   :suppliers suppliers
   :ingredient ingredient
   :product product
   :event-dates (meta 'dates)
   :quality-scores (meta 'quality)
   :sustainability-scores
     (normalize-scores (meta 'sustainability))))