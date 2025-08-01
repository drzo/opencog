;; record for treatment-instance
(define-record-type TREATMENT-INSTANCE
  (fields id connections datetime duration price outcome-rating embedding))

;; aggregate node embeddings by role
(define (aggregate-roles connections)
  (let ((embeds (map (λ (conn)
                       (let ((e (node-embedding (conn 'node-id))))
                         (* (role-attn (conn 'role)) e)))
                     connections)))
    (reduce vector-add embeds)))

;; create instance with attention allocation
(define (make-treatment-instance id conns meta)
  (let ((agg (aggregate-roles conns)))
    (make-TREATMENT-INSTANCE
     :id id
     :connections conns
     :datetime (meta 'datetime)
     :duration (meta 'duration)
     :price (meta 'price)
     :outcome-rating (meta 'rating)
     :embedding (attention-head agg))))