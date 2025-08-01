;; node tagging
(define-record-type NODE
  (fields id type props tenant-id sharing-policy embedding))

;; create shared-knowledge hyperedge
(define-record-type SHARED-KNOWLEDGE
  (fields tenants nodes policy embedding))

(define (make-shared-knowledge tenants nodes policy)
  (make-SHARED-KNOWLEDGE
   :tenants tenants
   :nodes nodes
   :policy policy
   :embedding
     (mask-tenant-traces
       (average (map node-embedding nodes)) policy)))