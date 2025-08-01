;; MachSpace Core - AtomSpace as Distributed Microkernel
;; This is where GNU Hurd meets OpenCog!

(define-module (machspace core)
  #:use-module (opencog)
  #:use-module (opencog atom-types)
  #:use-module (opencog exec)
  #:use-module (ice-9 threads)
  #:export (create-mach-port
            create-cognitive-port
            distribute-atomspace
            port->atom
            atom->port
            machspace-init))

;; Define new atom types for system objects
(define-atom-type SystemObject
  Atom
  "Base type for all system objects as atoms")

(define-atom-type MachPort
  Link
  "Mach port as hyperedge connecting system objects")

(define-atom-type CapabilityNode
  Node
  "Capability/permission atom")

;; The global MachSpace - our distributed AtomSpace
(define *machspace* #f)

;; Initialize MachSpace
(define (machspace-init)
  (set! *machspace* (cog-new-atomspace))
  (cog-set-atomspace! *machspace*)
  
  ;; Create fundamental system atoms
  (ConceptNode "kernel" (stv 1.0 1.0))
  (ConceptNode "user" (stv 0.8 0.9))
  (ConceptNode "root" (stv 1.0 1.0))
  
  ;; Create capability atoms
  (CapabilityNode "read")
  (CapabilityNode "write")
  (CapabilityNode "execute")
  (CapabilityNode "send")
  (CapabilityNode "receive")
  
  *machspace*)

;; Convert a traditional Mach port to an atom
(define (port->atom port-id capabilities)
  (let* ((port-node (NumberNode port-id))
         (port-atom (MachPort
                      port-node
                      (ConceptNode (string-append "port-" 
                                                 (number->string port-id)))))
         (attention-value (av 100 1))) ; Initial attention
    
    ;; Attach capabilities
    (for-each
      (lambda (cap)
        (MemberLink
          (CapabilityNode cap)
          port-atom))
      capabilities)
    
    ;; Set attention value for ECAN
    (cog-set-av! port-atom attention-value)
    
    port-atom))

;; Create a cognitive port with attention tracking
(define (create-cognitive-port name source-atom target-atom capabilities)
  (let* ((port-concept (ConceptNode name))
         (port-link (MachPort
                      source-atom
                      target-atom
                      port-concept)))
    
    ;; Add capabilities
    (for-each
      (lambda (cap)
        (EvaluationLink
          (PredicateNode "has-capability")
          (ListLink port-link (CapabilityNode cap))))
      capabilities)
    
    ;; Add to attention allocation
    (cog-set-av! port-link (av 50 0.8))
    
    port-link))

;; Distribute AtomSpace across cores/machines
(define (distribute-atomspace #:key 
                             (replication-factor 3)
                             (shards 4))
  (define shards-list '())
  
  ;; Create shard atomspaces
  (let loop ((i 0))
    (when (< i shards)
      (let ((shard (cog-new-atomspace)))
        (set! shards-list (cons shard shards-list))
        (loop (+ i 1)))))
  
  ;; Create replication links
  (ConceptNode "distributed-machspace"
    (av 1000 1) ; Highest importance
    (ListLink
      (map (lambda (shard)
             (ConceptNode (format #f "shard-~a" shard)))
           shards-list)))
  
  shards-list)

;; Convert atom back to port operations
(define (atom->port port-atom)
  (let* ((port-id (cog-name (car (cog-incoming-set port-atom))))
         (capabilities (map cog-name
                           (cog-chase-link 'MemberLink 'CapabilityNode
                                          port-atom))))
    (list 'port-id port-id
          'capabilities capabilities
          'attention (cog-av-sti port-atom))))

;; Example: Create a traditional Mach port
(define (create-mach-port port-id caps)
  (port->atom port-id caps))