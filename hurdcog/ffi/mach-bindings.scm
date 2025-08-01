;; Mach FFI Bindings for HurdCog
;; Bridge between cognitive layer and Mach microkernel

(define-module (ffi mach-bindings)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (machspace core)
  #:use-module (grip cognitive-grip)
  #:export (mach-port-allocate
            mach-port-deallocate
            mach-port-insert-right
            mach-msg
            cognitive-mach-port
            port->cognitive-atom))

;; Load the Mach library
(define libmach (dynamic-link "libmach"))

;; Basic Mach types
(define mach_port_t uint32)
(define mach_port_name_t uint32)
(define mach_msg_return_t int)
(define kern_return_t int)

;; Mach return codes
(define KERN_SUCCESS 0)
(define KERN_INVALID_ARGUMENT 4)
(define KERN_NO_SPACE 3)
(define KERN_RESOURCE_SHORTAGE 6)

;; Port rights
(define MACH_PORT_RIGHT_RECEIVE 1)
(define MACH_PORT_RIGHT_SEND 0)
(define MACH_PORT_RIGHT_SEND_ONCE 2)

;; Message options
(define MACH_SEND_MSG 1)
(define MACH_RCV_MSG 2)

;; FFI: mach_port_allocate
(define %mach-port-allocate
  (pointer->procedure kern_return_t
                     (dynamic-func "mach_port_allocate" libmach)
                     (list '* int '*))) ; task, right, name

;; Cognitive wrapper for port allocation
(define (mach-port-allocate task right)
  "Allocate a Mach port and create its cognitive representation"
  (let* ((port-ptr (make-bytevector 4))
         (result (%mach-port-allocate task right 
                                      (bytevector->pointer port-ptr))))
    
    (if (= result KERN_SUCCESS)
        (let* ((port-id (bytevector-u32-native-ref port-ptr 0))
               ;; Create cognitive atom for this port
               (port-atom (create-mach-port port-id 
                                          (right->capabilities right)))
               ;; Establish grip on the port
               (grip (grip-object port-atom)))
          
          ;; Return cognitive port structure
          (list 'cognitive-port
                'id port-id
                'atom port-atom
                'grip grip))
        
        (error "Failed to allocate Mach port" result))))

;; Convert Mach right to capability list
(define (right->capabilities right)
  (case right
    ((1) '("receive" "send")) ; RECEIVE right includes SEND
    ((0) '("send"))
    ((2) '("send-once"))
    (else '())))

;; FFI: mach_port_deallocate
(define %mach-port-deallocate
  (pointer->procedure kern_return_t
                     (dynamic-func "mach_port_deallocate" libmach)
                     (list '* mach_port_name_t)))

;; Cognitive wrapper for port deallocation
(define (mach-port-deallocate task cognitive-port)
  "Deallocate a Mach port and release its cognitive grip"
  (let* ((port-id (assoc-ref cognitive-port 'id))
         (grip (assoc-ref cognitive-port 'grip))
         (result (%mach-port-deallocate task port-id)))
    
    ;; Release cognitive grip
    (release-grip grip)
    
    ;; Reduce attention on the atom
    (let ((atom (assoc-ref cognitive-port 'atom)))
      (cog-set-av! atom (av 0 0)))
    
    (= result KERN_SUCCESS)))

;; FFI: mach_port_insert_right
(define %mach-port-insert-right
  (pointer->procedure kern_return_t
                     (dynamic-func "mach_port_insert_right" libmach)
                     (list '* mach_port_name_t mach_port_t int)))

;; Cognitive wrapper for inserting port rights
(define (mach-port-insert-right task name right-port right-type)
  "Insert a port right and update cognitive representation"
  (let* ((result (%mach-port-insert-right task name right-port right-type))
         ;; Find the cognitive port
         (port-atom (cog-node 'MachPort (number->string name))))
    
    (when (and (= result KERN_SUCCESS) port-atom)
      ;; Update capabilities in AtomSpace
      (let ((new-caps (right->capabilities right-type)))
        (for-each
          (lambda (cap)
            (MemberLink
              (CapabilityNode cap)
              port-atom))
          new-caps))
      
      ;; Increase attention due to new capability
      (let ((current-av (cog-av port-atom)))
        (cog-set-av! port-atom 
                     (av (+ 10 (cog-av-sti current-av))
                         (cog-av-vlti current-av)))))
    
    (= result KERN_SUCCESS)))

;; Mach message structure (simplified)
(define-record-type <mach-msg>
  (make-mach-msg header body)
  mach-msg?
  (header mach-msg-header)
  (body mach-msg-body))

;; FFI: mach_msg
(define %mach-msg
  (pointer->procedure mach_msg_return_t
                     (dynamic-func "mach_msg" libmach)
                     (list '* int mach_port_t int 
                           mach_port_t int int)))

;; Cognitive wrapper for message passing
(define (mach-msg msg option send-size rcv-size rcv-name timeout notify)
  "Send/receive Mach messages with cognitive tracking"
  
  ;; Convert message to cognitive representation
  (let* ((msg-atom (ConceptNode (format #f "msg-~a" (current-time))))
         (msg-grip (grip-object msg-atom)))
    
    ;; Track message flow in AtomSpace
    (when (and (> (logand option MACH_SEND_MSG) 0) send-port)
      (EvaluationLink
        (PredicateNode "sends-message")
        (ListLink
          (NumberNode (number->string send-port))
          msg-atom)))
    
    ;; Perform actual message operation
    (let ((result (%mach-msg (mach-msg->pointer msg)
                            option send-size rcv-size
                            rcv-name timeout notify)))
      
      ;; Track received messages
      (when (and (= result 0) 
                 (> (logand option MACH_RCV_MSG) 0))
        (EvaluationLink
          (PredicateNode "received-message")
          (ListLink
            (NumberNode (number->string rcv-name))
            msg-atom)))
      
      ;; Return cognitive message info
      (list 'result result
            'msg-atom msg-atom
            'grip msg-grip))))

;; Convert existing Mach port to cognitive atom
(define (port->cognitive-atom port-id capabilities)
  "Transform an existing Mach port into a cognitive atom"
  (let* ((existing (cog-node 'MachPort (number->string port-id)))
         (port-atom (or existing
                       (create-mach-port port-id capabilities)))
         (grip (grip-object port-atom)))
    
    ;; Ensure capabilities are set
    (for-each
      (lambda (cap)
        (unless (cog-link 'MemberLink 
                         (CapabilityNode cap)
                         port-atom)
          (MemberLink
            (CapabilityNode cap)
            port-atom)))
      capabilities)
    
    ;; Return cognitive port
    (list 'cognitive-port
          'id port-id
          'atom port-atom
          'grip grip)))

;; Helper: Convert Mach message to pointer
(define (mach-msg->pointer msg)
  ;; This would serialize the message structure
  ;; For now, simplified stub
  (bytevector->pointer 
    (make-bytevector 256))) ; Typical message size

;; Create a fully cognitive Mach port
(define (cognitive-mach-port name capabilities)
  "Create a Mach port that exists primarily in cognitive space"
  (let* ((port-atom (MachPort
                      (ConceptNode name)
                      (ConceptNode "cognitive-native")))
         (grip (grip-object port-atom)))
    
    ;; Set capabilities
    (for-each
      (lambda (cap)
        (MemberLink
          (CapabilityNode cap)
          port-atom))
      capabilities)
    
    ;; Set high attention - this is a cognitive-first port
    (cog-set-av! port-atom (av 200 0.9))
    
    ;; Create bidirectional link to track cognitive origin
    (InheritanceLink
      port-atom
      (ConceptNode "cognitive-mach-port")
      (stv 1.0 1.0))
    
    (list 'cognitive-port
          'id 'cognitive-native
          'atom port-atom
          'grip grip)))

;; Example usage:
;; (define my-port (mach-port-allocate (mach-task-self) MACH_PORT_RIGHT_RECEIVE))
;; (define cog-port (cognitive-mach-port "my-service" '("receive" "send")))

;; Export task self for convenience
(define (mach-task-self)
  ;; This would call the actual Mach function
  ;; For now, return a placeholder
  (make-pointer 1))