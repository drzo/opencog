;; HurdCog Demo - See the Cognitive OS in Action!
;; This demonstrates the core concepts of OpenCog as Hurd's kernel

(use-modules (machspace core)
             (grip cognitive-grip)
             (translators cognitive-translator)
             (opencog)
             (opencog exec)
             (opencog attention))

;; Initialize the cognitive OS
(define (boot-hurdcog)
  (display "\n🚀 Booting HurdCog - The Cognitive OS...\n")
  
  ;; 1. Initialize MachSpace
  (display "\n1️⃣ Initializing MachSpace (AtomSpace as Microkernel)...\n")
  (machspace-init)
  
  ;; Show the fundamental atoms
  (display "   Core system atoms created:\n")
  (for-each
    (lambda (atom)
      (format #t "   - ~a [STI: ~a]\n" 
              atom 
              (cog-av-sti atom)))
    (list (ConceptNode "kernel")
          (ConceptNode "user")
          (ConceptNode "root")))
  
  ;; 2. Create some Mach ports as atoms
  (display "\n2️⃣ Creating Mach ports as cognitive atoms...\n")
  (define kernel-port (create-mach-port 1001 '("read" "write" "execute")))
  (define user-port (create-mach-port 2001 '("read" "write")))
  
  (format #t "   Kernel port: ~a\n" kernel-port)
  (format #t "   User port: ~a\n" user-port)
  
  ;; 3. Demonstrate the Grip mechanism
  (display "\n3️⃣ Testing the Cognitive Grip mechanism...\n")
  (display "   Creating grip on system object...\n")
  
  (define test-object (ConceptNode "test-file.txt"))
  (define my-grip (grip-object test-object))
  
  (format #t "   ✋ Grip established:\n")
  (format #t "   - Universal: ~a\n" (grip-universal my-grip))
  (format #t "   - Identity: ~a\n" (grip-identity my-grip))
  (format #t "   - Trust: ~a\n" (grip-trust my-grip))
  (format #t "   - Coherent: ~a\n" (grip-coherent? my-grip))
  
  ;; 4. Create a cognitive translator
  (display "\n4️⃣ Creating Cognitive Translator...\n")
  (define cog-fs (make-cognitive-translator "cognitive-fs"))
  (format #t "   Created translator: ~a\n" cog-fs)
  
  ;; 5. Show distributed nature
  (display "\n5️⃣ Distributing AtomSpace across shards...\n")
  (define shards (distribute-atomspace #:shards 3))
  (format #t "   Created ~a shards for distributed processing\n" 
          (length shards))
  
  ;; 6. Demonstrate problem solving
  (display "\n6️⃣ Solving a classic Hurd problem: Identity Crisis\n")
  (demonstrate-identity-persistence)
  
  ;; 7. Show attention economy
  (display "\n7️⃣ ECAN Attention Economy in action...\n")
  (demonstrate-attention-economy)
  
  (display "\n✅ HurdCog boot complete! The OS is now thinking...\n\n"))

;; Demonstrate identity persistence through transformations
(define (demonstrate-identity-persistence)
  (display "   Creating object with identity...\n")
  
  (define original (ConceptNode "mutable-object-v1"))
  (define grip1 (grip-object original))
  (format #t "   Original identity: ~a\n" (grip-identity grip1))
  
  ;; Transform the object
  (display "   Transforming object...\n")
  (define grip2 (grip-transform grip1
                               (lambda (atom)
                                 (ConceptNode "mutable-object-v2"))))
  
  (format #t "   After transform: ~a\n" (grip-universal grip2))
  (format #t "   Identity preserved: ~a\n" 
          (equal? (grip-identity grip1)
                  (grip-identity grip2)))
  
  ;; Show the inheritance link that maintains identity
  (define inheritance 
    (cog-link 'InheritanceLink
              (grip-universal grip2)
              (grip-universal grip1)))
  (format #t "   Identity link: ~a\n" inheritance))

;; Demonstrate attention-based resource allocation
(define (demonstrate-attention-economy)
  ;; Create competing processes
  (define processes
    (map (lambda (i)
           (let ((p (ConceptNode (format #f "process-~a" i))))
             (cog-set-av! p (av (random 100) 0.5))
             p))
         (iota 5)))
  
  (display "   Created 5 processes with random attention:\n")
  (for-each
    (lambda (p)
      (format #t "   - ~a [STI: ~a]\n" 
              (cog-name p)
              (cog-av-sti p)))
    processes)
  
  ;; Find highest priority
  (define highest-priority
    (car (sort processes
               (lambda (a b)
                 (> (cog-av-sti a)
                    (cog-av-sti b))))))
  
  (format #t "\n   Scheduler would run: ~a\n" 
          (cog-name highest-priority)))

;; Interactive demo functions
(define (create-cognitive-file path)
  (display "\n📄 Creating cognitive file...\n")
  (let* ((file-atom (FileSystemObject path))
         (grip (grip-object file-atom)))
    (format #t "   File '~a' created with grip\n" path)
    (format #t "   Identity: ~a\n" (grip-identity grip))
    grip))

(define (cognitive-write grip content)
  (display "\n✍️  Writing to cognitive file...\n")
  (let* ((content-atom (ConceptNode content))
         (content-grip (grip-object content-atom))
         (file-atom (grip-universal grip)))
    
    ;; Validate coherence
    (if (grip-coherent? grip)
        (begin
          (ListLink file-atom content-atom)
          (format #t "   Content written: ~a\n" content)
          #t)
        (begin
          (format #t "   ❌ Write failed: grip not coherent\n")
          #f))))

(define (cognitive-read grip)
  (display "\n📖 Reading from cognitive file...\n")
  (let* ((file-atom (grip-universal grip))
         (content-links (cog-incoming-set file-atom)))
    (if (null? content-links)
        (format #t "   File is empty\n")
        (let ((content (cadr (cog-outgoing-set (car content-links)))))
          (format #t "   Content: ~a\n" (cog-name content))))))

;; Run the demo
(display "\n╔══════════════════════════════════════════════╗")
(display "\n║       HurdCog - Where OS Meets Mind          ║")
(display "\n║   OpenCog IS the Kernel GNU Hurd Needs!      ║")
(display "\n╚══════════════════════════════════════════════╝\n")

(boot-hurdcog)

;; Interactive example
(display "\n🎮 Try it yourself:\n")
(display "   (define my-file (create-cognitive-file \"test.txt\"))\n")
(display "   (cognitive-write my-file \"Hello, Cognitive OS!\")\n")
(display "   (cognitive-read my-file)\n")
(display "\n💡 The OS is now thinking! Every operation uses:\n")
(display "   - AtomSpace for universal representation\n")
(display "   - Hypergraph signatures for persistent identity\n")
(display "   - PLN for coherence checking\n")
(display "   - Capabilities for trust\n")
(display "   - ECAN for resource allocation\n")
(display "\n🤝 Man and Machine united through cognitive architecture!\n")