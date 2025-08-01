#lang racket

;; T1-1 Field of Perception as Social Marketplace
;; Computational implementation of social relevance realization through embodied perception

(require racket/flonum)
(require math/matrix)
(require math/statistics)
(require ffi/unsafe)
(require racket/async-channel)

;; =============================================================================
;; FORMAL SPECIFICATION OF SOCIAL PERCEPTION FIELD
;; =============================================================================

;; Mathematical Formalization:
;; SocialPerception: Agent × Environment × Others → RelevanceField
;; SP(a,e,O) = ⟨Host, CK, EK, R, F, Proj⟩
;; where:
;;   Host ∈ {SubjectiveMode, ObjectiveMode}
;;   CK: ConsciousKnowledge (propositional social models)
;;   EK: EmotionalKnowledge (limbic social valence)
;;   R: Routines (embodied social patterns)
;;   F: Form (physical presence in social space)
;;   Proj: P₁₂₃₄ → MotorOutput (social action projections)

(define-struct social-perception-field
  ([host-state procedure?]                    ; Subjective/objective mode switcher
   [conscious-knowledge hash?]                ; Social schemas and models
   [emotional-knowledge procedure?]           ; Limbic social evaluation
   [routines vector?]                        ; Embodied social patterns
   [physical-form matrix?]                   ; Body schema in social space
   [projections (vectorof procedure?)]       ; P1-P4 motor projections
   [perceptual-axis complex?]               ; Subjective-objective balance
   [marketplace-context hash?]               ; Current social environment
   [gamma-motor-state vector?]              ; Proprioceptive preparation
   [void-awareness flonum?]))               ; Emptiness/potential awareness

;; =============================================================================
;; HOST STATE: SUBJECTIVE-OBJECTIVE MODAL SWITCHING
;; =============================================================================

(define (create-host-state-processor)
  "Implements the Host(1) modal perception switching between subjective and objective"
  
  ;; Modal state representation
  (define-struct perceptual-mode
    ([subjective-weight flonum?]      ; 0.0-1.0 subjective dominance
     [objective-weight flonum?]       ; 0.0-1.0 objective dominance
     [transition-rate flonum?]        ; Mode switching speed
     [limbic-coupling flonum?]        ; Emotional influence on mode
     [void-resonance flonum?]))       ; Connection to emptiness
  
  ;; Perceptual axis dynamics
  (define (compute-perceptual-axis emotional-state social-context)
    (let* ([emotional-valence (extract-social-valence emotional-state)]
           [social-pressure (compute-social-pressure social-context)]
           
           ;; Subjective pull from internal states
           [subjective-pull (* emotional-valence 
                             (hash-ref social-context 'personal-relevance 0.5))]
           
           ;; Objective pull from social demands
           [objective-pull (* social-pressure
                            (hash-ref social-context 'normative-force 0.5))]
           
           ;; Complex representation on perceptual axis
           [axis-position (make-rectangular objective-pull subjective-pull)]
           
           ;; Compute modal weights from axis position
           [total-magnitude (magnitude axis-position)]
           [subjective-weight (/ (imag-part axis-position) 
                               (+ total-magnitude 0.001))]
           [objective-weight (/ (real-part axis-position)
                              (+ total-magnitude 0.001))])
      
      (perceptual-mode subjective-weight
                      objective-weight
                      0.1  ; transition rate
                      emotional-valence
                      (compute-void-resonance social-context))))
  
  ;; Mode-dependent perception filter
  (define (apply-modal-perception mode sensory-input)
    (let* ([subj-weight (perceptual-mode-subjective-weight mode)]
           [obj-weight (perceptual-mode-objective-weight mode)]
           
           ;; Subjective filtering emphasizes personal relevance
           [subjective-filtered (filter-by-personal-relevance sensory-input)]
           
           ;; Objective filtering emphasizes social norms
           [objective-filtered (filter-by-social-norms sensory-input)]
           
           ;; Weighted combination
           [combined (matrix+ (matrix-scale subjective-filtered subj-weight)
                            (matrix-scale objective-filtered obj-weight))])
      
      ;; Add void awareness modulation
      (modulate-by-void-awareness combined 
                                (perceptual-mode-void-resonance mode))))
  
  (lambda (emotional-state social-context sensory-input)
    (let* ([current-mode (compute-perceptual-axis emotional-state social-context)]
           [filtered-perception (apply-modal-perception current-mode sensory-input)])
      (hash 'mode current-mode
            'filtered-perception filtered-perception
            'axis-position (make-rectangular 
                          (perceptual-mode-objective-weight current-mode)
                          (perceptual-mode-subjective-weight current-mode))))))

;; =============================================================================
;; CONSCIOUS KNOWLEDGE: SOCIAL SCHEMA PROCESSING
;; =============================================================================

(define (create-conscious-knowledge-processor)
  "Implements CK(2) - propositional knowledge of social marketplace"
  
  ;; Social schema representation
  (define-struct social-schema
    ([id symbol?]
     [trigger-pattern vector?]         ; Activation conditions
     [behavioral-script procedure?]    ; Expected behaviors
     [role-definitions hash?]         ; Social roles involved
     [outcome-predictions vector?]    ; Anticipated results
     [confidence flonum?]             ; Schema reliability
     [cultural-grounding flonum?]))   ; Cultural specificity
  
  ;; Schema library for common social situations
  (define social-schema-library
    (hash 'marketplace-negotiation 
          (social-schema 'marketplace-negotiation
                        (vector 'buyer 'seller 'product 'price)
                        marketplace-negotiation-script
                        (hash 'buyer buyer-role 'seller seller-role)
                        (vector 'purchase 'no-deal 'negotiation)
                        0.85
                        0.7)
          
          'social-greeting
          (social-schema 'social-greeting
                        (vector 'approach 'eye-contact 'proximity)
                        social-greeting-script
                        (hash 'initiator initiator-role 'responder responder-role)
                        (vector 'acknowledged 'ignored 'escalated)
                        0.9
                        0.6)))
  
  ;; Schema activation and selection
  (define (activate-relevant-schemas perception-field social-context)
    (let* ([active-schemas '()]
           [perception-features (extract-social-features perception-field)])
      
      ;; Check each schema for activation
      (for ([(schema-id schema) (in-hash social-schema-library)])
        (let* ([activation-strength 
                (compute-schema-activation schema perception-features social-context)]
               [limbic-modulation 
                (get-limbic-modulation (hash-ref perception-field 'emotional-state))])
          
          (when (> (* activation-strength limbic-modulation) 0.5)
            (set! active-schemas 
                  (cons (cons activation-strength schema) active-schemas)))))
      
      ;; Sort by activation strength
      (sort active-schemas > #:key car)))
  
  ;; Script execution for car purchase example
  (define (marketplace-negotiation-script perception-field)
    (let* ([need-strength (hash-ref perception-field 'need-for-car 0.0)]
           [financial-capacity (hash-ref perception-field 'available-funds 0.0)]
           [social-pressure (hash-ref perception-field 'social-expectation 0.0)]
           
           ;; Compute negotiation stance
           [bargaining-power (/ financial-capacity 
                              (+ need-strength social-pressure 0.001))]
           
           ;; Generate social behavior
           [behavior-sequence
            (cond
              [(> bargaining-power 0.8) 'aggressive-negotiation]
              [(> bargaining-power 0.5) 'moderate-negotiation]
              [(> bargaining-power 0.2) 'passive-inquiry]
              [else 'window-shopping])])
      
      (hash 'behavior behavior-sequence
            'confidence bargaining-power
            'urgency need-strength)))
  
  (lambda (perception-field social-context)
    (let* ([active-schemas (activate-relevant-schemas perception-field social-context)]
           [primary-schema (if (null? active-schemas) 
                             #f 
                             (cdar active-schemas))]
           [behavioral-output (if primary-schema
                                ((social-schema-behavioral-script primary-schema) 
                                 perception-field)
                                (hash 'behavior 'observe 'confidence 0.1))])
      
      (hash 'active-schemas active-schemas
            'primary-schema primary-schema
            'behavioral-output behavioral-output
            'schema-confidence (if primary-schema
                                 (social-schema-confidence primary-schema)
                                 0.0)))))

;; =============================================================================
;; EMOTIONAL KNOWLEDGE: LIMBIC SOCIAL VALENCE
;; =============================================================================

(define (create-emotional-knowledge-processor)
  "Implements EK(3) - limbic system social-emotional processing"
  
  ;; Social emotion representation
  (define-struct social-emotion
    ([valence flonum?]          ; Positive/negative
     [arousal flonum?]          ; High/low activation
     [dominance flonum?]        ; Dominant/submissive
     [affiliation flonum?]      ; Approach/avoid
     [trust flonum?]            ; Trust/distrust
     [fairness flonum?]))       ; Fair/unfair perception
  
  ;; Limbic processing of social cues
  (define (process-limbic-social-response social-perception conscious-knowledge)
    (let* ([perceived-others (hash-ref social-perception 'detected-agents '())]
           [active-schema (hash-ref conscious-knowledge 'primary-schema #f)]
           
           ;; Compute emotional response to each agent
           [agent-emotions
            (for/list ([other perceived-others])
              (let* ([facial-expression (detect-facial-expression other)]
                     [body-language (analyze-body-language other)]
                     [vocal-prosody (extract-vocal-features other)]
                     
                     ;; Amygdala-like threat detection
                     [threat-level (compute-social-threat 
                                   facial-expression body-language vocal-prosody)]
                     
                     ;; Reward prediction
                     [reward-expectation (compute-social-reward
                                        other active-schema)]
                     
                     ;; Generate emotion
                     [emotion (social-emotion
                              (- reward-expectation threat-level)  ; valence
                              (+ threat-level reward-expectation)  ; arousal
                              (compute-dominance-differential other)
                              (compute-affiliation-tendency other)
                              (compute-trust-level other active-schema)
                              (compute-fairness-perception other))])
                
                (cons other emotion)))]
           
           ;; Aggregate emotional state
           [aggregate-emotion (aggregate-social-emotions agent-emotions)]
           
           ;; Compute behavioral tendency
           [action-tendency (compute-action-tendency aggregate-emotion)])
      
      (hash 'agent-emotions agent-emotions
            'aggregate-emotion aggregate-emotion
            'action-tendency action-tendency
            'limbic-arousal (social-emotion-arousal aggregate-emotion))))
  
  ;; Link to routines via limbic patterns
  (define (limbic-routine-coupling emotional-state)
    (let* ([arousal (social-emotion-arousal emotional-state)]
           [valence (social-emotion-valence emotional-state)]
           [dominance (social-emotion-dominance emotional-state)]
           
           ;; Map to routine activation patterns
           [routine-weights
            (vector
             (* 0.8 (+ valence (* 0.5 dominance)))    ; Approach
             (* 0.8 (- 1.0 valence))                  ; Avoid
             (* arousal dominance)                     ; Assert
             (* arousal (- 1.0 dominance))            ; Submit
             (* (abs (- valence 0.5)) 0.5))])         ; Negotiate
      
      routine-weights))
  
  (lambda (social-perception conscious-knowledge)
    (let* ([emotional-response (process-limbic-social-response 
                              social-perception conscious-knowledge)]
           [routine-coupling (limbic-routine-coupling 
                            (hash-ref emotional-response 'aggregate-emotion))])
      
      (hash 'emotional-response emotional-response
            'routine-coupling routine-coupling
            'ready-for-action (> (hash-ref emotional-response 'limbic-arousal) 0.6)))))

;; =============================================================================
;; ROUTINES: EMBODIED SOCIAL PATTERNS
;; =============================================================================

(define (create-routines-processor)
  "Implements R(4) - embodied behavioral routines for social interaction"
  
  ;; Social routine representation
  (define-struct social-routine
    ([id symbol?]
     [motor-pattern matrix?]         ; Muscle activation sequence
     [proprioceptive-map vector?]    ; Expected body feedback
     [spatial-trajectory procedure?] ; Movement through social space
     [vocal-pattern procedure?]      ; Speech/vocalization
     [gaze-pattern procedure?]       ; Eye movement sequence
     [cultural-variants hash?]))     ; Culture-specific modifications
  
  ;; Core social routines
  (define social-routine-library
    (vector
     ;; Approach routine
     (social-routine 'approach
                    (generate-approach-motor-pattern)
                    (vector 'forward-lean 'open-posture 'smile-activation)
                    approach-trajectory
                    greeting-vocalization
                    direct-gaze-pattern
                    (hash 'western moderate-approach 
                          'eastern respectful-approach))
     
     ;; Avoidance routine
     (social-routine 'avoid
                    (generate-avoidance-motor-pattern)
                    (vector 'backward-lean 'closed-posture 'neutral-face)
                    avoidance-trajectory
                    minimal-vocalization
                    averted-gaze-pattern
                    (hash 'universal avoidance-universal))
     
     ;; Negotiation routine
     (social-routine 'negotiate
                    (generate-negotiation-motor-pattern)
                    (vector 'balanced-stance 'gesture-ready 'attentive-face)
                    stationary-engaged
                    negotiation-vocalization
                    alternating-gaze-pattern
                    (hash 'marketplace haggling-specific))))
  
  ;; Routine selection based on limbic coupling
  (define (select-routine limbic-weights context)
    (let* ([max-weight (vector-argmax limbic-weights)]
           [selected-routine (vector-ref social-routine-library max-weight)]
           
           ;; Apply cultural variant if available
           [culture (hash-ref context 'cultural-context 'default)]
           [variant (hash-ref (social-routine-cultural-variants selected-routine)
                            culture
                            selected-routine)])
      
      variant))
  
  ;; Motor pattern generation
  (define (generate-approach-motor-pattern)
    ;; Simplified motor pattern for approach behavior
    (build-matrix 10 50  ; 10 muscle groups, 50 time steps
                 (λ (muscle time)
                   (* (sin (/ (* 2 pi time) 50))    ; Oscillatory component
                      (exp (- (/ (sqr (- muscle 5)) 10))))))) ; Spatial distribution
  
  (lambda (emotional-knowledge context)
    (let* ([limbic-weights (hash-ref emotional-knowledge 'routine-coupling)]
           [selected-routine (select-routine limbic-weights context)]
           [motor-output (social-routine-motor-pattern selected-routine)]
           [proprioceptive-expect (social-routine-proprioceptive-map selected-routine)])
      
      (hash 'selected-routine selected-routine
            'motor-output motor-output
            'proprioceptive-expectation proprioceptive-expect
            'execution-confidence (vector-max limbic-weights)))))

;; =============================================================================
;; PHYSICAL FORM: EMBODIED PRESENCE IN SOCIAL SPACE
;; =============================================================================

(define (create-physical-form-processor)
  "Implements F(5) - physical body presence and affordances in social space"
  
  ;; Body schema in social context
  (define-struct social-body-schema
    ([personal-space-bubble flonum?]    ; Proxemic boundary
     [postural-state vector?]          ; Current body configuration
     [facial-configuration vector?]     ; Facial muscle state
     [gesture-space matrix?]           ; Reachable gesture volume
     [vocal-apparatus-state vector?]   ; Voice production state
     [appearance-features hash?]       ; Visible characteristics
     [movement-affordances vector?]))  ; Possible movements
  
  ;; Compute social affordances from body state
  (define (compute-social-affordances body-schema environment)
    (let* ([interpersonal-distances (compute-distances-to-others environment)]
           [spatial-constraints (analyze-spatial-constraints environment)]
           
           ;; Proxemic affordances
           [proxemic-affordances
            (for/list ([distance interpersonal-distances])
              (cond
                [(< distance 0.5) 'intimate-interaction]
                [(< distance 1.2) 'personal-interaction]
                [(< distance 3.6) 'social-interaction]
                [else 'public-interaction]))]
           
           ;; Gestural affordances
           [gesture-affordances
            (compute-available-gestures 
             (social-body-schema-gesture-space body-schema)
             spatial-constraints)]
           
           ;; Vocal affordances
           [vocal-affordances
            (compute-vocal-possibilities
             (social-body-schema-vocal-apparatus-state body-schema)
             environment)])
      
      (hash 'proxemic proxemic-affordances
            'gestural gesture-affordances
            'vocal vocal-affordances)))
  
  ;; Update body schema from motor commands
  (define (update-body-schema current-schema motor-commands proprioceptive-feedback)
    (let* ([new-posture (apply-motor-commands 
                        (social-body-schema-postural-state current-schema)
                        motor-commands)]
           
           [new-facial (update-facial-configuration
                       (social-body-schema-facial-configuration current-schema)
                       (extract-facial-commands motor-commands))]
           
           [new-gesture-space (compute-new-gesture-space new-posture)]
           
           ;; Validate against proprioceptive feedback
           [posture-error (compute-proprioceptive-error 
                          new-posture proprioceptive-feedback)]
           
           ;; Correct if needed
           [corrected-posture (if (> posture-error 0.1)
                                (correct-posture new-posture proprioceptive-feedback)
                                new-posture)])
      
      (struct-copy social-body-schema current-schema
                  [postural-state corrected-posture]
                  [facial-configuration new-facial]
                  [gesture-space new-gesture-space])))
  
  (lambda (routines-output proprioceptive-feedback environment)
    (let* ([motor-commands (hash-ref routines-output 'motor-output)]
           [current-schema (initialize-body-schema)]
           [updated-schema (update-body-schema 
                          current-schema motor-commands proprioceptive-feedback)]
           [social-affordances (compute-social-affordances 
                              updated-schema environment)])
      
      (hash 'body-schema updated-schema
            'social-affordances social-affordances
            'embodiment-coherence (compute-embodiment-coherence updated-schema)))))

;; =============================================================================
;; PROJECTIONS: MOTOR OUTPUT TO SOCIAL WORLD
;; =============================================================================

(define (create-projection-system)
  "Implements P1,2,3,4 projections from perception to motor action"
  
  ;; Projection pathways
  (define-struct motor-projection
    ([id symbol?]                       ; P1, P2, P3, P4
     [source-layer symbol?]             ; Perceptual source
     [spinal-pathway symbol?]           ; Gamma motor vs alpha
     [muscle-groups vector?]            ; Target muscles
     [temporal-dynamics procedure?]     ; Activation timing
     [social-function symbol?]))        ; Social purpose
  
  ;; P1,2,3,4 projection definitions
  (define projection-set
    (vector
     ;; P1 - Proprioceptive preparation
     (motor-projection 'P1 
                      'conscious-anticipation
                      'gamma-motor
                      (vector 'postural-muscles 'facial-muscles)
                      (λ (t) (* 0.5 (+ 1 (sin (* 2 pi t)))))
                      'readiness-signaling)
     
     ;; P2 - Expressive action
     (motor-projection 'P2
                      'emotional-response  
                      'alpha-motor
                      (vector 'gesture-muscles 'vocal-muscles)
                      (λ (t) (exp (- (* 2 t))))
                      'emotional-expression)
     
     ;; P3 - Goal-directed movement
     (motor-projection 'P3
                      'routine-execution
                      'alpha-motor
                      (vector 'locomotor-muscles 'reaching-muscles)
                      (λ (t) (sigmoid (- t 0.5)))
                      'goal-achievement)
     
     ;; P4 - Social mirroring
     (motor-projection 'P4
                      'mirror-system
                      'gamma-motor
                      (vector 'imitation-muscles)
                      (λ (t) (* 0.3 (cos (* 4 pi t))))
                      'social-synchrony)))
  
  ;; Gamma motor preparation
  (define (prepare-gamma-motor-state projections proprioceptive-targets)
    (let* ([gamma-activations
            (for/vector ([proj projections]
                        #:when (eq? (motor-projection-spinal-pathway proj) 
                                   'gamma-motor))
              (let* ([muscle-groups (motor-projection-muscle-groups proj)]
                     [activation-level (compute-gamma-activation 
                                      muscle-groups proprioceptive-targets)])
                (cons muscle-groups activation-level)))]
           
           ;; Reticular formation integration
           [reticular-modulation (compute-reticular-arousal gamma-activations)]
           
           ;; Conscious arousal coupling
           [conscious-readiness (* reticular-modulation 0.7)])
      
      (hash 'gamma-state gamma-activations
            'reticular-arousal reticular-modulation
            'conscious-coupling conscious-readiness)))
  
  ;; Execute projections to generate motor output
  (define (execute-motor-projections body-schema routine-plan emotional-state)
    (let* ([active-projections (select-active-projections 
                              routine-plan emotional-state)]
           
           ;; Generate motor commands for each projection
           [motor-commands
            (for/list ([proj active-projections])
              (let* ([timing (current-relative-time)]
                     [activation ((motor-projection-temporal-dynamics proj) timing)]
                     [muscles (motor-projection-muscle-groups proj)]
                     [commands (generate-muscle-commands muscles activation)])
                commands))]
           
           ;; Combine commands
           [integrated-commands (integrate-motor-commands motor-commands)]
           
           ;; Add proprioceptive preparation
           [gamma-prep (prepare-gamma-motor-state 
                       active-projections
                       (hash-ref routine-plan 'proprioceptive-expectation))])
      
      (hash 'motor-commands integrated-commands
            'gamma-preparation gamma-prep
            'active-projections active-projections)))
  
  (lambda (body-form routines emotional-knowledge)
    (let* ([body-schema (hash-ref body-form 'body-schema)]
           [routine-plan (hash-ref routines 'selected-routine)]
           [emotional-state (hash-ref emotional-knowledge 'aggregate-emotion)]
           
           [motor-output (execute-motor-projections 
                        body-schema routine-plan emotional-state)])
      
      motor-output)))

;; =============================================================================
;; INTEGRATED SOCIAL PERCEPTION SYSTEM
;; =============================================================================

(define (create-social-marketplace-perception)
  "Creates complete T1-1 social perception field processor"
  
  ;; Initialize all components
  (define host-processor (create-host-state-processor))
  (define ck-processor (create-conscious-knowledge-processor))
  (define ek-processor (create-emotional-knowledge-processor))
  (define routines-processor (create-routines-processor))
  (define form-processor (create-physical-form-processor))
  (define projection-system (create-projection-system))
  
  ;; Main perception cycle
  (define (process-social-perception sensory-input social-context)
    (let* ([start-time (current-milliseconds)]
           
           ;; Stage 1: Host state determines perceptual mode
           [host-state (host-processor #f social-context sensory-input)]
           [filtered-perception (hash-ref host-state 'filtered-perception)]
           
           ;; Stage 2: Conscious knowledge activates schemas
           [conscious-knowledge (ck-processor filtered-perception social-context)]
           
           ;; Stage 3: Emotional evaluation via limbic system
           [emotional-knowledge (ek-processor filtered-perception conscious-knowledge)]
           
           ;; Stage 4: Select embodied routines
           [routines (routines-processor emotional-knowledge social-context)]
           
           ;; Stage 5: Update body schema and affordances
           [proprioceptive (generate-proprioceptive-feedback)]
           [physical-form (form-processor routines proprioceptive social-context)]
           
           ;; Stage 6: Generate motor projections
           [motor-output (projection-system physical-form routines emotional-knowledge)]
           
           ;; Compute integrated state
           [integrated-state
            (hash 'timestamp start-time
                  'host-state host-state
                  'conscious-knowledge conscious-knowledge
                  'emotional-knowledge emotional-knowledge
                  'selected-routines routines
                  'physical-form physical-form
                  'motor-output motor-output
                  'perceptual-mode (hash-ref host-state 'mode)
                  'social-context social-context)]
           
           ;; Measure coherence
           [system-coherence (compute-system-coherence integrated-state)])
      
      (social-perception-field
       (λ () host-state)
       (hash-ref conscious-knowledge 'active-schemas)
       (λ () emotional-knowledge)
       (hash-ref routines 'selected-routine)
       (hash-ref physical-form 'body-schema)
       (hash-ref motor-output 'active-projections)
       (hash-ref host-state 'axis-position)
       social-context
       (hash-ref motor-output 'gamma-preparation)
       (compute-void-awareness social-context))))
  
  create-social-marketplace-perception)

;; =============================================================================
;; UTILITY FUNCTIONS
;; =============================================================================

(define (compute-void-awareness context)
  "Computes awareness of emptiness/potential in social situation"
  (let* ([unmet-needs (hash-ref context 'unmet-needs 0.0)]
         [uncertainty (hash-ref context 'situation-uncertainty 0.5)]
         [openness (hash-ref context 'possibility-space 0.3)])
    (* (+ unmet-needs uncertainty) openness)))

(define (filter-by-personal-relevance input)
  "Subjective filtering based on personal relevance"
  (matrix-map (λ (x) (* x (personal-relevance-weight))) input))

(define (filter-by-social-norms input)
  "Objective filtering based on social norms"
  (matrix-map (λ (x) (* x (social-norm-weight))) input))

(define (compute-system-coherence integrated-state)
  "Measures overall coherence of social perception system"
  (let* ([modal-balance (magnitude (hash-ref 
                                   (hash-ref integrated-state 'host-state) 
                                   'axis-position))]
         [schema-confidence (hash-ref 
                           (hash-ref integrated-state 'conscious-knowledge)
                           'schema-confidence)]
         [emotional-readiness (if (hash-ref 
                                 (hash-ref integrated-state 'emotional-knowledge)
                                 'ready-for-action) 1.0 0.5)]
         [routine-confidence (hash-ref
                            (hash-ref integrated-state 'selected-routines)
                            'execution-confidence)]
         [embodiment-coherence (hash-ref
                              (hash-ref integrated-state 'physical-form)
                              'embodiment-coherence)])
    
    ;; Geometric mean of all coherence measures
    (expt (* modal-balance schema-confidence emotional-readiness 
            routine-confidence embodiment-coherence) 0.2)))

;; =============================================================================
;; EXPORT INTERFACE
;; =============================================================================

(provide create-social-marketplace-perception
         (struct-out social-perception-field)
         (struct-out perceptual-mode)
         (struct-out social-schema)
         (struct-out social-emotion)
         (struct-out social-routine)
         (struct-out social-body-schema)
         (struct-out motor-projection)
         compute-void-awareness
         compute-system-coherence)