(use-modules (opencog))

;-------------------------------------------------------------------------------

(define (get-word-list sent-node)
    (List (append-map
        (lambda (w)
            ; Ignore LEFT-WALL and punctuations
            (if (or (string-prefix? "LEFT-WALL" (cog-name w))
                    (word-inst-match-pos? w "punctuation"))
                '()
                (cog-chase-link 'ReferenceLink 'WordNode w)))
        (car (sent-get-words-in-order sent-node))))
)

(define-public (get-input-word-list)
    (gar (cog-execute! (Get (TypedVariable (Variable "$x") (Type "ListLink"))
        (State input-utterance-words (Variable "$x")))))
)

(define-public (get-input-sent-node)
    (gar (cog-execute! (Get (TypedVariable (Variable "$x") (Type "SentenceNode"))
        (State input-utterance-sentence (Variable "$x")))))
)

(define-public (get-input-text-node)
    (gar (cog-execute! (Get (TypedVariable (Variable "$x") (Type "Node"))
        (State input-utterance-text (Variable "$x")))))
)

; This is generated by 'nlp-parse'
(define-public (get-input-time)
    (cog-name (gar (cog-execute! (Get
        (TypedVariable (Variable "$t") (Type "TimeNode"))
            (AtTime (Variable "$t") (get-input-sent-node)
                (Dialog "Dialogue-System"))))))
)

;-------------------------------------------------------------------------------

(define sentiment-analysis #f)
(define-public (do-sentiment-analysis val)
    (set! sentiment-analysis val)
)
