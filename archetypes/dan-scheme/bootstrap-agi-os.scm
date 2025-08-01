;; Stage0 Guile Bootstrap
(define (bootstrap-agi-os)
  (let* ((scheme-env (initialize-guile-stage0))
         (wolfram-pool (init-wolfram-kernel-pools scheme-env))
         (cogutil (load-cogutil scheme-env))
         (atomspace (init-atomspace scheme-env cogutil))
         (asfs (mount-atomspace-filesystem atomspace)))
    (register-cogserver scheme-env cogutil atomspace asfs)
    (start-recursive-adaptive-attention asfs wolfram-pool)))