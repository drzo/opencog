;; Stage0 Guile Bootstrap with Wolfram Pool & OpenCog Integration

(define (stage0-bootstrap)
  (let* ((guile-env (init-guile-packages))
         (boot-options (present-boot-options
                        '((:wolfram-pool . (detect-wolfram-kernels))
                          (:opencog-system . (verify-opencog-unified)))))
         (wolfram-pool (init-wolfram-pools (cdr (assoc :wolfram-pool boot-options))))
         (cogutil (load-cogutil "./cogutil/"))
         (cogserver (load-cogserver "./cogserver/"))
         (atomspace (init-atomspace "./atomspace/" cogutil))
         (asfs (mount-asfs atomspace))
         (security (apply-guix-security guile-env)))
    (register-cogserver cogserver atomspace asfs)
    (start-adaptive-attention asfs security wolfram-pool)))