#| GNU Guix manifest

This file is a GNU Guix manifest file. It can be used with GNU Guix to
create a profile or an environment to work on the project. |#

(use-modules (gnu packages))


(define DEV_PACKAGES
  (list "guile-proba"))

(define PRODUCTION_PACKAGES
  (list "guile"
        "guile-json"
        "artanis"))

(specifications->manifest
 (append DEV_PACKAGES
         PRODUCTION_PACKAGES))
