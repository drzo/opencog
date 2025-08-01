#| This is a Guix channels specification. It indicates the revisions
of the channels that provide the package dependencies with which the
software is known to work correctly. |#

(list
 (channel
  (name 'guix)
  (url "https://codeberg.org/guix/guix.git")
  (branch "master")
  (commit
   "c099efbf59008a435b9ab306859d9229cafab21b")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
