;;;; youtube-local-playlist

(asdf:defsystem #:youtube-local-playlist
  :description "Simple youtube playlist"
  :author "Masaya Tojo"
  :license  "BSD"
  :version "0.0.0"
  :class :package-inferred-system
  :depends-on (#:clog
               #:dexador
               #:plump
               #:clss
               #:youtube-local-playlist/youtube-local-playlist))
