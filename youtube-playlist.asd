;;;; youtube-playlist

(asdf:defsystem #:youtube-playlist
  :description "Simple youtube playlist"
  :author "Masaya Tojo"
  :license  "BSD"
  :version "0.0.0"
  :serial t
  :depends-on (#:clog #:dexador #:plump #:clss)
  :components ((:file "youtube-playlist")))
