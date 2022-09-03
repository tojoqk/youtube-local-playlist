;;;; youtube-local-playlist

(asdf:defsystem #:youtube-local-playlist
  :description "Simple youtube playlist"
  :author "Masaya Tojo"
  :license  "BSD"
  :version "0.0.0"
  :serial t
  :depends-on (#:clog #:dexador #:plump #:clss)
  :components ((:file "youtube-local-playlist")
               (:file "playlist")
               (:file "item")))
