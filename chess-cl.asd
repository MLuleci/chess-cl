;;;; Define the chess-cl system

(defsystem "chess-cl"
    :description "The game of chess written in Common Lisp."
    :version "0.0.1"
    :author "Mert Luleci <mert@luleci.net>"
    :serial t
    :components ((:file "packages")
                 (:file "term")
                 (:file "util")
                 (:file "piece")
                 (:file "player")
                 (:file "control")))
