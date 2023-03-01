(asdf:defsystem :freddie-laker
  :depends-on (:bordeaux-threads :safe-queue
               :drakma :websocket-driver :jsown
               :trivial-backtrace)
  :serial t
  :components ((:file "package")
               (:file "bot")
               (:file "gateway")
               (:file "send")))
