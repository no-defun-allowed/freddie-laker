(in-package :freddie-laker)

(defun was-pinged-p (bot text)
  (search (format nil "<@~A>" (user-id bot)) text))

(defun send-message (bot channel-id text)
  (drakma:http-request (format nil "https://discord.com/api/channels/~a/messages" channel-id)
                       :method :post
                       :additional-headers `(("Authorization" . ,(format nil "Bot ~a" (token bot))))
                       :content-type "application/json"
                       :content (jsown:to-json (obj "content" text))))
