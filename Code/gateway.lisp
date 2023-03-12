(in-package :freddie-laker)

(alexandria:define-constant +gateway-api-endpoint+ "https://discord.com/api/gateway" :test #'string=)
(alexandria:define-constant +operating-system+ "There shouldn't be one." :test #'string=)
(alexandria:define-constant +library-name+ "Freddie Laker" :test #'string=)

(defvar *die-hard* nil)

(defun call-with-exponential-backoff (function &key (start 2) (end 30))
  (when *die-hard*
    (return-from call-with-exponential-backoff (funcall function)))
  (loop for n = start then (min end (* n 2))
        do (handler-case
               (funcall function)
             (error (e)
               (print e)
               (sleep n)
               (setf n (* n 2)))
             (:no-error (&rest r)
               (return (values-list r))))))
(defmacro with-exponential-backoff (() &body body)
  `(call-with-exponential-backoff (lambda () ,@body)))

(defun gateway-url ()
  (with-exponential-backoff ()
   (let ((drakma:*text-content-types* '(("application" . "json"))))
     (jsown:val
      (jsown:parse
       (drakma:http-request +gateway-api-endpoint+))
      "url"))))

(defun start (bot)
  (unless (running-p bot)
    (setf (running-p bot) t)
    (connect bot)))

(defun connect (bot)
  (bt:with-lock-held ((connection-lock bot))
    ;; Don't try to make another connection if someone else did.
    (when (null (connection bot))
      (with-exponential-backoff ()
        (let ((client (wsd:make-client (gateway-url))))
          (setf (connection bot) client
                (connection-state bot) (make-instance 'connection-state))
          (wsd:on :message client
                  (lambda (m) (handle-message bot m)))
          (wsd:on :close client
                  (lambda (&key code reason)
                    (handle-close bot code reason)))
          (wsd:start-connection client)
          (send-actor (watchdog-actor bot) :start))))))

(defun stop (bot)
  (setf (running-p bot) nil)
  (disconnect bot))

(defun disconnect (bot)
  (bt:with-lock-held ((connection-lock bot))
    (wsd:close-connection (connection bot))
    (send-actor (heartbeat-actor bot) :stop)))

(defun handle-close (bot code reason)
  (bt:with-lock-held ((connection-lock bot))
    (send-actor (heartbeat-actor bot) :stop)
    (setf (connection bot) nil))
  (when (running-p bot)
    (format *debug-io* "~a disconnected with code=~s reason=~s~%"
            bot code reason)
    (send-actor (defer-actor bot)
                (lambda ()
                  (sleep 10)
                  (connect bot)))))
    
(defun reconnect (bot &key because)
  (format *debug-io* "~a Reconnecting ~a.~%"
          because bot)
  (disconnect bot))

;;; IO basics
(defun %send (bot opcode data)
  (let ((text (jsown:to-json
               `(:obj
                  ("op" . ,opcode)
                  ("d" . ,data)))))
    (write-line text *debug-io*)
    (wsd:send (connection bot) text)))
(defmacro obj (&body data)
  ``(:obj ,,@(loop for (key value) on data by #'cddr
                   collect `(cons ,(string-downcase key) ,value))))
(defmacro send (bot opcode &body data)
  `(%send ,bot ,opcode (obj ,@data)))
(defun bits (&rest ns)
  (loop for n in ns sum (ash 1 n)))

;;; Messages to send
(defun identify (bot)
  (send bot 2
    token (token bot)
    properties (obj
                 os +operating-system+
                 browser +library-name+
                 device +library-name+)
    intents (bits 9 12)))

(defun heartbeat (bot)
  (%send bot 1 (last-sequence (connection-state bot))))
    
;;; Receiving messages
(defgeneric handle-message-by-op (bot op data type)
  (:method (bot op data type)
    (warn "Unknown opcode ~d sent to ~s" bot op)))

(defvar *type-handlers* (make-hash-table :test 'equal))
(defmacro define-type-handler (name (bot data) &body body)
  `(setf (gethash ,(string-upcase name) *type-handlers*)
         (lambda (,bot ,data) ,@body)))
    
(defun handle-message (bot m)
  (let* ((text (jsown:parse m))
         (s (jsown:val-safe text "s")))
    (when (integerp s)
      (setf (last-sequence (connection-state bot)) s))
    (handle-message-by-op bot
                          (jsown:val text "op")
                          (jsown:val text "d")
                          (jsown:val text "t"))))

(define-type-handler ready (bot d)
  (send-actor (watchdog-actor bot) :ok)
  (setf (finished-handshake-p (connection-state bot)) t
        (user-id bot) (jsown:val (jsown:val d "user") "id")))

(define-type-handler message_create (bot d)
  (funcall (on-message bot) bot d))

(defmethod handle-message-by-op (bot (message (eql 0)) data type)
  (let ((handler (gethash type *type-handlers*)))
    (block out
      (handler-bind ((error
                       (lambda (e)
                         (trivial-backtrace:print-backtrace e)
                         (return-from out))))
        (unless (null handler)
          (funcall handler bot data))))))

(defmethod handle-message-by-op (bot (reconnect (eql 7)) data type)
  (reconnect bot :because "Discord asked us to reconnect."))

(defmethod handle-message-by-op (bot (invalid-session (eql 9)) data type)
  (reconnect bot :because "Session invalidated by Discord."))

(defmethod handle-message-by-op (bot (hello (eql 10)) data type)
  (setf (heartbeat-interval (connection-state bot)) (jsown:val data "heartbeat_interval"))
  (send-actor (heartbeat-actor bot) :start))

(defmethod handle-message-by-op (bot (heartbeat-ack (eql 11)) data type)
  (setf (received-heartbeat-p (connection-state bot)) t)
  (unless (finished-handshake-p (connection-state bot))
    (identify bot))) 

;;; Actor behaviours
(defun heartbeat-action (actor)
  (tagbody
   idle
     (receive (actor)
       ((:start)
        (go heartbeat))
       (otherwise (go idle)))
   heartbeat
     (handler-bind ((error
                      (lambda (e)
                        (trivial-backtrace:print-backtrace e)
                        (reconnect (bot actor) :because "Failed to send heartbeat.")
                        (go idle))))
         (heartbeat (bot actor)))
     (let ((cs (connection-state (bot actor))))
       (receive (actor :timeout (/ (heartbeat-interval cs) 1000))
         ((nil) ; Timeout expired, check and send heartbeat.
          (cond
            ((received-heartbeat-p cs)
             (setf (received-heartbeat-p cs) nil)
             (go heartbeat))
            (t
             (reconnect (bot actor) :because "Didn't receive a heartbeat.")
             (go idle))))
         ((:stop) (go idle))
         (otherwise (go heartbeat))))))
  
(defun watchdog-action (actor)
  (tagbody
   idle
     (receive (actor)
       ((:start)
        (go watchdog))
       (otherwise (go idle)))
   watchdog
     (receive (actor :timeout 20)
       ((:ok)
        (write-line "Finished handshaking." *debug-io*)
        (go idle))
       ((nil) ; Timeout expired.
        (reconnect (bot actor) :because "Didn't finish handshaking."))
       (otherwise (go watchdog)))))

(defun defer-action (actor)
  (loop
    (funcall (safe-queue:mailbox-receive-message (mailbox actor)))))
