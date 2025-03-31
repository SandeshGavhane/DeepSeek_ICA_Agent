;;;
;;; ACT-R interface for geomates - FINAL PATCH VERSION
;;;

;; in ACT-R, one would directly load the model (agent)
; (load-act-r-model "ACT-R:my-great-model.lisp")

     
#-SBCL (eval-when (:compile :load-toplevel)
	 (error "Sorry, this experiment requires ACT-R based on the SBCL compiler; install SBCL and load ACT-R"))

#+SBCL (eval-when (:compile-toplevel :load-toplevel)
	 (require :sb-bsd-sockets))

(defvar *socket* nil
  "TCP/IP socket for talking to the game")

(defparameter *gstream* nil
  "stream for interfacing the game which uses *socket* for communication")

(defparameter *geomates-host* #(127 0 0 1)
  "host on which the game server is running")
(defparameter *geomates-port* 45678
  "TCP port on which the game server is listening for agent connections")

;; Add a variable to track which player we're controlling
(defparameter *player-type* nil
  "Type of player we're controlling (disc or rect)")

(defparameter *message*  ()
  "variable that holds latest message received from other agent")

;; Function to skip the telnet protocol bytes
(defun skip-telnet-bytes (socket)
  "Skip the telnet control bytes on the socket"
  (format t "Skipping telnet control bytes...~%")
  (let ((buffer (make-array 6 :element-type '(unsigned-byte 8))))
    (handler-case
        (progn
          ;; Read 6 bytes directly from the socket
          (sb-bsd-sockets:socket-receive socket buffer nil)
          (format t "Skipped telnet bytes: ~A~%" buffer))
      (error (e)
        (format t "Error skipping telnet bytes: ~A~%" e)))))

;; Function to read the player type message
(defun read-player-type (socket)
  "Read the player type message from the socket"
  (let ((buffer (make-array 512 :element-type '(unsigned-byte 8) :initial-element 0)))
    (handler-case
        (progn
          ;; Read the player type message from the socket
          (let ((bytes-read (sb-bsd-sockets:socket-receive socket buffer nil)))
            (when bytes-read
              ;; Find the actual message length (look for CR+LF)
              (let ((message-end (or (position 10 buffer :start 1 :from-end nil) bytes-read)))
                ;; Convert the bytes to a string (excluding any trailing nulls)
                (let ((message (sb-ext:octets-to-string 
                                (subseq buffer 0 message-end)
                                :external-format :utf-8)))
                  (format t "Received message: ~A~%" message)
                  ;; Extract player type
                  (let ((data (ignore-errors (read-from-string message))))
                    (when (and (listp data) (eq (car data) :playing))
                      (setf *player-type* (cadr data))
                      (format t "We are controlling the ~A player~%" *player-type*))))))))
      (error (e)
        (format t "Error reading player type: ~A~%" e)))))

(defun ensure-connection ()
  "(re)establishes the socket connection to the server"
  (declare (optimize (safety 3) (debug 3)))
  (unless (and *gstream* (open-stream-p *gstream*))
    (format t "Connecting...~%")
    (unless *socket*
      (setf *socket* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)
	    (sb-bsd-sockets:sockopt-tcp-nodelay *socket*)
	    (sb-bsd-sockets:sockopt-reuse-address *socket*)))
    (handler-case
	(progn
          (sb-bsd-sockets:socket-connect *socket* *geomates-host* *geomates-port*)
          (format t "Socket connected~%")
          
          ;; Skip telnet protocol bytes
          (skip-telnet-bytes *socket*)
          
          ;; Read player type message
          (read-player-type *socket*)
          
          ;; Create the character stream for further communication
          (setf *gstream* (sb-bsd-sockets:socket-make-stream 
                          *socket* 
                          :input t 
                          :output t 
                          :element-type 'character)))
      (error (e)
	(format *error-output* "Error: Failed to connect - ~a~%" e)
	(setf *socket* nil *gstream* nil)))))

;; sending messages to the other agent
;; messages need to be s-expressions or anything 'read'able by the lisp parser (lists, numbers, strings, etc.)
;; sticking to a well-defined language such as KQML/KIF is a good idea
(defun send-message (msg)
  "sends a message (anything printable, but should be an s-expression)"
  (ensure-connection)
  (when *gstream*
    (format t "Raw message received: ~s~%" msg)  ; ~s will print it with readability preserved
    ;; First send the 'm' character (ASCII 109) which is the message command
    (write-char #\m *gstream*)
    ;; Then write the message itself in a way that can be read by the Lisp reader
    (write msg :stream *gstream* :readably t :pretty nil)
    ;; Ensure the message is sent immediately
    (finish-output *gstream*)))

;
;; function to be called by ACT-R to handle key presses by the model
;; keypress is send to gameserver and updated scene is read back and inserted into visicon
(defun respond-to-key-press (model key)
  "forward key presses to game server and update visual buffer"
  (declare (optimize (safety 3) (debug 3)) (ignore model))
  ;; send data
  (ensure-connection)
  (when *gstream*
    (clear-input *gstream*)
    (format *gstream* key)
    (finish-output *gstream*)
    ;; read updated scene
    (multiple-value-bind (updated-scene err) (ignore-errors (read *gstream* nil nil nil))
      (when err
        (format *error-output* "~&error reading from game server (err: ~w).~%" err))
      (when (consp updated-scene)
        (format *standard-output* "~&installing scene in visicon, scene: ~w~%" updated-scene)

        (delete-all-visicon-features) ; reset visicon
        
        ;; Add player-type information to visicon first so it's always available
        (when *player-type*
          (add-visicon-features `(isa (polygon-feature polygon)
                                   screen-x 0
                                   screen-y 0
                                   value (polygon "player-info")
                                   player-type ,*player-type*)))
        
        ;; Process each element in the scene data safely
        (loop for item in updated-scene do
          (cond
            ;; Skip processing for string elements (empty messages)
            ((stringp item)
             (format *standard-output* "~&Skipping empty message string: ~S~%" item))
            
            ;; Process normal list elements
            ((listp item)
             (let ((what (first item))
                   (attributes (rest item)))
               (case what
                 (:msg->rect 
                  (setf *message* attributes)
                  (when (and *message* (listp *message*))
                    (add-visicon-features `(isa (text-feature text)
                                             screen-x 10
                                             screen-y 10
                                             value (text "message")
                                             message ,*message*
                                             sender "disc"))))
                
                 (:msg->disc 
                  (setf *message* attributes)
                  (when (and *message* (listp *message*))
                    (add-visicon-features `(isa (text-feature text)
                                             screen-x 10
                                             screen-y 10
                                             value (text "message")
                                             message ,*message*
                                             sender "rect"))))
                
                 (:platform (destructuring-bind (x1 y1 x2 y2) attributes
                              (add-visicon-features `(isa (polygon-feature polygon)
                                                   screen-x ,(* 0.5 (+ x1 x2))
                                                   screen-y ,(* 0.5 (+ y1 y2))
                                                   value (polygon "platform")
                                                   height ,(abs (- y2 y1))
                                                   width  ,(abs (- x2 x1))
                                                   color black regular (true nil) sides (nil 4)))))
                
                 (:diamond (destructuring-bind (x y) attributes
                             (add-visicon-features `(isa (polygon-feature polygon) 
                                                 screen-x ,x screen-y ,y
                                                 value (polygon "diamond")))))
                
                 (:disc (destructuring-bind (x y radius diamonds) attributes
                          (add-visicon-features `(isa oval
                                               screen-x ,x
                                               screen-y ,y
                                               value (oval "disc")
                                               radius ,radius
                                               diamonds ,diamonds))))
                
                 (:rect (destructuring-bind (x y width height rotation diamonds) attributes
                          (add-visicon-features 
                           `(isa (polygon-feature polygon)
                             screen-x ,x
                             screen-y ,y
                             value (polygon "rect")
                             height ,height
                             width  ,width
                             rotation ,rotation
                             diamonds ,diamonds
                             color red regular (true nil) sides (nil 4))))))))))))))
            

(defun geomates-experiment (&optional human)
  (declare (optimize (debug 3) (safety 3)))
  ; Reset the ACT-R system and any models that are defined to
  ; their initial states.

  (reset)
  
  (let* ((window (open-exp-window "Geomates")))
    (ensure-connection)
    (add-text-to-exp-window window
			    (if (and *gstream* (open-stream-p *gstream*))
				"connected"
				"NO CONNECTION!")
			    :x 100 :y 100)

    ;; Display player type if known
    (when *player-type*
      (add-text-to-exp-window window
                             (format nil "Controlling ~a player" *player-type*)
                             :x 100 :y 130))

    ;; Create a command in ACT-R that corresponds to our respond-to-key-press
    ;; function so that ACT-R is able to use the function. Then install hook
    ;; for monitoring key presses.
    
    (add-act-r-command "geomates-key-press" 'respond-to-key-press 
                       "Assignment 2 task output-key monitor")
    (monitor-act-r-command "output-key" "geomates-key-press")

    ;; run the model!
    (if human
	(add-text-to-exp-window window "human not allowed here, use telnet" :x 100 :y 70)
	(progn
					; If it is not a human then use install-device so that
					; the features in the window will be seen by the model
					; (that will also automatically provide the model with
					; access to a virtual keyboard and mouse).  Then use
					; the ACT-R run function to run the model for up to 10
					; seconds in real-time mode.
        
        (install-device window)
        
        ;; Wait for second player to connect if needed
        (format t "Running the model - will wait for game to initialize...~%")
        (run 600 t)))
    
	;; clean-up: remove hooks
	(remove-act-r-command-monitor "output-key" "geomates-key-press")
	(remove-act-r-command "geomates-key-press")

	t))