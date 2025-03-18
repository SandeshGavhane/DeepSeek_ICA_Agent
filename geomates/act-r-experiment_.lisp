;;;
;;; ACT-R interface for geomates
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
          (setf *gstream* (sb-bsd-sockets:socket-make-stream *socket* :input t :output t :element-type 'character)))
      (error (e)
	(format *error-output* "Error: Failed to connect - ~a~%" e)
	(setf *socket* nil *gstream* nil)))))

;; sending messages to the other agent
;; messages need to be s-expressions or anything 'read'able by the lisp parser (lists, numbers, strings, etc.)
;; sticking to a well-defined language such as KQML/KIF is a good idea
(defun send-message (msg)
  "sends a message (anyting printable, but should be an s-expression)"
  (format *gstream* "~w~a~a" msg #\Return #\Newline)
  (finish-output *gstream*))

;; function to be called by ACT-R to handle key presses by the model
;; keypress is send to gameserver and updated scene is read back and inserted into visicon
(defun respond-to-key-press (model key)
  "forward key presses to game server and update visual buffer"
  (declare (optimize (safety 3) (debug 3)) (ignore model))
  ;; send data
  (ensure-connection)
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
      (loop for (what . attributes) in updated-scene do
	(case what
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
               color red regular (true nil) sides (nil 4)))))))))) 

(defun geomates-experiment (&optional human)
  (declare (optimize (debug 3) (safety 3)))
  ; Reset the ACT-R system and any models that are defined to
  ; their initial states.

  (reset)
  
  ; Create variable for the items needed to run the exeperiment:
  ;   items - a randomized list of letter strings which is randomized
  ;           using the ACT-R function permute_list
  ;   target - the first string from the randomized list which will be the
  ;            one that is different in the display
  ;   foil   - the second item from the list which will be displayed 
  ;            twice
  ;   window - the ACT-R window device list returned by using the ACT-R
  ;            function open-exp-window to create a new window for 
  ;            displaying the experiment 
  ;   text# - three text items that will hold the letters to be 
  ;           displayed all initialized to the foil letter to start
  ;   index - a random value from 0-2 generated from the act-r-random
  ;           function which is used to determine which of the three
  ;           text variables will be set to the target
 
  (let* ((window (open-exp-window "Geomates")))
    (ensure-connection)
    (add-text-to-exp-window window
			    (if (output-stream-p *gstream*)
				"connected"
				"NO CONNECTION!")
			    :x 100 :y 100)

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
        (run 30 t)))
    
	;; clean-up: remove hooks
	(remove-act-r-command-monitor "output-key" "geomates-key-press")
	(remove-act-r-command "geomates-key-press")

	t))
    

