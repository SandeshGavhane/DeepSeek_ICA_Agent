;;;
;;; Complete ACT-R model for Geomates game
;;; With reliable player detection and message testing
;;;  

(clear-all)

;;; in the agent, arbitrary helper functions may be defined
;;; using Common Lisp, but also all add-ons of the SBCL lisp
;;; system, in particular loading shared libraries and calling
;;; functions in those libraries.
;;; For details, see SBCL manual regarding its alien function interace
;;; or have a look into geomates.lisp which connects to a C library
;;;
;;; Additionally, you can use run-program to call any external software.
;;; Note that the process will be run in a null environment by default, so
;;; all pathnames must be explicit. To handle different locations, a simple
;;; "or" may be all it takes:

(defparameter *my-ls* (or (probe-file "/bin/ls")
                        (probe-file "/usr/bin/ls")
                        (probe-file "some/path"))
  "binds to the first file that exists")

(defun count-entries ()
  "counts the number of files/directories in the root directory"
  (count #\Newline ; just count linebreaks since after printing a name, ls prints a newline
         (with-output-to-string (result) ; temporary string output stream
           (run-program (probe-file "/bin/ls") (list "/") :output result))))

;;;
;;; ACT-R model for Geomates game - Player Detection and Message Testing
;;;  

;;; Define the model
(define-model geomates-communication-test

;;; Define chunk types for game objects and communication
(chunk-type (polygon-feature (:include visual-location)) regular)
(chunk-type (polygon (:include visual-object)) sides height width rotation diamonds player-type)
(chunk-type (text-feature (:include visual-location)))
(chunk-type (text (:include visual-object)) message sender)
(chunk-type goal state)
(chunk-type time ticks)
(chunk-type player-info type)
(chunk-type outgoing-message content)
(chunk-type message-info content sender response-required)

;;; Do this to avoid warnings when chunks are created
(define-chunks true false polygon)

;;; Set vision module to look for the lowest (nearest) screen-x item first
(set-visloc-default screen-x lowest)

;;; Add declarative memory chunks
(add-dm
 ;; Keyboard controls
 (w) (a) (s) (d) (m)
 
 ;; Goal states
 (init-visicon)
 (waiting-for-update-1)
 (press-for-second-update)
 (waiting-for-update-2)
 (visicon-ready)
 (finding-diamond)
 (planning)
 (reading-player-type)
 (player-identified)
 
 ;; Message testing states
 (msg-start)
 (msg-key-sent)
 (msg-content-sent)
 (msg-waiting)
 
 ;; Predefined messages
 (msg-1 isa outgoing-message content (:hello :i-am "testing communication"))
 (msg-2 isa outgoing-message content (:status :position (:x 25 :y 30) :player-type "unknown")) 
 (msg-3 isa outgoing-message content (:ready-to-collaborate :task "diamond collection"))
 
 ;; Initial goal
 (first-goal isa goal state init-visicon)
)

;;; Set initial goal
(goal-focus first-goal)

;;;
;;; VISICON INITIALIZATION PHASE
;;;

;;; First press 'd' key to trigger visicon update
(p init-visicon-press-d
   =goal>
      state init-visicon
   ?manual>
      state free
==>
   !eval! (format t "~%Starting visicon update: pressing 'd'~%")
   +manual>
      cmd press-key
      key "d"
   =goal>
      state waiting-for-update-1
)

;;; Wait for first update (approx 1 second)
(p wait-for-first-update
   =goal>
      state waiting-for-update-1
   ?manual>
      state free
==>
   +temporal>
      isa time
      ticks 1.0  ; Wait 1 second for visicon to update
   =goal>
      state press-for-second-update
)

;;; Press 'a' key to trigger second visicon update 
(p press-for-second-update
   =goal>
      state press-for-second-update
   ?temporal>
      buffer requested
==>
   !eval! (format t "~%Second visicon update: pressing 'a'~%")
   +manual>
      cmd press-key
      key "a"
   =goal>
      state waiting-for-update-2
)

;;; Wait for second update
(p wait-for-second-update
   =goal>
      state waiting-for-update-2
   ?manual>
      state free
==>
   +temporal>
      isa time
      ticks 1.0  ; Wait another second
   =goal>
      state visicon-ready
)

;;; Transition to player detection phase
(p visicon-ready-transition
   =goal>
      state visicon-ready
   ?temporal>
      buffer requested
==>
   !eval! (format t "~%Visicon update complete - transitioning to player detection~%")
   =goal>
      state finding-diamond  ; Changed to finding-diamond for player detection
)

;;;
;;; PLAYER DETECTION PHASE
;;;

;;; Look for "player-info" in the visicon
(p detect-player-info
   =goal>
      state finding-diamond
   ?visual-location>
      state free
==>
   !eval! (format t "~%Looking for player-info in visicon~%")
   +visual-location>
      isa visual-location
      value "player-info"    ; Look for the player-info value
   =goal>
      state planning
)

;;; Process the player info once found
(p process-player-info
   =goal>
      state planning
   =visual-location>
      isa visual-location
   ?visual>
      state free
==>
   !eval! (format t "~%Found player-info, attending to it~%")
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state reading-player-type
)

;;; Extract and store the player type
(p extract-player-type
   =goal>
      state reading-player-type
   =visual>
      isa polygon
      player-type =type    ; Get the player-type slot value
   ?imaginal>
      state free
==>
   !eval! (format t "~%Player type identified as: ~S~%" =type)
   +imaginal>
      isa player-info
      type =type           ; Store the player type
   =goal>
      state player-identified
)

;;; Alternative search for player using "rect" value if player-info search fails
(p detect-rect-player
   =goal>
      state finding-diamond
   ?visual-location>
      state error
==>
   !eval! (format t "~%No player-info found, looking for rect player~%")
   +visual-location>
      isa visual-location
      value "rect"         ; Try looking for "rect" directly
   =goal>
      state planning
)

;;; Alternative search for player using "disc" value if rect search fails
(p detect-disc-player
   =goal>
      state planning
   ?visual-location>
      state error
==>
   !eval! (format t "~%No rect player found, looking for disc player~%")
   +visual-location>
      isa visual-location
      value "disc"         ; Try looking for "disc" directly
   =goal>
      state planning
)

;;; Fallback if player detection completely fails
(p player-detection-fallback
   =goal>
      state planning
   ?visual-location>
      state error
==>
   !eval! (format t "~%Failed to detect player by any method, continuing with message testing anyway~%")
   =goal>
      state player-identified
)

;;;
;;; SIMPLIFIED MESSAGE TESTING PHASE
;;;

;; First production sends the message
(p begin-message-test-simple
   =goal>
      state player-identified
   =imaginal>
      isa player-info
      type =type
   ?manual>
      state free
==>
   =goal>
      state msg-content-sent  ; Change goal state to indicate message was sent
   =imaginal>  ; Keep the player info
   !eval! (progn
            (format t "~%Sending message about position~%")
            (send-message '(:inform :position 10 20)))
)

;; Second production presses a key after the message was sent
(p press-key-after-message
   =goal>
      state msg-content-sent  ; This matches the state from previous production
   =imaginal>
      isa player-info
      type =type
   ?manual>
      state free
==>
   =goal>
      state msg-key-sent  ; Update state to indicate key was pressed
   !eval! (format t "~%Now pressing key 'd' after message~%")
   +manual>
      cmd press-key
      key "d"
)



;;; UTILITY PRODUCTIONS

;; Periodic visual update to keep checking the environment

;;; Set parameters for the model
(sgp :v t                ; verbose output
     :trace-detail high  ; detailed trace
     :buffer-trace t     ; buffer tracing
     :esc t              ; enable subsymbolic computations 
     :mas 3.0            ; maximum associative strength
     :ans 0.5            ; base level constant
     :rt -10.0           ; retrieval threshold (much lower to ensure retrievals always succeed)
     :pm 1               ; partial matching enabled
     :visual-movement-tolerance 3.0 ; tolerance for visual movements
     :visual-num-finsts 15        ; number of visual finsts
     :visual-finst-span 10.0       ; visual finst span
     :show-focus t                 ; show visual focus
     :dat 0.05           ; default action time - make actions quicker
     :randomize-time nil ; don't randomize timing for more predictable behavior 
     :egs 0.1            ; expected gain noise
     :time-master-start-increment 1.0 ; for temporal module timing
     :time-mult 1.0                   ; for accurate temporal delays
     )
)