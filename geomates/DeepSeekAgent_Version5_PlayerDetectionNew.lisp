;;;
;;; Complete ACT-R model for Geomates game
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
;;; ACT-R model for Geomates game - Player Detection Focus
;;;  

;;; Define the model
(define-model geomates-player-detection

;;; Define chunk types for game objects and player state awareness
(chunk-type (polygon-feature (:include visual-location)) regular)
(chunk-type (polygon (:include visual-object)) sides height width rotation diamonds player-type)
(chunk-type goal state intention target-x target-y warm-up-count)
(chunk-type control intention button)
(chunk-type time ticks)
(chunk-type player-info type)
(chunk-type diamond-location x y distance)
(chunk-type platform-location x y width height)
(chunk-type path-planning step next-move obstacle-ahead)

(chunk-type platform-info id x y width height accessibility)
(chunk-type diamond-info id x y distance-from-player)
(chunk-type spatial-map platform-count diamond-count last-updated)


(chunk-type (text-feature (:include visual-location)))
(chunk-type (text (:include visual-object)) message sender)
(chunk-type message-info content sender response-required)

(chunk-type message-test state count last-sent timer)
(chunk-type outgoing-message content priority)
(chunk-type message-check-timer last-check)

;;; Do this to avoid warnings when chunks are created
(define-chunks true false polygon)

;;; Set vision module to look for the lowest (nearest) screen-x item first
(set-visloc-default screen-x lowest)

;;; Add declarative memory chunks
(add-dm
 ;; Movement intentions
 (move-left) (move-right) (move-up) (move-down)
 (jump) (stretch) (compress)
 
 ;; Keyboard controls
 (w) (a) (s) (d)
 
 ;; Goal states
 (warming-up) (warm-up-waiting) (warm-up-delay) (planning) (finding-diamond) (moving-to-diamond) 
 (avoiding-obstacle) (jumping) (transforming)
 
 ;; Control mappings
 (up-control isa control intention move-up button w)
 (down-control isa control intention move-down button s)
 (left-control isa control intention move-left button a)
 (right-control isa control intention move-right button d)

   (message-test-control isa message-test state ready count 0 last-sent 0 timer 5.0)
   (test-msg-1 isa outgoing-message content (:diamond-found :x 25 :y 30) priority 1)
   (test-msg-2 isa outgoing-message content (:need-help :obstacle platform) priority 2)
   (test-msg-3 isa outgoing-message content (:going-to :x 40 :y 20) priority 3)
   (message-timer isa message-check-timer last-check 0)
 
 (init-visicon)(waiting-for-update-1)(press-for-second-update)(waiting-for-update-2)(visicon-ready)
 ;; Initial goal
 (first-goal isa goal state init-visicon)
)

;;; Set initial goal
(goal-focus first-goal)



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

;;; Transition to main behavior once visicon is ready
(p visicon-ready-transition
   =goal>
      state visicon-ready
   ?temporal>
      buffer requested
==>
   !eval! (format t "~%Visicon update complete - transitioning to main task~%")
   =goal>
      state finding-diamond  ; Transition to your main task state
)

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
      state finding-diamond
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
   !eval! (format t "~%Failed to detect player by any method, continuing with diamond search~%")
   =goal>
      state finding-diamond
)




;;; Set parameters for the model
(sgp :v t                ; verbose output
     :trace-detail high  ; detailed trace
     :buffer-trace t
     )
)