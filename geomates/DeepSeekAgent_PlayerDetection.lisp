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
;;; Now comes the core Act-R agent
;;;

(define-model geomates-agent

  ;; [find explanation in actr7.x/examples/vision-module]
  (chunk-type (polygon-feature (:include visual-location)) regular rotation diamonds value)
  (chunk-type (polygon (:include visual-object)) sides screen-x screen-y radius rotation)
  (chunk-type (oval (:include visual-object)) screen-x screen-y radius diamonds)
  ;; Add new chunk type to store positions and player information
  (chunk-type position-record rect-x rect-y rect-width test-sent player-type)
  
  ;; [might be obsolete] Do this to avoid the warnings when the chunks are created
  (define-chunks true false polygon)
  
  ;; [might be obsolete] stuff the leftmost item
  (set-visloc-default screen-x lowest)

  (chunk-type goal state intention)
  (chunk-type control intention button)

  (add-dm
   (move-left) (move-right)
   (move-up)  (move-down)
   (w) (a) (s) (d)
   (i-dont-know-where-to-go)
   (initializing-game)
   (waiting-for-objects)
   (checking-for-rect)
   (found-rect)
   (ready-for-test)
   (waiting-for-response)
   (checking-rect)
   (player-identified)
   (something-should-change)
   (i-want-to-do-something)
   (first-step)
   (second-step)
   (warming-up-game)
   (up-control isa control intention move-up button w)
   (down-control isa control intention move-down button s)
   (left-control isa control intention move-left button a)
   (right-control isa control intention move-right button d)
   (first-goal isa goal state first-step intention nil)
   (initial-positions isa position-record rect-x nil rect-y nil test-sent nil player-type unknown)
   )

  (goal-focus first-goal)
  
  ;; === PLAYER DETECTION PRODUCTIONS ===
  
  ;; Production to detect connected text and initialize game
  (p step-1
     =goal>
        state first-step
     ?manual>
        state free
  ==>
     =goal>
        state second-step
     +visual-location>
       kind text 
     !output! (find 'connected' text preparing to initialize game)
  )

  (p detect-connected-text
     =goal>
        state second-step
      =visual-location>
      kind text

  ==>
     =goal>
        state initializing-game
     =visual-location>
     +visual>
        cmd move-attention
        screen-pos =visual-location
     !output! (Found 'connected' text preparing to initialize game)
  )

  ;; Production to send key after attending to text
  (p send-initial-key
   =goal>
      state initializing-game
   =visual-location>
      kind text
   ?manual>
      state free
==>
   =goal>
      state warming-up-game  ; Changed from waiting-for-objects
   +manual>
      cmd press-key
      key w
   !output! (Sending initial key to initialize game world)
)

; Add a new production that waits a moment before checking for objects
(p wait-for-game-initialization
   =goal>
      state warming-up-game
   ?manual>
      state free
==>
   =goal>
      state warming-up-game  ; Stay in the same state
   +manual>
      cmd press-key
      key w              ; Press a neutral key that doesn't affect gameplay
   !output! (Waiting for game to initialize - delaying check)
)

(p finish-warming-up
   =goal>
      state warming-up-game
   ?manual>
      state free
   !eval! (> (mp-time) 1)  ; Only match after 800ms of model time has passed
==>
   -visual>
   =goal>
      state waiting-for-objects
   !output! (Game should be initialized now - ready to check for objects)
)

    
(p check-for-rect-object
   =goal>
      state waiting-for-objects
   ?visual-location>
      state free
==>
   =goal>
      state checking-for-rect
   +visual-location>
      kind polygon
      color red
   !output! (Checking if rectangle object is available )
)

(p found-rect-object
   =goal>
      state checking-for-rect
   =visual-location>
      screen-x =x
      screen-y =y
      width =w
   ?visual>
      state free
   ?imaginal>
        state free
==>
   =goal>
      state ready-for-test
   +visual>
      cmd move-attention
      screen-pos =visual-location
   +imaginal>
        isa position-record
        rect-x =x
        rect-y =y
        rect-width =w
        test-sent nil
        player-type unknown
   !output! (Found potential rectangle object at x =x y =y width =w)
)

;; Production to send test movement command
  (p send-test-movement
     =goal>
        state ready-for-test
     =imaginal>
        isa position-record
        rect-x =rx
        rect-y =ry
        test-sent nil
     ?manual>
        state free
  ==>
     =goal>
        state waiting-for-response
     =imaginal>
        test-sent true
     +manual>
        cmd press-key
        key a
     !output! (Sending test movement command a)
  )

  (p check-after-delay
   =goal>
      state waiting-for-response
   =imaginal>
      test-sent true
   ?manual>
      state free       ;; Changed from busy to free - wait until keypress is complete
==>
   =goal>
      state waiting-for-visual-update
   =imaginal>
   !output! (Key press complete waiting for visual update)
)

;; New production to add a delay before checking the rectangle
(p wait-for-visual-update
   =goal>
      state waiting-for-visual-update
   =imaginal>
   !eval! (> (mp-time) 0.5)  ;; Wait for at least 500ms of model time
==>
   =goal>
      state checking-rect
   =imaginal>
   +visual-location>
      kind polygon
      color red
   !output! (Sufficient time passed checking if rectangle has moved)
)


  (p detect-rect-player
   =goal>
      state checking-rect
   =visual-location>
      screen-x =new-x
      screen-y =new-y
      width =new-w
   =imaginal>
      isa position-record
      rect-x =old-x
      rect-y =old-y
      rect-width =old-w
   !eval! (or (/= =new-x =old-x) 
              (/=  =new-y =old-y)
              (/=  =new-w =old-w))
==>
   =goal>
      state player-identified
   =imaginal>
      player-type rect
   !output! (We are controlling the RECT player - detected movement or width change)
   !output!  (Found new rectangle object at x =new-x y =new-y width =new-w oldx =old-x oldy =old-y width =old-w)
   )


(p detect-disc-player
     =goal>
      state checking-rect
   =visual-location>
      screen-x =new-x
      screen-y =new-y
      width =new-w
   =imaginal>
      isa position-record
      rect-x =old-x
      rect-y =old-y
      rect-width =old-w
      !eval! (and (= =new-x =old-x) 
               (= =new-y =old-y)
               (= =new-w =old-w))
  ==>
     =goal>
        state player-identified
     =imaginal>
        player-type disc
     !output! (We are controlling the DISC player)
     !output!  (Found new rectangle object at x =new-x y =new-y width =new-w oldx =old-x oldy =old-y width =old-w)
  )

  
)