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


  (chunk-type player-state 
   player-type    ; rect or disc
   rect-x rect-y  ; rectangle position for detecting player type
   rect-width     ; for testing player type
   test-sent      ; for player detection
   diamond-x diamond-y  ; diamond location
   horizontal-first     ; movement strategy
   current-goal)        ; what the agent is trying to do

  
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
   (searching-for-diamond)
   (found-diamond)
   (try-search-method-2)
   (random-exploration)
   (planning-path-to-diamond)
   (move-horizontally)
   (move-vertically)
   (executing-movement)
   (waiting-for-movement)
   (reassess-position)
   (check-next-object)
   (executing-random-movement)
   (locating-player)
   (locate-player)
   (check-found-object)
   (checking-object)
   (executing-complex-movement)
   (complex-move)
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
      isa player-state
      rect-x =x
      rect-y =y
      rect-width =w
      test-sent nil
      player-type unknown
      current-goal detect-player
   !output! (Found potential rectangle object at x =x y =y)
)

;; Production to send test movement command
  (p send-test-movement
     =goal>
        state ready-for-test
     =imaginal>
        isa player-state
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
      isa player-state
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
      current-goal find-diamond
   !output! (We are controlling the RECT player - detected movement or width change)
   )


(p detect-disc-player
     =goal>
      state checking-rect
   =visual-location>
      screen-x =new-x
      screen-y =new-y
      width =new-w
   =imaginal>
      isa player-state
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
        current-goal find-diamond
     !output! (We are controlling the DISC player)
  )

  ;; Production to initiate diamond search after player identification
(p begin-diamond-search
   =goal>
      state player-identified
   =imaginal>
      isa player-state
      player-type =player
      current-goal find-diamond
   ?visual-location>
      state free
==>
   =goal>
      state searching-for-diamond
   =imaginal>
      current-goal searching
   -visual>
   +visual-location>
      kind polygon
      value "diamond"
   !output! (Beginning search for diamonds as =player player)
)

(p diamond-found-initial-search
   =goal>
      state searching-for-diamond
   =imaginal>
      current-goal searching
   =visual-location>
      screen-x =x
      screen-y =y
   ?visual>
      state free
==>
   =goal>
      state found-diamond
   =imaginal>
      diamond-x =x
      diamond-y =y
   =visual-location>
   +visual>
      cmd move-attention
      screen-pos =visual-location
   !output! (Found diamond directly at x =x y =y)
)

;; Production to handle finding a diamond
(p found-diamond-state
   =goal>
      state found-diamond
   =visual-location>
   =visual>
   =imaginal>
==>
   =goal>
      state planning-path-to-diamond
   -visual-location>
   =visual>
   =imaginal>
      current-goal plan-path
   !output! (Found diamond at - planning path)
)

;; After finding a diamond, find player position
(p find-player-position
   =goal>
      state planning-path-to-diamond
   =imaginal>
      isa player-state
      diamond-x =dx
      diamond-y =dy
      player-type =type
      current-goal plan-path
   ?visual-location>
      state free
==>
   =goal>
      state locating-player
   =imaginal>
      current-goal locate-player
   -visual>
   +visual-location>
      isa polygon-feature
      value =type  ; "rect" or "disc" depending on player type
   !output! (Located diamond now finding player position)
)

;; If player is rect, find object with appropriate color
(p find-rect-player
   =goal>
      state locating-player
   =imaginal>
      isa player-state
      player-type rect
      current-goal locate-player
   ?visual-location>
      state free
==>
   =goal>
      state locating-player
   =imaginal>
   +visual-location>
      kind polygon
      color red
   !output! (Looking for rectangle player (red color))
)

;; If player is disc, find oval object 
(p find-disc-player
   =goal>
      state locating-player
   =imaginal>
      isa player-state
      player-type disc
      current-goal locate-player
   ?visual-location>
      state free
==>
   =goal>
      state locating-player
   =imaginal>
   +visual-location>
      value "disc"
   !output! (Looking for disc player (oval object))
)

;; Production to plan horizontal-first approach for rectangle player
(p plan-rect-approach
   =goal>
      state locating-player
   =visual-location>
      screen-x =player-x
      screen-y =player-y
   =imaginal>
      isa player-state
      player-type rect
      diamond-x =diamond-x
      diamond-y =diamond-y
      current-goal locate-player
==>
   =goal>
      state move-horizontally
   =imaginal>
      horizontal-first true
      current-goal execute-movement
   =visual-location>
   +visual>
      cmd move-attention
      screen-pos =visual-location
   !output! (Rectangle player at =player-x =player-y planning movement to diamond at =diamond-x =diamond-y)
)

;; For disc player
(p plan-disc-movement
   =goal>
      state locating-player
   =visual-location>
      screen-x =player-x
      screen-y =player-y
   =imaginal>
      isa player-state
      player-type disc
      diamond-x =diamond-x
      diamond-y =diamond-y
      current-goal locate-player
==>
   =goal>
   =imaginal>
      current-goal execute-movement
   =visual-location>
   +visual>
      cmd move-attention
      screen-pos =visual-location
   !eval! (if (> (abs (- =diamond-x =player-x)) (abs (- =diamond-y =player-y)))
             (mod-buffer-chunk 'goal '(state move-horizontally))
             (mod-buffer-chunk 'goal '(state move-vertically)))
   !output! (Disc player at =player-x =player-y planning optimal path to diamond at =diamond-x =diamond-y)
)

;; === MOVEMENT PRODUCTIONS ===

;; Production to move horizontally toward diamond
(p move-horizontally-left
   =goal>
      state move-horizontally
   =imaginal>
      isa player-state
      diamond-x =target-x
      current-goal execute-movement
   =visual-location>
      screen-x =current-x
   !eval! (< =target-x =current-x)
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
   +manual>
      cmd press-key
      key a
   !output! (Moving left toward diamond)
)

(p move-horizontally-right
   =goal>
      state move-horizontally
   =imaginal>
      isa player-state
      diamond-x =target-x
      current-goal execute-movement
   =visual-location>
      screen-x =current-x
   !eval! (> =target-x =current-x)
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
   +manual>
      cmd press-key
      key d
   !output! (Moving right toward diamond)
)

;; Switch to vertical movement after horizontal movement gets us close enough
(p switch-to-vertical-after-horizontal
   =goal>
      state move-horizontally
   =imaginal>
      isa player-state
      diamond-x =target-x
      diamond-y =target-y
      current-goal execute-movement
   =visual>
      screen-x =current-x
   !eval! (< (abs (- =target-x =current-x)) 5.0)  ; Close enough horizontally
   ?manual>
      state free
==>
   =goal>
      state move-vertically
   =imaginal>
      horizontal-first false
   !output! (Close enough horizontally switching to vertical movement)
)

;; Stretch the rectangle to reach diamonds
(p stretch-rectangle-up
   =goal>
      state move-vertically
   =imaginal>
      isa player-state
      player-type rect
      diamond-y =target-y
      current-goal execute-movement
   =visual>
      screen-y =current-y
   !eval! (< =target-y =current-y)
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
   +manual>
      cmd press-key
      key w
   !output! (Stretching rectangle upward to reach diamond)
)

;; Compress the rectangle when diamond is below
(p compress-rectangle-down
   =goal>
      state move-vertically
   =imaginal>
      isa player-state
      player-type rect
      diamond-y =target-y
      current-goal execute-movement
   =visual>
      screen-y =current-y
   !eval! (> =target-y =current-y)
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
   +manual>
      cmd press-key
      key s
   !output! (Compressing rectangle downward to reach diamond)
)

;; Production to move vertically (mainly for disc player)
(p move-vertically-up
   =goal>
      state move-vertically
   =imaginal>
      isa player-state
      diamond-y =target-y
      current-goal execute-movement
   =visual-location>
      screen-y =current-y
   !eval! (< =target-y =current-y)
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
   +manual>
      cmd press-key
      key w
   !output! (Moving up toward diamond)
)

(p move-vertically-down
   =goal>
      state move-vertically
   =imaginal>
      isa player-state
      diamond-y =target-y
      current-goal execute-movement
   =visual-location>
      screen-y =current-y
   !eval! (> =target-y =current-y)
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
   +manual>
      cmd press-key
      key s
   !output! (Moving down toward diamond)
)

;; Production to switch from horizontal to vertical movement for rect
(p switch-to-vertical-movement
   =goal>
      state executing-movement
   =visual-location>
   =imaginal>
      isa player-state
      horizontal-first true
      current-goal execute-movement
   ?manual>
      state free
==>
   =goal>
      state move-vertically
   =visual-location>
   =imaginal>
      horizontal-first false
   !output! (Switching to vertical movement)
)

;; Production to switch from vertical to horizontal movement for disc
(p switch-to-horizontal-movement
   =goal>
      state executing-movement
   =visual-location>
   =imaginal>
      isa player-state
      horizontal-first nil
      current-goal execute-movement
   ?manual>
      state free
==>
   =goal>
      state move-horizontally
   =visual-location>
   =imaginal>
      horizontal-first true
   !output! (Switching to horizontal movement)
)



;; Production to wait for manual system to be free
(p wait-for-movement-completion
   =goal>
      state executing-movement
   =imaginal>
      current-goal execute-movement
   ?manual>
      state busy
==>
   =goal>
      state waiting-for-movement
   =imaginal>
   !output! (Waiting for movement to complete)
)

;; Production to check position after movement
(p check-position-after-movement
   =goal>
      state waiting-for-movement
   =imaginal>
      current-goal execute-movement
   ?manual>
      state free
==>
   =goal>
      state reassess-position
   =imaginal>
      current-goal reassess
   -visual>
   +visual-location>
      kind polygon
      - value nil
   !output! (Movement complete reassessing position)
)

;; Production to reset to search after reassessing
(p after-movement-search-again
   =goal>
      state reassess-position
   =imaginal>
      current-goal reassess
   ?visual-location>
      buffer failure
==>
   =goal>
      state searching-for-diamond
   =imaginal>
      current-goal searching
   +visual-location>
      kind polygon
      value "diamond"
   !output! (Reassessing failed (buffer failure) searching for diamond again)
)

;; Alternative reset to search when visual-location is empty or no diamond found
(p after-movement-search-again-alternative
   =goal>
      state reassess-position
   =imaginal>
      current-goal reassess
   ?visual-location>
      state free
==>
   =goal>
      state searching-for-diamond
   =imaginal>
      current-goal searching
   +visual-location>
      kind polygon
      value "diamond"
   !output! (Reassessing complete searching for diamond again)
)

;; Handle case when any visual object is found during reassessment
(p found-object-during-reassessment
   =goal>
      state reassess-position
   =imaginal>
      current-goal reassess
   =visual-location>
==>
   =goal>
      state check-found-object
   =imaginal>
      current-goal checking-object
   +visual>
      cmd move-attention
      screen-pos =visual-location
   !output! (Found object during reassessment checking what it is)
)

;; If found object is a diamond
(p found-diamond-during-check
   =goal>
      state check-found-object
   =imaginal>
      current-goal checking-object
   =visual>
      value "diamond"
   =visual-location>
      screen-x =x
      screen-y =y
==>
   =goal>
      state found-diamond
   =imaginal>
      diamond-x =x
      diamond-y =y
      current-goal plan-path
   !output! (Found diamond during check)
)

;; If found object is not a diamond
(p found-non-diamond-during-check
   =goal>
      state check-found-object
   =imaginal>
      current-goal checking-object
   =visual>
      - value "diamond"
==>
   =goal>
      state searching-for-diamond
   =imaginal>
      current-goal searching
   -visual-location>
   +visual-location>
      kind polygon
      value "diamond"
   !output! (Object is not a diamond resuming search)
)

;; === RANDOM EXPLORATION ===

;; Production for random exploration when no diamonds are found
(p random-move-left
   =goal>
      state random-exploration
   =imaginal>
      current-goal explore
   ?manual>
      state free
==>
   =goal>
      state executing-random-movement
   =imaginal>
   +manual>
      cmd press-key
      key a
   !output! (Random exploration - moving left)
)

(p random-move-right
   =goal>
      state random-exploration
   =imaginal>
      current-goal explore
   ?manual>
      state free
==>
   =goal>
      state executing-random-movement
   =imaginal>
   +manual>
      cmd press-key
      key d
   !output! (Random exploration - moving right)
)

(p random-move-jump
   =goal>
      state random-exploration
   =imaginal>
      isa player-state
      player-type disc
      current-goal explore
   ?manual>
      state free
==>
   =goal>
      state executing-random-movement
   =imaginal>
   +manual>
      cmd press-key
      key w
   !output! (Random exploration - disc player jumping)
)

(p random-move-stretch
   =goal>
      state random-exploration
   =imaginal>
      isa player-state
      player-type rect
      current-goal explore
   ?manual>
      state free
==>
   =goal>
      state executing-random-movement
   =imaginal>
   +manual>
      cmd press-key
      key w
   !output! (Random exploration - rect player stretching)
)

;; Production to go back to diamond search after random movement
(p return-to-search-after-random-move
   =goal>
      state executing-random-movement
   =imaginal>
      current-goal explore
   ?manual>
      state free
==>
   =goal>
      state searching-for-diamond
   =imaginal>
      current-goal searching
   +visual-location>
      kind polygon
      value "diamond"
   !output! (Random movement complete returning to diamond search)
)

; Fallback production when checking an object doesn't lead to any further actions
(p check-object-fallback
   =goal>
      state check-found-object
   =visual>
   ?visual-location>
      state free
   =imaginal>
==>
   =goal>
      state random-exploration
   =imaginal>
      current-goal explore
   !output! (Falling back to random exploration when checking stalls)
)

;; Enhanced jump for disc player - more forceful jump
(p jump-disc-higher
   =goal>
      state move-vertically
   =imaginal>
      isa player-state
      player-type disc
      diamond-y =target-y
      current-goal execute-movement
   =visual>
      screen-y =current-y
   !eval! (< =target-y =current-y)
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
   +manual>
      cmd press-key
      key w
   !output! (Jumping disc player higher toward diamond)
)

;; Repeated jumps for disc player to reach higher platforms
(p continue-jumping
   =goal>
      state executing-movement
   =imaginal>
      isa player-state
      player-type disc
      diamond-y =target-y
      current-goal execute-movement
   =visual>
      screen-y =current-y
   !eval! (< =target-y =current-y)
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
   +manual>
      cmd press-key
      key w
   !output! (Continuing to jump to reach higher diamond)
)

;; Try diagonal movement for disc (jump and move right simultaneously)
(p jump-right-diagonal
   =goal>
      state random-exploration
   =imaginal>
      isa player-state
      player-type disc
      current-goal explore
   ?manual>
      state free
==>
   =goal>
      state executing-complex-movement
   =imaginal>
      current-goal complex-move
   +manual>
      cmd press-key
      key w
   !output! (Attempting diagonal jump-right movement)
)

;; Follow up with rightward movement after jump starts
(p follow-jump-with-right
   =goal>
      state executing-complex-movement
   =imaginal>
      isa player-state
      player-type disc
      current-goal complex-move
   ?manual>
      state free
==>
   =goal>
      state executing-random-movement
   =imaginal>
   +manual>
      cmd press-key
      key d
   !output! (Following jump with rightward movement)
)

;; Try diagonal movement for disc (jump and move left simultaneously)
(p jump-left-diagonal
   =goal>
      state random-exploration
   =imaginal>
      isa player-state
      player-type disc
      current-goal explore
   ?manual>
      state free
==>
   =goal>
      state executing-complex-movement
   =imaginal>
      current-goal complex-move
   +manual>
      cmd press-key
      key w
   !output! (Attempting diagonal jump-left movement)
)

;; Follow up with leftward movement after jump starts
(p follow-jump-with-left
   =goal>
      state executing-complex-movement
   =imaginal>
      isa player-state
      player-type disc
      current-goal complex-move
   ?manual>
      state free
==>
   =goal>
      state executing-random-movement
   =imaginal>
   +manual>
      cmd press-key
      key a
   !output! (Following jump with leftward movement)
)

;; Recovery production when model gets stuck
(p recovery-from-stuck
   =goal>
      state check-found-object
   =visual>
   =imaginal>
      current-goal checking-object
   !eval! (> (mp-time) 0.5) ; If we've been in this state too long
==>
   =goal>
      state searching-for-diamond
   =imaginal>
      current-goal searching
   +visual-location>
      kind polygon
      value "diamond"
   !output! (Recovering from stuck state by resuming diamond search)
)

;; More powerful horizontal movement for rectangle
(p move-rect-horizontally-left-stronger
   =goal>
      state move-horizontally
   =imaginal>
      isa player-state
      diamond-x =target-x
      player-type rect
      current-goal execute-movement
   =visual>
      screen-x =current-x
   !eval! (< =target-x =current-x)
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
   +manual>
      cmd press-key
      key a
   !output! (Moving rectangle strongly left toward diamond)
)

;; More powerful horizontal movement for rectangle
(p move-rect-horizontally-right-stronger
   =goal>
      state move-horizontally
   =imaginal>
      isa player-state
      diamond-x =target-x
      player-type rect
      current-goal execute-movement
   =visual>
      screen-x =current-x
   !eval! (> =target-x =current-x)
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
   +manual>
      cmd press-key
      key d
   !output! (Moving rectangle strongly right toward diamond)
)

;; Rectangle needs to stretch upward to reach diamonds
(p rectangle-stretch-up
   =goal>
      state move-vertically
   =imaginal>
      isa player-state
      player-type rect
      diamond-y =target-y
      current-goal execute-movement
   =visual>
      screen-y =current-y
   !eval! (< =target-y =current-y)
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
   +manual>
      cmd press-key
      key w
   !output! (Stretching rectangle upward to reach diamond)
)

;; Rectangle needs to compress downward
(p rectangle-compress-down
   =goal>
      state move-vertically
   =imaginal>
      isa player-state
      player-type rect
      diamond-y =target-y
      current-goal execute-movement
   =visual>
      screen-y =current-y
   !eval! (> =target-y =current-y)
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
   +manual>
      cmd press-key
      key s
   !output! (Compressing rectangle downward to reach diamond)
)

;; Rapidly alternate stretch and horizontal movement to reach diamonds
(p rectangle-stretch-and-move
   =goal>
      state random-exploration
   =imaginal>
      isa player-state
      player-type rect
      current-goal explore
   ?manual>
      state free
==>
   =goal>
      state executing-complex-movement
   =imaginal>
      current-goal complex-move
   +manual>
      cmd press-key
      key w
   !output! (Rectangle stretching up as part of composite movement)
)

;; Continue with horizontal movement after stretching
(p rectangle-move-after-stretch
   =goal>
      state executing-complex-movement
   =imaginal>
      isa player-state
      player-type rect
      current-goal complex-move
   ?manual>
      state free
==>
   =goal>
      state executing-random-movement
   =imaginal>
   +manual>
      cmd press-key
      key d
   !output! (Moving rectangle right after stretching)
)

;; Detect when rectangle is blocking disc's path
(p detect-rectangle-obstacle
   =goal>
      state move-horizontally
   =imaginal>
      isa player-state
      player-type disc
      diamond-x =target-x
      current-goal execute-movement
   =visual>
      screen-x =current-x
   ?visual-location>
      state free
==>
   =goal>
      state checking-for-obstacles
   =imaginal>
      current-goal check-path
   +visual-location>
      kind polygon
      color red
   !output! (Checking if rectangle is blocking path)
)

;; If rectangle is in the way, try jumping over it
(p jump-over-rectangle
   =goal>
      state checking-for-obstacles
   =imaginal>
      isa player-state
      player-type disc
      current-goal check-path
   =visual-location>
      screen-x =rect-x
      screen-y =rect-y
   ?manual>
      state free
==>
   =goal>
      state executing-movement
   =imaginal>
      current-goal execute-movement
   +manual>
      cmd press-key
      key w
   !output! (Jumping to get over rectangle obstacle)
)

;; After jumping, continue horizontal movement
(p continue-after-jump
   =goal>
      state executing-movement
   =imaginal>
      isa player-state
      player-type disc
      diamond-x =target-x
      current-goal execute-movement
   =visual>
      screen-x =current-x
   !eval! (or (< =target-x =current-x) (> =target-x =current-x))
   ?manual>
      state free
==>
   =goal>
      state move-horizontally
   =imaginal>
   !output! (Continuing horizontal movement after jumping)
)

;; Try to find alternative path when blocked
(p find-alternative-path
   =goal>
      state checking-for-obstacles
   =imaginal>
      isa player-state
      player-type disc
      current-goal check-path
   ?manual>
      state free
==>
   =goal>
      state move-vertically
   =imaginal>
      current-goal execute-movement
   +manual>
      cmd press-key
      key w
   !output! (Finding alternative vertical path around obstacle)
)
  
)