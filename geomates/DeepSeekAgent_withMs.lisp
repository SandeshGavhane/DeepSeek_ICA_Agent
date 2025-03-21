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


;;;
;;; Basic ACT-R model for Geomates game - For testing player detection
;;;  

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
 
 ;; Initial goal
 (first-goal isa goal state warming-up warm-up-count 0)
)

;;; Set initial goal
(goal-focus first-goal)

;;; Warm-up phase productions to initiate visicon updates with delays
(p warm-up-press-key
   =goal>
      state warming-up
      warm-up-count =count
   ?manual>
      state free
   !eval! (and (< =count 5) (evenp =count))  ; Even counts send "d"
==>
   =goal>
      state warm-up-waiting
   +manual>
      cmd press-key
      key "d"  ; Send a movement key to trigger visicon update
)

(p warm-up-press-different-key
   =goal>
      state warming-up
      warm-up-count =count
   ?manual>
      state free
   !eval! (and (< =count 5) (oddp =count))  ; Odd counts send "a"
==>
   =goal>
      state warm-up-waiting
   +manual>
      cmd press-key
      key "a"  ; Send a different movement key
)

(p warm-up-waiting
   =goal>
      state warm-up-waiting
   ?manual>
      state free
==>
   +temporal>
      isa time
      ticks 1.0  ; Wait approximately 1 second
   =goal>
      state warm-up-delay
)

(p warm-up-delay-complete
   =goal>
      state warm-up-delay
      warm-up-count =count
   ?temporal>
      buffer requested
==>
   =goal>
      state warming-up
      warm-up-count (+ =count 1)
)

(p warm-up-complete
   =goal>
      state warming-up
      warm-up-count =count
   ?manual>
      state free
   !eval! (>= =count 5)
==>
   =goal>
      state finding-diamond  ;; Keep the original transition
      warm-up-count 0
   +imaginal>  ;; Add environmental tracking in parallel
      isa spatial-map
      platform-count 0
      diamond-count 0
)

;;; Production rules for detecting player type
(p detect-player-type
   =goal>
      state finding-diamond
   ?visual-location>
      state free
==>
   +visual-location>
      isa visual-location
      value "player-info"
   =goal>
      state planning
)

(p process-player-type
   =goal>
      state planning
   =visual-location>
      isa visual-location
      value "player-info"
   ?visual>
      state free
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state finding-diamond
)

(p remember-player-type
   =goal>
      state finding-diamond
   =visual>
      isa polygon
      player-type =type
==>
   +imaginal>
      isa player-info
      type =type
   =goal>
      state finding-diamond
)

;;; Production rules for finding and moving to diamonds
(p find-diamond
   =goal>
      state finding-diamond
   ?visual-location>
      state free
   ?imaginal>
     state free
==>
   +visual-location>
      isa visual-location
      value "diamond"
   =goal>
      state moving-to-diamond
)

(p find-diamond-using-spatial-map
   =goal>
      state finding-diamond
   ?visual-location>
      state free
   ?retrieval>
      state free
   =imaginal>
      isa spatial-map
      - diamond-count 0
==>
   +retrieval>
      isa diamond-info
   =goal>
      state retrieving-diamond-location
)

(p use-retrieved-diamond-location
   =goal>
      state retrieving-diamond-location
   =retrieval>
      isa diamond-info
      x =x
      y =y
==>
   =goal>
      state moving-to-diamond
      target-x =x
      target-y =y
)

(p no-diamond-found
   =goal>
      state finding-diamond
   ?visual-location>
      state error
==>
   +manual>
      cmd press-key
      key "d"  ; Send a movement key to update the visicon
   =goal>
      state planning
)

(p focus-on-diamond
   =goal>
      state moving-to-diamond
   =visual-location>
      isa visual-location
      screen-x =x
      screen-y =y
   ?visual>
      state free
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state planning
      target-x =x
      target-y =y
)

(p mark-environment-scanned
   =goal>
      state continue-scanning
   =imaginal>
      isa spatial-map
==>
   =imaginal>
      last-updated (mp-time)
   =goal>
      state finding-diamond  ;; Return to your existing state flow
)

;;; Production rules for disc player (circular character)
(p disc-move-right-to-diamond
   =goal>
      state planning
      target-x =tx
   =visual>
      screen-x =vx
   =imaginal>
      type disc
   ?manual>
      state free
   !eval! (> =tx (+ =vx 2))
   
   ;; New retrieval condition using spatial awareness
   =retrieval>
      isa spatial-map
      - platform-count 0
==>
   +manual>
      cmd press-key
      key "d"
   =goal>
      state moving-to-diamond
)

(p disc-move-left-to-diamond
   =goal>
      state planning
      target-x =tx
   =visual>
      screen-x =vx
   =imaginal>
      type disc
   ?manual>
      state free
   !eval! (< =tx (- =vx 2))
==>
   +manual>
      cmd press-key
      key "a"
   =goal>
      state moving-to-diamond
)

(p disc-jump-to-diamond
   =goal>
      state planning
      target-y =ty
   =visual>
      screen-y =vy
   =imaginal>
      type disc
   ?manual>
      state free
   !eval! (> =ty (+ =vy 2))
==>
   +manual>
      cmd press-key
      key "w"
   =goal>
      state jumping
)

;;; Production rules for rect player (rectangular character)
(p rect-move-right-to-diamond
   =goal>
      state planning
      target-x =tx
   =visual>
      screen-x =vx
   =imaginal>
      type rect
   ?manual>
      state free
   !eval! (> =tx (+ =vx 2))
==>
   +manual>
      cmd press-key
      key "d"
   =goal>
      state moving-to-diamond
)

(p rect-move-left-to-diamond
   =goal>
      state planning
      target-x =tx
   =visual>
      screen-x =vx
   =imaginal>
      type rect
   ?manual>
      state free
   !eval! (< =tx (- =vx 2))
==>
   +manual>
      cmd press-key
      key "a"
   =goal>
      state moving-to-diamond
)

(p rect-stretch-horizontally
   =goal>
      state planning
   =imaginal>
      type rect
   ?manual>
      state free
==>
   +manual>
      cmd press-key
      key "w"
   =goal>
      state transforming
)

(p rect-compress-horizontally
   =goal>
      state planning
   =imaginal>
      type rect
   ?manual>
      state free
==>
   +manual>
      cmd press-key
      key "s"
   =goal>
      state transforming
)

;;; General navigation rules
(p after-movement-look-for-diamond
   =goal>
      - state finding-diamond
   ?manual>
      state free
==>
   =goal>
      state finding-diamond
)

(p after-finding-diamond-analyze-position
   =goal>
      state moving-to-diamond
   ?visual>
      state free
==>
   =goal>
      state planning
)

(p at-diamond-look-for-next
   =goal>
      state planning
      target-x =tx
      target-y =ty
   =visual>
      screen-x =vx
      screen-y =vy
   !eval! (and (< (abs (- =tx =vx)) 2)
               (< (abs (- =ty =vy)) 2))
==>
   =goal>
      state finding-diamond
)

;;; Look for platforms to avoid or use
(p locate-platform
   =goal>
      state planning
   ?visual-location>
      state free
==>
   +visual-location>
      isa visual-location
      value "platform"
   =goal>
      state avoiding-obstacle
)

(p process-platform
   =goal>
      state avoiding-obstacle
   =visual-location>
      isa visual-location
   ?visual>
      state free
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state planning
)

;;; Fallback behavior if stuck
(p try-random-move-if-stuck
   =goal>
      state planning
   ?manual>
      state free
   ?imaginal>
      buffer empty
==>
   +manual>
      cmd press-key
      key "d"
   =goal>
      state moving-to-diamond
)

(p try-random-jump-if-stuck
   =goal>
      state planning
   ?manual>
      state free
   ?imaginal>
      buffer empty
==>
   +manual>
      cmd press-key
      key "w"
   =goal>
      state jumping
)

(p random-try-left-if-stuck
   =goal>
      state planning
   ?manual>
      state free
   ?imaginal>
      buffer empty
==>
   +manual>
      cmd press-key
      key "a"
   =goal>
      state moving-to-diamond
)

(p begin-environment-scan
   =goal>
      state finding-diamond
   ?visual-location>
      state free
   ?imaginal>
      state free
==>
   +imaginal>
      isa spatial-map
      platform-count 0
      diamond-count 0
   =goal>
      state scanning-environment
)

(p scan-for-platforms
   =goal>
      state scanning-environment
   =imaginal>
      isa spatial-map
   ?visual-location>
      state free
==>
   +visual-location>
      isa visual-location
      value "platform"
   =goal>
      state scanning-platforms
)

(p process-platform-location
   =goal>
      state scanning-platforms
   =visual-location>
      isa visual-location
      screen-x =x
      screen-y =y
      value "platform"
   ?visual>
      state free
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state encoding-platform
)

(p store-platform-info
   =goal>
      state encoding-platform
   =visual>
      isa polygon
      screen-x =x
      screen-y =y
      height =h
      width =w
   =imaginal>
      isa spatial-map
      platform-count =count
   ?retrieval>
      state free
==>
   +retrieval>
      isa platform-info
      id =count
   =imaginal>
      platform-count (+ =count 1)
   =goal>
      state continue-scanning
)

(p scan-for-diamonds
   =goal>
      state continue-scanning
   ?visual-location>
      state free
==>
   +visual-location>
      isa visual-location
      value "diamond"
   =goal>
      state scanning-diamonds
)

(p process-diamond-location
   =goal>
      state scanning-diamonds
   =visual-location>
      isa visual-location
      screen-x =x
      screen-y =y
      value "diamond"
   ?visual>
      state free
   =imaginal>
      isa spatial-map
      diamond-count =count
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   +retrieval>
      isa diamond-info
      id =count
   =imaginal>
      diamond-count (+ =count 1)
   =goal>
      state encoding-diamond
)

(p calculate-diamond-accessibility
   =goal>
      state encoding-diamond
   =visual>
      isa visual-object
      screen-x =diamond-x
      screen-y =diamond-y
   =imaginal>
      isa spatial-map
   ?retrieval>
      state free
==>
   +retrieval>
      isa platform-info
   =goal>
      state evaluating-access
)

(p mark-environment-scanned
   =goal>
      state continue-scanning
   =imaginal>
      isa spatial-map
==>
   =imaginal>
      last-updated (mp-time)
   =goal>
      state finding-diamond
)

(p opportunistic-rect-movement-right
   =goal>
      state scanning-environment
   =imaginal>
      isa spatial-map
   =retrieval>
      isa player-info
      type rect
   ?manual>
      state free
==>
   +manual>
      cmd press-key
      key "d"
   =goal>
      state scanning-environment
)

(p opportunistic-rect-movement-left
   =goal>
      state scanning-environment
   =imaginal>
      isa spatial-map
   =retrieval>
      isa player-info
      type rect
   ?manual>
      state free
==>
   +manual>
      cmd press-key
      key "a"
   =goal>
      state scanning-environment
)

(p fast-rect-stretch-if-diamond-above
   =goal>
      state finding-diamond
   =visual-location>
      isa visual-location
      value "diamond"
      screen-y =y
   =visual>
      screen-y =vy
   =imaginal>
      type rect
   ?manual>
      state free
   !eval! (> =y =vy)
==>
   +manual>
      cmd press-key
      key "w"
   =goal>
      state moving-to-diamond
)

(p scan-environment-selectively
   =goal>
      state scanning-environment
   =imaginal>
      isa spatial-map
   ?visual-location>
      state free
==>
   +visual-location>
      isa visual-location
      :nearest current-position
   =goal>
      state processing-nearby-object
)

(p interrupt-scanning-for-diamond
   =goal>
      state scanning-environment
   =visual-location>
      isa visual-location
      value "diamond"
   ?visual>
      state free
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state moving-to-diamond
)

(p calculate-diamond-distance
   =goal>
      state finding-diamond
   =visual-location>
      isa visual-location
      value "diamond"
      screen-x =dx
      screen-y =dy
   =visual>
      isa polygon
      screen-x =px
      screen-y =py
   ?imaginal>
      state free
==>
   +imaginal>
      isa diamond-location
      x =dx
      y =dy
      distance (sqrt (+ (sq (- =dx =px)) (sq (- =dy =py))))
   =goal>
      state evaluating-diamond
)

(p select-nearest-diamond
   =goal>
      state evaluating-diamond
   =imaginal>
      isa diamond-location
      x =x
      y =y
      distance =d
   =retrieval>  ; Previously retrieved diamond
      isa diamond-location
      distance =old-d
   !eval! (< =d =old-d)  ; Current diamond is closer
==>
   =imaginal>  ; Keep this as new best candidate
   =goal>
      state finding-more-diamonds
)

(p keep-previous-diamond
   =goal>
      state evaluating-diamond
   =imaginal>
      isa diamond-location
      distance =d
   =retrieval>
      isa diamond-location
      distance =old-d
      x =old-x
      y =old-y
   !eval! (>= =d =old-d)  ; Current diamond is not better
==>
   =imaginal>  ; Replace with previous best diamond
      x =old-x
      y =old-y
      distance =old-d
   =goal>
      state finding-more-diamonds
)

(p check-path-to-diamond
   =goal>
      state planning
   =imaginal>
      isa diamond-location
      x =target-x
      y =target-y
   ?visual-location>
      state free
==>
   +visual-location>
      isa visual-location
      value "platform"
      :nearest current-location
   =goal>
      state checking-obstacles
)

(p identify-obstacle-in-path
   =goal>
      state checking-obstacles
   =visual-location>
      isa visual-location
      value "platform"
   =visual>
      isa visual-object
   =imaginal>
      isa diamond-location
      x =target-x
      y =target-y
   ?retrieval>
      state free
==>
   +retrieval>
      isa path-planning
   =goal>
      state planning-path
)

(p plan-path-around-obstacle
   =goal>
      state planning-path
   =visual>
      isa polygon
      height =h
      width =w
      screen-x =obstacle-x
      screen-y =obstacle-y
   =imaginal>
      isa diamond-location
      x =target-x
      y =target-y
==>
   +imaginal>
      isa path-planning
      step 1
      next-move (if (> =target-x =obstacle-x) "right" "left")
      obstacle-ahead t
   =goal>
      state executing-path
)

(p execute-path-step
   =goal>
      state executing-path
   =imaginal>
      isa path-planning
      next-move =move
   ?manual>
      state free
==>
   +manual>
      cmd press-key
      key =move
   =goal>
      state moving-to-diamond
)

;;; Message Communication Productions
(p detect-unreachable-diamond
   =goal>
      state planning
      target-y =ty
   =visual>
      screen-y =vy
   =imaginal>
      type disc
   ?manual>
      state free
   !eval! (> (- =ty =vy) 10)  ; Diamond is too high for disc player
==>
   +manual>
      cmd press-key
      key "m(:need-help :diamond-at =ty)"  ; Send message with m prefix
   =goal>
      state waiting-for-help
)

(p receive-help-request
   =goal>
      state finding-diamond
   =visual>
      isa message
      content "(:need-help :diamond-at =y)"  ; Message from other player
   =imaginal>
      type rect  ; I'm the rectangle player
==>
   =goal>
      state helping-partner
      target-y =y
   +imaginal>
      isa help-request
      target-y =y
)

(p notify-position-ready
   =goal>
      state helping-partner
   =visual>
      screen-x =x
      screen-y =y
   =imaginal>
      target-y =ty
   !eval! (< (abs (- =y =ty)) 5)  ; Close to target position
==>
   +manual>
      cmd press-key
      key "m(:ready-at =x =y)"  ; Tell disc player I'm in position
   =goal>
      state holding-position
)
(p assess-movement-cost
   =goal>
      state planning
   =imaginal>
      isa diamond-location
      x =target-x
      y =target-y
   =visual>
      screen-x =current-x
      screen-y =current-y
==>
   =goal>
      state moving-to-diamond
      ; Set horizontal and vertical costs based on distance
      target-x =target-x
      target-y =target-y
)


;;; Collaborative Positioning Productions
(p rect-position-as-platform
   =goal>
      state helping-partner
   =visual>
      isa oval  ; Looking at disc player
      screen-x =disc-x
   =imaginal>
      target-y =ty
   ?manual>
      state free
==>
   =goal>
      state moving-to-position
      target-x =disc-x  ; Position above disc player
      target-y (+ =ty 3)  ; Slightly below the diamond
)

(p rect-stretch-as-platform
   =goal>
      state holding-position
   =imaginal>
      type rect
   ?manual>
      state free
==>
   +manual>
      cmd press-key
      key "w"  ; Stretch horizontally to create wider platform
   =goal>
      state transformed-for-help
)

(p disc-use-rect-platform
   =goal>
      state waiting-for-help
   =visual>
      isa polygon
      value "rect"
      screen-x =rx
      screen-y =ry
   =visual-location>
      isa message
      content "(:ready-at =x =y)"
   ?manual>
      state free
==>
   =goal>
      state moving-to-platform
      target-x =rx
)

(p disc-jump-from-platform
   =goal>
      state moving-to-platform
   =visual>
      isa polygon
      value "rect"
      screen-x =rx 
      screen-y =ry
   =imaginal>
      type disc
   !eval! (< (abs (- =current-x =rx)) 2)  ; Positioned under rectangle
==>
   +manual>
      cmd press-key
      key "w"  ; Jump to use rectangle as platform
   =goal>
      state jumping-to-diamond
)

;;; Diamond Allocation Productions
(p analyze-diamond-heights
   =goal>
      state scanning-diamonds
   =visual>
      isa visual-object
      value "diamond"
      screen-y =y
   =imaginal>
      type =player-type
==>
   +imaginal>
      isa diamond-allocation
      height =y
      suitable-for (if (> =y 15) 'rect 'disc)  ; High diamonds for rect, low for disc
   =goal>
      state allocating-targets
)

(p select-diamonds-by-suitability
   =goal>
      state finding-diamond
   ?visual-location>
      state free
   =imaginal>
      type =my-type
==>
   +visual-location>
      isa visual-location
      value "diamond"
      :nearest (if (eq =my-type 'rect) 'highest 'lowest)  ; Rect focuses on high diamonds
   =goal>
      state selecting-suitable-diamond
)

(p claim-suitable-diamond
   =goal>
      state selecting-suitable-diamond
   =visual-location>
      screen-y =y
   =imaginal>
      type =my-type
   !eval! (or (and (eq =my-type 'rect) (> =y 15))
              (and (eq =my-type 'disc) (< =y 15)))  ; Diamond height suits player type
==>
   +manual>
      cmd press-key
      key "m(:claiming-diamond =y)"  ; Inform other player
   =goal>
      state moving-to-diamond
)

;;; Coordinated Strategies Productions
(p rect-create-bridge
   =goal>
      state evaluating-access
   =visual>
      isa visual-object
      screen-x =x1
      screen-y =y
   =retrieval>
      isa platform-info
      x =x2
   !eval! (and (> (abs (- =x1 =x2)) 3)  ; Gap between platforms
               (< (abs (- =x1 =x2)) 10)  ; But within reach if stretched
               (< (abs (- =y =retrieval-y)) 3))  ; At similar heights
==>
   =goal>
      state creating-bridge
      target-x (/ (+ =x1 =x2) 2)  ; Position in the middle of gap
)

(p coordinate-diamond-collection
   =goal>
      state finding-diamond
   =visual>
      isa visual-object
      value "diamond"
      screen-x =dx
      screen-y =dy
   =retrieval>
      isa player-info
      other-player-x =px
      other-player-y =py
   !eval! (< (abs (- =dx =px)) (abs (- =dx =current-x)))  ; Other player is closer
==>
   +manual>
      cmd press-key
      key "m(:you-get-diamond =dx =dy)"  ; Suggest other player gets it
   =goal>
      state finding-next-diamond  ; Look for a different diamond
)
;;; Enhanced Environmental Awareness Productions
(p track-other-player-position
   =goal>
      state scanning-environment
   =visual>
      isa visual-object
      - value "player-info"  ; Not looking at self
      - value "diamond"      ; Not a diamond
      - value "platform"     ; Not a platform
   ?imaginal>
      state free
==>
   +imaginal>
      isa partner-tracking
      partner-x =visual-x
      partner-y =visual-y
      timestamp (mp-time)
   =goal>
      state updating-partner-model
)

(p analyze-level-structure
   =goal>
      state warm-up-complete
   ?visual-location>
      state free
==>
   +visual-location>
      isa visual-location
      :attended nil
   =goal>
      state mapping-environment
)

(p build-level-knowledge
   =goal>
      state mapping-environment
   =visual-location>
      isa visual-location
      screen-x =x 
      screen-y =y
      value =type
   =imaginal>
      isa spatial-map
==>
   =imaginal>
      last-object-type =type
      last-object-location (list =x =y)
   =goal>
      state mapping-environment
      ; Continue mapping until we have a good understanding
)
;;; Recovery and Adaptation Productions
(p detect-stuck-state
   =goal>
      state moving-to-diamond
   =visual>
      screen-x =current-x
      screen-y =current-y
   =retrieval>
      isa position-history
      x =prev-x
      y =prev-y
      timestamp =time
   !eval! (and (< (abs (- =current-x =prev-x)) 0.5)
               (< (abs (- =current-y =prev-y)) 0.5)
               (> (- (mp-time) =time) 3))  ; Position unchanged for 3 seconds
==>
   =goal>
      state recovery-mode
)

(p try-alternative-approach
   =goal>
      state recovery-mode
   =imaginal>
      type =player-type
   ?manual>
      state free
==>
   +manual>
      cmd press-key
      key (if (eq =player-type 'disc) "w" "s")  ; Jump if disc, compress if rect
   =goal>
      state finding-new-path
)

(p request-partner-assistance
   =goal>
      state recovery-mode
   ?manual>
      state free
==>
   +manual>
      cmd press-key
      key "m(:stuck-need-help =current-x =current-y)"
   =goal>
      state waiting-for-rescue
)

;;; Test message sending production for disc player
;;; Test message sending production for disc player
(p disc-send-test-message
   =goal>
      state finding-diamond
   =imaginal>
      type disc
   ?manual>
      state free
==>
   !eval! (format t "~%DISC PLAYER ATTEMPTING TO SEND MESSAGE~%")
   +manual>
      cmd press-key
      key "m"
   =goal>
      state sending-message-part1
)

(p disc-complete-message
   =goal>
      state sending-message-part1
   ?manual>
      state free
==>
   !eval! (format t "~%DISC PLAYER SENDING MESSAGE CONTENT~%")
   +manual>
      cmd press-key
      key "(:test-message-from-disc)"
   =goal>
      state finding-diamond
)

;;; Test message sending production for rect player
(p rect-send-test-message
   =goal>
      state finding-diamond
   =imaginal>
      type rect
   ?manual>
      state free
==>
   !eval! (format t "~%RECT PLAYER ATTEMPTING TO SEND MESSAGE~%")
   +manual>
      cmd press-key
      key "m"
   =goal>
      state sending-message-part1
)

(p rect-complete-message
   =goal>
      state sending-message-part1
   ?manual>
      state free
==>
   !eval! (format t "~%RECT PLAYER SENDING MESSAGE CONTENT~%")
   +manual>
      cmd press-key
      key "(:test-message-from-rect)"
   =goal>
      state finding-diamond
)

;;; Message detection productions
(p search-for-message-to-rect
   =goal>
      state finding-diamond
   =imaginal>
      type rect
   ?visual-location>
      state free
==>
   !eval! (format t "~%RECT PLAYER SEARCHING FOR MESSAGES~%")
   +visual-location>
      isa visual-location
      value "message-to-rect"
   =goal>
      state checking-messages
)

(p search-for-message-to-disc
   =goal>
      state finding-diamond
   =imaginal>
      type disc
   ?visual-location>
      state free
==>
   !eval! (format t "~%DISC PLAYER SEARCHING FOR MESSAGES~%")
   +visual-location>
      isa visual-location
      value "message-to-disc"
   =goal>
      state checking-messages
)

(p process-found-message
   =goal>
      state checking-messages
   =visual-location>
      isa visual-location
   ?visual>
      state free
==>
   !eval! (format t "~%PLAYER FOUND A MESSAGE IN VISICON~%")
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state processing-message-content
)

(p message-not-found
   =goal>
      state checking-messages
   ?visual-location>
      state error
==>
   !eval! (format t "~%NO MESSAGE FOUND IN VISICON~%")
   =goal>
      state finding-diamond
)


(sgp :v t                ; verbose output
     :trace-detail high  ; detailed trace
     :show-focus t       ; show visual focus
     :visual-num-finsts 15
     ;; ... other settings ...
     :cmdt t             ; Command trace
     :actr-output-all t  ; Output all details
)

)