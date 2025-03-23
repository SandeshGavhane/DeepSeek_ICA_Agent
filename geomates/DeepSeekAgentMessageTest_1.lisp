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
   !eval! (let ((new-count (if (numberp =count) (+ =count 1) 1)))
            (set-chunk-slot-value-fct goal 'warm-up-count new-count))
   =goal>
      state warming-up
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
      - state checking-message
      - state reading-message
      - state evaluating-message
      - state message-testing
      - state preparing-message
      - state message-sent
      - state waiting-before-next
      - state waiting-timer
   ?manual>
      state free
==>
   =goal>
      state finding-diamond
)

(p after-finding-diamond-analyze-position
   =goal>
      state moving-to-diamond
      - state checking-message
      - state reading-message
      - state evaluating-message
   ?visual>
      state free
==>
   =goal>
      state planning
)

(p at-diamond-look-for-next
   =goal>
      state planning
      - state message-testing
      - state preparing-message
      - state message-sent
      - state waiting-before-next
      - state waiting-timer
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


(p periodic-diamond-search
   =goal>
      state checking-message
   ?visual-location>
      state error
   ?manual>
      state free
==>
   +manual>
      cmd press-key
      key "d"  ;; Send a movement command to update the visicon
   =goal>
      state finding-diamond
)

(p locate-message
   =goal>
      - state warm-up-waiting
      - state warm-up-delay
      - state checking-message
      - state message-testing
      - state preparing-message
      - state message-sent
      - state waiting-before-next
      - state waiting-timer
   ?retrieval>
      state free
   ?visual-location>
      state free
==>
   +retrieval>
      isa message-check-timer
   =goal>
      state checking-message-timer
)

(p check-message-timer
   =goal>
      state checking-message-timer
   =retrieval>
      isa message-check-timer
      last-check =last
   !eval! (> (- (mp-time) =last) 2.0)  ;; Only check every 2 seconds
==>
   =retrieval>
      last-check (mp-time)
   +visual-location>
      isa visual-location
      value "message"
   =goal>
      state checking-message
)

(p too-soon-for-message-check
   =goal>
      state checking-message-timer
   =retrieval>
      isa message-check-timer
      last-check =last
   !eval! (<= (- (mp-time) =last) 2.0)  ;; Don't check too frequently
==>
   =goal>
      state finding-diamond
)

;; Add this new production to handle message search failure
(p message-not-found
   =goal>
      state checking-message
   ?visual-location>
      state error
==>
   =goal>
      state finding-diamond  ;; Return to normal behavior
)
(p attend-to-message
   =goal>
      state checking-message
   =visual-location>
      isa visual-location
      value "message"
   ?visual>
      state free
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state reading-message
)

(p process-message-content
   =goal>
      state reading-message
   =visual>
      isa text
      message =content
      sender =sender
   ?imaginal>
      state free
==>
   +imaginal>
      isa message-info
      content =content
      sender =sender
      response-required t
   =goal>
      state evaluating-message
   ;; Log that we received a message
   !eval! (format t "~%RECEIVED MESSAGE: ~S from ~S~%" =content =sender)
)

(p evaluate-diamond-location-message
   =goal>
      state evaluating-message
   =imaginal>
      isa message-info
      content =content
   ?retrieval>
      state free
   ;; This checks if the message appears to be about a diamond location
   !eval! (and (listp =content) 
               (or (member :diamond-found =content) 
                   (member 'diamond-found =content)))
==>
   ;; Extract location information from the message
   ;; This is simplified - you'd need to adapt based on your message format
   =imaginal>
      response-required nil
   =goal>
      state finding-diamond
   !eval! (format t "~%DIAMOND LOCATION MESSAGE PROCESSED~%")
)

(p respond-to-message
   =goal>
      state evaluating-message
   =imaginal>
      isa message-info
      response-required t
   ?manual>
      state free
==>
   =imaginal>
      response-required nil
   =goal>
      state finding-diamond
   ;; Send a simple acknowledgment message
   !eval! (progn
            (format t "~%SENDING ACKNOWLEDGMENT~%")
            (send-message '(:acknowledge :received)))
)

(p return-to-normal-after-message
   =goal>
      state evaluating-message
   =imaginal>
      isa message-info
      response-required nil
==>
   =goal>
      state finding-diamond
   -imaginal>
)


;; Start message testing after warm-up
(p initialize-message-testing
   =goal>
      state finding-diamond
      warm-up-count =count
   ?retrieval>
      state free
   !eval! (>= =count 5)  ;; Only start message testing after warm-up is complete
==>
   +retrieval>
      isa message-test
      state ready
   =goal>
      state message-testing
)

;; Retrieve a test message to send
(p retrieve-message-to-send
   =goal>
      state message-testing
   =retrieval>
      isa message-test
      state ready
      count =count
   !eval! (< =count 4)  ;; Stop after sending 3 messages
==>
   +retrieval>
      isa outgoing-message
      priority (1+ =count)
   =goal>
      state preparing-message
)

;; Send the test message
(p send-test-message
   =goal>
      state preparing-message
   =retrieval>
      isa outgoing-message
      content =content
   ?manual>
      state free
==>
   !eval! (progn
            (format t "~%SENDING TEST MESSAGE: ~S~%" =content)
            (send-message =content))
   =goal>
      state message-sent
)

;; Update message test counter
(p update-message-test-counter
   =goal>
      state message-sent
   ?retrieval>
      state free
==>
   +retrieval>
      isa message-test
   =goal>
      state waiting-before-next
)

;; Record that we sent a message
(p record-message-sent
   =goal>
      state waiting-before-next
   =retrieval>
      isa message-test
      count =count
      - state done
==>
   =retrieval>
      count (1+ =count)
      last-sent (mp-time)
   +temporal>
      isa time
      ticks 5.0  ;; Wait 5 seconds before next message
   =goal>
      state waiting-timer
)

;; Wait between messages
(p wait-for-next-message
   =goal>
      state waiting-timer
   =retrieval>
      isa message-test
   ?temporal>
      buffer requested
==>
   =retrieval>
      state ready
   =goal>
      state message-testing
)

;; End message testing after sending all messages
(p finish-message-testing
   =goal>
      state message-testing
   =retrieval>
      isa message-test
      count =count
   !eval! (>= =count 3)
==>
   =retrieval>
      state done
   =goal>
      state finding-diamond
)

;; Return to normal goal state if message testing is done
(p return-to-normal-after-testing
   =goal>
      state waiting-before-next
   =retrieval>
      isa message-test
      state done
==>
   =goal>
      state finding-diamond
)


;;; Set parameters for the model
(sgp :v t                ; verbose output
     :trace-detail high  ; detailed trace
     :esc t              ; enable subsymbolic computations 
     :mas 5.0            ; maximum associative strength
     :ans 0.5            ; base level constant
     :rt 0               ; retrieval threshold
     :pm 1               ; partial matching enabled
     :visual-movement-tolerance 3.0 ; tolerance for visual movements
     :visual-num-finsts 15          ; number of visual finsts
     :visual-finst-span 10.0        ; visual finst span
     :show-focus t                  ; show visual focus
     :dat 0.05           ; default action time - make actions quicker for warm-up
     :randomize-time nil ; don't randomize timing for more predictable behavior
     :er t               ; enable randomness in utilities
     :egs 0.1            ; expected gain noise
     :time-master-start-increment 1.0 ; for temporal module timing
     :time-mult 1.0                   ; for accurate temporal delays
     )
)