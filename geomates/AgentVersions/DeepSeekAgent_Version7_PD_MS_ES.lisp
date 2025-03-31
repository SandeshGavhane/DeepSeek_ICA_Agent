;;;
;;; Enhanced DeepSeekAgent for Geomates game
;;; With environment scanning and collaboration features
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

;;; In case you need to differentiate different environments/OS/compilers:
;;; have a look at Common-Lisps reader macros #+/#- (like #ifdef in C),
;;; which refer to the global variable *features*
;;; examples:
;;; #+SBCL (print "I'm running the SBCL compiler")
;;; (defparameter *magic-code* #+LITTLE-ENDIAN #x0f12 #-LITTLE-ENDIAN 0x120f)




;;; ACT-R model for Geomates game with enhanced environment scanning and collaboration
;;;  

;;; Define the model
(define-model geomates-enhanced-agent

;;; Define chunk types for game objects, communication, and environment mapping
(chunk-type (polygon-feature (:include visual-location)) regular)
(chunk-type (polygon (:include visual-object)) sides height width rotation diamonds player-type)
(chunk-type (text-feature (:include visual-location)))
(chunk-type (text (:include visual-object)) message sender)
(chunk-type goal state substate last-message)
(chunk-type time ticks)
(chunk-type player-info type diamonds)
(chunk-type other-player-info type position-x position-y diamonds)
(chunk-type outgoing-message content)
(chunk-type message-info content sender response-required)
(chunk-type diamond-location id x-pos y-pos distance reachable assigned-to)
(chunk-type environment-map platforms diamonds obstacles)
(chunk-type navigation-target x-pos y-pos status)
(chunk-type strategy mode priority-target fallback-target)

;;; Do this to avoid warnings when chunks are created
(define-chunks 
 true false polygon
 disc rect
 unreachable pending reachable collected
 efficient defensive collaborative
 horizontal vertical)

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
 (scanning-environment)
 (mapping-environment)
 (detecting-diamonds)
 (prioritizing-diamonds)
 (calculating-distances)
 (planning-collection)
 (approaching-target)
 (avoiding-obstacle)
 (communicating)
 (waiting-for-response)
 (processing-message)
 (coordinating-action)
 
 ;; Message testing states
 (msg-start)
 (msg-key-sent)
 (msg-content-sent)
 (msg-waiting)
 
 ;; Predefined messages
 (msg-1 isa outgoing-message content (:hello :i-am "deep-seek-agent"))
 (msg-2 isa outgoing-message content (:ready-to-collaborate :task "diamond-collection"))
 (msg-3 isa outgoing-message content (:strategy :mode "collaborative"))
 
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

;;; Wait for first update
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
      diamonds 0           ; Initialize diamond count
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
   !eval! (format t "~%Failed to detect player by any method, assuming disc player~%")
   ?imaginal>
      state free
==>
   +imaginal>
      isa player-info
      type "disc"        ; Default assumption
      diamonds 0         ; Initialize diamond count
   =goal>
      state player-identified
)


;;;
;;; ENVIRONMENT SCANNING PHASE
;;;

;;; Initialize environment scanning after player identification
(p begin-environment-scan
   =goal>
      state player-identified
   =imaginal>
      isa player-info
      type =type
   ?manual>
      state free
==>
   !eval! (format t "~%Beginning environment scan as ~A player~%" =type)
   !eval! (send-message `(:announce :player-type =type :status "scanning"))
   +manual>
      cmd press-key
      key "s"  ; Press 's' as a dummy key (shape change for rect, minimal effect for disc)
   =goal>
      state scanning-environment
      substate "init"
   =imaginal>  ; Keep player info in imaginal buffer
)

;;; Scan for platforms but skip very large ones (walls)
(p scan-for-platforms
   =goal>
      state scanning-environment
      substate "init"
   ?visual-location>
      state free
==>
   !eval! (format t "~%Scanning for platforms in environment~%")
   +visual-location>
      isa visual-location
      value "platform"
      :attended nil  ; Only unattended platforms
   =goal>
      state scanning-environment
      substate "platforms"
)

;;; Process platform locations
(p process-platform
   =goal>
      state scanning-environment
      substate "platforms"
   =visual-location>
      isa visual-location
   ?visual>
      state free
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state mapping-environment
      substate "platforms"
)

;;; Map platform details with condition to skip large walls
(p map-platform-details
   =goal>
      state mapping-environment
      substate "platforms"
   =visual>
      isa polygon
      value "platform"
      screen-x =x
      screen-y =y
      width =width
      height =height
   ?manual>
      state free
==>
   !eval! (format t "~%Mapped platform at (~A,~A) with size ~A x ~A~%" =x =y =width =height)
   ;; If it's a large wall (height > 20), we won't spend time processing it
   !eval! (if (> =height 20) 
             (format t "~%Large wall detected - skipping detailed mapping~%")
             (format t "~%Regular platform mapped for navigation~%"))
   +manual>
      cmd press-key
      key "d"  ; Move right to continue exploration
   =goal>
      state scanning-environment
      substate "diamonds"  ; Skip directly to diamond finding for efficiency
)

;;; Strategic diamond approach with specific target
(p direct-diamond-approach
   =goal>
      state =any-state
      - state approaching-specific-target
   ?manual>
      state free
==>
   !eval! (format t "~%CRITICAL: Breaking loop - targeting specific diamond at (20.0, 26.0)~%")
   +manual>
      cmd press-key
      key "w"  ; Stretch horizontally to reach up
   =goal>
      state approaching-specific-target
      substate "stretching"
)

;;; Rect player stretching sequence for specific diamond
(p rect-specific-stretch-sequence
   =goal>
      state approaching-specific-target
      substate "stretching"
   ?manual>
      state free
==>
   !eval! (format t "~%RECT player stretching to reach diamond at (20,26)~%")
   +manual>
      cmd press-key
      key "w"  ; Stretch even more horizontally
   =goal>
      state approaching-specific-target
      substate "check"
)

;;; Check if we collected and try vertical positioning
(p rect-position-for-collection
   =goal>
      state approaching-specific-target
      substate "check"
   ?manual>
      state free
==>
   !eval! (format t "~%Trying to position under diamond~%")
   +manual>
      cmd press-key
      key "d"  ; Move slightly right to position under diamond
   =goal>
      state approaching-specific-target
      substate "final-stretch"
)

;;; Final stretch to collect
(p rect-final-stretch-collect
   =goal>
      state approaching-specific-target
      substate "final-stretch"
   ?manual>
      state free
==>
   !eval! (format t "~%Final stretch to collect diamond~%")
   +manual>
      cmd press-key
      key "w"  ; Stretch to maximum to collect
   =goal>
      state verify-collection
      substate "checking"
)

;;; Verify collection
(p verify-collection
   =goal>
      state verify-collection
      substate "checking"
   ?manual>
      state free
==>
   !eval! (format t "~%Verifying diamond collection~%")
   +manual>
      cmd press-key
      key "s"  ; Compress back to normal to verify collection
   =goal>
      state scanning-environment
      substate "diamonds"
)

;;; Emergency reset for stuck states
(p emergency-reset
   =goal>
   =temporal>
      ticks =t
   !eval! (> =t 140)  ; If time is very advanced, force reset
==>
   !eval! (format t "~%!!! EMERGENCY RESET !!!~%")
   =goal>
      state approaching-specific-target
      substate "stretching"
)

;;; Force transition to diamond approach
(p force-approach-after-detection
   =goal>
      state detecting-diamonds
      substate "searching"
   =visual-location>
      isa visual-location
      value "diamond"
      screen-x =x
      screen-y =y
   ?manual>
      state free
==>
   !eval! (format t "~%Found diamond at (~A,~A) - forcing approach~%" =x =y)
   +manual>
      cmd press-key
      key "w"  ; Try jumping/expanding to reach it
   =goal>
      state approaching-target
      substate "moving"
)

;;; Specialized rect player approach
(p rect-approach-diamond
   =goal>
      state approaching-target
      substate "moving"
   =visual-location>
      isa visual-location
      value "diamond"
      screen-x =diamond-x
   =retrieval>
      isa player-info
      type "rect"
   =visual>
      isa polygon
      screen-x =current-x
   ?manual>
      state free
==>
   !eval! (let ((direction (if (< =current-x =diamond-x) "d" "a")))
            (format t "~%RECT player moving ~A toward diamond at ~A~%" 
                   direction =diamond-x))
   +manual>
      cmd press-key
      key =direction
   =goal>
      state approaching-target
      substate "stretching"
)

;;; Rect player stretching to reach diamond
(p rect-stretch-for-diamond
   =goal>
      state approaching-target
      substate "stretching"
   =retrieval>
      isa player-info
      type "rect"
   ?manual>
      state free
==>
   !eval! (format t "~%RECT player stretching to reach diamond~%")
   +manual>
      cmd press-key
      key "w"  ; Stretch horizontally
   =goal>
      state approaching-target
      substate "testing-collection"
)

;;; Check if we collected the diamond after stretching
(p check-collection-after-stretch
   =goal>
      state approaching-target
      substate "testing-collection"
   ?manual>
      state free
==>
   !eval! (format t "~%Checking if diamond was collected~%")
   +manual>
      cmd press-key
      key "s"  ; Press any key to get scene update
   =goal>
      state approaching-target
      substate "verification"
)

;;; Timeout for approaching target
(p approach-target-timeout
   =goal>
      state approaching-target
      substate =any-substate
   =temporal>
      ticks =t
   !eval! (> =t 10)  ; If we've been stuck for more than 10 time units
   ?manual>
      state free
==>
   !eval! (format t "~%Timeout in diamond approach - resetting strategy~%")
   +manual>
      cmd press-key
      key "a"  ; Try moving left to get a different approach
   =goal>
      state scanning-environment  ; Reset to scanning to try again
      substate "diamonds"
)

;;; Specific handler for large wall-like platforms
(p handle-large-wall-platform
   =goal>
      state mapping-environment
      substate "platforms"
   =visual>
      isa polygon
      value "platform"
      height =height
   !eval! (> =height 20)  ; This is a wall
   ?manual>
      state free
==>
   !eval! (format t "~%Large wall detected (~A height) - moving away from wall~%" =height)
   +manual>
      cmd press-key
      key "d"  ; Move right (away from wall)
   =goal>
      state scanning-environment
      substate "diamonds"  ; Skip to diamond search
)

;;; Continue scanning for more platforms
(p continue-platform-scan
   =goal>
      state scanning-environment
      substate "continue-platforms"
   ?visual-location>
      state free
==>
   +visual-location>
      isa visual-location
      value "platform"
      :nearest current-location
   =goal>
      state scanning-environment
      substate "platforms"
)

;;; Transition to diamond scanning when platform scanning complete
(p platform-scan-complete
   =goal>
      state scanning-environment
      substate "continue-platforms"
   ?visual-location>
      state error
==>
   !eval! (format t "~%Platform scanning complete, looking for diamonds~%")
   =goal>
      state scanning-environment
      substate "diamonds"
)

;;; Scan for diamonds
(p scan-for-diamonds
   =goal>
      state scanning-environment
      substate "diamonds"
   ?visual-location>
      state free
==>
   +visual-location>
      isa visual-location
      value "diamond"
   =goal>
      state detecting-diamonds
)

;;; Direct transition to diamond search after minimal platform scanning
(p prioritize-diamond-search
   =goal>
      state scanning-environment
      substate =any-platform-substate
   =visual>  ; Having any platform in visual buffer
      isa polygon
      value "platform"
   ?manual>
      state free
==>
   !eval! (format t "~%Prioritizing diamond search over extensive platform mapping~%")
   +manual>
      cmd press-key
      key "d"  ; Move right for exploration
   =goal>
      state scanning-environment
      substate "diamonds"
)

;;; Process diamond location
(p process-diamond-location
   =goal>
      state detecting-diamonds
   =visual-location>
      isa visual-location
      screen-x =x
      screen-y =y
   ?visual>
      state free
   ?imaginal>
      state free
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   +imaginal>
      isa diamond-location
      id "diamond-1"  ; Simple ID, would need sequencing in real system
      x-pos =x
      y-pos =y
      distance nil    ; Will calculate later
      reachable nil   ; Will evaluate later
      assigned-to nil ; Not assigned yet
   =goal>
      state calculating-distances
)

;;; Calculate distance to diamond
(p calculate-diamond-distance
   =goal>
      state calculating-distances
   =imaginal>
      isa diamond-location
      x-pos =diamond-x
      y-pos =diamond-y
   =visual>
      isa polygon
   ?retrieval>
      state free
==>
   +retrieval>
      isa player-info
   =goal>
      state prioritizing-diamonds
   =imaginal>
)

;;; Evaluate diamond reachability based on player type
(p evaluate-diamond-reachability-disc
   =goal>
      state prioritizing-diamonds
   =imaginal>
      isa diamond-location
      x-pos =diamond-x
      y-pos =diamond-y
   =retrieval>
      isa player-info
      type "disc"
==>
   !eval! (let ((reachable (if (< =diamond-y 30) "reachable" "unreachable")))
            (format t "~%Diamond at (~A,~A) is ~A for disc player~%" =diamond-x =diamond-y reachable)
            (set-slot-value =imaginal 'reachable reachable))
   =goal>
      state planning-collection
   =imaginal>
)

;;; Evaluate diamond reachability for rectangle player
(p evaluate-diamond-reachability-rect
   =goal>
      state prioritizing-diamonds
   =imaginal>
      isa diamond-location
      x-pos =diamond-x
      y-pos =diamond-y
   =retrieval>
      isa player-info
      type "rect"
==>
   !eval! (let ((reachable (if (< =diamond-y 10) "reachable" "unreachable")))
            (format t "~%Diamond at (~A,~A) is ~A for rectangle player~%" =diamond-x =diamond-y reachable)
            (set-slot-value =imaginal 'reachable reachable))
   =goal>
      state planning-collection
   =imaginal>
)

;;; After evaluating diamond, share info with other player
(p share-diamond-information
   =goal>
      state planning-collection
   =imaginal>
      isa diamond-location
      id =id
      x-pos =x
      y-pos =y
      reachable =reachability
   =retrieval>
      isa player-info
      type =my-type
   ?manual>
      state free
==>
   !eval! (format t "~%Sharing diamond information with other player~%")
   !eval! (send-message `(:diamond-info :id ,=id :position (:x ,=x :y ,=y) 
                         :reachable ,=reachability :reported-by ,=my-type))
   +manual>
      cmd press-key
      key "s"  ; Dummy key press to trigger scene update
   =goal>
      state communicating
      substate "waiting-reply"
      last-message "diamond-info"
   =imaginal>  ; Keep diamond info in imaginal buffer
)

;;; Propose collection assignment based on reachability
(p propose-collection-assignment
   =goal>
      state communicating
      substate "waiting-reply"
      last-message "diamond-info"
   =imaginal>
      isa diamond-location
      id =id
      reachable "reachable"  ; Only propose for diamonds we can reach
   =retrieval>
      isa player-info
      type =my-type
   ?manual>
      state free
==>
   !eval! (format t "~%Proposing to collect diamond ~A myself~%" =id)
   !eval! (send-message `(:propose-collection :diamond-id ,=id :collector ,=my-type))
   +manual>
      cmd press-key
      key "s"  ; Dummy key press to trigger scene update
   =goal>
      state communicating
      substate "waiting-assignment-confirm"
   =imaginal>  ; Keep diamond info
)

;;; Look for other player to coordinate
(p locate-other-player
   =goal>
      state communicating
      substate "waiting-assignment-confirm"
   ?visual-location>
      state free
   =retrieval>
      isa player-info
      type "disc"
==>
   !eval! (format t "~%Looking for rect player to coordinate~%")
   +visual-location>
      isa visual-location
      value "rect"
   =goal>
      state coordinating-action
)

;;; Look for other player to coordinate (if we're rect player)
(p locate-other-player-disc
   =goal>
      state communicating
      substate "waiting-assignment-confirm"
   ?visual-location>
      state free
   =retrieval>
      isa player-info
      type "rect"
==>
   !eval! (format t "~%Looking for disc player to coordinate~%")
   +visual-location>
      isa visual-location
      value "disc"
   =goal>
      state coordinating-action
)

;;; Store information about other player
(p store-other-player-info
   =goal>
      state coordinating-action
   =visual-location>
      isa visual-location
   ?visual>
      state free
==>
   +visual>
      isa move-attention
      screen-pos =visual-location
   =goal>
      state planning-collection
)

;;; Record other player's position
(p record-other-player-position
   =goal>
      state planning-collection
   =visual>
      isa polygon  ; Could be disc or rect
      screen-x =other-x
      screen-y =other-y
      diamonds =other-diamonds
   ?imaginal-state>
      state free
==>
   +imaginal>
      isa other-player-info
      position-x =other-x
      position-y =other-y
      diamonds =other-diamonds
   =goal>
      state approaching-target
)

;;;
;;; DIAMOND COLLECTION PHASE
;;;

;;; Begin approaching target diamond
(p begin-approach-to-diamond
   =goal>
      state approaching-target
   =imaginal>
      isa diamond-location
      x-pos =target-x
      y-pos =target-y
      reachable "reachable"
   =retrieval>
      isa player-info
      type =player-type
   ?manual>
      state free
==>
   !eval! (format t "~%Beginning approach to diamond at (~A,~A)~%" =target-x =target-y)
   !eval! (send-message `(:status :approaching-diamond :position (:x ,=target-x :y ,=target-y)))
   +manual>
      cmd press-key
      key "s"  ; Dummy key press to trigger scene update
   =goal>
      state approaching-target
      substate "moving"
   =imaginal>  ; Keep diamond info
)

;;; Move right towards diamond
(p move-right-to-diamond
   =goal>
      state approaching-target
      substate "moving"
   =imaginal>
      isa diamond-location
      x-pos =target-x
   =visual>  ; This would be the player's position
      isa polygon
      screen-x =current-x
   !eval! (< =current-x =target-x)  ; Diamond is to the right
   ?manual>
      state free
==>
   !eval! (format t "~%Moving right towards diamond~%")
   +manual>
      cmd press-key
      key "d"
   =goal>
      state approaching-target
      substate "continuing"
   =imaginal>  ; Keep diamond info
)

;;; Move left towards diamond
(p move-left-to-diamond
   =goal>
      state approaching-target
      substate "moving"
   =imaginal>
      isa diamond-location
      x-pos =target-x
   =visual>  ; This would be the player's position
      isa polygon
      screen-x =current-x
   !eval! (> =current-x =target-x)  ; Diamond is to the left
   ?manual>
      state free
==>
   !eval! (format t "~%Moving left towards diamond~%")
   +manual>
      cmd press-key
      key "a"
   =goal>
      state approaching-target
      substate "continuing"
   =imaginal>  ; Keep diamond info
)

;;; Jump if needed (for disc player)
(p jump-for-diamond-disc
   =goal>
      state approaching-target
      substate "continuing"
   =imaginal>
      isa diamond-location
      y-pos =target-y
   =visual>  ; Current player position
      isa polygon
      screen-y =current-y
   =retrieval>
      isa player-info
      type "disc"
   !eval! (> =target-y (+ =current-y 2))  ; Diamond is significantly higher
   ?manual>
      state free
==>
   !eval! (format t "~%Jumping to reach higher diamond~%")
   +manual>
      cmd press-key
      key "w"
   =goal>
      state approaching-target
      substate "moving"
   =imaginal>  ; Keep diamond info
)

;;; Expand horizontally for rect player
(p expand-horizontally-rect
   =goal>
      state approaching-target
      substate "continuing"
   =retrieval>
      isa player-info
      type "rect"
   ?manual>
      state free
==>
   !eval! (format t "~%Expanding horizontally to reach diamond~%")
   +manual>
      cmd press-key
      key "w"
   =goal>
      state approaching-target
      substate "moving"
   =imaginal>  ; Keep diamond info
)

;;; Check if diamond was collected
(p check-diamond-collection
   =goal>
      state approaching-target
      substate "continuing"
   =retrieval>
      isa player-info
      type =player-type
      diamonds =old-count
   =visual>  ; Current player state
      isa polygon
      diamonds =new-count
   !eval! (> =new-count =old-count)  ; Diamond count increased
   ?manual>
      state free
==>
   !eval! (format t "~%Diamond collected! Count increased from ~A to ~A~%" =old-count =new-count)
   !eval! (send-message `(:diamond-collected :collector ,=player-type :count ,=new-count))
   +manual>
      cmd press-key
      key "s"  ; Dummy key press to trigger scene update
   =goal>
      state scanning-environment  ; Return to scanning for more diamonds
      substate "init"
   +imaginal>  ; Update player info with new diamond count
      isa player-info
      type =player-type
      diamonds =new-count
)

;;; Continue approach after movement
(p continue-diamond-approach
   =goal>
      state approaching-target
      substate "continuing"
   ?manual>
      state free
==>
   =goal>
      state approaching-target
      substate "moving"
)

;;;
;;; MESSAGE PROCESSING PHASE
;;;

;;; Check for incoming messages
(p check-for-messages
   =goal>
      state - communicating  ; Any state except communicating
   =visual>
      isa text
      message =msg
      sender =sender
==>
   !eval! (format t "~%Received message from ~A: ~A~%" =sender =msg)
   =goal>
      state processing-message
)

;;; Process diamond info message
(p process-diamond-info-message
   =goal>
      state processing-message
   =visual>
      isa text
      message =msg
   !eval! (and (listp =msg) (eq (first =msg) :diamond-info))
==>
   !eval! (format t "~%Processing diamond info message~%")
   =goal>
      state communicating
      substate "responding-to-info"
)

;;; Process proposal message
(p process-proposal-message
   =goal>
      state processing-message
   =visual>
      isa text
      message =msg
   !eval! (and (listp =msg) (eq (first =msg) :propose-collection))
   ?manual>
      state free
==>
   !eval! (format t "~%Processing collection proposal~%")
   !eval! (send-message '(:accept-proposal :response "agreed"))
   +manual>
      cmd press-key
      key "s"  ; Dummy key press to trigger scene update
   =goal>
      state scanning-environment  ; Return to environment scanning
      substate "init"
)

;;; Process status update from other player
(p process-status-update
   =goal>
      state processing-message
   =visual>
      isa text
      message =msg
   !eval! (and (listp =msg) (eq (first =msg) :status))
==>
   !eval! (format t "~%Received status update from other player~%")
   =goal>
      state scanning-environment  ; Continue with own tasks
      substate "init"
)

;;; Process diamond collection notification
(p process-collection-notification
   =goal>
      state processing-message
   =visual>
      isa text
      message =msg
   !eval! (and (listp =msg) (eq (first =msg) :diamond-collected))
==>
   !eval! (format t "~%Other player collected a diamond!~%")
   =goal>
      state scanning-environment  ; Look for remaining diamonds
      substate "diamonds"
)

;;;
;;; OBSTACLE AVOIDANCE PHASE
;;;

;;; Detect obstacle in path
(p detect-obstacle
   =goal>
      state approaching-target
      substate "moving"
   =visual-location>
      isa visual-location
      value "platform"
   !eval! (< (abs (- =visual-location>screen-x =target-x)) 5)  ; Platform is in the path
==>
   !eval! (format t "~%Detected obstacle in path to diamond~%")
   =goal>
      state avoiding-obstacle
)

;;; Jump over obstacle (disc player)
(p jump-over-obstacle-disc
   =goal>
      state avoiding-obstacle
   =retrieval>
      isa player-info
      type "disc"
   ?manual>
      state free
==>
   !eval! (format t "~%Jumping over obstacle~%")
   +manual>
      cmd press-key
      key "w"
   =goal>
      state approaching-target
      substate "moving"
)

;;; Try to go around obstacle (rect player)
(p go-around-obstacle-rect
   =goal>
      state avoiding-obstacle
   =retrieval>
      isa player-info
      type "rect"
   ?manual>
      state free
==>
   !eval! (format t "~%Trying to reshape to go around obstacle~%")
   +manual>
      cmd press-key
      key "s"  ; Reshape to be thinner
   =goal>
      state approaching-target
      substate "moving"
)

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