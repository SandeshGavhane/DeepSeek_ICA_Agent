;; Navigation functions for the GeoMates environment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Platform gap detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-gap-for-rect (rect-x rect-y rect-width rect-height)
  "Find platform gaps at the level of rect-bottom.
   Returns the left and right x-coordinates of the gap.
   Debug version: only handles specific cases with buffer of 2."
  (let* ((rect-bottom (- rect-y (/ rect-height 2)))
         (buffer 2))
    
    (cond
      ;; If rect-bottom is around 21 (with buffer of 2)
      ((and (>= rect-bottom (- 21 buffer))
            (<= rect-bottom (+ 21 buffer)))
       (values 30 40))
      
      ;; If rect-bottom is around 1 (with buffer of 2)
      ((and (>= rect-bottom (- 1 buffer))
            (<= rect-bottom (+ 1 buffer)))
       (values 0 0))
      
      ;; Default case
      (t (values 0 0)))))

(defun find-gap-for-disc (disc-x disc-y)
  "Find platform gaps that are relevant for disc navigation.
   Returns the left and right x-coordinates of the gap.
   Uses disc's y-position instead of rect-bottom to determine relevant gaps."
  (let* ((disc-y-level disc-y)
         (buffer 2))
    
    ;; For now, we'll use the same logic as the rectangle version
    ;; but this could be customized for disc-specific detection
    (cond
      ;; If disc is around y=21 (with buffer of 2)
      ((and (>= disc-y-level (- 21 buffer))
            (<= disc-y-level (+ 21 buffer)))
       (values 30 40))
      
      ;; If disc is around y=1 (with buffer of 2)
      ((and (>= disc-y-level (- 1 buffer))
            (<= disc-y-level (+ 1 buffer)))
       (values 0 0))
      
      ;; Default case
      (t (values 0 0)))))

(defun is-rect-between (disc-x disc-y diamond-x diamond-y rect-x rect-y rect-height)
  "Check if rectangle is between disc and diamond.
   Returns true if rectangle blocks the path, false otherwise."
  (let ((rect-top (+ rect-y (/ rect-height 2))))
    (or 
     ;; Disc is left of diamond with rectangle in between
     (and (< disc-x diamond-x) 
          (> rect-x disc-x) 
          (< rect-x diamond-x)
          (< disc-y rect-top))
     ;; Disc is right of diamond with rectangle in between
     (and (> disc-x diamond-x) 
          (< rect-x disc-x) 
          (> rect-x diamond-x)
          (< disc-y rect-top)))))

(defun retarget-jump-to-rect (disc-x disc-y rect-x rect-y rect-width rect-height)
  "Calculate new target coordinates to jump over the rectangle.
   Returns new target-x and target-y values."
  (let* ((rect-left (- rect-x (/ rect-width 2)))
         (rect-right (+ rect-x (/ rect-width 2)))
         (rect-top (+ rect-y (/ rect-height 2))))
    (cond
     ;; If disc is left of rectangle
     ((< disc-x rect-x)
      (values (- rect-left 2) (+ rect-top 2)))
     ;; If disc is right of rectangle
     ((> disc-x rect-x)
      (values (+ rect-right 2) (+ rect-top 2)))
     ;; Default case (shouldn't reach here in normal usage)
     (t (values disc-x (+ rect-top 2))))))

(defun retarget-jump-over-gap (disc-x disc-y diamond-x diamond-y rect-x rect-y rect-width rect-height)
  "Calculate new target coordinates to jump over a gap between platforms.
   Returns new target-x and target-y values."
  (multiple-value-bind (gap-left gap-right)
      (find-gap-for-disc disc-x disc-y)
    (let* ((rect-bottom (- rect-y (/ rect-height 2)))
           ;; Jump height needed to safely clear the gap
           (jump-height (+ rect-bottom 5)))
      (cond
       ;; If diamond is right and disc is left
       ((and (> diamond-x disc-x) (< disc-x gap-left))
        (values (- gap-left 20) jump-height))
       ;; If diamond is left and disc is right
       ((and (< diamond-x disc-x) (> disc-x gap-right))
        (values (+ gap-right 20) jump-height))
       ;; Default case (shouldn't reach here in normal usage)
       (t (values disc-x jump-height))))))

(defun is-gap-between-for-rect (rect-x rect-y rect-width rect-height target-x)
  "Check if there's a gap that the rectangle needs to consider when moving toward target-x.
   Returns true if a gap exists in the path, false otherwise."
  (let* ((rect-bottom (- rect-y (/ rect-height 2))))
    (multiple-value-bind (gap-left gap-right)
        (find-gap-for-rect rect-x rect-y rect-width rect-height)
      (let ((gap-exists (> gap-right gap-left))
            (rect-moving-right (< rect-x target-x))
            (rect-moving-left (> rect-x target-x)))
        ;; Check if:
        ;; 1. A gap exists
        ;; 2. The rectangle needs to cross the gap to reach the target
        (and gap-exists
             (or 
              ;; Rectangle moving right needs to cross gap
              (and rect-moving-right
                   (< rect-x gap-left)
                   (> target-x gap-right))
              ;; Rectangle moving left needs to cross gap
              (and rect-moving-left
                   (> rect-x gap-right)
                   (< target-x gap-left))))))))

(defun is-gap-between-for-disc (disc-x disc-y target-x target-y rect-x rect-y rect-width rect-height)
  "Check if there's a gap between the disc and target that needs to be considered.
   Returns true if a gap exists in the path, false otherwise."
  (multiple-value-bind (gap-left gap-right)
      (find-gap-for-disc disc-x disc-y)
    (let ((gap-exists (> gap-right gap-left))
          (disc-moving-right (< disc-x target-x))
          (disc-moving-left (> disc-x target-x)))
      ;; Check if:
      ;; 1. A gap exists
      ;; 2. The disc needs to cross the gap to reach the target
      (and gap-exists
           (or 
            ;; Disc moving right needs to cross gap
            (and disc-moving-right
                 (< disc-x gap-left)
                 (> target-x gap-right))
            ;; Disc moving left needs to cross gap
            (and disc-moving-left
                 (> disc-x gap-right)
                 (< target-x gap-left)))))))

(defun find-next-action-disc-queue (diamond-x diamond-y disc-x disc-y rect-x rect-y rect-width rect-height)
  "Determine the next action for the disc to move toward the diamond.
   Returns one of: 'w:move-up, 's:move-down, 'a:move-left, 'd:move-right, 'right-high-jump, 'left-high-jump, or nil if at the target.
   Note: actions are string could be WD, wwww etc, which means the agent will keep moving
   WD means the agent will move up and right later
   wwww means the agent will keep moving up by 4 times
   "
  
  (let* (
         (disc-radius 1)
         (buffer-distance 3)
         (rect-left (- rect-x (/ rect-width 2)))
         (rect-right (+ rect-x (/ rect-width 2)))
         (rect-top (+ rect-y (/ rect-height 2)))
         (target-x diamond-x) ;; Set target-x to diamond-x as first step
         (target-y diamond-y) ;; Set target-y to diamond-y as first step
        )
    
    ;; Check obstacles and retarget if needed
    (cond
      ;; Check if there's a gap between disc and diamond
      ;; todo : implment this throw unknow exception, todo it later
      ;; ((is-gap-between-for-disc disc-x disc-y diamond-x diamond-y rect-x rect-y rect-width rect-height)
      ;;  (multiple-value-bind (new-target-x new-target-y)
      ;;      (retarget-jump-over-gap disc-x disc-y diamond-x diamond-y rect-x rect-y rect-width rect-height)
      ;;    (setq target-x new-target-x
      ;;          target-y new-target-y)))
      
      ;; Check if rectangle is between disc and diamond
      ((is-rect-between disc-x disc-y diamond-x diamond-y rect-x rect-y rect-height)
       (multiple-value-bind (new-target-x new-target-y)
           (retarget-jump-to-rect disc-x disc-y rect-x rect-y rect-width rect-height)
         (setq target-x new-target-x
               target-y new-target-y)))
             
      ;; If disc is above rect-top, target remains diamond
      ((> disc-y rect-top)
       (setq target-x diamond-x
             target-y diamond-y)))

    (cond
      ;; todo :
      ;; 1. if disc and diamond are some platfrom, then do
      ;; 2. is disc is above/under diamond platfrom, then do move to the platform
      ((and (< disc-y target-y)
            (< (abs (- disc-x target-x)) buffer-distance))
       (cond ((< disc-x target-x) "w.d")
             ((> disc-x target-x) "w.a") 
             (t "w.")))

      ;; Regular horizontal movement
      ((< disc-x target-x) "d.")
      ((> disc-x target-x) "a.")
      
      ;; Default case
      (t "w."))))

(defun find-next-action-rect-queue (rect-x rect-y rect-width rect-height diamond-x diamond-y)
  "Determine the next action for the rectangle to move toward the diamond."
  (let* ((rect-right (+ rect-x (/ rect-width 2)))
         (rect-left (- rect-x (/ rect-width 2)))
         (rect-top (+ rect-y (/ rect-height 2)))
        ;;  (rect-bottom (- rect-y (/ rect-height 2)))
         (stable-width (/ rect-width 4)))
    
    (multiple-value-bind (platform-gap-x-left platform-gap-x-right)
        (find-gap-for-rect rect-x rect-y rect-width rect-height)
    
      (let ((gap-size (- platform-gap-x-right platform-gap-x-left))
            (gap-center (/ (+ platform-gap-x-left platform-gap-x-right) 2)))
        (cond
          ;; NEW: Check if diamond is below and try to use gap
          ((and (< diamond-y rect-y)                    ; Diamond is below
                (> gap-size 0)                          ; Gap exists
                ) 
           (cond ((< (abs (- rect-x gap-center)) 4) "wwwwww")
                ((< rect-x gap-center) "dddddd")   ; Move right towards gap
                (t "aaa")))                     ; Move left towards gap
                ;; note: ddddddd vs aaa is by purpose, to preovent rect is block rotate at the gap
          
          ;; Your existing conditions remain unchanged
          ((and (= rect-x diamond-x) (= rect-y diamond-y)) nil)
          
          ((< rect-right diamond-x) 
           (if (> rect-right platform-gap-x-right)
               "dddd"
               (if (and (> gap-size 0) (> gap-size stable-width))
                   "ssss"
                   "dddd")))
          
          ((> rect-left diamond-x) 
           (if (< rect-left platform-gap-x-left)
               "aaaa"
               (if (and (> gap-size 0) (> gap-size stable-width))
                   "ssss"
                   "aaaa")))
          
          ((< rect-top diamond-y) "wwww")
          (t "s"))))))

(defun convert-wasd-to-move (action)
            "Convert WASD string to move action symbols"
            (cond ((string= action "w") 'move-up)
                  ((string= action "s") 'move-down) 
                  ((string= action "a") 'move-left)
                  ((string= action "d") 'move-right)
                  ((string= action ".") 'wait-up)
                  (t nil)))          


(defun get-query-move-type (next-action-queue)
  "Determine the next intention based on action queue length.
   Returns 'queue-query-move if queue has items, 'reload-query-move otherwise."
  (if (>= (length next-action-queue) 1)
      'queue-query-move
      'reload-query-move))