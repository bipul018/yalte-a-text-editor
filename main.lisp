(ql:quickload :cl-raylib)
(ql:quickload :cffi)
(ql:quickload :bordeaux-threads)

(defpackage :editr
  ;(:use :cl)
  (:use :cl :cl-user)
  (:local-nicknames (:rl :raylib)))
(in-package :editr)

(defun random-item (node-wt-list)
  "Accepts a list of node and weight cons cell and returns the random choice from that"
  (let* ((total-weight (loop for node in node-wt-list
			     for wt = (cdr node)
			     with sum = 0.0
			     do (incf sum wt)
			     finally (return sum)))
	 (cumulative-weight (loop for node in node-wt-list
				  for wt = (cdr node)
				  with sum = 0.0
				  collect (/ (+ wt sum) total-weight)
				  do (incf sum wt))))
    (loop named daloop
	  with rand-wt = (random 1.0)
	  for node in node-wt-list
	  for cwt in cumulative-weight
	  when (<= rand-wt cwt)
	  do (return-from daloop (car node))
	  finally (return-from daloop (car node)))))

;; First split by newlines
;; Store by newlines
;; When writing to files, convert accordingly

;; So text (for now) = list of strings
(defun measure-text (text font-size)
  " A raw text measuring fxn "
  ;;(rl:measure-text-ex rl:load-font-ex(
  (rl:measure-text text font-size))

(defun get-char-width (char font-size)
  ;;(declaim (type (standard-char char)
;;		 (number font-size)))
  (unless (char= char #\newline)
    (if (char= char #\return) 0
	(let ((one-sp-len (measure-text " " font-size))
	      (two-sp-len (measure-text "  " font-size)))
	  (+ (measure-text (format nil "~a" char) font-size)
	     two-sp-len
	     (* -2 one-sp-len))))))

;; Expects the text entries to not have any newlines 
(defun render-text (rect text font-size color)
  ;;(declaim (type (rl::rectangle rect)
  ;;	 (number font-size)))
  (let ((r.x (rl:rectangle-x rect))
	(r.y (rl:rectangle-y rect))
	(r.w (rl:rectangle-width rect))
	(r.h (rl:rectangle-height rect)))
      (let ((p.x r.x) (p.y r.y))
	(loop for ln in text
	      do (loop for ch across ln
		    for ch-wid = (get-char-width ch font-size)
		    do (when (>= (+ ch-wid p.x) r.w)
			 (setf p.x r.x)
			 (incf p.y font-size))
		    do (rl:draw-text
			(format nil "~a" ch)
			p.x p.y font-size color)
		    do (unless (>= (+ ch-wid p.x) r.w)
			 (incf p.x ch-wid)))
	      do (incf p.y font-size)
	      do (setf p.x r.x)
	      while (< p.y r.h)))))


;; An auxiliary 'position map' building fxn
;; Given a desired width and font size and ... (later maybe some settings such as orientation)
;;   returns a parallel kind of list of vector2
;; Well, not exactly parallel, each line must have one more position
(defun get-render-positions (text-lines width font-size)
  (let ((p.x 0) (p.y 0))
	(loop for ln in text-lines
	      ;; The additonal 'null' at the end is a hack
	      collect (loop for ch across (format nil "~a~a" ln (code-char 0))
		    for ch-wid = (get-char-width ch font-size)
		    do (when (>= (+ ch-wid p.x) width)
			 (setf p.x 0)
			 (incf p.y font-size))
			    collect (cons p.x p.y)
		    do (unless (>= (+ ch-wid p.x) width)
			 (incf p.x ch-wid)))
	      do (incf p.y font-size)
	      do (setf p.x 0))))
	      ;;while (< p.y r.h))))

;; Draw text but using pre-generated positions
(defun render-text-pos (rect text-lines text-poses font-size color)
  ;; First find the first visible line
  ;; Then start to draw character by character
  ;; Draw until the line is still visible (more efficient on rendering)
  ;; TODO:: For now, ignore it all
  (loop for ln  in text-lines
	for lnp in text-poses
	do (loop for ch  across ln
		 for chp in lnp
		 do (rl:draw-text
		     (format nil "~a" ch)
		     (+ (car chp) (rl:rectangle-x rect))
		     (+ (cdr chp) (rl:rectangle-y rect))
		     font-size color))))

(defun render-text-2 (rect text-lines font-size color)
  (render-text-pos rect text-lines
		   (get-render-positions text-lines (rl:rectangle-width rect) font-size)
		   font-size color))

;; Gives previous and next cursors
(defun offset-cursor-fwd (curr-pos text-lines-or-poses offset)
  ;; Assume offset is +ve
  ;; If at the end of this line, move pos.y, else move pos.x
  
  )

;; Draw the cursor
;;TODO:: Current model doesnot care much about inter-char spacing, fix that
(defun render-cursor (text-poses cursor begin-pos font-size color &key (blink-ms 500))
  (when (<= (/ (rem (* 1000.0 (rl:get-time)) blink-ms) blink-ms) 0.5)
    (let* ((line-n (cdr cursor))
	   (line (nth line-n text-poses))
	   (offset (min (car cursor) (- (length line) 1)))
	   (pos (nth offset line))
	   (p.x (car pos))
	   (p.y (cdr pos)))
      ;; start x,y is p.x,p.y
      ;; end y is p.y + font-size
      ;; for end x, if end of line, do something else
      ;;  else, end x is begin x of next char
      (rl:draw-rectangle
       (+ (car begin-pos) p.x -1) (+ (cdr begin-pos) p.y)
       (+ 1 (if (>= (+ 1 offset) (length line))
		10
		(- (car (nth (+ 1 offset) line)) p.x)))
       font-size
       color))))
		 
;; Function to pre-process a single bulk text
(defun pre-process-text (src)
  ;; TODO:: Later make this process those non-ascii characters also
  (reverse (reduce (lambda (ans ch)
	    (if (char= ch #\return) ans
		(if (char= ch #\newline) (cons "" ans)
		    (cons (format nil "~a~a" (car ans) ch) (cdr ans)))))
	  src :initial-value (list ""))))

;; A fxn that reads the file (just uiop wrapper)

;; A fxn that writes onto the file
(defun write-lines-to-file (lines filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (line lines)
      (format stream "~A~%" line))))

;; Helper that accesses the value directly for a assoc list
(defmacro assoc-val (key assoc-list)
  `(cdr (assoc ,key ,assoc-list)))

;; fxn is called with those args
(defun start-thrd (fxn shared-vars-assoc &rest cargs)
  (bt2:make-thread (lambda ()
		     (let ((shared-vars shared-vars-assoc)
			   (args cargs)
			   (fn fxn))
		       (apply fn (cons shared-vars args))))))

;; A 'subscriber' and 'publisher' mechanism for key presses
(defun collect-key-actions (fxn key-list)
  (loop for key in key-list
	when (funcall fxn key)
	collect key))

;; Given an alist of vars, setfs the given var with mutex locked
;; Expects the mutex to be yet another var with a particular name 'var-lock'
(defun replace-var-locked (var-list var-sym new-val)
  (bt2:with-lock-held ((assoc-val 'var-lock var-list))
    (let ((old-val (assoc-val var-sym var-list)))
      (setf (assoc-val var-sym var-list) new-val)
      old-val)))

;; Used to atomically append items to the list, assumes 'var-sym' is pointing to a list 
(defun append-var-locked (var-list var-sym new-val)
  (bt2:with-lock-held ((assoc-val 'var-lock var-list))
    (let ((old-val (assoc-val var-sym var-list)))
      (setf (assoc-val var-sym var-list) (append old-val new-val))
      old-val)))

"
The replace-var-locked fxn can be used to 'empty' the queue
And the append-var-locked fxn can be used to 'fill' the queue atomically
"

;; A fxn that 'merges' the old and new keys to process
(defun publish-key-evt (var-list key-fn var-name test-keys)
  " A fxn to be used in GUI thread that fills the key event queue "
  (let ((new-keys (collect-key-actions key-fn test-keys)))
    (when new-keys
      (append-var-locked var-list var-name new-keys))))

(defun setup-keys-read (cxt)
  " To be used before running GUI thread, to setup keys, or later maybe not "
  (when (and cxt (cdr cxt) (assoc-val 'var-lock (cdr cxt)))
    (replace-var-locked (cdr cxt) 'key-press-list
			'(:key-right :key-left :key-up :key-down))))

(defun process-key-evt (cxt)
  " To be used in non GUI side, that processes the key events "
  (let ((key-evts (replace-var-locked (cdr cxt) 'key-press-queue nil))
	(cursor-pos (assoc-val 'cursor-pos (cdr cxt)))
	(text-poses (assoc-val 'text-poses (cdr cxt))))
    (loop for key in key-evts
	  do (case key
	       (:key-right (incf (car cursor-pos)))
	       (:key-left (decf (car cursor-pos)))
	       (:key-down (incf (cdr cursor-pos)))
	       (:key-up (decf (cdr cursor-pos))))
	  do (setf (cdr cursor-pos)
		   (min (max 0 (cdr cursor-pos))
			(- (length text-poses) 1))))

    ;; TODO:: Add logic to pan around the screen as well here after that loop
    
    (unless (and (= (car cursor-pos) (car (assoc-val 'cursor-pos (cdr cxt))))
		 (= (cdr cursor-pos) (cdr (assoc-val 'cursor-pos (cdr cxt)))))
      ;; TODO:: The following operation might not need a lock at all
      (replace-var-locked (cdr cxt) 'cursor-pos cursor-pos))
    cursor-pos))

;; NEXT:: Need to add a lock variable, list of key presses subscribed and the notif queue,
;;        cursor pos as variables
;; Use the `for .. = (assoc-val '.. vars)` in the loop for read only shared variable
;; Later, upgrade the text lines and positions to be also shared variables
(defun app (vars file-to-open)
  " vars: win-w win-h bg-col txt-col to-quit var-lock key-press-queue key-press-list cursor-pos text-poses"
  (format t "To open the file `~a`, the vars are: ~a~%" file-to-open vars)
  ;; These vars cannot be updated that easily, they need 'double-buffering'
  (let ((win-w (assoc-val 'win-w vars))
	(win-h (assoc-val 'win-h vars)))
    (rl:with-window (win-w win-h (format nil "file:~a" file-to-open))
      (rl:set-target-fps 60)
      ;; These vars probably need to be made shared variables again later
      (let* ((text-lines (uiop:read-file-lines file-to-open))
	     (font-size 25)
	     (text-begin (cons 10 100))
	     (text-poses (get-render-positions text-lines
					       (- (assoc-val 'win-w vars) (* 2 (car text-begin)))
					       font-size)))
	;; TODO:: After editing part starts, all these are going to be controlled from outside
	(setf (assoc-val 'text-poses vars) text-poses)
	(loop while (not (or (assoc-val 'to-quit vars) (rl:window-should-close)))
	      ;; These vars are to be 'refreshed' every loop, in essence 'hot-reloaded'
	      for cursor-pos = (assoc-val 'cursor-pos vars)
	      for bg-col = (assoc-val 'bg-col vars)
	      for txt-col = (assoc-val 'txt-col vars)
	      for key-press-list = (assoc-val 'key-press-list vars)

	      ;; Update the keybindings
	      do (publish-key-evt vars #'rl:is-key-released 'key-press-queue key-press-list)
	      ;; This setup might techinically cause delay in UI updates, need to research more

	      ;; Draw according to the info available
	      do (rl:with-drawing (rl:clear-background bg-col)
		   (rl:draw-fps 10 10)
		   (render-cursor text-poses cursor-pos text-begin
				  font-size :red)
		   ;; TODO:: Later need to make it so that win-w and win-h are also shared properly
		   (render-text-pos (rl:make-rectangle
				     :x (car text-begin) :y (cdr text-begin)
				     :width (- win-w (* 2 (car text-begin)))
				     :height (- win-h (* 2 (cdr text-begin))))
				    text-lines text-poses font-size txt-col)))
	(setf (assoc-val 'to-quit vars) t)))))


(defun start (file-to-open &key (win-w 800) (win-h 800) (bg-col :raywhite))
  ;; TODO:: Doing this copy-list is not working
  ;;        at the very least, 'cursor-pos' is shared between runs
  ;;        one possible soln is using , for each value, probably it all is set in stone at compile time otherwise
  (let ((vars (copy-list `((win-w . ,win-w) (win-h . ,win-h) (to-quit . nil)
			   (bg-col . ,bg-col) (txt-col . :black)
			   (var-lock . ,(bt2:make-lock))
			   (key-press-queue . ,nil) (key-press-list . ,nil)
			   (cursor-pos . ,(cons 0 0))
			   (text-poses . ())))))
    (let ((cxt (cons (start-thrd 'app vars file-to-open) vars)))
      (setup-keys-read cxt)
      cxt)))

(defun stop (thrd-obj)
  (setf (assoc-val 'to-quit (cdr thrd-obj)) t)
  (bt2:join-thread (car thrd-obj))
  (setf (assoc-val 'to-quit (cdr thrd-obj)) nil)
  (setf (car thrd-obj) nil
	(cdr thrd-obj) nil))

;; A 'key-handler' , a probably temporary solution
(defun run-handler (cxt)
  (start-thrd (lambda (vars cxt)
		(loop while (not (assoc-val 'to-quit (cdr cxt)))
		      do (process-key-evt cxt)
		      ;; A hack to not overwhelm CPU
		      do (sleep 0.08)))
	      '() cxt))
