(ql:quickload :cl-raylib)
(ql:quickload :cffi)
(ql:quickload :bordeaux-threads)
(ql:quickload :3d-vectors)
(ql:quickload :3d-matrices)


;; A mechanism used to 'export' all the math items under a single package
(defpackage :vec-math
  (:use :cl :3d-vectors :3d-matrices))
(in-package :vec-math)
(do-external-symbols (sym (find-package :3d-vectors)) (export sym))
(do-external-symbols (sym (find-package :3d-matrices)) (export sym))

(defpackage :editr
  ;(:use :cl)
  (:use :cl :cl-user)
  (:local-nicknames (:rl :raylib) (:rm :vec-math)))

(load "task-channels.lisp")
(in-package :editr)

;; Helper that accesses the value directly for a assoc list
(defmacro assoc-val (key assoc-list)
  `(cdr (assoc ,key ,assoc-list)))

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

;; The 'app' struct
(defstruct app-args
  (win-w) (win-h) (to-quit)
  (bg-col) (txt-col)
  (var-lock)
  (key-press-queue) (key-press-list)
  (cursor-pos) (text-begin)
  (text-poses) (text-lines)
  (tasks-before-draw) (tasks-top-draw) (tasks-bot-draw))


;; First split by newlines
;; Store by newlines
;; When writing to files, convert accordingly

;; A global 'font', later to modularize better
(defvar *glob-font* nil)
(defun load-font (file-name font-size &optional cxt)
  "
Modifies the global font used, not a good design, I know.
First loads the new font of the given size.
Then, if the context is provided, then also updates that context's font positions
"
  (setf *glob-font*
	(list (cons 'font (rl:load-font-ex file-name (round font-size) (cffi:null-pointer) 0))
	      (cons 'font-size (coerce font-size 'single-float))
	      (cons 'spacing (* (coerce font-size 'single-float) 0.1))))
  (when cxt
    (let* ((vars (cdr cxt))
	   (new-poses (get-render-positions (app-args-text-lines vars)
					    (- (app-args-win-w vars)
					       (* 2 (car (app-args-text-begin vars)))))))
      (setf (app-args-text-poses vars) new-poses))))
;; Use this fxn to access the global font parameter
(defun font-param (param-name)
  (unless *glob-font*
    (load-font "fonts/CascadiaMono.ttf" 25))
  (when *glob-font*
    (assoc-val param-name *glob-font*)))


;; Whenver trying to modify font later on, also provide context

;; So text (for now) = list of strings
(defun measure-text (text)
  " A raw text measuring fxn "
  (rm:vx (rl:measure-text-ex (font-param 'font )
			      text
			      (font-param 'font-size )
			      (font-param 'spacing ))))

(defun get-char-width (char)
  ;;(declaim (type (standard-char char)
;;		 (number font-size)))
  (unless (char= char #\newline)
    (if (char= char #\return) 0
	(let ((one-sp-len (measure-text " "))
	      (two-sp-len (measure-text "  ")))
	  (+ (measure-text (format nil "~a" char))
	     two-sp-len
	     (* -2 one-sp-len))))))

;; Expects the text entries to not have any newlines 
(defun render-text (rect text color)
  ;;(declaim (type (rl::rectangle rect)))
  (let ((r.x (rl:rectangle-x rect))
	(r.y (rl:rectangle-y rect))
	(r.w (rl:rectangle-width rect))
	(r.h (rl:rectangle-height rect)))
      (let ((p.x r.x) (p.y r.y))
	(loop for ln in text
	      do (loop for ch across ln
		       for ch-wid = (get-char-width ch)
		       do (when (>= (+ ch-wid p.x) r.w)
			    (setf p.x r.x)
			    (incf p.y (font-param 'font-size )))
		       do (rl:draw-text-ex (font-param 'font )
					   (format nil "~a" ch)
					   (rm:vec2 p.x p.y)
					   (font-param 'font-size )
					   (font-param 'spacing )
					   color)
		       do (unless (>= (+ ch-wid p.x) r.w)
			    (incf p.x ch-wid)))
	      do (incf p.y (font-param 'font-size ))
	      do (setf p.x r.x)
	      while (< p.y r.h)))))


;; An auxiliary 'position map' building fxn
;; Given a desired width and font size and ... (later maybe some settings such as orientation)
;;   returns a parallel kind of list of vector2
;; Well, not exactly parallel, each line must have one more position
(defun get-render-positions (text-lines width)
  (let ((p (rm:vec 0 0)))
	(loop for ln in text-lines
	      ;; The additonal 'null' at the end is a hack
	      collect (loop for ch across (format nil "~a~a" ln (code-char 0))
		    for ch-wid = (get-char-width ch)
		    do (when (>= (+ ch-wid (rm:vx p)) width)
			 (setf (rm:vx p) 0)
			 (incf (rm:vy p) (font-param 'font-size )))
			    collect (rm:vec (rm:vx p) (rm:vy p))
		    do (unless (>= (+ ch-wid (rm:vx p)) width)
			 (incf (rm:vx p) ch-wid)))
	      do (incf (rm:vy p) (font-param 'font-size ))
	      do (setf (rm:vx p) 0))))

;; Draw text but using pre-generated positions
(defun render-text-pos (rect text-lines text-poses color)
  ;; First find the first visible line
  ;; Then start to draw character by character
  ;; Draw until the line is still visible (more efficient on rendering)
  ;; TODO:: For now, ignore it all
  (loop for ln  in text-lines
	for lnp in text-poses
	do (loop for ch  across ln
		 for chp in lnp
		 do (rl:draw-text-ex (font-param 'font )
				     (format nil "~a" ch)
				     (rm:vec2
				      (+ (rm:vx chp) (rl:rectangle-x rect))
				      (+ (rm:vy chp) (rl:rectangle-y rect)))
				     (font-param 'font-size )
				     (font-param 'spacing )
				     color))))

(defun render-text-2 (rect text-lines color)
  (render-text-pos rect text-lines
		   (get-render-positions text-lines (rl:rectangle-width rect))
		   color))

;; Gives previous and next cursors
(defun offset-cursor-fwd (curr-pos text-lines-or-poses offset)
  ;; Assume offset is +ve
  ;; If at the end of this line, move pos.y, else move pos.x
  
  )

;; Draw the cursor
;;TODO:: Current model doesnot care much about inter-char spacing, fix that
(defun render-cursor (text-poses cursor begin-pos color &key (blink-ms 500))
  (when (<= (/ (rem (* 1000.0 (rl:get-time)) blink-ms) blink-ms) 0.5)
    (let* ((line-n (cdr cursor))
	   (line (nth line-n text-poses))
	   (offset (min (car cursor) (- (length line) 1)))
	   (pos (nth offset line)))
      ;; start x,y is p.x,p.y
      ;; end y is p.y + font-size
      ;; for end x, if end of line, do something else
      ;;  else, end x is begin x of next char
      (rl:draw-rectangle-v
       (rm:v+ pos (rm:vec (- (car begin-pos) 1) (cdr begin-pos)))
       (rm:vec (+ 1 (if (>= (+ 1 offset) (length line))
			10
			(- (rm:vx (nth (+ 1 offset) line)) (rm:vx pos))))
	       (font-param 'font-size))
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


;; fxn is called with those args
(defun start-thrd (thread-name fxn shared-vars-assoc &rest cargs)
  (bt2:make-thread (lambda ()
		     (let ((shared-vars shared-vars-assoc)
			   (args cargs)
			   (fn fxn))
		       (apply fn (cons shared-vars args))))
		   :name thread-name))

;; A 'subscriber' and 'publisher' mechanism for key presses
(defun collect-key-actions (fxn key-list)
  (loop for key in key-list
	when (funcall fxn key)
	collect key))

;; In the cases below, there is a problem wherein the 'locks' are freed and then
;;    'with-lock-held' is called sometimes

;; Given an alist of vars, setfs the given var with mutex locked
;; Expects the mutex to be yet another var with a particular name 'var-lock'
(defun replace-var-locked (var-list var-sym new-val)
  ;; TODO:: To be replaced with something like conditional var later
  (bt2:with-lock-held ((app-args-var-lock var-list))
    (let ((old-val (slot-value var-list var-sym)))
      (setf (slot-value var-list var-sym) new-val)
      old-val)))

;; Used to atomically append items to the list, assumes 'var-sym' is pointing to a list 
(defun append-var-locked (var-list var-sym new-val)
  ;; TODO:: To be replaced with something like conditional var later
  (bt2:with-lock-held ((app-args-var-lock var-list))
    (let ((old-val (slot-value var-list var-sym)))
      (setf (slot-value var-list var-sym) (append old-val new-val))
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
      (append-var-locked var-list var-name new-keys))
    new-keys))

(defun setup-keys-read (cxt)
  " To be used before running GUI thread, to setup keys, or later maybe not "
  (when (and cxt (cdr cxt) (app-args-var-lock (cdr cxt)))
    (replace-var-locked (cdr cxt) 'key-press-list
			'(:key-right :key-left :key-up :key-down))))

(defun process-key-evt (cxt)
  " To be used in non GUI side, that processes the key events "
  (let ((key-evts (replace-var-locked (cdr cxt) 'key-press-queue nil))
	(cursor-pos (app-args-cursor-pos (cdr cxt)))
	(text-poses (app-args-text-poses (cdr cxt))))

    (loop for key in key-evts
	  ;; In case of left/right movements, first also try to clip first
	  do (case key ((:key-right :key-left)
			(setf (car cursor-pos)
			      (min (car cursor-pos)
				   (- (length (nth (cdr cursor-pos) text-poses)) 1)))))
	  do (case key
	       (:key-right (incf (car cursor-pos)))
	       (:key-left (decf (car cursor-pos)))
	       (:key-down (incf (cdr cursor-pos)))
	       (:key-up (decf (cdr cursor-pos))))
	  
	  ;; Clip the cursors always to the 'leftmost' position
	  do (setf (car cursor-pos)
		   (max 0 (car cursor-pos)))
	  ;; Clip the line number of the cursor
	  do (setf (cdr cursor-pos)
		   (min (max 0 (cdr cursor-pos))
			(- (length text-poses) 1))))

    ;; TODO:: Add logic to pan around the screen as well here after that loop
    (unless (and (= (car cursor-pos) (car (app-args-cursor-pos (cdr cxt))))
		 (= (cdr cursor-pos) (cdr (app-args-cursor-pos (cdr cxt)))))
      ;; TODO:: The following operation might not need a lock at all
      (replace-var-locked (cdr cxt) 'cursor-pos cursor-pos))
    cursor-pos))



;; NEXT:: Need to add a lock variable, list of key presses subscribed and the notif queue,
;;        cursor pos as variables
;; Use the `for .. = (assoc-val '.. vars)` in the loop for read only shared variable
;; Later, upgrade the text lines and positions to be also shared variables
(defun run-app (vars file-to-open)
  " vars: win-w win-h bg-col txt-col to-quit var-lock key-press-queue key-press-list cursor-pos text-poses text-lines text-begin"
  (format t "To open the file `~a`, the vars are: ~a~%" file-to-open vars)
  ;; These vars cannot be updated that easily, they need 'double-buffering'
  (let* ((win-w (app-args-win-w vars))
	 (win-h (app-args-win-h vars))
	 ;; A way to `rate-limit` key presses
	 ;; A list of keys in the 'cooldown' period
	 (cooled-keys nil)
	 ;; TODO:: This method might not work well
	 ;;        You might have to 'refresh' each frame's key events to make it proper
	 (fill-recent (lambda (curr-keys)
			;; Removal
			(setf cooled-keys
			      ;; The number is the 'cooldown period'
			      (remove-if (lambda (k) (> (- (rl:get-time) (cdr k)) 0.2))
					 cooled-keys))
			;; Insertion
			(loop for k in curr-keys
			      do (setf cooled-keys
				       (cons (cons k (rl:get-time)) cooled-keys)))))
	 ;; The function to use to check for key presses that are not in cooldown
	 (check-key-down (lambda (key)
			   (and (not (assoc-val key cooled-keys))
				(rl:is-key-down key)))))
    (rl:with-window (win-w win-h (format nil "file:~a" file-to-open))
      (rl:set-target-fps 60)
	;; TODO:: After editing part starts, all these are going to be controlled from outside
	(setf (app-args-text-begin vars) (cons 10 100))
	(setf (app-args-text-lines vars) (uiop:read-file-lines file-to-open))
	(setf (app-args-text-poses vars)
	      (get-render-positions (app-args-text-lines vars)
				    (- (app-args-win-w vars)
				       (* 2 (car (app-args-text-begin vars))))))

	(loop while (not (or (app-args-to-quit vars) (rl:window-should-close)))
	      ;; These vars are to be 'refreshed' every loop, in essence 'hot-reloaded'
	      for text-begin = (app-args-text-begin vars)
	      for text-lines = (app-args-text-lines vars)
	      for text-poses = (app-args-text-poses vars)
	      
	      for cursor-pos = (app-args-cursor-pos vars)
	      for bg-col = (app-args-bg-col vars)
	      for txt-col = (app-args-txt-col vars)
	      for key-press-list = (app-args-key-press-list vars)

	      ;; Update the keybindings
	      do (funcall fill-recent
			  (publish-key-evt vars check-key-down 'key-press-queue key-press-list))

	      ;; This setup might techinically cause delay in UI updates, need to research more

	      do (execute-tasks (app-args-tasks-before-draw vars))

	      ;; Draw according to the info available
	      do (rl:with-drawing (rl:clear-background bg-col)
		   (execute-tasks (app-args-tasks-top-draw vars))
		   (rl:draw-fps 10 10)
		   (render-cursor text-poses cursor-pos text-begin :red)
		   ;; TODO:: Later need to make it so that win-w and win-h are also shared properly
		   (render-text-pos (rl:make-rectangle
				     :x (car text-begin) :y (cdr text-begin)
				     :width (- win-w (* 2 (car text-begin)))
				     :height (- win-h (* 2 (cdr text-begin))))
				    text-lines text-poses txt-col)
		   (execute-tasks (app-args-tasks-bot-draw vars))))
	(setf (app-args-to-quit vars) t))))


(defun start (file-to-open &key (win-w 800) (win-h 800) (bg-col :raywhite))
  ;; TODO:: Doing this copy-list is not working
  ;;        at the very least, 'cursor-pos' is shared between runs
  ;;        one possible soln is using , for each value, probably it all is set in stone at compile time otherwise
  ;; TODO:: If not called stop, does something weird to next run
  (let ((vars (copy-tree (make-app-args
			  :win-w win-w :win-h win-h :to-quit nil
			  :bg-col bg-col :txt-col :black
			  :var-lock (bt2:make-lock)
			  :key-press-queue (list) :key-press-list (list)
			  :cursor-pos (cons 0 0) :text-begin nil
			  :text-poses (list) :text-lines (list)
			  :tasks-before-draw (make-task-dispatcher :lock (bt2:make-lock))
			  :tasks-top-draw (make-task-dispatcher :lock (bt2:make-lock))
			  :tasks-bot-draw (make-task-dispatcher :lock (bt2:make-lock))))))
    (let ((cxt (cons (start-thrd "GUI Thread" 'run-app vars file-to-open) vars)))
      (setup-keys-read cxt)
      cxt)))

(defun stop (thrd-obj)
  (setf (app-args-to-quit (cdr thrd-obj)) t)
  (bt2:join-thread (car thrd-obj))
  ;; (rl:unload-font (assoc-val 'font *glob-font*))
  (setf *glob-font* nil)
  (setf (app-args-to-quit (cdr thrd-obj)) nil)
  (setf (car thrd-obj) nil
	(cdr thrd-obj) nil))

;; A 'key-handler' , a probably temporary solution
(defun run-handler (cxt)
  (start-thrd "Key Event Handler Thread"
	      (lambda (vars cxt)
		(declare (ignorable vars))
		(loop while (not (app-args-to-quit (cdr cxt)))
		      do (process-key-evt cxt)
		      ;; A hack to not overwhelm CPU
		      do (sleep 0.08)))
	      '() cxt))
(defvar *app* nil)
