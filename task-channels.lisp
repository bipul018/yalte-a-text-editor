(in-package :editr)

;; A channel-like mechanism consisting of a single producer and consumer
(defstruct task-dispatcher
  (queue (list))
  (lock)
  (paused-p nil))

(defun submit-task (tasks task-item)
    (bt2:with-lock-held ((task-dispatcher-lock tasks))
      (let ((old-val (task-dispatcher-queue tasks)))
	(setf (task-dispatcher-queue tasks) (append old-val (list task-item)))
	old-val)))

	 
(defmacro dispatch-once-async (tasks &rest body)
  `(submit-task ,tasks (lambda () ,@body)))

(defmacro dispatch-once-sync (tasks &rest body)
  (let ((lck (gensym)))
  `(let ((,lck (bt2:make-semaphore)))
     (submit-task ,tasks
		  (lambda ()
		    (funcall (lambda ()
			       ,@body))
		    (bt2:signal-semaphore ,lck)))
     (bt2:thread-yield)
     (bt2:wait-on-semaphore ,lck))))

(defmacro dispatch-for (tasks time-expr desired-value &rest body)
  "This adds the task list so that it runs until the 'time-expr' produces >= 'desired-value'"
  (let ((tgt (gensym))
	(fn (gensym)))
    `(let ((,tgt (+ ,time-expr ,desired-value)))
       (labels ((,fn ()
		  (funcall (lambda () ,@body))
		  (unless (>= ,time-expr ,tgt)
		    (submit-task ,tasks #',fn))))
	 (submit-task ,tasks #',fn)))))

(defun execute-tasks (tasks)
  ;; TODO:: Implement mechanisms of timeouts and task limiting
  (unless (task-dispatcher-paused-p tasks)
    (let ((tasks ()))
      (bt2:with-lock-held ((task-dispatcher-lock tasks))
	(setf tasks (task-dispatcher-queue tasks))
	(setf (task-dispatcher-queue tasks) ()))
      (mapcar #'funcall tasks))))

;; A simple 'task loop' consumer
(defun run-task-loop (tasks)
  (bt2:make-thread (lambda ()
		       (loop until (task-dispatcher-paused-p *tasks*)
			     do (execute-tasks *tasks*)
			     do (sleep 0.1)))
		   :initial-bindings (cons (cons '*tasks* tasks) bt2:*default-special-bindings*)
		   :name "Task Runner"))

