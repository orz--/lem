(defpackage :lem.search-replace
  (:use :cl :lem :alexandria)
  #+sbcl
  (:lock t))
(in-package :lem.search-replace)

(define-condition highlight-matches (after-executing-command) ())

(define-attribute highlight
  (t :foreground "white" :background "dark red"))

(define-attribute active-highlight
  (t :foreground "black" :background "cyan"))

(define-key *global-keymap* "C-s" 'search-start)

(defvar *prompt-keymap* (make-keymap :name 'search-prompt))
(define-key *prompt-keymap* "C-s" 'search-next-matched)
(define-key *prompt-keymap* "C-r" 'search-previous-matched)

(define-minor-mode search-prompt-mode
    (:name "Search"
     :keymap *prompt-keymap*
     :global t))

(defun compute-window-region (window)
  (let ((start (window-view-point window)))
    (list start
          (or (line-offset (copy-point start :temporary)
                           (window-height window))
              (buffer-end-point (window-buffer window))))))

(defmacro with-window-region ((start-point end-point window) &body body)
  `(destructuring-bind (,start-point ,end-point)
       (compute-window-region ,window)
     ,@body))

(defun call-with-buffer-check (function)
  (flet ((num-points (buffer)
           (length (lem-base::buffer-points buffer))))
    (let* ((before-buffer (current-buffer))
           (before-num-points (num-points before-buffer)))
      (unwind-protect (funcall function)
        (assert (eq before-buffer (current-buffer)))
        (assert (= before-num-points (num-points (current-buffer))))))))

(defmacro with-buffer-check (() &body body)
  `(call-with-buffer-check (lambda () ,@body)))

(defvar *context* nil)

(defstruct context
  search-forward
  search-backward
  prompt-window
  target-window
  cursor
  last-matched
  saved-point)

(defstruct matched
  (start (required-argument :start) :type point)
  (end (required-argument :end) :type point))

(defun make-current-context (search-forward search-backward)
  (let* ((prompt-window (active-prompt-window))
         (target-window (lem::caller-of-prompt-window prompt-window))
         (cursor (buffer-point (window-buffer target-window))))
    (make-context :search-forward search-forward
                  :search-backward search-backward
                  :prompt-window prompt-window
                  :target-window target-window
                  :cursor cursor
                  :saved-point (copy-point cursor))))

(defun delete-context (context)
  (delete-point (context-saved-point context))
  (setf (context-saved-point context) nil))

(defun clear-context ()
  (when *context*
    (delete-context *context*)))

(defun restore ()
  (when *context*
    (let ((point (context-saved-point *context*)))
      (move-point (buffer-point (point-buffer point)) point))))

(defun get-context ()
  (or *context*
      (setf *context* (make-current-context #'search-forward #'search-backward))))

(defun context-search-string (context)
  (get-prompt-input-string (context-prompt-window context)))

(defun create-matched-overlay (context matched)
  (let ((overlay
          (make-overlay (matched-start matched)
                        (matched-end matched)
                        (if (point<= (matched-start matched)
                                     (context-cursor context)
                                     (matched-end matched))
                            'active-highlight
                            'highlight)))
        (buffer (point-buffer (context-cursor context))))
    (push overlay (buffer-value buffer 'highlight-overlays))
    overlay))

(defun clear-all-highlight (buffer)
  (mapc #'delete-overlay (buffer-value buffer 'highlight-overlays))
  (setf (buffer-value buffer 'highlight-overlays) '()))

(defun next-match (context point &key limit (forward t))
  (multiple-value-bind (search-forward search-backward)
      (if forward
          (values (context-search-forward context)
                  (context-search-backward context))
          (values (context-search-backward context)
                  (context-search-forward context)))
    (when-let* ((next-point
                 (funcall search-forward
                          point
                          (context-search-string context)
                          limit))
                (previous-point
                 (funcall search-backward
                          (copy-point next-point :temporary)
                          (context-search-string context))))
      (let ((matched-start previous-point)
            (matched-end (copy-point next-point :temporary)))
        (assert (not (or (eq matched-start matched-end)
                         (eq matched-start point)
                         (eq matched-end point))))
        (let ((matched (if (point< matched-start matched-end)
                           (make-matched :start matched-start :end matched-end)
                           (make-matched :end matched-start :start matched-end))))
          (setf (context-last-matched context) matched)
          matched)))))

(defun update-highlight (context)
  (clear-all-highlight (window-buffer (context-target-window context)))
  (unless (string= "" (context-search-string context))
    (with-window-region (start-point end-point (context-target-window context))
      (with-point ((point start-point))
        (loop :for matched := (next-match context point :limit end-point)
              :while matched
              :do (create-matched-overlay context matched))))))

(defun adjust-current-matched (context)
  (when-let ((matched (context-last-matched context)))
    (when (point<= (matched-start matched)
                   (context-cursor context)
                   (matched-end matched))
      (move-point (context-cursor context)
                  (matched-start matched)))))

(defun move-matched-and-update-highlight (context &key (forward t))
  (loop :repeat (if forward 2 1)
        :do (next-match context (context-cursor context) :forward forward))
  (adjust-current-matched context)
  (update-highlight context)
  (window-see (context-target-window context)))

(define-command search-next-matched () ()
  (move-matched-and-update-highlight (get-context) :forward t))

(define-command search-previous-matched () ()
  (move-matched-and-update-highlight (get-context) :forward nil))

(defun prompt-for-search ()
  (with-buffer-check ()
    (search-prompt-mode t)
    (let ((*context* nil))
      (unwind-protect
           (handler-bind ((highlight-matches
                            (lambda (c)
                              (declare (ignore c))
                              (update-highlight (get-context))))
                          (editor-abort
                            (lambda (c)
                              (declare (ignore c))
                              (restore))))
             (prompt-for-string "Search: "
                                :gravity :topright))
        (clear-all-highlight (current-buffer))
        (search-prompt-mode nil)
        (delete-context *context*)))))

(define-command search-start () ()
  (let ((string (prompt-for-search)))
    string))
