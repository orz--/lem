(defpackage :lem-modeline-battery
  (:use :cl :lem)
  (:export :enable
           :disable))
(in-package :lem-modeline-battery)

(defvar *last-time* nil)
(defvar *cache-values* nil)
(defvar *interval-second* 10)

(defun update-cache-p ()
  (let ((time (get-internal-real-time)))
    (when (or (null *last-time*)
              (< (* *interval-second* 1000) (- time *last-time*)))
      (setf *last-time* time)
      t)))

(defun battery (window)
  (declare (ignore window))
  (when (update-cache-p)
    (message "update cache")
    (alexandria:when-let ((info (trivial-battery:battery-info)))
      (let ((percentage (alexandria:assoc-value info "percentage" :test #'equal))
            (charging (alexandria:assoc-value info "charging" :test #'equal)))
        (setf *cache-values*
              (if charging
                  (list (format nil "[~D% AC] " percentage) nil :right)
                  (list (format nil "[~D% BAT] " percentage) nil :right))))))
  (apply #'values *cache-values*))

(defun enable ()
  (modeline-add-status-list 'battery))

(defun disable ()
  (modeline-remove-status-list 'battery))