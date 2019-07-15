;;;; Copyright (C) 2018, 2019 Jan Moringen
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Library General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Library General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Library General Public
;;;; License along with this library; if not, write to the
;;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;;; Boston, MA  02111-1307  USA.

(cl:in-package #:new-inspector)

;;; Utilities

(declaim (inline package-locked?))
(defun package-locked? (package)
  #+sbcl (sb-ext:package-locked-p package)
  #-sbcl nil)

;;; `symbol-slot-place'

(defclass symbol-slot-place (basic-place) ; TODO make a base class without cell slot
  ())

#+sbcl (defmethod supportsp :around ((place symbol-slot-place) (operation t))
         (and (if-let ((package (symbol-package (container place))))
                (not (package-locked? package))
                t)
              (call-next-method)))

(defmethod supportsp ((place symbol-slot-place) (operation (eql 'remove-value)))
  t)

;;; `symbol-value-place'
;;;
;;; TODO constants
;;; TODO show type
;;; TODO check value against type

(defclass symbol-value-place (symbol-slot-place)
  ())

(defmethod supportsp ((place symbol-value-place) (operation t))
  (and (not (constantp (container place)))
       (call-next-method)))

(defmethod valuep ((place symbol-value-place))
  (boundp (container place)))

(defmethod value ((place symbol-value-place))
  (symbol-value (container place)))

(defmethod (setf value) ((new-value t) (place symbol-value-place))
  (setf (symbol-value (container place)) new-value))

(defmethod remove-value ((place symbol-value-place))
  (makunbound (container place)))

;;; `symbol-function-place'

(defclass symbol-function-place (symbol-slot-place)
  ())

(defmethod accepts-value-p ((place symbol-function-place) (value t))
  nil)

(defmethod accepts-value-p ((place symbol-function-place) (value function))
  t)

(defmethod valuep ((place symbol-function-place))
  (fboundp (container place)))

(defmethod value ((place symbol-function-place))
  ;; (symbol-function (container place))
  (fdefinition (container place)))

(defmethod (setf value) ((new-value function) (place symbol-function-place))
  ;; (setf (symbol-function (container place)) new-value)
  (setf (fdefinition (container place)) new-value))

(defmethod remove-value ((place symbol-function-place))
  (fmakunbound (container place)))

;;; `symbol-type-place'

(defclass symbol-type-place (symbol-slot-place)
  ())

(defmethod accepts-value-p ((place symbol-type-place) (value t))
  nil)

(defmethod accepts-value-p ((place symbol-type-place) (value class))
  t)

(defmethod valuep ((place symbol-type-place))
  (find-class (container place) nil))

(defmethod value ((place symbol-type-place))
  (find-class (container place) nil))

(defmethod (setf value) ((new-value class) (place symbol-type-place))
  (setf (find-class (container place) nil) new-value))

(defmethod remove-value ((place symbol-type-place))
  (setf (find-class place nil) nil))

(defmethod make-object-state ((object class)
                              (place  symbol-type-place))
  (make-instance (object-state-class object place) :place place
                                                   :style :name-only))

;;; Object states

(defclass inspected-package (inspected-instance)
  ((%symbol-filter :initarg  :symbol-filter
                   :accessor symbol-filter
                   :initform nil))
  (:default-initargs
   :slot-style nil))

(defmethod (setf symbol-filter) :after ((new-value t)
                                        (object    inspected-package))
  ())

(defmethod object-state-class ((object package) (place t))
  'inspected-package)

;;; Object inspection methods

(defmethod inspect-object-using-state ((object symbol)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (call-next-method)

  (write-char #\Space stream)
  (let ((package (symbol-package object)))
    (cond ((null package)
           (badge stream "uninterned"))
          (t
           (let ((state (nth-value 1 (find-symbol (symbol-name object) package)))) ; TODO ugly
             (badge stream "~(~A~)" state)))))

  #+sbcl (when-let ((kind (sb-cltl2:variable-information object)))
           (write-char #\Space stream)
           (badge stream "~(~A~)" kind)))

(defmethod inspect-object-using-state ((object symbol)
                                       (state  inspected-object)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (print-documentation object stream :namespace 'function) ; TODO not just function

  (formatting-table (stream)
    (formatting-row (stream)
      (formatting-place (stream object 'pseudo-place (symbol-name object) present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Name" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream)))

      (formatting-place (stream object 'pseudo-place (symbol-package object) present inspect) ; TODO should be mutable
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Package" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))

    (formatting-row (stream)
      (formatting-place (stream object 'symbol-value-place nil present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Value" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream)))

      (formatting-place (stream object 'symbol-function-place nil present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Function" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))

    (formatting-row (stream)
      (formatting-place (stream object 'symbol-type-place nil present inspect)
        (with-style (stream :slot-like)
          (formatting-cell (stream) (write-string "Type" stream))
          (formatting-cell (stream) (present stream)))
        (formatting-cell (stream) (inspect stream))))
    )
  ;; plist
  )

(defun package-symbols (package &key filter)
  (let ((result (make-array 100 :adjustable t :fill-pointer 0)))
    (do-external-symbols (symbol package)
      (when (and (eq (symbol-package symbol) package)
                 (or (not filter)
                     (funcall filter symbol)))
        (vector-push-extend symbol result)))
    (sort result #'string-lessp :key #'symbol-name)))

(defmethod inspect-object-using-state ((object package)
                                       (state  inspected-object)
                                       (style  (eql :expanded-header))
                                       (stream t))
  (call-next-method)
  (when (package-locked? object)
    (write-char #\Space stream)
    (badge stream "locked")))

;; TODO local nicknames
;; TODO style symbols as table
;; TODO style symbols grouped by external etc.
(defmethod inspect-object-using-state ((object package)
                                       (state  inspected-package)
                                       (style  (eql :expanded-body))
                                       (stream t))
  (with-preserved-cursor-x (stream)
    (formatting-table (stream)
      (formatting-row (stream)
        (formatting-place (stream object 'pseudo-place (package-name object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Name" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream)))
        (formatting-place (stream object 'pseudo-place (package-nicknames object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Nicknames" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream)))
        #+sbcl (formatting-place (stream object 'pseudo-place (package-locked? object) present inspect)
                 (with-style (stream :slot-like)
                   (formatting-cell (stream) (write-string "Locked" stream))
                   (formatting-cell (stream) (present stream)))
                 (formatting-cell (stream) (inspect stream))))
      (formatting-row (stream)
        (formatting-place (stream object 'pseudo-place (package-use-list object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Uses" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream)))
        (formatting-place (stream object 'pseudo-place (package-used-by-list object) present inspect)
          (with-style (stream :slot-like)
            (formatting-cell (stream) (write-string "Used by" stream))
            (formatting-cell (stream) (present stream)))
          (formatting-cell (stream) (inspect stream))))))

  (print-documentation object stream)

  ;; Slots (not displayed by default)
  (call-next-method)

  (let ((cell (list nil nil nil nil nil)))
    (with-section (stream)
        (setf (third cell)
              (updating-output (stream :unique-id :symbol-count)
                (format stream "Symbols (~:D total, ~:D shown)" (fourth cell) (fifth cell))))
      (with-preserved-cursor-x (stream) ; TODO should not be needed
        ;; (write-string "Sort by" stream)
        (with-preserved-cursor-y (stream)
          (with-output-as-gadget (stream)
            (make-pane 'toggle-button :background +white+ :label "External")))
        (with-preserved-cursor-y (stream)
          (with-output-as-gadget (stream)
            (make-pane 'toggle-button :background +white+ :label "Documented")))
        (with-preserved-cursor-y (stream)
          (with-output-as-gadget (stream)
            (make-pane 'toggle-button :background +white+ :label "Function")))
        (with-preserved-cursor-y (stream)
          (with-output-as-gadget (stream)
            (make-pane 'toggle-button :background +white+ :label "Value")))
        (write-string "Filter " stream)
        (with-output-as-gadget (stream)
          (make-pane 'text-field
                     :width 200
                     :background +beige+
                     :value-changed-callback (lambda (gadget value)
                                               (declare (ignore gadget))
                                               (let ((string (string-upcase value)))
                                                 (setf (symbol-filter state)
                                                       (lambda (symbol)
                                                         (search string (symbol-name symbol)))))
                                               (when (first cell)
                                                 (let ((*parent-place* (first cell)))
                                                   (redisplay (second cell) stream)))
                                               (when (third cell)
                                                 (redisplay (third cell) stream))))))

      (setf (first cell) *parent-place*
            (second cell)
            (updating-output (stream :unique-id :symbol-table)

              (with-drawing-options (stream :text-size :smaller)
                (formatting-table (stream)
                  (formatting-header (stream) "Symbol" "Value" "Function" "Type")

                  ;; TODO should be able to use updating-output here
                  (setf (fifth cell) 0)
                  (flet ((symbol-row (symbol)
                           (incf (fifth cell))
                           (formatting-row (stream)
                             (formatting-place (stream object 'pseudo-place symbol nil inspect* :place-var place)
                               (formatting-cell (stream) (inspect* stream))

                               (let ((*parent-place* place)) ; TODO this is not good
                                 ;; Value slot
                                 (formatting-place (stream symbol 'symbol-value-place nil present inspect)
                                   (formatting-cell (stream) (present stream) (inspect stream)))
                                 ;; Function slot
                                 (formatting-place (stream symbol 'symbol-function-place nil present inspect)
                                   (formatting-cell (stream) (present stream) (inspect stream)))
                                 ;; Type slot
                                 (formatting-place (stream symbol 'symbol-type-place nil present inspect)
                                   (formatting-cell (stream) (present stream) (inspect stream))))))))
                    (map nil #'symbol-row (package-symbols object :filter (symbol-filter state)))))))))))

;; TODO command: trace all symbols
