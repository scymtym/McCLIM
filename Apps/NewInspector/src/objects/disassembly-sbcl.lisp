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

;; The DISASSEMBLE function will print out implementation-specific
;; things to *STANDARD-OUTPUT* in an implementation-specific way. This
;; is generally optimized for console output, and is therefore not the
;; most aesthetically pleasing way for CLIM to display it. This file
;; is where custom disassembly printers are defined. For each
;; implementation, you can define a pretty-printer for the disassembly
;; of a function.

(define-presentation-type basic-blocks ())

(defun maybe-extract (thing)
  (etypecase thing
    (basic-block thing)
    (successor   (basic-block thing))))

(define-presentation-method present ((object sequence)
                                     (type   basic-blocks)
                                     (stream extended-output-stream)
                                     (view   t)
                                     &key)
  (let ((roots (remove-if (lambda (bb)
                            (some (lambda (other)
                                    (find bb (successors other) :key #'basic-block))
                                  object))
                          object)))
    (format-graph-from-roots
     roots
     (lambda (object stream)
       (let ((basic-block (maybe-extract object)))
         (present basic-block 'basic-block :stream stream :view view)))
     (compose #'successors #'maybe-extract)
     :stream stream
     :graph-type :directed-graph :merge-duplicates t :duplicate-key #'maybe-extract
     :arc-drawer (lambda (stream from-node to-node x1 y1 x2 y2
                          &rest with-drawing-options)
                   (let ((ink (typecase (climi::graph-node-object to-node)
                                (fall-through-successor +black+)
                                (false-successor        +dark-red+)
                                (true-successor         +forest-green+))))
                     (apply #'climi::arrow-arc-drawer #+later #'climi::bezier-arc-drawer
                            stream from-node to-node x1 y1 x2 y2
                            :ink ink with-drawing-options)))
     :orientation :vertical :maximize-generations t)))

(defclass basic-block ()
  ((%label        :initarg :label
                  :reader  label)
   (%instructions :initarg  :instructions
                  :accessor instructions
                  :initform '())
   (%successors   :initarg  :successors
                  :accessor successors
                  :initform '())))

(define-presentation-method present ((object basic-block)
                                     (type   basic-block)
                                     (stream extended-output-stream)
                                     (view   t)
                                     &key)
  (surrounding-output-with-border (stream :shape :rectangle)
    (with-drawing-options (stream :text-size :tiny)
      (when-let ((label (label object)))
        (with-drawing-options (stream :text-face :bold :ink +dark-slate-blue+)
          (write-string label stream)))
      (formatting-table (stream :y-spacing 0)
        (map nil (rcurry #'present 'instruction :stream stream :view view)
             (instructions object))))))

(defclass successor ()
  ((%basic-block :initarg :basic-block
                 :reader  basic-block)))

(defclass fall-through-successor (successor)
  ())

(defclass false-successor (successor)
  ())

(defclass true-successor (successor)
  ())

(defclass instruction ()
  ((%offset  :initarg :offset
             :reader  offset)
   (%label   :initarg :label
             :reader  label)
   (%bytes   :initarg :bytes
             :reader  bytes)
   (%decoded :initarg :decoded
             :reader  decoded)
   (%comment :initarg :comment
             :reader  comment)))

(defmethod unconditional-jump-p ((instruction instruction))
  (starts-with-subseq "JMP" (decoded instruction)))

(defmethod conditional-jump-p ((instruction instruction))
  (and (starts-with-subseq "J" (decoded instruction))
       (not (starts-with-subseq "JMP" (decoded instruction)))))

(defmethod returnp ((instruction instruction))
  (or (starts-with-subseq "RET" (decoded instruction))
      (and (starts-with-subseq "BREAK" (decoded instruction))
           (not (starts-with-subseq "BREAK 9" (decoded instruction))))
      (starts-with-subseq "BYTE" (decoded instruction))))

(define-presentation-type instruction (&key show-label))

(define-presentation-method present ((object instruction)
                                     (type   instruction)
                                     (stream extended-output-stream)
                                     (view   t)
                                     &key)
  (let ((offset  (offset object))
        (label   (label object))
        (bytes   (bytes object))
        (decoded (decoded object))
        (comment (comment object)))
    (formatting-row (stream)
      (formatting-cell (stream :align-x :right)
        (when offset
          (with-drawing-options (stream :ink +darkgray+ :text-size :smaller)
            (format stream "~X" offset))))
      #+no (when show-label
        (formatting-cell (stream)
          (when (and offset label)      ; HACK
            (with-drawing-options (stream :ink +dark-slate-blue+ :font-family :fix :font-face :bold)
              (write-string label stream)))))
      (formatting-cell (stream)
        (when bytes
          (with-drawing-options (stream :ink +darkgray+ :text-size :smaller :font-family :fix)
            (write-string bytes stream))))
      (formatting-cell (stream)
        (when decoded
          (with-drawing-options (stream :font-size :small)
            (write-string decoded stream))))
      (formatting-cell (stream)
        (when comment
          (with-drawing-options (stream :ink +firebrick+ :font-family :fix)
            (write-string comment stream)))))))

;; With SBCL, we want to cut out the first two characters of every
;; line, which are always going to be "; ", and if anything is left
;; print it.
(defun parse-line (line)
  (prog (offset label bytes decoded comment
         (index     0)
         (semicolon (position #\; line)))
   :offset
     (let ((end (position #\: line)))
       (if (and end (every (alexandria:rcurry #'digit-char-p 16)
                           (subseq line 0 end))
                (or (not semicolon) (< end semicolon)))
           (setf offset (parse-integer (subseq line 0 end) :radix 16)
                 index  (+ end 2))
           (go :comment)))
   :label
     (alexandria:when-let ((colon (position #\: line :start index :end semicolon)))
       (setf label (subseq line index colon)
             index colon))
   :bytes
     (let* ((start (position-if (alexandria:rcurry #'digit-char-p 16)
                                line :start index))
            (end   (position-if-not (alexandria:rcurry #'digit-char-p 16)
                                    line :start start)))
       (setf bytes (subseq line start end)
             index end))
   :decoded
     (let* ((start (position #\Space line :start index :test-not #'char=))
            (end   (1+ (position #\Space line :end semicolon :from-end t :test-not #'char=))))
       (setf decoded (subseq line start end)))
   :comment
     (setf comment (cond (semicolon
                          (subseq line (+ semicolon 2)))
                         ((not offset)
                          line)))
   :end
     (return (values offset label bytes decoded comment))))



(defun parse-disassembly (disassembly-string)
  (with-input-from-string (stream disassembly-string)
    (loop with block = (make-instance 'basic-block :label nil)

          for line = (read-line stream nil nil)
          while line   ; (and line (not (parse-line (subseq line 2))))
          for (offset label bytes decoded comment) =
             (multiple-value-list
              (parse-line (subseq line 2)))
          for instruction = (when offset ; TODO necessary?
                              (make-instance 'instruction :offset offset :label label :bytes bytes :decoded decoded :comment comment))

          when (and label (instructions block))
          collect block :into blocks
          and do (let* ((new-block (make-instance 'basic-block :label label))
                        (successor (make-instance 'fall-through-successor
                                                  :basic-block new-block)))
                   (push successor (successors block))
                   (setf block new-block))
          when (and label (not (instructions block)))
          do (reinitialize-instance block :label label)

          when offset
          do (appendf (instructions block) (list instruction))

          when (and instruction (or (unconditional-jump-p instruction)
                                    (conditional-jump-p instruction)
                                    (returnp instruction)))
          collect block :into blocks
          and do (let ((new-block (make-instance 'basic-block :label label)))
                   (when (conditional-jump-p instruction)
                     (let ((successor (make-instance 'false-successor
                                                     :basic-block new-block)))
                      (push successor (successors block))))
                   (setf block new-block))

          finally (return (list* block blocks)))))

(defun link-blocks (blocks)
  (let ((label->block (make-hash-table :test #'equal)))
    (map nil (lambda (block)
               (setf (gethash (label block) label->block) block))
         blocks)
    (map nil (lambda (block)
               (when (instructions block) ; HACK
                 (let* ((jump    (last-elt (instructions block)))
                        (decoded (decoded jump)))
                   (cond ((or (unconditional-jump-p jump)
                              (conditional-jump-p jump))
                          (print (subseq decoded (1+ (position #\Space decoded))) *error-output*)
                          (print (hash-table-alist label->block) *error-output*)
                          ;; TODO there should definitely be a successor
                          (when-let* ((successor (gethash (subseq decoded (1+ (position #\Space decoded)))
                                                          label->block))
                                      (successor (make-instance 'true-successor
                                                                :basic-block successor)))
                            (push successor (successors block))))))))
         blocks)
    blocks))

;; do (with-drawing-options (pane :ink +firebrick+ :font-family :fix)
;;      (write-string line pane))
;; (terpri pane)
;; finally (setf unread-line line)

(parse-line "9e2: L0:   488FF CMP RDI, #x20100      ; NIL")
(parse-line "   ; NIL")

(parse-disassembly "9e2: L0:   488FF CMP RDI, #x20100      ; NIL")

(defun display-disassembly (object pane)
  (terpri pane)
  (let* ((disassembly-string (with-output-to-string (*standard-output*)
                               (sb-disassem:disassemble-code-component object)))
         (blocks             (parse-disassembly (print disassembly-string *error-output*)))
         (graph              (link-blocks blocks))
         unread-line)
    (present graph 'basic-blocks :stream pane)
    #+no (formatting-table (pane)
           (loop for line = unread-line then (read-line stream nil nil)
                 while line
                 do       (parse-line (subseq line 2))))))
