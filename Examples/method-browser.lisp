;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2005 Andy Hefner <ahefner@gmail.com>
;;;  (c) copyright 2006 Christophe Rhodes <crhodes@common-lisp.net>
;;;  (c) copyright 2008 Troels Henriksen <thenriksen@common-lisp.net>
;;;  (c) copyright 2016,2017 Daniel Kochmanski <daniel@turtleware.eu>
;;;  (c) copyright 2018 Henry Harrington <henry.harrington@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Method-Browser Example
;;;
;;; This is an example of how to write a CLIM application with a
;;; "normal" GUI, where "normal" is a completely event driven app
;;; built using gadgets and not using the command-oriented framework.
;;;
;;; Running the method-browser:
;;;   (clim-demo::run-test 'clim-demo::method-browser)
;;;
;;; How to use this app: Position the mouse over the text field labelled
;;; "Enter Name of Generic Function." Type the name of a generic
;;; function (the text field currently behaves in a focus-follows-mouse
;;; fashion) and hit enter. The specializers pane below will fill
;;; with buttons for each required argument of the function. Clicking
;;; each button produces a menu of types which that argument of the
;;; function is specialized on. As you adjust the argument types,
;;; the bottom pane of the application will display which methods
;;; would be applicable for the given arguments.
;;;
;;; This example demonstrates:
;;;   * Conventional gadget-oriented interface
;;;   * Dynamic creation of interface objects
;;;   * Use of CLIM extended-output-streams (fonts, text-styles, etc)
;;;   * CLIM table formatting
;;;
;;; TODO:
;;;   * Nicer, more clever display of methods than simply listing them
;;;     in a row.  To do this right really involves some non-portable
;;;     fun and a codewalker.  You could probably write something that
;;;     just understood the standard method combination and qualifiers
;;;     with substantially less work.
;;;   * Change focus behavior of McCLIM text entry gadget
;;;   * Implement focus-aware cursor shapes in McCLIM

(defpackage #:clim-demo.method-browser
  (:use
   #:clim-lisp
   #:clim)

  (:import-from #:alexandria
   #:when-let
   #:if-let)

  (:export
   #:method-browser))

(in-package #:clim-demo.method-browser)

;;; CLOS / MOP Utilities

(defun compute-gf-specializers (gf)
  "Computes a list of lists of the types for which required argument is
specialized on, removing duplicates"
  (loop with all-specializers = (mapcar #'c2mop:method-specializers
                                        (c2mop:generic-function-methods gf))
        with arity = (length (first all-specializers))
        for index from 0 below arity
        collect (remove-duplicates (mapcar (lambda (specs)
                                             (nth index specs))
                                           all-specializers))))

(defun compute-specializer-object (specializer)
  (cond ((typep specializer 'c2mop:class)
         ;; Implementation-dependent whether prototypes for
         ;; built-in classes (like integer, t) are available.
         (handler-case
             (c2mop:class-prototype (c2mop:ensure-finalized specializer))
           (error ()
             'no-prototype)))
        ((typep specializer 'c2mop:eql-specializer)
         (c2mop:eql-specializer-object specializer))
        (t
         (error "Can't compute effective methods, specializer ~A is ~
                 not understood."
                specializer))))

;;; FIXME: returns nil if there is both an EQL specializer and a
;;; class specializer for which no prototype instance is available.
(defun compute-applicable-methods-from-specializers (gf specializers)
  (multiple-value-bind (applicable-methods validp)
      (c2mop:compute-applicable-methods-using-classes gf specializers)
    (if validp
        applicable-methods
        (let ((instances (mapcar #'compute-specializer-object specializers)))
          (unless (member 'no-prototype instances)
            (compute-applicable-methods gf instances))))))

;;; FIXME: Support EQL specializers.
;;; This is hard to do ideally, and I'm not really trying.
;;; So we just make sure that T ends up at the head of the list.
(defun sorted-gf-specializers (gf)
  "Sort a list of specializers for aesthetic purposes"
  (mapcar (lambda (types)
            (sort (copy-list types)
                  (lambda (a b)
                    (cond
                      ((eql a (find-class t)) t)
                      ((eql b (find-class t)) nil)
                      ((and (typep a 'c2mop:class)
                            (typep b 'c2mop:class))
                       (string< (class-name a)
                                (class-name b)))
                      ((and (typep a 'c2mop:eql-specializer)
                            (not (typep b 'c2mop:eql-specializer)))
                       nil)
                      ((and (not (typep a 'c2mop:eql-specializer))
                            (typep b 'c2mop:eql-specializer))
                       t)
                      ((and (typep a 'c2mop:eql-specializer)
                            (typep b 'c2mop:eql-specializer))
                       (string<
                        (princ-to-string (c2mop:eql-specializer-object a))
                        (princ-to-string (c2mop:eql-specializer-object b))))
                      (t (warn "Received specializer of unknown type")
                         nil) ))))
          (compute-gf-specializers gf)))

(defun simple-generic-function-lambda-list (gf)
  "Returns the required arguments of a generic function"
  (values (alexandria:parse-ordinary-lambda-list
           (c2mop:generic-function-lambda-list gf))))

(defun specializer-pretty-name (spec)
  "Pretty print the name of a method specializer"
  (cond ((typep spec 'c2mop:class)
         (princ-to-string (class-name spec)))
        ((typep spec 'c2mop:eql-specializer)
         (format nil "(EQL '~A)" (c2mop:eql-specializer-object spec)))
        (t (princ-to-string spec))))

(defun maybe-find-gf (name)
  "Search for the generic function named by the user"
  (ignore-errors
    (let ((sym (read-from-string name)))
      (and sym
           (fboundp sym)
           (typep (symbol-function sym) 'generic-function)
           (symbol-function sym)))))

(defun methodp (object)
  (typep object 'method))

;;; Walk the form returned by compute-effective-method, noting any methods in
;;; the order we find them. This is good enough for the sort of output we are
;;; producing. I hope.
(defun walk-em-form (form)
  "Walks an effective methods form, attempting to determine what order methods will be called"
  (cond
    ((typep form 'cons)
     (append (walk-em-form (car form))
             (walk-em-form (cdr form))))
    ((methodp form) (list form))
    (t nil)))

;;; CLIM GUI
;;;
;;; Every CLIM app starts with an application frame, an object which
;;; encapsulates the state of an application. Windowing and abstractions
;;; such as commands and menus are designed around application frames.
;;; DEFINE-APPLICATION-FRAME is an extension of DEFCLASS adding options
;;; to define the layout(s) and content of your application window,
;;; commands within your application, a menu bar, etc.
;;;
;;; The :panes option is typically used to define and name the important
;;; elements of your interface. CLIM provides some syntactic sugar, for
;;; example (arg-pane :vrack-pane) below is equivalent to
;;; (arg-pane (make-pane 'vrack-pane)).
;;;
;;; The :layouts option defines the hierarchy of windows to instantiate.
;;; Multiple layouts can be defined, but a single default layout is sufficient.
;;; When defining a layout, the things defined by :pane can be referred to by
;;; name. The forms within the default layout below are actual lisp code -
;;; vertically, labelling, scrolling, etc. are macros which can be called at
;;; any time, provided some context is established first. Similarly, you could
;;; call make-pane here to construct a pane anonymously.

(define-application-frame method-browser ()
  ((gf :accessor gf :initarg :gf :initform nil)
   (arg-types :accessor arg-types :initarg :arg-types :initform nil))
  (:menu-bar nil)
  (:panes
   ;; Text box for the user to enter a function name
   (gf-name-input :text-field :value-changed-callback 'gf-name-input-callback)
   ;; Empty vertical layout pane where option-panes for arguments are added
   (arg-pane :vrack-pane)
   ;; Blank pane where the program can render output
   (output-pane :application-pane :display-time t
                                  :display-function 'display-methods))
  (:layouts
   (default
    (vertically ()
      (labelling (:label "Enter Name of Generic Function")
        gf-name-input)
      (labelling (:label "Specializers")
        (spacing (:thickness 6)
          arg-pane))
      (labelling (:label "Applicable methods")
       (scrolling (:width 800 :height 600)
         output-pane))))))

;;; When the user types a method name and hits enter, the callback function
;;; below will be called, setting in motion the process of updating the
;;; slots in the application-frame, examining the generic function to
;;; build a set of controls for selecting argument types, and finally
;;; printing a table listing the methods.

(defun gf-name-input-callback (gadget value)
  "Callback invoked by the text input gadget when the user hits enter"
  (setup-new-gf (gadget-client gadget) (maybe-find-gf value)))

(defun setup-new-gf (frame gf)
  "Update the application frame to display the supplied generic function"
  (setf (gf frame) gf
        (arg-types frame) (when gf (compute-initial-arg-types gf)))
  (changing-space-requirements ()
    (make-arg-pane frame))
  (when-let ((output (get-frame-pane frame 'output-pane)))
    (redisplay-frame-pane frame output :force-p t)))

(defun compute-initial-arg-types (gf)
  "Returns a list containing the initial specializers to use for each required argument of a function"
  (mapcar #'first (sorted-gf-specializers gf)))

;;; Within the macro WITH-LOOK-AND-FEEL-REALIZATION, panes may be created
;;; at runtime. This macro sets the environment up such that an abstract pane
;;; class such as 'push-button can be translated to a concrete pane class
;;; appropriate for your window system.

(defun make-arg-pane (frame)
  "Generates contents of argument pane. For each required argument an
option-pane is created allowing the user to select one of the specializers
available for that argument."
  (when-let ((container (find-pane-named frame 'arg-pane)))
   (let ((gf        (gf frame))
         (arg-types (arg-types frame)))
     ;; Delete the children of the container pane
     (dolist (child (sheet-children container))
       (sheet-disown-child container child))
     ;; Repopulate the container pane with a new table pane containing
     ;; option-panes for each specializer argument.
     (when gf
       (let ((fm (frame-manager frame)))
         (with-look-and-feel-realization (fm frame)
           (let* ((contents (make-arg-table-contents gf arg-types))
                  (pane     (make-pane 'table-pane :spacing  8 ; McCLIM issue: spacing initarg
                                                   :contents contents)))
             (sheet-adopt-child container pane))))))))

(defun make-arg-table-contents (gf arg-types)
  (loop for index from 0 by 1
        for curval in arg-types
        for specs  in (sorted-gf-specializers gf)
        for name   in (simple-generic-function-lambda-list gf)
        collect (list
                 (make-pane 'label-pane :label (princ-to-string name))
                 (make-pane 'option-pane
                            :items specs
                            :value curval
                            :value-changed-callback
                            (let ((index index))
                              (lambda (gadget value)
                                (setf (nth index arg-types) value)
                                (let ((frame (gadget-client gadget)))
                                  (when-let ((output (find-pane-named frame 'output-pane)))
                                    (redisplay-frame-pane frame output :force-p t)))))
                            :name-key #'specializer-pretty-name))))

;;; Generate contents of output-pane

(defparameter *column-header-ink* +gray50+)
(defparameter *column-header-text-style* (make-text-style :sans-serif :bold :small))

(defparameter *method-qualifier-ink* +red+)
(defparameter *specializer-text-style* (make-text-style :sans-serif :roman :normal))

(defmethod present-method (method stream)
  "Produces one table row to describe a method"
  (let ((*print-pretty* nil))
    ;; Method type, if not standard-method
    (formatting-cell (stream :align-x :left)
      (when (not (typep method 'standard-method))
        (princ (type-of method) stream)))
    ;; Method qualifiers
    (formatting-cell (stream :align-x :center)
      (with-drawing-options (stream :ink *method-qualifier-ink*)
        (when-let ((m-q (method-qualifiers method)))
          (let ((first t))
            (dolist (symbol m-q)
              (if first
                  (setf first nil)
                  (princ " " stream))
              (present symbol (presentation-type-of symbol) :stream stream))))))
    ;; Method specializers
    ;; This is very silly, but put the surrounding parens in their own
    ;; column because I'm anal about the formatting.
    (formatting-cell (stream :align-x :right)
      (princ "      (" stream))
    (dolist (spec (c2mop:method-specializers method))
      (formatting-cell (stream :align-x :center)
        (with-drawing-options (stream :text-style *specializer-text-style*
                                      :ink (ink-for-specializer spec))
          (with-output-as-presentation (stream spec (presentation-type-of spec))
            (princ (specializer-pretty-name spec) stream)))))
    (formatting-cell (stream :align-x :left)
      (princ ")" stream))))

(defun display-methods (frame pane)
  "Generates the display of applicable methods in the output-pane"
  (if-let ((gf (gf frame)))
    (let* ((methods (compute-applicable-methods-from-specializers gf (arg-types frame)))
           (combination (c2mop:generic-function-method-combination gf))
           (effective-methods (ignore-errors (c2mop:compute-effective-method gf combination methods)))
           (serial-methods (walk-em-form effective-methods)))
      ;; Generate a table for the methods
      (formatting-table (pane :x-spacing "   ")
        (formatting-row (pane)
          (gf-column-headers (gf frame) pane))
        (dolist (method serial-methods)
          (formatting-row (pane)
            (present-method method pane))))
      (terpri pane))
    (with-drawing-options (pane :text-face :italic :ink +gray40+)
      (write-string "No generic function found" pane))))

(defun ink-for-specializer (spec)
  "Determine a color to use when displaying a specializer, highlighting if one
of the types selected by the user."
  (cond ((not (typep *application-frame* 'method-browser))
         +foreground-ink+)
        ((member spec (arg-types *application-frame*))
         +olivedrab4+)
        (t
         +grey18+)))

(defun gf-column-headers (gf stream)
  "Produces a row of column titles for the method table"
  (flet ((header (label)
           (formatting-cell (stream :align-x :center)
             (when label
               (with-drawing-options (stream :ink *column-header-ink*
                                             :text-style *column-header-text-style*)
                 (surrounding-output-with-border (stream :shape :underline)
                   (princ label stream)))))))
    ;; Method type
    (header nil)
    (header "Qualifier")
    (header nil)
    (dolist (arg (simple-generic-function-lambda-list gf))
      (header (princ-to-string arg)))))
