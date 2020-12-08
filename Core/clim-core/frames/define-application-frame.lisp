;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com)
;;;  (c) copyright 2000 by Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;  (c) copyright 2000 by Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2014 by Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2004 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2019, 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The DEFINE-APPLICATION-FRAME macro and supporting code.
;;;

(in-package #:clim-internals)

(defun coerce-pane-name (pane name)
  (setf (slot-value pane 'name) name)
  pane)

(defun %generic-make-or-reinitialize-pane
    (panes-for-layout constructor type name &rest initargs)
  ;; If PANES-FOR-LAYOUT contains a pane for NAME, try to reinitialize
  ;; it. Otherwise, make a new pane using CONSTRUCTOR.
  (let ((pane-or-parent (alexandria:assoc-value
                         panes-for-layout name :test #'eq)))
    ;; For stream panes, NAME may be associated with an ancestor of
    ;; the stream pane. It is also possible that the pane named NAME
    ;; is now of a different type than before. In any case, use TYPE
    ;; to find the descendant.
    (if-let ((pane (when pane-or-parent
                     (find-pane-of-type pane-or-parent type))))
      (progn
        (when-let ((parent (sheet-parent pane-or-parent)))
          (sheet-disown-child parent pane-or-parent))
        (apply #'reinitialize-instance pane initargs)
        pane-or-parent)
      (apply constructor :name name initargs))))

(defun generate-ensure-pane-form (name form realizer-var frame-var
                                  &optional panes-for-layout-var)
  (destructuring-bind (pane &rest options) form
    (flet ((generate (constructor type)
             (if panes-for-layout-var
                 `(%generic-make-or-reinitialize-pane
                   ,panes-for-layout-var ,constructor ,type ',name ,@options)
                 `(funcall ,constructor :name ',name ,@options))))
      (cond ((and (null options) (listp pane)) ; Single form which is a function call
             `(coerce-pane-name ,pane ',name))
            ((eq pane :application) ; Standard pane (i.e `:application')
             (generate ''make-clim-application-pane ''application-pane))
            ((eq pane :interactor)
             (generate ''make-clim-interactor-pane ''interactor-pane))
            ((eq pane :pointer-documentation)
             (generate ''make-clim-pointer-documentation-pane ''pointer-documentation-pane))
            ((eq pane :command-menu)
             (generate ''make-clim-command-menu-pane ''command-menu-pane))
            ;; Non-standard pane designator fed to `make-pane'
            (t
             (alexandria:with-unique-names (pane-class-var)
               `(let ((,pane-class-var
                        (find-concrete-pane-class ,realizer-var ',pane)))
                  ,(generate `(curry #'make-pane-1 ,realizer-var ,frame-var
                                     ,pane-class-var)
                             pane-class-var))))))))

;;; FIXME The menu-bar code in the following function is incorrect.  it
;;; needs to be moved to somewhere after the backend, since it depends
;;; on the backend chosen.
;;;
;;; This hack slaps a menu-bar into the start of the application-frame,
;;; in such a way that it is hard to find.
(defun generate-generate-panes-form (class-name menu-bar panes layouts
                                     pointer-documentation reinitializep)
  (when pointer-documentation
    (setf panes (list* '(%pointer-documentation% pointer-documentation-pane)
                       panes)))
  `(defmethod generate-panes ((fm frame-manager) (frame ,class-name))
     (with-look-and-feel-realization (fm frame)
       ;; Make (or reinitialize) pane instances and establish
       ;; lexical variables so layout forms can use them.
       (let* ,(flet ((pane-bindings (&optional old-panes-var)
                       (loop for (name . form) in panes
                             collect `(,name ,(generate-ensure-pane-form
                                               name form 'fm 'frame old-panes-var)))))
                (if (and (not (null panes)) reinitializep)
                    (alexandria:with-gensyms (old-panes-var)
                      `((,old-panes-var (frame-panes-for-layout frame))
                        ,@(pane-bindings old-panes-var)))
                    (pane-bindings)))
         (setf (frame-panes-for-layout frame)
               (list ,@(loop for (name) in panes
                             collect `(cons ',name ,name))))
         ;; [BTS] added this, but is not sure that this is correct for
         ;; adding a menu-bar transparently, should also only be done
         ;; where the exterior window system does not support menus
         (setf (frame-panes frame)
               (ecase (frame-current-layout frame)
                 ,@(if (or menu-bar pointer-documentation)
                       (mapcar (lambda (layout)
                                 `(,(first layout)
                                   (vertically ()
                                     ,@(cond
                                         ((eq menu-bar t)
                                          `((setf (frame-menu-bar-pane frame)
                                                  (make-menu-bar ',class-name))))
                                         ((consp menu-bar)
                                          `((make-menu-bar
                                             (make-command-table
                                              nil :menu ',menu-bar))))
                                         (menu-bar
                                          `((make-menu-bar ',menu-bar)))
                                         (t nil))
                                     ,@(rest layout)
                                     ,@(when pointer-documentation
                                         '(%pointer-documentation%)))))
                               layouts)
                       layouts)))))
     ;; Update frame-current-panes and the special pane slots.
     (update-frame-pane-lists frame)))

(defun geometry-specification-p (thing)
  (and (alexandria:proper-list-p thing)
       (evenp (length thing))
       (loop for (key value) on thing by #'cddr
             always (member key '(:left :top :right :bottom :width :height)))))

(defun parse-define-application-frame-options (options)
  ;; Note that options are always of the form (KEY . ARGUMENTS).
  ;; Depending on KEY, ARGUMENTS is expected to have one of two
  ;; shapes:
  ;; 1) an arbitrary list, denoted by * in the INFOS table. In this
  ;;    case, the "value" of the option is just ARGUMENTS.
  ;; 2) a singleton list (that is the option is of the form (KEY
  ;;    VALUE)), denoted by 1 in the INFOS table. In this case the
  ;;    "value" of the option is (first arguments). :TYPE in the INFOS
  ;;    table is the expected type of this value, not of ARGUMENTS.
  (let ((infos '(;; CLIM
                 (:pane                             * :conflicts (:panes :layouts))
                 (:panes                            * :conflicts (:pane))
                 (:layouts                          * :conflicts (:pane))
                 (:command-table                    1 :type (cons symbol list))
                 (:command-definer                  1 :type symbol)
                 (:menu-bar                         1 :type (or symbol list))
                 (:disabled-commands                *)
                 (:top-level                        1 :type (cons (or symbol cons) list))
                 (:icon                             1)
                 (:geometry                         * :type (satisfies geometry-specification-p))
                 (:resize-frame                     1)
                 ;; McCLIM extensions
                 (:pointer-documentation            1)
                 (:update-instances-on-redefinition 1)
                 ;; Default initargs
                 (:pretty-name                      1)
                 ;; Common Lisp
                 (:default-initargs                 *)
                 (:metaclass                        1)))
        (all-values '()))
    (labels ((definedp (key)
               (not (eq (getf all-values key 'undefined) 'undefined)))
             (maybe-check-type (key arguments expected-type expected-argument-count)
               (flet ((check-value (value)
                        (when (and expected-type (not (typep value expected-type)))
                          (error "~@<The argument ~S for option ~S is ~
                                  not of type ~A.~@:>"
                                 value key expected-type))
                        value))
                 (ecase expected-argument-count
                   (1
                    (unless (typep arguments '(cons t null))
                      (error "~@<The option ~S takes a single ~
                              argument (not ~@[~{~S~^ ~} which are ~]~
                              ~R).~@:>"
                             key arguments (length arguments)))
                    (check-value (first arguments)))
                   (*
                    (check-value arguments)))))
             (parse-option (key arguments)
               (when-let ((info (find key infos :key #'first)))
                 (destructuring-bind (name value-count &key conflicts type) info
                   (declare (ignore name))
                   (cond ((when-let ((other (find-if #'definedp conflicts)))
                            (error "~@<The options ~S and ~S are mutually ~
                                    exclusive.~@:>"
                                   key other)))
                         ((definedp key)
                          (error "~@<The option ~S cannot be supplied ~
                                  multiple times.~@:>"
                                 key))
                         ;; Canonicalize :pane, :panes and :layouts to
                         ;; just :panes and :layouts.
                         ((eq key :pane)
                          (setf (getf all-values :pane)
                                t
                                (getf all-values :panes)
                                `((single-pane ,@arguments))
                                (getf all-values :layouts)
                                `((:default single-pane))))
                         ((eq key :default-initargs)
                          (destructuring-bind
                              (&key ((:pretty-name user-pretty-name) nil pretty-name-p)
                               &allow-other-keys)
                              arguments
                            (when pretty-name-p
                              (parse-option :pretty-name (list user-pretty-name))))
                          (setf (getf all-values :user-default-initargs)
                                (alexandria:remove-from-plist arguments :pretty-name)))
                         (t
                          (setf (getf all-values key)
                                (maybe-check-type key arguments type value-count)))))
                 t)))
      (loop :for option :in options
            :for (key . values) = option
            :do (with-current-source-form (option)
                  (when (not (parse-option key values))
                    (push option (getf all-values :other-options)))))
      (alexandria:remove-from-plist all-values :pane))))

(defun generate-metaclass-name-and-form
    (name update-instances-on-redefinition metaclass)
  (cond ((and update-instances-on-redefinition metaclass)
         (let* ((name (let ((*package* (find-package '#:climi)))
                        (alexandria:symbolicate
                         (package-name (symbol-package name)) ":"
                         name '#:-metaclass)))
                (form `((defclass ,name (,metaclass
                                         redefinition-updates-instances-class)
                          ()))))
           (values name form)))
        (metaclass)
        (update-instances-on-redefinition
         'redefinition-updates-instances-class)))

(defmacro define-application-frame (name superclasses slots &rest options)
  (when (null superclasses)
    (setq superclasses '(standard-application-frame)))
  (destructuring-bind (&key panes
                            layouts
                            (command-table (list name))
                            (command-definer t)
                            (menu-bar t)
                            disabled-commands
                            (top-level '(default-frame-top-level))
                            (icon nil icon-supplied-p)
                            geometry
                            resize-frame
                            ;; McCLIM extensions
                            pointer-documentation
                            update-instances-on-redefinition
                            ;; Default initargs
                            (pretty-name (string-capitalize name))
                            ;; Common Lisp
                            user-default-initargs
                            metaclass
                            other-options
                            ;; Helpers
                            (current-layout (first (first layouts)))
                            (frame-arg (gensym "FRAME-ARG")))
      (parse-define-application-frame-options options)
    (when (eq command-definer t)
      (setf command-definer
            (alexandria:symbolicate '#:define- name '#:-command)))
    ;; If the frame class is being (re)defined with instance updating,
    ;; delay the redefinition notification until after all forms have
    ;; executed.
    (multiple-value-bind (metaclass-name define-metaclass-form)
        (generate-metaclass-name-and-form
         name update-instances-on-redefinition metaclass)
      `(,@(if update-instances-on-redefinition
              '(with-delayed-redefinition-notifications ())
              '(progn))

        ,@(when define-metaclass-form
            `(,define-metaclass-form))

        (defclass ,name ,superclasses
          ,slots
          (:default-initargs
           :name              ',name
           :pretty-name       ,pretty-name
           ,@(when icon-supplied-p `(:icon ,icon))
           :command-table     (find-command-table ',(first command-table))
           :disabled-commands ',disabled-commands
           :menu-bar          ',menu-bar
           :current-layout    ',current-layout
           :layouts           ',layouts
           :resize-frame      ',resize-frame
           :top-level         (list ',(car top-level) ,@(cdr top-level))
           :top-level-lambda  (lambda (,frame-arg)
                                (,(car top-level) ,frame-arg
                                 ,@(cdr top-level)))
           ,@geometry
           ,@user-default-initargs)
          ,@(when-let ((metaclass metaclass-name))
              `((:metaclass ,metaclass)))
          ,@other-options)

        ,@(when (or panes layouts)
            `(,(generate-generate-panes-form
                name menu-bar panes layouts pointer-documentation
                update-instances-on-redefinition)))

        ,@(when command-table
            `((define-command-table ,@command-table)))

        ,@(when command-definer
            `((defmacro ,command-definer (name-and-options arguments &rest body)
                (destructuring-bind (name &rest options)
                    (alexandria:ensure-list name-and-options)
                  `(define-command (,name :command-table ,',(first command-table)
                                          ,@options)
                       ,arguments ,@body)))))))))
