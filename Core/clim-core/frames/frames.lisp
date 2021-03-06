;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2000 Iban Hatchondo <hatchond@emi.u-bordeaux.fr>
;;;  (c) copyright 2000 Julien Boninfante <boninfan@emi.u-bordeaux.fr>
;;;  (c) copyright 2000,2014 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2004 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2019,2020,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Application frame classes and implementations of related protocol.

(in-package #:clim-internals)

;; *application-frame* is in decls.lisp
(defvar *default-frame-manager* nil)

;;; Frame-Manager class

;; FIXME: The spec says the port must "conform to options".  I've added a check
;; that the ports match, but we've no protocol for testing the other
;; options. -Hefner
(defun find-frame-manager (&rest options &key port &allow-other-keys)
  (declare (special *frame-manager*))
  (if (and (boundp '*frame-manager*)
           (or (null port) (eql port (port *frame-manager*))))
      *frame-manager*
      (if (and *default-frame-manager*
               (frame-manager-p *default-frame-manager*)
               (or (null port) (eql port (port *default-frame-manager*))))
          *default-frame-manager*
          (first (frame-managers (or port (apply #'find-port options)))))))

(defmacro with-frame-manager ((frame-manager) &body body)
  `(let ((*frame-manager* ,frame-manager))
     (declare (special *frame-manager*))
     (locally ,@body)))

(defmacro with-Look-and-feel-realization ((frame-manager frame) &body body)
  `(let ((*pane-realizer* ,frame-manager)
         (*application-frame* ,frame))
     (locally
         ,@body)))

(defun map-over-frames (function &key port frame-manager)
  (cond (frame-manager
         (mapc function (frame-manager-frames frame-manager)))
        (port
         (loop for manager in (frame-managers port)
               do (map-over-frames function :frame-manager manager)))
        (t (loop for p in *all-ports*
                 do (map-over-frames function :port p)))))

(defmethod note-frame-enabled ((fm frame-manager) frame)
  (declare (ignore frame))
  t)

(defmethod note-frame-disabled ((fm frame-manager) frame)
  (declare (ignore frame))
  t)

;;; XXX These should force the redisplay of the menu bar. They don't yet.

(defmethod note-command-enabled (frame-manager frame command-name)
  (declare (ignore frame-manager frame command-name))
  nil)

(defmethod note-command-disabled (frame-manager frame command-name)
  (declare (ignore frame-manager frame command-name))
  nil)

(declaim (type (or null pattern) *default-icon-large* *default-icon-small*))
(defvar *default-icon-large* nil)
(defvar *default-icon-small* nil)

(defclass standard-application-frame (application-frame
                                      presentation-history-mixin)
  (;; Windowing
   (port                   :initarg  :port
                           :accessor port
                           :initform nil)
   (graft                  :initarg  :graft
                           :accessor graft
                           :initform nil)
   (manager                :reader   frame-manager
                           :accessor %frame-manager
                           :initform nil)
   ;; Name and properties
   (state                  :initarg  :state
                           :type     (member :disowned :enabled :disabled :shrunk)
                           :reader   frame-state
                           :initform :disowned)
   (name                   :initarg  :name
                           :reader   frame-name)
   (pretty-name            :initarg  :pretty-name
                           :accessor frame-pretty-name)
   (icon                   :accessor frame-icon
                           :documentation
                           "If non-NIL, an array pattern or a sequence
                            of array patterns that should be used by
                            the host's window manager to represent the
                            frame, for example when it is iconified.")
   (properties             :initarg  :properties
                           :type     list ; plist
                           :accessor %frame-properties
                           :initform '())
   (client-settings        :accessor client-settings
                           :initform nil)
   ;; Geometry
   (geometry-left          :initarg :left
                           :accessor geometry-left
                           :initform nil)
   (geometry-right         :initarg :right
                           :accessor geometry-right
                           :initform nil)
   (geometry-top           :initarg :top
                           :accessor geometry-top
                           :initform nil)
   (geometry-bottom        :initarg :bottom
                           :accessor geometry-bottom
                           :initform nil)
   (geometry-width         :initarg :width
                           :accessor geometry-width
                           :initform nil)
   (geometry-height        :initarg :height
                           :accessor geometry-height
                           :initform nil)
   (resize-frame           :initarg  :resize-frame
                           :initform nil
                           :accessor frame-resize-frame)
   ;; Panes and layouts
   (top-level-sheet        :reader   frame-top-level-sheet
                           :initform nil)
   (panes                  :accessor frame-panes
                           :initform nil
                           :documentation
                           "The root of the tree of panes in the current layout.

                            The value of this slot changes when the
                            current layout changes and potentially
                            changes when the frame class is
                            redefined.")
   (current-panes          :type     list
                           :accessor frame-current-panes
                           :initform nil
                           :documentation
                           "

                            The value of this slot changes when the
                            current layout changes and potentially
                            changes when the frame class is
                            redefined.")
   (panes-for-layout       :type     list ; alist
                           :accessor frame-panes-for-layout
                           :initform nil
                           :documentation
                           "An alist of names and panes (as returned by
                            `make-pane') that can be referenced in
                            layouts.

                            The value of this slot does not change
                            when the current layout changes. The value
                            of this slot potentially changes when the
                            frame class is redefined.")
   (output-pane            :accessor frame-standard-output
                           :accessor frame-error-output
                           :initform nil)
   (input-pane             :accessor frame-standard-input
                           :initform nil)
   (documentation-pane     :accessor frame-pointer-documentation-output
                           :initform nil)
   (menu-bar               :initarg  :menu-bar
                           :initform nil)
   (menu-bar-pane          :accessor frame-menu-bar-pane
                           :initform nil)
   (layouts                :initarg  :layouts
                           :type     list
                           :reader   frame-layouts
                           :writer   (setf %frame-layouts)
                           :initform '())
   (current-layout         :initarg  :current-layout
                           :accessor frame-current-layout
                           :initform nil)
   ;; Command processing
   (command-table          :initarg  :command-table
                           :accessor frame-command-table
                           :initform nil)
   (disabled-commands      :initarg  :disabled-commands
                           :type     list
                           :accessor disabled-commands
                           :accessor frame-disabled-commands
                           :initform nil
                           :documentation
                           "A list of command names that have been
                            disabled in this frame.")
   (top-level              :initarg  :top-level
                           :reader   frame-top-level
                           :initform '(default-frame-top-level))
   (top-level-lambda       :initarg  :top-level-lambda
                           :reader   frame-top-level-lambda)
   (calling-frame          :initarg  :calling-frame
                           :reader   frame-calling-frame
                           :initform nil
                           :documentation
                           "The frame that is the parent of this
                            frame, if any.")
   ;; Event processing
   (process                :accessor frame-process
                           :initform nil)
   (event-queue            :initarg  :frame-event-queue
                           :accessor frame-event-queue
                           :initform nil
                           :documentation
                           "The event queue that, by default, will be
                            shared by all panes in the frame.")
   (input-buffer           :initarg  :frame-input-buffer
                           :accessor frame-input-buffer
                           :initform (make-instance 'concurrent-event-queue :port nil)
                           :documentation
                           "The input buffer queue that, by default,
                            will be shared by all input streams in the
                            frame.")
   ;; Presentations
   (highlited-presentation :initarg  :highlited-presentation
                           :accessor frame-highlited-presentation
                           :initform nil)
   (documentation-state    :accessor frame-documentation-state
                           :initform nil
                           :documentation
                           "Used to keep of track of what needs to be
                            rendered in the pointer documentation
                            frame.")
   (documentation-record   :initform nil
                           :accessor documentation-record
                           :documentation
                           "Updating output record for pointer
                            documentation produced by
                            presentations.")))

;;; This method causes related frames to share the same queue by
;;; default (on both SMP and non-SMP systems). Thanks to that we have
;;; a single loop processing events. Alternative approach is executed
;;; with window-stream frames which have a standalone-event-loop (see
;;; panes.lisp). -- jd 2018-12-27
(defmethod initialize-instance :after ((obj standard-application-frame)
                                       &key (icon nil icon-supplied-p)
                                       &allow-other-keys)
  (labels ((coerce-to-icon (thing)
             (typecase thing
               ((or string pathname)
                (make-pattern-from-bitmap-file thing))
               (sequence
                (map 'list #'coerce-to-icon thing))
               (t
                thing))))
    (setf (slot-value obj 'icon)
          (cond ((not icon-supplied-p)
                 (remove nil (list *default-icon-large* *default-icon-small*)))
                ((null icon)
                 nil)
                (t
                 (coerce-to-icon icon)))))
  (unless (frame-event-queue obj)
    (when-let* ((calling-frame (frame-calling-frame obj))
                (calling-queue (frame-event-queue calling-frame)))
      (setf (frame-event-queue obj) calling-queue)
      (return-from initialize-instance))
    (setf (frame-event-queue obj)
          (if *multiprocessing-p*
              (make-instance 'concurrent-event-queue)
              (make-instance 'simple-event-queue)))))

(defmethod (setf frame-manager) (fm (frame application-frame))
  (let ((old-manager (frame-manager frame)))
    (setf (%frame-manager frame) nil)
    (when old-manager
      (disown-frame old-manager frame)
      (setf (frame-panes frame) nil)
      (setf (slot-value frame 'layouts) nil))
    (setf (%frame-manager frame) fm)))

(defmethod frame-parent ((frame standard-application-frame))
  (or (frame-calling-frame frame)
      (frame-manager frame)))

(defmethod (setf frame-pretty-name) :after (new-value frame)
  ;; If there is a top-level sheet, set its pretty name. The port can
  ;; reflect this change in the window title.
  (when-let ((top-level-sheet (frame-top-level-sheet frame)))
    (setf (sheet-pretty-name top-level-sheet) new-value))
  ;; Let client code know.
  (clime:note-frame-pretty-name-changed (frame-manager frame) frame new-value))

(defmethod (setf frame-icon) :after (new-value frame)
  ;; If there is a top-level sheet, set its icon. The port can reflect
  ;; this change by telling the window manager which might display the
  ;; new icon somewhere.
  (when-let ((top-level-sheet (frame-top-level-sheet frame)))
    (setf (sheet-icon top-level-sheet) new-value))
  ;; Let client code know.
  (note-frame-icon-changed (frame-manager frame) frame new-value))

(defmethod frame-properties ((frame application-frame) property)
  (getf (%frame-properties frame) property))

(defmethod (setf frame-properties) (value (frame application-frame) property)
  (setf (getf (%frame-properties frame) property) value))

(defgeneric frame-geometry* (frame))

(defmethod frame-geometry* ((frame standard-application-frame))
  "-> width height &optional top left"
  (let ((pane (frame-top-level-sheet frame)))
    ;(destructuring-bind (&key left top right bottom width height) (frame-geometry frame)
    (with-slots (geometry-left geometry-top geometry-right
                               geometry-bottom geometry-width
                               geometry-height) frame
      ;; Find width and height from looking at the respective options
      ;; first, then at left/right and top/bottom and finally at what
      ;; compose-space says.
      (let* ((width (or geometry-width
                        (and geometry-left geometry-right
                             (- geometry-right geometry-left))
                        (space-requirement-width (compose-space pane))))
             (height (or geometry-height
                         (and geometry-top geometry-bottom (- geometry-bottom geometry-top))
                         (space-requirement-height (compose-space pane))))
             ;; See if a position is wanted and return left, top.
             (left (or geometry-left
                       (and geometry-right (- geometry-right geometry-width))))
             (top (or geometry-top
                      (and geometry-bottom (- geometry-bottom geometry-height)))))
      (values width height left top)))))

(defun find-pane-of-type (parent type)
  "Returns a pane of `type' in the forest growing from `parent'."
  (map-over-sheets (lambda (pane)
                     (when (typep pane type)
                       (return-from find-pane-of-type pane)))
                   parent)
  nil)

(defmethod get-frame-pane ((frame application-frame) pane-name)
  (let ((pane (find-pane-named frame pane-name)))
    (if (typep pane 'clim-stream-pane)
        pane
        nil)))

(defmethod find-pane-named ((frame application-frame) pane-name)
  (map-over-sheets (lambda (pane)
                     (when (eql pane-name (pane-name pane))
                       (return-from find-pane-named pane)))
                   (frame-panes frame))
  nil)

#+nil
(defmethod redisplay-frame-panes ((frame application-frame) &key force-p)
  (map-over-sheets
   (lambda (sheet)
     (when (typep sheet 'pane)
       (when (and (typep sheet 'clim-stream-pane)
                  (not (eq :no-clear (pane-redisplay-needed sheet))))
         (window-clear sheet))
       (redisplay-frame-pane frame sheet :force-p force-p)))
   (frame-top-level-sheet frame)))

(defmethod redisplay-frame-panes ((frame application-frame) &key force-p)
  (map-over-sheets (lambda (sheet)
                     (when (sheet-viewable-p sheet)
                       (redisplay-frame-pane frame sheet :force-p force-p)))
                   (frame-top-level-sheet frame)))

(defmethod frame-replay (frame stream &optional region)
  (declare (ignore frame))
  (stream-replay stream region))

;;; Panes an layouts

(defun disown-named-panes (frame &key (allp nil))
  ;; Maybe disown the "layout root" sheet from the top level sheet of
  ;; FRAME.
  (when-let* ((root (frame-panes frame))
              (tls  (frame-top-level-sheet frame)))
    (when (eq (sheet-parent root) tls)
      (sheet-disown-child tls root)))
  ;; In order to build a new layout including the named sheets in
  ;; FRAME, disown all of those from their respective parents if
  ;; necessary. Unless ALLP is true, do not disown panes that a
  ;; ancestor which is a named pane since that situation implies that
  ;; the descendant will not be referenced in the layout. This doesn't
  ;; do anything for the initial layout.
  (let ((panes-for-layout (frame-panes-for-layout frame)))
    (labels ((named-ancestor-p (pane)
               (or (rassoc pane panes-for-layout)
                   (when-let ((parent (sheet-parent pane)))
                     (named-ancestor-p parent)))))
      (loop for (nil . pane) in panes-for-layout
            do (when-let ((parent (sheet-parent pane)))
                 (when (or allp (not (named-ancestor-p parent)))
                   (sheet-disown-child parent pane)))))))

(defun update-frame-pane-lists (frame)
  (let ((all-panes     (frame-panes frame))
        (named-panes   (mapcar #'cdr (frame-panes-for-layout frame)))
        (current-panes '()))
    ;; Find intersection of named panes and current layout panes.
    (map-over-sheets (lambda (sheet)
                       (when-let ((index (position sheet named-panes)))
                         (push (cons sheet index) current-panes)))
                     all-panes)
    (setf current-panes (mapcar #'car (sort current-panes #'< :key #'cdr)))
    ;; Populate current-pane list and special pane slots.
    (let ((interactor            (find-pane-of-type current-panes 'interactor-pane))
          (application           (find-pane-of-type current-panes 'application-pane))
          (pointer-documentation (find-pane-of-type all-panes 'pointer-documentation-pane)))
      (setf (frame-current-panes frame) current-panes
            (frame-standard-output frame) (or application interactor)
            (frame-standard-input frame) (or interactor (frame-standard-output frame))
            (frame-pointer-documentation-output frame) pointer-documentation))))

(defmethod generate-panes :around (frame-manager (frame application-frame))
  (with-look-and-feel-realization (frame-manager frame)
    (call-next-method)))

(defmethod generate-panes :before (frame-manager (frame application-frame))
  (disown-named-panes frame :allp t))

(defmethod apply-layout :after ((frame application-frame))
  (let ((top-level-sheet (frame-top-level-sheet frame)))
    (sheet-adopt-child top-level-sheet (frame-panes frame))
    (unless (sheet-parent top-level-sheet)
      (sheet-adopt-child (graft frame) top-level-sheet))
    ;; Update frame-current-panes and the special pane slots.
    (update-frame-pane-lists frame)
    ;; Find the size of the new frame
    (multiple-value-bind (w h) (frame-geometry* frame)
      ;; automatically generates a window-configuation-event
      ;; which then calls allocate-space
      ;;
      ;; Not any longer, we turn off CONFIGURE-NOTIFY events until the
      ;; window is mapped and do the space allocation now, so that all
      ;; sheets will have their correct geometry at once. --GB
      (change-space-requirements top-level-sheet :width w :height h
                                                 :resize-frame t)
      (setf (sheet-region top-level-sheet) (make-bounding-rectangle 0 0 w h))
      (allocate-space top-level-sheet w h))))

;;; These defaults are used when `define-application-frame' does not
;;; generate a specialized `generate-panes' method for an application
;;; frame class.
(defmethod generate-panes (frame-manager (frame application-frame))
  (unless (frame-panes-for-layout frame)
    (setf (frame-panes-for-layout frame)
          `((single-pane . ,(make-clim-interactor-pane :name 'single-pane)))))
  (let ((single-pane
          (alexandria:assoc-value (frame-panes-for-layout frame)
                                  'single-pane :test #'eq)))
    (setf (frame-panes frame) single-pane))
  (apply-layout frame))

(defmethod apply-layout ((frame application-frame)))

(defmethod frame-query-io ((frame standard-application-frame))
  (or (frame-standard-input frame)
      (frame-standard-output frame)))

(defmethod frame-all-layouts ((frame application-frame))
  (mapcar #'car (frame-layouts frame)))

(define-condition frame-layout-changed (condition)
  ((frame :initarg :frame :reader frame-layout-changed-frame)))

(defmethod (setf frame-current-layout) :around (name (frame application-frame))
  (unless (eql name (frame-current-layout frame))
    (disown-named-panes frame)
    (call-next-method)
    (when-let ((frame-manager (frame-manager frame)))
      (with-look-and-feel-realization (frame-manager frame)
        (if-let ((tls (and (frame-resize-frame frame)
                           (frame-top-level-sheet frame))))
          (multiple-value-bind (width height) (bounding-rectangle-size tls)
            (apply-layout frame)
            (layout-frame frame width height))
          (progn
            (apply-layout frame)
            (layout-frame frame))))
      (signal 'frame-layout-changed :frame frame))))

(defmethod layout-frame ((frame application-frame) &optional width height)
  (when (and (or width height)
             (not (and width height)))
    (error "LAYOUT-FRAME must be called with both WIDTH and HEIGHT or neither"))
  (with-inhibited-dispatch-repaint ()
    (let ((pane (frame-panes frame)))
      (when (and (null width) (null height))
        (let (;;I guess this might be wrong. --GB 2004-06-01
              (space (compose-space pane)))
          (setq width (space-requirement-width space))
          (setq height (space-requirement-height space))))
      (let ((tpl-sheet (frame-top-level-sheet frame)))
        (unless (and (= width (bounding-rectangle-width tpl-sheet))
                     (= height (bounding-rectangle-height tpl-sheet)))
          (resize-sheet tpl-sheet width height)))
      (allocate-space pane width height))))

;;; Command loop interface

(defmethod (setf frame-command-table) :around (new-command-table frame)
  (flet ((get-menu (x) (slot-value x 'menu)))
    (if (and (get-menu (frame-command-table frame))
             (get-menu new-command-table))
        (prog1 (call-next-method)
          (when-let ((menu-bar-pane (frame-menu-bar-pane frame)))
            (update-menu-bar menu-bar-pane new-command-table)))
        (call-next-method))))

(define-condition frame-exit (condition)
  ((frame :initarg :frame :reader frame-exit-frame)
   (handled :accessor %frame-exit-handled :initform nil)))

(defmethod frame-exit ((frame standard-application-frame))
  (signal 'frame-exit :frame frame))

(defmethod redisplay-frame-pane ((frame application-frame) pane &key force-p)
  (declare (ignore pane force-p))
  nil)

(defgeneric medium-invoke-with-possible-double-buffering (frame pane medium continuation))

(defmethod medium-invoke-with-possible-double-buffering (frame pane medium continuation)
  (funcall continuation))

(defgeneric invoke-with-possible-double-buffering (frame pane continuation))

(defmethod invoke-with-possible-double-buffering (frame pane continuation)
  (declare (ignore frame pane))
  (funcall continuation))

(defmethod invoke-with-possible-double-buffering (frame (pane sheet-with-medium-mixin) continuation)
  (medium-invoke-with-possible-double-buffering frame pane (sheet-medium pane) continuation))

(defmacro with-possible-double-buffering ((frame pane) &body body)
  `(invoke-with-possible-double-buffering ,frame ,pane (lambda () ,@body)))

(defmethod redisplay-frame-pane :around ((frame application-frame) pane
                                         &key force-p)
  (let ((pane-object (if (typep pane 'pane)
                         pane
                         (find-pane-named frame pane))))
    (restart-case
        (multiple-value-bind (redisplayp clearp)
            (pane-needs-redisplay pane-object)
          (when force-p
            (setq redisplayp (or redisplayp t)
                  clearp t))
          (when redisplayp
            (when-let ((highlited (frame-highlited-presentation frame)))
              (highlight-presentation-1 (car highlited)
                                        (cdr highlited)
                                        :unhighlight)
              (setf (frame-highlited-presentation frame) nil))
            (with-possible-double-buffering (frame pane-object)
              (when clearp
                (window-clear pane-object))
              (call-next-method))
            (unless (or (eq redisplayp :command-loop) (eq redisplayp :no-clear))
              (setf (pane-needs-redisplay pane-object) nil))))
      (clear-pane-try-again ()
       :report "Clear the output history of the pane and reattempt forceful redisplay."
       (window-clear pane)
       (redisplay-frame-pane frame pane :force-p t))
      (clear-pane ()
       :report "Clear the output history of the pane, but don't redisplay."
       (window-clear pane))
      (skip-redisplay ()
       :report "Skip this redisplay."))))

(defmethod run-frame-top-level ((frame application-frame)
                                &key &allow-other-keys)
  (letf (((frame-process frame) (current-process)))
    (funcall (frame-top-level-lambda frame) frame)))

(defmethod run-frame-top-level :around ((frame application-frame) &key)
  (let ((*application-frame* frame)
        (*input-context* nil)
        (*input-wait-test* nil)
        (*input-wait-handler* nil)
        (*pointer-button-press-handler* nil)
        (original-state (frame-state frame)))
    (declare (special *input-wait-test* *input-wait-handler*
                      *pointer-button-press-handler*))
    (when (eq (frame-state frame) :disowned) ; Adopt frame into frame manager
      (adopt-frame (or (frame-manager frame) (find-frame-manager))
                   frame))
    (unless (or (eq (frame-state frame) :enabled)
                (eq (frame-state frame) :shrunk))
      (enable-frame frame))
    (unwind-protect
         (loop named run-frame-loop
               for query-io = (frame-query-io frame)
               for *default-frame-manager* = (frame-manager frame)
               do (block run-frame-iter
                    (handler-bind
                        ((frame-layout-changed
                           (lambda (condition)
                             (declare (ignore condition))
                             (return-from run-frame-iter)))
                         (frame-exit
                           (lambda (condition)
                             (unless (%frame-exit-handled condition)
                               (setf (%frame-exit-handled condition) t)
                               (let ((exiting-frame (frame-exit-frame condition)))
                                 (if (eq exiting-frame frame)
                                     (return-from run-frame-loop)
                                     (disown-frame (frame-manager exiting-frame)
                                                   exiting-frame)))))))
                      (return-from run-frame-loop
                        (if query-io
                            (with-input-focus (query-io)
                              (call-next-method))
                            (call-next-method))))))
      (case original-state
        (:disabled
         (disable-frame frame))
        (:disowned
         (when-let ((fm (frame-manager frame)))
           (disown-frame fm frame)))))))

(defparameter +default-prompt-style+ (make-text-style :sans-serif :bold :normal))

(defgeneric execute-frame-command (frame command))

(defmethod default-frame-top-level
    ((frame application-frame)
     &key (command-parser 'command-line-command-parser)
          (command-unparser 'command-line-command-unparser)
          (partial-command-parser
           'command-line-read-remaining-arguments-for-partial-command)
          (prompt "Command: "))
  ;; Give each pane a fresh start first time through.
  (let ((needs-redisplay t)
        (first-time t))
    (loop
       ;; The variables are rebound each time through the loop because the
       ;; values of frame-standard-input et al. might be changed by a command.
       ;;
       ;; We rebind *QUERY-IO* ensuring variable is always a stream,
       ;; but we use FRAME-QUERY-IO for our own actions and to decide
       ;; whenever frame has the query IO stream associated with it..
       (let* ((frame-query-io (frame-query-io frame))
              (interactorp (typep frame-query-io 'interactor-pane))
              (*standard-input*  (or (frame-standard-input frame)  *standard-input*))
              (*standard-output* (or (frame-standard-output frame) *standard-output*))
              (*query-io* (or frame-query-io *query-io*))
              ;; during development, don't alter *error-output*
              ;; (*error-output* (frame-error-output frame))
              (*pointer-documentation-output* (frame-pointer-documentation-output frame))
              (*command-parser* command-parser)
              (*command-unparser* command-unparser)
              (*partial-command-parser* partial-command-parser))
         (restart-case
             (flet ((execute-command ()
                      (when-let ((command (read-frame-command frame :stream frame-query-io)))
                        (setq needs-redisplay t)
                        (execute-frame-command frame command))))
               (when needs-redisplay
                 (redisplay-frame-panes frame :force-p first-time)
                 (setq first-time nil
                       needs-redisplay nil))
               (when interactorp
                 (setf (cursor-visibility (stream-text-cursor frame-query-io)) nil)
                 (when prompt
                   (with-text-style (frame-query-io +default-prompt-style+)
                     (if (stringp prompt)
                         (write-string prompt frame-query-io)
                         (funcall prompt frame-query-io frame))
                     (force-output frame-query-io))))
               (execute-command)
               (when interactorp
                 (fresh-line frame-query-io)))
           (abort ()
             :report "Return to application command loop."
             (if interactorp
                 (format frame-query-io "~&Command aborted.~&")
                 (beep))))))))

(defmethod read-frame-command :around ((frame application-frame)
                                       &key (stream *standard-input*))
  (with-input-context ('menu-item)
      (object)
      (call-next-method)
    (menu-item
     (let* ((command (alexandria:ensure-list (command-menu-item-value object)))
            (table (frame-command-table frame))
            (canonical (partial-command-from-name (car command) table)))
       ;; When the command has more arguments than its "canonical form", that
       ;; is the command with all required arguments filled, that means that
       ;; it has all required arguments *and* some optional arguments.
       (unless (> (length command) (length canonical))
         (map-into canonical #'identity command)
         (setf command canonical))
       (if (partial-command-p command)
           (funcall *partial-command-parser* table stream command 0)
           command)))))

(defmethod read-frame-command ((frame application-frame)
                               &key (stream *standard-input*))
  ;; The following is the correct interpretation according to the spec.
  ;; I think it is terribly counterintuitive and want to look into
  ;; what existing CLIMs do before giving in to it.
  ;; If we do things as the spec says, command accelerators will
  ;; appear to not work, confusing new users.
  #+NIL (read-command (frame-command-table frame) :use-keystrokes nil :stream stream)
  (if stream
      (read-command (frame-command-table frame) :use-keystrokes t :stream stream)
      (simple-event-loop frame)))

(define-event-class execute-command-event (window-manager-event)
  ((sheet :initarg :sheet :reader event-sheet)
   (command :initarg :command :reader execute-command-event-command)))

(defmethod execute-frame-command ((frame application-frame) command)
  ;; ### FIXME: I'd like a different method than checking for
  ;; *application-frame* to decide, which process processes which
  ;; frames command loop. Perhaps looking ath the process slot?
  ;; --GB 2005-11-28
  (check-type command cons)
  (cond ((eq *application-frame* frame)
         (restart-case
             (apply (command-name command) (command-arguments command))
           (try-again ()
            :report (lambda (stream)
                      (format stream "Try executing the command ~S again." (command-name command)))
            (execute-frame-command frame command))))
        (t
         (let ((eq (sheet-event-queue (frame-top-level-sheet frame))))
           (event-queue-append eq (make-instance 'execute-command-event
                                                  :sheet frame
                                                  :command command))))))

(defmethod handle-event ((frame application-frame) (event execute-command-event))
  (execute-frame-command frame (execute-command-event-command event)))

(defmethod command-enabled (command-name (frame standard-application-frame))
  (and (command-accessible-in-command-table-p command-name
                                              (frame-command-table frame))
       (not (member command-name (disabled-commands frame)))))

(defmethod (setf command-enabled)
    (enabled command-name (frame standard-application-frame))
  (unless (command-accessible-in-command-table-p command-name
                                                 (frame-command-table frame))
    (return-from command-enabled nil))
  (with-accessors ((disabled-commands disabled-commands))
      frame
    (if enabled
        (progn
          (setf disabled-commands (delete command-name disabled-commands))
          (note-command-enabled (frame-manager frame)
                                frame
                                command-name)
          enabled)
        (progn
          (pushnew command-name disabled-commands)
          (note-command-disabled (frame-manager frame)
                                 frame
                                 command-name)
          nil))))

(defmethod display-command-menu (frame (stream fundamental-output-stream)
                                 &rest args &key
                                 (command-table (frame-command-table frame))
                                 initial-spacing row-wise max-width
                                 max-height n-rows n-columns
                                 (cell-align-x :left) (cell-align-y :top))
  (declare (ignore initial-spacing row-wise max-width max-height
                   n-rows n-columns cell-align-x cell-align-y))
  (with-keywords-removed (args (:command-table))
    (apply #'display-command-table-menu command-table stream args)))

(defmethod make-pane-1 :around (fm (frame standard-application-frame) type
                                &rest args
                                &key (event-queue nil evq-p) &allow-other-keys)
  ;; Default event-queue to the frame event queue.
  (declare (ignore event-queue))
  (if (null evq-p)
      (let ((evq (frame-event-queue frame))
            (*input-buffer* (frame-input-buffer frame)))
        (apply #'call-next-method fm frame type :event-queue evq args))
      (call-next-method)))

(defmethod find-pane-for-frame ((fm frame-manager) (frame application-frame))
  (make-pane-1 fm frame 'top-level-sheet-pane
               :name (frame-name frame)
               :pretty-name (frame-pretty-name frame)
               :icon (frame-icon frame)
               ;; sheet is enabled from enable-frame
               :enabled-p nil))

(defmethod adopt-frame ((fm frame-manager) (frame application-frame))
  (setf (slot-value fm 'frames) (cons frame (slot-value fm 'frames)))
  (setf (frame-manager frame) fm)
  (setf (port frame) (port fm))
  (setf (graft frame) (find-graft :port (port frame)))
  (let ((*application-frame* frame)
        (event-queue (frame-event-queue frame)))
    (setf (slot-value frame 'top-level-sheet)
          (find-pane-for-frame fm frame))
    (generate-panes fm frame)
    (setf (slot-value frame 'state) :disabled)
    (when (typep event-queue 'event-queue)
      (setf (event-queue-port event-queue) (port fm)))
    frame))

(defmethod disown-frame ((fm frame-manager) (frame application-frame))
  (when-let* ((event-queue (frame-event-queue frame))
              (calling-frame (frame-calling-frame frame))
              (calling-queue (frame-event-queue calling-frame))
              (another-queue-p (not (eql calling-queue event-queue))))
    (setf (event-queue-port event-queue) nil))
  (setf (slot-value fm 'frames) (remove frame (slot-value fm 'frames)))
  (sheet-disown-child (graft frame) (frame-top-level-sheet frame))
  (setf (%frame-manager frame) nil)
  (setf (slot-value frame 'state) :disowned)
  (port-force-output (port fm))
  frame)

(defmethod enable-frame ((frame application-frame))
  (let ((t-l-s (frame-top-level-sheet frame)))
    (setf (sheet-enabled-p t-l-s) t)
    (when-let ((port (port t-l-s)))
      (port-force-output port)))
  (setf (slot-value frame 'state) :enabled)
  (note-frame-enabled (frame-manager frame) frame))

(defmethod disable-frame ((frame application-frame))
  (let ((t-l-s (frame-top-level-sheet frame)))
    (setf (sheet-enabled-p t-l-s) nil)
    (when-let ((port (port t-l-s)))
      (port-force-output port)))
  (setf (slot-value frame 'state) :disabled)
  (note-frame-disabled (frame-manager frame) frame))

(defmethod destroy-frame ((frame application-frame))
  (when (eq (frame-state frame) :enabled)
    (disable-frame frame))
  (disown-frame (frame-manager frame) frame))

(defmethod raise-frame ((frame application-frame))
  (raise-sheet (frame-top-level-sheet frame)))

(defmethod bury-frame ((frame application-frame))
  (bury-sheet (frame-top-level-sheet frame)))

(defun make-application-frame (frame-name
                               &rest options
                               &key (frame-manager nil frame-manager-p)
                                    enable
                                    (state nil state-supplied-p)
                                    save-under (frame-class frame-name)
                               &allow-other-keys)
  (declare (ignore save-under))
  (with-keywords-removed (options (:frame-manager :enable :state
                                   :save-under :frame-class))
    (let ((frame (apply #'make-instance frame-class
                        :name frame-name
                        options)))
      (when frame-manager-p
        (adopt-frame frame-manager frame))
      (cond ((or enable (eq state :enabled))
             (enable-frame frame))
            ((and (eq state :disowned)
                  (not (eq (frame-state frame) :disowned)))
             (disown-frame (frame-manager frame) frame))
            (state-supplied-p
             (warn ":state ~S not supported yet." state)))
      frame)))

(defgeneric clim-extensions:find-frame-type (frame)
  (:method ((frame t))
    nil)
  (:documentation "Returns the type of the given frame. The return value of this
function can be used by the frame manager to determine the behaviour
of the frame.

This function should never be called by application code. Instead, the
application should define a method for this function that returns the
appropriate value for a frame.

The following values are currently supported by the CLX backend:

NIL - Default frame behaviour.

:OVERRIDE-REDIRECT - The frame will be displayed in front of all other
frames and will not have focus.

:DIALOG - The frame will not have any decorations added by the window manager."))

;;; From Franz Users Guide

(defun find-application-frame (frame-name &rest initargs
                               &key (create t) (activate t)
                               (own-process *multiprocessing-p*) port
                               frame-manager frame-class
                               &allow-other-keys)
  (declare (ignorable frame-class))
  (let ((frame (unless (eq create :force)
                 (block
                     found-frame
                   (map-over-frames
                    #'(lambda (frame)
                        (when (eq (frame-name frame) frame-name)
                          (return-from found-frame frame)))
                    :port port
                    :frame-manager frame-manager)))))
    (unless (or frame create)
      (return-from find-application-frame nil))
    (unless frame
      (with-keywords-removed (initargs (:create :activate :own-process))
        (setq frame (apply #'make-application-frame frame-name initargs))))
    (when (and frame activate)
      (cond ((frame-process frame)
             (raise-frame frame))
            (own-process
             (clim-sys:make-process #'(lambda ()
                                        (run-frame-top-level frame))
                                    :name (format nil "~A" frame-name)))
            (t (run-frame-top-level frame))))
    frame))

;;; Menu frame class

(defclass menu-frame ()
  ((left :initform 0 :initarg :left)
   (top :initform 0 :initarg :top)
   (min-width :initform nil :initarg :min-width)
   (top-level-sheet :initform nil :reader frame-top-level-sheet)
   (panes :reader frame-panes :initarg :panes)
   (graft :initform nil :accessor graft)
   (state :initarg :state
          :initform :disowned
          :reader frame-state)
   (manager :initform nil :accessor frame-manager)))

(defclass menu-unmanaged-top-level-sheet-pane (unmanaged-top-level-sheet-pane)
  ())

(defmethod adopt-frame ((fm frame-manager) (frame menu-frame))
  (setf (slot-value fm 'frames) (cons frame (slot-value fm 'frames)))
  (setf (frame-manager frame) fm)
  (let* ((t-l-s (make-pane-1 fm *application-frame*
                             'menu-unmanaged-top-level-sheet-pane
                             :name 'top-level-sheet
                             ;; enabling should be left to enable-frame
                             :enabled-p nil)))
    (setf (slot-value frame 'top-level-sheet) t-l-s)
    (sheet-adopt-child t-l-s (frame-panes frame))
    (let ((graft (find-graft :port (port fm))))
      (sheet-adopt-child graft t-l-s)
      (setf (graft frame) graft))
    (let ((pre-space (compose-space t-l-s))
          (frame-min-width (slot-value frame 'min-width)))
      (multiple-value-bind (width min-width max-width height min-height max-height)
          (space-requirement-components pre-space)
        (flet ((foomax (x y) (max (or x 1) (or y 1))))
          (let ((space (make-space-requirement :min-width  (foomax frame-min-width min-width)
                                               :width      (foomax frame-min-width width)
                                               :max-width  (foomax frame-min-width max-width)
                                               :min-height min-height
                                               :height     height
                                               :max-height max-height)))
            (allocate-space (frame-panes frame)
                            (space-requirement-width space)
                            (space-requirement-height space))
            (setf (sheet-region t-l-s)
                  (make-bounding-rectangle 0 0
                                           (space-requirement-width space)
                                           (space-requirement-height space))))
          (setf (sheet-transformation t-l-s)
                (make-translation-transformation (slot-value frame 'left)
                                                 (slot-value frame 'top))))))))

(defmethod disown-frame ((fm frame-manager) (frame menu-frame))
  (setf (slot-value fm 'frames) (remove frame (slot-value fm 'frames)))
  (sheet-disown-child (graft frame) (frame-top-level-sheet frame))
  (setf (frame-manager frame) nil))

(defmethod enable-frame ((frame menu-frame))
  (setf (sheet-enabled-p (frame-top-level-sheet frame)) t)
  (setf (slot-value frame 'state) :enabled)
  (note-frame-enabled (frame-manager frame) frame))

(defmethod disable-frame ((frame menu-frame))
  (let ((t-l-s (frame-top-level-sheet frame)))
    (setf (sheet-enabled-p t-l-s) nil)
    (when (port t-l-s)
      (port-force-output (port t-l-s))))
  (setf (slot-value frame 'state) :disabled)
  (note-frame-disabled (frame-manager frame) frame))

(defun make-menu-frame (pane &key (left 0) (top 0) (min-width 1))
  (make-instance 'menu-frame :panes pane :left left :top top :min-width min-width))

;;; Frames and presentations
(defmethod frame-maintain-presentation-histories
    ((frame standard-application-frame))
  (if (find-pane-of-type (frame-panes frame) 'interactor-pane)
      t
      nil))

(defmethod frame-find-innermost-applicable-presentation
    ((frame standard-application-frame) input-context stream x y
     &key event)
  (find-innermost-applicable-presentation input-context stream
                                          x y
                                          :frame frame :event event))

(defmethod frame-input-context-button-press-handler
    ((frame standard-application-frame)
     (stream output-recording-stream)
     event)
  (when-let ((presentation (find-innermost-presentation-match
                            *input-context*
                            (stream-output-history stream)
                            frame stream
                            (pointer-event-x event)
                            (pointer-event-y event)
                            event)))
    (throw-highlighted-presentation presentation *input-context* event)))

(defmethod frame-input-context-button-press-handler
    ((frame standard-application-frame) stream button-press-event)
  (declare (ignore stream button-press-event))
  nil)

(defmethod frame-input-context-track-pointer
    ((frame standard-application-frame)
     input-context
     (stream output-recording-stream) event)
  (declare (ignore input-context event))
  nil)

(defmethod frame-input-context-track-pointer
    ((frame standard-application-frame) input-context stream event)
  (declare (ignore input-context stream event))
  nil)

(defun frame-highlight-at-position (frame stream event input-context
                                    &key (highlight t))
  "Given stream x,y; key modifiers; input-context, find the applicable
   presentation and maybe highlight it."
  (flet ((maybe-unhighlight (presentation)
           (when (and (frame-highlited-presentation frame)
                      (or (not highlight)
                          (not (eq presentation
                                   (car (frame-highlited-presentation frame))))))
             (highlight-presentation-1 (car (frame-highlited-presentation frame))
                                       (cdr (frame-highlited-presentation frame))
                                       :unhighlight)
             (setf (frame-highlited-presentation frame) nil))))
    (if (output-recording-stream-p stream)
        (let* ((top-record (stream-output-history stream))
               (presentation (find-innermost-presentation-match
                              input-context top-record frame stream
                              (device-event-x event)
                              (device-event-y event)
                              event
                              :override '(:type nil))))
          (maybe-unhighlight presentation)
          (when (and presentation
                     highlight
                     (not (eq presentation
                              (car (frame-highlited-presentation frame)))))
            (setf (frame-highlited-presentation frame)
                  (cons presentation stream))
            (highlight-presentation-1 presentation stream :highlight))
          presentation)
        (progn
          (maybe-unhighlight nil)
          nil))))

(defmethod frame-input-context-track-pointer :before
    ((frame standard-application-frame) input-context
     (stream output-recording-stream) event)
  (frame-highlight-at-position frame stream event input-context)
  (frame-update-pointer-documentation frame input-context stream event))

(defun simple-event-loop (&optional (frame *application-frame*))
  "A simple event loop for applications that want all events to be handled by
 handle-event methods"
  (let ((queue (frame-event-queue frame)))
    (loop for event = (event-queue-read queue)
       ;; EVENT-QUEUE-READ in single-process mode calls PROCESS-NEXT-EVENT itself.
       do (handle-event (event-sheet event) event))))

;;; Am I missing something?  Does this need to do more? - moore
(defmacro with-application-frame ((frame) &body body)
  `(let ((,frame *application-frame*))
     ,@body))

(defmethod (setf client-setting) (value frame setting)
  (setf (getf (client-settings frame) setting) value))

(defmethod reset-frame (frame &rest client-settings)
  (loop for (setting value) on client-settings by #'cddr
        do (setf (client-setting frame setting) value)))


(defmethod frame-drag-and-drop-feedback
    ((frame standard-application-frame) from-presentation stream
     initial-x initial-y x y state))

(defmethod frame-drag-and-drop-feedback
    ((frame standard-application-frame) from-presentation (stream encapsulating-stream)
     initial-x initial-y x y state)
  (frame-drag-and-drop-feedback frame from-presentation (encapsulating-stream-stream stream)
                                initial-x initial-y x y state))

(defmethod frame-drag-and-drop-feedback
    ((frame standard-application-frame) from-presentation (stream output-recording-stream)
     initial-x initial-y x y state)
  (with-bounding-rectangle* (fp-x1 fp-y1 fp-x2 fp-y2)
      from-presentation
    ;; Offset from origin of presentation is preserved throughout
    (let* ((x-off (- fp-x1 initial-x))
           (y-off (- fp-y1 initial-y))
           (highlite-x1 (+ x-off x))
           (highlite-y1 (+ y-off y))
           (highlite-x2 (+ highlite-x1 (- fp-x2 fp-x1)))
           (highlite-y2 (+ highlite-y1 (- fp-y2 fp-y1))))
      (with-identity-transformation (stream)
        (ecase state
          (:highlight
           (with-output-recording-options (stream :record nil)
             (draw-rectangle* stream highlite-x1 highlite-y1 highlite-x2 highlite-y2
                              :filled nil :line-dashes #(4 4))))
          (:unhighlight
           (with-output-recording-options (stream :record nil)
             (draw-rectangle* stream
                              highlite-x1 highlite-y1
                              (1+ highlite-x2) (1+ highlite-y2)
                              :ink (medium-background (sheet-medium stream))))
           (stream-replay stream (make-rectangle* highlite-x1 highlite-y1
                                                  (1+ highlite-x2) (1+ highlite-y2)))))))))

(defmethod frame-drag-and-drop-highlighting
    ((frame standard-application-frame) to-presentation stream state)
  (highlight-presentation-1 to-presentation stream state))
