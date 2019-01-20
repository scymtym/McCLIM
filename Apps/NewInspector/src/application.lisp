(cl:in-package #:new-inspector)

;;; Application frame

(define-application-frame inspector ()
  ;; The object slot is used once to initialize the inspector pane,
  ;; then unused.
  ((%object :initarg :object
            :reader  object))
  (:panes
   (history    history-pane)
   (inspector  inspector-pane
               :root (object *application-frame*))
   (interactor :interactor
               :min-height 100))
  (:layouts
   (default
    (vertically ()
      (scrolling (:scroll-bars :horizontal
                  :height      0)
        history)
      (:fill (scrolling () inspector))
      (make-pane 'clime:box-adjuster-gadget)
      (1/16 interactor))))
  (:command-table (application :inherit-from (inspector)))
  (:command-definer nil)
  (:pointer-documentation t)
  (:default-initargs
   :width  1000
   :height 800
   :object (error "~@<Missing required initarg ~S.~@:>" :object)))

(defmethod frame-standard-output ((frame inspector))
  (find-pane-named frame 'interactor))

;;; Additional commands

(define-command (com-quit :command-table application
                          :name          t)
    ()
  (frame-exit *application-frame*))

;;; Interface

(defun inspector (object &key (new-process nil))
  (when (typep *application-frame* 'inspector)
    (restart-case (error "Clouseau called from inside Clouseau, possibly infinite recursion")
      (continue ()
        :report "Continue by starting a new Clouseau instance")
      (abort-clouseau ()
        :report "Abort this call to Clouseau"
        (return-from inspector))))

  (let ((frame (make-application-frame 'inspector :object object)))
    (flet ((run ()
             (let ((*print-length* 10)
                   (*print-level* 10))
               (run-frame-top-level frame))))
      (if new-process
          (let ((name (format nil "Inspector Clouseau: ~S" object)))
            (clim-sys:make-process #'run :name name))
          (run))
      ; (sleep 1)
      ; (clouseau:inspector (state (find-pane-named frame 'inspector)) :new-process t)
      object)))
