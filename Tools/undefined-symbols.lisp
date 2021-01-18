(in-package #:clim-user)

(defun check ()
  (loop with error-count = 0
        with info = (alexandria:with-input-from-file (stream "../data/clim-symbols.sexp")
                      (read stream))
        for (name kind) in info
        do (case kind
             (:concept)
             (:option
              (unless (find-symbol (string-upcase (subseq name 1)) '#:keyword)
                (incf error-count)
                (format t "~&Error: ~S not in KEYWORD package.~%" name)))
             (t
              (let ((symbol (find-symbol (string-upcase name) '#:clim)))
                (cond ((not symbol)
                       (incf error-count)
                       (format t "~&Error: ~S not in CLIM package.~%" name))
                      ((not (check-symbol symbol kind))
                       (incf error-count)
                       (format t "~&Error: ~S does not name a ~S.~%" symbol kind))))))
        finally (when (plusp error-count)
                  (format t "~D error~:P~%" error-count))))

(defgeneric check-symbol (symbol category))

(defmethod check-symbol (symbol (category (eql :concept)))
  t)

(defmethod check-symbol (symbol (category (eql :option)))
  t)

(defmethod check-symbol (symbol (category (eql :function)))
  (fboundp symbol))

(defmethod check-symbol (symbol (category (eql :macro)))
  (macro-function symbol))

(defmethod check-symbol (symbol (category (eql :variable)))
  #+sbcl (eq (sb-cltl2:variable-information symbol) :special)
  #-sbcl t)

(defmethod check-symbol (symbol (category (eql :constant)))
  (boundp symbol))

(defmethod check-symbol (symbol (category (eql :condition)))
  (subtypep symbol 'condition))

(defmethod check-symbol (symbol (category (eql :error)))
  (subtypep symbol 'error))

(defmethod check-symbol (symbol (category (eql :generic-function)))
  (and (fboundp symbol)
       (typep (symbol-function symbol) 'generic-function)))

(defmethod check-symbol (symbol (category (eql :predicate)))
  (check-symbol symbol :generic-function))

(defmethod check-symbol (symbol (category (eql :class)))
  (find-class symbol nil))

(defmethod check-symbol (symbol (category (eql :protocol-class)))
  (check-symbol symbol :class))

(defmethod check-symbol (symbol (category (eql :command-table)))
  (find-command-table symbol :errorp nil))

(defmethod check-symbol (symbol (category (eql :presentation-type)))
  (clim:find-presentation-type-class symbol nil))

(defmethod check-symbol (symbol (category (eql :presentation-type-abbrev)))
  (gethash symbol climi::*presentation-type-abbreviations*))

(defmethod check-symbol (symbol (category (eql :presentation-method)))
  (gethash symbol climi::*presentation-gf-table*))

(defmethod check-symbol (symbol (category (eql :presentation-type-option)))
  (alexandria:maphash-keys
   (lambda (key)
     (when (member symbol (clim:presentation-type-options key)
                   :key #'alexandria:ensure-car)
       (return-from check-symbol t)))
   climi::*presentation-type-table*)
  nil)

(defmethod check-symbol (symbol (category (eql :presentation-type-parameter)))
  (alexandria:maphash-keys
   (lambda (key)
     (when (member symbol (clim:presentation-type-parameters key)
                   :key #'alexandria:ensure-car)
       (return-from check-symbol t)))
   climi::*presentation-type-table*)
  nil)

(defmethod check-symbol (symbol (category (eql :frame)))
  (and (find-class symbol nil)
       (subtypep (find-class symbol nil) 'clim:application-frame)))

(defmethod check-symbol (symbol (category (eql :callback)))
  t)

(defmethod check-symbol (symbol (category (eql :pane)))
  t)

(defmethod check-symbol (symbol (category (eql :type)))
  t)
