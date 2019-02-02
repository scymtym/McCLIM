;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-
;;;
;;;  (c) copyright 2001 by Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;
;;; See toplevel file 'Copyright' for the copyright details.
;;;

(in-package :clim-internals)

(defun get-environment-variable (string)
  #+excl (sys:getenv string)
  #+(or cmu scl) (cdr (assoc string ext:*environment-list* :test #'string=))
  #+clisp (ext:getenv (string string))
  #+sbcl (sb-ext::posix-getenv string)
  #+openmcl (ccl::getenv string)
  #+lispworks (lw:environment-variable string)
  #+ecl (ext:getenv string)
  #+clasp (ext:getenv string)
  #-(or ecl excl cmu scl clisp sbcl openmcl lispworks clasp)
  (error "GET-ENVIRONMENT-VARIABLE not implemented"))

;;; It would be nice to define this macro in terms of letf, but that
;;; would change the top-levelness of the enclosed forms.

#+excl
(defmacro with-system-redefinition-allowed (&body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (excl:package-definition-lock (find-package :common-lisp)) nil))
     ,@body
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (excl:package-definition-lock (find-package :common-lisp)) t))))

#+clisp
(defmacro with-system-redefinition-allowed (&body body)
  `(ext:without-package-lock ("COMMON-LISP")
     ,@body))

#+openmcl
(defmacro with-system-redefinition-allowed (&body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setq ccl::*warn-if-redefine-kernel* nil))
     ,@body
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setq ccl::*warn-if-redefine-kernel* t))))

#+cmu
(eval-when (:compile-toplevel :execute)
  (when (find-symbol "PACKAGE-LOCK" :ext)
    (pushnew 'clim-internals::package-locks *features*)))

#+(and cmu clim-internals::package-locks)
(eval-when (:load-toplevel)
  (unless (find-symbol "PACKAGE-LOCK" :ext)
    (error "Binary incompatibility: your CMUCL does not have package locks")))

#+cmu
(defmacro with-system-redefinition-allowed (&body body)
  #+clim-internals::package-locks
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (ext:package-definition-lock (find-package :common-lisp)) nil))
     ,@body
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (ext:package-definition-lock (find-package :common-lisp)) t)))
  #-clim-internals::package-locks
  `(progn ,@body))

#+sbcl
(eval-when (:compile-toplevel :execute)
  (when (find-symbol "UNLOCK-PACKAGE" :sb-ext)
    (pushnew 'clim-internals::package-locks *features*)))

#+sbcl
(defmacro with-system-redefinition-allowed (&body body)
  #+clim-internals::package-locks
  `(progn
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (sb-ext:unlock-package :common-lisp))
    ,@body
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (sb-ext:lock-package :common-lisp)))
  #-clim-internals::package-locks
  `(progn
     ,@body))

#+ecl
(defmacro with-system-redefinition-allowed (&body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (ext:package-lock (find-package :common-lisp) nil))
     ,@body
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (ext:package-lock (find-package :common-lisp) t))))

#-(or ecl excl openmcl cmu sbcl clisp)
(defmacro with-system-redefinition-allowed (&body body)
  `(progn
     ,@body))

(defun 2+ (x)
  (+ x 2))

(defun 2- (x)
  (- x 2))

(defun unlerp (v a b)
  "Inverse linear interpolate (lerp).

Given an interpolated value V and two extreme values A and B, return
the blending factor. More precisely, return c such that

  V = (1 - c) A + c B.

When A = B, return 0.5."
  (if (= a b)
      0.5
      (/ (- v a) (- b a))))


(defun check-letf-form (form)
  (assert (and (listp form)
               (= 2 (length form)))))

(defun valueify (list)
  (if (and (consp list)
           (endp (rest list)))
      (first list)
      `(values ,@list)))

(defmacro letf ((&rest forms) &body body &environment env)
  "LETF ({(Place Value)}*) Declaration* Form* During evaluation of the
Forms, SETF the Places to the result of evaluating the Value forms.
The places are SETF-ed in parallel after all of the Values are
evaluated."
  (mapc #'check-letf-form forms)
  (let* (init-let-form save-old-values-setf-form
         new-values-set-form old-values-set-form
         update-form)
    (loop for (place new-value) in forms
	  for (vars vals store-vars writer-form reader-form)
	    = (multiple-value-list (get-setf-expansion place env))
	  for old-value-names = (mapcar (lambda (var)
					  (declare (ignore var))
					  (gensym))
					store-vars)
	  nconc (mapcar #'list vars vals)
	    into temp-init-let-form
	  nconc (copy-list store-vars)
	    into temp-init-let-form
	  nconc (copy-list old-value-names)
	    into temp-init-let-form
	  nconc `(,(valueify old-value-names) ,reader-form)
	    into temp-save-old-values-setf-form
	  nconc `(,(valueify store-vars) ,new-value)
	    into temp-new-values-set-form
	  nconc `(,(valueify store-vars) ,(valueify old-value-names))
	    into temp-old-values-set-form
	  collect writer-form
	    into temp-update-form
	  finally (setq init-let-form temp-init-let-form
			save-old-values-setf-form temp-save-old-values-setf-form
			new-values-set-form temp-new-values-set-form
			old-values-set-form temp-old-values-set-form
			update-form (cons 'progn temp-update-form)))
    `(let* ,init-let-form
       (setf ,@save-old-values-setf-form)
       (unwind-protect
            (progn (setf ,@new-values-set-form)
                   ,update-form
                   (progn ,@body))
         (setf ,@old-values-set-form)
         ,update-form))))

(defun map-repeated-sequence (result-type n function sequence)
  "Like CL:MAP, but applies \\arg{function} to \\arg{n} consecutive
elements of \\arg{sequence}. All the function's return values will be
gathered into the output sequence. \\arg{result-type} can also be NIL,
in which case the function is only applied for effect.

Examples:

 (map-repeated-sequence 'list 2 #'list '(1 2 3 4 5 6)) => ((1 2) (3 4) (5 6))
 (map-repeated-sequence 'list 2 #'+ '(1 2 3 4 5 6)) => (3 7 11)
 (map-repeated-sequence 'vector 3 #'+ '(1 2 3 4 5 6)) => #(6 15)

 (map-repeated-sequence 'list 2 #'floor '(2 1 4 3 6 5))
 => (2 0 1 1 1 1)

 (map-repeated-sequence 'list 2 #'cons '(color red weight 17 name fred))
 => ((COLOR . RED) (WEIGHT . 17) (NAME . FRED))

 (map-repeated-sequence 'list 1 #'(lambda (p) (values (car p) (cdr p)))
                        '((color . red) (weight . 17) (name . fred)))
 => (COLOR RED WEIGHT 17 NAME FRED)

Note:
 Be careful, since this function is quite sensible to the number of values
 returned by \\arg{function}.
"
  (assert (>= n 1))
  (cond ((eq result-type 'nil)
         ;; just map for effect
         (cond ((vectorp sequence)
                (loop for i from 0 below (length sequence) by n
		      do (apply function
				(loop for j from 0 below n
				      collect (aref sequence (+ i j))))))
               ((listp sequence)
                (let ((q sequence))
                  (loop until (null q)
			do (apply function
				  (loop for j from 0 below n
					collect (pop q))))))))
        (t
         ;; Otherwise, we (for now) take the easy route of calling
         ;; COERCE.
         (coerce
          (cond ((vectorp sequence)
                 (loop for i from 0 below (length sequence) by n
                       nconc (multiple-value-list
			      (apply function
				     (loop for j from 0 below n
					   collect (aref sequence (+ i j)))))))
                ((listp sequence)
                 (let ((q sequence))
                   (loop until (null q)
			 nconc (multiple-value-list
				(apply function
				       (loop for j from 0 below n
					     collect (pop q))))))))
          result-type))))

;;; A different way of attacking iteration of sequences
(defmacro do-sequence ((vars sequence &optional result-form) &body body)
  "Iterate over SEQUENCE.  VARS is a list of symbols (or a single
symbol).  At each iteration the variables in VARS are bound to the
initial elements of the sequence.  The iteration is then \"stepped\"
by the number of variables in VARS."
  (flet ((list-accessor (n)
	   (case n
	     (0 'car)
	     (1 'cadr)
	     (2 'caddr)
	     (3 'cadddr)
	     (t `(lambda (list) (nth ,n list)))))
	 (list-stepper (n)
	   (case n
	     (1 'cdr)
	     (2 'cddr)
	     (3 'cdddr)
	     (4 'cddddr)
	     (t `(lambda (list) (nthcdr ,n list))))))
    (when (not (listp vars))
      (setq vars (list vars)))
    (let* ((body-fun (gensym "BODY-FUN"))
	   (var-length (length vars))
	   (seq-var (gensym "SEQ-VAR"))
	   (tail-var (gensym "TAIL-VAR"))
	   (i (gensym "I"))
	   (list-args (loop for j from 0 below var-length
			    collect `(,(list-accessor j) ,tail-var)))
	   (vector-args (loop for j from 0 below var-length
			      collect `(aref ,seq-var (+ ,i ,j)))))
      `(block nil
	 (flet ((,body-fun ,vars
		  (tagbody
		     ,@body)))
	   (let ((,seq-var ,sequence))
	     (etypecase ,seq-var
	       (list
		(loop for ,tail-var on ,seq-var by #',(list-stepper var-length)
		      do (,body-fun ,@list-args)))
	       (vector
		(loop for ,i of-type fixnum from 0 below (length ,seq-var) by ,var-length
		      do (,body-fun ,@vector-args))))))
	 ,@(when result-form
	     `((let ,vars		;Bind variables to nil
		 (declare (ignorable ,@vars))
		 ,result-form)))))))

;;;;
;;;; meta functions
;;;;

;; these are as in Dylan

(defun curry (fun &rest args)
  #'(lambda (&rest more)
      (apply fun (append args more))))

(define-compiler-macro curry (fun &rest args)
  `(lambda (&rest more)
     (apply ,fun ,@args more)))

(defun always (x)
  #'(lambda (&rest more)
      (declare (ignore more))
      x))

(define-compiler-macro always (x)
  (let ((g (gensym)))
    `(let ((,g ,x))
       (lambda (&rest more)
         (declare (ignore more))
         ,g))))

;;; Convenience macros

(define-modify-macro maxf (&rest args) max)
(define-modify-macro minf (&rest args) min)
(define-modify-macro nconcf (&rest args) nconc)
(define-modify-macro orf (&rest args) or)
(define-modify-macro clampf (min max) clamp)

;;; Move this early so it can be used in presentations.lisp, which
;;; comes before commands.lisp.

(defmacro do-command-table-inheritance ((command-table-var command-table)
					&body body)
  `(apply-with-command-table-inheritance
    #'(lambda (,command-table-var)
	,@body)
    (find-command-table ,command-table)))


(defun parse-method (description)
  (loop
     for (qualifier-or-ll . body) on description
     until (listp qualifier-or-ll)
     collect qualifier-or-ll into qualifiers
     finally (return
               (values qualifiers
                       (c2mop:extract-specializer-names qualifier-or-ll)
                       (c2mop:extract-lambda-list qualifier-or-ll)
                       body))))

(defun get-body-declarations (body)
  "Collect all declaration forms from a body of forms that may have
 declarations at its top. Returns as values a list of the declarations and the
 rest of the body."
  (loop for bod on body
	for (form) = bod
	if (and (consp form) (eq (car form) 'declare))
	  collect form into decls
	else
	  return (values decls bod)
	finally	(return (values decls nil)))) ;It's all (declare ...)

(defun decode-specializer (specializer-name)
  (if (atom specializer-name)
      (find-class specializer-name)
      (c2mop:intern-eql-specializer (second specializer-name))))

(defmacro with-method ((name &rest description) &body body)
  "Executes BODY installing the specified method on the generic
  function named NAME."
  (multiple-value-bind (qualifiers specializers)
      (parse-method description)
    (with-gensyms (old-method decoded-specializers new-method)
      `(let* ((,decoded-specializers
               (mapcar #'decode-specializer ',specializers))
              (,old-method (find-method #',name
                                        ',qualifiers
                                        ,decoded-specializers
                                        nil))
              (,new-method
               (defmethod ,name ,@description)))
         (unwind-protect
              (locally ,@body)
           (remove-method #',name ,new-method)
           (when ,old-method (add-method #',name ,old-method)))))))

;;; Anaphoric

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro aand (&rest args)
  (cond ((endp args) t)
        ((endp (rest args)) (first args))
        (t `(aif ,(first args) (aand ,@(rest args))))))

;;;
(declaim (inline maybe-funcall maybe-apply))

(defun maybe-funcall (function &rest args)
  "If FUNCTION is not NIL, funcall it."
  (when function (apply function args)))

(defun maybe-apply (function &rest args)
  "If FUNCTION is not NIL, apply it."
  (when function (apply #'apply function args)))

;;; Remove keyword pairs from an argument list, consing as little as
;;; possible.
(defun remove-keywords (arg-list keywords)
  (let ((clean-tail arg-list))
    ;; First, determine a tail in which there are no keywords to be removed.
    (loop for arg-tail on arg-list by #'cddr
	  for (key) = arg-tail
	  do (when (member key keywords :test #'eq)
	       (setq clean-tail (cddr arg-tail))))
    ;; Cons up the new arg list until we hit the clean-tail, then nconc that on
    ;; the end.
    (loop for arg-tail on arg-list by #'cddr
	  for (key value) = arg-tail
	  if (eq arg-tail clean-tail)
	    nconc clean-tail
	    and do (loop-finish)
	  else if (not (member key keywords :test #'eq))
		 nconc (list key value)
	  end)))

(defmacro with-keywords-removed ((var keywords &optional (new-var var))
				 &body body)
  "binds NEW-VAR (defaults to VAR) to VAR with the keyword arguments specified
in KEYWORDS removed."
  `(let ((,new-var (remove-keywords ,var ',keywords)))
     ,@body))

(defun symbol-concat (&rest symbols)
  "Actually this function raises the next question: what is *PACKAGE* supposed to be?
   The correct answer: listen to the elders and don't use this function or any variant
   of it -- Don't construct symbols, instead let the user specify them."
  (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))

(defun stream-designator-symbol (symbol default)
  "Maps T to DEFAULT, barfs if argument does not look good.
   To be used in the various WITH-... macros."
  (cond ((eq symbol 't)
         default)
        ((symbolp symbol)
         symbol)
        (t
         (error "~S Can not be a stream designator for ~S" symbol default))))

(defun declare-ignorable-form (variables)
  #+CMU
  ;; CMUCL barfs if you declare a special variable ignorable, work
  ;; around that.
  `(declare (ignorable
             ,@(remove-if (lambda (symbol)
                            (eq :special (lisp::info lisp::variable lisp::kind symbol)))
                          variables)))
  #-CMU
  `(declare (ignorable ,@variables)))

;; spread version:

(defun declare-ignorable-form* (&rest variables)
  (declare-ignorable-form variables))

(defun gen-invoke-trampoline (fun to-bind to-pass body)
  "Macro helper function, generates the LABELS / INVOKE-WITH-... ideom."
  (let ((cont (gensym ".CONT.")))
    `(labels ((,cont (,@to-bind)
               ,(declare-ignorable-form to-bind)
               ,@body))
      (declare (dynamic-extent #',cont))
      (,fun ,@to-bind #',cont ,@to-pass))))

;;;; ----------------------------------------------------------------------

(defun parse-space (stream specification direction)
  "Returns the amount of space given by SPECIFICATION relating to the
STREAM in the direction DIRECTION."
  ;; This implementation lives unter the assumption that an
  ;; extended-output stream is also a sheet and has a graft.
  ;; --GB 2002-08-14
  (etypecase specification
    (real specification)
    ((or string character) (multiple-value-bind (width height)
                               (text-size stream (string specification))
                             (ecase direction
                               (:horizontal width)
                               (:vertical height))))
    #+nil ; WITH-OUTPUT-TO-OUTPUT-RECORD not yet defined as a macro
    (function (let ((record (with-output-to-output-record (stream)
                              (funcall specification))))
                (ecase direction
                  (:horizontal (bounding-rectangle-width record))
                  (:vertical (bounding-rectangle-height record)))))
    (cons
     (destructuring-bind (value unit)
         specification
       (ecase unit
         (:character
          (* value (stream-character-width stream #\M)))
         (:line
          (* value (stream-line-height stream)))
         ((:point :pixel :mm)
          (let* ((graft (graft stream))
                 (gunit (graft-units graft)))
            ;; mungle specification into what grafts talk about
            (case unit
              ((:point)  (setf value (/ value 72) unit :inches))
              ((:pixel)  (setf unit :device))
              ((:mm)     (setf unit :millimeters)))
            ;;
            (multiple-value-bind (dx dy)
                (multiple-value-call
                    #'transform-distance
                  (compose-transformation-with-scaling
                   (sheet-delta-transformation stream graft)
                   (/ (graft-width graft :units unit)
                      (graft-width graft :units gunit))
                   (/ (graft-height graft :units unit)
                      (graft-height graft :units gunit)))
                  (ecase direction
                    (:horizontal (values 1 0))
                    (:vertical   (values 0 1))))
              (/ value (sqrt (+ (* dx dx) (* dy dy))))))))))))

(defun delete-1 (item list &key (test #'eql) (key #'identity))
  "Delete 1 ITEM from LIST. Second value is T if item was deleted."
  (loop
     for tail on list
       and tail-prev = nil then tail
     for (list-item) = tail
     if (funcall test item (funcall key list-item))
       do (return-from delete-1
	    (if tail-prev
		(progn
		  (setf (cdr tail-prev) (cdr tail))
		  (values list t))
		(values (cdr tail) t)))
     finally (return (values list nil))))

(defun rebind-arguments (arg-list)
  "Create temporary variables for non keywords in a list of
  arguments. Returns two values: a binding list for let, and a new
  argument list with the temporaries substituted in."
  (loop
     for arg in arg-list
     for var = (gensym)
     if (keywordp arg)
       collect arg into new-arg-list
     else
       collect `(,var ,arg) into bindings
       and collect var into new-arg-list
     end
     finally (return (values bindings new-arg-list))))

(defun bisect (start end predicate-fn &optional predicament-fn)
  "Finds the rightmost index meeting the PREDICATE-FN between START and END. It
is assumed that START always meets the predicate while END may but doesn't have
to meet it. That means that function always return some index.

PREDICATE-FN INDEX
Should return NIL if index does not meet the predicate and something else
otherwise.

PREDICAMENT-FN INDEX-1 INDEX-2
Returns next index between its arguments for test.  If there is nothing more to
test must return NIL. When not supplied default function looks always for an
index being halfway between INDEX-1 and INDEX-2."
  (when (funcall predicate-fn end)
    (return-from bisect end))
  (unless predicament-fn
    (setf predicament-fn (lambda (last-good last-bad)
                           (let ((predicament (floor (+ last-good last-bad) 2)))
                             (and (/= predicament last-good)
                                  (/= predicament last-bad)
                                  predicament)))))
  (loop
     with last-good = start
     with last-bad = end
     as current-guess = (funcall predicament-fn last-good last-bad)
     until (null current-guess)
     do (if (funcall predicate-fn current-guess)
            (setf last-good current-guess)
            (setf last-bad current-guess))
     finally (return last-good)))

;; Implementing line breaking as defined in Unicode[1] is left as an excercise
;; for the reader. [1] https://unicode.org/reports/tr14/ -- jd 2019-01-08
(defun line-break-opportunities (string start end &optional (break-characters '(#\space)))
  "Returns a sequence of string indexes where line can break."
  (loop
     with space-indexes = (make-array 1 :fill-pointer 0 :adjustable t :element-type 'fixnum)
     for i from start below (1- end)
     do (when (member (aref string i) break-characters :test #'char=)
          (vector-push-extend i space-indexes))
     finally
       (vector-push-extend (1- end) space-indexes)
       (return space-indexes)))

;;; Command name utilities that are useful elsewhere.

(defun command-name-from-symbol (symbol)
  (let ((name (symbol-name symbol)))
    (string-capitalize
     (substitute
      #\Space #\-
      (subseq name (if (string= '#:com- name :end2 (min (length name) 4))
		       4
		       0))))))

(defun keyword-arg-name-from-symbol (symbol)
  (let ((name (symbol-name symbol)))
    (string-capitalize (substitute #\Space #\- name))))

;;; taken from https://stackoverflow.com/questions/11067899/is-there-a-generic-method-for-cloning-clos-objects#11068536, use with care (should work for "ordinary" classes though).
(defun shallow-copy-object (original)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'c2mop:slot-definition-name (c2mop:class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))

;;; String utilities

(defmacro with-string-subseq ((string start end emptyp &key subseq)
                              &body body)
  "Establish a binding for EMPTYP and new bindings for STRING, START, END.
   STRING is bound to a string representation of the original value
   which may be a string or character. START and END are bound to
   bounding indices based on the original and the length of
   STRING. EMPTYP is bound to a Boolean indicating whether the
   designated subsequence is empty."
  (check-type string symbol)
  (check-type start symbol)
  (check-type end symbol)
  (check-type emptyp symbol)
  `(multiple-value-bind (,string ,start ,end ,@(when emptyp `(,emptyp)))
       (etypecase ,string
         (character
          (values (string ,string) 0 1 nil))
         (string
          (let ((end (or ,end (length ,string))))
            (values ,(if subseq
                         `(subseq ,string ,start end)
                         string)
                    ,start end (= ,start end)))))
     (declare (ignore ,@(when (eq subseq :only) `(,start))
                      ,@(when (eq subseq :only) `(,end))))
     ,@body))

(defmacro dolines ((line string &optional result) &body body)
  "Iterates over lines in string separated by #\newline."
  (alexandria:with-gensyms (substr end)
    (alexandria:once-only (string)
      `(do* ((,substr ,string (subseq ,substr (1+ ,end)))
             (,end  #1=(position #\newline ,substr) #1#)
             (,line #2=(subseq ,substr 0 ,end) #2#))
            ((null ,end) ,@body ,result)
         ,@body))))

;;;; The Collect macro:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun collect-normal-expander (n-value fun forms)
    `(progn
       ,@(mapcar #'(lambda (form) `(setq ,n-value (,fun ,form ,n-value))) forms)
       ,n-value))

  (defun collect-list-expander (n-value n-tail forms)
    (let ((n-res (gensym)))
      `(progn
         ,@(mapcar #'(lambda (form)
                       `(let ((,n-res (cons ,form nil)))
                          (cond (,n-tail
                                 (setf (cdr ,n-tail) ,n-res)
                                 (setq ,n-tail ,n-res))
                                (t
                                 (setq ,n-tail ,n-res  ,n-value ,n-res)))))
                   forms)
         ,n-value))))

(defmacro collect (collections &body body)
  (let (macros binds)
    (dolist (spec collections)
      (cond ((atom spec)
             (setf spec (list spec)))
            ((not (<= 1 (length spec) 3))
             (error "Malformed collection specifier: ~S." spec)))
      (let ((n-value (gensym))
            (name (first spec))
            (default (second spec))
            (kind (or (third spec) 'collect)))
        (push `(,n-value ,default) binds)
        (if (eq kind 'collect)
            (let ((n-tail (gensym)))
              (if default
                  (push `(,n-tail (last ,n-value)) binds)
                  (push n-tail binds))
              (push `(,name (&rest args)
                            (collect-list-expander ',n-value ',n-tail args))
                    macros))
            (push `(,name (&rest args)
                          (collect-normal-expander ',n-value ',kind args))
                  macros))))
    `(macrolet ,macros (let* ,(nreverse binds) ,@body))))

(defun coord-seq->point-seq (sequence)
  (collect (collect-point)
    (do-sequence ((x y) sequence (collect-point))
      (collect-point (make-point x y)))))

(defun remove-duplicated-points (point-sequence &optional closed)
  "Given points A B C ... Z removes consecutive points which are duplicated. If
a flag CLOSED is T then beginning and end of the list are consecutive too."
  (collect (collect-point)
    (let* ((first-point (elt point-sequence 0))
           (last-point first-point))
      (collect-point first-point)
      (mapc (lambda (current-point)
              (unless (region-equal current-point last-point)
                (setf last-point current-point)
                (collect-point last-point)))
            point-sequence)
      (if (and closed
               (region-equal first-point last-point)
               (null (alexandria:length= 1 (collect-point))))
          (butlast (collect-point))
          (collect-point)))))

;;
;; pretty printing
;;
;; This is a simplified version of the approach used by David
;; Lichteblau for pretty-printing objects in cxml-stp/node.lip
(defgeneric slots-for-pprint-object (node)
  (:documentation "A generic function that returns the slot names of
  objects to be pretty-printed by simple-pprint-object. Providers of
  print-object methods that intend to use simple-pprint-object should
  provide their own methods that return the appropriate slot-names.")
  (:method-combination append)
  (:method append ((object t)) nil))

;; print :slot-name slot-value for each slot
(defgeneric simple-pprint-object-args (stream object)
  (:method (stream object)
    (let ((slots (mapcan (lambda (slot)
                           (let ((value (slot-value object slot)))
                             (list (list slot value))))
                         (slots-for-pprint-object object))))
      (loop for (slot-name slot-value) in slots
         do
           (write-char #\Space stream)
           (pprint-newline :fill stream)
           (write-char #\: stream)
           (princ slot-name stream)
           (write-char #\Space stream)
           (unless (atom slot-value)
             (princ "'" stream))
           (write slot-value :stream stream)))))

(defgeneric simple-pprint-object (stream object)
  (:method (stream object)
    (pprint-logical-block (stream (list object) :prefix "#.(" :suffix ")")
      (write (intern "MAKE-INSTANCE" *package*) :stream stream)
      (write-char #\Space stream)
      (write-char #\' stream)
      (write (class-name (class-of object)) :stream stream)
      (simple-pprint-object-args stream object))))

(defmacro maybe-print-readably ((self sink) &body body)
  `(cond
     ((and *print-readably* (not *read-eval*))
      (error "cannot readably print object of type ~A when not *read-eval*." (type-of ,self)))
     ((and *print-pretty* *print-readably*)
      (simple-pprint-object ,sink ,self))
     (t ,@body)))
