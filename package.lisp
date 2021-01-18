;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2001 Arnaud Rouanet
;;;  (c) copyright 2001-2003 Mike McDonald
;;;  (c) copyright 2001-2009 Gilbert Baumann <gbaumann@common-lisp.net>
;;;  (c) copyright 2002-2006 Timothy Moore <tmoore@common-lisp.net>
;;;  (c) copyright 2002 Brian Spilsbury
;;;  (c) copyright 2002 Alexey Dejneka
;;;  (c) copyright 2004-2008 Andy Hefner <ahefner@common-lisp.net>
;;;  (c) copyright 2006-2007 David Lichteblau <dlichteblau@common-lisp.net>
;;;  (c) copyright 2006-2008 Troels Henriksen <thenriksen@common-lisp.net>
;;;  (c) copyright 2016-2020 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2020,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Package definitions for McCLIM.

(in-package #:common-lisp-user)

;;; The CLIM-LISP package
;;;
;;; Our CLIM-LISP also contains gray streams, as I consider them part
;;; of Common Lisp.
;;;
;;; If you want to patch a CL symbol, you define it in CLIM-LISP-PATCH
;;; and export it.
#.(let ((all-ansi-symbols
         '(#:&allow-other-keys #:&aux #:&body #:&environment #:&key #:&optional #:&rest #:&whole #:*
           #:** #:*** #:*break-on-signals* #:*compile-file-pathname* #:*compile-file-truename*
           #:*compile-print* #:*compile-verbose* #:*debug-io* #:*debugger-hook*
           #:*default-pathname-defaults* #:*error-output* #:*features* #:*gensym-counter*
           #:*load-pathname* #:*load-print* #:*load-truename* #:*load-verbose* #:*macroexpand-hook*
           #:*modules* #:*package* #:*print-array* #:*print-base* #:*print-case* #:*print-circle*
           #:*print-escape* #:*print-gensym* #:*print-length* #:*print-level* #:*print-lines*
           #:*print-miser-width* #:*print-pprint-dispatch* #:*print-pretty* #:*print-radix*
           #:*print-readably* #:*print-right-margin* #:*query-io* #:*random-state* #:*read-base*
           #:*read-default-float-format* #:*read-eval* #:*read-suppress* #:*readtable*
           #:*standard-input* #:*standard-output* #:*terminal-io* #:*trace-output* #:+ #:++ #:+++ #:-
           #:/ #:// #:/// #:/= #:1+ #:1- #:< #:<= #:= #:> #:>= #:abort #:abs #:acons #:acos #:acosh
           #:add-method #:adjoin #:adjust-array #:adjustable-array-p #:allocate-instance
           #:alpha-char-p #:alphanumericp #:and #:append #:apply #:apropos #:apropos-list #:aref
           #:arithmetic-error #:arithmetic-error-operands #:arithmetic-error-operation #:array
           #:array-dimension #:array-dimension-limit #:array-dimensions #:array-displacement
           #:array-element-type #:array-has-fill-pointer-p #:array-in-bounds-p #:array-rank
           #:array-rank-limit #:array-row-major-index #:array-total-size #:array-total-size-limit
           #:arrayp #:ash #:asin #:asinh #:assert #:assoc #:assoc-if #:assoc-if-not #:atan #:atanh
           #:atom #:base-char #:base-string #:bignum #:bit #:bit-and #:bit-andc1 #:bit-andc2
           #:bit-eqv #:bit-ior #:bit-nand #:bit-nor #:bit-not #:bit-orc1 #:bit-orc2 #:bit-vector
           #:bit-vector-p #:bit-xor #:block #:boole #:boole-1 #:boole-2 #:boole-and #:boole-andc1
           #:boole-andc2 #:boole-c1 #:boole-c2 #:boole-clr #:boole-eqv #:boole-ior #:boole-nand
           #:boole-nor #:boole-orc1 #:boole-orc2 #:boole-set #:boole-xor #:boolean #:both-case-p
           #:boundp #:break #:broadcast-stream #:broadcast-stream-streams #:built-in-class #:butlast
           #:byte #:byte-position #:byte-size #:caaaar #:caaadr #:caaar #:caadar #:caaddr #:caadr
           #:caar #:cadaar #:cadadr #:cadar #:caddar #:cadddr #:caddr #:cadr #:call-arguments-limit
           #:call-method #:call-next-method #:car #:case #:catch #:ccase #:cdaaar #:cdaadr #:cdaar
           #:cdadar #:cdaddr #:cdadr #:cdar #:cddaar #:cddadr #:cddar #:cdddar #:cddddr #:cdddr
           #:cddr #:cdr #:ceiling #:cell-error #:cell-error-name #:cerror #:change-class #:char
           #:char-code #:char-code-limit #:char-downcase #:char-equal #:char-greaterp #:char-int
           #:char-lessp #:char-name #:char-not-equal #:char-not-greaterp #:char-not-lessp
           #:char-upcase #:char/= #:char< #:char<= #:char= #:char> #:char>= #:character #:characterp
           #:check-type #:cis #:class #:class-name #:class-of #:clear-input #:clear-output #:close
           #:clrhash #:code-char #:coerce #:compilation-speed #:compile #:compile-file
           #:compile-file-pathname #:compiled-function #:compiled-function-p #:compiler-macro
           #:compiler-macro-function #:complement #:complex #:complexp #:compute-applicable-methods
           #:compute-restarts #:concatenate #:concatenated-stream #:concatenated-stream-streams
           #:cond #:condition #:conjugate #:cons #:consp #:constantly #:constantp #:continue
           #:control-error #:copy-alist #:copy-list #:copy-pprint-dispatch #:copy-readtable
           #:copy-seq #:copy-structure #:copy-symbol #:copy-tree #:cos #:cosh #:count #:count-if
           #:count-if-not #:ctypecase #:debug #:decf #:declaim #:declaration #:declare #:decode-float
           #:decode-universal-time #:defclass #:defconstant #:defgeneric #:define-compiler-macro
           #:define-condition #:define-method-combination #:define-modify-macro
           #:define-setf-expander #:define-symbol-macro #:defmacro #:defmethod #:defpackage
           #:defparameter #:defsetf #:defstruct #:deftype #:defun #:defvar #:delete
           #:delete-duplicates #:delete-file #:delete-if #:delete-if-not #:delete-package
           #:denominator #:deposit-field #:describe #:describe-object #:destructuring-bind
           #:digit-char #:digit-char-p #:directory #:directory-namestring #:disassemble
           #:division-by-zero #:do #:do* #:do-all-symbols #:do-external-symbols #:do-symbols
           #:documentation #:dolist #:dotimes #:double-float #:double-float-epsilon
           #:double-float-negative-epsilon #:dpb #:dribble #:dynamic-extent #:ecase #:echo-stream
           #:echo-stream-input-stream #:echo-stream-output-stream #:ed #:eighth #:elt
           #:encode-universal-time #:end-of-file #:endp #:enough-namestring
           #:ensure-directories-exist #:ensure-generic-function #:eq #:eql #:equal #:equalp #:error
           #:etypecase #:eval #:eval-when #:evenp #:every #:exp #:export #:expt #:extended-char
           #:fboundp #:fceiling #:fdefinition #:ffloor #:fifth #:file-author #:file-error
           #:file-error-pathname #:file-length #:file-namestring #:file-position #:file-stream
           #:file-string-length #:file-write-date #:fill #:fill-pointer #:find #:find-all-symbols
           #:find-class #:find-if #:find-if-not #:find-method #:find-package #:find-restart
           #:find-symbol #:finish-output #:first #:fixnum #:flet #:float #:float-digits
           #:float-precision #:float-radix #:float-sign #:floating-point-inexact
           #:floating-point-invalid-operation #:floating-point-overflow #:floating-point-underflow
           #:floatp #:floor #:fmakunbound #:force-output #:format #:formatter #:fourth #:fresh-line
           #:fround #:ftruncate #:ftype #:funcall #:function #:function-keywords
           #:function-lambda-expression #:functionp #:gcd #:generic-function #:gensym #:gentemp #:get
           #:get-decoded-time #:get-dispatch-macro-character #:get-internal-real-time
           #:get-internal-run-time #:get-macro-character #:get-output-stream-string #:get-properties
           #:get-setf-expansion #:get-universal-time #:getf #:gethash #:go #:graphic-char-p
           #:handler-bind #:handler-case #:hash-table #:hash-table-count #:hash-table-p
           #:hash-table-rehash-size #:hash-table-rehash-threshold #:hash-table-size #:hash-table-test
           #:host-namestring #:identity #:if #:ignorable #:ignore #:ignore-errors #:imagpart #:import
           #:in-package #:incf #:initialize-instance #:inline #:input-stream-p #:inspect #:integer
           #:integer-decode-float #:integer-length #:integerp #:interactive-stream-p #:intern
           #:internal-time-units-per-second #:intersection #:invalid-method-error #:invoke-debugger
           #:invoke-restart #:invoke-restart-interactively #:isqrt #:keyword #:keywordp #:labels
           #:lambda #:lambda-list-keywords #:lambda-parameters-limit #:last #:lcm #:ldb #:ldb-test
           #:ldiff #:least-negative-double-float #:least-negative-long-float
           #:least-negative-normalized-double-float #:least-negative-normalized-long-float
           #:least-negative-normalized-short-float #:least-negative-normalized-single-float
           #:least-negative-short-float #:least-negative-single-float #:least-positive-double-float
           #:least-positive-long-float #:least-positive-normalized-double-float
           #:least-positive-normalized-long-float #:least-positive-normalized-short-float
           #:least-positive-normalized-single-float #:least-positive-short-float
           #:least-positive-single-float #:length #:let #:let* #:lisp-implementation-type
           #:lisp-implementation-version #:list #:list* #:list-all-packages #:list-length #:listen
           #:listp #:load #:load-logical-pathname-translations #:load-time-value #:locally #:log
           #:logand #:logandc1 #:logandc2 #:logbitp #:logcount #:logeqv #:logical-pathname
           #:logical-pathname-translations #:logior #:lognand #:lognor #:lognot #:logorc1 #:logorc2
           #:logtest #:logxor #:long-float #:long-float-epsilon #:long-float-negative-epsilon
           #:long-site-name #:loop #:loop-finish #:lower-case-p #:machine-instance #:machine-type
           #:machine-version #:macro-function #:macroexpand #:macroexpand-1 #:macrolet #:make-array
           #:make-broadcast-stream #:make-concatenated-stream #:make-condition
           #:make-dispatch-macro-character #:make-echo-stream #:make-hash-table #:make-instance
           #:make-instances-obsolete #:make-list #:make-load-form #:make-load-form-saving-slots
           #:make-method #:make-package #:make-pathname #:make-random-state #:make-sequence
           #:make-string #:make-string-input-stream #:make-string-output-stream #:make-symbol
           #:make-synonym-stream #:make-two-way-stream #:makunbound #:map #:map-into #:mapc #:mapcan
           #:mapcar #:mapcon #:maphash #:mapl #:maplist #:mask-field #:max #:member #:member-if
           #:member-if-not #:merge #:merge-pathnames #:method #:method-combination
           #:method-combination-error #:method-qualifiers #:min #:minusp #:mismatch #:mod
           #:most-negative-double-float #:most-negative-fixnum #:most-negative-long-float
           #:most-negative-short-float #:most-negative-single-float #:most-positive-double-float
           #:most-positive-fixnum #:most-positive-long-float #:most-positive-short-float
           #:most-positive-single-float #:muffle-warning #:multiple-value-bind #:multiple-value-call
           #:multiple-value-list #:multiple-value-prog1 #:multiple-value-setq #:multiple-values-limit
           #:name-char #:namestring #:nbutlast #:nconc #:next-method-p #:nil #:nintersection #:ninth
           #:no-applicable-method #:no-next-method #:not #:notany #:notevery #:notinline #:nreconc
           #:nreverse #:nset-difference #:nset-exclusive-or #:nstring-capitalize #:nstring-downcase
           #:nstring-upcase #:nsublis #:nsubst #:nsubst-if #:nsubst-if-not #:nsubstitute
           #:nsubstitute-if #:nsubstitute-if-not #:nth #:nth-value #:nthcdr #:null #:number #:numberp
           #:numerator #:nunion #:oddp #:open #:open-stream-p #:optimize #:or #:otherwise
           #:output-stream-p #:package #:package-error #:package-error-package #:package-name
           #:package-nicknames #:package-shadowing-symbols #:package-use-list #:package-used-by-list
           #:packagep #:pairlis #:parse-error #:parse-integer #:parse-namestring #:pathname
           #:pathname-device #:pathname-directory #:pathname-host #:pathname-match-p #:pathname-name
           #:pathname-type #:pathname-version #:pathnamep #:peek-char #:phase #:pi #:plusp #:pop
           #:position #:position-if #:position-if-not #:pprint #:pprint-dispatch
           #:pprint-exit-if-list-exhausted #:pprint-fill #:pprint-indent #:pprint-linear
           #:pprint-logical-block #:pprint-newline #:pprint-pop #:pprint-tab #:pprint-tabular #:prin1
           #:prin1-to-string #:princ #:princ-to-string #:print #:print-not-readable
           #:print-not-readable-object #:print-object #:print-unreadable-object #:probe-file
           #:proclaim #:prog #:prog* #:prog1 #:prog2 #:progn #:program-error #:progv #:provide
           #:psetf #:psetq #:push #:pushnew #:quote #:random #:random-state #:random-state-p #:rassoc
           #:rassoc-if #:rassoc-if-not #:ratio #:rational #:rationalize #:rationalp #:read
           #:read-byte #:read-char #:read-char-no-hang #:read-delimited-list #:read-from-string
           #:read-line #:read-preserving-whitespace #:read-sequence #:reader-error #:readtable
           #:readtable-case #:readtablep #:real #:realp #:realpart #:reduce #:reinitialize-instance
           #:rem #:remf #:remhash #:remove #:remove-duplicates #:remove-if #:remove-if-not
           #:remove-method #:remprop #:rename-file #:rename-package #:replace #:require #:rest
           #:restart #:restart-bind #:restart-case #:restart-name #:return #:return-from #:revappend
           #:reverse #:room #:rotatef #:round #:row-major-aref #:rplaca #:rplacd #:safety #:satisfies
           #:sbit #:scale-float #:schar #:search #:second #:sequence #:serious-condition #:set
           #:set-difference #:set-dispatch-macro-character #:set-exclusive-or #:set-macro-character
           #:set-pprint-dispatch #:set-syntax-from-char #:setf #:setq #:seventh #:shadow
           #:shadowing-import #:shared-initialize #:shiftf #:short-float #:short-float-epsilon
           #:short-float-negative-epsilon #:short-site-name #:signal #:signed-byte #:signum
           #:simple-array #:simple-base-string #:simple-bit-vector #:simple-bit-vector-p
           #:simple-condition #:simple-condition-format-arguments #:simple-condition-format-control
           #:simple-error #:simple-string #:simple-string-p #:simple-type-error #:simple-vector
           #:simple-vector-p #:simple-warning #:sin #:single-float #:single-float-epsilon
           #:single-float-negative-epsilon #:sinh #:sixth #:sleep #:slot-boundp #:slot-exists-p
           #:slot-makunbound #:slot-missing #:slot-unbound #:slot-value #:software-type
           #:software-version #:some #:sort #:space #:special #:special-operator-p #:speed #:sqrt
           #:stable-sort #:standard #:standard-char #:standard-char-p #:standard-class
           #:standard-generic-function #:standard-method #:standard-object #:step #:storage-condition
           #:store-value #:stream #:stream-element-type #:stream-error #:stream-error-stream
           #:stream-external-format #:streamp #:string #:string-capitalize #:string-downcase
           #:string-equal #:string-greaterp #:string-left-trim #:string-lessp #:string-not-equal
           #:string-not-greaterp #:string-not-lessp #:string-right-trim #:string-stream #:string-trim
           #:string-upcase #:string/= #:string< #:string<= #:string= #:string> #:string>= #:stringp
           #:structure #:structure-class #:structure-object #:style-warning #:sublis #:subseq
           #:subsetp #:subst #:subst-if #:subst-if-not #:substitute #:substitute-if
           #:substitute-if-not #:subtypep #:svref #:sxhash #:symbol #:symbol-function
           #:symbol-macrolet #:symbol-name #:symbol-package #:symbol-plist #:symbol-value #:symbolp
           #:synonym-stream #:synonym-stream-symbol #:t #:tagbody #:tailp #:tan #:tanh #:tenth
           #:terpri #:the #:third #:throw #:time #:trace #:translate-logical-pathname
           #:translate-pathname #:tree-equal #:truename #:truncate #:two-way-stream
           #:two-way-stream-input-stream #:two-way-stream-output-stream #:type #:type-error
           #:type-error-datum #:type-error-expected-type #:type-of #:typecase #:typep #:unbound-slot
           #:unbound-slot-instance #:unbound-variable #:undefined-function #:unexport #:unintern
           #:union #:unless #:unread-char #:unsigned-byte #:untrace #:unuse-package #:unwind-protect
           #:update-instance-for-different-class #:update-instance-for-redefined-class
           #:upgraded-array-element-type #:upgraded-complex-part-type #:upper-case-p #:use-package
           #:use-value #:user-homedir-pathname #:values #:values-list #:variable #:vector
           #:vector-pop #:vector-push #:vector-push-extend #:vectorp #:warn #:warning #:when
           #:wild-pathname-p #:with-accessors #:with-compilation-unit #:with-condition-restarts
           #:with-hash-table-iterator #:with-input-from-string #:with-open-file #:with-open-stream
           #:with-output-to-string #:with-package-iterator #:with-simple-restart #:with-slots
           #:with-standard-io-syntax #:write #:write-byte #:write-char #:write-line #:write-sequence
           #:write-string #:write-to-string #:y-or-n-p #:yes-or-no-p #:zerop))
        ;; XXX: we could use `closer-common-lisp', but some of McCLIM
        ;; MOP code is not conformant it seems (we have problems with
        ;; unknown `:default' argument dropping us in the debugger).
        ;(packages '(:closer-common-lisp))
        (packages '(:common-lisp))
        (gray-symbols
         '(#:fundamental-stream
           #:fundamental-input-stream
           #:fundamental-output-stream
           #:fundamental-character-stream
           #:fundamental-binary-stream
           #:fundamental-character-input-stream
           #:fundamental-character-output-stream
           #:fundamental-binary-input-stream
           #:fundamental-binary-output-stream
           #:stream-read-char
           #:stream-unread-char
           #:stream-read-char-no-hang
           #:stream-peek-char
           #:stream-listen
           #:stream-read-line
           #:stream-clear-input
           #:stream-write-char
           #:stream-line-column
           #:stream-start-line-p
           #:stream-write-string
           #:stream-terpri
           #:stream-fresh-line
           #:stream-finish-output
           #:stream-force-output
           #:stream-advance-to-column
           #:stream-clear-output
           #:stream-read-byte
           #:stream-write-byte))
        (gray-packages '(#:trivial-gray-streams)))
    ;;
    (labels ((seek-symbol (name packages)
               ;; seek the a symbol named 'name' in `packages'
               (or (some (lambda (p)
                           (multiple-value-bind (sym res)
                               (find-symbol (symbol-name name) p)
                             (if (eql res :external)
                                 (list sym)
                                 nil)))
                         packages)
                   (progn (format t "~&there is no ~A." name)
                          (force-output)
                          nil)))
             (dump-defpackage (&aux imports export-ansi export-gray)
               (labels ((push-import-from (symbol package)
                          (let ((pair (assoc package imports)))
                            (if pair
                                (push symbol (cdr pair))
                                (push `(,package . (,symbol))
                                      imports))))
                        (grok (symbols packages)
                          (let ((res nil))
                            (dolist (nam symbols)
                              (let ((sym (seek-symbol nam packages)))
                                (when sym
                                  (push (car sym) res)
                                  (cond
                                    ((and (find-package '#:clim-lisp-patch)
                                          (multiple-value-bind (sym2 res)
                                              (find-symbol (symbol-name nam) '#:clim-lisp-patch)
                                            (and sym2 (eq res :external))))
                                     ;;
                                     (format t "~&;; ~S is patched." sym)
                                     (force-output)
                                     (push-import-from nam '#:clim-lisp-patch))
                                    (t
                                     (setf sym (car sym))
                                     ;; clisp has no (:import ..) arg!
                                     (push-import-from
                                      (symbol-name sym)
                                      (package-name (symbol-package sym))))))))
                            res)))
                                  ;;
                 ;; Don't redefine a perfectly working CL:DESCRIBE,
                 ;; which more often than not has special knowledge
                 ;; about objects you can't possibly gain though some
                 ;; portable implementation.
                 ;; --GB 2004-11-20
                 (setf all-ansi-symbols (remove '#:describe all-ansi-symbols
                                                :test #'string-equal)
                       all-ansi-symbols (remove '#:describe-object all-ansi-symbols
                                                :test #'string-equal))
                 ;;
                 (setf export-ansi (grok all-ansi-symbols packages)
                       export-gray (grok gray-symbols gray-packages))
                 `(progn
                    (defpackage #:clim-lisp
                      (:use)
                      ,@(mapcar (lambda (spec)
                                  (destructuring-bind (package . syms) spec
                                    `(:import-from ,package ,@syms)))
                                imports)
                      (:shadow #:describe #:describe-object)
                      (:export #:describe #:describe-object)
                      (:export
                       ,@(mapcar #'symbol-name export-ansi)
                       ,@(mapcar #'symbol-name export-gray)))))))
      (dump-defpackage)))

;;; The CLIM package
(defpackage #:clim
  (:use)

  ;; Export color names. The contents of data/colors.sexp is
  ;; automatically generated from X11 color definitions (usually
  ;; /usr/share/X11/rgb.txt).
  (:export
   . #.(let* ((file-pathname (or *compile-file-pathname*
                                 *load-pathname*))
              (colors-pathname (merge-pathnames "data/colors.sexp"
                                                file-pathname)))
         (with-open-file (stream colors-pathname :direction :input)
           (loop for (symbol-name) = (read stream nil nil)
                 while symbol-name
                 collect (make-symbol symbol-name)))))

  (:import-from #:clim-lisp
   #:and
   #:boolean
   #:character
   #:complex
   #:float
   #:integer
   #:keyword
   #:member
   #:nil
   #:null
   #:number
   #:or
   #:pathname
   #:ratio
   #:rational
   #:real
   #:sequence
   #:string
   #:symbol
   #:t)

  ;; symbols, which were exported as of 2002-02-09, but no longer are.

  ;; dispatch-repaint:
  ;; several mentions in silica.tex.

  ;; labelled

  ;; labelled-gadget, though there is a labelled-gadget-mixin
  ;; mute-repainting-mixin, though there is a sheet-mute-repainting-mixin
  ;; oriented-gadget, though there is a oriented-gadget-mixin

  ;; pointer-button-click-event
  ;; this is mentioned in silica.tex. spelling error?

  ;; bordering
  ;; border-pane
  ;; this is mentioned in LW CLIM documentation

  ;; spacer-pane
  ;; this is mentioned in our own documentation (!) but absent

  ;; absolutely no mention of the following in the spec:

  ;; display-cursor
  ;; draw-triangle
  ;; draw-triangle*
  ;; gadget-label-text-style
  ;; pointer-button-click-and-hold-event
  ;; pointer-button-double-click-event
  ;; pointer-port
  ;; push-button-show-as-default-p

  . #.(let* ((file-pathname (or *compile-file-pathname*
                                *load-pathname*))
             (data-pathname (merge-pathnames
                             #P"data/clim-symbols.sexp"
                             file-pathname))
             (data          (alexandria:with-input-from-file (stream data-pathname)
                              (read stream))))
        `((:import-from #:clim-lisp
           ,@(loop :with seen = (make-hash-table :test #'eq)
                   :for (name kind references) :in data
                   :when (and (not (member kind '(:option :concept)))
                              (not (gethash name seen))
                              (or (find "D" references :test #'equal :key #'first)
                                  (string= name "interactive-stream-p"))
                              (not (member name '("stream-pathname" "stream-truename")
                                           :test #'string=)))
                   :do (setf (gethash name seen) t)
                   :and :collect (make-symbol (string-upcase name))))
          (:export
           ,@(loop :with seen = (make-hash-table :test #'eq)
                   :for (name kind) :in data
                   :unless (or (member kind '(:option :concept))
                               (gethash name seen))
                   :do (setf (gethash name seen) t)
                   :and :collect (make-symbol (string-upcase name)))))))

(defpackage #:clim-sys
  (:use)

  (:export
   . #.(let* ((file-pathname (or *compile-file-pathname*
                                 *load-pathname*))
              (data-pathname (merge-pathnames
                              #P"data/clim-sys-symbols.sexp"
                              file-pathname))
              (data          (alexandria:with-input-from-file (stream data-pathname)
                               (read stream))))
         (loop :with seen = (make-hash-table :test #'eq)
               :for (name kind) :in data
               :unless (or (member kind '(:option :concept))
                           (gethash name seen))
               :do (setf (gethash name seen) t)
               :and :collect (make-symbol (string-upcase name))))))

(defpackage #:clim-extensions
  (:use)
  (:nicknames #:clime)
  (:export
   ;; events
   #:event-read-with-timeout
   #:event-listen-or-wait
   #:schedule-event
   #:window-manager-focus-event
   ;; frames
   #:frame-icon
   ;; sheets
   #:top-level-sheet-mixin
   #:unmanaged-sheet-mixin
   #:sheet-name
   #:sheet-pretty-name
   #:sheet-icon

   #:always-repaint-background-mixin
   #:never-repaint-background-mixin
   #:background
   #:foreground
   #:line-style-effective-thickness
   #:line-style-effective-dashes
   ;; medium
   #:medium-miter-limit
   #:medium-draw-circle*
   ;; panes
   #:raised-pane #:raising
   #:lowered-pane #:lowering
   #:viewport-pane
   #:device-font-text-style-p
   #:draw-image
   #:image-pane
   #:draw-label
   #:label-pane-label
   #:box-adjuster-gadget
   #:compose-space-aux
   #:simple-event-loop
   #:invoke-with-output-to-pointer-documentation
   #:with-output-to-pointer-documentation
   #:frame-display-pointer-documentation-string
   #:list-pane-items
   #:output-record-baseline
   #:merging-dead-keys

   #:draw-output-border-over
   #:draw-output-border-under
   #:make-bordered-output-record
   #:bordered-output-record

   #:draw-rounded-rectangle*

   #:highlight-output-record-tree
   #:text-selection-mixin
   #:mouse-wheel-scroll-mixin
   ;; page abstraction (seos mixin)
   #:stream-cursor-initial-position
   #:stream-cursor-final-position
   #:stream-page-region
   #:stream-text-margins
   #:stream-line-width
   #:with-temporary-margins
   #:invoke-with-temporary-page
   ;; designs and patterns
   #:pattern
   #:image-pattern
   #:rectangular-tile
   #:transformed-design
   #:transformed-pattern
   #:effective-transformed-design
   #:rectangular-tile-design
   ;; readers
   #:pattern-array
   #:transformed-design-design
   #:transformed-design-transformation
   ;; inks
   #:indirect-ink
   #:indirect-ink-p
   #:indirect-ink-ink
   #:color-rgba
   #:design-ink
   ;; Font listing extension:
   #:font-family
   #:font-face
   #:port-all-font-families
   #:font-family-name
   #:font-family-port
   #:font-family-all-faces
   #:font-face-name
   #:font-face-family
   #:font-face-all-sizes
   #:font-face-scalable-p
   #:font-face-text-style

   #:define-bitmap-file-reader
   #:unsupported-bitmap-format
   #:bitmap-format
   #:*default-vertical-scroll-bar-position*
   ;; ports and frame managers
   #:note-input-focus-changed
   #:find-frame-type
   #:note-frame-pretty-name-changed
   #:note-frame-icon-changed
   ;; images
   #:rgb-image
   #:xpm-parse-file
   #:*xpm-x11-colors*
   ;; selection
   #:define-selection-translator
   #:release-selection
   #:publish-selection
   #:request-selection
   ;; franz
   #:pointer-place-rubber-band-line*
   #:pointer-input-rectangle*
   #:pointer-input-rectangle))

;;; Symbols that must be defined by a backend.
;;;
;;; To start with, I grabbed the methods defined by the CLX backend
;;; whose symbol package is CLIM or CLIMI.
(defpackage #:clim-backend
  (:nicknames #:climb)
  (:use #:clim #:clim-extensions)
  (:export
   ;; CLIM-INTERNALS
   #:find-port-type
   #:make-graft
   #:medium-draw-circle*
   #:mirror-transformation
   #:port-allocate-pixmap
   #:port-deallocate-pixmap
   #:port-disable-sheet
   #:port-enable-sheet
   #:port-force-output
   #:port-grab-pointer
   #:port-ungrab-pointer
   #:with-pointer-grabbed
   #:port-set-mirror-name
   #:port-set-mirror-icon
   #:port-set-mirror-region
   #:port-set-mirror-transformation
   #:queue-callback
   #:set-sheet-pointer-cursor
   #:synthesize-pointer-motion-event
   #:window-manager-focus-event
   #:with-port
   #:invoke-with-port
   #:find-concrete-pane-class
   ;; Text-style
   #:text-style-character-width
   #:text-bounding-rectangle*
   #:normalize-font-size
   #:parse-text-style*
   ;; Font abstraction
   #:font-face
   #:font-size
   #:font-character-width
   #:font-string-width
   #:font-string-glyph-codes
   #:font-glyph-code-char
   #:font-text-extents
   #:font-ascent
   #:font-descent
   #:font-leading
   #:font-tracking
   #:font-fixed-width
   #:font-kerning-p
   #:font-glyph-width
   #:font-glyph-height
   #:font-glyph-top
   #:font-glyph-left
   #:font-glyph-bottom
   #:font-glyph-right
   #:font-glyph-dx
   #:font-glyph-dy
   ;; Mixins available for backends
   #:multiline-text-medium-mixin
   #:font-rendering-medium-mixin
   #:approx-bbox-medium-mixin
   #:transform-coordinates-mixin
   ;; From CLIM (mentioned in the spec)
   #:adopt-frame
   #:allocate-space
   #:destroy-mirror
   #:destroy-port
   #:graft
   #:graftp
   #:graft-height
   #:graft-width
   #:handle-repaint
   #:make-medium
   #:make-pane-1
   #:medium-beep
   #:medium-buffering-output-p
   #:medium-clear-area
   #:medium-clipping-region
   #:medium-copy-area
   #:medium-draw-ellipse*
   #:medium-draw-line*
   #:medium-draw-lines*
   #:medium-draw-point*
   #:medium-draw-points*
   #:medium-draw-polygon*
   #:medium-draw-rectangle*
   #:medium-draw-rectangles*
   #:medium-draw-text*
   #:medium-finish-output
   #:medium-force-output
   #:medium-line-style
   #:medium-text-style
   #:note-space-requirements-changed
   #:pointer-button-state
   #:pointer-modifier-state
   #:pointer-position
   #:realize-mirror
   #:text-size
   #:text-style-ascent
   #:text-style-descent
   #:text-style-height
   #:text-style-mapping
   #:text-style-width
   ;; CLIM-EXTENSIONS
   #:medium-miter-limit
   #:medium-draw-circle*
   ;; selection
   #:release-selection
   #:publish-selection
   #:request-selection
   #:selection-object
   #:selection-object-content
   #:selection-object-type
   #:selection-object-owner
   ;; command output destinations
   #:invoke-with-standard-output
   #:register-output-destination-type
   #:output-destination
   #:stream-destination
   #:destination-stream
   #:file-destination
   #:destination-file
   #:destination-element-type))

(defpackage #:clim-internals
  (:use #:clim #:clim-sys #:clim-extensions #:clim-backend #:clim-lisp)
  (:nicknames #:climi)
  #+excl
  (:import-from :excl compile-system load-system)
  (:import-from #:alexandria
   #:clamp
   #:make-keyword
   #:ensure-gethash
   #:last-elt
   #:with-gensyms
   #:if-let
   #:when-let
   #:when-let*)
  (:intern #:letf))

(defpackage #:clim-user
  (:use #:clim #:clim-lisp))
