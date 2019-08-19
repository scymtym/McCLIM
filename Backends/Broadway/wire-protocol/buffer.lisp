(cl:in-package #:clim-broadway)

;;;

(declaim (inline +/32 -/32 */32))
(defun +/32 (x y)
  (logand (+ x y) #xffffffff))

(defun -/32 (x y)
  (logand (- x y) #xffffffff))

(defun */32 (x y)
  (logand (* x y) #xffffffff))

(define-modify-macro incf/32 (delta) +/32)
(define-modify-macro decf/32 (delta) -/32)

;;;

(cl:defconstant +block-size+ 32)

(macrolet ((define-primes (name1 name2 value)
             `(progn
               (cl:defconstant ,name1 ,value)

               (cl:defconstant ,name2
                 (logand (expt ,name1 +block-size+) #xffffffff)))))

  (define-primes +prime+ +end-prime+ #x1f821e2d)

  (define-primes +vprime+ +end-vprime+ #x0137b89d))

(deftype hash-value ()
  '(unsigned-byte 32))

;;;

(deftype argb-pixel ()
  '(unsigned-byte 32))

(deftype argb-pixel-array ()
  '(simple-array argb-pixel 2))

;;;

(cl:defconstant +chunk-size+ 1024
  )

(deftype chunk-index ()
  `(integer 0 ,+chunk-size+))

(deftype chunk-array ()
  `(nibbles:simple-octet-vector ,+chunk-size+))

(declaim (inline make-chunk))
(defun make-chunk ()
  (nibbles:make-octet-vector +chunk-size+))

(defstruct (encoder (:constructor make-encoder))
  (buffer      (make-chunk)      :type nibbles:simple-octet-vector)
  (index       0                 :type array-index)
  (used-chunks (make-array 0 :adjustable t :fill-pointer 0) :type (array t 1))
  (free-chunks (make-array 0 :adjustable t :fill-pointer 0) :type (array t 1))
  (color-value 0                 :type argb-pixel)
  (color-run   0                 :type (unsigned-byte 32))
  (delta-value 0                 :type argb-pixel)
  (delta-run   0               :type (unsigned-byte 32))
  (stats       (vector 0 0 0)))

(defun reset-encoder (encoder)
  ;; Reset Buffer, index and chunks
  (let ((free-chunks (encoder-free-chunks encoder))
        (used-chunks (encoder-used-chunks encoder)))
    (setf (encoder-index encoder) 0)
    (dotimes (i (length used-chunks))
      (vector-push-extend (vector-pop used-chunks) free-chunks)))
  ;; Reset runs
  (setf (encoder-color-value encoder) 0
        (encoder-color-run encoder)   0
        (encoder-delta-value encoder) 0
        (encoder-delta-run encoder)   0))

(defun flush-chunk (encoder)
  (vector-push-extend (encoder-buffer encoder) (encoder-used-chunks encoder))
  (let* ((free-chunks (encoder-free-chunks encoder))
         (new-chunk   (if (plusp (fill-pointer free-chunks))
                          (vector-pop free-chunks)
                          (make-chunk))))
    (setf (encoder-index encoder)  0
          (encoder-buffer encoder) new-chunk)))

(defmacro encoding ((encoder required-space push-byte-name
                     &optional (buffer-var (gensym (string '#:buffer)))
                               (index-var  (gensym (string '#:index))))
                    &body body)
  (once-only (encoder)
    (with-unique-names (free fitsp old-buffer)
      `(let* ((,index-var  (encoder-index ,encoder))
              (,free       (- +chunk-size+ ,index-var))
              (,fitsp      (<= ,required-space ,free))
              (,old-buffer (encoder-buffer ,encoder))
              (,buffer-var (cond (,fitsp
                                  ,old-buffer)
                                 (t
                                  (setf ,index-var 0)
                                  (flush-chunk ,encoder)))))
         (declare (cl:type chunk-index ,index-var)
                  (cl:type chunk-array ,old-buffer ,buffer-var))
         (macrolet ((,push-byte-name (value)
                      `(prog1
                           (locally
                               #+sbcl (declare
                                       (optimize (sb-c::insert-array-bounds-checks 0)))
                               (setf (aref ,',buffer-var ,',index-var) ,value))
                         (incf ,',index-var))))
           ,@body)
         (when (not ,fitsp)
           (setf (subseq ,old-buffer (- +chunk-size+ ,free))
                 (subseq ,buffer-var 0 ,free))
           (setf (subseq ,buffer-var 0 (- ,index-var ,free))
                 (subseq ,buffer-var ,free ,index-var))
           (decf ,index-var ,free))
         (setf (encoder-index ,encoder) ,index-var)))))

;;;

(cl:defconstant +transparent-pixel+ #x00)
(cl:defconstant +delta-0-run+       #x1)
(cl:defconstant +block-reference+   #x2)
(cl:defconstant +color-run+         #x3)
(cl:defconstant +delta-run+         #x4)

(cl:defconstant +run-length-limit+ #xffff) ; TODO why not 24 bit?

(defun pixel (encoder r g b a) ; TDOO unused
  (let ((buffer (encoder-buffer encoder))
        (index  (1- (encoder-index encoder))))
    (setf (aref buffer (incf index)) b
          (aref buffer (incf index)) g
          (aref buffer (incf index)) r
          (aref buffer (incf index)) a)

    (setf (encoder-index encoder) (1+ index))))

(defun block-reference (encoder block-id x y)
  (declare (cl:type encoder encoder)
           (cl:type (unsigned-byte 32) block-id)
           (cl:type (unsigned-byte 16) x y)
           (optimize speed))
  (encoding (encoder 8 push-byte)
    (push-byte (ldb (byte 8 0) block-id))
    (push-byte (ldb (byte 8 8) block-id))
    (push-byte (logior +block-reference+ (ldb (byte 4 16) block-id)))
    (push-byte 0)

    (push-byte (ldb (byte 8 0) y))
    (push-byte (ldb (byte 8 8) y))
    (push-byte (ldb (byte 8 0) x))
    (push-byte (ldb (byte 8 8) x)))
  #+old (let ((buffer (encoder-buffer encoder))
        (index  (1- (encoder-index encoder))))
    (setf (aref buffer (incf index)) (ldb (byte 8 0) block-id)
          (aref buffer (incf index)) (ldb (byte 8 8) block-id)
          (aref buffer (incf index)) (logior +block-reference+ (ldb (byte 4 16) block-id))
          (aref buffer (incf index)) 0

          (aref buffer (incf index)) (ldb (byte 8 0) y)
          (aref buffer (incf index)) (ldb (byte 8 8) y)
          (aref buffer (incf index)) (ldb (byte 8 0) x)
          (aref buffer (incf index)) (ldb (byte 8 8) x))

    (setf (encoder-index encoder) (1+ index))))

(macrolet ((define-run-encoder (name code valuep &optional can-skip-length-p)
             `(defun ,name (encoder ,@(when valuep `(value)) length)
                (declare (cl:type (unsigned-byte 32) length)
                         (optimize speed))
                (encoding (encoder
                           (+ ,(if can-skip-length-p
                                   `(if (> length 1) 4 0)
                                   4)
                              ,(if valuep 4 0))
                           push-byte
                           buffer index)
                  (when ,(if can-skip-length-p
                             `(> length 1)
                             t)
                    (setf (nibbles:ub32ref/le buffer index)
                          (logior ,(ash (symbol-value code) 20) length))
                    (incf index 4)
                    #+no (progn
                           (push-byte (ldb (byte 8 0) length))
                          (push-byte (ldb (byte 8 8) length))
                          (push-byte (logior ,code (ldb (byte 4 16) length)))
                          (push-byte 0)))

                  ,@(when valuep
                      (if can-skip-length-p ; actually means "color-run"
                          `((setf (nibbles:ub32ref/be buffer index)
                                  (logior #x000000ff
                                          (ash (ldb (byte 8  8) value) 24)
                                          (ash (ldb (byte 8 16) value) 16)
                                          (ash (ldb (byte 8 24) value)  8)))
                            (incf index 4))
                          `((setf (nibbles:ub32ref/le buffer index) value)
                            (incf index 4)))
                      #+no `((push-byte (ldb (byte 8  8) value))
                        (push-byte (ldb (byte 8 16) value))
                        (push-byte (ldb (byte 8 24) value))
                        (push-byte 255 ; (ldb (byte 8 24) value)
                                   )))))))
  (define-run-encoder color-run          +color-run+   t   t)
  (define-run-encoder delta-non-zero-run +delta-run+   t)
  (define-run-encoder delta-zero-run     +delta-0-run+ nil))

(declaim (inline delta-run))
(defun delta-run (encoder delta length)
  (declare (cl:type argb-pixel delta))
  (if (zerop delta)
      (delta-zero-run encoder length)
      (delta-non-zero-run encoder delta length)))

(defparameter *sync* nil)

(macrolet ((reset-run (value encoder-value encoder-run)
             `(setf (,encoder-value encoder) ,value
                    (,encoder-run encoder) 1))
           (update-run (value encoder-value encoder-run value-writer run-writer)
             `(cond ((= ,value ,encoder-value)
                     (setf (,run-writer encoder) (+ ,encoder-run 1)))
                    (t
                     (reset-run ,value ,value-writer ,run-writer))))
           (field (shift)
             (let ((mask (ash #xff shift)))
               `(logand (- (logand color ,mask)
                           (logand old-color ,mask))
                        ,mask))))

  (declaim (inline encode-pixel))
  (defun encode-pixel (encoder color old-color)
    (declare (cl:type encoder    encoder)
             (cl:type argb-pixel color old-color)
             (optimize (speed 3) (safety 0) (debug 0)))
    (let* ((delta            (cond ((= color old-color)
                                    0)
                                   ((= 0 old-color)
                                    color)
                                   (t
                                    (logior (field 0) (field 8) (field 16) (field 24))
                                    )))
           (color-value      (encoder-color-value encoder))
           (color-run        (encoder-color-run encoder))
           (delta-value      (encoder-delta-value encoder))
           (delta-run        (encoder-delta-run encoder))
           (color-mismatch-p (/= color color-value))
           (delta-mismatch-p (/= delta delta-value)))
      (cond
        ((or (and t #+no *sync* (= color-run 1)) (and color-mismatch-p (> color-run delta-run))
             (= color-run +run-length-limit+))
                                        ; (incf (aref (encoder-stats encoder) 1))
         (color-run encoder color-value color-run)
         (reset-run color encoder-color-value encoder-color-run)
         (reset-run delta encoder-delta-value encoder-delta-run))
        ((or (and delta-mismatch-p (> delta-run color-run))
             (= delta-run +run-length-limit+))
                                        ; (incf (aref (encoder-stats encoder) 2))
         (delta-run encoder delta-value delta-run)
         (reset-run color encoder-color-value encoder-color-run)
         (reset-run delta encoder-delta-value encoder-delta-run)) ; TODO make sure run length 1 is encoded as color-run
        ((and color-mismatch-p delta-mismatch-p (plusp delta-run))
         (delta-run encoder delta-value delta-run)
         #+no (if (= color-run 1)
                  (incf (aref (encoder-stats encoder) 0))
                  (incf (aref (encoder-stats encoder) 2)))
                                        ; (incf (aref (encoder-stats encoder) 2))
                                        ; (color-run encoder color-value color-run)
         (reset-run color encoder-color-value encoder-color-run)
         (reset-run delta encoder-delta-value encoder-delta-run))
        (t
                                        ; (print :update)
         (update-run color color-value color-run encoder-color-value encoder-color-run)
         (update-run delta delta-value delta-run encoder-delta-value encoder-delta-run)))))

  (defun encoder-flush (encoder)
    (let ((color-run (encoder-color-run encoder))
          (delta-run (encoder-delta-run encoder)))
      (cond ((> color-run delta-run)
             (color-run encoder (encoder-color-value encoder) color-run))
            ((plusp delta-run)
             (delta-run encoder (encoder-delta-value encoder) delta-run))))))

;;;

(declaim (inline hash-update))
(defun hash-update (old-hash value1 value2)
  (+/32 (-/32 (*/32 old-hash +prime+)
              (*/32 value1 +end-prime+))
        (or value2 0)))

(defun encode-buffer (encoder pixels old-pixels width height) ; TODO re-use encoder
  (declare (cl:type encoder encoder)
           (cl:type argb-pixel-array pixels old-pixels)
           (cl:type array-index width height)
           (optimize speed (debug 0) (safety 0)))
  (reset-encoder encoder)
  (let ((hashes (make-array width :element-type 'hash-value)))
    (loop :for y :below height
          :for bottom-y :of-type array-index = (+ y +block-size+)
          :do ;; First +BLOCK-SIZE+ columns
             #+no (loop :for x :below +block-size+
                        :for hash = 0 :then (*/32 hash +prime+)
                        :for bottom-hash = 0
                        :do (when (< x width)
                              (incf/32 hash (aref pixels y x)))
                            (when (< bottom-y height)
                              (setf bottom-hash (*/32 bottom-hash +prime+))
                              (when (< x width)
                                (incf bottom-hash (aref pixels bottom-y x))))
                        )
             ;; All columns
             (loop :for x :below width
                   :for index :from (array-row-major-index pixels y 0)
                   :for right-x :of-type array-index = (+ x +block-size+)
                   ; :for hash = 0 :then (hash-update hash top-left-pixel top-right-pixel)
                                        ; :for bottom-hash = 0 :then (hash-update bottom-hash bottom-left-pixel bottom-right-pixel)
                   :for top-left-pixel = (row-major-aref pixels index) ; (aref pixels y x)
                   ;; :for top-right-pixel = (when (< right-x width)
                   ;;                          (aref pixels y right-x))
                                        ; :for bottom-left-pixel = (aref pixels bottom-y x)
                   ;; :for bottom-right-pixel = (when (< right-x width)
                   ;;                             (aref pixels bottom-y right-x))
                   :do #+no (cond (nil
                                   ;; (encode-pixel buffer index )
                                   )
                                  (nil
                                   ;; (block-reference buffer index block-id x y)
                                   ;; (encode-pixel buffer index 0 0 0 0)
                                   )
                                  (t
                                   (encode-pixel encoder top-left-pixel (aref old-pixels y x))))
                       #+no (setf (aref hashes x)
                                  (+/32 (*/32 (aref hashes x) +vprime+)
                                        (-/32 bottom-hash (*/32 hash +end-vprime+))))
                       (encode-pixel encoder top-left-pixel 0 #+no(row-major-aref old-pixels index) #+no (aref old-pixels y x)))) ; TODO avoid function call and encoder structure
    (encoder-flush encoder)
    ; (print (encoder-stats encoder))
    encoder))
