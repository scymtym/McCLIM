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

(defconstant +transparent-pixel+ #x00)
(defconstant +delta-0-run+       #x10)
(defconstant +block-reference+   #x20)
(defconstant +color-run+         #x30)
(defconstant +delta-run+         #x40)

(defconstant +run-length-limit+ #xffff) ; TODO why not 24 bit?

(defun pixel (encoder r g b a) ; TDOO unused
  (let ((buffer (encoder-buffer encoder))
        (index  (1- (encoder-index encoder))))
    (setf (aref buffer (incf index)) b
          (aref buffer (incf index)) g
          (aref buffer (incf index)) r
          (aref buffer (incf index)) a)

    (setf (encoder-index encoder) (1+ index))))

(defun block-reference (encoder block-id x y)
  (let ((buffer (encoder-buffer encoder))
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
                (let ((buffer (encoder-buffer encoder))
                      (index  (1- (encoder-index encoder))))
                  (when ,(if can-skip-length-p
                             `(> length 1)
                             t)
                    (setf (aref buffer (incf index)) (ldb (byte 8 0) length))
                    (setf (aref buffer (incf index)) (ldb (byte 8 8) length))
                    (setf (aref buffer (incf index)) (logior ,code (ldb (byte 4 16) length)))
                    (setf (aref buffer (incf index)) 0))

                  ,@(when valuep
                      `((setf (aref buffer (incf index)) (ldb (byte 8 24) value))
                        (setf (aref buffer (incf index)) (ldb (byte 8 16) value))
                        (setf (aref buffer (incf index)) (ldb (byte 8  8) value))
                        (setf (aref buffer (incf index)) 255 ; (ldb (byte 8 24) value)
                              )))

                  (setf (encoder-index encoder) (1+ index))))))
  (define-run-encoder color-run          +color-run+   t   t)
  (define-run-encoder delta-non-zero-run +delta-run+   t)
  (define-run-encoder delta-zero-run     +delta-0-run+ nil))

(defun delta-run (encoder delta length)
  (if (zerop delta)
      (delta-zero-run encoder length)
      (delta-non-zero-run encoder delta length)))

;;;

(cl:defconstant +chunk-size+ 1024)

(defstruct (encoder (:constructor make-encoder))
  (buffer      (nibbles:make-octet-vector 10000000) :type nibbles:simple-octet-vector)
  (index       0                                        :type array-index)
  (color-value 0                                        :type (unsigned-byte 32))
  (color-run   0                                        :type array-index)
  (delta-value 0                                        :type (unsigned-byte 32))
  (delta-run   0                                        :type array-index)
  (stats       (vector 0 0 0)))

(defparameter *sync* nil)

(macrolet ((reset-run (value encoder-value encoder-run)
             `(setf (,encoder-value encoder) ,value
                    (,encoder-run encoder) 1))
           (update-run (value encoder-value encoder-run)
             `(cond ((= ,value (,encoder-value encoder)) ; TODO we have this cache
                     (incf (,encoder-run encoder)))
                    (t
                     (reset-run ,value ,encoder-value ,encoder-run))))
           (field (shift)
             (let ((mask (ash #xff shift)))
               `(logand (- (logand color ,mask)
                           (logand old-color ,mask))
                        ,mask))))

  (defun encode-pixel (encoder color old-color)
    (let* ((delta            (cond ((= color old-color)
                                    0)
                                   ((= 0 old-color)
                                    color)
                                   (t
                                    (logior (field 0) (field 8) (field 16) (field 24)))))
           (color-value      (encoder-color-value encoder))
           (color-run        (encoder-color-run encoder))
           (delta-value      (encoder-delta-value encoder))
           (delta-run        (encoder-delta-run encoder))
           (color-mismatch-p (/= color color-value))
           (delta-mismatch-p (/= delta delta-value)))
      (cond
        ((or (and *sync* (= color-run 1)) (and color-mismatch-p (> color-run delta-run))
             (= color-run +run-length-limit+))
         (incf (aref (encoder-stats encoder) 1))
         (color-run encoder color-value color-run)
         (reset-run color encoder-color-value encoder-color-run)
         (reset-run delta encoder-delta-value encoder-delta-run))
        ((or (and delta-mismatch-p (> delta-run color-run))
             (= delta-run +run-length-limit+))
         (incf (aref (encoder-stats encoder) 2))
         (delta-run encoder delta-value delta-run)
         (reset-run color encoder-color-value encoder-color-run)
         (reset-run delta encoder-delta-value encoder-delta-run)) ; TODO make sure run length 1 is encoded as color-run
        ((and color-mismatch-p delta-mismatch-p (plusp delta-run))
         (delta-run encoder delta-value delta-run)
         #+no (if (= color-run 1)
                  (incf (aref (encoder-stats encoder) 0))
                  (incf (aref (encoder-stats encoder) 2)))
         (incf (aref (encoder-stats encoder) 2))
                                        ; (color-run encoder color-value color-run)
         (reset-run color encoder-color-value encoder-color-run)
         (reset-run delta encoder-delta-value encoder-delta-run))
        (t
                                        ; (print :update)
         (update-run color encoder-color-value encoder-color-run)
         (update-run delta encoder-delta-value encoder-delta-run)))))

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

(defun encode-buffer (pixels old-pixels width height)
  (declare (cl:type (simple-array (unsigned-byte 32) 2) pixels)
           (cl:type array-index width height)
           ; (optimize speed (debug 0) (safety 0))
           )
  (let ((encoder (make-encoder))
                                        ; (buffer (nibbles:make-octet-vector 24))
                                        ; (index  0)

        (hashes (make-array width :element-type 'hash-value)))
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
                   :for right-x = (+ x +block-size+)
                   :for hash = 0 :then (hash-update hash top-left-pixel top-right-pixel)
                                        ; :for bottom-hash = 0 :then (hash-update bottom-hash bottom-left-pixel bottom-right-pixel)
                   :for top-left-pixel = (aref pixels y x)
                   :for top-right-pixel = (when (< right-x width)
                                            (aref pixels y right-x))
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
                       (encode-pixel encoder top-left-pixel (aref old-pixels y x)
                                     )))
    (encoder-flush encoder)
    (print (encoder-stats encoder))
    encoder))
