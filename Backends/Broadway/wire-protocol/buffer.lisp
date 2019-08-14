(cl:in-package #:clim-broadway)

(defconstant +block-size+        32)

(defconstant +transparent-pixel+ #x00)
(defconstant +delta-0-run+       #x10)
(defconstant +block-reference+   #x20)
(defconstant +color-run+         #x30)
(defconstant +delta-run+         #x40)

(defun encode-pixel (buffer index r g b a)
  (let ((index (1- index)))
    (setf (aref buffer (incf index)) b
          (aref buffer (incf index)) g
          (aref buffer (incf index)) r
          (aref buffer (incf index)) a)

    (1+ index)))

(defun block-reference (buffer index block-id x y)
  (let ((index (1- index)))
    (setf (aref buffer (incf index)) (ldb (byte 8 0) block-id)
          (aref buffer (incf index)) (ldb (byte 8 8) block-id)
          (aref buffer (incf index)) (logior +block-reference+ (ldb (byte 4 16) block-id))
          (aref buffer (incf index)) 0

          (aref buffer (incf index)) (ldb (byte 8 0) y)
          (aref buffer (incf index)) (ldb (byte 8 8) y)
          (aref buffer (incf index)) (ldb (byte 8 0) x)
          (aref buffer (incf index)) (ldb (byte 8 8) x))

    (1+ index)))

(defun color-run (buffer index r g b alpha length)
  (let ((index (1- index)))
    (setf (aref buffer (incf index)) (ldb (byte 8 0) length))
    (setf (aref buffer (incf index)) (ldb (byte 8 8) length))
    (setf (aref buffer (incf index)) (logior +color-run+ (ldb (byte 4 16) length)))
    (setf (aref buffer (incf index)) 0)

    (setf (aref buffer (incf index)) b)
    (setf (aref buffer (incf index)) g)
    (setf (aref buffer (incf index)) r)
    (setf (aref buffer (incf index)) alpha)
    (1+ index)))

(defun delta-run (buffer index r g b alpha length)
  (let ((index (1- index)))
    (setf (aref buffer (incf index)) (ldb (byte 8 0) length))
    (setf (aref buffer (incf index)) (ldb (byte 8 8) length))
    (setf (aref buffer (incf index)) (logior +delta-run+ (ldb (byte 4 16) length)))
    (setf (aref buffer (incf index)) 0)

    (setf (aref buffer (incf index)) b)
    (setf (aref buffer (incf index)) g)
    (setf (aref buffer (incf index)) r)
    (setf (aref buffer (incf index)) alpha)
    (1+ index)))

(defun encode-buffer ()
  (let ((buffer (nibbles:make-octet-vector 24))
        (index  0))
    (setf index (color-run buffer index (random 255) (random 255) (random 255) 255 20000))
    (setf index (color-run buffer index (random 255) (random 255) (random 255) 255 20000))
    ;; (setf index (delta-run buffer index 1            1            1            0   20000))

    (setf index (block-reference buffer index 0 20 20))

    buffer))
