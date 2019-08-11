(cl:in-package #:clim-broadway)

(defclass tile ()
  ((%region :initarg :region
            :reader  region)))

;;; Tile upload

(defun tile->png (tile image-pixels)
  (with-bounding-rectangle* (x1 y1 x2 y2) (region tile)
    (let* ((width       (- x2 x1))         
           (height      (- y2 y1))
           (tile-pixels (nibbles:make-octet-vector (* 3 width height))))
      (loop :with marker = (random 10)
            :for y-in  :from  y1 :below y2
            :for y-out :below height
            :do (loop :for x-in  :from  x1 :below x2
                      :for x-out :below width
                      :for index = (* 3 (+ (* y-out width) x-out))
                      :for pixel = (aref image-pixels y-in x-in)
                      :do (setf (aref tile-pixels (+ index 0)) (ldb (byte 8 24) pixel)
                                (aref tile-pixels (+ index 1)) (max 0 (- (ldb (byte 8 16) pixel) marker))
                                (aref tile-pixels (+ index 2)) (ldb (byte 8  8) pixel))))
      (let ((png    (make-instance 'zpng:png
                                   :width      width
                                   :height     height
                                   :image-data tile-pixels))
            (stream (flexi-streams:make-in-memory-output-stream)))
        (zpng:write-png-stream png stream)
        (flexi-streams:get-output-stream-sequence stream)))))

;;; Tile creation

(defun map-intervals (function limit interval-size)
  (loop :for start = 0 :then end
        :for end :from interval-size :to limit :by interval-size
        :do (funcall function start end)
        :finally (funcall function start limit)))

(defun map-tile-rectangles (function width height tile-size)
  (map-intervals (lambda (x1 x2)
                   (map-intervals (lambda (y1 y2)
                                    (funcall function x1 y1 x2 y2))
                                  height tile-size))
                 width tile-size))

(defun make-tiles (width height tile-size)
  (let ((tiles '()))
    (map-tile-rectangles
     (lambda (x1 x2 y1 y2)
       (push (make-instance 'tile
                            :region (make-rectangle* x1 x2 y1 y2))
             tiles))
     width height tile-size)
    tiles))
