;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 1998-2000 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2019 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Region algebra method for the region set classes.

(in-package #:climi)

;;; utilities

(defun rectangle-set->polygon-union (rs)
  (let ((res nil))
    (map-over-region-set-regions (lambda (r) (push r res)) rs)
    (make-instance 'standard-region-union :regions res)))

;;; REGION-UNION

;;; STANDARD-REGION-UNION
(defmethod region-union ((a standard-region-union) (b standard-region-union))
  (assert (not (eq b +nowhere+)))
  (assert (not (eq a +nowhere+)))
  (make-instance 'standard-region-union
                 :regions (append (standard-region-set-regions a)
                                  (standard-region-set-regions b))))

(defmethod region-union ((a standard-region-union) (b bounding-rectangle))
  (make-instance 'standard-region-union
                 :regions (cons b (standard-region-set-regions a))))

(defmethod region-union ((b bounding-rectangle) (a standard-region-union))
  (make-instance 'standard-region-union
                 :regions (cons b (standard-region-set-regions a))))

;;; STANDARD-RECTANGLE-SET
(defmethod region-union ((a standard-rectangle-set) (b path)) a)
(defmethod region-union ((b path) (a standard-rectangle-set)) a)
(defmethod region-union ((a standard-rectangle-set) (b point)) a)
(defmethod region-union ((b point) (a standard-rectangle-set)) a)

(defmethod region-union ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-union (standard-rectangle-set-bands xs)
                (standard-rectangle-set-bands ys))))

(defmethod region-union ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-union xs (rectangle->standard-rectangle-set ys)))

(defmethod region-union ((xs standard-rectangle) (ys standard-rectangle-set))
  (region-union (rectangle->standard-rectangle-set xs) ys))

(defmethod region-union ((a standard-rectangle-set) (b polygon))
  (region-union (rectangle-set->polygon-union a) b))

(defmethod region-union ((a polygon) (b standard-rectangle-set))
  (region-union a (rectangle-set->polygon-union b)))

;;; STANDARD-REGION-INTERSECTION
;;; STANDARD-REGION-DIFFERENCE
(defmethod region-union ((a standard-region-difference) (b bounding-rectangle))
  (make-instance 'standard-region-union :regions (list a b)))

(defmethod region-union ((a bounding-rectangle) (b standard-region-difference))
  (make-instance 'standard-region-union :regions (list a b)))

;;; REGION-INTERSECTION

;;; STANDARD-REGION-UNION
(defmethod region-intersection ((a standard-region-union) (b bounding-rectangle))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (r) (setf res (region-union res (region-intersection r b)))) a)
    res))

(defmethod region-intersection ((a bounding-rectangle) (b standard-region-union))
  (region-intersection b a))

;;; STANDARD-RECTANGLE-SET
(defmethod region-intersection ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-intersection (standard-rectangle-set-bands xs)
                       (standard-rectangle-set-bands ys))))

(defmethod region-intersection ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-intersection xs (rectangle->standard-rectangle-set ys)))

(defmethod region-intersection ((xs standard-rectangle) (ys standard-rectangle-set))
  (region-intersection (rectangle->standard-rectangle-set xs) ys))

(defmethod region-intersection ((a standard-rectangle-set) (b bounding-rectangle))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (r)
       (setf res (region-union res (region-intersection r b))))
     a)
    res))

(defmethod region-intersection ((a bounding-rectangle) (b standard-rectangle-set))
  (region-intersection b a))

;;; STANDARD-REGION-INTERSECTION
(defmethod region-intersection ((a bounding-rectangle) (b standard-region-intersection))
  (map-over-region-set-regions
   (lambda (r)
     (setf a (region-intersection a r)))
   b)
  a)

(defmethod region-intersection ((a standard-region-intersection) (b bounding-rectangle))
  (region-intersection b a))

;;; STANDARD-REGION-DIFFERENCE
(defmethod region-intersection ((x bounding-rectangle) (y standard-region-difference))
  (with-slots (a b) y
    (region-difference (region-intersection x a) b)))

(defmethod region-intersection ((x standard-region-difference) (y bounding-rectangle))
  (with-slots (a b) x
    (region-difference (region-intersection y a) b)))

;;; REGION-DIFFERENCE

;;; STANDARD-REGION-UNION
(defmethod region-difference ((x bounding-rectangle) (y standard-region-union))
  ;; A \ (B1 u B2 .. u Bn) = ((((A \ B1) \ B2) ... ) \ Bn)
  (let ((res x))
    (map-over-region-set-regions (lambda (a)
                                   (setf res (region-difference res a)))
                                 y)
    res))

(defmethod region-difference ((x standard-region-union) (y bounding-rectangle))
  ;; (A u B) \ C = A\C u B\C
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-union res (region-difference a y))))
     x)
    res))

;;; STANDARD-RECTANGLE-SET
(defmethod region-difference ((x bounding-rectangle) (y standard-rectangle-set))
  (let ((res x))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-difference res a)))
     y)
    res))

(defmethod region-difference ((x standard-rectangle-set) (y bounding-rectangle))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (a)
       (setf res (region-union res (region-difference a y))))
     x)
    res))

(defmethod region-difference ((xs standard-rectangle-set) (ys standard-rectangle-set))
  (make-standard-rectangle-set
   (bands-difference (standard-rectangle-set-bands xs)
                     (standard-rectangle-set-bands ys))))

(defmethod region-difference ((xs standard-rectangle-set) (ys standard-rectangle))
  (region-difference xs (rectangle->standard-rectangle-set ys)))

(defmethod region-difference ((xs standard-rectangle) (ys standard-rectangle-set))
  (region-difference (rectangle->standard-rectangle-set xs) ys))

;;; STANDARD-REGION-INTERSECTION
(defmethod region-difference ((x bounding-rectangle) (y standard-region-intersection))
  (let ((res +nowhere+))
    (map-over-region-set-regions
     (lambda (b)
       (setf res (region-union res (region-difference x b))))
     y)
    res))

;;; STANDARD-REGION-DIFFERENCE
(defmethod region-difference ((x bounding-rectangle) (y standard-region-difference))
  (with-slots (a b) y
    (region-union (region-difference x a) (region-intersection x b))))

(defmethod region-difference ((x standard-region-difference) (y bounding-rectangle))
  ;; (A\B)\C = A \ (B u C)
  (with-slots (a b) x
    (region-difference a (region-union b y))))
