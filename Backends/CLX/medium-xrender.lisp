;;;;  Copyright (c) 2003       Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;;  Copyright (c) 2018       Daniel Kochmański <daniel@turtleware.eu>
;;;;
;;;;    License:  LGPL-2.1-or-later

(in-package #:clim-clx)

(defclass clx-render-medium (clx-medium
                             climb:font-rendering-medium-mixin)
  ((picture :initform nil)))

(defun clx-render-medium-picture (medium)
  (with-slots (picture) medium
    (when-let* ((mirror (medium-drawable medium))
                (format (xlib:find-window-picture-format (xlib:drawable-root mirror))))
      (cond
        ((null picture)
         (setf picture (xlib:render-create-picture mirror :format format)))
        ;; We need this comparison to mitigate a rogue mirror swaps with a
        ;; pixmap, i.e in WITH-TEMP-MIRROR%%% for double buffering.
        ((eq mirror (xlib:picture-drawable picture))
         picture)
        (T ;; mirror has been swapped!
         (xlib:render-free-picture picture)
         (setf picture (xlib:render-create-picture mirror :format format)))))))


(defun medium-draw-rectangle-xrender (medium x1 y1 x2 y2 filled)
  (declare (ignore filled))
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (let ((x1 (round-coordinate x1))
              (y1 (round-coordinate y1))
              (x2 (round-coordinate x2))
              (y2 (round-coordinate y2)))
          (multiple-value-bind (r g b a) (clime:color-rgba (medium-ink medium))
            ;; Hmm, XRender uses pre-multiplied alpha, how useful!
            (setf r (min #xffff (max 0 (round (* #xffff a r))))
                  g (min #xffff (max 0 (round (* #xffff a g))))
                  b (min #xffff (max 0 (round (* #xffff a b))))
                  a (min #xffff (max 0 (round (* #xffff a)))))
            ;; If there is no picture that means that sheet does not have a
            ;; registered mirror. Happens with DREI panes during the startup..
            (when-let ((picture (clx-render-medium-picture medium)))
              (setf (xlib:picture-clip-mask picture) (clipping-region->rect-seq
                                                      (or (last-medium-device-region medium)
                                                          (medium-device-region medium))))
              (xlib:render-fill-rectangle picture :over (list r g b a)
                                          (max #x-8000 (min #x7FFF x1))
                                          (max #x-8000 (min #x7FFF y1))
                                          (max 0 (min #xFFFF (- x2 x1)))
                                          (max 0 (min #xFFFF (- y2 y1)))))))))))

(defmethod medium-draw-rectangle* ((medium clx-render-medium) left top right bottom filled)
  #- (or)
  (call-next-method medium left top right bottom filled)
  #+ (or) ;; this causes regressions (i.e in draggable graph)
  (if filled
      (typecase (medium-ink medium)
        ((or climi::uniform-compositum clim:color clim:opacity)
         (medium-draw-rectangle-xrender medium left top right bottom filled))
        (otherwise
         (call-next-method)))
      (call-next-method)))

(defmethod climb:text-size ((medium clx-render-medium) string &key text-style (start 0) end
                            &aux
                              (string (string string))
                              (end (or end (length string))))
  (when (= start end)
    (return-from text-size (values 0 0 0 0 (text-style-ascent text-style medium))))
  (let* ((text-style (merge-text-styles text-style
                                        (medium-merged-text-style medium)))
         (font (text-style-mapping (port medium) text-style))
         (text (subseq (string string) start end))
         (ascent (climb:font-ascent font))
         (line-height (+ ascent (climb:font-descent font)))
         (leading (climb:font-leading font))
         (current-dx 0)
         (maximum-dx 0)
         (current-y 0))
    (dolines (text text)
      (loop
         with origin-x fixnum = 0
         for code across (climb:font-string-glyph-codes font text)
         do (incf origin-x (climb:font-glyph-dx font code))
         finally
           (alexandria:maxf maximum-dx origin-x)
           (setf current-dx origin-x)
           (incf current-y leading)))
    (values maximum-dx (+ current-y line-height (- leading))
            current-dx (- current-y leading)
            ascent)))

(defvar *draw-font-lock* (clim-sys:make-lock "draw-font"))
(defmethod medium-draw-text* ((medium clx-render-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs
                              &aux (end (if (null end)
                                            (length string)
                                            (min end (length string)))))
  ;; Possible optimizations:
  ;;
  ;; * with-clx-graphics already creates appropriate pixmap for us (correct one!) and we have
  ;; medium picture in place - there is no need for gcontext-picture (see xrender-fonts)
  ;; * don't use (PICTURE-DRAWABLE (CLX-RENDER-MEDIUM-PICTURE MEDIUM)) - it is slow due to possible
  ;; mirror swaps, use (SHEET-XMIRROR (MEDIUM-SHEET MEDIUM)) instead. It might be a good idea to
  ;; wrap our own (CLX-RENDER-MEDIUM-MIRROR MEDIUM) function.
  (declare (ignore toward-x toward-y))
  (when (alexandria:emptyp string)
    (return-from medium-draw-text*))
  (with-clx-graphics () medium
    (clim-sys:with-lock-held (*draw-font-lock*)
      (mcclim-font:draw-glyphs medium mirror gc x y string
                               :start start :end end
                               :align-x align-x :align-y align-y
                               :translate #'translate
                               :transformation (sheet-device-transformation (medium-sheet medium))
                               :transform-glyphs transform-glyphs))))
