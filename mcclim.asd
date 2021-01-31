;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2000,2014 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2005,2006,2007 Andreas Fuchs <asf@boinkor.net>
;;;  (c) copyright 2006,2007,2008 David Lichteblau <dlichteblau@common-lisp.net>
;;;  (c) copyright 2006,2007,2008 Troels Henriksen <thenriksen@common-lisp.net>
;;;  (c) copyright 2016-2020 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2019,2020,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The actual McCLIM system that people should to use in their ASDF
;;; package dependency lists.

(in-package #:asdf-user)

(defsystem "mcclim"
  :author (:read-file-form "data/authors.sexp")
  :license "LGPL-2.1+"
  :version "0.9.7"
  :description "McCLIM is an implementation of the CLIM 2.0 specification."
  :long-description "McCLIM is an implementation of the CLIM 2.0 specification.

CLIM (Common Lisp Interface Manager) is an advanced graphical user
interface management system."
  :depends-on ("mcclim/looks" "mcclim/extensions")
  :components ((:file "default-icons"))
  :in-order-to ((test-op (test-op "mcclim/test"))))

;;; A system that loads the appropriate backend for the current
;;; platform.
(defsystem #:mcclim/looks
  :depends-on (#:clim
               #:mcclim-clx                                 #| raw clim-clx backend |#
               #:mcclim-clx/truetype                        #| adds truetype        |#
               #+mcclim-ffi-freetype #:mcclim-clx/freetype  #| adds freetype        |#
               #:mcclim-clx-fb                              #| experimental backend |#

               ;; null backend
               #:mcclim-null))

(defsystem #:mcclim/extensions
  :depends-on (#:mcclim-bitmaps
               #:conditional-commands
               #:mcclim-layouts/tab
               #:mcclim-bezier
               #:clim-pdf
               #:clim-postscript
               #:mcclim-franz))

(defmethod perform :after ((op load-op) (c (eql (find-system :mcclim))))
  (pushnew :clim *features*)) ; The fact that CLIM itself is available is true when all is loaded.

(defsystem "mcclim/test"
  :depends-on ("mcclim"
               "fiveam"
               "mcclim/test-util")
  :components ((:module "Tests"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "input-editing")
                             (:file "input-streams")
                             (:file "commands")
                             (:file "text-selection")
                             (:file "text-formatting")
                             (:file "text-styles")
                             (:file "setf-star")
                             (:module "geometry"
                              :depends-on ("package")
                              :serial t
                              :components ((:file "transforms")
                                           (:file "regions")
                                           (:file "bounding-rectangles")))
                             (:module "drawing"
                              :depends-on ("package")
                              :components ((:file "medium")
                                           (:file "design")
                                           (:file "bezier")
                                           (:file "graphics")))
                             (:module "extended-streams"
                              :depends-on ("package")
                              :components ((:file "recording")
                                           (:file "gestures")))
                             (:module "presentations"
                              :depends-on ("package")
                              :components ((:file "presentation-types")
                                           (:file "presentation-functions")
                                           (:file "presentation-inheritance")
                                           (:file "translators")
                                           (:file "standard-presentations")))
                             (:module "formatting"
                              :depends-on ("package")
                              :components ((:file "misc-formatting")))
                             (:module "frames"
                              :depends-on ("package")
                              :components ((:file "define-application-frame"))))))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:clim-tests '#:run-tests)))

(defsystem "mcclim/test-util"
  :depends-on ("mcclim"
               "mcclim-raster-image"
               "fiveam")
  :components ((:module "Tests/util"
                :serial t
                :components ((:file "package")
                             (:file "graphics-comparison")
                             (:file "test-page")))))

;;; The fact that our CLIM implementation is McCLIM is already true now.
;;; This feature is notably used by ESA and DREI, in cases where they need to
;;; know whether they are compiled with McCLIM or another CLIM implementation.
(pushnew :mcclim *features*)
