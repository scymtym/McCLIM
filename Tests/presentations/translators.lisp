;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020 Daniel Kochmański <daniel@turtleware.eu>
;;;  (c) copyright 2020,2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Tests for presentation translators.

(in-package #:clim-tests)

(def-suite* :mcclim.presentation-translators
  :in :mcclim)

(define-gesture-name test-gesture
  :pointer-button-release (:left))

(test %test-presentation-translator.smoke
  (define-command-table %test-presentation-translator.smoke)
  (let ((translator (define-presentation-translator %test-presentation-translator.smoke
                        (real string %test-presentation-translator.smoke
                         :gesture test-gesture)
                        (object))))
    (mapcar
     (lambda (case)
       (destructuring-bind (presentation-type context-type
                            (event-class &rest event-initargs
                                         &key (button         +pointer-left-button+)
                                              (modifier-state 0))
                            override expected)
           case
         (let* ((presentation (make-instance 'standard-presentation
                                             :object 1
                                             :type   presentation-type))
                (event        (apply #'make-instance event-class
                                     :sheet          nil
                                     :modifier-state modifier-state
                                     :button         button
                                     (alexandria:remove-from-plist
                                      event-initargs :button :modifier-state)))
                (result       (climi::%test-presentation-translator
                               translator presentation context-type nil nil 0 0 event
                               :override override)))
           (is (eq expected result)
               "Testing translator ~A with presentation ~A, context ~
                type ~S, event ~A and override ~S returned ~A, but ~
                expected ~A"
               translator presentation context-type event override
               result expected))))
     `(;; Presentation mismatch
       (complex string  (pointer-button-release-event)                                 ()                               nil)
       ;; Context mismatch
       (integer command (pointer-button-release-event)                                 ()                               nil)
       ;; Gesture mismatches
       (integer string  (pointer-button-press-event)                                   ()                               nil)
       (integer string  (pointer-button-release-event :button ,+pointer-right-button+) ()                               nil)
       (integer string  (pointer-button-release-event :modifier-state ,+meta-key+)     ()                               nil)
       ;; Matches
       (integer string  (pointer-button-release-event)                                 ()                               t)
       (ratio   string  (pointer-button-release-event)                                 ()                               t)
       ;; Override
       (integer string  (pointer-button-press-event)                                   (:type :pointer-button-release)  t)
       (integer string  (pointer-button-press-event)                                   (:type nil)                      t)
       (integer string  (pointer-button-release-event :button ,+pointer-right-button+) (:button ,+pointer-left-button+) t)
       (integer string  (pointer-button-release-event :button ,+pointer-right-button+) (:button nil)                    t)
       (integer string  (pointer-button-release-event :modifier-state ,+meta-key+)     (:modifier-state 0)              t)
       (integer string  (pointer-button-release-event :modifier-state ,+meta-key+)     (:modifier-state nil)            t)))))

(test presentation-translators.smoke
  (define-command-table pt.smoke-ct)
  (let ((tr (define-presentation-translator pt-smoke-tr
                (integer string pt.smoke-ct)
                (object)
              (format nil "~a" object)))
        (all (find-presentation-translators 'integer 'string 'pt.smoke-ct)))
    (is (member tr all))))

(test presentation-translators.meta
  (define-command-table pt.smoke-ct2)
  (let ((translator (define-presentation-translator pt-smoke-tr2
                ((or real string) string pt.smoke-ct2)
                (object)
              (etypecase object
                (real (format nil "~a" object))
                (string object)))))
    (labels ((find-translators (from to)
               (find-presentation-translators from to 'pt.smoke-ct2))
             (is-applicable (from to)
               (is (member translator (find-translators from to))
                   "~@<Expected ~A to be applicable when translating from ~
                    ~S to ~S~@:>"
                   translator from to)
               ;; Run the same query again, so the cache is used.
               (is (member translator (find-translators from to))
                   "~@<Expected ~A to be applicable when translating WITH ~
                    CACHE from ~S to ~S~@:>"
                   translator from to)))
      (is-applicable 'real                                'string)
      (is-applicable 'string                              'string)
      (is-applicable '(or string real)                    'string)
      (is-applicable '(or string integer)                 'string)
      (is-applicable '(and string (completion ("a" "b"))) 'string)
      (is-applicable '(completion ("a" "b"))              'string)
      (is-applicable '(completion ("dan" 3))              'string)
      (is-applicable 'number                              'string)
      (is-applicable '(or string number)                  'string)
      (is-applicable '(completion ("dan" :foo))           'string)
      ;; Make sure meta type as "to" type do not result in invalid caching.
      (is-applicable 'real                                '(or real string))
      (is-applicable 'real                                '(or real number)))))
