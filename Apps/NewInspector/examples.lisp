(cl:in-package #:new-inspector)

(inspector #1=#(1 2 2
                (1 2 3)
                (1 2 . 3)
                ((:a . "foo") (:b . "bar"))
                #1#)
           :new-process t)

(inspector `#(1 2 2
              (1 2 3)
              (1 2 . 3)
              ,(make-array 3 :initial-contents '(1 2 3) :adjustable t)
              ,(alist-hash-table '(("foo" . 3) (:bar . #C(1 2))))
              ,(find-class 'class)
              ((:a . "foo") (:b . "bar"))
              )
           :new-process t)

(inspector (make-application-frame 'clouseau::inspector :object '(1 2 2)) :new-process t)

(inspector #'inspect-object-using-state)

(inspector #'documentation)
