(cl:in-package #:clim-broadway)

;;; Server -> client operations

(define-protocol operation
    ((opcode 1)
     (serial 4))

  ((grab-pointer 0)
   (id           2)
   (owner-event? boolean))

  ((ungrab-pointer 1))

  ((new-surface 2 :print-spec ("~D" id))
   (id     2)
   (x      2)
   (y      2)
   (width  2)
   (height 2)
   (temp?  boolean))

  ((show-surface 3 :print-spec ("~D" id))
   (id 2))

  ((hide-surface 4 :print-spec ("~D" id))
   (id 2))

  ((raise-surface 5 :print-spec ("~D" id))
   (id 2))

  ((lower-surface 6 :print-spec ("~D" id))
   (id 2))

  ((destroy-surface 7 :print-spec ("~D" id))
   (id 2))

  ((move-resize 8 :print-spec ("~D ~D,~D ~Dx~D" id x y width height))
   (id     2)
   (flags  1) ; TODO
   (x      2) ; TODO only if "has-pos"
   (y      2)
   (width  2) ; TODO only if "has-size"
   (height 2))

  ((set-transient-for 9)
   (id        2)
   (parent-id 2))

  ((disconnected 10)
   )

  #+unused ((surface-update 11))

  ((set-show-keyboard 12)
   (show 2))

  ;; SIZE indicates the size of a PNG blob immediately following SIZE.
  ((upload-texture 13 :print-spec ("~D [~:D byte~:P]" id size))
   (id   4)
   (size 4)
   ;; (data octets)
   )

  ((release-texture 14 :print-spec ("~D" id))
   (id 4))

  ;; Set the nodes of a surface. SIZE indicates the size of a payload
  ;; immediately following the SIZE field encoded and encoded using
  ;; the node operation and node protocols (see below).
  ((set-nodes 15 :print-spec ("~D [~:D byte~:P]" id size))
   (id   2)
   (size 4))

  ((roundtrip 16 :print-spec ("~D ~A" id tag))
   (id  2)
   (tag 4)))

;;; Node creation operations

#+unused (define-enum *node-kind*
  (:texture         0)
  (:container       1)
  (:color           2)
  (:border          3)
  (:outset-shadow   4)
  (:inset-shadow    5)
  (:rounded-clip    6)
  (:linear-gradient 7)
  (:shadow          8)
  (:opacity         9)
  (:clip            10)
  (:transform       11)
  (:debug           12)
  (reuse           13))

(define-protocol make-node
    ((type 4)
     (id   4))

  ((texture 0 :print-spec ("~D" id))
   (x      :float32)
   (y      :float32)
   (width  :float32)
   (height :float32)
   (id     4))

  ((container 1))

  ((color 2)
   (x      :float32)
   (y      :float32)
   (width  :float32)
   (height :float32)

   (red    1)
   (green  1)
   (blue   1)
   (alpha  1))

  ((border 3)
   (x             :float32)
   (y             :float32)
   (width         :float32)
   (height        :float32)

   (top-radius-x    :float32)
   (top-radius-y    :float32)
   (right-radius-x  :float32)
   (right-radius-y  :float32)
   (bottom-radius-x :float32)
   (bottom-radius-y :float32)
   (left-radius-x   :float32)
   (left-radius-y   :float32)

   (top-width     :float32)
   (right-width   :float32)
   (bottom-width  :float32)
   (left-width    :float32)

   (top-color     4)
   (right-color   4)
   (bottom-color  4)
   (left-color    4))

  ((outset-shadow 4)
   (x               :float32)
   (y               :float32)
   (width           :float32)
   (height          :float32)

   (top-radius-x    :float32)
   (top-radius-y    :float32)
   (right-radius-x  :float32)
   (right-radius-y  :float32)
   (bottom-radius-x :float32)
   (bottom-radius-y :float32)
   (left-radius-x   :float32)
   (left-radius-y   :float32)

   (red             1)
   (green           1)
   (blue            1)
   (alpha           1)

   (dx              :float32)
   (dy              :float32)
   (spread          :float32)
   (blur            :float32))

  ((inset-shadow 5))

  ((reuse 13)))

;;; Node operations

(define-protocol node-operation
    ((opcode 4))

  ((insert-node 0 :print-spec ("parent ~D sibling ~D"
                               parent-id previous-sibling-id))
   (parent-id           4)
   (previous-sibling-id 4))

  ((remove-node 1 :print-spec ("id ~D" id))
   (id 4))

  ((move-after-child 2 :print-spec ("id ~D parent ~D sibling ~D"
                                    reused-node-id parent-id
                                    previous-sibling-id))
   (parent-id           4)
   (previous-sibling-id 4)
   (reused-node-id      4))

  ((patch-texture 3 :print-spec ("node ~D texture ~D"
                                 node-id texture-id))
   (node-id    4)
   (texture-id 4))

  ((patch-transform 4)))

;;; Client -> server events

(define-protocol event
    ((opcode    4)
     (serial    4)
     (timestamp 4))

  ((enter 0 :print-spec ("~D ~D" surface id))
   (surface    4)
   (id         4)
   (root-x     4)
   (root-y     4)
   (win-x      4)
   (win-y      4)
   (last-state 4)
   (?          4))

  ((leave 1 :print-spec ("~D ~D" surface id))
   (surface    4)
   (id         4)
   (root-x     4)
   (root-y     4)
   (win-x      4)
   (win-y      4)
   (last-state 4)
   (?          4))

  ((pointer-move 2 :print-spec ("~D,~D [~D,~D in ~D/~D]"
                                root-x root-y win-x win-y surface id))
   (surface    4)
   (id         4)
   (root-x     4)
   (root-y     4)
   (win-x      4)
   (win-y      4)
   (last-state 4))

  ((button-press 3 :print-spec ("~A ~D,~D [~D,~D in ~D/~D]"
                                button root-x root-y win-x win-y surface id))
   (surface    4)
   (id         4)
   (root-x     4)
   (root-y     4)
   (win-x      4)
   (win-y      4)
   (last-state 4)
   (button     4))

  ((button-release 4 :print-spec ("~A ~D,~D [~D,~D in ~D/~D]"
                                  button root-x root-y win-x win-y surface id))
   (surface    4)
   (id         4)
   (root-x     4)
   (root-y     4)
   (win-x      4)
   (win-y      4)
   (last-state 4)
   (button     4))

  ((touch 5))

  ((scroll 6 :print-spec ("~A ~D,~D [~D,~D in ~D/~D]"
                          direction root-x root-y win-x win-y surface id))
   (surface    4)
   (id         4)
   (root-x     4)
   (root-y     4)
   (win-x      4)
   (win-y      4)
   (last-state 4)
   (direction  4))

  ((key-press 7 :print-spec ("~D" keysym)) ; TODO "~C [~D]" (code-char keysym) keysym
   (keysym     4)
   (last-state 4))

  ((key-release 8 :print-spec ("~D" keysym))
   (keysym     4)
   (last-state 4))

  ((grab-notify 9))

  ((ungrab-notify 10))

  ((configure-notify 11)
   (surface 4)
   (x       4)
   (y       4)
   (width   4)
   (height  4))

  ((screen-size-changed 12)
   (width  4)
   (height 4))

  ((focus               13))

  ((roundtrip-notify 14)))
