;; -*- mode: Gimp; -*-
;
; Slice Layers
;
; ===========================================================================
;
; LICENSE
;
; Copyright (C) 2020 - Sergey Goldgaber
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU Affero General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU Affero General Public License for more details.
;
; You should have received a copy of the GNU Affero General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
; ===========================================================================


(define get-type
  (lambda (x)
    (cond
     ((null? x) "null")
     ((char? x) "char")
     ((list? x) "list")
     ((number? x) "number")
     ((pair? x) "pair")
     ((string? x) "string")
     ((symbol? x) "symbol")
     ((vector? x) "vector")
     (#t "unknown type"))))


; Main entry point in to the script
; It is registered using script-fu-register and script-fu-menu-register below
(define (script-fu-slice-layers
         given-image
         given-layer
         vertical-or-horizontal
         slice-width-in-pixels)
  (gimp-image-undo-group-start given-image)
  ; Save the current selection and foreground color
  (let* ((old-selection (car (gimp-selection-save given-image)))
         (old-foreground-color (car (gimp-context-get-foreground))))
    ; Do the slicing
    (slice-layers-aux
     given-image
     vertical-or-horizontal
     slice-width-in-pixels)
    ; Restore old selection
    (gimp-image-select-item given-image CHANNEL-OP-REPLACE old-selection)
    ; Restore old foreground color
    (gimp-context-set-foreground old-foreground-color))
  (gimp-image-undo-group-end given-image)
  (gimp-displays-flush))


(define (slice-layer
         given-image
         layer-id
         vertical-or-horizontal
         slice-width-in-pixels
         layer-id
         number-of-layers)
  (let* ((layer-position
          (car (gimp-image-get-item-position
                given-image
                layer-id)))
         (lowest-layer-id (- number-of-layers 1)))
    ; Skip the lowest layer
    ; because only the rest of the layers need layer masks
    (if (< layer-position lowest-layer-id)
        (let* ((ignored (gimp-layer-add-alpha layer-id))
               (mask (car (gimp-layer-create-mask layer-id ADD-MASK-WHITE))))
          (gimp-layer-add-mask layer-id mask)
          (slice-layer-fill-mask-with-pattern
           given-image
           layer-id
           vertical-or-horizontal
           slice-width-in-pixels
           layer-position
           number-of-layers
           mask)))))


(define (slice-layer-fill-mask-with-pattern
         given-image
         layer-id
         vertical-or-horizontal
         slice-width-in-pixels
         layer-position
         number-of-layers
         mask)
  (let* ((image-layer-height (car (gimp-drawable-height layer-id)))
         (image-layer-width  (car (gimp-drawable-width  layer-id)))
         (pattern-layer-height (* number-of-layers slice-width-in-pixels))
         (pattern-layer-width pattern-layer-height)
         (pattern-layer-type  (car (gimp-drawable-type layer-id)))
         (pattern-layer-name "Slice Layers Pattern")
         (pattern-layer-opacity 100)
         (pattern-layer-mode LAYER-MODE-NORMAL)
         (pattern-layer
          (car (gimp-layer-new
                given-image
                pattern-layer-width
                pattern-layer-height
                pattern-layer-type
                pattern-layer-name
                pattern-layer-opacity
                pattern-layer-mode)))
         (pattern-layer-parent 0)
         (pattern-layer-position -1)
         (ignored
          (gimp-image-insert-layer
           given-image
           pattern-layer
           pattern-layer-parent
           pattern-layer-position)))))

(define (slice-layers-aux
         given-image
         vertical-or-horizontal
         slice-width-in-pixels)
  (let* ((all-layers (gimp-image-get-layers given-image))
         (all-layer-ids
          (vector->list
           (cadr all-layers)))
         (number-of-layers
          (car all-layers)))
    (map (lambda (layer-id)
             (slice-layer
              given-image
              layer-id
              vertical-or-horizontal
              slice-width-in-pixels
              layer-id
              number-of-layers)) all-layer-ids)))


(script-fu-register "script-fu-slice-layers"
                    "Slice Layers..."
                    "Slice a set of layers"
                    "Sergey Goldgaber"
                    "Copyright 2020, Sergey Goldgaber"
                    "Sep 15, 2020"
                    "RGB, RGBA"
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Layer" 0
                    SF-OPTION "Vertical or horizontal slices?" '("Vertical" "Horizontal")
                    SF-ADJUSTMENT "Slice width in pixels:" '(1 1 100000000 1 10 0 SF-SPINNER))


(script-fu-menu-register "script-fu-slice-layers" "<Image>/Filters/Artistic")
