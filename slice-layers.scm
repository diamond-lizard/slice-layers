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
  ;; Do some sanity checks
  (let* ((all-layers (gimp-image-get-layers given-image))
         (all-layer-ids
          (vector->list
           (cadr all-layers)))
         (number-of-layers
          (car all-layers))
         (minimum-number-of-layers 2))
    (when (< number-of-layers minimum-number-of-layers)
      (begin
        (gimp-message
         "Error: The Slice Layers script requires a minimum of two layers to work.")
        (quit))))
  (gimp-image-undo-group-start given-image)
  ; Save the current selection and foreground color
  (let* ((old-selection (car (gimp-selection-save given-image)))
         (old-pattern (car (gimp-context-get-pattern)))
         (old-foreground-color (car (gimp-context-get-foreground))))
    ; Do the slicing
    (slice-layers--aux
     given-image
     vertical-or-horizontal
     slice-width-in-pixels)
    ; Restore old selection
    (gimp-image-select-item given-image CHANNEL-OP-REPLACE old-selection)
    ; Restore old pattern
    (gimp-context-set-pattern old-pattern)
    ; Restore old foreground color
    (gimp-context-set-foreground old-foreground-color))
  (gimp-image-undo-group-end given-image)
  (gimp-displays-flush))


(define (slice-layers--aux
         given-image
         vertical-or-horizontal
         slice-width-in-pixels)
  (let* ((all-layers (gimp-image-get-layers given-image))
         (all-layer-ids
          (vector->list
           (cadr all-layers)))
         (number-of-layers
          (car all-layers))
         (lowest-layer-position (- number-of-layers 1))
         (layers-and-positions
          (map (lambda (layer-id)
                 (let ((layer-position
                        (car (gimp-image-get-item-position
                              given-image
                              layer-id))))
                   (cons layer-id layer-position)))
               all-layer-ids)))
    (map (lambda (layer-and-position)
           (let ((layer-id
                  (car layer-and-position))
                 (layer-position
                  (cdr layer-and-position)))
             (slice-layers--slice-layer
              given-image
              layer-id
              layer-position
              lowest-layer-position
              vertical-or-horizontal
              slice-width-in-pixels
              number-of-layers)))
         layers-and-positions)))


(define (slice-layers--fill-layer-with-black
         layer)
  (let* ((black '(0 0 0))
         (ignored
          (gimp-context-set-foreground black))
         (gimp-drawable-fill-type FILL-FOREGROUND))
    (gimp-drawable-fill
     layer
     gimp-drawable-fill-type)))


(define (slice-layers--fill-layer-with-white
         layer)
  (let* ((white '(255 255 255))
         (ignored
          (gimp-context-set-foreground white))
         (gimp-drawable-fill-type FILL-FOREGROUND))
    (gimp-drawable-fill
     layer
     gimp-drawable-fill-type)))


(define (slice-layer--fill-mask-with-pattern
         given-image
         layer-id
         vertical-or-horizontal
         slice-width-in-pixels
         layer-position
         number-of-layers
         mask)
  (let* ((pattern-layer
          (slice-layers--make-pattern-layer
           given-image
           layer-id
           number-of-layers
           slice-width-in-pixels))
         (ignored
          (slice-layers--fill-layer-with-black
           pattern-layer))
         (ignored
          (slice-layers--fill-slice-with-white
           given-image
           pattern-layer
           layer-position
           number-of-layers
           vertical-or-horizontal
           slice-width-in-pixels))
         (ignored
          (slice-layers--select-all
           given-image
           pattern-layer))
         (copy-result
          (car (gimp-edit-copy pattern-layer))))
    (gimp-selection-none given-image)
    (if (equal? copy-result TRUE)
        (begin
          (gimp-image-remove-layer
           given-image
           pattern-layer)
          (gimp-context-set-pattern "Clipboard Image")
          (gimp-edit-fill
           mask
           FILL-PATTERN))
        (begin
          (gimp-message "Error: Copying pattern failed.  This should never happen.")
          (gimp-image-undo-group-end given-image)
          (quit)))))


(define (slice-layers--fill-slice-with-black
         given-image
         layer
         layer-position
         number-of-layers
         vertical-or-horizontal
         slice-width-in-pixels)
  (let ((upper-left-x
         (if (equal? vertical-or-horizontal 0)
             (* layer-position slice-width-in-pixels)
             0))
        (upper-left-y
         (if (equal? vertical-or-horizontal 0)
             0
             (* layer-position slice-width-in-pixels)))
        (width
         (if (equal? vertical-or-horizontal 0)
             slice-width-in-pixels
             (car (gimp-drawable-width layer))))
        (height
         (if (equal? vertical-or-horizontal 0)
             (car (gimp-drawable-height layer))
             slice-width-in-pixels))
        (black '(0 0 0))
        (gimp-drawable-fill-type FILL-FOREGROUND))
    (gimp-image-select-rectangle
     given-image
     CHANNEL-OP-REPLACE
     upper-left-x
     upper-left-y
     width
     height)
    (gimp-context-set-foreground black)
    (gimp-edit-fill
     layer
     gimp-drawable-fill-type)
    (gimp-selection-none given-image)))


(define (slice-layers--fill-slice-with-white
         given-image
         layer
         layer-position
         number-of-layers
         vertical-or-horizontal
         slice-width-in-pixels)
  (let ((upper-left-x
         (if (equal? vertical-or-horizontal 0)
             (* layer-position slice-width-in-pixels)
             0))
        (upper-left-y
         (if (equal? vertical-or-horizontal 0)
             0
             (* layer-position slice-width-in-pixels)))
        (width
         (if (equal? vertical-or-horizontal 0)
             slice-width-in-pixels
             (car (gimp-drawable-width layer))))
        (height
         (if (equal? vertical-or-horizontal 0)
             (car (gimp-drawable-height layer))
             slice-width-in-pixels))
        (white '(255 255 255))
        (gimp-drawable-fill-type FILL-FOREGROUND))
    (gimp-image-select-rectangle
     given-image
     CHANNEL-OP-REPLACE
     upper-left-x
     upper-left-y
     width
     height)
    (gimp-context-set-foreground white)
    (gimp-edit-fill
     layer
     gimp-drawable-fill-type)
    (gimp-selection-none given-image)))


(define (slice-layers--make-pattern-layer
         given-image
         layer-id
         number-of-layers
         slice-width-in-pixels)
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
           pattern-layer-position)))
    pattern-layer))


(define (slice-layers--select-all
         given-image
         given-layer)
  (let ((operation CHANNEL-OP-REPLACE)
        (upper-left-x 0)
        (upper-left-y 0)
        (width  (car (gimp-drawable-width  given-layer)))
        (height (car (gimp-drawable-height given-layer))))
    (gimp-image-select-rectangle
     given-image
     operation
     upper-left-x
     upper-left-y
     width
     height)))


(define (slice-layers--slice-layer
         given-image
         layer-id
         layer-position
         lowest-layer-position
         vertical-or-horizontal
         slice-width-in-pixels
         number-of-layers)
  ; Skip the lowest layer
  ; because only the rest of the layers need layer masks
  (if (< layer-position lowest-layer-position)
      (let* ((ignored (gimp-layer-add-alpha layer-id))
             (mask (car (gimp-layer-create-mask layer-id ADD-MASK-WHITE))))
        (gimp-layer-add-mask layer-id mask)
        (slice-layer--fill-mask-with-pattern
         given-image
         layer-id
         vertical-or-horizontal
         slice-width-in-pixels
         layer-position
         number-of-layers
         mask))))


(script-fu-register "script-fu-slice-layers"
                    "Slice Layers..."
                    "Slice a set of layers"
                    "Sergey Goldgaber"
                    "Copyright 2020, Sergey Goldgaber"
                    "Sep 15, 2020"
                    ""
                    SF-IMAGE "Image" 0
                    SF-DRAWABLE "Layer" 0
                    SF-OPTION "Vertical or horizontal slices?" '("Vertical" "Horizontal")
                    SF-ADJUSTMENT "Slice width in pixels:" '(1 1 100000000 1 10 0 SF-SPINNER))


(script-fu-menu-register "script-fu-slice-layers" "<Image>/Filters/Artistic")
