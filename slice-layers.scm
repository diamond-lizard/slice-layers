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
         size-of-slice-as-percentage)
  (gimp-image-undo-group-start given-image)
  ; Save the current selection and foreground color
  (let* ((old-selection (car (gimp-selection-save given-image)))
         (old-foreground-color (car (gimp-context-get-foreground))))
    ; Do the slicing
    (slice-layers-aux
     given-image
     vertical-or-horizontal
     size-of-slice-as-percentage)
    ; Restore old selection
    (gimp-image-select-item given-image CHANNEL-OP-REPLACE old-selection)
    ; Restore old foreground color
    (gimp-context-set-foreground old-foreground-color))
  (gimp-image-undo-group-end given-image)
  (gimp-displays-flush))


(define (slice-layers-aux
         given-image
         vertical-or-horizontal
         size-of-slice-as-percentage))


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
                    SF-ADJUSTMENT "Size of slice, as a percentage:" '(2 1 100 1 10 0 SF-SPINNER))


(script-fu-menu-register "script-fu-slice-layers" "<Image>/Filters/Artistic")
