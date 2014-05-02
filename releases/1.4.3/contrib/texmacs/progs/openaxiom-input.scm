
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : openopenaxiom-input.scm
;; DESCRIPTION : OpenAxiom input converters
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (openaxiom-input)
  (:use (texmacs plugin plugin-convert)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (openaxiom-input-var-row r)
  (if (not (null? r))
      (begin
	(display ", ")
	(plugin-input (car r))
	(openaxiom-input-var-row (cdr r)))))

(define (openaxiom-input-row r)
  (display "[")
  (plugin-input (car r))
  (openaxiom-input-var-row (cdr r))
  (display "]"))

(define (openaxiom-input-var-rows t)
  (if (not (null? t))
      (begin
	(display ", ")
	(openaxiom-input-row (car t))
	(openaxiom-input-var-rows (cdr t)))))

(define (openaxiom-input-rows t)
  (display "matrix([")
  (openaxiom-input-row (car t))
  (openaxiom-input-var-rows (cdr t))
  (display "])"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-input-converters openaxiom
  (rows openaxiom-input-rows))
