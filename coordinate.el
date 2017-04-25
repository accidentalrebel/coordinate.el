;;; coordinate.el --- Buffer editing using col and row coordinates.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Juan Karlo Licudine

;; Author: Juan Karlo Licudine <accidentalrebel@gmail.com>
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides convenience functions for editing the buffer through col and row coordinates.
;;
;; This package is designed with games in mind but can be used for anything that requires
;; editing of the buffer using col and row coordinates.

;;; Code:

;;;###autoload
(defun coordinate-initialize-view-area (cols rows &optional char)
  "Initialize an area for drawing.
This is the first thing that should be called as initializes the draw area.
If this is not done col and row positions might return incorrectly.

COLS specify the number of columns.
ROWS specify the number of rows.
&optional CHAR is the caharter to place."
  (dotimes (row rows)
    (dotimes (_col cols)
      (insert (if char
		  char
		" "))
      )
    (when (< row (- rows 1))
      (newline))
    ))

(defun coordinate-current-col ()
  "Return the current col at point position.
Has an index of 0."
  (current-column))

(defun coordinate-current-row ()
  "Return the current row at point position.
Has an index of 0."
  (- (line-number-at-pos) 1))

(defun coordinate-position-point-at (col row)
  "Positions the point at COL and ROW coondinates.
Be sure to initialize the draw area with coordinate-initialize-view-area first
otherwise, this would not behave properly.
Coordinates use a starting index of 0."
  (goto-char (point-min))
  (forward-line row)
  (move-to-column col))

(defun coordinate-place-char-at (col row char &optional attributes)
  "Place char at COL and ROW coordinates.
CHAR is the character to place.
&optional ATTRIBUTES is the face attribute to use for the character.
Coordinates use a starting index of 0."
  (save-excursion
    (coordinate-position-point-at col row)
    (replace-rectangle (point) (+ (point) 1) (propertize char 'font-lock-face attributes))))

(defun coordinate-place-string-at-area (col row str &optional attributes)
  "Places at COL and ROW a given STR.
&optional ATTRIBUTES is the face attribute to use for the string.
Can accept a multiline string."
  (save-excursion
    (let ((lines (split-string str "[\n\r]+")))
      (dotimes (index (length lines))
	(coordinate-position-point-at col (+ row index))
	(replace-rectangle (point) (+ (point) (string-width (nth index lines))) (propertize (nth index lines) 'font-lock-face attributes))))))

(defun coordinate-place-char-at-area (col row width height char &optional attributes)
  "Place a character at the given COL and ROW.
WIDTH is the number of columns to repeat the character.
HEIGHT is the number of rows to repeat the character.
CHAR is the character to place.
&optional ATTRIBUTES is the face attribute to use for the character."
  (dotimes (y height)
    (dotimes (x width)
      (coordinate-place-char-at (+ col x) (+ row y) char attributes)
      )
    ))

(defun coordinate-get-char-at (col row)
  "Gets the char at COL and ROW coordinates.
Coordinates use a starting index of 0."
  (coordinate-position-point-at col row)
  (string (char-after)))

(defun coordinate-set-text-property-at (col row attributes)
  "Set the text property at COL and ROW with ATTRIBUTES."
  (coordinate-position-point-at col row)
  (put-text-property (point) (+ (point) 1) 'font-lock-face attributes))

(defun coordinate-remove-text-property-at (col row)
  "Remove the text property at COL and ROW."
  (coordinate-position-point-at col row)
  (remove-text-properties (point) (+ (point) 1) '(font-lock-face)))

(defun coordinate-get-text-property-at (col row)
  "Get the color at COL and ROW."
  (coordinate-position-point-at col row)
  (get-text-property (point) 'font-lock-face))

(provide 'coordinate)
;;; coordinate.el ends here
