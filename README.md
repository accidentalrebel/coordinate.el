# coordinate.el

[![Build Status](https://travis-ci.org/accidentalrebel/coordinate.el.svg)](https://travis-ci.org/accidentalrebel/coordinate.el)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

Provides convenience functions for editing the buffer through col and row coordinates. This libary is designed with games in mind but can be used for anything that requires editing of the buffer using col and row coordinates.

## Usage ##
| Function | Use |
|----------|-----|
| coordinate-initialize-view-area | Initializes a rectangular area for drawing using the other coordinate.el functions |
| coordinate-current-col | Returns the current column at point position. Has an index of 0 |
| coordinate-current-row | Returns the current row at point position. Has an index of 0 |
| coordinate-position-point-at | Moves the point at the given column and row coordinates |
| coordinate-place-char-at | Places a single character at the given column and row coordinates. Can accept text attributes |
| coordinate-place-string-at-area | Places a whole string at the given column and row coordinates. Can accept text attributes |
| coordinate-place-char-at-area | Places a single character at the given colmun and row coordinates. The character is repeated until it fills the given width and height of the area. Can accept text attributes. |
| coordinate-get-char-at | Gets the character at the given position |
| coordinate-set-text-property-at | Sets the text property at the given column and row coordinates |
| coordinate-get-text-property-at | Gets the text property at the given column and row coordinates |
| coordinate-remove-text-propert-at | Removes all text properties at the given column and row coordinates |

## Examples ##
Currently used by [tic-tac-toe-improved](https://github.com/accidentalrebel/emacs-tic-tac-toe-improved).

## Notes ##
This library is under development.
