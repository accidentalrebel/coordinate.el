;;; test-helper --- Test helper for coordinate

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar coordinate-test-path
  (f-dirname (f-this-file)))

(defvar coordinate-root-path
  (f-parent coordinate-test-path))

(defvar coordinate-sandbox-path
  (f-expand "sandbox" coordinate-test-path))

(when (f-exists? coordinate-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" coordinate-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory coordinate-sandbox-path))
     (when (f-exists? coordinate-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir coordinate-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
  (require 'cl))
(require 'coordinate)

;;; test-helper.el ends here
