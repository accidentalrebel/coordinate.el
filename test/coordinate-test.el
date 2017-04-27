;;; Test for `coordinate'

;;; Commentary:
;; These are the tests for `coordinate'

;;; Code:

(ert-deftest coordinate-initialize-view-area ()
  (with-temp-buffer
    (coordinate-initialize-view-area 3 3)
    (should
     (equal
      (buffer-string) "   \n   \n   ")))
  (with-temp-buffer
    (coordinate-initialize-view-area 1 1 "x")
    (should
     (equal
      (buffer-string) "x")))
  (with-temp-buffer
    (coordinate-initialize-view-area 0 1)
    (should
     (equal
      (buffer-string) ""))))

(ert-deftest coordinate-place-char-at ()
  (with-temp-buffer
   (coordinate-initialize-view-area 3 3 "x")
   (coordinate-place-char-at 0 0 "a")
   (coordinate-place-char-at 1 1 "b")
   (coordinate-place-char-at 2 2 "c")
   (should
    (equal
     (buffer-string) "axx\nxbx\nxxc"))))

(ert-deftest coordinate-place-string-at-area ()
  (with-temp-buffer
    (coordinate-initialize-view-area 4 4 "-")
    (coordinate-place-string-at-area 1 1 "xo\n x
xo")
    (should
     (equal (buffer-string) "----\n-xo-\n- x-\n-xo-"))))

(ert-deftest coordinate-get-char-at ()
  (with-temp-buffer
    (coordinate-initialize-view-area 3 3 "x")
    (coordinate-place-char-at 0 0 "a")
    (coordinate-place-char-at 1 1 "b")
    (coordinate-place-char-at 2 2 "c")
    (should (equal (coordinate-get-char-at 0 0) "a"))
    (should (equal (coordinate-get-char-at 1 1) "b"))
    (should (equal (coordinate-get-char-at 2 2) "c"))
    (should (equal (coordinate-get-char-at 2 0) "x"))))


(ert-deftest coordinate--text-properties ()
  (with-temp-buffer
    (coordinate-initialize-view-area 3 3 "x")
    (coordinate-set-text-property-at 0 0 '(:background "green" :foreground "black"))
    (should (equal (coordinate-get-text-property-at 0 0) '(:background "green" :foreground "black")))
    (coordinate-place-char-at 1 0 "y" '(:height 10))
    (should (equal (coordinate-get-text-property-at 1 0) '(:height 10)))
    (coordinate-place-string-at-area 0 1 "str" '(:weight 'bold))
    (should (equal (coordinate-get-text-property-at 0 1) '(:weight 'bold)))
    (should (equal (coordinate-get-text-property-at 1 1) '(:weight 'bold)))
    (should (equal (coordinate-get-text-property-at 2 1) '(:weight 'bold)))
    (coordinate-place-char-at-area 0 2 3 1 "z" '(:underline t))
    (should (equal (coordinate-get-text-property-at 0 2) '(:underline t)))
    (should (equal (coordinate-get-text-property-at 1 2) '(:underline t)))
    (should (equal (coordinate-get-text-property-at 2 2) '(:underline t)))
    ))
