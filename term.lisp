;;;; Functions to control terminal colors & cursor

(in-package :chess-cl)

;;; Constant values

;; Colors
(defconstant +term-black+   30)
(defconstant +term-red+     31)
(defconstant +term-green+   32)
(defconstant +term-yellow+  33)
(defconstant +term-blue+    34)
(defconstant +term-magenta+ 35)
(defconstant +term-cyan+    36)
(defconstant +term-white+   37)

;; Attributes
(defconstant +term-none+       0)
(defconstant +term-bold+       1)
(defconstant +term-underscore+ 4)
(defconstant +term-blink+      5)

;;; Functions

(defun escape-string (str)
  "Helper that places 'Esc[' prefix before sequences"
  (format t "~c[~a" #\esc str))

(defun clear-screen ()
  "Clears the terminal & moves cursor to (0,0)"
  (escape-string "2J"))

(defun reset-color ()
  (escape-string "0m"))

(defun set-color (&key (attr nil) (fg nil) (bg nil))
  "Changes terminal attrs and colors"
  (escape-string (format nil "~@[~a~];~@[~a~];~@[~a~]m" attr fg (+ bg 10))))

(defun move-cursor (x y)
  "Moves the cursor to the given (x,y)"
  (escape-string (format nil "~a;~aH" y x)))
