;;;; Game controller methods

(in-package :chess-cl)

;;; Functions

(defun draw-board ()
  (loop for y from 7 downto 0
     do (format t "~a " (1+ y))
       (dotimes (x 8)
         (if (evenp (+ x y))
             (set-color :fg +term-white+ :bg +term-black+)
             (set-color :fg +term-black+ :bg +term-white+))
         (let ((seq (find-piece :file x :rank y)))
           (format t "~a " (if seq (car seq) #\space))))
       (reset-color)
       (format t "~%"))
  (format t "  a b c d e f g h~%"))

(defun do-color-turn (color)
  "Do one turn of the player with given color"
  (let* ((player (player-with-color color))
         (input (get-player-input player)))
    ; TODO
    
