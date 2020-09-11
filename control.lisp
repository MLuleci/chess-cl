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

(defun do-player-turn (player)
  "Do player turn i.e. execute input"
  (loop thereis (eval (get-player-input player))))

;; Executor functions
; All return t on success, nil otherwise

(defun move (piece square)
  "Execute a move"
  (let* ((file (cadr square))
         (rank (cadddr square))
         (seq (remove-if-not
              (lambda (obj) ; Filter those that can move
                (valid-move-p obj file rank))
              (apply #'find-piece piece))))
    (cond ((null seq) ; No match
           (format t "Invalid move, try again~%"))
          ((> (length seq) 1) ; Too many matches
           (format t "Ambiguous move, try again~%"))
          (t
           (with-slots (x-pos y-pos) (car seq)
             (setf x-pos file)
             (setf y-pos rank))))))

(defun capture (piece square)
  "Execute a capture"
  (let* ((file (cadr square))
         (rank (cadddr square))
         (seq (remove-if-not
               (lambda (obj)
                 (valid-capture-p obj file rank))
               (apply #'find-piece piece))))
    (cond ((null seq)
           (format t "Invalid capture, try again~%"))
          ((> (length seq) 1)
           (format t "Ambiguous capture, try again~%"))
          (t
           (let* ((attack (car seq))
                  (victim (car (find-piece :file file :rank rank)))
                  (player (player-with-color (piece-color attack)))
                  (versus (player-with-color (piece-color victim))))
             #| 1. Increment attacker's score
              | 2. Decrement versus' score
              | 3. Remove captured piece from board
              | 4. Move attacking piece
             |#
             (incf (player-score player) (piece-value victim))
             (decf (player-score versus) (piece-value victim))
             (delete victim (gethash (type-of victim) (player-pieces versus)))
             (with-slots (x-pos y-pos) attack
               (setf x-pos file)
               (setf y-pos rank)))))))

(defun promotion (square name)
  "Execute a promotion"
  (let* ((file (cadr square))
         (rank (cadddr square))
         (type (cadr name))
         (seq (remove-if-not
               (lambda (obj)
                 (valid-promotion-p obj file rank type))
               (find-piece :kind 'pawn))))
    (cond ((null seq)
           (format t "Invalid promotion, try again~%"))
          ((> (length seq) 1)
           (format t "Ambiguous promotion, try again~%"))
          (t
           (let* ((pawn (car seq))
                  (color (piece-color pawn))
                  (new (make-instance type :color color :x file :y rank))
                  (player (player-with-color color)))
             #| 1. Increment player's score
              | 2. Remove pawn from board
              | 3. Add promoted piece to board
             |#
             (incf (player-score player) (- (piece-value new) 1))
             (delete pawn (gethash 'pawn (player-pieces player)))
             (push new (gethash type (player-pieces player))))))))

(defun castle (side)
  "Execute a castle"
  #| TODO! |#)
