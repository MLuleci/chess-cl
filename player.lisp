;;;; Chess player class

(in-package :chess-cl)

;;; Class definition

(defclass player ()
  ((color :reader player-color
          :initarg :color
          :type keyword
          :documentation "Player color, either black or white")
   (score :accessor player-score
          :initform 0
          :type integer
          :documentation "Player score")
   (pieces :accessor player-pieces
           :initform (make-hash-table)
           :type hash-table
           :documentation "Player's pieces, (key=piece type, value=list)"))
  (:documentation "Player class"))

;;; AI subclass

(defclass ai-player (player) ())

;;; Global variables

(defparameter *white-player* nil)
(defparameter *black-player* nil)

;;; Method definitions

;; Initializer methods
(defmethod initialize-instance :before ((p player) &key (color nil colorp))
  "Ensure :color is supplied and valid"
  (cond ((not colorp)
         (error "Cannot construct ~S without given :color" (type-of p)))
        ((not (valid-color-p color))
         (error "Invalid :color ~S, must be either BLACK or WHITE" color))))

(defmethod initialize-instance :after ((p player) &key color)
  "Populate player's hash table with pieces"
  (with-slots (pieces) p
    (flet ((add-piece (type x y)
             "Helper for constructing/placing pieces"
             (let ((new (make-instance type :color color :x x :y y)))
               (push new (gethash type pieces))
               (incf (player-score p) (piece-value new))))
      (let ((main-rank (if (eq color 'white) 0 7))
            (pawn-rank (if (eq color 'white) 1 6)))
        (add-piece 'king 4 main-rank)
        (add-piece 'queen 3 main-rank)
        (add-piece 'rook 0 main-rank)
        (add-piece 'rook 7 main-rank)
        (add-piece 'bishop 2 main-rank)
        (add-piece 'bishop 5 main-rank)
        (add-piece 'knight 1 main-rank)
        (add-piece 'knight 6 main-rank)
        (dotimes (x 8)
          (add-piece 'pawn x pawn-rank))))))

;; Input getting method(s)
(defgeneric get-player-input (p)
  (:documentation "Get input from player, must return a valid parsed tree"))

(defmethod get-player-input ((p player))
  "Prompt player until they input a valid move"
  (loop do (format t "> ")
       (finish-output)
       (let ((tree (caar ; First tree w/ best parsing
                    (remove-if
                     (lambda (pair) 
                       (or (null (car pair)) ; No tree
                           (not (emptyp (cdr pair))))) ; Scrap input
                     (parse-start (read-line))))))
         (if tree
             (return tree)
             (format t "Invalid input, try again~%")))))

; TODO: Later to be moved to its own file
(defmethod get-player-input ((p ai-player))
  "Allow AI to play its turn"
  (format t "AI not implemented!~%")
  (call-next-method))

; Should be in `piece.lisp' but needs to be here
(defmethod valid-move-p ((obj piece) x y)
  "Test if a move is valid for given piece"
  (and (valid-position-p x y)
       (valid-delta-p obj x y)
       (valid-path-p obj x y)))

(defmethod valid-capture-p ((obj piece) x y)
  "Test if a capture is valid for given piece"
  (and (valid-position-p x y)
       (valid-delta-p obj x y)
       (let ((dx (signum (- x (piece-x obj))))
             (dy (signum (- y (piece-y obj)))))
         #| Validate path up-to (but excluding) destination
          | square which must have an enemy piece on it.
         |#
         (valid-path-p obj (- x dx) (- y dy)))
       (let ((victim (car (find-piece :file x :rank y))))
         (and victim ; There is a piece to capture...
              (not (eq (piece-color obj) ; ...that's an enemy
                       (piece-color victim)))))))

(defmethod valid-promotion-p ((obj piece) x y new-kind)
  (and (valid-move-p obj x y)
       (subtypep new-kind 'piece)))

;;; Helper functions

(defun player-with-color (color)
  "Returns global player object matching given color"
  (when (valid-color-p color)
    (if (eq color 'white)
        *white-player*
        *black-player*)))

(defun iterate-hash-tables (fn &rest rest)
  "Apply fn to every (key,value) pair in the tables given by rest"
  (dolist (table rest)
    (maphash fn table)))

(defmacro iterate-pieces (fn)
  "Apply fn to every piece on the board"
  `(iterate-hash-tables
    (lambda (key value)
      (declare (ignore key))
      (dolist (piece value)
        (funcall ,fn piece)))
    (player-pieces *white-player*)
    (player-pieces *black-player*)))

(defun find-piece
    (&key (color nil colorp) (kind nil kindp) (rank nil rankp) (file nil filep))
  "Find piece(s) matching the given parameters"
  (let ((seq nil))
    (iterate-pieces
     (lambda (p)
       (with-slots (x-pos y-pos (col color)) p
         (when (and (imply colorp (eq col color))
                    (imply kindp (eq (type-of p) kind))
                    (imply rankp (eq y-pos rank))
                    (imply filep (eq x-pos file)))
           (push p seq)))))
    seq))

(defun valid-path-p (obj x y)
  "Iterate over every piece, checking for blockages in the path"
  (declare (ftype (function (piece real real) boolean)))
  (let* ((path (make-path (piece-x obj) (piece-y obj) x y))
         (copy (make-bitboard path)))
    (iterate-pieces
     (lambda (p)
       (unless (eq p obj)
         (setb copy (piece-x p) (piece-y p) 0))))
      (equal path copy)))

(defun make-path (x0 y0 x1 y1)
  "Draw a path between two points on a bitboard"
  ; https://en.wikipedia.org/wiki/Bresenham%27s_line_algorithm
  (flet ((plot-low (x0 y0 x1 y1 plot-fn)
           (let* ((dx (- x1 x0))
                  (dy (abs (- y1 y0)))
                  (yi (signum (- y1 y0)))
                  (D (- (* 2 dy) dx))
                  (y y0))
             (loop for x from x0 to x1
                do (funcall plot-fn x y)
                  (when (> D 0)
                    (incf y yi)
                    (decf D (* 2 dx)))
                  (incf D (* 2 dy)))))
         (plot-high (x0 y0 x1 y1 plot-fn)
           (let* ((dx (abs (- x1 x0)))
                  (dy (- y1 y0))
                  (xi (signum (- x1 x0)))
                  (D (- (* 2 dx) dy))
                  (x x0))
             (loop for y from y0 to y1
                do (funcall plot-fn x y)
                  (when (> D 0)
                    (incf x xi)
                    (decf D (* 2 dy)))
                  (incf D (* 2 dx))))))
    (let* ((board (make-bitboard))
           (fn (lambda (x y) (setb board x y 1))))
      (if (< (abs (- y1 y0)) (abs (- x1 x0)))
          (if (> x0 x1)
              (plot-low x1 y1 x0 y0 fn)
              (plot-low x0 y0 x1 y1 fn))
          (if (> y0 y1)
              (plot-high x1 y1 x0 y0 fn)
              (plot-high x0 y0 x1 y1 fn)))
      board)))
