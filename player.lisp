;;;; Chess player class

(in-package :chess-cl)

;;; Class definitions

;; Base class
(defclass player ()
  ((color :reader player-color
          :initarg :color
          :type keyword
          :documentation "Player color, either black or white")
   (score :accessor player-score
          :initform 39
          :type integer
          :documentation "Player score")
   (pieces :accessor player-pieces
           :initform (make-hash-table)
           :type hash-table
           :documentation "Player's pieces, (key=piece type, value=list)")
   (board :accessor player-board
          :initform (make-bitboard)
          :type bitboard
          :documentation "Player's board (1=piece, 0=empty)"))
  (:documentation "Player class"))

;; AI subclass
(defclass ai-player (player) ())

;;; Global variables

(defparameter *white-player* nil)
(defparameter *black-player* nil)

;;; Helpers

(defun enemies-p (a b)
  "Test if two pieces are enemies"
  (and (typep a 'piece)
       (typep b 'piece)
       (not (eq (piece-color a) (piece-color b)))))

(defgeneric player-with-color (v)
  (:documentation "Return player with color deduced from either a symbol or piece")
  (:method ((color symbol))
    (when (valid-color-p color)
      (if (eq color 'white)
          *white-player*
          *black-player*)))
  (:method ((obj piece))
    (player-with-color (piece-color obj))))

(defmethod opposite-color ((obj player))
  (if (eq (player-color obj) 'white)
      'black
      'white))

(defun empty-square-p (x y &optional (color nil))
  "Test if a square is empty on given color board (both players used if none given)"
  (if color
      (= (getb (player-board (player-with-color color)) x y) 0)
      (= (getb (player-board *white-player*) x y)
         (getb (player-board *black-player*) x y)
         0)))

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
          (with-slots ((x file) (y rank) (c color)) p
              (when (and (imply colorp (eq color c))
                         (imply kindp (eq kind (type-of p)))
                         (imply filep (eq file x))
                         (imply rankp (eq rank y)))
                (push p seq)))))
    seq))

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
  (with-slots (pieces board) p
    (flet ((add-piece (type x y)
             "Helper for constructing/placing pieces"
             (push (make-instance type :color color :file x :rank y) (gethash type pieces))
             (setb board x y 1)))
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
  (:documentation "Get input from player, must return a valid parsed tree")
  (:method ((p player))
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
  (:method ((p ai-player))
    "Allow AI to play its turn"
    (format t "AI not implemented!~%")
    (call-next-method)))
