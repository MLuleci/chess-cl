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
             (push (make-instance type :color color :x x :y y)
                   (gethash type pieces))))
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

(defgeneric get-player-input (p)
  (:documentation "Get input from player, should return a parsed tree"))

(defmethod get-player-input ((p player))
  "Prompt player until valid parsed input"
  (loop do (format t "> ")
       (finish-output)
       (let ((trees (remove-if (lambda (pair)
                                 (or (null (car pair)) ; No tree
                                     (not (emptyp (cdr pair))))) ; Scrap input
                               (parse-start (read-line)))))
         (if trees
             (return (caar trees)) ; Return first tree w/ best parsing
             (format t "Invalid input, try again~%")))))

; Later to be moved to its own file
(defmethod get-player-input ((p ai-player))
  "Allow AI to play its turn"
  (format t "AI not implemented!")
  (call-next-method))

(defun player-with-color (color)
  "Helper returns global player object given color"
  (when (valid-color-p color)
    (if (eq color 'white)
        *white-player*
        *black-player*)))

(defun find-piece
    (&key (color nil colorp) (kind nil kindp) (rank nil rankp) (file nil filep))
  "Find piece(s) matching the given parameters"
  (flet ((search-table (table)
           (with-hash-table-iterator (next-entry table)
             (loop with seq = nil
                do (multiple-value-bind (more? key value) (next-entry)
                     (unless more? (return seq))
                     (dolist (piece value)
                       (with-slots (x-pos y-pos) piece
                         (when (and (imply kindp (eql kind key))
                                    (imply rankp (eql y-pos rank))
                                    (imply filep (eql x-pos file)))
                           (push piece seq)))))))))
    (if colorp
        (search-table (slot-value (player-with-color color) 'pieces))
        (append (search-table (slot-value *black-player* 'pieces))
                (search-table (slot-value *white-player* 'pieces))))))
