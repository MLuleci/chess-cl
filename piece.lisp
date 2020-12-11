;;;; Chess piece classes & their methods

(in-package :chess-cl)

;;; Class definitions

;; Base class
(defclass piece ()
  ((color :reader piece-color
          :initarg :color
          :type keyword
          :documentation "Color - 'black or 'white")
   (char :reader piece-char
         :type character
         :documentation "Character representation, unicode U+2654 through U+265F")
   (file :accessor piece-file
          :initarg :file
          :type integer
          :documentation "Current file")
   (rank :accessor piece-rank
          :initarg :rank
          :type integer
          :documentation "Current rank")
   (value :reader piece-value
          :type integer
          :documentation "Relative value")
   (last-moved :accessor piece-last-moved
               :initform -1
               :type integer
               :documentation "Turn number when piece was last moved"))
  (:documentation "Base piece class"))

;; Subclasses
(defclass king (piece) ())
(defclass queen (piece) ())
(defclass rook (piece) ())
(defclass bishop (piece) ())
(defclass knight (piece) ())
(defclass pawn (piece)
  ((jumped? :accessor pawn-jumped?
                :initform nil
                :type boolean
                :documentation "Whether pawn has double-stepped")))

;;; Helpers

(defmacro if-color (obj white-clause black-clause)
  "Execute a branch depending on piece color"
  `(if (eq (piece-color ,obj) 'white)
       ,white-clause
       ,black-clause))

(defgeneric opposite-color (obj)
  (:documentation "Return the opposite color given piece or player")
  (:method ((obj piece)) (if-color obj 'black 'white)))

(defmacro with-delta ((obj x y) &body body)
  "Helper that binds dx and dy inside body given x, y, and piece"
  `(with-slots (file rank) ,obj
     ,(append `(let ((dx (abs- ,x file))
                     (dy (abs- ,y rank))))
              ,body)))

(defmacro gen-board (seq &body body)
  "Generate board given pairs and a predicate, assumes context has `obj' defined"
  `(let ((board (make-bitboard)))
     (with-slots (file rank) obj
       (dolist (pair ,seq)
         (with-cons (dx dy) pair
           ,(car body))))
       board))

; Shorthand, used in combination below
(defvar cross-moves '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
(defvar diag-moves '((1 . 1) (-1 . 1) (1 . -1) (-1 . -1)))

;;; Method definitions

;; General initialize method
(defmethod initialize-instance :around
    ((obj piece) &key (color nil colorp) file rank &allow-other-keys)
  (cond ((not colorp)
         (error "Cannot construct ~s without given :color" (type-of obj)))
        ((not (valid-color-p color))
         (error "Invalid :color ~s, must be either BLACK or WHITE" color))
        ((not (valid-position-p file rank))
         (error "Invalid position (~s,~s)" file rank))
        (t
         (call-next-method))))

(defmethod initialize-instance :after ((obj king) &key &allow-other-keys)
  (with-slots (color char value) obj
    (setf char (if (eq color 'white) #\♔ #\♚))
    (setf value 0))) ; King is valued at 0 as an exception

(defmethod initialize-instance :after ((obj queen) &key &allow-other-keys)
  (with-slots (color char value) obj
    (setf char (if (eq color 'white) #\♕ #\♛))
    (setf value 9)))

(defmethod initialize-instance :after ((obj rook) &key &allow-other-keys)
  (with-slots (color char value) obj
    (setf char (if (eq color 'white) #\♖ #\♜))
    (setf value 5)))

(defmethod initialize-instance :after ((obj bishop) &key &allow-other-keys)
  (with-slots (color char value) obj
    (setf char (if (eq color 'white) #\♗ #\♝))
    (setf value 3)))

(defmethod initialize-instance :after ((obj knight) &key &allow-other-keys)
  (with-slots (color char value) obj
    (setf char (if (eq color 'white) #\♘ #\♞))
    (setf value 3)))

(defmethod initialize-instance :after ((obj pawn) &key &allow-other-keys)
  (with-slots (color char value) obj
    (setf char (if (eq color 'white) #\♙ #\♟))
    (setf value 1))))

;; `print-object' implementation for `piece' class
(defmethod print-object ((obj piece) stream)
  "Print piece objects; obeys printer directives"
  (cond (*print-readably*
         (error (make-condition 'print-not-readable :object obj)))
        (*print-escape*
         (print-unreadable-object (obj stream :type t :identity t)
           (format stream "~a,~a" (piece-file obj) (piece-rank obj))))
        (*print-pretty*
         (format stream "~c" (piece-char obj)))))

;; Valid delta predicates
(defgeneric valid-delta-p (obj x y)
  (:documentation "Test if given square could be reached by given piece")
  (:method ((obj king) x y)
    (with-delta (obj x y)
      (and (<= dx 1) (<= dy 1))))
  
  (:method ((obj queen) x y)
    (with-delta (obj x y)
      (or (= dx 0)
          (= dy 0)
          (= dx dy))))
  
  (:method ((obj rook) x y)
    (with-delta (obj x y)
      (or (= dx 0) (= dy 0))))
  
  (:method ((obj bishop) x y)
    (with-delta (obj x y)
      (= dx dy)))
  
  (:method ((obj knight) x y)
    (with-delta (obj x y)
      (or (and (= dx 2) (= dy 1))
          (and (= dx 1) (= dy 2)))))

  (:method ((obj pawn) x y)
    (with-delta (obj x y)
      (and (<= dx 1)
           (if (pawn-jumped? obj)
               (= dy 1)
               (<= dy 2))
           (funcall (if-color obj #'+ve #'-ve)
                    (- y (piece-rank obj)))))))

;; Piece delta getters
(defgeneric get-delta (obj)
  (:documentation "Return list of directions given piece can move in")
  (:method ((obj king))
    (append cross-moves diag-moves))
  
  (:method ((obj queen))
    (append cross-moves diag-moves))
  
  (:method ((obj rook))
    cross-moves)
  
  (:method ((obj bishop))
    diag-moves)
  
  (:method ((obj knight))
    '((2 . 1) (-2 . 1) (2 . -1) (-2 . -1)
      (1 . 2) (-1 . 2) (1 . -2) (-1 . -2))))

;; Move predicate
(defun valid-move-p (obj x y)
  (declare (ftype (function (piece integer integer) boolean)))
  (check-board
   (gen-board (if (typep obj 'pawn)
                  '((0 . 1) (0 . -1) (0 . 2) (0 . -2))
                  (get-delta obj))
              (loop with nx = (+ file dx) and ny = (+ rank dy)
                    while (and (valid-position-p nx ny)
                               (valid-delta-p obj nx ny)
                               (empty-square-p nx ny))
                    do (setb board nx ny 1)
                       (incf nx dx)
                       (incf ny dy)))
   x y))

;; Capture predicate
(defun valid-capture-p (obj x y)
  (declare (ftype (function (piece integer integer) boolean)))
  (check-board
   (gen-board (if (typep obj 'pawn)
                  diag-moves
                  (get-delta obj))
              (loop with nx = (+ file dx) and ny = (+ rank dy)
                    while (and (valid-position-p nx ny)
                               (valid-delta-p obj nx ny)
                               (empty-square-p nx ny (piece-color obj)))
                    do (when (not (empty-square-p nx ny (opposite-color obj)))
                         (setb board nx ny 1))
                       (incf nx dx)
                       (incf ny dy)))
   x y))

;; Promotion predicate
#| TODO: Just realised promotions can be made together with captures
 | i.e. exd8=Q is a valid promotion. That's easy to implement here
 | (and currently is) but `parse.lisp' has to be updated.
|#
(defun valid-promotion-p (obj x y)
  (declare (ftype (function (pawn integer integer) boolean)))
  (and (typep obj 'pawn)
       (= y (if-color obj 7 0))
       (or (valid-move-p obj x y)
           (valid-capture-p obj x y))))
