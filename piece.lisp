;;;; Chess piece classes & their methods

(in-package :chess-cl)

;;; Class definitions

;; Base class
(defclass piece ()
  ((color :reader piece-color
          :initarg :color
          :type keyword
          :documentation "Piece color, either black or white")
   (char :reader piece-char
         :type character
         :documentation "Piece representation, unicode U+2654 through U+265F")
   (x-pos :accessor piece-x
          :initarg :x
          :type integer
          :documentation "Piece x position i.e. its file")
   (y-pos :accessor piece-y
          :initarg :y
          :type integer
          :documentation "Piece y position i.e. its rank")
   (last-moved :accessor piece-moved
               :initform nil
               :type integer
               :documentation "Turn number when piece was last moved")
   (value :reader piece-value
          :type integer
          :documentation "Relative value of the piece"))
  (:documentation "Base piece class"))

;; Subclasses
(defclass king (piece) ())
(defclass queen (piece) ())
(defclass rook (piece) ())
(defclass bishop (piece) ())
(defclass knight (piece) ())
(defclass pawn (piece) ())

;;; Method definitions

;; General initialize method
(defmethod initialize-instance :before
    ((obj piece) &key (color nil colorp) x y &allow-other-keys)
  "Ensure :color is supplied and valid"
  (cond ((not colorp)
         (error "Cannot construct ~S without given :color" (type-of obj)))
        ((not (valid-color-p color))
         (error "Invalid :color ~S, must be either BLACK or WHITE" color))
        ((not (valid-position-p x y))
         (error "Invalid position (~S,~S)" x y))))

;; Specific initialize methods
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
    (setf value 1)))

;; `print-object' implementation for `piece' class
(defmethod print-object ((obj piece) stream)
  "Print piece objects; obeys printer directives"
  (cond (*print-readably*
         (error (make-condition 'print-not-readable :object obj)))
        (*print-escape*
         (print-unreadable-object (obj stream :type t :identity t)
           (format stream "~a,~a" (piece-x obj) (piece-y obj))))
        (*print-pretty*
         (format stream "~c" (piece-char obj)))))

;; Valid move checks
(defmethod valid-move-p :before ((obj piece) x y)
  (unless (not (valid-position-p x y))
      (call-next-method)))

(defmethod valid-move-p ((obj king) x y)
  (with-slots (x-pos y-pos) obj
    (let ((dx (abs (- x x-pos)))
          (dy (abs (- y y-pos))))
      (and (<= dx 1) (<= dy 1)))))

(defmethod valid-move-p ((obj queen) x y)
  (with-slots (x-pos y-pos) obj
    (let ((dx (abs (- x x-pos)))
          (dy (abs (- y y-pos))))
      (or (= dx 0)
          (= dy 0)
          (= dx dy)))))

(defmethod valid-move-p ((obj rook) x y)
  (with-slots (x-pos y-pos) obj
    (let ((dx (abs (- x x-pos)))
          (dy (abs (- y y-pos))))
      (or (= dx 0) (= dy 0)))))

(defmethod valid-move-p ((obj bishop) x y)
  (with-slots (x-pos y-pos) obj
    (let ((dx (abs (- x x-pos)))
          (dy (abs (- y y-pos))))
      (= dx dy)))) ; May move diagonally

(defmethod valid-move-p ((obj knight) x y)
  (with-slots (x-pos y-pos) obj
    (let ((dx (abs (- x x-pos)))
          (dy (abs (- y y-pos))))
      (or (and (= dx 2) (= dy 1))
          (and (= dx 1) (= dy 2))))))
      
(defmethod valid-move-p ((obj pawn) x y)
  (with-slots (x-pos y-pos color) obj
    (let ((dx (abs (- x x-pos)))
          (dy (abs (- y y-pos))))
      (and (<= dx 1) ; Assuming capture checks have already passed
           (if (eq color 'white) ; Assuming white pawns travel upwards
               (> dy 0)
               (< dy 0))))))

;; General move method
(defmethod move-piece ((obj piece) x y)
  "Move a piece by updating its x- and y-coordinates"
  (when (valid-move-p obj x y)
    (with-slots (x-pos y-pos) obj
      (setf x-pos x)
      (setf y-pos y))
    obj)) ; Return the modified object on success, nil otherwise
