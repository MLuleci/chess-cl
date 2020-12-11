;;;; Game controller methods

(in-package :chess-cl)

;;; Global variables

(defparameter *turn-number* 0) ; Keeps track of half-turns i.e. just one player's go

; fifty- & seventy-five-move rules (100 & 150 half-turns respectively)
; https://en.wikipedia.org/wiki/Fifty-move_rule
; https://en.wikipedia.org/wiki/Seventy-five-move_rule
(defparameter *last-capture* nil)
(defparameter *last-pawn-move* nil)

; Player who has resigned oxr offered a draw
(defparameter *resigned* nil)
(defparameter *draw-offer* nil)

;;; Helpers

(defun get-current-player ()
  "Return the player whose turn it is"
  (if (evenp *turn-number*)
      *white-player*
      *black-player*))

(defun get-current-color ()
  "Return the color of the current player"
  (if (evenp *turn-number*)
      'white
      'black))

(defun fifty-move-p ()
  "Test for the fifty-move rule; enables either player to claim a draw"
  (or (> (- *turn-number* *last-capture*) 100)
      (> (- *turn-number* *last-pawn-move*) 100)))

(defun seventy-five-move-p ()
  "Test for the fifty-move rule; enables either player to claim a draw"
  (when (or (> (- *turn-number* *last-capture*) 150)
            (> (- *turn-number* *last-pawn-move*) 150))
    (setq *draw-offer* t)))

(defun offer-draw ()
  "Prompt player for a draw (if offered)"
  (when *draw-offer*
    (yes-or-no-p "Accept draw offer?")))

(defun game-over-p ()
  "Test if the game is over by any means"
  (or (checkmate-p (get-current-player))
      (offer-draw)
      (and *draw-offer* (fifty-move-p)) ; Draw is claimed, not offered
      (seventy-five-move-p)
      *resigned*))

(defun checked-p (player)
  "Test if given player is in check"
  (let ((king (car (find-piece :kind 'king :color (player-color player)))))
    (not (null (remove-if-not
                (lambda (enemy)
                  (valid-capture-p enemy (file king) (rank king)))
                (find-piece :color (opposite-color player)))))))

; Valid move checks should be done already
(defgeneric try-move (obj x y)
  (:documentation "Try and perform a move, undo if not possible; returns boolean")
  (:method ((obj piece) x y)
    (let ((oldx (piece-file obj))
          (oldy (piece-rank obj)))
      (setb (player-board (get-current-player)) x y 1)
      (psetf (piece-file obj) x
             (piece-rank obj) y)
      (if (checked-p (get-current-player))
          (progn
            (setb (player-board (get-current-player)) x y 0)
            (psetf (piece-file obj) oldx
                   (piece-rank obj) oldy))
          (imply (typep obj 'pawn)
                 (setf *last-pawn-move* *turn-number*))))))

(defgeneric try-capture (obj x y)
  (:documentation "Try and perform a capture, undo if not possible; returns boolean")
  (:method ((obj piece) x y)
    (let* ((player (get-current-player))
          (versus (player-with-color (opposite-color player)))
          (victim (car (find-piece :file x :rank y :color (opposite-color player))))
          (oldx (piece-file obj))
          (oldy (piece-rank obj)))
      (when victim
        (delete victim (gethash (type-of victim) (player-pieces versus)))
        (if (try-move obj x y)
            (progn
              (incf (player-score player) (piece-value victim))
              (decf (player-score versus) (piece-value victim))
              (setf *last-capture* *turn-number*))
            (progn
              (push victim (gethash (type-of victim) (player-pieces versus)))
              nil)))))
  (:method ((obj pawn) x y)
    (if (null (find-piece :file x :rank y)) ; En-passant
        (let* ((player (get-current-player))
              (versus (player-with-color (opposite-color player)))
              (victim (car (find-piece :file x :rank (piece-y obj) :color (opposite-color player)))))
          (when victim
            (delete victim (gethash (type-of victim) (player-pieces versus)))
            (if (try-move obj x y)
                (progn
                  (incf (player-score player) (piece-value victim))
                  (decf (player-score versus) (piece-value victim))
                  (setf *last-capture* *turn-number*))
                (progn
                  (push victim (gethash (type-of victim) (player-pieces versus)))
                  nil))))
        (call-next-method))))

;;; Player action functions
; All return t on success, nil otherwise

(defun help ()
  (format t "All moves must be in algebraic notation: ~% ~
             [File]    [a-h] ~% ~
             [Rank]    [1-8] ~% ~
             [Square]  <File><Rank> ~% ~
             [Piece]   <Name>[<File>[<Rank>]] ~% ~
             [Move]    <Piece><Square> (e.g. Nb4)~% ~
             [Capture] <Piece>x<Square> (e.g. exd5)~% ~
             [Promote] <Square>=<Name> (e.g. e8=Q)~% ~
             [Castle]  O-O[-O] (e.g. O-O-O)~%~% ~
             Other commands: ~% ~
             help - display this message ~% ~
             draw - offer (or claim) a draw ~% ~
             resign - resign from the game~%"))

(defun draw ()
  "Set draw offering player"
  (setq *draw-offer* (get-current-player)))

(defun resign ()
  "Resign from the game"
  (setq *resigned* (get-current-player)))

(defun move (piece square)
  "Execute a move"
  (with-cons (file rank) square
    (let ((seq (remove-if-not
                (lambda (obj)
                  (valid-move-p obj file rank))
                (apply #'find-piece piece :color (get-current-color)))))
      (cond ((null seq) ; No match
             (format t "Invalid move, try again~%"))
            ((> (length seq) 1) ; Too many matches
             (format t "Ambiguous move, try again~%"))
            (t
             (unless (try-move (car seq) file rank)
               (format t "Can't move while in check, try again~%")))))))

(defun capture (piece square)
  "Execute a capture"
  (with-cons (file rank) square
    (let ((seq (remove-if-not
                (lambda (obj)
                  (valid-capture-p obj file rank))
                (apply #'find-piece piece :color (get-current-player)))))
      (cond ((null seq)
             (format t "Invalid capture, try again~%"))
            ((> (length seq) 1)
             (format t "Ambiguous capture, try again~%"))
            (t
             (unless (try-capture (car seq) file rank)
               (format t "Can't capture while in check, try again~%")))))))

(defun promotion (square name)
  "Execute a promotion"
  (with-cons (file rank) square
    (let* ((type (cadr name))
           (color (get-current-color))
           (seq (remove-if-not
                 (lambda (obj)
                   (valid-promotion-p obj file rank))
                 (find-piece :kind 'pawn :color color))))
      (cond ((null seq)
             (format t "Invalid promotion, try again!%"))
            ((> (length seq) 1)
             (format t "Ambiguous promotion, try again!%"))
            (t
             (let ((pawn (car seq))
                   (player (get-current-player))
                   (new (make-instance type :color color :file file :rank rank)))
               (if (try-move pawn file rank) ; TODO: Should also allow for captures here
                   (progn
                     (delete pawn (gethash 'pawn (player-pieces player)))
                     (push new (gethash type (player-pieces player)))
                     (incf (player-score player) (- (piece-value new) 1))
                     (setf (piece-moved new) *turn-number*))
                   (format t "Can't promote while in check, try again~%"))))))))

(defun castle (side)
  "Execute a castle"
  (let* ((player (get-current-player))
         (king (car (find-piece :kind 'king :color (player-color player))))
         (rook (car (find-piece :kind 'rook :color (player-color player)
                                :file (if (eq side 'king-side) 7 0)))))
    (when (and king (-ve (piece-last-moved king))
               rook (-ve (piece-last-moved rook))

                   

;;; Game logic functions

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
  "Do one half-turn"
  (loop thereis (eval (get-player-input player))))

(defun main-loop ()
  "Main game loop"
  (loop until (game-over-p)
     do (draw-board)
       (do-player-turn (get-current-player))
       (incf *turn-number*))
  (cond (*resigned*
         (format t "~a has resigned~%" (player-color *resigned*)))
        (*draw-offer*
         (format t "Game ended in a draw~%"))
        (t
         (format t "Checkmate, ~a wins!~%"
                 (if (evenp *turn-number*)
                     'black
                     'white)))))
