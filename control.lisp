;;;; Game controller methods

(in-package :chess-cl)

;;; Global variables
(defparameter *turn-number* 0) ; Keeps track of half-turns i.e. just one player's go

; fifty- & seventy-five-move rules (100 & 150 half-turns respectively)
; https://en.wikipedia.org/wiki/Fifty-move_rule
; https://en.wikipedia.org/wiki/Seventy-five-move_rule
(defparameter *last-capture* nil)
(defparameter *last-pawn-move* nil)

; Player who has resigned or offered a draw
(defparameter *resigned* nil)
(defparameter *draw-offer* nil)

;;; Generic methods
; All return t on success, nil otherwise

;; Move methods
; Tests if this move would cause player to be in check, undos if that's the case
(defmethod move-piece ((obj piece) x y)
  "Move a piece"
  (with-slots (x-pos y-pos last-moved) obj
    (let ((prev-x x-pos)
          (prev-y y-pos))
      (setf x-pos x
            y-pos y)
      (if (checked-p (get-current-player))
          (psetf x-pos prev-x
                 y-pos prev-y)
          (setf last-moved *turn-number*)))))

(defmethod move-piece ((obj pawn) x y)
  "Move a pawn"
  (let ((double? (= (abs- (piece-y obj) y) 2)))
    (when (call-next-method)
      (when double?
        (setf (pawn-double-step obj) t))
      (setf *last-pawn-move* *turn-number*))))
  
;; Capture methods
(defmethod capture-piece ((obj piece) x y)
  "Capture a piece"
  (let ((player (player-with-color (piece-color obj)))
        (versus (player-with-color (piece-color victim)))
        (victim (car (find-piece :file x :rank y))))
    #| 1. Remove captured piece from board
     | 2. Move attacking piece
     | -- test for check --
     | 3. Increment attacker's score
     | 4. Decrement versus' score
     | 5. Set *last-capture*
    |#
    (delete victim (gethash (type-of victim) (player-pieces versus)))
    (if (move-piece obj x y)
        (progn
          (incf (player-score player) (piece-value victim))
          (decf (player-score versus) (piece-value victim))
          (setf *last-capture* *turn-number*))
        (progn
          (push victim (gethash (type-of victim) (player-pieces versus)))
          (return nil)))))

(defmethod capture-piece ((obj pawn) x y)
  "Capture a/by pawn (possibly en-passant)"
  (if (null (find-piece :file x :rank y)) ; En-passant
      (let ((player (player-with-color (piece-color obj)))
            (versus (player-with-color (piece-color victim)))
            (victim (car (find-piece :file x :rank (piece-y obj)))))
        (delete victim (gethash (type-of victim) (player-pieces versus)))
        (if (move-piece obj x y)
            (progn
              (incf (player-score player) (piece-value victim))
              (decf (player-score versus) (piece-value victim))
              (setf *last-capture* *turn-number*))
            (progn
              (push victim (gethash (type-of victim) (player-pieces versus)))
              (return nil))))
      (call-next-method)))

;;; Helpers

(defun get-current-player ()
  "Return the player whose turn it is"
  (if (evenp *turn-number*)
      *white-player*
      *black-player*))

(defun checked-p (player)
  "Test if given player is in check"
  (let* ((color (player-color player))
         (king (car (find-piece :kind 'king :color color)))
         (file (piece-x king))
         (rank (piece-y king)))
    (block test
      (iterate-pieces
       (lambda (obj)
         (when (valid-capture-p obj file rank)
           (return-from test t)))))))

(defun checkmate-p (player)
  "Test if given player has been mated"
  nil)
  #|
  (when (checked-p player) ; Must be checked to be mated
    #| TODO |#))|#

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

;;; Executor functions
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
  (let* ((color (player-color (get-current-player)))
         (file (cadr square))
         (rank (cadddr square))
         (seq (remove-if-not
               (lambda (obj) ; Filter those that can move
                 (and (eq (piece-color obj) color)
                      (valid-move-p obj file rank)))
               (apply #'find-piece piece))))
    (cond ((null seq) ; No match
           (format t "Invalid move, try again~%"))
          ((> (length seq) 1) ; Too many matches
           (format t "Ambiguous move, try again~%"))
          (t
           (unless (move-piece (car seq) file rank)
             (format t "Invalid move while in check, try again~%"))))))

(defun capture (piece square)
  "Execute a capture"
  (let* ((color (player-color (get-current-player)))
         (file (cadr square))
         (rank (cadddr square))
         (seq (remove-if-not
               (lambda (obj)
                 (and (eq (piece-color obj) color)
                      (valid-capture-p obj file rank)))
               (apply #'find-piece piece))))
    (cond ((null seq)
           (format t "Invalid capture, try again~%"))
          ((> (length seq) 1)
           (format t "Ambiguous capture, try again~%"))
          (t
           (unless (capture-piece (car seq) file rank)
             (format t "Invalid capture while in check, try again~%"))))))

(defun promotion (square name)
  "Execute a promotion"
  (let* ((color (player-color (get-current-player)))
         (file (cadr square))
         (rank (cadddr square))
         (type (cadr name))
         (seq (remove-if-not
               (lambda (obj)
                 (valid-promotion-p obj file rank type))
               (find-piece :kind 'pawn :color color))))
    (cond ((null seq)
           (format t "Invalid promotion, try again~%"))
          ((> (length seq) 1)
           (format t "Ambiguous promotion, try again~%"))
          (t
           (let* ((pawn (car seq))
                  (color (piece-color pawn))
                  (new (make-instance type :color color :x file :y rank))
                  (player (player-with-color color)))
             #| 1. Remove pawn from board
              | 2. Add promoted piece to board
              | -- test for check --
              | 3. Increment player's score
              | 4. Set last-moved on promoted piece
              | 5. Set *last-pawn-move*
             |#
             (delete pawn (gethash 'pawn (player-pieces player)))
             (push new (gethash type (player-pieces player)))
             (if (checked-p (get-current-player))
                 (progn
                   (push pawn (gethash 'pawn (player-pieces player)))
                   (delete new (gethash type (player-pieces player)))
                   (format t "Invalid promotion while in check, try again~%"))
                 (progn
                   (incf (player-score player) (- (piece-value new) 1))
                   (setf (piece-moved new) *turn-number*)
                   (setf *last-pawn-move* *turn-number*))))))))

; TODO: Optimize this
(defun castle (side)
  "Execute a castle"
  (let* ((player (get-current-player))
         (king (car (gethash 'king (player-pieces player))))
         (rook (car (remove-if
                     (lambda (obj)
                       (and (not (piece-moved obj))
                            (= (piece-x obj) (if (eq side 'king-side) 7 0))))
                     (gethash 'rook (player-pieces player))))))
    (if (and rook
             (not (piece-moved king))
             (valid-path-p king ; King's path is empty
                           (if (eq side 'king-side)
                               (1- (piece-x rook))
                               (1+ (piece-x rook)))
                           (piece-y rook))
             (not (block test ; King not checked or crossing/ending on an attacked square
                    (iterate-pieces
                     (lambda (obj)
                       (when (and (not (eq (piece-color obj) (piece-color king)))
                                  #| Iterate over every square between king and 
                                   | rook (inclusive) checking for possible captures
                                   |#
                                  (loop with dx = (signum (- (piece-x rook) (piece-x king)))
                                     and x = (piece-x king)
                                     and y = (piece-y king)
                                     thereis (or (valid-move-p obj x y)
                                                 (= (- x dx) (piece-x rook)))
                                     do (incf x dx)
                                     finally (return nil)))
                         (return-from test t)))))))
        (let ((kx (piece-x king))
              (ky (piece-y king))
              (rx (piece-x rook))
              (ry (piece-y rook)))
          (move-piece king rx ry)
          (move-piece rook kx ky))
        (format t "Cannot castle ~a, try again~%" side))))

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
