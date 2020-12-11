;;;; Helper functions

(in-package :chess-cl)

;;; Definitions

(defun valid-color-p (color)
  "Test if color is valid, meaning either black or white"
  (or (eql color 'white)
      (eql color 'black)))

(defun valid-position-p (x y)
  "Test if a position is valid i.e. within the board bounds"
  (not (or (< x 0) (> x 7)
           (< y 0) (> y 7))))

(defmethod emptyp ((x sequence))
  "Test if a sequence is empty"
  (= (length x) 0))

(defun prompt-choices (&rest seq)
  "Prompt user with a list of choices, return chosen index"
  (loop do
     (loop for i from 0 and str in seq
        do (format t "~a. ~a~%" i str)
        finally (format t "> "))
     (finish-output)
     (let ((in (parse-integer (read-line) :junk-allowed t)))
       (if (and in (>= in 0) (< in (length seq)))
           (return in)
           (format t "Invalid input, try again~%")))))

(defun xor (&rest vars)
  "Logical XOR operator, remember that XOR is associative"
  (let ((out (car vars))) ; out := vars[0]
    (dolist (i (cdr vars) out) ; for i in vars[1..]
      (setf out (or (and (not i) out)
                    (and i (not out))))))) ; out := vars[i] xor vars[i-1]

(defun +ve (arg)
  (> arg 0))

(defun -ve (arg)
  (< arg 0))

(defun imply (ant con)
  "Logical IMPLY operator; ant -> con"
  (if ant con t))

(defun mapf (fn seq)
  "Apply mapcon to each subsequence, like mapcon + maplist"
  (mapcon (lambda (subsq)
            (let ((head (car subsq)))
              (if (consp head)
                  (mapf fn head)
                  (funcall fn subsq))))
          seq))

(defun flatten (seq)
  "Flatten nested lists (non-destructive)"
  (when seq
    (if (consp (car seq))
        (append (flatten (car seq))
                (flatten (cdr seq)))
        (cons (car seq)
              (flatten (cdr seq))))))

(deftype bitboard ()
  `(bit-vector 64))

(defun make-bitboard (&optional (other nil otherp))
  "Create an empty (or copy) bit board"
  (declare (ftype (function () bitboard)))
  (if otherp
      (make-array 64 :element-type 'bit :fill-pointer t :initial-contents other)
      (make-array 64 :element-type 'bit :fill-pointer t)))

(defun setb (board x y bit)
  "Set square value on bitboard specified by subscripts"
  (when (and (typep board 'bitboard)
             (valid-position-p x y))
    (setf (bit board (+ x (* y 8))) bit)))

(defun getb (board x y)
  "Get square value on bitboard specified by subscripts"
  (when (and (typep board 'bitboard)
             (valid-position-p x y))
    (bit board (+ x (* y 8)))))

(defun check-board (board x y)
  "Test if board(x, y) = 1"
  (= (getb board x y) 1))

(defun abs- (&rest numbers)
  "Calculate absolute difference"
  (abs (apply #'- numbers)))

(defun  print-bitboard (board)
  (dotimes (y 8)
    (dotimes (x 8)
      (format t "~a" (getb board x (- 7 y))))
    (format t "~%")))

(defmacro with-cons ((left right) pair &body body)
  "Unpack a cons and bind its (car . cdr) to (left right) respectively"
  (append `(let ((,left (car ,pair)) (,right (cdr ,pair))))
          body))
