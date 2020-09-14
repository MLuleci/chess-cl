;;;; Parsing functions

(in-package :chess-cl)

#|
 | Parsers are functions which return a list of pairs given
 | some input string. The pairs consist of a some type A and 
 | any unconsumed input like (A,String). An empty list of pairs
 | i.e. nil indicates the parser failed to make any sense of
 | the input. Multiple pairs returned indicates input was 
 | ambiguous.
 |
 | More rigorously, this is a monadic parser adapted from the 
 | paper that can be found here:
 | http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf
 | 
 | Input alphabet (Γ):
 | K, Q, R, B, N, a-h, 1-8, O, x, -, =, +, #, *, $, ?
 | 'x' - Captures e.g. exd5
 | '-' - Castling e.g. O-O-O
 | '=' - Promotions e.g. e8=Q
 | '+' - Check e.g. Nc6+
 | '#' - Checkmate e.g. e8=Q#
 | '*' - Draw or forfeit
 | '$' - End Of Input (EOI) *not actually typed
 | '?' - Help
 | Ignored symbols which should be stripped: {+,#,*,?}
 |
 | Stack alphabet (Σ):
 | Terminal symbols: Γ\{+,#,*,$,?}
 | Non-terminal symbols: (see below)
 | 'ε' is an empty string i.e. pop from stack
 |
 | Production rules (Backus-Naur form):
 | <Start> ::= <Move> | <Capture> | <Promotion> | <Castle>
 | <Move> ::= <Piece><Square>
 | <Capture> ::= <Piece>x<Square>
 | <Promotion> ::= <Square>=<Name>
 | <Castle> ::= O-O | O-O-O
 | <Piece> ::= <Name> | <Name><Square> |
 |             <Name><File> | <Name><Rank> |
 |             <File>
 | <Square> ::= <File><Rank>
 | <Name> ::= K | Q | R | B | N
 | <File> ::= [a-h]
 | <Rank> ::= [1-8]
 | 
 | Parsed trees:
 | file: (:file [0-7])
 | rank: (:rank [0-7])
 | square: (<file> <rank>)
 | name: (:kind king|queen|rook|bishop|knight|pawn)
 | piece: (<name> [<square>|<file>|<rank>])
 | move: (move <piece> <square>)
 | capture: (capture <piece> <square>)
 | castle: (castle king-side|queen-side)
 | promotion: (promotion <square> <name>) *name is "=X"
|#

;;; Type definitions for compilation

(deftype parser () '(function (string) list))
(deftype unit (type) `(function (,type) parser))

;;; Primitive parsers and combinators

(defun bind (p f)
  "Given p:Parser a and f:a->Parser b return λx -> Parser b"
  (declare (type parser p)
           (type (unit t) f))
  (lambda (inp)
    (mapcan (lambda (pair) ; Get (value,string) pairs
              (let ((q (funcall f (car pair)))) ; Apply f to value, get parser
                (funcall q (cdr pair)))) ; Apply q to unconsumed string
            (funcall p inp)))) ; Apply parser to input

(defun result (v)
  "Given any value v return a Parser v that doesn't consume any of its input"
  (declare (ftype (unit t)))
  (lambda (inp) (list (cons v inp))))

(defun then (p q)
  "Given two parsers p and q bind them together i.e. p then q"
  (bind p (lambda (x) (bind q (lambda (y) (result (list x y)))))))

(defun plus (p q)
  "Given two parsers p and q apply both on the input"
  (lambda (inp) (nconc (funcall p inp) (funcall q inp))))

(defun parse-zero (inp)
  "Always fails regardless of input"
  (declare (ftype parser)
           (ignore inp))
  nil)

(defun parse-item (inp)
  "Consumes the first character if the input is non-empty, fails otherwise"
  (declare (ftype parser))
  (when (not (emptyp inp))
    (list (cons (char inp 0) (subseq inp 1)))))

(defun parse-n (n)
  "Create a parser that consumes n characters"
  (declare (ftype (unit integer)))
  (lambda (inp)
    (when (>= (length inp) n)
      (list (cons (subseq inp 0 n) (subseq inp n))))))

(defun sat (f)
  "Given a predicate f:char->bool return a Parser char"
  (declare (ftype (unit (function (character) boolean))))
  (bind #'parse-item (lambda (x)
                       (if (funcall f x)
                           (result x)
                           #'parse-zero))))

;;; Generic parsers and combinators

(defun parse-char (x)
  "Create parser for given character"
  (declare (ftype (unit character)))
  (sat (lambda (y) (char= x y))))

(defun parse-digit (inp)
  "Parse any digit"
  (declare (ftype parser))
  (funcall (sat #'digit-char-p) inp))

(defun parse-lower (inp)
  "Parse any lower case character"
  (declare (ftype parser))
  (funcall (sat #'lower-case-p) inp))

(defun parse-upper (inp)
  "Parse any upper case character"
  (declare (ftype parser))
  (funcall (sat #'upper-case-p) inp))

(defun parse-letter (inp)
  "Parse any letter [a-zA-Z]"
  (declare (ftype parser))
  (funcall (plus #'parse-lower #'parse-upper) inp))

(defun parse-alphanum (inp)
  "Parse any alphanumeric character"
  (declare (ftype parser))
  (funcall (plus #'parse-digit #'parse-letter) inp))

(defun parse-word (inp)
  "Parse a word and all its substrings"
  (declare (ftype parser))
  (funcall (plus (result "")
                 (bind #'parse-letter
                       (lambda (x)
                         (bind #'parse-word
                               (lambda (xs)
                                 (result (concatenate 'string (string x) xs)))))))
           inp))

(defun parse-string (str)
  "Create parser for given string"
  (declare (ftype (unit string)))
  (if (emptyp str)
      (result "")
      (bind (parse-char (char str 0))
            (lambda (x)
              (bind (parse-string (subseq str 1))
                    (lambda (xs)
                      (result (concatenate 'string (string x) xs))))))))

(defun any (&rest seq)
  "Create parser which matches any of the ones given"
  (do* ((p seq (cdr p))
        (result (car p) (plus (car p) result)))
       ((null (cdr p)) result)))

(defun chain (&rest seq)
  "Create parser which chains the ones given in order"
  (do* ((p (nreverse seq) (cdr p))
        (result (car p) (then (car p) result)))
       ((null (cdr p)) result)))

;;; Specific chess move parsers

(defun parse-file (inp)
  "Parse a <File>"
  (funcall (bind (sat (lambda (x) (and (char>= x #\a) (char<= x #\h))))
                 (lambda (x)
                   (result (list :file
                                 (- (char-int x) (char-int #\a))))))
           inp))

(defun parse-rank (inp)
  "Parse a <Rank>"
  (funcall (bind (sat (lambda (x) (and (char>= x #\1) (char<= x #\8))))
                 (lambda (x)
                   (result (list :rank
                                 (- (char-int x) (char-int #\0) 1)))))
           inp))

(defun parse-square (inp)
  "Parse a <Square> ::= <File><Rank>"
  (funcall (bind (then #'parse-file #'parse-rank)
                 (lambda (x)
                   (result (flatten x))))
           inp))

(defun parse-name (inp)
  "Parse a <Name> ::= K | Q | R | B | N"
  (funcall (bind #'parse-item
                 (lambda (x)
                   (cond ((char= x #\K) (result (list :kind 'king)))
                         ((char= x #\Q) (result (list :kind 'queen)))
                         ((char= x #\R) (result (list :kind 'rook)))
                         ((char= x #\B) (result (list :kind 'bishop)))
                         ((char= x #\N) (result (list :kind 'knight)))
                         (t #'parse-zero))))
           inp))

(defun parse-piece (inp)
  "Parse a <Piece> ::= (see above)"
  (mapcar (lambda (pair)
            (cons (flatten (car pair)) (cdr pair)))
          (funcall (any #'parse-name
                        (then #'parse-name #'parse-square)
                        (then #'parse-name #'parse-file)
                        (then #'parse-name #'parse-rank)
                        (bind #'parse-file
                              (lambda (x)
                                (lambda (i)
                                  (declare (ignore i))
                                  `(((:kind 'pawn ,x) . ,inp))))))
                   inp)))

(defun parse-castle (inp)
  "Parse a <Castle> ::= O-O | O-O-O"
  (cond ((and (> (length inp) 4)
              (string= (subseq inp 0 5) "O-O-O"))
         (list (cons '(castle queen-side) (subseq inp 5))))
        ((and (> (length inp) 2)
              (string= (subseq inp 0 3) "O-O"))
         (list (cons '(castle king-side) (subseq inp 3))))))

(defun parse-promotion (inp)
  "Parse a <Promotion> ::= <Square>=<Name>"
  (funcall (bind #'parse-square
                 (lambda (sq)
                   (bind (parse-char #\=)
                         (lambda (op)
                           (declare (ignore op))
                           (bind #'parse-name
                                 (lambda (n)
                                   (result `(promotion ,sq ,n))))))))
           inp))

(defun parse-capture (inp)
  "Parse a <Capture> ::= <Piece>x<Square>"
  (funcall (bind (plus #'parse-piece
                       (bind #'parse-file
                             (lambda (f)
                               (result `(pawn ,f)))))
                 (lambda (p)
                   (bind (parse-char #\x)
                         (lambda (op)
                           (declare (ignore op))
                           (bind #'parse-square
                                 (lambda (sq)
                                   (result `(capture ,p ,sq))))))))
           inp))

(defun parse-move (inp)
  "Parse a <Move> ::= <Piece><Square>"
  (funcall (bind #'parse-piece
                 (lambda (p)
                   (bind #'parse-square
                         (lambda (sq)
                           (result `(move ,p ,sq))))))
           inp))

(defun parse-start (inp)
  "Parse any chess move <Start> ::= <Move> | <Capture> | <Promotion> | <Castle>"
  (funcall (any (then (parse-string "help") (result '(help)))
                (then (parse-string "draw") (result '(draw)))
                (then (parse-string "resign") (result '(resign)))
                #'parse-move
                #'parse-capture
                #'parse-promotion
                #'parse-castle)
           inp))
