(in-package #:rectumon)

(defparameter +screen-width+ 80)

(defun seed-random ()
  (setf *random-state* (make-random-state t)))

(defmacro defterrain (&body mappings)
  `(progn
     ,@(loop for (symbol char-str) on mappings by #'cddr
             collect `(progn
                        (defparameter ,symbol nil)
                        (setq ,symbol (string ,char-str))))))

;; Set indents for defterrain:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (get 'defterrain 'common-lisp-indent-function) '(1 &body)))

(defterrain
  WOODS "^"
  PLAYER "@"
  FIELD "."
  PIT "v"
  WATER "~")

(defun generate-terrain (w h)
  (loop for y to h
        collect
        (loop for x to w
              collect (if (equal 0 (random 5))
                          (rand-nth (list WOODS
                                          FIELD
                                          PIT
                                          WATER))
                          FIELD))))

(defmethod repr (x) (format nil "~a" x))

(defclass location ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

(defmethod repr ((l location)) (format nil "(~a, ~a)" (x l) (y l)))

(defclass player ()
  ((location :accessor location :initarg :location)))

(defun make-player (x y)
  (make-instance 'player :location (make-instance 'location :x x :y y)))

(defmethod repr ((p player))
  (format nil "Player at ~a" (repr (location p))))

(comment
 (repr (make-player 3 4))
 ;;=>
 '"Player at (3, 4)")

(defclass game ()
  ((terrain :accessor terrain :initarg :terrain)
   (player :accessor player :initarg :player)
   (message :accessor game-message :initarg :message)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)))

(defun make-game (w h)
  (let* ((player-x (random w))
         (player-y (random h))
         (player (make-player player-x player-y))
         (terr (generate-terrain w h)))
    (make-instance 'game
                   :terrain terr
                   :player player
                   :message ""
                   :width w
                   :height h)))

(defmethod set-message ((g game) msg)
  (setf (game-message g) msg))

;; Return the available width for descriptions
(defun description-width (w)
  (- +screen-width+ w 1))

(defun split-words (sentence)
  (cl-ppcre:split "\\s+" sentence))

(defun join-words (words)
  (reduce (lambda (a b) (concatenate 'string a " " b))
          words))

(defun words-length (words)
  ;; Line length of words joined together with spaces:
  (length (join-words words)))

(words-length '(""))
(words-length '("a"))
(words-length '("a" "b"))

(defun phrases-fitting-available-width (sentence w)
  (let ((words (split-words sentence))
        (width (description-width w)))
    ;; Collect a list words fitting into width; in each line, a single
    ;; space is added to the end of each word except the last one.
    (loop with line = ()
          with lines = ()
          for word in words
          do
             (if (< (words-length (cons word line)) width)
                 (push word line)
                 (progn
                   (push (join-words (reverse line)) lines)
                   (setf line (list word))))
             (pop words)
          finally
             (when line
               (push (join-words (reverse line)) lines))
             (return (reverse lines)))))

(phrases-fitting-available-width (random-field-description) 40)
;;=>
'("A light mist rises from the ground,"
  "blurring the horizon with soft"
  "tendrils of white.")

(defun terrain-row (row-num player-pos terrain-row)
  (format nil "~{~a~}"
          (if (not (equal (y player-pos) row-num))
              terrain-row
              (loop for x from 0
                    for c in terrain-row
                    collect (if (equal x (x player-pos))
                                (highlight-char #\@)
                                c)))))

(defun board-str (terr pos message description board-width)
  (let* ((desc-w (description-width board-width))
         (description-lines (phrases-fitting-available-width
                             description
                             desc-w))
         (lines
           (loop for terrain-row in terr
                 for y from 0
                 for is-player-row = (equal y (y pos))
                 for terrain-str = (terrain-row y pos terrain-row)
                 for desc-str = (if description-lines
                                    (pop description-lines)
                                    nil)
                 collect (str (clear-line-sequence)
                              (if desc-str
                                  (str terrain-str " " desc-str)
                                  terrain-str)))))
    (format nil "~{~A~^~%~}" (nconc lines (list message)))))

(defun move (player-loc w h dx dy)
  (let ((new-x (+ (x player-loc) dx))
        (new-y (+ (y player-loc) dy)))
    (if (and (>= new-x 0)
             (<= new-x w)
             (>= new-y 0)
             (<= new-y h))
        (setf (x player-loc) new-x
              (y player-loc) new-y))))

(defun terrain-at (loc terr)
  (elt (elt terr (y loc)) (x loc)))

(defun in-woods-p (loc terr)
  (equal (terrain-at loc terr) WOODS))

(defun in-field-p (loc terr)
  (equal (terrain-at loc terr) FIELD))

(defun in-pit-p (loc terr)
  (equal (terrain-at loc terr) PIT))

(defun in-water-p (loc terr)
  (equal (terrain-at loc terr) WATER))

(defun location-description (player-location terr)
  (cond
    ((in-field-p player-location terr)
     (random-field-description))
    ((in-woods-p player-location terr)
     (random-forest-description))
    ((in-pit-p player-location terr)
     "")  ;; Display in message area
    ((in-water-p player-location terr)
     (random-water-description))
    (t
     "You are in an unknown place.")))

(defmethod repr ((g game))
  (board-str (terrain g)
             (location (player g))
             (game-message g)
             (location-description (location (player g))
                                   (terrain g))
             (width g)))

(defun draw-game (g)
  (clj-print (repr g))
  (up (+ 2 (height g))))

(defun main ()
  (format t "Welcome to rectumon!~%")
  (seed-random)
  (hide-cursor)
  (let* ((w (+ 20 (random 20)))
         (h (+ 5 (random 10)))
         (g (make-game w h))
         (player-loc (location (player g)))
         (terr (terrain g)))
    (loop do
      (when (in-pit-p player-loc terr)
        (progn
          (set-message g "You fall into a pit!")
          (draw-game g)
          (down (+ 2 (height g)))
          (read-single-keystroke)
          (println)
          (return)))
      (draw-game g)
      (fresh-line)
      (let ((key (read-single-keystroke)))
        (case key
          (#\q
           (down (height g))
           (format t "~%Bye!~%")
           (return))
          (#\k (move player-loc w h 0 -1))
          (#\j (move player-loc w h 0 1))
          (#\h (move player-loc w h -1 0))
          (#\l (move player-loc w h 1 0))
          (#\y (move player-loc w h -1 -1))
          (#\u (move player-loc w h 1 -1))
          (#\b (move player-loc w h -1 1))
          (#\n (move player-loc w h 1 1))))))
  (restore-cursor))
