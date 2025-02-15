(in-package #:rectumon)

(defun read-single-keystroke ()
  (let* ((fd (sb-sys:fd-stream-fd sb-sys:*stdin*))
         ;; Get original terminal settings
         (orig-termios (sb-posix:tcgetattr fd))
         (new-termios (make-instance 'sb-posix:termios)))
    (setf (sb-posix:termios-iflag new-termios)
          (sb-posix:termios-iflag orig-termios)

          (sb-posix:termios-oflag new-termios)
          (sb-posix:termios-oflag orig-termios)

          (sb-posix:termios-cflag new-termios)
          (sb-posix:termios-cflag orig-termios)

          (sb-posix:termios-lflag new-termios)
          (logand (sb-posix:termios-lflag orig-termios)
                  (lognot sb-posix:icanon)
                  (lognot sb-posix:echo))

          (sb-posix:termios-cc new-termios)
          (sb-posix:termios-cc orig-termios))
    ;; Apply new settings
    (sb-posix:tcsetattr fd sb-posix:tcsanow new-termios)
    (unwind-protect
         (read-char)  ; Read a single character, blocking
      ;; Restore original settings
      (sb-posix:tcsetattr fd sb-posix:tcsanow orig-termios))))

(defun seed-random ()
  (setf *random-state* (make-random-state t)))

(defmacro defterrain (&body mappings)
  `(progn
     ,@(loop for (symbol char-str) on mappings by #'cddr
             collect `(progn
                        (defparameter ,symbol nil)
                        (setq ,symbol (string ,char-str))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (get 'defterrain 'common-lisp-indent-function) '(1 &body)))

(defterrain
  WOODS "^"
  PLAYER "@"
  FIELD "."
  PIT "v"
  WATER "~")

(defun rand-nth (lst)
  (nth (random (length lst)) lst))

(defun terrain (w h)
  (loop for y to h
        collect
        (loop for x to w
              collect (if (equal 0 (random 5))
                          (rand-nth (list WOODS
                                          FIELD
                                          PIT
                                          WATER))
                          FIELD))))

(defun map-with-player (terr w h player-loc)
  ;; Superimpose the player on the terrain
  (let ((new-terr (copy-seq terr)))
    (setf (elt (elt new-terr (cadr player-loc))
               (car player-loc))
          PLAYER)
    new-terr))

(defun map-as-str (terr)
  (format nil "~{~{~a~}~^~%~}" terr))

(defun up (n)
  (loop repeat n do
    (format t "~c[1A" #\escape)  ; Equivalent to "\033[F"
    (force-output)))

(defun up-one-line () (up 1))

(defun down (n)
  (loop repeat n do
    (format t "~C[B" #\Escape))
  (force-output))

(defun down-one-line () (down 1))

(defun beginning-of-line ()
  (format t "~c[1G" #\escape))

(defun clear-line ()
  "Clears the current line in the terminal."
  (format t "~c[2K" #\escape)  ; Equivalent to "\033[K"
  (force-output))

(defun back (n)
  (loop repeat n do
    (format t "~C[D" #\Escape))
  (force-output))

(defun back-one-space ()
  (back 1))

(defun forward (n)
  (loop repeat n do
    (format t "~C[C" #\Escape))
  (force-output))

(defun forward-one-space ()
  (forward 1))

(defun clear-map (h)
  (loop repeat h do
    (up-one-line)
    (beginning-of-line)
    (clear-line)))

(defun print-lines (n)
  (loop repeat n do (format t "~%")))

(defun player-x (loc)
  (car loc))

(defun player-y (loc)
  (cadr loc))

(defun draw-map (terr w h player-x player-y)
  (loop for y to h do
    (progn
      (loop for x to w do
        (let ((outc
                (if (and (equal x player-x)
                         (equal y player-y))
                    PLAYER
                    (elt (elt terr y) x))))
          ;; Avoid format/escape issue, use princ:
          (princ (string outc))
          (force-output)))
      (when (< y h) (format t "~%")))))

(defun move-to-end-of-map (h player-loc)
  (down (- h (player-y player-loc))))

(defun clear-space-for-map (h)
  (print-lines h))

(defun draw-game (terr w h player-loc)
  (clear-map h)
  (princ (map-as-str (map-with-player terr w h player-loc)))
  (back (1+ (- w (player-x player-loc))))
  (up (- h (player-y player-loc))))

(defun main ()
  (format t "Welcome to rectumon!~%")
  (seed-random)
  (let* ((w (+ 30 (random 40)))
         (h (+ 5 (random 10)))
         (player-loc (list (random w)
                           (random h)))
         (terr (terrain w h))
         (first-time t))
    (loop do
      (when first-time
        (clear-space-for-map h))
      (setf first-time nil)
      (draw-game terr w h player-loc)
      (let ((key (read-single-keystroke)))
        (move-to-end-of-map h player-loc)
        (when (char= key #\q)
          (format t "~%Bye!~%")
          (return-from main))))))
