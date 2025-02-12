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

(defmacro defterrain (name &body mappings)
  `(progn
     ,@(loop for (symbol char-str) on mappings by #'cddr
             collect `(setq ,symbol (string ,char-str)))))

(defterrain tmap
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
        (apply #'concatenate 'string
               (loop for x to w
                     collect (rand-nth (list WOODS
                                             FIELD
                                             PIT
                                             WATER))))))

(defun draw-map (terr w h player-x player-y)
  (loop for x to w do
    (progn
      (loop for y to h do
        (let ((outc
                (if (and (equal x player-x)
                         (equal y player-y))
                    PLAYER
                    (elt (elt terr y) x))))
          (princ (string outc))))
      (format t "~%"))))

(defun main ()
  (format t "Welcome to rectumon!~%")
  (loop do
    (let* ((w (+ 10 (random 40)))
           (h (+ 7 (random 15)))
           (player-loc (list (random w)
                             (random h)))
           (terr (terrain w h)))
      (draw-map terr w h (car player-loc) (cadr player-loc)))
    (format t "Please type a key...")
    (force-output)
    (let ((key (read-single-keystroke)))
      (format t "You typed: ...'~a'...~%" key)
      (when (char= key #\q)
        (format t "Thanks again for using rectumon!~%")
        (return-from main)))))
