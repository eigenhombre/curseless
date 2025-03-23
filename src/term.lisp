(in-package #:curseless)

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

(defun clear-line-sequence ()
  (format nil "~c[2K" #\escape))

(defun clear-line ()
  "
  Clears the current line in the terminal.
  "
  (format t (clear-line-sequence))
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

(defun highlight-char (char)
  "
  Prints CHAR with reverse video (simulated cursor).
  "
  (format nil "~c[7m~c~c[0m" #\escape char #\escape))

(defun hide-cursor ()
  (format t "~c[?25l" #\escape))

(defun restore-cursor ()
  (format t "~c[?25h" #\escape))
