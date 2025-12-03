;; https://learnopengl.com/Getting-started/Creating-a-window
(in-package :cl-user)

(ql:quickload :sdl2)
(ql:quickload :cl-opengl)

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :w 960 :h 540 :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)
        (gl:viewport 0 0 960 540)

        (sdl2:with-event-loop (:method :poll)
          (:idle ()
                 (gl:clear-color 0.0 1.0 1.0 1.0)
                 (gl:clear :color-buffer)
                 (sdl2:gl-swap-window win))
          (:quit () t))))))

