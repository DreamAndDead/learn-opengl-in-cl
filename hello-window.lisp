;; https://learnopengl.com/Getting-started/Hello-Window
(in-package :cl-user)

(require :sdl2)
(require :cl-opengl)

(defun main ()
  (sdl2:with-init (:everything)
    (progn
      (sdl2:gl-set-attr :context-major-version 3)
      (sdl2:gl-set-attr :context-minor-version 3)
      (sdl2:gl-set-attr :context-profile-mask
                        sdl2-ffi:+sdl-gl-context-profile-core+)
      (sdl2:gl-set-attr :doublebuffer 1)
      #+darwin
      (sdl2:gl-set-attr :context-forward-compatible-flag
                        sdl2-ffi:+sdl-gl-context-forward-compatible-flag+))

    (sdl2:with-window (win :w 960 :h 540 :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)
        (gl:viewport 0 0 960 540)

        (sdl2:with-event-loop (:method :poll)
          (:idle ()
                 (gl:clear-color 0.0 0.4 1.0 1.0)
                 (gl:clear :depth-buffer-bit :color-buffer-bit)
                 (sdl2:delay 33)
                 (sdl2:gl-swap-window win))
          (:quit () t))))))
