;; https://learnopengl.com/Getting-started/Shaders
(in-package :cl-user)

(require :sdl2)
(require :cl-opengl)

(defparameter *width* 960)
(defparameter *height* 540)

(defvar *gl-triangle* nil)
(defvar *gl-triangle-indices* nil)

(defvar *shader-program-id* nil)
(defvar *vao* nil)
(defvar *vbo* nil)
(defvar *ebo* nil)

(defparameter *vertex-shader-source*
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;

out vec3 ourColor;

void main()
{
  gl_Position = vec4(aPos, 1.0);
  ourColor = aColor;
}")

(defparameter *fragment-shader-source*
  "#version 330 core
in vec3 ourColor;

out vec4 FragColor;

void main()
{
  FragColor = vec4(ourColor, 1.0);
}")

(defun assert-no-shader-errors (shader-id)
  (let ((success (cffi:foreign-alloc :int :initial-element 0)))
    (unwind-protect
         (progn
           (%gl:get-shader-iv shader-id :compile-status success)
           (when (/= 1 (cffi:mem-aref success :int))
             (error "OpenGl error:~%~A" (gl:get-shader-info-log shader-id))))
      (cffi:foreign-free success))))

(defun assert-no-program-errors (program-id)
  (let ((success (cffi:foreign-alloc :int :initial-element 0)))
    (unwind-protect
         (progn
           (%gl:get-program-iv program-id :link-status success)
           (when (/= 1 (cffi:mem-aref success :int))
             (error "OpenGl error:~%~A" (gl:get-program-info-log program-id))))
      (cffi:foreign-free success))))

(defun main()
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

    (sdl2:with-window (win :w *width* :h *height* :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)

        (let ((vertex-shader (gl:create-shader :vertex-shader))
              (fragment-shader (gl:create-shader :fragment-shader)))
          (gl:shader-source vertex-shader *vertex-shader-source*)
          (gl:compile-shader vertex-shader)
          (assert-no-shader-errors vertex-shader)

          (gl:shader-source fragment-shader *fragment-shader-source*)
          (gl:compile-shader fragment-shader)
          (assert-no-shader-errors fragment-shader)

          (setf *shader-program-id* (gl:create-program))
          (gl:attach-shader *shader-program-id* vertex-shader)
          (gl:attach-shader *shader-program-id* fragment-shader)
          (gl:link-program *shader-program-id*)
          (assert-no-program-errors *shader-program-id*)

          (gl:delete-shader vertex-shader)
          (gl:delete-shader fragment-shader))


        (let ((vertices #(0.5 0.5 0.0    0.0 0.0 0.0
                          0.5 -0.5 0.0   1.0 0.0 0.0
                          -0.5 -0.5 0.0  0.0 1.0 0.0
                          -0.5 0.5 0.0   0.0 0.0 1.0)))
          (setf *gl-triangle*
                (loop :with gl-array = (gl:alloc-gl-array :float (length vertices))
                      :for i :from 0 :below (length vertices) :do
                        (setf (gl:glaref gl-array i)
                              (elt vertices i))
                      :finally (return gl-array))))

        (let ((indices #(0 1 3
                         1 2 3)))
          (setf *gl-triangle-indices*
                (loop :with gl-array = (gl:alloc-gl-array :unsigned-int (length indices))
                      :for i :from 0 :below (length indices) :do
                        (setf (gl:glaref gl-array i)
                              (elt indices i))
                      :finally (return gl-array))))

        (setf *vao* (gl:gen-vertex-array))
        (setf *vbo* (gl:gen-buffer))
        (setf *ebo* (gl:gen-buffer))

        (gl:bind-vertex-array *vao*)

        (gl:bind-buffer :array-buffer *vbo*)
        (gl:buffer-data :array-buffer :static-draw *gl-triangle*)

        (gl:bind-buffer :element-array-buffer *ebo*)
        (gl:buffer-data :element-array-buffer :static-draw *gl-triangle-indices*)
          
        (gl:enable-vertex-attrib-array 0)
        (gl:vertex-attrib-pointer 0 3 :float 0 (* 6 (cffi:foreign-type-size :float)) 0)
        (gl:enable-vertex-attrib-array 1)
        (gl:vertex-attrib-pointer 1 3 :float 0 (* 6 (cffi:foreign-type-size :float)) (* 3 (cffi:foreign-type-size :float)))

        (gl:bind-vertex-array 0)
        (gl:bind-buffer :array-buffer 0)
        (gl:bind-buffer :element-array-buffer 0)
          
        (gl:viewport 0 0 *width* *height*)
        (sdl2:with-event-loop (:method :poll)
          (:idle ()
                 (gl:clear-color 0.0 1.0 1.0 1.0)
                 (gl:clear :color-buffer-bit)

                 (gl:use-program *shader-program-id*)
                 (gl:bind-vertex-array *vao*)

                 ;; (let* ((time (* 0.001 (sdl2:get-ticks)))
                 ;;        (green (+ 0.5 (/ (sin time) 2)))
                 ;;        (loc (gl:get-uniform-location *shader-program-id* "ourColor")))
                 ;;   (gl:uniformf loc 0.0 green 0.0 1.0))
                 (%gl:draw-elements :triangles 6 :unsigned-int 0)
                 (sdl2:delay 33)
                 (sdl2:gl-swap-window win))
          (:quit () t))

        (progn
          (gl:use-program 0)
          (gl:delete-program *shader-program-id*)
          (setf *shader-program-id* 0)
          (gl:bind-vertex-array 0)
          (gl:delete-vertex-arrays (list *vao*))
          (setf *vao* nil)
          (gl:delete-buffers (list *vbo* *ebo*))
          (setf *vbo* nil
                *ebo* nil)
          (gl:free-gl-array *gl-triangle*)
          (gl:free-gl-array *gl-triangle-indices*)
          (setf *gl-triangle* nil
                *gl-triangle-indices* nil))))))
