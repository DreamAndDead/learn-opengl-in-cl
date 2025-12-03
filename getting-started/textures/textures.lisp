;; https://learnopengl.com/Getting-started/Textures
(in-package :cl-user)

(require :sdl2)
(require :sdl2-image)
(require :cl-opengl)

(defparameter *width* 960)
(defparameter *height* 540)

(defvar *gl-triangle* nil)
(defvar *gl-triangle-indices* nil)

(defvar *shader* nil)
(defvar *vao* nil)
(defvar *vbo* nil)
(defvar *ebo* nil)
(defvar *texture* nil)

(defclass shader ()
  ((program-id :type unsigned-int :initarg :program-id :reader program-id)))

(defun use (shader)
  (gl:use-program (program-id shader)))

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

(defun make-shader (vert frag)
  (let ((vertex-shader-source (uiop:read-file-string (uiop:merge-pathnames* vert)))
        (fragment-shader-source (uiop:read-file-string (uiop:merge-pathnames* frag)))
        (vertex-shader (gl:create-shader :vertex-shader))
        (fragment-shader (gl:create-shader :fragment-shader))
        (shader-program-id (gl:create-program)))
    (gl:shader-source vertex-shader vertex-shader-source)
    (gl:compile-shader vertex-shader)
    (assert-no-shader-errors vertex-shader)

    (gl:shader-source fragment-shader fragment-shader-source)
    (gl:compile-shader fragment-shader)
    (assert-no-shader-errors fragment-shader)

    (gl:attach-shader shader-program-id vertex-shader)
    (gl:attach-shader shader-program-id fragment-shader)
    (gl:link-program shader-program-id)
    (assert-no-program-errors shader-program-id)

    (gl:delete-shader vertex-shader)
    (gl:delete-shader fragment-shader)

    (make-instance 'shader :program-id shader-program-id)))


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
      (sdl2-image:init '(:jpg :png))

      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)

        (setf *shader* (make-shader "getting-started/textures/shader.vert"
                                    "getting-started/textures/shader.frag"))

        (let ((vertices #(0.5 0.5 0.0    0.0 0.0 0.0   1.0 1.0   ;; top right
                          0.5 -0.5 0.0   1.0 0.0 0.0   1.0 0.0   ;; bottom right
                          -0.5 -0.5 0.0  0.0 1.0 0.0   0.0 0.0   ;; bottom left
                          -0.5 0.5 0.0   0.0 0.0 1.0   0.0 1.0   ;; top left
                          )))
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
        (gl:vertex-attrib-pointer 0 3 :float 0 (* 8 (cffi:foreign-type-size :float)) 0)
        (gl:enable-vertex-attrib-array 1)
        (gl:vertex-attrib-pointer 1 3 :float 0 (* 8 (cffi:foreign-type-size :float)) (* 3 (cffi:foreign-type-size :float)))
        (gl:enable-vertex-attrib-array 2)
        (gl:vertex-attrib-pointer 2 2 :float 0 (* 8 (cffi:foreign-type-size :float)) (* 6 (cffi:foreign-type-size :float)))

        (gl:bind-vertex-array 0)
        (gl:bind-buffer :array-buffer 0)
        (gl:bind-buffer :element-array-buffer 0)
          
        (progn
          (setf *texture* (gl:gen-texture))
          (gl:bind-texture :texture-2d *texture*)
          (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
          (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
          (gl:tex-parameter :texture-2d :texture-min-filter :linear)
          (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

          (let ((wall (sdl2-image:load-jpg-rw (uiop:merge-pathnames* "./getting-started/textures/wall.jpg"))))
            (gl:tex-image-2d :texture-2d 0 :rgb
                             (sdl2:surface-width wall) (sdl2:surface-height wall)
                             0 :rgb :unsigned-byte (sdl2:surface-pixels wall))
            (gl:generate-mipmap :texture-2d)
            ;; FIXME image upside down
            ;; FIXME free jpg image
            ))

        (gl:viewport 0 0 *width* *height*)
        (use *shader*)
        (let ((loc (gl:get-uniform-location (program-id *shader*) "ourTexture")))
          (gl:uniformi loc 0))
        
        (sdl2:with-event-loop (:method :poll)
          (:idle ()
                 (gl:clear-color 0.0 1.0 1.0 1.0)
                 (gl:clear :color-buffer-bit)

                 (use *shader*)
                 (gl:active-texture :texture0)
                 (gl:bind-texture :texture-2d *texture*)
                 (gl:bind-vertex-array *vao*)
                 (%gl:draw-elements :triangles 6 :unsigned-int 0)

                 (sdl2:delay 33)
                 (sdl2:gl-swap-window win))
          (:quit () t))


        (sdl2-image:quit)

        (progn
          (gl:use-program 0)
          (gl:delete-program (program-id *shader*))
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
