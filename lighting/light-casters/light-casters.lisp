;; https://learnopengl.com/Lighting/Light-casters
(in-package :cl-user)

(ql:quickload :sdl2)
(ql:quickload :sdl2-image)
(ql:quickload :cl-opengl)
(ql:quickload :glkit)

(defparameter *width* 960)
(defparameter *height* 540)

(defvar *gl-triangle* nil)

(defvar *shader* nil)
(defvar *shader-light* nil)
(defvar *vao* nil)
(defvar *vao-light* nil)
(defvar *vbo* nil)

(defvar *diffuse-texture* nil)
(defvar *specular-texture* nil)

(defvar *camera* nil)

(defparameter *light-color* '(1.0 1.0 1.0))
(defparameter *light-pos* '(1.2 0.0 0.0))

(defclass camera ()
  ((pos :type kit.glm:vec3 :initarg :pos :accessor pos)
   (front :type kit.glm:vec3 :initarg :front :accessor front)
   (up :type kit.glm:vec3 :initarg :up :reader camera-up)
   (move-speed :type float :initarg :speed :reader move-speed)
   (yaw :type float :initarg :yaw :accessor yaw)
   (pitch :type float :initarg :pitch :accessor pitch)
   (fov :type float :initarg :fov :accessor fov)))

(defun update-camera-front ()
  (let* ((yaw (yaw *camera*))
         (pitch (pitch *camera*))
         (x (* (cos (kit.glm:deg-to-rad yaw)) (cos (kit.glm:deg-to-rad pitch))))
         (y (sin (kit.glm:deg-to-rad pitch)))
         (z (* (sin (kit.glm:deg-to-rad yaw)) (cos (kit.glm:deg-to-rad pitch))))
         (front (kit.glm:normalize (kit.glm:vec x y z))))
    (setf (front *camera*) front)))

(defun get-view-mat ()
  (kit.glm:look-at (pos *camera*)
                   (kit.glm:vec+ (pos *camera*) (front *camera*))
                   (camera-up *camera*)))

(defun move-camera (dt dir)
  (cond
    ((eq dir :forward)
     (setf (pos *camera*)
           (kit.glm:vec+ (pos *camera*)
                         (kit.glm:vec* (front *camera*) (* dt (move-speed *camera*))))))
    ((eq dir :backward)
     (setf (pos *camera*)
           (kit.glm:vec- (pos *camera*)
                         (kit.glm:vec* (front *camera*) (* dt (move-speed *camera*))))))
    ((eq dir :up)
     (setf (pos *camera*)
           (kit.glm:vec+ (pos *camera*)
                         (kit.glm:vec* (camera-up *camera*) (* dt (move-speed *camera*))))))
    ((eq dir :down)
     (setf (pos *camera*)
           (kit.glm:vec- (pos *camera*)
                         (kit.glm:vec* (camera-up *camera*) (* dt (move-speed *camera*))))))
    ((eq dir :left)
     (setf (pos *camera*)
           (kit.glm:vec- (pos *camera*)
                         (kit.glm:vec* (kit.glm:normalize (kit.glm:cross-product (front *camera*) (camera-up *camera*)))
                                       (* dt (move-speed *camera*))))))
    ((eq dir :right)
     (setf (pos *camera*)
           (kit.glm:vec+ (pos *camera*)
                         (kit.glm:vec* (kit.glm:normalize (kit.glm:cross-product (front *camera*) (camera-up *camera*)))
                                       (* dt (move-speed *camera*))))))))

(defun rotate-camera (x y)
  (let ((yaw (+ (yaw *camera*) x))
        (pitch (- (pitch *camera*) y)))
    (if (< pitch -89.0)
        (setf pitch -89.0))
    (if (> pitch 89.0)
        (setf pitch 89.0))
    (setf (yaw *camera*) yaw)
    (setf (pitch *camera*) pitch)
    (update-camera-front)))

(defun adjust-camera-fov (angle)
  (let ((fov (+ (fov *camera*) angle)))
    (if (< fov 10.0)
        (setf fov 10.0))
    (if (> fov 60.0)
        (setf fov 60.0))
    (setf (fov *camera*) fov)))

(defclass shader ()
  ((program-id :type unsigned-int :initarg :program-id :reader program-id)))

(defun use (shader)
  (gl:use-program (program-id shader)))

(defun set-uniformi (shader name x &optional y z w)
  (use shader)
  (let ((loc (gl:get-uniform-location (program-id shader) name)))
    (cond
      (w (gl:uniformi loc x y z w))
      (z (gl:uniformi loc x y z))
      (y (gl:uniformi loc x y))
      (x (gl:uniformi loc x)))))

(defun set-uniformf (shader name x &optional y z w)
  (use shader)
  (let ((loc (gl:get-uniform-location (program-id shader) name)))
    (cond
      (w (gl:uniformf loc x y z w))
      (z (gl:uniformf loc x y z))
      (y (gl:uniformf loc x y))
      (x (gl:uniformf loc x)))))

(defun set-mat4fv (shader name mat &optional (transpose nil))
  (use shader)
  (let ((loc (gl:get-uniform-location (program-id shader) name)))
    (gl:uniform-matrix-4fv loc mat transpose)))

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

(defun move-light ()
  (let ((time (float (/ (sdl2:get-ticks) 1000))))
    (setf (first *light-pos*) (* 1.0  (sin (* 1 time))))
    (setf (second *light-pos*) (* 1.0 (cos (* 1 time))))
    (setf (third *light-pos*) 1.0)))

(defun draw ()
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:enable :depth-test)
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (use *shader*)

  (let ((view (kit.glm:identity-matrix))
        (proj (kit.glm:identity-matrix)))
    (setf view (get-view-mat))
    (setf proj (kit.glm:perspective-matrix (kit.glm:deg-to-rad (fov *camera*)) (float (/ *width* *height*)) 0.1 100.0))
    (set-mat4fv *shader* "view" view)
    (set-mat4fv *shader* "proj" proj))

  (let ((pos (pos *camera*)))
    (set-uniformf *shader* "viewPos" (aref pos 0) (aref pos 1) (aref pos 2)))

  (set-uniformf *shader* "light.position" (first *light-pos*) (second *light-pos*) (third *light-pos*))

  (set-uniformf *shader* "light.ambient" 0.2 0.2 0.2)
  (set-uniformf *shader* "light.diffuse" 0.5 0.5 0.5)
  (set-uniformf *shader* "light.specular" 1.0 1.0 1.0)

  (set-uniformf *shader* "light.constant" 1.0)
  (set-uniformf *shader* "light.linear" 0.09)
  (set-uniformf *shader* "light.quadratic" 0.0032)

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d *diffuse-texture*)
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d *specular-texture*)
  (set-uniformf *shader* "material.shininess" (* 0.1 128))

  (gl:bind-vertex-array *vao*)

  (let ((positions '(
                     ( 0.0  0.0  0.0)
                     ( 2.0  5.0 -15.0)
                     (-1.5 -2.2 -2.5)
                     (-3.8 -2.0 -12.3)
                     ( 2.4 -0.4 -3.5)
                     (-1.7  3.0 -7.5)
                     ( 1.3 -2.0 -2.5)
                     ( 1.5  2.0 -2.5)
                     ( 1.5  0.2 -1.5)
                     (-1.3  1.0 -1.5))))
    (loop for p in positions
          for i from 0 below (length positions)
          do
             (let ((model (kit.glm:matrix*
                           (kit.glm:translate* (first p) (second p) (third p))
                           (kit.glm:rotate* (* i 10.0) (* i 10.0) (* i 10.0)))))
               (set-mat4fv *shader* "model" model)
               (gl:draw-arrays :triangles 0 36))))


  (use *shader-light*)

  (let ((view (kit.glm:identity-matrix))
        (proj (kit.glm:identity-matrix)))
    (setf view (get-view-mat))
    (setf proj (kit.glm:perspective-matrix (kit.glm:deg-to-rad (fov *camera*)) (float (/ *width* *height*)) 0.1 100.0))
    (set-mat4fv *shader-light* "view" view)
    (set-mat4fv *shader-light* "proj" proj))

  (set-uniformf *shader-light* "lightColor" (first *light-color*) (second *light-color*) (third *light-color*))
  
  (gl:bind-vertex-array *vao-light*)
  (let ((model (kit.glm:matrix* (kit.glm:translate* (first *light-pos*)
                                                    (second *light-pos*)
                                                    (third *light-pos*))
                                (kit.glm:scale* 0.1 0.1 0.1))))
    (set-mat4fv *shader-light* "model" model)
    (gl:draw-arrays :triangles 0 36)))

(defun set-shader ()
  (setf *shader* (make-shader "lighting/light-casters/shader.vert"
                              "lighting/light-casters/shader.frag"))
  (setf *shader-light* (make-shader "lighting/light-casters/light.vert"
                                    "lighting/light-casters/light.frag")))

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

        (set-shader)

        (setf *camera* (make-instance 'camera
                                      :pos (kit.glm:vec 0.0 0.0 3.0)
                                      :front (kit.glm:vec 0.0 0.0 -1.0)
                                      :up (kit.glm:vec 0.0 1.0 0.0)
                                      :speed 1.0
                                      :yaw -90.0
                                      :pitch 0.0
                                      :fov 45.0))

        (let ((vertices #(
                          -0.5 -0.5 -0.5  0.0  0.0 -1.0  0.0 0.0
                          0.5 -0.5 -0.5  0.0  0.0 -1.0  1.0 0.0
                          0.5  0.5 -0.5  0.0  0.0 -1.0  1.0 1.0
                          0.5  0.5 -0.5  0.0  0.0 -1.0  1.0 1.0
                          -0.5  0.5 -0.5  0.0  0.0 -1.0  0.0 1.0
                          -0.5 -0.5 -0.5  0.0  0.0 -1.0  0.0 0.0

                          -0.5 -0.5  0.5  0.0  0.0 1.0   0.0 0.0
                          0.5 -0.5  0.5  0.0  0.0 1.0   1.0 0.0
                          0.5  0.5  0.5  0.0  0.0 1.0   1.0 1.0
                          0.5  0.5  0.5  0.0  0.0 1.0   1.0 1.0
                          -0.5  0.5  0.5  0.0  0.0 1.0   0.0 1.0
                          -0.5 -0.5  0.5  0.0  0.0 1.0   0.0 0.0

                          -0.5  0.5  0.5 -1.0  0.0  0.0  1.0 0.0
                          -0.5  0.5 -0.5 -1.0  0.0  0.0  1.0 1.0
                          -0.5 -0.5 -0.5 -1.0  0.0  0.0  0.0 1.0
                          -0.5 -0.5 -0.5 -1.0  0.0  0.0  0.0 1.0
                          -0.5 -0.5  0.5 -1.0  0.0  0.0  0.0 0.0
                          -0.5  0.5  0.5 -1.0  0.0  0.0  1.0 0.0

                          0.5  0.5  0.5  1.0  0.0  0.0  1.0 0.0
                          0.5  0.5 -0.5  1.0  0.0  0.0  1.0 1.0
                          0.5 -0.5 -0.5  1.0  0.0  0.0  0.0 1.0
                          0.5 -0.5 -0.5  1.0  0.0  0.0  0.0 1.0
                          0.5 -0.5  0.5  1.0  0.0  0.0  0.0 0.0
                          0.5  0.5  0.5  1.0  0.0  0.0  1.0 0.0

                          -0.5 -0.5 -0.5  0.0 -1.0  0.0  0.0 1.0
                          0.5 -0.5 -0.5  0.0 -1.0  0.0  1.0 1.0
                          0.5 -0.5  0.5  0.0 -1.0  0.0  1.0 0.0
                          0.5 -0.5  0.5  0.0 -1.0  0.0  1.0 0.0
                          -0.5 -0.5  0.5  0.0 -1.0  0.0  0.0 0.0
                          -0.5 -0.5 -0.5  0.0 -1.0  0.0  0.0 1.0

                          -0.5  0.5 -0.5  0.0  1.0  0.0  0.0 1.0
                          0.5  0.5 -0.5  0.0  1.0  0.0  1.0 1.0
                          0.5  0.5  0.5  0.0  1.0  0.0  1.0 0.0
                          0.5  0.5  0.5  0.0  1.0  0.0  1.0 0.0
                          -0.5  0.5  0.5  0.0  1.0  0.0  0.0 0.0
                          -0.5  0.5 -0.5  0.0  1.0  0.0  0.0 1.0
                          )))
          (setf *gl-triangle*
                (loop :with gl-array = (gl:alloc-gl-array :float (length vertices))
                      :for i :from 0 :below (length vertices) :do
                        (setf (gl:glaref gl-array i)
                              (elt vertices i))
                      :finally (return gl-array))))

        
        (setf *vao* (gl:gen-vertex-array))
        (setf *vao-light* (gl:gen-vertex-array))
        (setf *vbo* (gl:gen-buffer))

        (gl:bind-vertex-array *vao*)
        (gl:bind-buffer :array-buffer *vbo*)
        (gl:buffer-data :array-buffer :static-draw *gl-triangle*)
        (gl:enable-vertex-attrib-array 0)
        (gl:vertex-attrib-pointer 0 3 :float 0 (* 8 (cffi:foreign-type-size :float)) 0)
        (gl:enable-vertex-attrib-array 1)
        (gl:vertex-attrib-pointer 1 3 :float 0 (* 8 (cffi:foreign-type-size :float)) (* 3 (cffi:foreign-type-size :float)))
        (gl:enable-vertex-attrib-array 2)
        (gl:vertex-attrib-pointer 2 2 :float 0 (* 8 (cffi:foreign-type-size :float)) (* 6 (cffi:foreign-type-size :float)))

        (gl:bind-vertex-array *vao-light*)
        (gl:bind-buffer :array-buffer *vbo*)
        ;; data already in gpu
        (gl:enable-vertex-attrib-array 0)
        (gl:vertex-attrib-pointer 0 3 :float 0 (* 8 (cffi:foreign-type-size :float)) 0)

        (gl:bind-vertex-array 0)
        (gl:bind-buffer :array-buffer 0)


        (progn
          (setf *diffuse-texture* (gl:gen-texture))
          (gl:bind-texture :texture-2d *diffuse-texture*)
          (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
          (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
          (gl:tex-parameter :texture-2d :texture-min-filter :linear)
          (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

          (let ((image (sdl2-image:load-png-rw "./assets/container2.png")))
            (gl:tex-image-2d :texture-2d 0 :rgb
                             (sdl2:surface-width image) (sdl2:surface-height image)
                             0 :rgba :unsigned-byte (sdl2:surface-pixels image))
            (gl:generate-mipmap :texture-2d)
            (sdl2:free-surface image)))

        (progn
          (setf *specular-texture* (gl:gen-texture))
          (gl:bind-texture :texture-2d *specular-texture*)
          (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
          (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
          (gl:tex-parameter :texture-2d :texture-min-filter :linear)
          (gl:tex-parameter :texture-2d :texture-mag-filter :linear)

          (let ((image (sdl2-image:load-png-rw "./assets/container2_specular.png")))
            (gl:tex-image-2d :texture-2d 0 :rgb
                             (sdl2:surface-width image) (sdl2:surface-height image)
                             0 :rgba :unsigned-byte (sdl2:surface-pixels image))
            (gl:generate-mipmap :texture-2d)
            (sdl2:free-surface image)))
          
        (gl:viewport 0 0 *width* *height*)
        (set-uniformi *shader* "material.diffuse" 0)
        (set-uniformi *shader* "material.specular" 1)

        (sdl2:with-event-loop (:method :poll)
          (:keydown (:keysym keysym)
                    (let ((scancode (sdl2:scancode-value keysym))
                          (dt (float (/ 33 1000))))
                      (cond
                        ((sdl2:scancode= scancode :scancode-w)
                         (move-camera dt :forward))
                        ((sdl2:scancode= scancode :scancode-s)
                         (move-camera dt :backward))
                        ((sdl2:scancode= scancode :scancode-a)
                         (move-camera dt :left))
                        ((sdl2:scancode= scancode :scancode-d)
                         (move-camera dt :right))
                        ((sdl2:scancode= scancode :scancode-q)
                         (move-camera dt :up))
                        ((sdl2:scancode= scancode :scancode-e)
                         (move-camera dt :down))
                        ((sdl2:scancode= scancode :scancode-j)
                         (rotate-camera 0 1))
                        ((sdl2:scancode= scancode :scancode-k)
                         (rotate-camera 0 -1))
                        ((sdl2:scancode= scancode :scancode-h)
                         (rotate-camera -1 0))
                        ((sdl2:scancode= scancode :scancode-l)
                         (rotate-camera 1 0))
                        ((sdl2:scancode= scancode :scancode-i)
                         (adjust-camera-fov -1))
                        ((sdl2:scancode= scancode :scancode-o)
                         (adjust-camera-fov 1)))))
          (:idle ()
                 (move-light)
                 (draw)
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
          (gl:delete-buffers (list *vbo*))
          (setf *vbo* nil)
          (gl:free-gl-array *gl-triangle*)
          (setf *gl-triangle* nil))))))
