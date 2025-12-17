;; https://learnopengl.com/Advanced-OpenGL/Face-culling
(in-package :cl-user)

(ql:quickload :sdl2)
(ql:quickload :sdl2-image)
(ql:quickload :cl-opengl)
(ql:quickload :glkit)
(ql:quickload :classimp)
(ql:quickload :static-vectors)

(defparameter *width* 960)
(defparameter *height* 540)

(defvar *camera* nil)
(defvar *shader* nil)
(defvar *shader-pure-color* nil)
(defvar *model* nil)
(defvar *model-floor* nil)
(defvar *model-grass* nil)
(defvar *model-glass* nil)

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



(cffi:defcstruct vertex
  (position :float :count 3)
  (normal :float :count 3)
  (tex-coords :float :count 2)
  (tangent :float :count 3)
  (bitangent :float :count 3))

(defclass texture ()
  ((id :accessor id :initarg :id)
   (type :accessor texture-type :initarg :type)))

(defclass mesh ()
  ((vertices :accessor vertices :initarg :vertices)
   (indices :accessor indices :initarg :indices)
   (textures :accessor textures :initarg :textures)
   (vao :accessor vao)
   (buffers :accessor buffers :initform nil)))

(defclass model ()
  ((meshes :initform nil :accessor meshes)
   (base-directory :initarg :base-directory :accessor base-directory)))

(defun draw-mesh (mesh shader)
  (let ((diffuse# 0)
        (specular# 0)
        (normal# 0)
        (height# 0))
    (loop for i from 0
          for texture in (textures mesh)
          for tex-type = (texture-type texture)
          for number = (case tex-type
                         (:texture_diffuse
                          (incf diffuse#))
                         (:texture_specular
                          (incf specular#))
                         (:texture_normal
                          (incf normal#))
                         (:texture_height
                          (incf height#)))
          do (set-uniformi shader (format nil "~(~a~a~)" tex-type number) i)
             (gl:active-texture i)
             (gl:bind-texture :texture-2d (id texture)))
    (gl:bind-vertex-array (vao mesh))
    (%gl:draw-elements :triangles (length (indices mesh)) :unsigned-int 0)
    (gl:bind-vertex-array 0)
    (gl:active-texture :texture0)))

(defun draw-model (model shader)
  (loop for mesh in (meshes model)
        do (draw-mesh mesh shader)))

(defun draw ()
  (gl:clear-color 0.0 0.0 0.0 1.0)

  (gl:enable :stencil-test)
  (gl:stencil-mask #xff) ; int val AND op before writing into stencil buffer
  ;; (gl:stencil-mask #x00) ; disable
  (gl:stencil-func :always 1 #xff)
  (gl:stencil-op :keep :keep :keep)
  (gl:clear-stencil 0)
  
  (gl:enable :depth-test)
  ;; (gl:depth-mask :false) ; read only depth buffer
  (gl:depth-func :less)
  
  (gl:clear :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit)

  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  (gl:enable :cull-face)
  (gl:front-face :ccw)
  (gl:cull-face :back)

  (use *shader*)

  (let ((model (kit.glm:identity-matrix))
        (view (kit.glm:identity-matrix))
        (proj (kit.glm:identity-matrix)))
    (setf view (get-view-mat))
    (setf proj (kit.glm:perspective-matrix (kit.glm:deg-to-rad (fov *camera*)) (float (/ *width* *height*)) 0.1 100.0))
    (set-mat4fv *shader* "model" model)
    (set-mat4fv *shader* "view" view)
    (set-mat4fv *shader* "proj" proj))

  (let ((pos (pos *camera*)))
    (set-uniformf *shader* "viewPos" (aref pos 0) (aref pos 1) (aref pos 2)))

  (set-uniformf *shader* "dirLight.direction" 0.0 -1.0 -1.0)
  (set-uniformf *shader* "dirLight.ambient" 0.2 0.2 0.2)
  (set-uniformf *shader* "dirLight.diffuse" 0.5 0.5 0.5)
  (set-uniformf *shader* "dirLight.specular" 1.0 1.0 1.0)

  (set-uniformf *shader* "material.shininess" (* 0.1 128))


  ;; opaque first
  ;; sort transparent objects
  ;; draw transparent
  (let ((model (kit.glm:translate* 0.0 -1.0 0.0)))
    (set-mat4fv *shader* "model" model))
  (draw-model *model-floor* *shader*)
  
  (let ((model (kit.glm:identity-matrix)))
    (set-mat4fv *shader* "model" model))
  (draw-model *model* *shader*)

  (let ((model (kit.glm:translate* 0.0 0.0 -3.0)))
    (set-mat4fv *shader* "model" model))
  (draw-model *model* *shader*)


  (let ((model (kit.glm:translate* 3.0 0.0 0.0)))
    (set-mat4fv *shader* "model" model))
  (draw-model *model-glass* *shader*)

  (let ((model (kit.glm:translate* 3.0 0.0 1.0)))
    (set-mat4fv *shader* "model" model))
  (draw-model *model-glass* *shader*))

(defun load-texture (file &key (wrap-s :clamp-to-edge) (wrap-t :clamp-to-edge)
                            (min :linear-mipmap-linear)
                            (mag :linear))
  (let ((tex-id (gl:gen-texture)))
    (gl:bind-texture :texture-2d tex-id)
    (gl:tex-parameter :texture-2d :texture-wrap-s wrap-s)
    (gl:tex-parameter :texture-2d :texture-wrap-t wrap-t)
    (gl:tex-parameter :texture-2d :texture-min-filter min)
    (gl:tex-parameter :texture-2d :texture-mag-filter mag)

    (let* ((type (pathname-type file))
           (image (cond
                    ((string= type "jpg") (sdl2-image:load-jpg-rw file))
                    ((string= type "png") (sdl2-image:load-png-rw file))
                    (t (format t "Error: load texture unsupported format ~a~%" type))))
           (pixel-format (cond
                           ((string= type "png") :rgba)
                           ((string= type "jpg") :rgb)
                           (t :rgb))))
      (gl:tex-image-2d :texture-2d 0 pixel-format
                       (sdl2:surface-width image) (sdl2:surface-height image)
                       0 pixel-format :unsigned-byte (sdl2:surface-pixels image))
      (gl:generate-mipmap :texture-2d)
      (sdl2:free-surface image))
    (gl:bind-texture :texture-2d 0)
    tex-id))

(defvar *texture-cache* (make-hash-table))

(defun load-texture-cached (file)
  (cond
    ((and (boundp '*texture-cache*)
          (gethash file *texture-cache*)))
    ((boundp '*texture-cache*)
     (setf (gethash file *texture-cache*)
           (load-texture file)))
    (t
     ;; no cache way
     (load-texture file))))

(defun load-material-textures (model material type type-name)
  (loop with files = (gethash "$tex.file" material)
        ;; type, channel, name
        for (nil nil file) in (remove type files :key 'car :test-not 'eql)
        collect (make-instance 'texture
                               :type type-name
                               :id (load-texture-cached
                                    (merge-pathnames file (base-directory model))))))

(defun setup-mesh (mesh)
  (let ((vbo (gl:gen-buffer))
        (ebo (gl:gen-buffer))
        (vao (gl:gen-vertex-array))
        (vertex-size (cffi:foreign-type-size '(:struct vertex))))
    (gl:bind-vertex-array vao)

    (gl:bind-buffer :element-array-buffer ebo)
    (static-vectors:with-static-vector (sv (length (indices mesh))
                                           :element-type '(unsigned-byte 32))
      (replace sv (indices mesh))
      (%gl:buffer-data :element-array-buffer
                       (* (length sv)
                          (cffi:foreign-type-size :unsigned-int))
                       (static-vectors:static-vector-pointer sv)
                       :static-draw))

    (gl:bind-buffer :array-buffer vbo)
    (cffi:with-foreign-object (buffers '(:struct vertex) (length (vertices mesh)))
      (loop for (position normal tex-coords tangent bitangent) in (vertices mesh)
            for i from 0
            for buffer = (cffi:mem-aptr buffers '(:struct vertex) i)
            do (macrolet ((copy (var/slot count)
                            `(when ,var/slot
                               (cffi:lisp-array-to-foreign
                                ,var/slot
                                (cffi:foreign-slot-pointer buffer
                                                           '(:struct vertex)
                                                           ',var/slot)
                                '(:array :float ,count)))))
                 (copy position 3)
                 (copy normal 3)
                 (copy tex-coords 2)
                 (copy tangent 3)
                 (copy bitangent 3)))
      (%gl:buffer-data :array-buffer
                       (* (length (vertices mesh)) vertex-size)
                       buffers :static-draw))

    (macrolet ((offset (member)
                 `(cffi:foreign-slot-offset '(:struct vertex) ',member)))
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil vertex-size (offset position))
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 3 :float nil vertex-size (offset normal))
      (gl:enable-vertex-attrib-array 2)
      (gl:vertex-attrib-pointer 2 2 :float nil vertex-size (offset tex-coords))
      (gl:enable-vertex-attrib-array 3)
      (gl:vertex-attrib-pointer 3 3 :float nil vertex-size (offset tangent))
      (gl:enable-vertex-attrib-array 4)
      (gl:vertex-attrib-pointer 4 3 :float nil vertex-size (offset bitangent)))

    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)

    (setf (buffers mesh) (list vbo ebo)
          (vao mesh) vao))
  mesh)

(defun process-mesh (model mesh scene)
  (make-instance
   'mesh
   :vertices
   (loop for i below (length (ai:vertices mesh))
         for vertex = (aref (ai:vertices mesh) i)
         for normal = (aref (ai:normals mesh) i)
         for tangent = (when (ai:tangents mesh)
                         (aref (ai:tangents mesh) i))
         for bitangent = (when (ai:bitangents mesh)
                           (aref (ai:bitangents mesh) i))
         for tex-coords = (unless (zerop (length (ai:texture-coords mesh)))
                            (subseq
                             (aref (aref (ai:texture-coords mesh) 0) i)
                             0 2))
         collect (list vertex normal tex-coords tangent bitangent))
   :indices
   (loop for face across (ai:faces mesh)
         collect (aref face 0)
         collect (aref face 1)
         collect (aref face 2))
   :textures
   (when (plusp (ai:material-index mesh))
     (let ((material (aref (ai:materials scene) (ai:material-index mesh))))
       (append
        (load-material-textures model material :ai-texture-type-diffuse
                                :material.diffuse)
        (load-material-textures model material :ai-texture-type-specular
                                :material.specular))))))

(defun process-node (model node scene)
  (loop for mesh-index across (classimp:meshes node)
        for mesh = (aref (classimp:meshes scene) mesh-index)
        do (push (setup-mesh (process-mesh model mesh scene)) (meshes model)))
  (loop for child across (classimp:children node)
        do (process-node model child scene)))

(defun load-model (path)
  (classimp:with-log-to-stdout ()
    (let ((scene (classimp:import-into-lisp
                  path :processing-flags '(:ai-process-triangulate
                                           ;; sdl image load image is upside down agaist opengl, so no flip uv here
                                           ;; :ai-process-flip-u-vs
                                           )))
          (model (make-instance 'model)))
      (unless (and scene
                   (not (classimp:scene-incomplete-p scene))
                   (classimp:root-node scene))
        (format t "Error: Assimp ~a~%" (%ai:ai-get-error-string)))
      (setf (base-directory model) (uiop:pathname-directory-pathname path))
      (process-node model (classimp:root-node scene) scene)
      (setf (meshes model) (reverse (meshes model)))
      model)))


(defun main()
  (sdl2:with-init (:everything)
    (progn
      (sdl2:gl-set-attr :context-major-version 3)
      (sdl2:gl-set-attr :context-minor-version 3)
      (sdl2:gl-set-attr :context-profile-mask
                        sdl2-ffi:+sdl-gl-context-profile-core+)
      (sdl2:gl-set-attr :doublebuffer 1)
      (sdl2:gl-set-attr :depth-size 24)
      (sdl2:gl-set-attr :stencil-size 8)
      #+darwin
      (sdl2:gl-set-attr :context-forward-compatible-flag
                        sdl2-ffi:+sdl-gl-context-forward-compatible-flag+))

    (sdl2:with-window (win :w *width* :h *height* :flags '(:shown :opengl))
      (sdl2-image:init '(:jpg :png))

      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)

        (setf *camera* (make-instance 'camera
                                      :pos (kit.glm:vec 4.0 4.0 4.0)
                                      :front (kit.glm:vec -0.5 -0.5 -0.6)
                                      :up (kit.glm:vec 0.0 1.0 0.0)
                                      :speed 2.0
                                      :yaw -120.0
                                      :pitch -33.0
                                      :fov 45.0))

        (setf *shader* (make-shader "advanced-opengl/face-culling/shader.vert"
                                    "advanced-opengl/face-culling/shader.frag"))
        (setf *shader-pure-color* (make-shader "advanced-opengl/face-culling/pure-color.vert"
                                               "advanced-opengl/face-culling/pure-color.frag"))

        (setf *model* (load-model "assets/box/box.obj"))
        ;; (setf *model* (load-model "assets/backpack/backpack.obj"))
        (setf *model-floor* (load-model "assets/floor/floor.obj"))
        (setf *model-grass* (load-model "assets/box/grass.obj"))
        (setf *model-glass* (load-model "assets/box/glass.obj"))
        

        (gl:viewport 0 0 *width* *height*)

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
                 (draw)
                 (sdl2:delay 33)
                 (sdl2:gl-swap-window win))
          (:quit () t))

        (sdl2-image:quit)

        (progn
          (gl:delete-program (program-id *shader*))
          (gl:use-program 0)
          (setf *texture-cache* (make-hash-table))
          (gl:bind-vertex-array 0))))))
