;; El Psy Kongaroo

(message "El Psy Kongaroo")

(put-text-property 1 2 'display '(image :type jpeg :file "/home/zarkli/Pictures/avatars/christina_avatar.jpeg" :scale 0.1 :format nil :transform-smoothing t))

(put-image (create-image "~/Pictures/avatars/christina_avatar.jpeg") 2)

(insert-image (create-image "~/Pictures/avatars/christina_avatar.jpeg") "hello")

(insert-image-file "~/Pictures/avatars/christina_avatar.jpeg")

(let ((ol (make-overlay 1 10)))
  (overlay-put ol 'before-string "hello"))

(put-text-property 1 2 'display "hello")

(overlay-get (peek-get-window-overlay) 'after-string)

(buffer-string)

;;;;;
;;;;;
;;;;;
;;;;;
;;;;;
;;;;;
;;;;;
;;;;;
;;;;;
;;;;;
