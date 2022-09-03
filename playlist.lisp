(in-package :youtube-local-playlist)

(defclass playlist (clog:clog-panel)
  ((first-item :accessor first-item :initform nil)
   (item-area :accessor item-area)))

(defmethod change-class :after (playlist
                                (classname (eql 'playlist))
                                &key &allow-other-keys)
  (let* ((item-area (clog:create-div playlist))
         (form (clog:create-form playlist))
         (label (clog:create-label form :content "URL: "))
         (input (clog:create-form-element
                 form :text
                 :name "url"
                 :label label))
         (button (clog:create-form-element form :submit :value "Add")))
    (flet ((set-on-drop*-do-nothing (obj)
             (clog:set-on-drag-over obj (lambda (o) (declare (ignore o)) nil))
             (clog:set-on-drop obj (lambda (o e) (declare (ignore o e)) nil)))
           (input-on-drop (obj data)
             (cond ((equal "dragging-item" (getf data :drag-data))
                    (let ((dragging-item
                            (clog:connection-data-item obj "dragging-item")))
                      (when (not (null dragging-item))
                        (move-item-to-playlist dragging-item
                                               playlist))))
                   (t
                    (fetch-item playlist (getf data :drag-data)))))
           (button-on-click (form)
             (let ((url (clog:name-value form "url")))
               (setf (clog:value input) "")
               (fetch-item playlist url))))
      (setf (item-area playlist) item-area)
      (setf (clog:width input) "120px")
      (mapc #'set-on-drop*-do-nothing (list playlist label button))
      (clog:set-on-drop input #'input-on-drop)
      (clog:set-on-click button #'button-on-click))))

(defmethod item-list ((playlist playlist))
  (let ((x (first-item playlist))
        (acc '()))
    (loop :while (not (null x))
          :do (progn (push x acc)
                     (setf x (next-item x)))
          :finally (return (reverse acc)))))

(defmethod last-item ((playlist playlist))
  (car (last (item-list playlist))))

(defun playlist-to-sexpr (playlist)
  (loop :for item :in (item-list playlist)
        :collect (list :title (title item)
                       :image-url (image-url item)
                       :video-id (video-id item))))

(defun new-playlist (obj)
  (let ((win (clog-gui:create-gui-window obj :title "Playlist")))
    (flet ((set-on-drop*-do-nothing (obj)
             (clog:set-on-drag-over obj (lambda (o) (declare (ignore o)) nil))
             (clog:set-on-drop obj (lambda (o e) (declare (ignore o e)) nil))))
      (set-on-drop*-do-nothing (clog-gui:window-content win))
      (let ((playlist
              (change-class (clog:create-div (clog-gui:window-content win))
                            'playlist)))
        (clog-gui:set-on-window-focus win
                                      (lambda (obj)
                                        (setf (clog:connection-data-item
                                               obj "current-playlist")
                                              playlist)))
        playlist))))

(defun new-playlist-from-sexpr (obj sexpr)
  (let ((playlist (new-playlist obj)))
    (loop :for item-expr :in sexpr
          :do (create-item (getf item-expr :title)
                           (getf item-expr :video-id)
                           (getf item-expr :image-url)
                           playlist))))
