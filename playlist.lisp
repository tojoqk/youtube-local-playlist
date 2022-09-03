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
