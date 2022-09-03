(defpackage #:youtube-local-playlist/playlist
  (:use #:cl)
  (:export #:new-playlist
           #:new-playlist-from-sexpr
           #:playlist-to-sexpr
           #:next-item
           #:item
           #:play))

(in-package :youtube-local-playlist/playlist)

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

(defclass item (clog:clog-panel)
  ((title :reader title :initarg :title)
   (image-url :reader image-url :initarg :image-url)
   (video-id :reader video-id :initarg :video-id)
   (playlist :accessor playlist :initarg :playlist)
   (next-item :accessor next-item :initform nil)))

(defmethod play ((item item))
  (setf (clog:connection-data-item item "playlist")
        (playlist item))
  (when (not (null (clog:connection-data-item item "playing")))
    (let ((old-item (clog:connection-data-item item "playing")))
      (clog:remove-class old-item "w3-gray")))
  (clog:add-class item "w3-gray")
  (setf (clog:connection-data-item item "playing")
        item)
  (clog:js-execute item
                   (format nil
                           "player.loadVideoById(\"~a\",0,\"large\")"
                           (video-id item))))

(defmethod change-class :after (item (classname (eql 'item))
                                &key title image-url playlist)
  (if (null (first-item playlist))
      (setf (first-item playlist) item)
      (setf (next-item (last-item playlist)) item))
  (setf (clog:display item) "flex")
  (setf (clog:align-items item) "center")
  (let* ((action-area (clog:create-div item))
         (img (clog:create-img action-area :url-src image-url))
         (title (clog:create-label action-area :content title))
         (wastebucket-wrap (clog:create-div item))
         (wastebucket (clog:create-button wastebucket-wrap :content "🗑")))
    (declare (ignore img title))
    (setf (clog:display action-area) "flex")
    (setf (clog:align-items action-area) "center")
    (clog:set-styles wastebucket-wrap '(("margin-left" "auto")
                                        ("padding" "10px")))
    (clog:set-on-click action-area
                       (lambda (obj)
                         (declare (ignore obj))
                         (play item)))
    (clog:set-on-click wastebucket
                       (lambda (obj)
                         (declare (ignore obj))
                         (remove-item item)
                         (clog:remove-from-dom item))))
  (setf (clog:draggablep item) t)
  (flet ((item-drag-start (obj)
           (setf (clog:connection-data-item obj "dragging-item")
                 obj)
           obj)
         (item-drag-over (obj)
           (declare (ignore obj))
           nil)
         (item-drag-drop (obj event)
           (when (equal (getf event :drag-data)
                        "dragging-item")
             (let ((dragging-item
                     (clog:connection-data-item obj "dragging-item")))
               (move-item-before dragging-item obj)))))
    (clog:set-on-drag-start item #'item-drag-start
                            :drag-data "dragging-item")
    (clog:set-on-drag-over item #'item-drag-over)
    (clog:set-on-drop item #'item-drag-drop)))

(defmethod previous-item (item)
  (let ((x (first-item (playlist item))))
    (loop :while (not (null x))
          :when (eq item (next-item x))
            :return (return x)
          :do (setf x (next-item x))
          :finally (return nil))))

(defmethod move-item-before (item item-dst)
  (remove-item item)
  (let ((prev-dst (previous-item item-dst)))
    (if (null prev-dst)
        (progn
          (assert (eq item-dst (first-item (playlist item-dst))))
          (setf (first-item (playlist item-dst)) item))
        (setf (next-item prev-dst) item))
    (setf (next-item item) item-dst))
  (setf (playlist item) (playlist item-dst))
  (clog:place-before item-dst item))

(defmethod move-item-to-playlist (item playlist)
  (let ((last (last-item playlist)))
    (cond
      ((null last)
       (assert (null (first-item playlist)))
       (remove-item item)
       (setf (next-item item) nil)
       (setf (first-item playlist) item)
       (setf (playlist item) playlist)
       (clog:place-inside-bottom-of (item-area playlist) item))
      (t (move-item-after item last)))))

(defmethod move-item-after (item item-dst)
  (remove-item item)
  (setf (next-item item) (next-item item-dst))
  (setf (next-item item-dst) item)
  (setf (playlist item) (playlist item-dst))
  (clog:place-after item-dst item))

(defmethod remove-item (item)
  (let ((prev (previous-item item)))
    (cond
      ((eq (first-item (playlist item)) item)
       (setf (first-item (playlist item))
             (next-item item)))
      ((not (null prev))
       (setf (next-item prev) (next-item item))))))

(defun create-item (title video-id image-url playlist)
  (change-class (clog:create-div (item-area playlist))
                'item
                :title title
                :video-id video-id
                :image-url image-url
                :playlist playlist))

(defun fetch-item (playlist url)
  (flet ((fetch-title (url)
           (handler-case
               (multiple-value-bind (body) (dexador:get url)
                 (first
                  (map 'list #'plump:text
                       (clss:select "title" (plump:parse body)))))
             (error () nil)))
         (url-to-video-id (url)
           (cl-ppcre:register-groups-bind (x)
               ("https://[^/]*/watch\\?v=([^&]*)" url)
             x)))
    (let ((video-id (url-to-video-id url)))
      (when video-id
        (let ((title (fetch-title (format nil "https://www.youtube.com/watch?v=~a"
                                          video-id))))
          (when title
            (create-item (subseq title 0 (- (length title) 10))
                         video-id
                         (format nil "http://img.youtube.com/vi/~a/default.jpg"
                                 video-id)
                         playlist)))))))
