(defpackage #:youtube-playlist
  (:use #:cl)
  (:export start-app))

(in-package :youtube-playlist)

(defclass playlist (clog:clog-panel)
  ((first-item :accessor first-item :initform nil)))

(defclass item (clog:clog-panel)
  ((title :reader title :initarg :title)
   (image-url :reader image-url :initarg :image-url)
   (video-id :reader video-id :initarg :video-id)
   (playlist :reader playlist :initarg :playlist)
   (next-item :accessor next-item :initform nil)))

(defun fetch-title (url)
  (handler-case
      (multiple-value-bind (body) (dexador:get url)
        (first
         (map 'list #'plump:text
              (clss:select "title" (plump:parse body)))))
    (error () nil)))

(defun url-to-video-id (url)
  (cl-ppcre:register-groups-bind (x)
      ("https://www\\.youtube\\.com/watch\\?v=([^&]*)" url)
    x))

(defun play-item (item)
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

(defun item-list (playlist)
  (let ((x (first-item playlist))
        (acc '()))
    (loop :while (not (null x))
          :do (progn (push x acc)
                     (setf x (next-item x)))
          :finally (return (reverse acc)))))

(defun last-item (playlist)
  (car (last (item-list playlist))))

(defun create-item (playlist url)
  (let ((video-id (url-to-video-id url)))
    (when video-id
      (let ((title (fetch-title (format nil "https://www.youtube.com/watch?v=~a"
                                        video-id))))
        (when title
          (let ((item
                  (change-class (clog:create-div playlist)
                                'item
                                :title
                                (subseq title
                                        0 (- (length title) 10))
                                :video-id video-id
                                :image-url
                                (format nil "http://img.youtube.com/vi/~a/default.jpg"
                                        video-id)
                                :playlist playlist)))
            (if (null (first-item playlist))
                (setf (first-item playlist) item)
                (setf (next-item (last-item playlist)) item))
            (setf (clog:display item) "flex")
            (setf (clog:align-items item) "center")
            (clog:create-img item :url-src (image-url item))
            (clog:create-label item :content (title item))
            (clog:set-on-click item #'play-item)))))))

(defun on-playlist (obj)
  (let ((win (clog-gui:create-gui-window obj :title "Playlist")))
    (let* ((playlist (change-class (clog:create-div (clog-gui:window-content win))
                                   'playlist))
           (form (clog:create-form (clog-gui:window-content win)))
           (input (clog:create-form-element
                   form :text
                   :name "url"
                   :label (clog:create-label form :content "URL: "))))
      (setf (clog:width input) "120px")
      (clog:set-on-drop input
                        (lambda (obj data)
                          (declare (ignore obj))
                          (create-item playlist
                                       (getf data :drag-data))))
      (clog:set-on-click (clog:create-form-element form :submit :value "Add")
                         (lambda (form)
                           (let ((url (clog:name-value form "url")))
                             (setf (clog:value input) "")
                             (create-item playlist url))))
      (loop
       (unless (clog:validp playlist)
         (return))
       (sleep 1)))))

(defun load-youtube-player (win)
  (clog:create-div (clog-gui:window-content win) :html-id "player")
  (clog:create-child (clog-gui:window-content win)
                "
    <script>
      var tag = document.createElement('script');
      tag.src = \"https://www.youtube.com/iframe_api\";
      var firstScriptTag = document.getElementsByTagName('script')[0];
      firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);
      var player;
      var currentVideo = \"\";
      var nextVideo = \"\";
      function onYouTubeIframeAPIReady() {
        player = new YT.Player('player', {
          height: '320',
          width: '640',
          host: 'https://www.youtube-nocookie.com',
        });
      }
      function playNextVideo() {
        currentVideo = nextVideo;
        console.log(nextVideo)
        player.loadVideoById(nextVideo, 0, \"large\")
        player.playVideo()
        nextVideo = \"\"
      }
    </script>"))

(defun get-player-state (obj)
  (let ((str (clog:js-query obj "player.getPlayerState()")))
    (cond
      ((equal str "-1") 'UNSTARTED)
      ((equal str "0") 'ENDED)
      ((equal str "1") 'PLAYING)
      ((equal str "2") 'PAUSED)
      ((equal str "3") 'BUFFERING)
      ((equal str "4") 'CUED)
      (t nil))))

(defun main-loop (obj)
  (setf (clog:connection-data-item obj "playlist") nil)
  (setf (clog:connection-data-item obj "playing") nil)
  (loop
   (unless (clog:validp obj)
     (return))

   ;; 一度 Playlist が選択されてからループを開始する
   (when (clog:connection-data-item obj "playlist")
     (let ((item (clog:connection-data-item obj "playing")))
       ;; JavaScript へのアクセスができない場合はループを終了する
       (unless (clog:js-query obj "player")
         (return))
       (case (get-player-state obj)
         ((ENDED)
          (when (not (null (next-item item)))
            (play-item (next-item item)))))))
   (sleep 1)))

(defun on-new-window (body)
  (clog-gui:clog-gui-initialize body)
  (clog:add-class body "w3-gray")
  (let* ((menu (clog-gui:create-gui-menu-bar body)))
    (clog-gui:create-gui-menu-icon menu)
    (clog-gui:create-gui-menu-item menu :content "Playlist" :on-click #'on-playlist)
    ;; YouTube の iframe API の利用規約によると
    ;; 一つのページで複数のプレイヤーによる同時再生は禁止されているため
    ;; YouTube の Window は起動時に一つだけ作成する
    ;; XXX: Window を誤って消してまうとリロードする以外に元に戻す手段がない
    ;; https://developers.google.com/youtube/terms/required-minimum-functionality#autoplay-and-scripted-playbacks
    ;; > A page or screen must not have more than one YouTube player that
    ;; > automatically plays content simultaneously.
    (let* ((win (clog-gui:create-gui-window menu :title "YouTube")))
      (clog:add-class (clog-gui:window-content win) "w3-black")
      (setf (clog:width win) 640)
      (setf (clog:height win) (+ 320 25))
      (clog-gui:window-center win)
      (load-youtube-player win)
      (clog-gui:set-on-window-size-done
       win
       (lambda (obj)
         (let ((w (max (clog:width obj) 640))
               (h (max (- (clog:height obj) 25) 320)))
           (clog:js-execute
            win
            (format nil "player.setSize(~a, ~a)" w h)))))
      ;; YouTube のプレイヤーに他の要素が被ってしまわないように
      ;; Player のウィンドウは一番上に表示する
      ;; https://developers.google.com/youtube/terms/required-minimum-functionality#youtube-player-attributes
      ;; > You must not display overlays, frames, or other visual elements in
      ;; > front of any part of a YouTube embedded player, including player
      ;; > controls. Similarly, you must not use overlays, frames or other
      ;; > visual elements to obscure any part of an embedded player, including
      ;; > player controls.
      (clog-gui:window-keep-on-top win)
      (main-loop body))))

(defun start-app ()
  (clog:initialize
   'on-new-window
   :static-root (merge-pathnames "./www/"
			         (asdf:system-source-directory
                                  :youtube-playlist)))
  (clog:open-browser))
