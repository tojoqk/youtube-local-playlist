(defpackage #:youtube-local-playlist/youtube-local-playlist
  (:nicknames #:youtube-local-playlist)
  (:use #:cl
        #:youtube-local-playlist/playlist)
  (:export start-app
           serve
           on-new-window))

(in-package :youtube-local-playlist/youtube-local-playlist)

(defun on-playlist (obj)
  (new-playlist obj))

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
    
    ;; 一度 Playlist が選択されてから処理を開始する
    (let ((item (clog:connection-data-item obj "playing")))
      (when item
        ;; JavaScript にアクセスできる場合のみ実行する
        (when (clog:js-query obj "player")
          (case (get-player-state obj)
            ((ENDED UNSTARTED)
             (when (not (null (next-item item)))
               (play (next-item item))))))))
    (sleep 1)))

(defun on-file-open (obj)
  (flet ((open-file (filename)
           (when filename
             (with-open-file (s filename)
               (let ((*read-eval* nil))
                 (new-playlist-from-sexpr obj (read s)))))))
    (clog-gui:server-file-dialog obj
                                 "Open..."
                                 (merge-pathnames "./playlist/"
			                          (asdf:system-source-directory
                                                   :youtube-local-playlist/youtube-local-playlist))
                                 #'open-file)))

(defun on-file-save (obj)
  (flet ((save-file (filename)
           (when filename
             (let ((playlist
                     (clog:connection-data-item obj "current-playlist")))
               (print playlist)
               (when playlist
                 (with-open-file (s filename :direction :output
                                             :if-exists :supersede)
                   (write (playlist-to-sexpr playlist) :stream s)))))))
    (clog-gui:server-file-dialog obj
                                 "Save..."
                                 (merge-pathnames "./playlist/"
			                          (asdf:system-source-directory
                                                   :youtube-local-playlist/youtube-local-playlist))
                                 #'save-file)))

(defun on-new-window (body)
  (clog-gui:clog-gui-initialize body)
  (clog:add-class body "w3-gray")
  (let* ((menu (clog-gui:create-gui-menu-bar body))
         (tmp (clog-gui:create-gui-menu-icon menu :on-click 'on-playlist))
         (playlist
           (clog-gui:create-gui-menu-drop-down menu :content "File")))
    (declare (ignore tmp))
    (clog-gui:create-gui-menu-item playlist
                                   :content "New"
                                   :on-click 'on-playlist)
    (clog-gui:create-gui-menu-item playlist
                                   :content "Open"
                                   :on-click 'on-file-open)
    (clog-gui:create-gui-menu-item playlist
                                   :content "Save"
                                   :on-click 'on-file-save)

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
  (clog:initialize 'on-new-window)
  (clog:open-browser))

(defun serve (&key (host "0.0.0.0") (port 8080))
  (clog:initialize 'on-new-window :host host :port port))
