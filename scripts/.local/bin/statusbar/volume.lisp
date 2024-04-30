#!/usr/bin/sbcl --script

(require 'uiop)

(defun get-command (audio-backend)
  (case audio-backend
    ((alsa)       "amixer get Master")
    ((pipewire)   "wpctl get-volume @DEFAULT_AUDIO_SINK@")
    ((pulseaudio) "pactl list sinks")
    ((sndio)      "sndioctl -f snd/default")))

(defun get-command-output (audio-backend)
  (remove-if
   (lambda (str) (string= "" str))
   (uiop:split-string
    (uiop:run-program
     (get-command audio-backend)
     :output :string)
    :separator (string #\Newline))))

(defun backend-is-muted-p (audio-backend cmd-output)
  (let ((mute-str
          (case audio-backend
            ((alsa)      "[off]")
            ((pipewire)  "MUTED")
            ((pulseaudio "Mute: yes"))
            ((sndio)     "mute=1"))))

    (reduce
     (lambda (a b) (or a b))
     (mapcar
      (lambda (line)
        (search mute-str line))
      cmd-output)
     :initial-value nil)))

(defun get-backend-volume (audio-backend cmd-output)
  (case audio-backend
    ((alsa)
     (truncate
      (parse-integer
       (second
        (uiop:split-string
         (car
          (remove-if-not
           (lambda (line)
             (search "] [" line))
           (get-command-output 'alsa)))
         :separator "["))
       :junk-allowed t)))

    ((pipewire)
     (truncate
      (* 100
         (read-from-string
          (remove-if-not
           (lambda (c)
             (or (digit-char-p c)
                 (char= (char "." 0) c)))
           (car cmd-output))))))
    ((pulseaudio)
     (truncate
      (parse-integer
       (second
        (uiop:split-string
         (car
          (remove-if-not
           (lambda (line)
             (and (search "Volume" line)
                  (not (search "Base" line))))
           (get-command-output 'pulseaudio)))
         :separator "/"))
       :junk-allowed t)))
    ((sndio)
     (* 100
        (read-from-string
         (second
          (uiop:split-string
           (car cmd-output)
           :separator '(#\=))))))))

(defun main ()
  (let* ((audio-backend (read-from-string (uiop:getenv "AUDIO")))
         (cmd-output    (get-command-output audio-backend))
         (muted-p       (backend-is-muted-p audio-backend cmd-output))
         (volume        (get-backend-volume audio-backend cmd-output))
         (icon
           (if muted-p
               "<x"
               (cond
                 ((>= volume 70) "<))")
                 ((>= volume 30) "<) ")
                 (t              "<  ")))))
    (if muted-p
        (format t "~a" icon)
        (format t "~a ~a%" icon volume))))

(main)
