(defun my-term-send-string (&optional buffer string)
  "Send STRING to a shell process associated with BUFFER.
By default, BUFFER is \"*terminal*\" and STRING is empty."
  (let ((process (get-buffer-process (or buffer "*terminal*"))))
    (when (process-live-p process)
      (with-current-buffer (process-buffer process)
        (let ((input (or string "")))
          (cond ((derived-mode-p 'comint-mode)
                 (insert input)
                 (comint-send-input))
                ((derived-mode-p 'term-mode)
                 (term-send-string process input)
                 (term-send-input))))))))

;; Send empty input to `*terminal*' every 30 seconds
(run-at-time t 30 #'my-term-send-string)

;; Send ":" null command to `*shell*' every minute
(run-at-time t 60 #'my-term-send-string "*shell*" ":")
