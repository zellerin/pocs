(defface net-utils-ip-address '((t :weight bold))
  "Face for IP addresses")

(defun tz-net-fontify ()
  (font-lock-add-keywords nil '((".* : No" . font-lock-comment-face)))
  (font-lock-add-keywords nil '(("Node Type .*" . font-lock-constant-face)))
  (font-lock-add-keywords nil '(("Host Name .* : \\(.*\\)" 1 'net-utils-ip-address)))
  (font-lock-add-keywords nil '(("IPv[46] Address.*: \\([^%(\n]*\\)" 1
				 'net-utils-ip-address))))

(add-hook 'net-utils-mode-hook 'tz-net-fontify)
