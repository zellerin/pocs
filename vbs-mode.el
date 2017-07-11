
(defvar vb-mode-syntax-table
  (let ( (synTable (make-syntax-table)))
    (modify-syntax-entry ?\' "<" synTable)
    (modify-syntax-entry ?\\ "." synTable)
    (modify-syntax-entry ?\n ">" synTable)
    synTable))

(defvar vb-mode-keywords
  (regexp-opt '("PUBLIC" "SUB" "NOT" "THEN" "NEXT" "END" "IF" "OR" "DIM"
		"SET")
	      'words))

(defvar vb-mode-functions
  (regexp-opt '("CreateObject" "Ascb" "LCase" "WriteLine"
		"ReadBinaryFile")
	      'words))

(defvar vb-mode-variables
  (regexp-opt '("ErrorLog" "WScript" "StdOut")
	      'words))

(define-derived-mode vbs-mode prog-mode "VBS"
  "Visual basic visualisation"
  (setq font-lock-defaults `((,vb-mode-keywords
			      (,vb-mode-functions . font-lock-function-name-face)
			      (,vb-mode-variables . font-lock-variable-name-face))
			     nil t)
	font-lock-syntax-table vb-mode-syntax-table))
