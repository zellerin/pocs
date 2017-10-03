;; tz-pic.el --- Fontification for Microchip midrange assembler

(defvar tz-pic-instruction-words
  (regexp-opt
   '("movwf" "movlw"
     "clrf" "goto"
     "call"
     "addlw" "andlw" "iorlw"
     "equ" "res"
     "global" "extern")
   'words))

(defvar tz-pic-instruction-words-wf
  (regexp-opt
   '("movf"  "swapf"
     "incf" "decf"
     "subwf"
     "rlf" "rrf"  "iorlw" "decfsz")
   'words))

(defvar tz-pic-instruction-noop-words
  (regexp-opt
   '( "retfie" "sleep" "return" "retfie" "wdtrst"
      "udata_shr" "code" "end")
   'words))

(defvar tz-pic-instruction-bit-words
  (regexp-opt
   '("bsf" "bcf" "btfsc" "btfss"
     "global" "extern")
   'words))

(defvar tz-pic-asm-words
  (regexp-opt
   '("code" "res" "end" "org" "list" "title" "radix DEC"
     "include" "__config" "db" "de" "dw"
     "udata" "udata_shr"
     "extern" "global" "LOW" "HIGH")
   'words))

(defvar tz-pic-var-words
  (regexp-opt
   '("PORTA" "PORTC" "TRISA" "TRISC" "STATUS"
     "CMCON" "TMR1H" "TMR1L" "T1CON"
     "EEADR" "EECON1" "EEDAT" "FSR" "INDF")
   'words))

(defvar tz-pic-var-bits
    (regexp-opt
     '("STATUS, C" "STATUS, Z" "STATUS, RP0"
       "PORTA, RS")
   'words))

(defvar tz-pic-constants
  (concat "\\<0x[0-9a-fA-F]+\\>\\|\\<[0-9]+\\>\\|"
	  (regexp-opt
	   '("_INTRC_OSC_NOCLKOUT"
	     "_WDT_ON"))))

(defun tz-pic-before-comment (n)
  "Go to nth word before comment"
  (let ((here (move-end-of-line 1)))
    (move-beginning-of-line 1)
    (unless (search-forward ";" here t)
      (move-end-of-line 1))
    (forward-word (- n))))

(defun tz-pic-fontify ()
  (setq font-lock-defaults
	`(((;; No op, just label
	    "^[^:\n[:space:]]+:?$" (0 font-lock-function-name-face))
	   ;; no op instruction
	   ("^\\([^:\n[:space:]]+:?\\)?\\s +[^,\n]+$"
	    ("^\\sw+" (move-beginning-of-line 1) nil
	     (0 font-lock-function-name-face))
	    (,tz-pic-instruction-noop-words (move-beginning-of-line 1) nil
				 (0 font-lock-keyword-face)))
	   ;; one op instructions
	   ("^\\([^:\n[:space:]]+:?\\)?\\s +[^,\n]+\\s +[^,\n]+\\s *$"
	    ("^\\sw+" (move-beginning-of-line 1) nil
	     (0 font-lock-function-name-face))
	    (,tz-pic-instruction-words (move-beginning-of-line 1) nil
				 (0 font-lock-keyword-face))
	    (,tz-pic-var-words (forward-word -1) nil
				(0 font-lock-variable-name-face))
	    (,tz-pic-constants (forward-word -1) nil
			 (0 font-lock-constant-face)))
	   ;; with direction
	   (,tz-pic-instruction-words-wf
	    (0 font-lock-keyword-face)
	    (,(concat tz-pic-var-words ", [WFwf]") nil nil
				(0 font-lock-variable-name-face))
	    (,tz-pic-constants nil nil
			 (0 font-lock-constant-face))
	    (", \\<[wfWF]\\>"
	     nil nil (0 font-lock-builtin-face))
	    (,tz-pic-asm-words (move-beginning-of-line 1) nil
			 (0 font-lock-builtin-face)))
	   ;; bit ops
	   (,tz-pic-instruction-bit-words
	    (0 font-lock-keyword-face)
	    (,tz-pic-var-bits nil nil
			(0 font-lock-variable-name-face))
	    (,(concat tz-pic-var-words ",") nil nil
			 (0 font-lock-variable-name-face))
	    (,tz-pic-constants nil nil
			 (0 font-lock-constant-face)))
	   ("aaa^.*,.*,.*$"
	    (,tz-pic-constants (move-beginning-of-line 1) nil
			 (0 font-lock-constant-face))
	    (,tz-pic-asm-words (move-beginning-of-line 1) nil
			 (0 font-lock-builtin-face))
	    (,tz-pic-var-words (move-beginning-of-line 1) nil
			 (0 font-lock-variable-name-face)))))))

(define-derived-mode pic-asm-mode asm-mode "Pic asm"
  (tz-pic-fontify))
