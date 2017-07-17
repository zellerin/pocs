;; tz-pic.el --- Fontification for Microchip midrange assembler

(defvar tz-pic-instruction-words
  (regexp-opt
   '("movf" "movwf" "movlw" "swapf"
     "retfie" "clrf" "bsf" "bcf" "goto"
     "call" "sleep" "incf" "decf" "btfsc"
     "addlw" "rlf" "rrf" "andlw" "iorlw" "return" "decfsz")
   'words))

(defvar tz-pic-asm-words
  (regexp-opt
   '("code" "res" "end" "org" "list" "title" "radix DEC"
     "include" "__config" "db" "de" "dw"
     "udata" "udata_shr")
   'words))

(defvar tz-pic-var-words
  (regexp-opt
   '("PORTA" "PORTC" "TRISA" "TRISC"
     "CMCON" "TMR1H" "TMR1L" "T1CON"
     "EEADR" "EECON1" "EEDAT" "FSR" "INDF")
   'words))

(defvar tz-pic-constants
  (concat "\\<0x[0-9a-fA-F]+\\>\\|\\<[0-9]+\\>\\|"
	  (regexp-opt
	   '("_INTRC_OSC_NOCLKOUT"
	     "_WDT_ON"))))

(defun tz-pic-fontify ()
  (setq font-lock-defaults
	`((("^\\S +:?\\s *" (0 font-lock-function-name-face))
	   ("^\\(\\sw+:?\\)?\\s +\\(\\(\\.?\\sw\\|\\s_\\)+\\(\\.\\sw+\\)*\\)"
	    ("^\\sw+" (move-beginning-of-line 1) nil
	     (0 font-lock-function-name-face))
	    (,tz-pic-instruction-words (move-beginning-of-line 1) nil
				       (0 font-lock-keyword-face))
	    (,tz-pic-asm-words (move-beginning-of-line 1) nil
			       (0 font-lock-builtin-face))
	    (,tz-pic-var-words nil nil
				(0 font-lock-variable-name-face))
	    (,tz-pic-constants nil nil
			       (0 font-lock-constant-face))
	    (",\s *[wf]"
	     nil nil (0 font-lock-builtin-face)))))))

(add-hook 'asm-mode-hook 'tz-pic-fontify)
