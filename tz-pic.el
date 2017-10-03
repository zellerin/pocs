;; tz-pic.el --- Fontification for Microchip midrange assembler

(defvar tz-pic-instruction-words
  (regexp-opt
   '("movwf" "movlw"
     "clrf" "goto"
     "call"
     "addlw" "andlw" "iorlw"
     "equ" "res")
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
   '( "retfie" "sleep" "return" "retfie" "wdtrst")
   'words))

(defvar tz-pic-vanilla-builtins
  (regexp-opt
   '("udata_shr" "code" "end" "title" "include")
   'words)  )

(defvar tz-pic-instruction-bit-words
  (regexp-opt
   '("bsf" "bcf" "btfsc" "btfss")
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
	    "^[^:\n[:space:]]+:?" (0 font-lock-variable-name-face))
	   ;; no op instruction
	   (,tz-pic-instruction-noop-words (0 font-lock-function-name-face))
	   ;; builtins with no special handling
	   (,tz-pic-vanilla-builtins (0 font-lock-builtin-face))
	   ;; one op instructions
	   (,tz-pic-instruction-words (0 font-lock-function-name-face)
	    (,tz-pic-var-words nil nil
			 (0 font-lock-constant-face))
	    (",.*" nil nil (0 font-lock-warning-face))
	    ("\\<[[:alpha:]_][[:alnum:]_]+\\>" nil nil (0 font-lock-variable-name-face))
	    (,tz-pic-constants nil nil
			 (0 font-lock-constant-face))
)
	   ("radix" (0 font-lock-builtin-face)
	    (,(regexp-opt '("DEC" "HEX") 'words)
	     nil nil (0 font-lock-constant-face)))
	   ("list" (0 font-lock-builtin-face)
	    ("\\<[pntc]=[[:alnum:]]+" nil nil (0 font-lock-constant-face)))
	   (,(regexp-opt '("global" "extern" "de" "dw") 'words)
	    (0 font-lock-builtin-face)
	    ("\\<[_[:alnum:]]+" nil nil
	     (0 font-lock-variable-name-face)))
	   ;; with direction
	   (,tz-pic-instruction-words-wf
	    (0 font-lock-function-name-face)
	    (,(concat tz-pic-var-words ", [WFwf]") nil nil
				(0 font-lock-constant-face))
	    (,tz-pic-constants nil nil
			 (0 font-lock-constant-face))
	    ("[_[:alpha:]][[:alnum:]_]+" nil nil (0 font-lock-variable-name-face))
	    (", \\<[wfWF]\\>"
	     nil nil (0 font-lock-builtin-face)))
	   ;; bit ops
	   (,tz-pic-instruction-bit-words
	    (0 font-lock-function-name-face)
	    (,tz-pic-var-bits nil nil
			(0 font-lock-constant-face))
	    (,(concat tz-pic-var-words ",") nil nil
			 (0 font-lock-constant-face))
	    ("[_[:alpha:]][[:alnum:]_]+" nil nil (0 font-lock-variable-name-face))
	    (,tz-pic-constants nil nil
			 (0 font-lock-constant-face)))))))

(define-derived-mode pic-asm-mode asm-mode "Pic asm"
  (tz-pic-fontify))
