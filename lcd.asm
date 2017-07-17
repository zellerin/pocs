;; -*- coding: utf-8 -*-
        list p=16F630,t=ON,c=132,n=80
        title "lcd module test"
        radix DEC
        include "p16f630.inc"


;;            ┌┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈─┐
;; ┌─────── 1 ┊ Vdd             Vss ┊ 14 ━━━━━━━━━┓
;; │          ┊                     ┊             ┃
;; │        2 ┊ T1CKI/RA5    ICSPDAT┊ 13          ┃
;; │          ┊                     ┊             ┃
;; │        3 ┊ ¬T1G/RA4     ICSPCLK┊ 12          ┃
;; │          ┊                     ┊             ┃
;; │        4 ┊ ¬MCLR           RA2 ┊ 11 ──┐      ┃
;; │          ┊                     ┊      │      ┃       ┌────────────┐
;; │ ┌───── 5 ┊ RC5             RC0 ┊ 10 ──│──────┃─ DB7  │ 14         │
;; │ │        ┊                     ┊      │      ┃       │  LCD 2x16  │
;; │ │ ┌─── 6 ┊ RC4             RC1 ┊ 9 ───│──────┃─ DB6  │ 13         │
;; │ │ │      ┊       16f630        ┊      │      ┃       │            │
;; │ │ │ ┌─ 7 ┊ RC3             RC2 ┊ 8 ───│──────┃─ DB5  │ 12         │
;; │ │ │ │    └┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┘      │      ┃       │            │
;; │ │ │ └─────────────────────────────────│──────┃─ DB4  │ 11         │
;; │ │ │                                   │      ┃       │            │
;; │ │ │                                   │      ┣━ DB3  │ 10         │
;; │ │ │                                   │      ┃       │            │
;; │ │ │                                   │      ┣━ DB3  │ 9          │
;; │ │ │                                   │      ┃       │            │
;; │ │ │                                   │      ┣━ DB1  │ 8          │
;; │ │ │                                   │      ┃       │            │
;; │ │ │                                   │      ┣━ DB0  │ 7          │
;; │ │ │                                   │      ┃       │            │
;; │ │ └───────────────────────────────────│──────┃─── E  │ 6          │
;; │ │                                     │      ┃       │            │
;; │ └─────────────────────────────────────│──────┃─ R/W  │ 5          │
;; │                                       │      ┃       │            │
;; │                                       └──────┃── RS  │ 4          │
;; │             ┌─────────────┐                  ┃       │            │
;; ├─────────────┤   Resistor  ├──────────────────┃── Vo  │ 3          │
;; │             └─────────────┘                  ┃       │            │
;; └──────────────────────────────────────────────┃─ Vcc  │ 2          │
;;                                                ┃       │            │
;;                                                ┗━ Vss  │ 1          │
;;                                                        └────────────┘

RS:	equ 2			; on porta
	;; actually, we do not read RW yet...
RW:	equ 5			; on portc
ENABLE:	equ 4			; on portc

        global init_lcd, print_text_from_prom, send_command, slow_shift_right, put_reg
	global put_reg_indf, wait_1

        udata_shr
scratch:
        ;; used by nibble rotating things
        res .1
duration:
        ;; used by short_wait as a parameter
        res .1
nibble:
        ;; parameter for nibble putting code
        res .1

        code
;;; Putting things to LCD
put_reg:
        ;; Put to screen content of a register addressed by W
        movwf FSR
put_reg_indf
        swapf INDF, W
        call put_nibble
        movf INDF, W
put_nibble:
        ;; Convert nibble to ascii and put to the screen
        andlw   0xf
        addlw   0xf6
        btfsc   STATUS, C
        addlw   0x7
        addlw   0x3a
put_char:
        ;; put a char in W to the screen
        bsf     PORTA, RS
put:
        ;; send a char or command in W to screen in two 4bit parts
        movwf   nibble
        movlw   0x3
        call    wait_1
        call    push_high_nibble
push_high_nibble:
        ;; Input: nibble = [ A B ] scratch = [ D (4bit) F E(3 bit) ]
        ;;      carry C
        ;; Output:
        ;;    nibble = [ B C rev E ] scratch = [ rev A D ] W = [ 1 A ]
        ;; Put reversed high nibble of  nibble register to low nibble of W
        ;; through high nibble of scratch
        ;; and push low nibble of 0x23 to its high nibble
        ;; n.b. initializing scratch with 0x10 to would work too, but
        ;; same instruction count as andlw/iorlw
        call rotate_2bit
        call rotate_2bit
        swapf   scratch, w
        andlw   0xf
        iorlw   (1<<ENABLE)
        movwf   PORTC
        bcf     PORTC, ENABLE
        return
rotate_2bit:
        call rotate_bit
rotate_bit:
        rlf     nibble, f
        rrf     scratch, f
        return

;;; Waiting
wait_1:
        ;; wait 256*W*constant times
        clrf    duration
        call    short_wait
        addlw   0xff
        btfsc   STATUS, Z
        return
        goto    wait_1
short_wait:
        ;; wait duration * constant times
        decfsz  duration, f
        goto    short_wait
        return

;;; Simple commands
slow_shift_right:
        movlw   0
        call    wait_1
        movlw   0x1c
send_command:
        bcf     PORTA, RS
        goto    put

;;;
print_text_from_prom:
        bsf     PORTA, RS
print_from_prom:
        ;; Put bytes from prom until 0 is seen
        bsf     STATUS, RP0 ;; bank 1
        movwf   EEADR
next_char:
        bsf     STATUS, RP0 ;; bank 1
        bsf     EECON1, 0
        incf    EEADR, f
        movf    EEDAT, w
        bcf     STATUS, RP0 ;; bank 0
        btfsc   STATUS, Z
        return
        call    put
        goto    next_char

;;; Initialization
init_lcd:
        bcf     PORTA, RS           ; commands follow
        clrf    PORTC
        movlw   0x14
        call    wait_and_reset
        movlw   0x6
        call    wait_and_reset
        movlw   0x1f
        movwf   duration
        call    short_wait
        call    send_reset
        movlw   0x3
        call    wait_1
        movlw   0x20            ; set 4bit
        call emit_w_nibble
        movlw   LOW(initcode)
        goto print_from_prom
wait_and_reset:
        call wait_1
send_reset:
        movlw   0x30            ; set 8bit
emit_w_nibble:
        movwf   nibble
        goto    push_high_nibble
DEEPROM code
initcode:
        de 0x28                 ; 4bit 2 lines
        de 8                    ; display off, no cursor, no blink
        de 1                    ; clear
        de 6                    ; increment, dont shift display
        de 0xc                  ; display on, no cursor, no blink
        de 0
        end
