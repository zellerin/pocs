;;; -*- coding:utf-8; -*-
;;; Copyright (C) 2011,2017 by Tomas Zellerin
;;;
;;; Drive LCD with microchip pic16f630
;;; Digged out from my archives
;;; 
	
	list p=16F630,t=ON,c=132,n=80
	title "lcd module test"
	radix DEC
	include "p16f630.inc"

	__config _INTRC_OSC_NOCLKOUT & _WDT_ON & _MCLRE_ON

;;; Registers
;;; PINS
;;;    this is for easy putting on prototype board 
;;; RC0 to RC3 -> D4-D7 (reversed)
;;; RA2 to RS (we want to set it separately from other wires)
;;; rc5 rw
;;; rc4 e

	
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
intr_w:	res .1
intr_status:
	res .1

	code
	goto	init
;;; interrupt code
;;; unused at the moment
	org 4
	movwf	intr_w
	movf	0x3, w
	movwf	intr_status
	movf	intr_status, w
	movwf	0x3
	swapf	intr_w, f
	swapf	intr_w, w
	retfie
;;; complete start
init:
	movlw	7
	movwf   CMCON
	clrf	TMR1H
	clrf	TMR1L
	clrf	PORTA
	clrf	PORTC
	movlw	0x3b
	bsf	0x3, 0x5 ;; bank 1
	movwf	TRISA ^ 0x80
	clrf	TRISC ^ 0x80
	movlw	0x7
	bcf	0x3, 0x5 ;; bank 0
	;; timer enabled, no prescaler, internal source
	movwf	T1CON	 ; ¬ t1sync , tmr1cs, tmr1on
main_loop:
	call init_lcd
	movlw	LOW(hello)
	call	print_text_from_prom
	movlw	0xc0 		; set ddram address = 0x40
	call    send_command	; i.e., move to 2nd row
	movlw	LOW(row)
	call	print_text_from_prom
	call	slow_shift_right
	call	slow_shift_right
	call	slow_shift_right
	sleep 			; wait long time (watchdog)
	call init_lcd
	movlw TMR1H
	call put_reg
	movlw TMR1L
	call put_reg
	sleep			; wait for watchdog
	goto	main_loop

;;; Putting things to LCD
put_reg:
	;; Put to screen content of a register addressed by W
	movwf FSR
	swapf INDF, W
	call put_nibble
	movf INDF, W
put_nibble:
	;; Convert nibble to ascii and put to the screen
	andlw	0xf
	addlw	0xf6
	btfsc	0x3, 0
	addlw	0x7
	addlw	0x3a
put_char:
	;; put a char in W to the screen
	bsf	PORTA, 2
put:
	;; send a char or command in W to screen in two 4bit parts
	movwf	nibble
	movlw	0x3
	call	wait_1
	call	push_high_nibble
push_high_nibble:
	;; Input: nibble = [ A B ] scratch = [ D (4bit) F E(3 bit) ]
	;;   	carry C
	;; Output:
	;;    nibble = [ B C rev E ] scratch = [ rev A D ] W = [ 1 A ]
	;; Put reversed high nibble of 	nibble register to low nibble of W
	;; through high nibble of scratch
	;; and push low nibble of 0x23 to its high nibble
	;; n.b. putting 0x10 to initialize scratch would work too, but
	;; same instruction count as andlw/iorlw
	call rotate_2bit
	call rotate_2bit
	swapf	scratch, w
	andlw	0xf
	iorlw	0x10 		; enable on
	movwf	PORTC
	bcf	PORTC, 0x4	; enable off
	return
rotate_2bit:
	call rotate_bit
rotate_bit:
	rlf	nibble, f
	rrf	scratch, f
	return

;;; Waiting
wait_1:
	;; wait 256*W*constant times
	clrf	duration
	call	short_wait
	addlw	0xff
	btfsc	0x3, 0x2
	return
	goto	wait_1
short_wait:
	;; wait duration * constant times
	decfsz	duration, f
	goto	short_wait
	return

;;; Simple commands
slow_shift_right:
	movlw	0
	call	wait_1
	movlw	0x1c
send_command:
	bcf	PORTA, 0x2
	goto	put

;;; 
print_text_from_prom:
	bsf	PORTA, 0x2
print_from_prom:
	;; Put bytes from prom until 0 is seen
	bsf	0x3, 0x5 ;; bank 1
	movwf	EEADR
next_char:
	bsf	0x3, 0x5 ;; bank 1
	bsf	EECON1, 0
	incf	EEADR, f
	movf	EEDAT, w
	bcf	0x3, 0x5 ;; bank 0
	btfsc	0x3, 0x2
	return
	call	put
	goto	next_char

;;; Initialization
init_lcd:
	clrf	PORTA		; commands follow
	clrf	PORTC
	movlw	0x14
	call	wait_and_reset
	movlw	0x6
	call 	wait_and_reset
	movlw	0x1f
	movwf	duration
	call	short_wait
	call 	send_reset
	movlw	0x3
	call    wait_1
	movlw	0x20		; set 4bit
	call emit_w_nibble
	movlw	LOW(initcode)
	goto print_from_prom
wait_and_reset:
	call wait_1
send_reset:
	movlw	0x30		; set 8bit
emit_w_nibble:
	movwf	nibble
	goto	push_high_nibble
DEEPROM	code
hello:	de	"Hello world\0"
row:
	de	"2nd row\0"
initcode:
	de 0x28			; 4bit 2 lines
	de 8			; display off, no cursor, no blink
	de 1			; clear
	de 6			; increment, dont shift display
	de 0xc			; display on, no cursor, no blink
	de 0
	end
