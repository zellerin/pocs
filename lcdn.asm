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

	extern init_lcd, print_text_from_prom, send_command, slow_shift_right, put_reg
	
	udata_shr
intr_w:	res 1
intr_status:
	res 1

	code
	org 0
	goto	init
;;; interrupt code
;;; unused at the moment
	org 4
	movwf	intr_w
	movf	STATUS, w
	movwf	intr_status
	movf	intr_status, w
	movwf	STATUS
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
	bsf	STATUS, RP0	;bank 1
	movlw	0x3b 	; pin2 is out, rest in
	movwf	TRISA ^ 0x80
	clrf	TRISC ^ 0x80	; all out
	bcf	STATUS, RP0 ;; bank 0
	;; timer enabled, no prescaler, internal source
	movlw	0x7
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
DEEPROM	code
hello:	de	"Hello world\0"
row:
	de	"2nd row\0"

	end
