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
	__config _INTRC_OSC_NOCLKOUT & _WDT_OFF & _MCLRE_ON

	extern init_lcd, print_text_from_prom, send_command, slow_shift_right, put_reg
	extern put_reg_indf, wait_1
	
	udata_shr
intr_w:	res 1
intr_status:
	res 1
top_stack:
	res 1
intr_fsr:
	res 1
current:
	res 1
	
	code
	org 0
	goto init
;;; interrupt code
	org 4
	movwf intr_w
	movf STATUS, w
	movwf intr_status
	movf intr_status, w
	bcf STATUS, RP0
	movwf FSR
	;; put counter 1 to bottom of stack
	movlw 0x40
	subwf top_stack, w
	btfss STATUS, C 	; below?
	goto stack_ok
	movlw 0x60
	movwf top_stack
stack_ok:	
	decf top_stack, f
	movf top_stack, w
	movwf FSR
	movf TMR1H, w
	movwf INDF
	decf top_stack, f
	decf FSR, f
	movf TMR1L, w
	movwf INDF
	clrf TMR1L
	clrf TMR1H
	bcf INTCON, RAIF
	;; cleanup
	movf PORTA, W
	movf intr_fsr, w
	movwf FSR
	movf intr_status, W
	movwf STATUS
	swapf intr_w, f
	swapf intr_w, w
	retfie
;;; complete start
init:
	movlw 7
	movwf   CMCON
	clrf TMR1H
	clrf TMR1L
	clrf PORTA
	clrf PORTC
	bsf STATUS, RP0	;bank 1
	movlw 0x3b 	; pin2 is out, rest in
	movwf TRISA ^ 0x80
	clrf TRISC ^ 0x80	; all out
	movlw 0x30
	movwf IOCA
	bcf STATUS, RP0 ;; bank 0
	;; Count internal clock. Interrupt will use it.
	movlw (1<<TMR1ON) | (1<<NOT_T1SYNC)
	movwf T1CON  ; Clock on gate
	movlw (1<<GIE) | (1<<RAIE)
	movwf INTCON
main_loop:
	;; reset stack
	movlw 0x60
	movwf top_stack
	call init_lcd
	movlw LOW(hello)
	call print_text_from_prom
	movlw 0xc0 		; set ddram address = 0x40
	call    send_command ; i.e., move to 2nd row
	movlw LOW(row)
	call print_text_from_prom
	call slow_shift_right
	call slow_shift_right
	call slow_shift_right
	movlw 0x0 		; wait for watchdog
	call wait_1
	call init_lcd
;;; Put numbers from top of stack down
	movlw 0x60
	movwf current
	movf top_stack, w
print_next:
	decf current
	movf current, w
	call put_reg
	movf current, w
	subwf top_stack, w
	btfsc STATUS, Z
	goto all_printed
	goto print_next
all_printed:
	call long_wait
	goto main_loop
long_wait:
	call $+1
	call $+1
	movlw 0x0 		; wait for watchdog
	call wait_1
	
DEEPROM code
hello:	de "Hello world\0"
row:
	de "2nd row\0"

	end
