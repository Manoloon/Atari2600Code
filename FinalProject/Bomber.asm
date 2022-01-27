
	processor 6502
        include "vcs.h"
        include "macro.h"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start an uninitialized segment at $80 for var declaration.
;; We have memory from $80 to $FF to work with, minus a few at
;; the end if we use the stack.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	seg.u Variables
    org $80
P0PosX 		byte
P0PosY		byte
P1PosX		byte
P1PosY		byte
P0SprPtr	word
P0ColPtr	word
P1SprPtr	word
P1ColPtr	word

P0_HEIGHT =	9
P1_HEIGHT =	9

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start our ROM code segment starting at $F000.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	seg Code
	org $f000

reset:
	CLEAN_START
        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda #0
	sta P0PosX
	lda #10
	sta P0PosY 
        lda #0
        sta P1PosX
        lda #83
	sta P1PosY
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize pointers to the correct lookup table addresses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        lda #<P0Sprite
        	sta P0SprPtr
        lda #>P0Sprite
    		sta P0SprPtr+1
                
        lda #<P1Sprite
        	sta P1SprPtr
        lda #>P1Sprite
        	sta P1SprPtr+1
                
        lda #<P0Color
        	sta P0ColPtr
        lda #>P0Color
        	sta P0ColPtr+1
                
        lda #<P1Color
        	sta P1ColPtr
        lda #>P1Color
        	sta P1ColPtr+1
        
NextFrame:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and task init for Player0 X position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda P0PosX
        ldy #0
        jsr SetActorPosX	; set player0 X position
        
        lda P1PosX
        ldy #1
        jsr SetActorPosX
        
        sta WSYNC
        sta HMOVE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by configuring VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
	lda #02
	sta VBLANK
	sta VSYNC
		
	REPEAT 3
		sta WSYNC 		; 3 time WSYNC
	REPEND
	
	lda #0
	sta VSYNC
        
	REPEAT 35	
		sta WSYNC
	REPEND
	
    	lda #0
	sta VBLANK
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start 96 lines visibles --> Using 2-line Kernel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
VisibleLineLoop:

	lda #$84
	sta COLUBK
	
        lda #$C6
	sta COLUPF
	
        lda #%00000001
	sta CTRLPF
	
        lda #$F0
	sta PF0			; enable reflect for the green part.
	
        lda #$FC 
	sta PF1
	
        lda #0
	sta PF2

	ldx #96
.GameLineLoop:
.CheckP0Bounds:
	txa
        sec
        sbc P0PosY
        cmp P0_HEIGHT
        bcc .DrawP0Sprite
        lda #0
.DrawP0Sprite:
	clc
	tay
        lda (P0SprPtr),y
        sta WSYNC
        sta GRP0
        lda (P0ColPtr),y
        sta COLUP0
.CheckP1Bounds:
	txa
        sec
        sbc P1PosY
        cmp P1_HEIGHT
        bcc .DrawP1Sprite
        lda #0
.DrawP1Sprite:
	tay
        lda #%0000101
        sta NUSIZ1
        lda (P1SprPtr),y
        sta WSYNC
        sta GRP1
        lda (P1ColPtr),y
        sta COLUP1
        
	dex
	bne .GameLineLoop
        
        lda #0
        sta WSYNC
	
       	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #2				
		sta VBLANK
	
    REPEAT 30
		sta WSYNC
	REPEND
	lda #0
        sta VBLANK
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control for Player0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
	lda #%00010000
        bit SWCHA
        bne CheckP0Down
        inc P0PosY
        
CheckP0Down:
	lda #%00100000
        bit SWCHA
        bne CheckP0Left
        dec P0PosY

CheckP0Left:
	lda #%01000000
        bit SWCHA
        bne CheckP0Right
	dec P0PosX
        
CheckP0Right:
	lda #%10000000
        bit SWCHA
        bne NoInput
        inc P0PosX
        
NoInput:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End of the frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        jmp NextFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subroutine for actor horizontal movement fine offset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A is the target x-coord pos in pixels of our actor
;;; Y is the actor type : (0:Player0,1:player1,2:Missile0,3:Missile1,4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetActorPosX subroutine
	sta WSYNC
	sec
.Div15Loop:
	sbc #15
	bcs .Div15Loop
	eor #7
	asl
	asl
	asl
	asl
	sta HMP0,Y
	sta RESP0,Y
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup table for the player graphics bitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

P0Sprite:		; Jet
	.byte %000000000; zero line pattern
        .byte #%00011100;$AA
        .byte #%00001000;$0E
        .byte #%01111111;$A8
        .byte #%00111110;$0A
        .byte #%00011100;$0C
        .byte #%00001000;$AC
        .byte #%00001000;$08
        .byte #%00000000;$0E
        
P3Sprite:		; navecita
	.byte %000000000; zero line pattern
        .byte #%00000000;$AA
        .byte #%00000000;$0E
        .byte #%01001001;$A8
        .byte #%01111111;$0A
        .byte #%01011101;$0C
        .byte #%01001001;$AC
        .byte #%00100010;$08
        .byte #%00000000;$0E
        
P0SpriteRight:
        .byte #%00001100;$AA
        .byte #%00001000;$0E
        .byte #%00001111;$A8
        .byte #%00001110;$0A
        .byte #%00001100;$0C
        .byte #%00001000;$AC
        .byte #%00001000;$08
        .byte #%00001000;$0E
P0SpriteLeft:
        .byte #%00011000;$AA
        .byte #%00001000;$0E
        .byte #%01111000;$A8
        .byte #%00111000;$0A
        .byte #%00011000;$0C
        .byte #%00001000;$AC
        .byte #%00001000;$08
        .byte #%00001000;$0E
           
P1Sprite:
        .byte %00000000
        .byte %00001000
        .byte %00011100
        .byte %00001000
        .byte %00001000
        .byte %00011100
        .byte %00111110
        .byte %01111111
        .byte %00101010

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup table for the player colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

P0Color:
        .byte #$AA;
        .byte #$0E;
        .byte #$A8;
        .byte #$0A;
        .byte #$0C;
        .byte #$AC;
        .byte #$08;
        .byte #$0E;

P1Color:
    	byte #$00
    	byte #$60
    	byte #$60
    	byte #$60
    	byte #$60
    	byte #$62
    	byte #$62
    	byte #$64
    	byte #$12
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	org $fffc
        .long reset	; reset vector
