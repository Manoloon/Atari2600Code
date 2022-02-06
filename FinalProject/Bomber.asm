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
P0AnimOffset	byte
Random          byte    ; random number generate

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

	lda #68
	sta P0PosX
	lda #10
	sta P0PosY 
        lda #60
        sta P1PosX
        lda #0	
	sta P1PosY
        lda #%11010100
        sta Random
        
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
        
	REPEAT 37	
	sta WSYNC
	REPEND
	
    	lda #0
	sta VBLANK
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 20 lines visibles --> Using For Scoreboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #0
        sta PF0
        STA PF1
        STA PF2
        STA GRP0
        STA GRP1
        STA COLUPF
	REPEAT 20	
		sta WSYNC
	REPEND       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start 84 lines visibles --> Using 2-line Kernel : (192 -20) /2 = 84
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

	ldx #84
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
        adc P0AnimOffset
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
        sta P0AnimOffset

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
        lda #0
        sta P0AnimOffset
        
CheckP0Down:
	lda #%00100000
        bit SWCHA
        bne CheckP0Left
        dec P0PosY
        lda #0
        sta P0AnimOffset

CheckP0Left:
	lda #%01000000
        bit SWCHA
        bne CheckP0Right
	dec P0PosX
        lda P0_HEIGHT
        sta P0AnimOffset
        
CheckP0Right:
	lda #%10000000
        bit SWCHA
        bne NoInput
        inc P0PosX
        lda P0_HEIGHT
        sta P0AnimOffset
        
NoInput:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calculate movement for enemy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
UpdateEnemyPos:
	lda P1PosY
        clc
        cmp #0
        bmi .ResetEnemyPos
        dec P1PosY
        jmp EndUpdateEnemyPos
.ResetEnemyPos:
	jsr GetRandomEnemyPos

EndUpdateEnemyPos:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check colision for P0 and enemy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0P1Collision:
        lda #%10000000
        bit CXPPMM      ; check collision between P0 and P1
        bne .CollisionP0P1
        jmp CheckP0PFCollision
.CollisionP0P1:
        jsr GameOver

CheckP0PFCollision:
        lda #%10000000
        bit CXP0FB      ; collision P0 
        bne .CollisionP0PF
        jmp EndCheckCollision
.CollisionP0PF:
        jsr GameOver

EndCheckCollision:      ; fallback
        sta CXCLR           ; clear collision flag
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GameOver Subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GameOver subroutine
        ; clear score
        lda #$42
        sta COLUBK
        rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subroutine for Enemy position using LFSR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate LFSR
;;; divide the random value by 4 to limit size to match river zone
;;; add 30 to compensate the left green zone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GetRandomEnemyPos subroutine
        lda Random              ; start LFSR
        asl 
        eor Random
        asl
        eor Random
        asl
        asl
        eor Random
        asl
        rol Random              ; End LFSR

        lsr
        lsr                     ; this two lsr are the divide by 4
        sta P1PosX
        lda #30                 ; no podra spawnear en PF Green
        adc P1PosX
        sta P1PosX              ; set the new value for X position of enemy
        
        lda #96
        sta P1PosY              ; set Y position for enemy
        rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup table for the player graphics bitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;---Graphics Data from PlayerPal 2600---

P0Sprite
	.byte #%00000000;$1C
        .byte #%00000000;$1C
        .byte #%00000000;$1C
        .byte #%00010100;$1C
        .byte #%01111111;$1C
        .byte #%00111110;$1C
        .byte #%00011100;$1C
        .byte #%00001000;$1C
        .byte #%00001000;$1C
P0SpriteRight
	.byte #%00000000;$1C
        .byte #%00000000;$1C
        .byte #%00000000;$1C
        .byte #%00001100;$1C
        .byte #%00111110;$1C
        .byte #%00011110;$1C
        .byte #%00011100;$1C
        .byte #%00001000;$1C
        .byte #%00001000;$1C
;---End Graphics Data---
           
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

;---Color Data from PlayerPal 2600---

P0Color
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
P0ColorF2
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
P0ColorF3
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
        .byte #$1C;
;---End Color Data---

P1Color:
    	byte #$00
    	byte #$01
    	byte #$10
    	byte #$10
    	byte #$10
    	byte #$12
    	byte #$02
    	byte #$14
    	byte #$02
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete ROM size
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	org $fffc
        .long reset	; reset vector