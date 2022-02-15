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
M0PosX		byte
M0PosY		byte
Score           byte            ; score is in mem pos = 85 and timer = 86 ;-) store as BCD
Timer           byte            ; importante es que timer esta pegado a score.. bit shift calcs.(like a bool) . store as BCD
Temp            byte            ; auxiliar var to handle digits.
OnesDigitOffset word            ; el offset para lookup table de la unidad
TensDigitOffset word            ; el offset para lookup table de la decena
P0SprPtr	word
P0ColPtr	word
P1SprPtr	word
P1ColPtr	word
P0AnimOffset	byte
Random          byte    ; random number generate
ScoreSprite     byte
TimerSprite     byte
TerrainColor	byte
RiverColor		byte

P0_HEIGHT =	9
P1_HEIGHT =	9
DIGITS_HEIGHT = 5

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
        lda #83	
	sta P1PosY
        lda #%11010100
        sta Random
        lda #0
        sta Score
        sta Timer               ; this gonna be the hiscore
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACRO : DRAW_MISSILE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	MAC DRAW_MISSILE
		lda #%00000000
		cpx M0PosY
        	bne .SkipDrawing
.DrawMissile:
	lda #%00000010
        inc M0PosY
.SkipDrawing:	
	sta ENAM0
	ENDM
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
;; Start a new frame by configuring VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
	lda #2
	sta VBLANK
	sta VSYNC
		
	REPEAT 3
	sta WSYNC 		; 3 time WSYNC
	REPEND
	
	lda #0
	sta VSYNC
        
	REPEAT 33               ; sacamos 4 ciclos porque seran usados por el calculo de la posicion X del player 0.	
	sta WSYNC
	REPEND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and task init for Player0 X position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda P0PosX
        ldy #0
        jsr SetActorPosX	; set player0 X position
        
        lda P1PosX
        ldy #1
        jsr SetActorPosX
	
        lda M0PosX
        ldy #2
        jsr SetActorPosX
        
        jsr CalcDigitsOffset
        
        sta WSYNC
        sta HMOVE

    	lda #0
	sta VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 20 lines visibles --> Using For Scoreboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #0
        sta COLUBK
        sta PF0
        sta PF1
        sta PF2
        sta GRP0
        sta GRP1
        sta CTRLPF              ; no reflect the Playfield
        

        lda #$1E
        sta COLUPF
        
        ldx #DIGITS_HEIGHT
.ScoreDigitLoop:
        ldy TensDigitOffset
        lda Digits,Y
        and #$F0
        sta ScoreSprite

        ldy OnesDigitOffset
        lda Digits,Y
        and #$0F

        ora ScoreSprite
        sta ScoreSprite
        sta WSYNC
        sta PF1

        ldy TensDigitOffset+1
        lda Digits,Y
        and #$F0
        sta TimerSprite

        ldy OnesDigitOffset+1
        lda Digits,Y
        and #$0F

        ora TimerSprite
        sta TimerSprite
        
        jsr Waste12Cycle

        sta PF1
        ldy ScoreSprite
        sta WSYNC

        sty PF1
        inc TensDigitOffset
        inc TensDigitOffset+1
        inc OnesDigitOffset
        inc OnesDigitOffset+1

        jsr Waste12Cycle

        dex 
        sta PF1
        bne .ScoreDigitLoop

        sta WSYNC

	lda #0
	sta PF0
	sta PF1
	sta PF2
	sta WSYNC
	sta WSYNC
	sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start 84 lines visibles --> Using 2-line Kernel : (192 -20) /2 = 84
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
VisibleLineLoop:

	lda RiverColor ;#$84
	sta COLUBK	
        lda TerrainColor ;#$C6
	sta COLUPF	
        lda #%00000001
	sta CTRLPF

        lda #$F0
	sta PF0			; enable reflect for the Terrain.
	
        lda #$FC 
	sta PF1
	
        lda #0
	sta PF2

	ldx #85
.GameLineLoop:
	DRAW_MISSILE
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
.P0UpPressed:
        lda P0PosY
        cmp #70			; Up Bound
        bpl CheckP0Down
        inc P0PosY
        lda #0
        sta P0AnimOffset
        
CheckP0Down:
	lda #%00100000
        bit SWCHA
        bne CheckP0Left
.P0DownPressed:
        lda P0PosY
        cmp #0			; bottom bound
        bmi CheckP0Left
        dec P0PosY
        lda #0
        sta P0AnimOffset

CheckP0Left:
	lda #%01000000
        bit SWCHA
        bne CheckP0Right
.P0LeftPressed:
	lda P0PosX
        cmp #33			; left bound
        bmi CheckP0Right
	dec P0PosX
        lda P0_HEIGHT
        sta P0AnimOffset
        
CheckP0Right:
	lda #%10000000
        bit SWCHA
        bne CheckP0button
.P0RightPressed:
        lda P0PosX
        cmp #100		; Right Bound
        bpl CheckP0button
        inc P0PosX
        lda P0_HEIGHT
        sta P0AnimOffset
        
CheckP0button:
	lda #%10000000
        bit INPT4		
        bne NoInput
.P0ButtonPressed:
        lda P0PosX
        clc
        adc #5
        sta M0PosX
        
        lda P0PosY
        clc
        adc #5
        sta M0PosY        
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
        sed
        clc
        lda Timer
        adc #1
        sta Timer
        cld

EndUpdateEnemyPos:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check colision for P0 and enemy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0P1Collision:
        lda #%10000000
        bit CXPPMM      ; check collision between P0 and P1
        bne .CollisionP0P1

        lda #$C6
	sta TerrainColor
	lda #$84
	sta RiverColor

        jmp CheckM0P1Collision
.CollisionP0P1:
        jsr GameOver

CheckM0P1Collision:
        lda #%10000000
        bit CXM0P      ; collision M0 con P1 
   	bne .CollisionM0P1
    	jmp EndCheckCollision
.CollisionM0P1:
        sed
        clc
        lda Score
        adc #1
        sta Score
        cld
        lda #0
        sta M0PosY
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
        lda #$4E
        sta TerrainColor
	lda #$41
	sta RiverColor
        lda #0
        sta Score
        ;;; TODO : check if score is > than hiscore -> hiscore = score and reset.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subroutine for handle Scoreboard display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; for the low nibble we need to multiply by 5 (height of the digit in the lookuptable)
; N*5 = (N*2*2)+N -> left shift op.
; for the high nibble -> since is base 16 we need to divide it. -> right shift
; (N/16)*5 = (N/2/2)+(N/2/2/2/2) = example : 4/16*5 = 4/4 + 4/16 = 20/16 = 4*4/4*4 + 4/16 = (20/16 = 20/16)
;  [0,0,0,0][0,0,0,0]
;    Score     Timer
CalcDigitsOffset subroutine
        ldx #1                  ; X regs is the loop counter
.PrepareScoreLoop               ; this will loop twice = X=1 and then X=0
        lda Score,x             ; load A with Timer (x=1) or Score (x=0) -> this is why they are one after the other.
        and #$0F                ; 0F = 00001111 -> 11011011 + 00001111 = 1011 ;-) So only the unit value.
        sta Temp                ; save the value of A into Temp var.
        asl                     ; N*2
        asl                     ; N*4
        adc Temp                ; calculate low nibble
        sta OnesDigitOffset,X   ; set the oneDigitOffset

        lda Score,x
        and #$F0
        sta Temp
        lsr                     ; N/2
        lsr                     ; N/4
        sta Temp
        lsr                     ; N/8
        lsr                     ; N/16                      
        adc Temp
        sta TensDigitOffset,X   ; set TensDigitOffset

        dex                     ; X--
        bpl .PrepareScoreLoop   ; while X >=0 , loop .

        rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; waste 12 cycles subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
Waste12Cycle subroutine
	rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup table for the player graphics bitmap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
    .byte %01110011          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

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