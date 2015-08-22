  ; Plops: v0.5

				Processor 6502		; set processor
				include "vcs.h"		; include 2600 files
				include "macro.h"
				include "graphics.h"
		
SPRITE_HEIGHT	= 13
SCREEN_HEIGHT	= 192

NUMBER_LINES	= 6
LINE_HEIGHT	= 7

XMIN      	= 8
XMAX     	= 150

STATE_SPLASH	= %00000001
STATE_GAME	= %00000010
STATE_GETREADY 	= %00000100
STATE_DYING	= %00001000
STATE_DEAD	= %00010000

GRAVITY		= 6

SPRITE_PLAYER0	= 0
SPRITE_PLAYER1	= 1
SPRITE_MISSILE0	= 2
SPRITE_MISSILE1	= 3
SPRITE_BALL	= 4


; color constants
COL_SKY = $AD
COL_POND = $A8
COL_NEARBANK = $C2
COL_MIDBANK = $C1
COL_FARBANK = $C0
COL_ENERGYOFF = $1f
COL_ENERGYON = $3F
COL_HULLS = $02
COL_SCORE = $A0
COL_STONE = $0
COL_SUN = $1f
COL_SPLASH = $A0

BLACK                   = $00
WHITE                   = $0E
YELLOW                  = $10
RED                     = $30
PURPLE                  = $60
BLUE                    = $90
GREEN_BLUE              = $A0
GREEN                   = $C0

; Game status flags
GS_HasRock		= %00000001
GS_RockCollision	= %00000010
GS_ResetRockPlease	= %00000100
GS_BaddieCollision	= %00001000

; Enemy status flags
ES_Direction	        = %00000001
ES_Active               = %00000010

; values for ENAMx and ENABL
DISABLE_BM        	= %00
ENABLE_BM         	= %10

SCORE_HEIGHT		= 5
SpriteAnimSpeed 	= 6

INIT_LEVEL_TIME		= 40
	
				SEG.U vars
				ORG $80
	
GameState		ds 1 ; Splash screen / running / 
GameStatus		ds 1 ; bit 0 = player holding rock etc - se GS_ constants

CurrLevel		ds 1
LivesLeft		ds 1
LevelTimer		ds 2
TimerTemp  		ds 1
	
CurrTone		ds 1
CurrFreq		ds 1
CurrVolume		ds 1
		
PlayerX			ds 1
PlayerY			ds 1
PlayerDir		ds 1
SpriteAnim		ds 1
SpriteTablePtr		ds 2
PlayerJumpTop		ds 2
PlayerWinding		ds 1
PlayerJumpSpeed		ds 1

ScoreHeight		ds 1
ScoreSpritePtr		ds 12

EnemyX			ds NUMBER_LINES*2
EnemyFC_X		ds NUMBER_LINES
EnemySpeed		ds NUMBER_LINES*2
EnemyStatus		ds NUMBER_LINES
Collision		ds NUMBER_LINES

EnemyHits		ds 1

StoneY			ds 2		; 16bit for subpixel
StoneSpeed		ds 2
StoneX 			ds 1
StonePower		ds 2
StoneLand		ds 1
StoneAnim		ds 2

StonePtr		ds 2
StoneOffset		ds 1		; Um, line to draw too?
StoneLine		ds 1		; Which line to draw
CurrLineY		ds 1
NextLineY		ds 1

; Bad guy down the bottom of the screen
BaddieX			ds 1
BaddieSpeed		ds 1
BaddieFrame		ds 1

NextRockX		ds 1
NextRockTime	ds 1
NextRockPtr		ds 2

Score			ds 2

frameCount		ds 1
LineCounter		ds 1
CurrScanLine	ds 1

randomSeed		ds 1

temp			ds 1
temp2			ds 2

MusicTimer		ds 1
MusicIndex		ds 1
MusicTimer2		ds 1
MusicIndex2		ds 1
MusicCounter	ds 1

				echo "----",($100 - *) , "bytes of RAM left"
; ---------------------------------------------------------------

				SEG code			; 
				ORG $F000
			
Reset				

    ; Clear RAM and all TIA registers

				ldx #0 
				lda #0 
Clear       	sta 0,x 
	            inx 
	            bne Clear

	            ldx #$FF
	            txs
				
;------------------------------------------------------------------

ResetGame
	; Set initial values
				lda #STATE_SPLASH
				sta GameState
				
				lda #%00000001		; Set reflection bit
				sta CTRLPF
			
				; disable missile
				lda #DISABLE_BM
				sta ENAM1
				sta ENAM0
							
				; Messin round with sprite copies
				lda #%0001000
				sta NUSIZ0
				;sta NUSIZ1
							
				jsr SetupMusic
				
							
; ==== Splash Screen ==== ---------------------------------------------------------------------------------
				lda #0
				sta frameCount
				sta PlayerWinding ; using to detect Fire released
				
				; Set up splash gfx
				lda #11
				sta ScoreHeight
				
; ==== Logo Pointers ====
				ldx #$B ; For 6 digits (2 bytes each)
SetupLogo
				; Store High byte
				lda #>Digits
				sta ScoreSpritePtr,x
				dex
				
				;Store low byte
				lda #<Digits
				sta ScoreSpritePtr,x
				dex
				
				bpl SetupLogo
								
				;lda #<BoatFrame0
				sta ScoreSpritePtr+$a
				sta ScoreSpritePtr+$0
				
								
				lda #COL_SKY
				sta COLUBK
SplashScreen
				jsr DoVerticalBlank
						
; ==== Splash Screen logic ====--------------------------------------------------------------------------

				lda #<Digits
				adc #11
				sta ScoreSpritePtr+2
				
				lda #<Digits
				adc #36
				sta ScoreSpritePtr+8
								
; ==== Check for Fire ====				
				lda INPT4		; Check fire button
				bmi NoSplashFire
				lda #1
				sta PlayerWinding
				jmp NoSplashRelease
NoSplashFire
				lda PlayerWinding
				beq NoSplashRelease
				; Fire Released
				lda #STATE_GETREADY
				sta GameState
NoSplashRelease

				jsr PlayMusic
				lda MusicCounter
				cmp #4
				bcc NoTrackTwo
				jsr PlayMusic2
NoTrackTwo
				lda MusicCounter
				cmp #8
				bcc MoreSong
				lda #0
				sta MusicCounter
				sta MusicTimer2
				sta MusicIndex2
				sta AUDV0
MoreSong				
								
				lda MusicIndex
				cmp #1
				bne AfterLogoColour

				jsr NextRandom
				sta COLUP0
				lsr
				sta COLUP1	
AfterLogoColour			
				
				jsr WaitForVBlankEnd
				
; ==== Splash Screen Kernel ==== ---------------------------------------------------------------------------
				lda #0
				sta CurrScanLine
SplashKernel
				ldx #SCREEN_HEIGHT/2-20
SplashTop
				inc CurrScanLine
				sta WSYNC
				dex
				bne SplashTop
				
				lda #0
				sta COLUBK

SplashLogo				
				lda #6
				sta ScoreHeight
						
				sta WSYNC
				jsr Draw48Pixer
							
SplashBottom
				sta WSYNC
				inc CurrScanLine
				
				lda #COL_SKY
				sta COLUBK
				
				lda CurrScanLine
				cmp #SCREEN_HEIGHT-11
				bcc SplashBottom
				
				jsr DoOverscan
				
				lda STATE_SPLASH
				bit GameState
				bne SplashScreen
				
				jsr StopMusic				; Turn off the killa tune
				

; ==== Start Game ==== --------------------------------------------------------------------------------				
				
InitGame

				lda #3
				sta LivesLeft
								
			
; Sprite Pointer Setups
				; Set up player sprite data
				lda #<Sprite0Frame1	; Point to the sprite data (high byte)
				sta SpriteTablePtr
				lda #>Sprite0Frame1	; Low byte
				sta SpriteTablePtr+1
				sta PlayerJumpTop+1
				
				; Stone sprite data
				lda #<StoneFrame3
				sta StonePtr
				lda #>StoneFrame3
				sta StonePtr+1
				
				; Next Stone sprite pointer
				lda #<RockOn
				sta NextRockPtr
				lda #>RockOn
				sta NextRockPtr+1
				
				; Set up the digit pointers
				ldx #$B ; For 6 digits (2 bytes each)
SetupDigits
				; Store High byte
				lda #>Digits
				sta ScoreSpritePtr,x
				dex
				
				;Store low byt
				lda #<Digits
				sta ScoreSpritePtr,x
				dex
				
				bpl SetupDigits


				; First init of enemies
				ldx #NUMBER_LINES-1	; Load number of groups
GameInitEnemy 		
				
				lda EnemyStatus,x
				ora #ES_Active
				sta EnemyStatus,x
				
				dex
				bpl GameInitEnemy
				
				lda #0
				sta EnemyHits

				
; ==== Initialise for new level/life ==== ----------------------------------------------------------		
InitLevel

				lda #0				; Set initial Player1 direction
				sta PlayerDir
				sta PlayerWinding
				sta PlayerJumpSpeed
				
				lda #GS_ResetRockPlease
				sta GameStatus
				
				lda #STATE_GETREADY
				sta GameState
				
				lda #30
				sta NextRockX
																
				lda #14				; Set vertical pos
				sta PlayerY
				lda #50				; Horizontal pos
				sta PlayerX
				
				
				; Anim attributes
				lda #SpriteAnimSpeed
				ora #%00010000		; High byte is curr frames
				sta SpriteAnim
				
				; Stone attributes
				lda #XMIN-1
				sta StoneX
				lda #0
				sta StoneLand
				sta StonePower
				sta frameCount
											
; ====
				lda #0
				sta LevelTimer
				lda #INIT_LEVEL_TIME
				sta LevelTimer+1
			
				
; ==== Initialise  Enemy Attributes ==== --------------------------------------------------------

				ldx #NUMBER_LINES-1	; Load number of groups
InitEnemy 		
				jsr NextRandom
				and #%00001111
		
				asl
				asl

				stx temp			; get x		
				asl temp			; X times 2
				ldy temp           
				sta EnemyX+1,y 		; Load the X pos into high byte
				
				lda #0
				sta EnemyX,y

				; Setup speed			
				jsr NextRandom
				sta EnemySpeed,y
				and #%00000001
				;sta EnemySpeed+1,y
				
				lda #$0
				sta Collision,x
				
				dex
				bpl InitEnemy
									
; ==== Start of a new frame ==== -----------------------------------------------------------------

StartOfFrame						
				jsr DoVerticalBlank
				
				lda GameState
				and #STATE_GETREADY
				beq GameLogic
				
				jmp WaitScreen
				
; -------------------------------  ==== Main Game Logic ==== -----------------------------------
GameLogic					
								
; ==== Joystick stuff ==== -----------------------------------------------------------------------
				lda StoneX		; If stone already goin, forget firebutton
				cmp #XMIN-1
				bne FireOver
				
				lda GameStatus
				bit #GS_HasRock
				beq FireOver
				
				
				lda INPT4		; Check fire button
				bmi NoFire
Fire
				lda StonePower
				cmp #50			; Max Power!
				beq FireOver
				
				lda #1				; Set player to "winding up" frame
				sta PlayerWinding
						
				inc StonePower
				inc StonePower
				jmp FireOver
				
NoFire
				lda StonePower
				cmp #0
				beq FireOver
				
; ==== Stone Fired ====
StoneFired 							; Fire button just let go - Fire the stone!
				lda StonePower
				
				sta StoneSpeed		; Set speed = stonepower * 8
				asl StoneSpeed
				rol StoneSpeed+1
				asl StoneSpeed
				rol StoneSpeed+1
				asl StoneSpeed
				rol StoneSpeed+1
				
				lda StonePower
				lsr
				lsr
				lsr
				lsr
				sta StoneAnim ;store anim count
				
				lda #0
				sta StonePower+1

				lda #2				; Set Player in "Throw" framw
				sta PlayerWinding
										
				lda PlayerX			; Centre stone to player
				clc
				ldx PlayerDir
				bne OffsetLeft
				sbc #4
				jmp OffsetStone
OffsetLeft
				adc #4
OffsetStone			
				sta StoneX
				
				lda #50				; Initial Y position
				sta StoneY+1
FireOver
				
				lda PlayerWinding	; If winding up or throwing, then skip left n right stuff!
				bne WindingUp

; ==== Joystick Movement ====
				lda #%00100000
				bit SWCHA
				beq MoveDown
				bpl MoveRight		;right
				bvc MoveLeft		;left
				jmp MoveUp
MoveLeft
				lda #0
				sta PlayerDir
				
				; X Check
				lda PlayerX
				cmp #XMIN
				beq EndMove
				
				dec PlayerX
				dec SpriteAnim
				jmp MoveUp
MoveRight
				lda #1
				sta PlayerDir
				
				; X Check
				lda PlayerX
				cmp #XMAX-5
				beq EndMove
				inc PlayerX
				dec SpriteAnim
				jmp MoveUp
MoveDown
				lda GameState
				eor #STATE_GETREADY
				sta GameState
				
				lda #100
				sta TimerTemp
				;sta GameStatus
				
				;lda #%00000000   ;a 1 in D3 of REFP0 says make it mirror
				;lda #%00001000   ;a 0 in D3 of REFP0 says no
				;sta REFP0
				;dec PlayerY
				jmp EndMove
MoveUp
				lda #%00010000
				bit SWCHA
				bne EndMove
				
				;inc PlayerY
				
				; PRessed up
				lda PlayerJumpSpeed
				cmp #1
				bcs EndMove

				; Set player jumping
				lda #21
				sta PlayerJumpSpeed
								
EndMove
				lda PlayerJumpSpeed
				beq PlayerAnimation
				sec
				sbc #1
				sta PlayerJumpSpeed
				adc #13
				sta PlayerY
				
; ==== Player animation ==== ---------------------------------------------------------------------
				jmp PlayerAnimation
WindingUp
				lda PlayerWinding
				cmp #1
				bne ChuckinIt
				ldx #<Sprite0Winding
				jmp CalcYPos

PlayerAnimation

				lda PlayerJumpSpeed
				bne JumpingMan
								
				lda SpriteAnim
				and #%00001111
				cmp #0
				bne FinishAnim
				
				lda SpriteAnim
				lsr	; Get highbyte
				lsr
				lsr
				lsr
				
				eor #$1
				cmp #1
				bne WalkFrame2
				
				ldx #<Sprite0Frame1
				jmp StoreWalkFrame
WalkFrame2
				ldx #<Sprite0Frame2
StoreWalkFrame
				; Store Now done below!
DoneWalkAnim
				asl ; Move highbyte back
				asl
				asl
				asl
				ora SpriteAnimSpeed
				sta SpriteAnim
				jmp CalcYPos
				
JumpingMan
				ldx #Sprite0Jump1
				
				; Reset anim
				lda #%00010000
				ora SpriteAnimSpeed
				sta SpriteAnim
				jmp CalcYPos

ChuckinIt
				ldx #<Sprite0Throw
				lda #0
				sta PlayerWinding
CalcYPos
				txa
				ldx PlayerJumpSpeed
				cpx #0
				beq StorePlayerPtr
				
				clc
				adc #28
				sec
				sbc JumpCurve,x
StorePlayerPtr
				sta SpriteTablePtr		
				clc
				adc #14
				sta PlayerJumpTop
				
FinishAnim
						
				

; ==== Collision Detection ==== -----------------------------------------------------------------------
				;lda #%10000000
				;bit CXPPMM
				;beq NoCollision
				;lda #PlayerY		;must be a hit! load in the YPos...
				;sta COLUBK		;and store as the bgcolor
				;jmp doneColl
;NoCollision
				;lda #0
				;sta COLUBK
;doneColl
				;sta CXCLR	;reset the collision detection for next time

; ==== Next Rock ====
RockCollision
				lda #GS_RockCollision
				bit GameStatus
				beq NoRockColl
			
				lda #<RockOff
				sta NextRockPtr
				
				lda GameStatus
				ora #GS_HasRock
				sta GameStatus			
NoRockColl

								
; ==== Stone movement === ------------------------------------------------------------------------------
MoveStone
				lda StoneX
				cmp #XMIN-1
				beq NoStone
			
				lda StoneLand
				bne GravityDone
UpdateGravity
				sec
				lda StoneSpeed
				sbc #<GRAVITY
				sta StoneSpeed
				lda StoneSpeed+1
				sbc #>GRAVITY
				sta StoneSpeed+1
		
UpdateStoneY
				sec
				lda StoneY
				sbc StoneSpeed
				sta StoneY
				lda StoneY+1
				sbc StoneSpeed+1
				sta StoneY+1
						
GravityDone
				; Check if stone finished arc
				;dec StoneAnim
				;lda StoneAnim
				;bne StoneFrameDone
				;lda StonePower
				;lsr
				;lsr
				;lsr
				;lsr
				;sta StoneAnim
				;inc StoneAnim+1
				;lda StoneAnim+1
				;cmp #2
				;bne StoneAnimOver
				;lda #0
				;sta StoneAnim+1
StoneAnimOver
				
				
				
StoneFrameDone
				
				
				lda StoneSpeed+1
				bpl NoStone
				lda StoneSpeed
				cmp #$90			; StoneSpeed+1 is negative, is low byte less $90?
				bcs NoStone
							
StoneLanded
				; Ending place of stone.... 
				lda StoneLand
				bne CheckStoneWait
				; Stone just stopped
				lda #<Splash
				sta StonePtr
				
				lda #14
				sta CurrTone
				lda #$9
				sta CurrFreq
				lda #$08
				sta CurrVolume
				
				jsr MakeSomeNoise
								
				ldx #$15
				stx StoneLand
				jmp NoStone
CheckStoneWait
				lda CurrVolume
				beq DoneSound
				
				lda CurrFreq
				cmp #1
				beq DoneFreq
				dec CurrFreq
DoneFreq				
				dec CurrVolume
DoneSound
				jsr MakeSomeNoise
				
				dec StoneLand
				lda StoneLand
				cmp #1
				bne NoStone

ResetStone				
				;reset stone
				lda #XMIN-1
				sta StoneX
				lda #0
				sta StonePower
				sta StoneLand
				sta StoneSpeed
				sta StoneSpeed+1
				
				lda #50
				sta StoneY+1
				
				lda #<StoneFrame1
				sta StonePtr
				
				lda GameStatus
				eor #GS_HasRock|GS_RockCollision|GS_ResetRockPlease
				sta GameStatus
								
				jsr StopSomeNoise
				
NoStone
				lda #0
				
; ==== Enemy Movement === ------------------------------------------------------------------------------
				ldx #NUMBER_LINES-1	; Load number of groups
MoveEnemies 
				stx temp
				asl temp
				ldy temp
				
				clc 
				lda EnemyX,y 		; Load the X pos
				adc EnemySpeed,y
				sta EnemyX,y 	; Store new x pos
				
				lda EnemyX+1,y
				adc EnemySpeed+1,y
				sta EnemyX+1,y
											
				cmp #XMIN
				bcc SwapXMin
				
				cmp #XMAX	; Compare with right side of screen
				bcs WrapAround 
				jmp MoveOk
SwapXMin
				lda #XMIN
				sec
				sbc EnemyX+1,y
				clc
				adc	#XMIN
				sta EnemyX+1,y
				
SwapX 
				lda EnemySpeed+1,y
				eor #$ff
				clc
				adc #1
				sta EnemySpeed+1,y

				
				jmp MoveOk
WrapAround
				jsr NextRandom
				;and #%00001000
				;bne SwapX
				
				lda #XMIN
				sta EnemyX+1,y
				
MoveOk
				lda EnemyX+1,y
				jsr ConvertToFC		; And convert it to "FC" positioning?
				sta EnemyFC_X,x		; and save the result in FC_XPOS				
				
; Collision detection
CheckCollision
				lda StoneLand
				cmp #0
				beq NoCollision

				lda Collision,x
				bpl NoCollision
HitBoat
				lda EnemyStatus,x
				eor #ES_Active
				sta EnemyStatus,x
AddOne
				lda ScoreSpritePtr+$A
				clc
				adc #6
				sta ScoreSpritePtr+$A

				
				; Check if all enemies done
				inc EnemyHits
				lda EnemyHits
				cmp #NUMBER_LINES
				beq WaveDestroyed
				
NoCollision
				lda #$0
				sta Collision,x
								
				dex
				bpl MoveEnemies
				
doneMoveEnemies
				jmp DoneWaitScreen
				
WaitScreen
				dec TimerTemp
				lda TimerTemp
				bne DoneWaitScreen
				
				lda #STATE_GAME
				sta GameState
				
DoneWaitScreen


; ==== Horizontal Positioning ==== ------------------------------------------------------------------------------
HorizontalPositioning 			

; ==== Position Ball (Next rock) ====

				lda NextRockX
				ldx #SPRITE_BALL
				jsr XPosition
				sta WSYNC
				sta HMOVE
			
				; Clear All Horizontal Fine Movement
				sta HMCLR
				
; ==== Pre Screen-Start ==== ----------------------------------------------------------------------------------
PreScreenStart
				lda #SCREEN_HEIGHT
				sta CurrScanLine
				
				lda #COL_SKY
				sta COLUBK
				
				lda #COL_FARBANK
				sta COLUPF

				; Clear playfield for top of screen
				lda #0
				sta PF0
				sta PF1
				sta PF2
				
				; No reflect player 0 for score
				sta REFP0		
				
				lda #COL_SCORE
				sta COLUP0
				sta COLUP1
															
				lda SCORE_HEIGHT
				sta ScoreHeight		
								
; ==== Setup Vertical placement variables ====
SetupStoneVert
				lda #0
				sta LineCounter		; Initialize group counter
				sta CurrLineY		; First line of first group
				lda #LINE_HEIGHT+1
				sta NextLineY		; First line of next (second) group
				lda StoneY+1		
				sta StoneLine		; Store stone y pos in the StoneLine

				jmp DoneLogic

; ==== Dodgy Reset Game ====
WaveDestroyed
				jmp ResetGame
				
DoneLogic

				nop
; ==== Finish Vertical Blank ==== -------------------------------------------------------------------------									
WaitVblank2
				ldy INTIM
				bne WaitVblank2
				sty VBLANK

; ==== KERNEL ==== ------------------------------------------------------------------------------------------
				inc frameCount
								
Kernel
				sta WSYNC
				jsr Draw48Pixer
						
				; Calculate current scan line
				clc
				lda CurrScanLine
				sbc SCORE_HEIGHT
				sta CurrScanLine
				dec CurrScanLine
				
; ==== Draw the Sun ====
				lda LivesLeft
				sta NUSIZ0
				
				lda #COL_SUN
				sta COLUP0
				
				ldx #10				; Sun Horizontal Position offset
PositionSun
				dex
				bpl PositionSun
				sta RESP0
				
				ldx #11				; Sun Height
DrawSun
				lda SunData,x
				sta GRP0
				sta WSYNC
				dec CurrScanLine
				dex
				bne DrawSun
				
				stx NUSIZ0
													
; ==== Position Stone ====
				ldx #SPRITE_PLAYER0
				lda StoneX
PositionStone
				jsr XPosition			
				sta WSYNC
 				sta HMOVE				

				dec CurrScanLine
							
				sta HMCLR		; Reset HOrizontal offsets
				
				; Solid lines for top of pond
				lda #$ff
				sta PF0
				sta PF1
				sta PF2
					
				; Set the water colour for the pond background
				lda #COL_POND
				sta COLUBK
										
StartPond
				dec CurrScanLine
				lda CurrScanLine
				
				sta WSYNC
				
				cmp #168
				bne StartPond
				
				; Set up the stone colour
				lda StoneLand			; Stone splashing or flying colour?
				beq ColourStone
				lda	#COL_SPLASH	
				jmp StoreStoneColour
ColourStone
				lda #COL_STONE
StoreStoneColour
				sta COLUP0
				sta WSYNC
				dec CurrScanLine
				
; ==== Draw Pond "Curve" ====				
				ldx #3					; Pond "curve" height
PondCurve
				lda PFData0,x
				sta PF0
				lda PFData1,x
				sta PF1
				lda PFData2,x
				sta PF2
						
				sta WSYNC
				dec CurrScanLine
				
				dex
				bne PondCurve		
				
; ==== Reset PF ====	
				nop
					
				lda #0
				sta PF0
				sta PF1
				sta PF2
		
; ==== Init Enemy Lines ====
				sta LineCounter
													
EnemyLines

StoneVert2
				;================================
				lda    StoneLine			; Distance between Player0<->top of group
				cmp    #LINE_HEIGHT+1		; Is Player0 inside current group?
				bcc    GetPlayerByte 		; Yes, we'll draw it...
				ldx    #0					; No, draw instead a
				beq    StorePlayerOffset	; blank sprite. (well - actually draw 0th byte of player data (#0)
	   
GetPlayerByte
				lda    NextLineY			; We must draw Player0, and we'll start 
				sec							; from the (NXTGRPY-PLYPOS)th byte.
				sbc    StoneY+1
				tax							; Put the index to the first byte into X
StorePlayerOffset
				stx    StoneOffset			; and remember it.
				;=================================

CollSet
				ldx LineCounter
				ldy StoneOffset
														
; ==== Draw the Enemies ==== --------------------------------------------------------------------------
				
				lda EnemyFC_X,x	; Get Player1 position
				
				ldx #0;(StonePtr),y	; Get Player0 pattern for "inbetween" lines
				
				ldy #0
				sta WSYNC		; Start with a new scanline. 
				stx GRP0		; Set Player0 pattern
				sty GRP1			
				sta HMP1		; Prepare Player1 fine motion
				and #$0F		; Prepare Player1 coarse positioning
				tay
PositionLine 
				dey				; Waste time
				bpl PositionLine
				sta RESP1		; Position Player1
								
				sta WSYNC		; Wait for next scanline
				sta HMOVE		; Apply fine motion
				
; Now prepare various things for the next group
StoneVert3
				lda NextLineY		; Updade this group and next group
				sta CurrLineY		; top line numbers
				clc
				adc #LINE_HEIGHT+1
				sta NextLineY

				lda StoneY+1		; Find out which 'slice'
				sec					; of Player0 we'll have to draw.
				sbc CurrLineY		; We need the distance of Player0
				bpl LinePositive	; from the top of the group.
				eor #$FF			; 
				clc
				adc #1				; A =    (PLYPOS-GRPY)
LinePositive
				sta StoneLine		;

;=======================================

				dec CurrScanLine
				dec CurrScanLine	
				
				inc StoneOffset		; Pointer to the next byte of Player0
				;inx					; pattern. Use X while drawing the group
				;stx temp2
				sta WSYNC
							
				ldy #LINE_HEIGHT-1	; Initialize line counter (going backwards)			
				ldx LineCounter
LineLoop
				tya			; Find the shade of Player1 color
				cmp #1
				bne ColourBoat
				lda #COL_HULLS
				jmp StoreColour
ColourBoat
				asl			; to be used in the next line
				ora BoatColours,x
StoreColour
				sta temp 		; ...and remember it.

				dec CurrScanLine
				sta WSYNC		; Wait for a new line
			
; ==== SEt up Boat line to draw =====
				lda EnemyStatus,x
				and #ES_Active
				beq DrawNoBoat		; Jump (store #0) if boat dead
				lda BoatFrame0,y	; Else load the line
DrawNoBoat		
				sta GRP1			; Set Boat line			
				
				lda temp			; And set boat colour
				sta COLUP1
				
				sty temp2
				ldy StoneOffset		; Get Stone Offeset back				
				lda StoneX		; If stone already goin, forget firebutton
				cmp #XMIN-1
				beq EmptyStone
				lda (StonePtr),y
				jmp StoreStone
EmptyStone
				lda #0
StoreStone
				sta GRP0		; Set Player0 shape
				ldy temp2
				
; ==== Done this line =====
				inc StoneOffset			; Add one to Stone Offset
				ldx LineCounter		; Get the LineCounter back
				
				dec CurrScanLine
				sta WSYNC			; Wait for a new scanline
				
				dey					; Decrement boat graphic line counter
				bpl LineLoop		; Go on with this group if needed
					
SetCollisions
				lda CXPPMM
				ora Collision,x
				sta Collision,x
				lda #0					; Clear collisions
				sta CXCLR				
				
				
				inc LineCounter			; Increment current group number
				lda LineCounter
				cmp #NUMBER_LINES		; Is there another group to do?
				bcs DoneLines			; No, exit
				jmp EnemyLines			; Yes, go back								
	;------------------------------------------------------------------------
DoneLines
				;lda #$A6 ; Finished LInes
				;sta COLUBK							
				lda #0
				sta GRP0
				
; ====Set Player Reflection ====
				lda PlayerDir
				beq NoReflect
				
				lda #%00001000   ;a 1 in D3 of REFP0 says reflect
				sta REFP0
NoReflect

; ==== Position Player ====				
				ldx #0			; sprite number
				lda PlayerX		; Sprite pos						
				jsr XPosition	; Position it X
								
; ==== Position Enemy ==== 
				;sta WSYNC
				ldx #1
				lda EnemyX+1
				jsr XPosition
								
				sta WSYNC
				sta HMOVE		
			
				
PlayerArea
				lda #COL_POND
				sta COLUBK 
				
				lda #COL_STONE
				sta COLUPF
				
				lda #$a4
				sta COLUP0
				
				;lda #%00000100
				;sta NUSIZ1
				
				ldy #SPRITE_HEIGHT
				lda #%00010001
				sta CTRLPF
				
				
				
WateryBit			
				sta WSYNC
				
				lda #BankColours,y
				sta COLUBK

				lda PlayerJumpSpeed
				beq DoneDrawHead		
				
				lda (PlayerJumpTop),y
				sta GRP0
							    
				;lda #Player1Colours,y
				;sta COLUP0	
DoneDrawHead		
				lda (NextRockPtr),y
				sta ENABL
								
				dey
				bpl WateryBit
				
				sta WSYNC
				
				lda #COL_NEARBANK
				sta COLUBK

				ldy #SPRITE_HEIGHT
				
GroundBit
			    lda (SpriteTablePtr),y
				sta GRP0
							    
				lda #Player1Colours,y
				sta COLUP0
				
				lda EnemySprite,y
				sta GRP1
				
				sta WSYNC
					
				dey
				bpl GroundBit
				
				lda #$f2
				sta COLUBK
				
TestPlayerColl
				lda #%01000000
				bit CXP0FB
				beq NoRockGet
				lda GameStatus
				ora #GS_RockCollision
				sta GameStatus
NoRockGet
				lda #%10000000
				bit CXPPMM
				beq NoBaddieHit
				lda GameStatus
				ora #GS_BaddieCollision
				sta GameStatus
NoBaddieHit
				
; ==== Done bottom play section ====
											
				;sta WSYNC
				
				lda StonePower		; Line up ball
				clc
				adc #100				; Horizontal offset
				ldx #SPRITE_BALL
				jsr XPosition
				sta WSYNC
				
				lda #$A0
				sta COLUBK 		

UpdateTimerBall
				lda LevelTimer+1
				cmp #5
				bcs NoWarnColour
				ldx #$30
				jmp TimerAdjust
NoWarnColour
				ldx #COL_ENERGYOFF					
TimerAdjust		
				stx COLUP1		
				clc			
				adc #20				; Horizontal offset
				ldx #SPRITE_PLAYER1
				jsr XPosition
				sta WSYNC
				
				sta HMOVE
					
; ==== Power Bars  ====
PowerBars			
				
				
				lda #$ff;#DISABLE_BM
				sta GRP1
				
				lda #%00110001
				sta CTRLPF
				
				lda #ENABLE_BM
				sta ENABL
							
				lda #COL_ENERGYOFF		
				sta COLUPF
								
				ldy #4
				sta WSYNC
				
				nop
				nop
				
PowerBit			
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop
				nop 
				nop
				
				lda #$A4
				sta COLUBK

				nop
				nop
				nop
				
				nop
				nop
				
				lda #$A0
				sta COLUBK
				
				nop
				nop
				nop
				
				lda #$A4
				sta COLUBK
				
				nop
				nop
				nop
				nop
				nop
				nop
	
				
				lda #$A0
				sta COLUBK
				
				
				sta WSYNC	
						
				dey
				bpl PowerBit
				
				sta WSYNC
				
; ==== Done Screen - reset stuff =====
				lda #0
				sta PF1
				sta ENABL
				sta GRP1
				sta CXCLR		; Clear collision between player and flying things
	

; ==== Overscan =====

; Turn off the TIA to hide display for overscan
				lda #2
				sta WSYNC
				sta VBLANK
								
; Final 30 scan lines of overscan
				;; Start the timer for the 30 vertical blank scan lines
				ldy #35 ;(30*76)/64 = 35.625
				sty TIM64T

; ==== Do some more stuff ==== -----------------------------------------------------------------------------------------------------
ResetRock
				lda #GS_ResetRockPlease
				bit GameStatus
				beq NoResetRock
ResetTheRock
				eor GameStatus ;Turn off rock please
				sta GameStatus
				
				lda #<RockOn
				sta NextRockPtr
				
				jsr NextRandom
				clc
				adc #15
				sta NextRockX				
NoResetRock

				lda #GS_BaddieCollision
				bit GameStatus
				beq TimerLogic
			
				; Collision!
				dec LivesLeft
				lda LivesLeft
				bne MoreLivesLeft
				jmp ResetGame
MoreLivesLeft
				jmp InitLevel
				
TimerLogic
				sec
				lda LevelTimer
				sbc #<3
				sta LevelTimer
				lda LevelTimer+1
				sbc #>3
				sta LevelTimer+1
				
				lda LevelTimer+1
				beq TimeOver
				
				jmp DoneLogicII
; ==== Ran outta time! ====
TimeOver  	
				dec LivesLeft
				lda LivesLeft
				bne MoreLivesLeft
				
				jmp ResetGame
	
DoneLogicII


; ==== End of overscan ====
OverscanEnd
				ldy INTIM
				bne OverscanEnd
						
; Frame done... lets do it again!
				jmp StartOfFrame
				
; ==== Functions ==== --------------------------------------------------------------------------------------------------------------------

; ==== Do Vertical Blank ==== : Does a vertical blank, and sets the timer in Y
DoVerticalBlank
				lda #0				; Wait for vertical sync
				sta VBLANK
				
				lda #2
				sta VSYNC
				
				sta WSYNC			; Then 3 scan lines
				sta WSYNC
				sta WSYNC
		
				lda #0				;Then someother vsyncy thing
				sta VSYNC
						
	; Start the timer for the 37 vertical blank scan lines
				; Set Vertical Blank Timer
				ldy #43                 ; (37*76)/64 = 43
				sty TIM64T
				
				rts
				
WaitForVBlankEnd
	; Wait until the timer we set above has expired for vblank
WaitVblank
				ldy INTIM
				bne WaitVblank
				sty VBLANK
				
				rts

; ==== Do the overscan ==== : 				
DoOverscan
				
				; Turn off the TIA to hide display for overscan
				lda #2
				sta WSYNC
				sta VBLANK
								
				; Final 30 scan lines of overscan
				ldx #30
Overscan
				sta WSYNC
				dex
				bne Overscan
				
				rts

; ==================----------; requires ScoreHeight to have height, and ScoreSpritePtr-----------------------------------------------------------------
Draw48Pixer
				lda #$03           ; set both players to 3 copies
			    sta NUSIZ0
			    sta NUSIZ1
			    lda #$1           ; set vertical delay on for both players
			    sta VDELP0
			    sta VDELP1
				sta WSYNC
				
			    ldx #$7            ; move players 12 columns over
DelayScore
				dex
				bne DelayScore
				stx RESP0
				sta RESP1
				
			    lda  #$f0           ; set player 0 to move left 1 pixel
			    sta  HMP0

			    sta  WSYNC
			    sta  HMOVE          ; move player 0
									
ScoreLoop
				ldy  ScoreHeight        	;+3  63  189
				lda  (ScoreSpritePtr),y     ;+5  68  204
				sta  GRP0            		;+3  71  213      D1     --      --     --
				sta  WSYNC           		;go
				lda  (ScoreSpritePtr+$2),y  ;+5   5   15
				sta  GRP1            		;+3   8   24      D1     D1      D2     --
				lda  (ScoreSpritePtr+$4),y  ;+5  13   39
				sta  GRP0            		;+3  16   48      D3     D1      D2     D2
				lda  (ScoreSpritePtr+$6),y  ;+5  21   63
				sta  temp2         			;+3  24   72
				lda  (ScoreSpritePtr+$8),y  ;+5  29   87
				tax                  		;+2  31   93
				lda  (ScoreSpritePtr+$A),y  ;+5  36  108
				tay                  		;+2  38  114
				lda  temp2         			;+3  41  123              !
				sta  GRP1            		;+3  44  132      D3     D3      D4     D2!
				stx  GRP0            		;+3  47  141      D5     D3!     D4     D4
				sty  GRP1            		;+3  50  150      D5     D5      D6     D4!
				sta  GRP0            		;+3  53  159      D4*    D5!     D6     D6
				dec  ScoreHeight        	;+5  58  174                             !
				bpl  ScoreLoop           	;+2  60  180
				
				;sta WSYNC
				
				lda #$00 
				sta VDELP0  ;Clear vertical delays
				sta VDELP1 
				sta GRP0    ;Clear players
				sta GRP1
				sta NUSIZ0
				sta NUSIZ1
				sta HMCLR

				rts
				
; ==== Music Player Routines ====
SetupMusic
				lda #0
				sta MusicTimer
				sta MusicIndex
				sta MusicTimer2
				sta MusicIndex2
				sta MusicCounter
				sta AUDV0
				sta AUDV1				
				rts
				
; ==== Play Music ==== : called every frame
PlayMusic
				lda MusicTimer
				beq GetNextNote
				dec MusicTimer
				rts
GetNextNote
				inc MusicIndex
				ldx MusicIndex
				cpx NoteData
				
				bne NextNote
ResetSong
				ldx #1
				stx MusicIndex
				inc MusicCounter
NextNote

				lda NoteData,x
				sta AUDF1			
				
				lda VolumeData,x
				sta AUDV1
				
				lda ToneData,x
				sta AUDC1
				
				lda TimerData,x
				sta MusicTimer				
				
				rts

; ==== Play Music ==== : called every frame -------------------------------------------------------
PlayMusic2
				lda MusicTimer2
				beq GetNextNote2
				dec MusicTimer2
				rts
GetNextNote2
				inc MusicIndex2
				ldx MusicIndex2
				cpx MNoteData
				
				bne NextNote2
ResetSong2
				ldx #1
				stx MusicIndex2
NextNote2

				lda MNoteData,x
				sta AUDF0
				
				lda MVolumeData,x
				sta AUDV0
				
				lda MToneData,x
				sta AUDC0
				
				lda MTimerData,x
				sta MusicTimer2
				
				rts
				
; ==== Stop Music ====
StopMusic
				lda #0
				sta AUDC0
				sta AUDC1
				
				sta AUDF0
				sta AUDF1
				
				sta AUDV0
				sta AUDV1
				
				rts

; ===========-----------------------------------------------------------------------------------------------

NextRandom
   lsr randomSeed
   rol
   eor randomSeed
   lsr
   lda randomSeed
   bcs .skipTap
   ora #%01000000
   sta randomSeed
.skipTap
   rts
   
   
StopSomeNoise
	lda #0
	sta AUDV0
	rts
	
MakeSomeNoise
	lda CurrTone
	sta AUDC0
	
	lda CurrFreq
	sta AUDF0
	
	lda CurrVolume
	sta AUDV0
	
	rts

; Sprite Horizontal Positioning Routine (Battlezone Variant)
	; On Entry: A = Coordinate and X = Object ID
XPosition
				sta WSYNC				; [0]
				sec						; [0] + 2
        ; Divide Position by 15
Divide15
				sbc #15					; [2] + 2
				bcs Divide15			; [4] + 2/3
				
				nop						; [6] + 2
				
				tay						; [8] + 2
				; Store Coarse and Fine Position
				lda fineAdjustTable,y	; [10] + 4
				sta HMP0,x				; [14] + 4
				sta RESP0,x				; [18] + 4
				
				; Position At Cycles: [22/27/32/37/42/47/52/57/62/67/72/77]
			
				rts
				

; Straight from "Air sea battle", here's the routine
; to convert from standard X positions to FC positions.

ConvertToFC
				sta    temp						; save the x position
				bpl    .determineCoarseValue	; unrequired
				cmp    #XMAX					; Check if in range
				bcc    .determineCoarseValue	; if it is, go get coarse value
				lda    #XMAX					; else set max
				sta    temp
.determineCoarseValue
				lsr							; top nibble to lower
				lsr
				lsr
				lsr	
				tay							; save in y
				lda    temp					; get x again
				and    #$0f					; mask upper nibble
				sty    temp					; save coarse value for later
				clc
				adc    temp					; add in coarse value (A = C + F)
				cmp    #$0F
				bcc    .skipSubtractions
				sbc    #$0F					; subtract 15
				iny							; and increment coarse value
.skipSubtractions
				cmp    #XMIN				; make sure not past x min
				eor    #$0F
				bcs    .skipFineIncrement
				adc    #$01					; increment fine motion value
				dey							; reduces coars value
.skipFineIncrement
				iny							; increment coarse
				asl							; Move fine motion to upper nibble
				asl
				asl
				asl
				sta temp					; save it
				tya							; move coars value to acc
				ora temp					; accumulator holds fine/coarse value
				rts
	   
			ORG $F800
fineAdjustBegin
	DC.B %01110000; Left 7
	DC.B %01100000; Left 6
	DC.B %01010000; Left 5
	DC.B %01000000; Left 4
	DC.B %00110000; Left 3
	DC.B %00100000; Left 2
	DC.B %00010000; Left 1
	DC.B %00000000; No movement.
	DC.B %11110000; Right 1
	DC.B %11100000; Right 2
	DC.B %11010000; Right 3
	DC.B %11000000; Right 4
	DC.B %10110000; Right 5
	DC.B %10100000; Right 6
	DC.B %10010000; Right 7
fineAdjustTable EQU fineAdjustBegin - %11110001; NOTE: %11110001 = -15

			ORG ($F900); + #SCREEN_HEIGHT)
; I got that ORG $FB00+screenheight from another site. is that right? why $fb00?
Sprite0Frame1
	.byte zz________
	.byte zz_X___X__
	.byte zzX_X___X_
	.byte zz___XXX__
	.byte zz___XX___
	.byte zz_X_XX_X_
	.byte zz__X_XX__
	.byte zz___XX___
	.byte zz___XX_X_
	.byte zz__XXXX__
	.byte zzXXXXXXXX
	.byte zz___XX___
	.byte zz________
		
Sprite0Frame2 
	.byte zz________
	.byte zz___XX___
	.byte zz____X___
	.byte zz____X___
	.byte zz___XX___
	.byte zz___XX___
	.byte zz___XX___
	.byte zz___XX___
	.byte zz___XX_X_
	.byte zz__XXXX__
	.byte zzXXXXXXXX
	.byte zz___XX___
	.byte zz________

Sprite0Winding
	.byte zz________
	.byte zz_XX_____
	.byte zz__X_X___
	.byte zz__X_X___
	.byte zz_X_XX___
	.byte zz_X_XX___
	.byte zz__XXXX__
	.byte zz___XX___
	.byte zz___XX_X_
	.byte zz__XXXX__
	.byte zzXXXXXXXX
	.byte zz___XX___
	.byte zz________	

Sprite0Throw
	.byte zz________
	.byte zz_____XX_
	.byte zz_XX__X__
	.byte zz__X__X__
	.byte zz___XX___
	.byte zz___XX_X_
	.byte zz__XXXX__
	.byte zz_X_XX___
	.byte zzX__XX___
	.byte zz__XXXX__
	.byte zzXXXXXXXX
	.byte zz___XX___
	.byte zz________	
	
Sprite0Jump1
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte zz________
	.byte zz_X___X__
	.byte zzX_X___X_
	.byte zz___XXX__
	.byte zz___XX___
	.byte zz_X_XX_X_
	.byte zz__X_XX__
	.byte zz___XX___
	.byte zz___XX_X_
	.byte zz__XXXX__
	.byte zzXXXXXXXX
	.byte zz___XX___
	.byte zz________
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0
	
JumpCurve
	.byte 0,15,19,22,24,26,27,28,28,29,29,29,29,28,28,27,26,24,22,19,15

RockOn
	.byte 0,0,0,0,0,0,0,0,0,0,2,2,2
RockOff
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0
	
EnemySprite
	.byte #$0
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00011000
	.byte #%01100110
	.byte #%01100110
	.byte #%00011000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #$0
	.byte #$0
	
Player1Colours
		.byte $0,$38,$38,$0,$0,$28,$28,$28,$35,$35,$38,$38,$38,$38
		
StoneFrame1
		.BYTE $00		; Pattern for Player0. Please note
        .BYTE $00		; the leading and trailing 0's
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE %00000000
        .BYTE %00000000
        .BYTE %00000000
        .BYTE %00011000
        .BYTE %00000000
        .BYTE %00000000
        .BYTE %00000000
        .BYTE $00
		.BYTE $00
        .BYTE $00
		.BYTE $00
        .BYTE $00
		.BYTE $00
        .BYTE $00
											
StoneFrame2
		.BYTE $00		; Pattern for Player0. Please note
        .BYTE $00		; the leading and trailing 0's
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE %00000000
        .BYTE %00000000
        .BYTE %00011000
        .BYTE %00111100
        .BYTE %00011000
        .BYTE %00000000
        .BYTE %00000000
        .BYTE $00
		.BYTE $00
        .BYTE $00
		.BYTE $00
        .BYTE $00
		.BYTE $00
        .BYTE $00

StoneFrame3
		.BYTE $00		; Pattern for Player0. Please note
        .BYTE $00		; the leading and trailing 0's
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE %00000000
        .BYTE %00011000
        .BYTE %00111100
        .BYTE %00111100
        .BYTE %00111100
        .BYTE %00011000
        .BYTE %00000000
        .BYTE $00
		.BYTE $00
        .BYTE $00
		.BYTE $00
        .BYTE $00
		.BYTE $00
        .BYTE $00
											
Splash
		.BYTE $00
		.BYTE $00		; Pattern for Player0. Please note
        .BYTE $00		; the leading and trailing 0's
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE %00100000
        .BYTE %01001010
        .BYTE %10100101
        .BYTE %00010101
        .BYTE %01001000
        .BYTE %00100010
        .BYTE %00000000
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00
        .BYTE $00

	
BoatFrame0 
		.byte %00000000
        .byte %01111110
        .byte %11111111
        .byte %00001000
        .byte %00111000
        .byte %00011000
        .byte %00001000
BoatColours
		.byte $10,$20,$30,$40,$50,$60,$70,$80,$90,$A0

PFData0
		.byte #0
        .byte #%00110000
        .byte #%11110000
        .byte #%11110000

PFData1
		.byte #0
        .byte #%00000000
        .byte #%11000000
        .byte #%11111111

PFData2
		.byte #0
        .byte #%00000000
        .byte #%00000000
        .byte #%00000000
		
SunData
		.byte #0
		.byte #0
		.byte #%00001000
		.byte #%00100010
		.byte #%00000000
		.byte #%10011000
		.byte #%00111101
		.byte #%00111100
		.byte #%00011000
		.byte #%01000010
		.byte #%00010000
		.byte #%00000000

BankColours
		.byte COL_FARBANK,COL_FARBANK,COL_POND,COL_POND
		.byte COL_POND,COL_POND,COL_POND,COL_POND,COL_POND
		.byte COL_POND,COL_POND,COL_POND,COL_POND,COL_POND
		

Digits
		.byte #%00000000
		.byte #%00111100
		.byte #%01100110
		.byte #%01100110
		.byte #%01100110
		.byte #%00111100
		
;Digit1
		.byte #%00000000
		.byte #%01111110
		.byte #%00011000
		.byte #%00011000
		.byte #%00011000
		.byte #%00111000
;Digit2
		.byte #%00000000
		.byte #%01111110
		.byte #%01100000
		.byte #%00111110
		.byte #%00000110
		.byte #%01111100	
;Digit3
		.byte #%00000000
		.byte #%01111110
		.byte #%00000110
		.byte #%00011110
		.byte #%00000110
		.byte #%01111110	
;Digit4
		.byte #%00000000
		.byte #%00001100
		.byte #%00001100
		.byte #%01111110
		.byte #%01101100
		.byte #%01101100	
;Digit5
		.byte #%00000000
		.byte #%01111110
		.byte #%00000110
		.byte #%01111110
		.byte #%01100000
		.byte #%01111110
;Digit6
		.byte #%00000000
		.byte #%00111100		
		.byte #%01100110
		.byte #%01111100
		.byte #%01100000
		.byte #%00111100
		
		.byte #%00000000
		.byte #%00001100		
		.byte #%00001100
		.byte #%00001100
		.byte #%00001100
		.byte #%01111100
		
		.byte #%00000000
		.byte #%01111110		
		.byte #%01100110
		.byte #%01111110
		.byte #%01100110
		.byte #%01111110
		
		.byte #%00000000
		.byte #%00000110		
		.byte #%00000110
		.byte #%01111110
		.byte #%01100110
		.byte #%01111110
		
NoteData
	.byte #17	; Pattern length
	.byte 9,0,1,0,7,0,1,0,11,0,30,0,7,0,1,0

ToneData
	.byte 0
	.byte 10,0,8,0,8,0,8,0,10,0,15,0,8,0,8,0
	
TimerData
	.byte 0
	.byte 8,8,2,14,4,12,2,14,8,8,4,12,4,12,2,14
	
VolumeData
	.byte 0
	.byte 8,0,3,0,8,0,3,0,8,0,6,0,8,0,3,0

;--------------------
	
MNoteData
	.byte #17
	.byte 25,0,10,0,12,10,12,0
	.byte 12,12,0,0,0,0,12,0

MToneData
	.byte 0
	.byte 12,0,12,0,12,12,12,0
	.byte 12,12,12,0,0,0,12,0

MTimerData
	.byte 0
	.byte 8,8,15,1,8,8,4,12
	.byte 8,8,8,8,8,8,8,8

MVolumeData
	.byte 0
	.byte 3,0,6,0,0,0,0,0
	.byte 8,2,0,0,0,0,8,0
		
		;---------------------------------

				ECHO [*-$F000+6]d," out of 4096 ROM bytes used",[$FFFA-*]d,"bytes left"
		 
				ORG $FFFA
		
InteruptVectors
				.word Reset			; NMI
				.word Reset			; RESET
				.word Reset			; IRQ

				END
			
