REM *************************************************************************
REM IntyBASIC Project: BeatBonzai
REM *************************************************************************
REM     Programmer: Brian Towne
REM     Created:    10/13/2015
REM     Updated:    11/1/2015
REM *************************************************************************
REM History:
REM 10/13/2015 - BeatBonzai project created.
REM 11/1/2015 - V0.1: Basic game with no menus
REM 11/13/2015 - V0.2: Added main menu and result screen
REM 11/21/15 - V0.3: Fixed bugs and reworked a large part of the code for reability and efficency
REM *************************************************************************
REM Tasks:
REM -Different play modes (survival, high score (normal), combo king)
REM -Different songs (including random)
REM -Prettification (better graphics)
REM -Sound (possibly also music (different music for each song possibly))
REM -Fix freezing error/other glitches
REM*************************************************************************

REM Include useful predefined constants
INCLUDE "constants.bas"

REM import graphics
DEFINE 0,12,graphics

REM declare constants
CONST BEATFRAMES = 8 'the spacing of frames between 'beats'
CONST ANI_SPEED = 16'The speed of cursor animations
CONST ROW = 20 'the distance of cards between rows

'various game speeds, notes the amount of frames between logical frames
CONST SPD_INSANE = 1
CONST SPD_FAST = 3
CONST SPD_MEDIUM = 6
CONST SPD_SLOW = 9
CONST SPD_TURTLE = 12
CONST SPD_TEST = 15

'various game statuses
CONST STS_CONCLUDE = 0
CONST STS_GAME_TRANS = 1
CONST STS_GAME = 2
CONST STS_SCORE_TRANS = 3
CONST STS_SCORE = 4
CONST STS_MENU_TRANS = 5
CONST STS_MENU = 6

'score values for various notes
CONST OK_SCORE = 5
CONST GOOD_SCORE = 20
CONST PERF_SCORE = 50

'bit positions used for bitwise operations
'inclusive bits
CONST BIT0 = 1
CONST BIT1 = 2
CONST BIT2 = 4
CONST BIT3 = 8
CONST BIT4 = 16
CONST BIT5 = 32
CONST BIT6 = 64
CONST BIT7 = 128

'exclusive bits
CONST BIT0_E = 254
CONST BIT1_E = 253
CONST BIT2_E = 251
CONST BIT3_E = 247
CONST BIT4_E = 239
CONST BIT5_E = 223
CONST BIT6_E = 191
CONST BIT7_E = 127

'varous color settings
CONST COLOR_NAME = FG_BLUE 'color for the option names
CONST COLOR_VALUE = FG_DARKGREEN 'color for the option values
CONST COLOR_TITLE = FG_RED'color for screen title
CONST COLOR_TRANS = FG_YELLOW'color for transition buttons
CONST COLOR_NOTEA = FG_TAN ' 'color for A notes
CONST COLOR_NOTEB = FG_TAN'color for B notes
CONST COLOR_CURTAIN = FG_RED'color for the ending curtain
CONST COLOR_ENV = FG_WHITE'color for the side walls of game
CONST COLOR_CATCH = FG_GREEN'color for the catches
CONST COLOR_DEBUG = FG_YELLOW'color for debug output
CONST COLOR_CURSOR = FG_GREEN'color for the cursor
CONST COLOR_RESPONSE = FG_RED'color for the feedback

REM declare variables
beatRegister = 0 'holds information for releasing beats. [0-3] next set of beats to be released, [4-7] following set of beats
beatNumber = 0 'Holds the amount of beats left in a particular song 
spriteRegister = 0 'keeps track of the currently active beats on the screen. [0,1] column 1, [2,3] column 2, [4,5] column 3, [6,7] column 4
buttonRegister = 0 'holds information about buttons. [0-4] if catch buttons are active, [5] if tab button is active, [6] previous frame status of tab button
catchRegister = 0 'holds information about the catches. [4-7] previous frame status of catch buttons, [0-3] on/off status of catches
gameStatus = STS_MENU_TRANS 'holds the current status of the game
tabRegister = 0 'Holds information used to catch notes. [0-3] the notes in the catching area, [4] note is perfect, [5] note is good, [7] previous note caught.
#score = 0 'holds the current score
combo = 1 'holds the current combo

'Y0-7 hold the location of beats on screen
Y0 = 0
Y1 = 0
Y2 = 0
Y3 = 0 
Y4 = 0 
Y5 = 0
Y6 = 0
Y7 = 0

speed = SPD_MEDIUM 'holds the current speed of the game. speed determines how often the logical frame is executed (Y0)
song = 1 'holds the current song being played
modeSetting = 1 'holds the current mode
speedSetting = 0 'holds the setting for speed
animation = 0 'holds the current animation status for beats in the game and cursor in the menu (Y1)
cursor = 0 'holds the current location of the cursor ()
perfCount = 0 'holds the amount of perfect notes made
okCount = 0 'holds the amount of ok notes made
missCount = 0 'holds the amount of missed notes
goodCount = 0 'holds the amount of good notes made
#logicalFrame = 0 'holds the current iteration of the logical frame


REM The gameloop. Gathers input data then goes to one of three logicalFrames according to the current status of the game.
gameloop:
	'button logic
	buttonRegister = buttonRegister AND BIT6 'set all bits to 0 except for prev tab bit
	IF (1 = CONT.KEY) AND NOT((catchRegister AND BIT4) > 0) THEN buttonRegister = buttonRegister OR BIT0 : catchRegister = catchRegister OR BIT4 ELSE IF NOT(1 = CONT.KEY) THEN catchRegister = catchRegister AND BIT4_E
	IF (2 = CONT.KEY) AND NOT((catchRegister AND BIT5) > 0) THEN buttonRegister = buttonRegister OR BIT1 : catchRegister = catchRegister OR BIT5 ELSE IF NOT(2 = CONT.KEY) THEN catchRegister = catchRegister AND BIT5_E
	IF (3 = CONT.KEY) AND NOT((catchRegister AND BIT6) > 0) THEN buttonRegister = buttonRegister OR BIT2 : catchRegister = catchRegister OR BIT6 ELSE IF NOT(3 = CONT.KEY) THEN catchRegister = catchRegister AND BIT6_E
	IF (4 = CONT.KEY) AND NOT((catchRegister AND BIT7) > 0) THEN buttonRegister = buttonRegister OR BIT3 : catchRegister = catchRegister OR BIT7 ELSE IF NOT(4 = CONT.KEY) THEN catchRegister = catchRegister AND BIT7_E
	IF CONT.B1 AND NOT((buttonRegister AND BIT5) > 0) THEN buttonRegister = buttonRegister OR (BIT5+BIT4) ELSE IF NOT(CONT.B1) THEN buttonRegister = buttonRegister AND BIT5_E
	
	REM Go to the appropriate logical frame acording to the current status of the game
	IF gameStatus = STS_GAME_TRANS THEN GOSUB gameTransition ELSE IF gameStatus = STS_GAME OR gameStatus = STS_CONCLUDE THEN GOSUB gameFrame ELSE IF gameStatus = STS_SCORE_TRANS THEN GOSUB scoreTransition ELSE IF gameStatus = STS_SCORE THEN GOSUB scoreFrame ELSE IF gameStatus = STS_MENU_TRANS THEN GOSUB menuTransition ELSE IF gameStatus = STS_MENU THEN GOSUB menuFrame
	
	'DEBUG
	'PRINT at 0 color 6, <3>Y0
	'PRINT at 20 color 6, <3>Y1
	'PRINT at 40 color 6, <3>Y2
	'PRINT at 60 color 6, <3>Y3
	'PRINT at 80 color 6, <3>Y4
	
	WAIT
GOTO gameloop

REM=========================================================================================
REM====================================FRAME SUBROUTINES====================================
REM=========================================================================================

REM The logical frame for transitioning to the game
gameTransition: PROCEDURE
	'setup the song
	IF song = 1 THEN RESTORE song1 ELSE IF song = 2 THEN RESTORE song2 
	READ beatNumber
	READ beatRegister 
	
	'build gamescreen
	GOSUB clearScreen
	FOR i = 0 TO 11 STEP 1
		PRINT at 20*i+1 color COLOR_ENV, "\189"
		PRINT at 20*i+6 color COLOR_ENV, "\188"
	NEXT i
	PRINT at 222 color COLOR_CATCH, "\258"
	PRINT at 223 color COLOR_CATCH, "\258"
	PRINT at 224 color COLOR_CATCH, "\258"
	PRINT at 225 color COLOR_CATCH, "\258"
	
	'set settings
	gameStatus = STS_GAME 
	#score = 0
	missCount = 0
	goodCount = 0
	okCount = 0
	perfCount = 0
	RETURN
END

REM The logicalFrame for when the game is running
gameFrame: PROCEDURE
	'button actions
	IF ((buttonRegister AND BIT0)>0) THEN GOSUB buttonOneGame
	IF ((buttonRegister AND BIT1)>0) THEN GOSUB buttonTwoGame
	IF ((buttonRegister AND BIT2)>0) THEN GOSUB buttonThreeGame 
	IF ((buttonRegister AND BIT3)>0) THEN GOSUB buttonFourGame 
	IF ((buttonRegister AND BIT4)>0) THEN GOSUB buttonTabGame 
	
	'Update the logic according to the speed
	IF (FRAME % speed) = 0 THEN GOSUB gameLogicalUpdate
	
	'update score
	PRINT at 7 color COLOR_NAME, "Score:"
	PRINT at 30 color COLOR_VALUE, <5>#score
	
	'update combo
	PRINT at 70 color 0, "    " : PRINT at 47 color 0, "      "
	IF combo > 1 THEN PRINT at 47 color COLOR_NAME, "Combo:" : IF combo < 10 THEN PRINT at 70 color COLOR_VALUE, <1>(combo-1) ELSE IF combo < 100 THEN PRINT at 70 color COLOR_VALUE, <2>(combo-1) ELSE PRINT at 70 color COLOR_VALUE, <3>(combo-1)
	
	'DEBUG
	PRINT at 107 color COLOR_DEBUG, <3>beatNumber
	RETURN
END

REM The logical frame for transitioning to the score screen
scoreTransition: PROCEDURE
	cursor = 0

	GOSUB clearScreen
	PRINT at 5 color COLOR_TITLE, "RESULTS"
	
	'final score
	PRINT at 25 color COLOR_NAME, "FINAL SCORE:"
	IF #score < 10 THEN PRINT at 47 color COLOR_VALUE, <1>#score ELSE IF #score < 100 THEN PRINT at 47 color COLOR_VALUE, <2>#score ELSE IF #score < 1000 THEN PRINT at 47 color COLOR_VALUE, <3>#score ELSE IF #score < 10000 THEN PRINT at 47 color COLOR_VALUE, <4>#score ELSE  PRINT at 47 color COLOR_VALUE, <5>#score
	
	'perfect count
	PRINT at 65 color COLOR_NAME, "PERFECT"
	IF perfCount < 10 THEN PRINT at 86 color COLOR_VALUE, <1>perfCount ELSE IF perfCount < 100 THEN PRINT at 86 color COLOR_VALUE, <2>perfCount ELSE PRINT at 86 color COLOR_VALUE, <3>perfCount
	
	'good count
	PRINT at 105 color COLOR_NAME, "GOOD"
	IF goodCount < 10 THEN PRINT at 126 color COLOR_VALUE, <1>goodCount ELSE IF goodCount < 100 THEN PRINT at 126 color COLOR_VALUE, <2>goodCount ELSE PRINT at 126 color COLOR_VALUE, <3>goodCount
	
	'ok count
	PRINT at 145 color COLOR_NAME, "OK"
	IF okCount < 10 THEN PRINT at 166 color COLOR_VALUE, <1>okCount ELSE IF okCount < 100 THEN PRINT at 166 color COLOR_VALUE, <2>okCount ELSE PRINT at 166 color COLOR_VALUE, <3>okCount
	
	'miss count
	PRINT at 185 color COLOR_NAME, "MISS"
	IF missCount < 10 THEN PRINT at 206 color COLOR_VALUE, <1>missCount ELSE IF missCount < 100 THEN PRINT at 206 color COLOR_VALUE, <2>missCount ELSE PRINT at 206 color COLOR_VALUE, <3>missCount
	
	'options
	PRINT at 223 color COLOR_TRANS, "RETRY    MENU"
	
	gameStatus = STS_SCORE
	RETURN
END


REM The logical frame for the ending score screen
scoreFrame: PROCEDURE
	IF ((buttonRegister AND (BIT0 + BIT1 + BIT2 + BIT3))>0) THEN GOSUB buttonScore 
	IF ((buttonRegister AND BIT4)>0) THEN GOSUB buttonTabScore
	
	'erase previous cursor
	PRINT at 222 color 0, " " : PRINT at 228 color 0, " " : PRINT at 231 color 0, " " : PRINT at 236 color 0, " "
	
	'print cursor
	IF animation = 0 THEN IF cursor = 0 THEN PRINT at 222 color COLOR_CURSOR, "\264" : PRINT at 228 color COLOR_CURSOR, "\264" ELSE PRINT at 231 color COLOR_CURSOR, "\264" : PRINT at 236 color COLOR_CURSOR, "\264"
	IF animation = 1 THEN IF cursor = 0 THEN PRINT at 222 color COLOR_CURSOR, "\265" : PRINT at 228 color COLOR_CURSOR, "\265" ELSE PRINT at 231 color COLOR_CURSOR, "\265" : PRINT at 236 color COLOR_CURSOR, "\265"
	IF animation = 2 THEN IF cursor = 0 THEN PRINT at 222 color COLOR_CURSOR, "\266" : PRINT at 228 color COLOR_CURSOR, "\266" ELSE PRINT at 231 color COLOR_CURSOR, "\266" : PRINT at 236 color COLOR_CURSOR, "\266"
	IF animation = 3 THEN IF cursor = 0 THEN PRINT at 222 color COLOR_CURSOR, "\267" : PRINT at 228 color COLOR_CURSOR, "\267" ELSE PRINT at 231 color COLOR_CURSOR, "\267" : PRINT at 236 color COLOR_CURSOR, "\267"
	
	'increment animation
	IF (FRAME % ANI_SPEED) = 0 THEN IF animation = 3 THEN animation = 0 ELSE animation = animation + 1
	RETURN
END

REM The logical frame for transitioning to the main menu
menuTransition: PROCEDURE
	cursor = 0
	
	GOSUB clearScreen
	PRINT at (ROW * 1) + 4 color COLOR_TITLE, "BEAT BONZAI"
	PRINT at (ROW * 3) + 6 color COLOR_NAME, "MODE"
	PRINT at (ROW * 4) + 7 color COLOR_VALUE, "standard"
	PRINT at (ROW * 5) + 6 color COLOR_NAME, "SPEED"
	PRINT at (ROW * 7) + 6 color COLOR_NAME, "SONG"
	PRINT at (ROW * 10) + 6 color COLOR_TRANS, "BEGIN!" 
	PRINT at 0 color COLOR_DEBUG, "V0.3 - 11/21/15"
	
	gameStatus = STS_MENU
	RETURN
END

REM The logical frame for the main menu
menuFrame: PROCEDURE
	'button actions
	IF ((buttonRegister AND BIT0)>0) THEN GOSUB buttonOneMenu
	IF ((buttonRegister AND BIT1)>0) THEN GOSUB buttonTwoMenu
	IF ((buttonRegister AND BIT2)>0) THEN GOSUB buttonThreeMenu 
	IF ((buttonRegister AND BIT3)>0) THEN GOSUB buttonFourMenu 
	IF ((buttonRegister AND BIT4)>0) THEN GOSUB buttonTabMenu 
	
	'speed setting
	IF (speed = SPD_TEST) THEN PRINT at ((ROW * 6) + 7) color COLOR_VALUE, "test  "  ELSE IF(speed = SPD_TURTLE) THEN PRINT at ((ROW * 6) + 7) color COLOR_VALUE, "turtle"  ELSE IF(speed = SED_SLOW) THEN PRINT at ((ROW * 6) + 7) color COLOR_VALUE, "slow  "  ELSE IF (speed = SPD_MEDIUM) THEN PRINT at ((ROW * 6) + 7) color COLOR_VALUE, "medium"  ELSE IF(speed = SPD_FAST) THEN PRINT at ((ROW * 6) + 7) color COLOR_VALUE, "fast  "  ELSE IF(speed = SPD_INSANE) THEN PRINT at ((ROW * 6) + 7) color COLOR_VALUE, "insane" 
	
	'song setting
	IF (song = 0) THEN PRINT at ((ROW * 8) + 7) color COLOR_VALUE, "random" ELSE IF (song = 1) THEN PRINT at ((ROW * 8) + 7) color COLOR_VALUE, "song 1" ELSE IF (song = 2) THEN PRINT at ((ROW * 8) + 7) color COLOR_VALUE, "song 2"
	
	'erase previous cursor
	PRINT at ((ROW * 3) + 5) color 0, " " : PRINT at ((ROW * 3) + 10) color 0, " " : PRINT at ((ROW * 5) + 5) color 0, " " : PRINT at ((ROW * 5) + 11) color 0, " "  : PRINT at ((ROW * 7) + 5) color 0, " " : PRINT at ((ROW * 7) + 10) color 0, " "  : PRINT at ((ROW * 10) + 5) color 0, " " : PRINT at ((ROW * 10) + 12) color 0, " " 
	
	'draw current cursor
	IF animation = 0 THEN IF cursor = 0 THEN PRINT at ((ROW * 3) + 5) color COLOR_CURSOR, "\264" : PRINT at ((ROW * 3) + 10) color COLOR_CURSOR, "\264" ELSE IF cursor = 1 THEN PRINT at ((ROW * 5) + 5) color COLOR_CURSOR, "\264" : PRINT at ((ROW * 5) + 11) color COLOR_CURSOR, "\264" ELSE IF cursor = 2 THEN PRINT at ((ROW * 7) + 5) color COLOR_CURSOR, "\264" : PRINT at ((ROW * 7) + 10) color COLOR_CURSOR, "\264" ELSE PRINT at ((ROW * 10) + 5) color COLOR_CURSOR, "\264" : PRINT at ((ROW * 10) + 12) color COLOR_CURSOR, "\264"
	IF animation = 1 THEN IF cursor = 0 THEN PRINT at ((ROW * 3) + 5) color COLOR_CURSOR, "\265" : PRINT at ((ROW * 3) + 10) color COLOR_CURSOR, "\265" ELSE IF cursor = 1 THEN PRINT at ((ROW * 5) + 5) color COLOR_CURSOR, "\265" : PRINT at ((ROW * 5) + 11) color COLOR_CURSOR, "\265" ELSE IF cursor = 2 THEN PRINT at ((ROW * 7) + 5) color COLOR_CURSOR, "\265" : PRINT at ((ROW * 7) + 10) color COLOR_CURSOR, "\265" ELSE PRINT at ((ROW * 10) + 5) color COLOR_CURSOR, "\265" : PRINT at ((ROW * 10) + 12) color COLOR_CURSOR, "\265"
	IF animation = 2 THEN IF cursor = 0 THEN PRINT at ((ROW * 3) + 5) color COLOR_CURSOR, "\266" : PRINT at ((ROW * 3) + 10) color COLOR_CURSOR, "\266" ELSE IF cursor = 1 THEN PRINT at ((ROW * 5) + 5) color COLOR_CURSOR, "\266" : PRINT at ((ROW * 5) + 11) color COLOR_CURSOR, "\266" ELSE IF cursor = 2 THEN PRINT at ((ROW * 7) + 5) color COLOR_CURSOR, "\266" : PRINT at ((ROW * 7) + 10) color COLOR_CURSOR, "\266" ELSE PRINT at ((ROW * 10) + 5) color COLOR_CURSOR, "\266" : PRINT at ((ROW * 10) + 12) color COLOR_CURSOR, "\266"
	IF animation = 3 THEN IF cursor = 0 THEN PRINT at ((ROW * 3) + 5) color COLOR_CURSOR, "\267" : PRINT at ((ROW * 3) + 10) color COLOR_CURSOR, "\267" ELSE IF cursor = 1 THEN PRINT at ((ROW * 5) + 5) color COLOR_CURSOR, "\267" : PRINT at ((ROW * 5) + 11) color COLOR_CURSOR, "\267" ELSE IF cursor = 2 THEN PRINT at ((ROW * 7) + 5) color COLOR_CURSOR, "\267" : PRINT at ((ROW * 7) + 10) color COLOR_CURSOR, "\267" ELSE PRINT at ((ROW * 10) + 5) color COLOR_CURSOR, "\267" : PRINT at ((ROW * 10) + 12) color COLOR_CURSOR, "\267"
	
	'increment animation
	IF (FRAME % ANI_SPEED) = 0 THEN IF animation = 3 THEN animation = 0 ELSE animation = animation + 1
	RETURN
END


gameLogicalUpdate: PROCEDURE
	REM increment logicalFrame
	#logicalFrame = #logicalFrame + 1
	
	REM change beat y location
	IF spriteRegister AND BIT0 THEN Y0 = Y0 + 1 : SPRITE 0, $200 + 24, Y0+$100, $800+COLOR_NOTEA+((animation)*8)
	IF spriteRegister AND BIT1 THEN Y1 = Y1 + 1 : SPRITE 1, $200 + 24, Y1+$100, $800+COLOR_NOTEB+((animation)*8)
	IF spriteRegister AND BIT2 THEN Y2 = Y2 + 1 : SPRITE 2, $200 + 32, Y2+$100, $800+COLOR_NOTEA+((animation)*8)
	IF spriteRegister AND BIT3 THEN Y3 = Y3 + 1 : SPRITE 3, $200 + 32, Y3+$100, $800+COLOR_NOTEB+((animation)*8)
	IF spriteRegister AND BIT4 THEN Y4 = Y4 + 1 : SPRITE 4, $200 + 40, Y4+$100, $800+COLOR_NOTEA+((animation)*8)
	IF spriteRegister AND BIT5 THEN Y5 = Y5 + 1 : SPRITE 5, $200 + 40, Y5+$100, $800+COLOR_NOTEB+((animation)*8)
	IF spriteRegister AND BIT6 THEN Y6 = Y6 + 1 : SPRITE 6, $200 + 48, Y6+$100, $800+COLOR_NOTEA+((animation)*8)
	IF spriteRegister AND BIT7 THEN Y7 = Y7 + 1 : SPRITE 7, $200 + 48, Y7+$100, $800+COLOR_NOTEB+((animation)*8)
	
	REM check if beats off screen
	IF Y0 > 102 THEN Y0=0 : spriteRegister = spriteRegister AND BIT0_E : SPRITE 0, 0, 0, 0 : GOSUB scoreMiss
	IF Y1 > 102 THEN Y1=0 : spriteRegister = spriteRegister AND BIT1_E : SPRITE 1, 0, 0, 0 : GOSUB scoreMiss
	IF Y2 > 102 THEN Y2=0 : spriteRegister = spriteRegister AND BIT2_E : SPRITE 2, 0, 0, 0 : GOSUB scoreMiss
	IF Y3 > 102 THEN Y3=0 : spriteRegister = spriteRegister AND BIT3_E : SPRITE 3, 0, 0, 0 : GOSUB scoreMiss
	IF Y4 > 102 THEN Y4=0 : spriteRegister = spriteRegister AND BIT4_E : SPRITE 4, 0, 0, 0 : GOSUB scoreMiss
	IF Y5 > 102 THEN Y5=0 : spriteRegister = spriteRegister AND BIT5_E : SPRITE 5, 0, 0, 0 : GOSUB scoreMiss
	IF Y6 > 102 THEN Y6=0 : spriteRegister = spriteRegister AND BIT6_E : SPRITE 6, 0, 0, 0 : GOSUB scoreMiss
	IF Y7 > 102 THEN Y7=0 : spriteRegister = spriteRegister AND BIT7_E : SPRITE 7, 0, 0, 0 : GOSUB scoreMiss
	
	REM change the size of the beats
	IF ((#logicalFrame+1) % 8) = 0 THEN IF animation THEN animation = 0 ELSE animation = 1
	
	REM release new notes
	IF (gameStatus = STS_CONCLUDE) AND ((#logicalFrame % BEATFRAMES) = 0) THEN GOSUB concludeFrame ELSE IF (#logicalFrame % (BEATFRAMES * 2)) = 0 THEN GOSUB beatFrame
	RETURN
END

clearScreen: PROCEDURE
	FOR i = 0 TO 239
		PRINT at i color 0, " "
	NEXT i
END


REM==========================================================================================
REM====================================BUTTON SUBROUTINES====================================
REM==========================================================================================

REM action for when button one is pressed during the game
buttonOneGame: PROCEDURE
	'catchRegister = catchRegister OR BIT4
	IF catchRegister AND BIT0 THEN GOTO BOneFalse ELSE GOTO BOneTrue
	BOneTrue:
		PRINT at 222 color COLOR_CATCH, "\259"
		catchRegister = catchRegister OR BIT0
	RETURN
	BOneFalse:
		PRINT at 222 color COLOR_CATCH, "\258"
		catchRegister = catchRegister AND BIT0_E
	RETURN
END

REM action for when button two is pressed during the game
buttonTwoGame: PROCEDURE
	'catchRegister = catchRegister OR BIT5
	IF catchRegister AND BIT1 THEN GOTO BTwoFalse ELSE GOTO BTwoTrue
	BTwoTrue:
		PRINT at 223 color COLOR_CATCH, "\259"
		catchRegister = catchRegister OR BIT1
	RETURN
	BTwoFalse:
		PRINT at 223 color COLOR_CATCH, "\258"
		catchRegister = catchRegister AND BIT1_E
	RETURN
END

REM action for when button three is pressed during the game
buttonThreeGame: PROCEDURE
	'catchRegister = catchRegister OR BIT6
	IF catchRegister AND BIT2 THEN GOTO BThreeFalse ELSE GOTO BThreeTrue
	BThreeTrue:
		PRINT at 224 color COLOR_CATCH, "\259"
		catchRegister = catchRegister OR BIT2
	RETURN
	BThreeFalse:
		PRINT at 224 color COLOR_CATCH, "\258"
		catchRegister = catchRegister AND BIT2_E
	RETURN
END

REM action for when button four is pressed during the game
buttonFourGame: PROCEDURE
	'catchRegister = catchRegister or BIT7
	IF catchRegister AND BIT3 THEN GOTO BFourFalse ELSE GOTO BFourTrue
	BFourTrue:
		PRINT at 225 color COLOR_CATCH, "\259"
		catchRegister = catchRegister OR BIT3
	RETURN
	BFourFalse:
		PRINT at 225 color COLOR_CATCH, "\258"
		catchRegister = catchRegister AND BIT3_E
	RETURN
END

REM action for when tab is pressed (catching notes) during the game. This is done by filling a tabRegister with the current locations of the notes and comparing that register with the currently active catches.
buttonTabGame: PROCEDURE
	'buttonRegister = buttonRegister OR BIT5
	'reset tab register
	tabRegister = 0
	
	'set tab register. First detect if there is a note and set on appropriate bit, then detect what quality of note if the quality of note has not been found yet.
	'column 1
	IF Y0 > 89 OR Y1 > 89 THEN tabRegister = BIT0 + BIT7 : IF Y0 = 96 OR Y1 = 96 THEN tabRegister = tabRegister OR BIT4 ELSE IF (Y0 > 92 AND Y0 < 100) OR (Y1 > 92 AND Y1 < 100) THEN tabRegister = tabRegister OR BIT5 ELSE tabRegister = tabRegister OR BIT6
	
	'column 2
	IF Y2 > 89 OR Y3 > 89 THEN tabRegister = tabRegister OR BIT1 : IF (tabRegister AND BIT7) = 0 THEN tabRegister = tabRegister OR BIT7 : IF Y2 = 96 OR Y3 = 96 THEN tabRegister = tabRegister OR BIT4 ELSE IF (Y2 > 92 AND Y2 < 100) OR (Y3 > 92 AND Y3 < 100) THEN tabRegister = tabRegister OR BIT5
	
	'column 3
	IF Y4 > 89 OR Y5 > 89 THEN tabRegister = tabRegister OR BIT2 : IF (tabRegister AND BIT7) = 0 THEN tabRegister = tabRegister OR BIT7 : IF Y4 = 96 OR Y5 = 96 THEN tabRegister = tabRegister OR BIT4 ELSE IF (Y4 > 92 AND Y4 < 100) OR (Y5 > 92 AND Y5 < 100) THEN tabRegister = tabRegister OR BIT5
	
	'column 4
	IF Y6 > 89 OR Y7 > 89 THEN tabRegister = tabRegister OR BIT3 : IF (tabRegister AND BIT7) = 0 THEN tabRegister = tabRegister OR BIT7 : IF Y6 = 96 OR Y7 = 96 THEN tabRegister = tabRegister OR BIT4 ELSE IF (Y6 > 92 AND Y6 < 100) OR (Y7 > 92 AND Y7 < 100) THEN tabRegister = tabRegister OR BIT5 
	
	'Check if there is a match
	IF (tabRegister AND BIT7) > 0 THEN IF (catchRegister AND 15) = (tabRegister AND 15) THEN GOSUB catchNotes : IF (tabRegister AND BIT4) > 0 THEN GOSUB scorePerfect ELSE IF (tabRegister AND BIT5) > 0 THEN GOSUB scoreGood ELSE GOSUB scoreOk
	RETURN
END

REM action for when button four is pressed on the menu. Move cursor up.
buttonOneMenu: PROCEDURE
	IF cursor = 0 THEN cursor = 3 ELSE cursor = cursor - 1
	RETURN
END

REM action for when button four is pressed on the menu. Move cursor down.
buttonTwoMenu: PROCEDURE
	IF cursor = 3 THEN cursor = 0 ELSE cursor = cursor + 1
	RETURN
END

REM action for when button four is pressed on the menu. Go to previous selection
buttonThreeMenu: PROCEDURE
	IF cursor = 1 THEN IF speed = SPD_FAST THEN speed = SPD_INSANE ELSE IF speed = SPD_INSANE THEN Speed = SPD_TEST ELSE speed = speed - 3
	IF cursor = 2 THEN IF song = 1 THEN song = 2 ELSE song = song - 1
	RETURN
END

REM action for when button four is pressed on the menu. Go to next selection.
buttonFourMenu: PROCEDURE
	IF cursor = 1 THEN IF speed = SPD_TEST THEN speed = SPD_INSANE ELSE IF speed = SPD_INSANE THEN speed = SPD_FAST ELSE speed = speed + 3
	IF cursor = 2 THEN IF song = 2 THEN song = 1 ELSE song = song + 1
	RETURN
END

REM action for when button four is pressed on the menu. Select start.
buttonTabMenu: PROCEDURE
	IF cursor = 3 THEN gameStatus = STS_GAME_TRANS
	RETURN
END

buttonScore: PROCEDURE
	IF cursor = 0 THEN cursor = 1 ELSE cursor = 0
	RETURN
END

buttonTabScore: PROCEDURE
	IF cursor = 0 THEN gameStatus = STS_GAME_TRANS ELSE gameStatus = STS_MENU_TRANS
	RETURN
END


REM=========================================================================================
REM====================================CATCH SUBROUTINES====================================
REM=========================================================================================

REM procedure for when beat is missed
scoreMiss: PROCEDURE
	#score = #score/2
	combo = 1
	print at 190 color COLOR_RESPONSE, "MISS    	"
	missCount = missCount + 1
	RETURN
END

REM procedure for when beat is okay
scoreOk: PROCEDURE
	#score = #score + OK_SCORE*combo
	combo = combo + 1
	print at 190 color COLOR_RESPONSE, "OKAY      "
	okCount = okCount + 1
	RETURN
END

REM procedure for when the beat is good
scoreGood: PROCEDURE
	#score = #score + GOOD_SCORE*combo
	combo = combo + 1
	print at 190 color COLOR_RESPONSE, "GOOD!     "
	goodCount = goodCount + 1
	RETURN
END

REM procedure when beat is perfect
scorePerfect: PROCEDURE
	#score = #score + PERF_SCORE*combo
	combo = combo + 1
	print at 190 color COLOR_RESPONSE, "PERFECT!!"
	perfCount = perfCount + 1
	RETURN
END

catchNotes: PROCEDURE
	IF Y0 > 89 THEN Y0 = 0 : spriteRegister = spriteRegister AND BIT0_E : SPRITE 0, 0, 0, 0
	IF Y1 > 89 THEN Y1 = 0 : spriteRegister = spriteRegister AND BIT1_E : SPRITE 1, 0, 0, 0
	IF Y2 > 89 THEN Y2 = 0 : spriteRegister = spriteRegister AND BIT2_E : SPRITE 2, 0, 0, 0
	IF Y3 > 89 THEN Y3 = 0 : spriteRegister = spriteRegister AND BIT3_E : SPRITE 3, 0, 0, 0
	IF Y4 > 89 THEN Y4 = 0 : spriteRegister = spriteRegister AND BIT4_E : SPRITE 4, 0, 0, 0
	IF Y5 > 89 THEN Y5 = 0 : spriteRegister = spriteRegister AND BIT5_E : SPRITE 5, 0, 0, 0
	IF Y6 > 89 THEN Y6 = 0 : spriteRegister = spriteRegister AND BIT6_E : SPRITE 6, 0, 0, 0
	IF Y7 > 89 THEN Y7 = 0 : spriteRegister = spriteRegister AND BIT7_E : SPRITE 7, 0, 0, 0
	RETURN
END

REM=============================================================================================
REM====================================BEATFRAME SUBROUTINES====================================
REM=============================================================================================

REM procedure to be handled at every beat frame (releasing notes)
beatFrame: PROCEDURE
	'if note detected, release proper note
	IF beatRegister AND BIT7 THEN IF (spriteRegister AND BIT0) = 0 THEN spriteRegister = spriteRegister OR BIT0 ELSE IF (spriteRegister AND BIT1) = 0 THEN spriteRegister = spriteRegister OR BIT1
	IF beatRegister AND BIT6 THEN IF (spriteRegister AND BIT2) = 0 THEN spriteRegister = spriteRegister OR BIT2 ELSE IF (spriteRegister AND BIT3) = 0 THEN spriteRegister = spriteRegister OR BIT3
	IF beatRegister AND BIT5 THEN IF (spriteRegister AND BIT4) = 0 THEN spriteRegister = spriteRegister OR BIT4 ELSE IF (spriteRegister AND BIT5) = 0 THEN spriteRegister = spriteRegister OR BIT5
	IF beatRegister AND BIT4 THEN IF (spriteRegister AND BIT6) = 0 THEN spriteRegister = spriteRegister OR BIT6 ELSE IF (spriteRegister AND BIT7) = 0 THEN spriteRegister = spriteRegister OR BIT7
	
	'update beatRegister
	beatRegister = beatRegister * BIT4
	IF beatRegister = 0 THEN READ beatRegister
	beatNumber = beatNumber - 1
	IF beatNumber = 0 THEN gameStatus = STS_CONCLUDE
	RETURN
END

concludeFrame: PROCEDURE
	FOR i = 2 TO 5 STEP 1
		PRINT at (ROW * (beatNumber-2)+i)-20 color COLOR_CURTAIN, "\95"
		IF beatNumber % 2 THEN PRINT at 20*(beatNumber-2)+i color COLOR_CURTAIN, "\260" ELSE PRINT at 20*(beatNumber-2)+i color COLOR_CURTAIN, "\261"
	NEXT i
	beatNumber = beatNumber + 1
	IF beatNumber = 14 THEN gameStatus = STS_SCORE_TRANS
	RETURN
END


REM================================================================================
REM====================================GRAPHICS====================================
REM================================================================================

graphics:
'256 - Beat Small
BITMAP "........"
BITMAP "...XX..."
BITMAP "..XXXX.."
BITMAP ".XXXXXX."
BITMAP ".XXXXXX."
BITMAP "..XXXX.."
BITMAP "...XX..."
BITMAP "........"

'257 - Beat Large
BITMAP "........"
BITMAP "..XXXX.."
BITMAP ".XXXXXX."
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP ".XXXXXX."
BITMAP "..XXXX.."
BITMAP "........"

'258 - Catch Open
BITMAP "X.XXXX.X"
BITMAP ".X....X."
BITMAP "X......X"
BITMAP "X......X"
BITMAP "X......X"
BITMAP "X......X"
BITMAP ".X....X."
BITMAP "X.XXXX.X"

'259 - Catch Closed
BITMAP "X.XXXX.X"
BITMAP ".X....X."
BITMAP "X.X..X.X"
BITMAP "X..XX..X"
BITMAP "X..XX..X"
BITMAP "X.X..X.X"
BITMAP ".X....X."
BITMAP "X.XXXX.X"

'260 - Curtain 1
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP "XXXXX..."
BITMAP ".XX....."

'261 - Curtain 2
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP "...XXXXX"
BITMAP ".....XX."

'262 - Arrow Right
BITMAP "XX......"
BITMAP "XXXX...."
BITMAP "X.XXXX.."
BITMAP ".X..XXXX"
BITMAP "X.XXXX.."
BITMAP "XXXX...."
BITMAP "XX......"
BITMAP "........"

'263 - Arrow Left
BITMAP "X......X"
BITMAP "XXX....X"
BITMAP "XXXXX..X"
BITMAP "XXXXXXXX"
BITMAP "XXXXX..X"
BITMAP "XXX....X"
BITMAP "X......X"
BITMAP "........"

'264 - Bonzai 1
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP ".XXXXXX."

'265 - Bonzai 2
BITMAP "........"
BITMAP "........"
BITMAP "........"
BITMAP "...X...."
BITMAP "....X..."
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP ".XXXXXX."

'266 - Bonzai 3
BITMAP "....XX.."
BITMAP "..XX...."
BITMAP "....X..."
BITMAP "...X...."
BITMAP "....X..."
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP ".XXXXXX."

'267 - Bonzai 4
BITMAP "....XXXX"
BITMAP "XXXX..XX"
BITMAP "XX..X..."
BITMAP "...X...."
BITMAP "....X..."
BITMAP "XXXXXXXX"
BITMAP "XXXXXXXX"
BITMAP ".XXXXXX."


REM=============================================================================
REM====================================SONGS====================================
REM=============================================================================

song1:
DATA $1C, $81, $24, $28, $14, $81, $24, $28, $14, $81, $24, $28, $14, $81, $24, $28, $14, $81, $24, $28, $14, $81, $24, $28, $14, $81, $24, $28, $14

song2:
DATA $6, $81, $24, $28