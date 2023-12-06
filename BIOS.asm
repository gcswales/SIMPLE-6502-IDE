;**************************************************
;*** BIOS ROUTINES ********************************
;**************************************************
;*** TITLE; 	BIOS 							***
;*** FILE;		BIOS.ASM						***
;*** VERSION; 	V1.0							***
;*** AUTHOR; 	GEOFFREY SWALES					***
;**************************************************
;*** DESCRIPTION;								***
;*** 											***
;*** A SERIES OF ROUTINES THAT PROVIDE HARDWARE	***
;*** SPECIFIC I/O FUNCTIONS.					***
;***											***
;**************************************************
;
BIOS_KEEPOUT	= $C0				; KEEPOUT MEMORY $C000 AND ABOVE
;
;**************************************************
;*** BIOS ROUTINES ********************************
;**************************************************
;
IO_REG 			= $D801				; SERIAL STATUS REGISTER
IO_DAT 			= $D800				; SERIAL DATA REGISTER
PATA			= $D000				; PATA REGISTER
;
OSLO			= $FE				; BIOS 16-BIT REGISTER LOW
OSHI			= $FF				; BDOS 16-BIT REGISTER HIGH
;   
;**************************************************
;*** PATA INTERFACE REGISTERS *********************
;**************************************************
;
PATA_DATA		= PATA + $00		; DATA REGISTER(R/W)
PATA_ERR		= PATA + $01		; ERROR REGISTER (R)
PATA_FEAT		= PATA + $01		; FEATURES REGISTER (W)
PATA_SECCO		= PATA + $02		; SECTOR COUNT (R/W)
PATA_LBA0		= PATA + $03		; LBA BITS 0-7 (R/W, LBA MODE)
PATA_LBA1		= PATA + $04		; LBA BITS 8-15 (R/W, LBA MODE)
PATA_LBA2		= PATA + $05		; LBA BITS 16-23 (R/W, LBA MODE)
PATA_LBA3		= PATA + $06		; LBA BITS 24-27 (R/W, LBA MODE)
PATA_STAT		= PATA + $07		; STATUS RESITER (R)
PATA_CMD		= PATA + $07		; COMMAND REGISTER (W)
;
;**************************************************
;*** BIOS MEMORY ($0420 - $04FF) ******************
;**************************************************
;

;   
;**************************************************
;*** BIOS START ADDRESS ***************************
;**************************************************
;
* 					= $E000			; START ADDRESS OF BIOS
;   
;**************************************************
;*** BIOS JUMP TABLE (V1) *************************
;**************************************************
;
		JMP		_COLD_START			; E000 INITIALISE BIOS HARDWARE FROM COLD START
		JMP		_WARM_START			; E003 RE-INITIALISE BIOS HARDWARE (IF REQUIRED)
		JMP		_CONSOLE_STAT		; E006 RETURNS 0 IF CONSOLE HAS INPUT
		JMP		_CONSOLE_GET		; E009 RETURNS INPUT CHARACTER FROM CONSOLE
		JMP		_CONSOLE_PUT		; E00C SENDS DISPLAY CHARACTER TO CONSOLE
		JMP		_SECTOR_SET			; E00F SETS DRIVE SECTOR NUMBER
		JMP		_SECTOR_READ		; E012 READS DRIVE SECTOR
		JMP		_SECTOR_WRITE		; E015 WRITES DRIVE SECTOR
		JMP		_DELAY_MS			; E018 WAITS MS
		JMP		_INFORMATION		; E01B BIOS INFORMATION STRING 
;   
;**************************************************
;*** BIOS COLD START ******************************
;**************************************************
;
_COLD_START:
		JSR		PATA_READY			; WAIT FOR PATA TO BE READY
		LDA		#$81 				; ENSURE 16-BIT MODE
		STA		PATA_FEAT			; STORE IN PATA FEATURE REGISTER
		JSR		PATA_READY			; WAIT FOR PATA TO BE READY
		LDA 	#$EF				; 'SET FEATURE' COMMAND
		STA		PATA_CMD			; WRITE TO PATA COMMAND REGISTER
		JSR		PATA_READY			; WAIT FOR PATA TO BE READY
		JMP		PATA_ERROR			; CHECK FOR ERRORS
;   
;**************************************************
;*** BIOS WARM START ******************************
;**************************************************
;
_WARM_START:
		RTS							; RETURN TO CALLING ROUTINE
;   
;**************************************************
;*** RETURNS 0 IN ACCUMULATOR IF KEY PRESSED ******
;**************************************************
;
_CONSOLE_STAT:	    
		BIT		IO_REG				; LOAD SERIAL STATUS
		RTS
;   
;**************************************************
;*** RETURNS KEYBOARD CHARACTER IN A **************
;**************************************************
;
_CONSOLE_GET:		    
		BIT 	IO_REG     			; TEST SERIAL STATUS
		BPL		_CONSOLE_GET		; BRANCH BACK IS 'N' NEGATIVE (BIT 7) CLEAR
		LDA 	IO_DAT     			; READ DATA FROM SERIAL PORT
		RTS             			; RETURN FROM SUBROUTINE
;   
;**************************************************
;*** PRINTS CHARACTER IN A TO DISPLAY *************
;**************************************************
;
_CONSOLE_PUT:		
		BIT 	IO_REG     			; TEST SERIAL STATUS
		BVS		_CONSOLE_PUT		; BRANCH BACK IF 'V' OVERFLOW (BIT 6) SET
		STA 	IO_DAT     			; WRITE DATA TO SERIAL PORT
		RTS            				; RETURN FROM SUBROUTINE
;   
;**************************************************
;*** WRITE LBA TO PATA ****************************
;**************************************************
;
_SECTOR_SET:
		JSR		PATA_READY			; WAIT FOR PATA TO BE READY
		LDA		OSLO				; LOAD LBA0 VALUE
		STA		PATA_LBA0			; STORE IN LBA0 PATA REGISTER
		LDA		OSHI				; LOAD LBA1 VALUE
		STA		PATA_LBA1			; STORE IN LBA1 PATA REGISTER
		LDA		#$00				; LBA2 IS JUST 0 FOR NOW
		STA		PATA_LBA2			; STORE IN LBA2 PATA REGISTER
		LDA		#$E0				; SET MASTER (4) WITH LBA (6) MODE
		STA		PATA_LBA3			; STORE THIS IN LBA3 REGISTER
		LDA		#$01				; ONLY READ/WRITE 1 SECTOR AT A TIME
		STA		PATA_SECCO			; STORE IN SECTOR COUNT REGISTER
		LDA		#NO_ERROR			; RETURN WITH NO ERRORS
		RTS							; RETURN TO CALLING ROUTINE
;   
;**************************************************
;*** READS 256 BYTES FROM PATA ********************
;**************************************************
;
_SECTOR_READ:
.(
		JSR		PATA_READY			; WAIT FOR PATA TO BE READY
		LDA 	#$20				; LOAD READ COMMAND
		STA 	PATA_CMD			; SEND COMMAND TO PATA DEVICE
		JSR		PATA_READY			; WAIT FOR PATA TO BE READY
		JSR		PATA_ERROR			; CHECK FOR ERRORS
		BNE		DONE				; BRANCH IF WE HAVE AN ERROR
		JSR		PATA_REQUEST		; WAIT FOR DATA REQUEST READY
		JSR		PATA_ERROR			; CHECK FOR ERRORS
		BNE		DONE				; BRANCH IF WE HAVE AN ERROR
		PHY							; PUSH Y ON STACK
		LDY		#$00				; INITIALISE Y FOR LOOP
LOOP:
		LDA		PATA_DATA			; READ DATA BYTE FROM PATA DEVICE
		STA 	(OSLO),Y			; STORE AT ADDRESS POINTER OFFSET BY Y
		INY							; INCREMENT Y OFFSET
		BNE		LOOP				; BRANCH IS NOT WRAPPED AROUND TO 0
		PLY							; RESTORE Y FROM STACK
		LDA		#NO_ERROR			; RETURN WITH NO ERRORS
DONE:
		RTS							; RETURN WITH ANY ERROR IN A
.)
;   
;**************************************************
;*** WRITES 256 BYTES TO PATA *********************
;**************************************************
;
_SECTOR_WRITE:
.(
		JSR		PATA_READY			; WAIT FOR PATA TO BE READY
		LDA 	#$30				; LOAD WRITE COMMAND
		STA 	PATA_CMD			; SEND COMMAND TO PATA DEVICE
		JSR		PATA_READY			; WAIT FOR PATA TO BE READY
		JSR		PATA_ERROR			; CHECK FOR ERRORS
		BNE		DONE				; BRANCH IS WE HAVE AN ERROR
		JSR		PATA_REQUEST		; WAIT FOR DATA REQUEST READY
		PHY							; SAVE Y TO STACK
		LDY		#$00				; INITIALISE Y FOR LOOP
LOOP:
		LDA 	(OSLO),Y			; LOAD FROM ADDRESS OFFSET BY Y
		STA		PATA_DATA			; STORE BYTE IN PATA DEVICE
		INY							; INCREMENT Y OFFSET
		BNE		LOOP				; BRANCH IF NOT WRAPPED AROUND TO 0
		PLY							; RESTORE Y FROM STACK
		JSR		PATA_READY			; WAIT FOR PATA TO BE READY
		JSR		PATA_ERROR			; CHECK FOR ERRORS
DONE:
		RTS							; RETURN WITH ANY ERROR IN A
.)
;   
;**************************************************
;*** WAITS NUMBER OF MILLISECONDS IN A ************
;**************************************************
;
_DELAY_MS:
		RTS							; DO NOTHING FOR NOW
;   
;**************************************************
;*** RETURNS HARDWARE INFORMATION *****************
;**************************************************
;
_INFORMATION:
		LDA		#<BIOS_MESSAGE		; LOAD BIOS MESSAGE ADDRESS LOW BYTE
		STA		OSLO				; STORE IN APILO
		LDA		#>BIOS_MESSAGE		; LOAD BIOS MESSAGE ADDRESS HIGH BYTE
		STA		OSHI				; STORE IN APIHI
		LDA		#BIOS_KEEPOUT		; LOAD X WITH RESERVED MEMORY PAGE
		RTS 						; RETURN TO OS
BIOS_MESSAGE:
		.BYT	"SERIAL BIOS V1.0",$00 ; NULL TERMINATED BIOS MESSAGE STRING
;   
;**************************************************
;*** WAITS UNTIL PATA DEVICE IS READY *************
;**************************************************
;
PATA_READY:
		BIT 	PATA_STAT			; TEST PATA STATUS REGISTER
		BMI		PATA_READY			; BRANCH BACK 'BUSY' (BIT 7) SET
		BVC		PATA_READY 			; BRANCH BACK 'DRIVE READY' (BIT 6) CLEAR
		RTS							; RETURN TO CALLING ROUTINE			
;   
;**************************************************
;*** WAITS UNTIL PATA DATA REQUEST READY **********
;**************************************************
;
PATA_REQUEST:
		LDA		PATA_STAT			; LOAD PATA STATUS
		BMI		PATA_REQUEST		; BRANCH BACK BUSY (BIT 7) SET			
		AND		#$08				; MASK OFF ALL BUT BIT 3 (DRQ)
		BEQ		PATA_REQUEST		; BRANCH IF BIT 3 NOT SET
		RTS							; RETURN TO CALLING ROUTINE
;   
;**************************************************
;*** CHECKS FOR PATA ERROR ************************
;**************************************************
;
PATA_ERROR:
.(
		LDA		PATA_STAT			; READ PATA STATUS REGISTER
		AND		#$01				; MASK OFF ALL BUT BIT 0 (ERR)
		BEQ		DONE				; BRANCH IF NO ERRORS
		LDA		#ERR_BIOS			; LOAD BIOS ERROR
DONE:
		RTS							; RETURN TO CALLING ROUTINE
.)
;   
;**************************************************
;*** WAIT CYCLE = 9*(A*256+Y)+20 ******************
;**************************************************
;
DELAY_LOOP:
		CPY		#$01
		DEY
		SBC		#$00
		BCS		DELAY_LOOP
		RTS
;   
;**************************************************
;*** END ******************************************
;**************************************************
;

