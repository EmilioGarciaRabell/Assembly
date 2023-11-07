            TTL CMPE 250 Exercise 08
;****************************************************************
;Exercise 08 - Multiprecision Arithmetic
;The purpose of this exercise is to investigate input, storage, addition, 
;and output of numbers larger than a KL05 word.
;Name: Emilio Garcia
;---------------------------------------------------------------
;Keil Template for KL05
;R. W. Melton
;September 13, 2020
;****************************************************************
;Assembler directives
            THUMB
            OPT  64  ;Turn on listing macro expansions
;****************************************************************
;Include files
            GET  MKL05Z4.s     ;Included by start.s
            OPT  1   ;Turn on listing
;****************************************************************
;EQUates

;--- queue structure sizes
Q_BUF_SZ    EQU  4   ;queue contents
Q_REC_SZ    EQU  18  ;queue management record

;--- from exercise 06
MAX_STRING	EQU  79
CR          EQU  0x0D
LF          EQU  0x0A
NULL        EQU  0x00
BS          EQU  0x08

;--- for C flag
C_MASK      EQU	 0x20
C_SHIFT     EQU	 24
	
;--- for PutNumHex subroutine
RIGHT_NIBBLE_MASK   EQU  0x0F
	
;--- for PutNumUB subroutine
PutNumUB_MASK       EQU  0xFF
	
;--- for exercise 08 multiprecision arithmetic
NUM_SIZE    EQU  16
WORD_COUNT  EQU  3

;---------------------------------------------------------------
;PORTx_PCRn (Port x pin control register n [for pin n])
;___->10-08:Pin mux control (select 0 to 8)
;Use provided PORT_PCR_MUX_SELECT_2_MASK
;---------------------------------------------------------------
;Port B
PORT_PCR_SET_PTB2_UART0_RX  EQU  (PORT_PCR_ISF_MASK :OR: \
                                  PORT_PCR_MUX_SELECT_2_MASK)
PORT_PCR_SET_PTB1_UART0_TX  EQU  (PORT_PCR_ISF_MASK :OR: \
                                  PORT_PCR_MUX_SELECT_2_MASK)
;---------------------------------------------------------------
;SIM_SCGC4
;1->10:UART0 clock gate control (enabled)
;Use provided SIM_SCGC4_UART0_MASK
;---------------------------------------------------------------
;SIM_SCGC5
;1->09:Port A clock gate control (enabled)
;Use provided SIM_SCGC5_PORTA_MASK
;---------------------------------------------------------------
;SIM_SOPT2
;01=27-26:UART0SRC=UART0 clock source select (MCGFLLCLK)
;---------------------------------------------------------------
SIM_SOPT2_UART0SRC_MCGFLLCLK  EQU  \
                                 (1 << SIM_SOPT2_UART0SRC_SHIFT)
;---------------------------------------------------------------
;SIM_SOPT5
; 0->   16:UART0 open drain enable (disabled)
; 0->   02:UART0 receive data select (UART0_RX)
;00->01-00:UART0 transmit data select source (UART0_TX)
SIM_SOPT5_UART0_EXTERN_MASK_CLEAR  EQU  \
                               (SIM_SOPT5_UART0ODE_MASK :OR: \
                                SIM_SOPT5_UART0RXSRC_MASK :OR: \
                                SIM_SOPT5_UART0TXSRC_MASK)
;---------------------------------------------------------------
;UART0_BDH
;    0->  7:LIN break detect IE (disabled)
;    0->  6:RxD input active edge IE (disabled)
;    0->  5:Stop bit number select (1)
;00001->4-0:SBR[12:0] (UART0CLK / [9600 * (OSR + 1)]) 
;UART0CLK is MCGPLLCLK/2
;MCGPLLCLK is 96 MHz
;MCGPLLCLK/2 is 48 MHz
;SBR = 48 MHz / (9600 * 16) = 312.5 --> 312 = 0x138
UART0_BDH_9600  EQU  0x01
;---------------------------------------------------------------
;UART0_BDL
;26->7-0:SBR[7:0] (UART0CLK / [9600 * (OSR + 1)])
;UART0CLK is MCGPLLCLK/2
;MCGPLLCLK is 96 MHz
;MCGPLLCLK/2 is 48 MHz
;SBR = 48 MHz / (9600 * 16) = 312.5 --> 312 = 0x138
UART0_BDL_9600  EQU  0x38
;---------------------------------------------------------------
;UART0_C1
;0-->7:LOOPS=loops select (normal)
;0-->6:DOZEEN=doze enable (disabled)
;0-->5:RSRC=receiver source select (internal--no effect LOOPS=0)
;0-->4:M=9- or 8-bit mode select 
;        (1 start, 8 data [lsb first], 1 stop)
;0-->3:WAKE=receiver wakeup method select (idle)
;0-->2:IDLE=idle line type select (idle begins after start bit)
;0-->1:PE=parity enable (disabled)
;0-->0:PT=parity type (even parity--no effect PE=0)
UART0_C1_8N1  EQU  0x00
;---------------------------------------------------------------
;UART0_C2
;0-->7:TIE=transmit IE for TDRE (disabled)
;0-->6:TCIE=transmission complete IE for TC (disabled)
;0-->5:RIE=receiver IE for RDRF (disabled)
;0-->4:ILIE=idle line IE for IDLE (disabled)
;1-->3:TE=transmitter enable (enabled)
;1-->2:RE=receiver enable (enabled)
;0-->1:RWU=receiver wakeup control (normal)
;0-->0:SBK=send break (disabled, normal)
UART0_C2_T_R  EQU  (UART0_C2_TE_MASK :OR: UART0_C2_RE_MASK)
;---------------------------------------------------------------
;UART0_C3
;0-->7:R8T9=9th data bit for receiver (not used M=0)
;           10th data bit for transmitter (not used M10=0)
;0-->6:R9T8=9th data bit for transmitter (not used M=0)
;           10th data bit for receiver (not used M10=0)
;0-->5:TXDIR=UART_TX pin direction in single-wire mode
;            (no effect LOOPS=0)
;0-->4:TXINV=transmit data inversion (not inverted)
;0-->3:ORIE=overrun IE for OR (disabled)
;0-->2:NEIE=noise error IE for NF (disabled)
;0-->1:FEIE=framing error IE for FE (disabled)
;0-->0:PEIE=parity error IE for PF (disabled)
UART0_C3_NO_TXINV  EQU  0x00
;---------------------------------------------------------------
;UART0_C4
;    0-->  7:MAEN1=match address mode enable 1 (disabled)
;    0-->  6:MAEN2=match address mode enable 2 (disabled)
;    0-->  5:M10=10-bit mode select (not selected)
;01111-->4-0:OSR=over sampling ratio (16)
;               = 1 + OSR for 3 <= OSR <= 31
;               = 16 for 0 <= OSR <= 2 (invalid values)
UART0_C4_OSR_16           EQU  0x0F
UART0_C4_NO_MATCH_OSR_16  EQU  UART0_C4_OSR_16
;---------------------------------------------------------------
;UART0_C5
;  0-->  7:TDMAE=transmitter DMA enable (disabled)
;  0-->  6:Reserved; read-only; always 0
;  0-->  5:RDMAE=receiver full DMA enable (disabled)
;000-->4-2:Reserved; read-only; always 0
;  0-->  1:BOTHEDGE=both edge sampling (rising edge only)
;  0-->  0:RESYNCDIS=resynchronization disable (enabled)
UART0_C5_NO_DMA_SSR_SYNC  EQU  0x00
;---------------------------------------------------------------
;UART0_S1
;0-->7:TDRE=transmit data register empty flag; read-only
;0-->6:TC=transmission complete flag; read-only
;0-->5:RDRF=receive data register full flag; read-only
;1-->4:IDLE=idle line flag; write 1 to clear (clear)
;1-->3:OR=receiver overrun flag; write 1 to clear (clear)
;1-->2:NF=noise flag; write 1 to clear (clear)
;1-->1:FE=framing error flag; write 1 to clear (clear)
;1-->0:PF=parity error flag; write 1 to clear (clear)
UART0_S1_CLEAR_FLAGS  EQU  (UART0_S1_IDLE_MASK :OR: \
                            UART0_S1_OR_MASK :OR: \
                            UART0_S1_NF_MASK :OR: \
                            UART0_S1_FE_MASK :OR: \
                            UART0_S1_PF_MASK)
;---------------------------------------------------------------
;UART0_S2
;1-->7:LBKDIF=LIN break detect interrupt flag (clear)
;             write 1 to clear
;1-->6:RXEDGIF=RxD pin active edge interrupt flag (clear)
;              write 1 to clear
;0-->5:(reserved); read-only; always 0
;0-->4:RXINV=receive data inversion (disabled)
;0-->3:RWUID=receive wake-up idle detect
;0-->2:BRK13=break character generation length (10)
;0-->1:LBKDE=LIN break detect enable (disabled)
;0-->0:RAF=receiver active flag; read-only
UART0_S2_NO_RXINV_BRK10_NO_LBKDETECT_CLEAR_FLAGS  EQU  \
        (UART0_S2_LBKDIF_MASK :OR: UART0_S2_RXEDGIF_MASK)

;****************************************************************
;Program
;Linker requires Reset_Handler
            AREA    MyCode,CODE,READONLY
            ENTRY
            EXPORT  Reset_Handler
            IMPORT  Startup
			
			;IMPORT  GetHexIntMulti
			;EXPORT  GetStringSB
				
			;IMPORT  PutHexIntMulti
            ;EXPORT  PutNumHex
			
Reset_Handler  PROC  {}
main
;---------------------------------------------------------------
;Mask interrupts
            CPSID   I
;KL05 system startup with 48-MHz system clock
            BL      Startup
;---------------------------------------------------------------
;>>>>> begin main program code <<<<<
	BL      Init_UART0_Polling

Restart  
	LDR     R0,=FirstPromptP         
	MOVS    R1,#MAX_STRING
	BL      PutStringSB         ;print prompt

FirstPr   
    LDR     R0,=Number1	        ;First n-word 
	MOVS    R1,#WORD_COUNT      ;number of words in input
	BL      GetHexIntMulti
	BCS     OneInvalid ; Show if invalid

	BL      NewLine
	LDR	    R0,=SecondPromptP
	MOVS    R1,#MAX_STRING
	BL      PutStringSB

SecndPr  
    LDR     R0,=Number2	        ;2nd n-word number from user
	MOVS    R1,#WORD_COUNT      ;number of words in input
	BL      GetHexIntMulti

	BCS     TwoInvalid

	; Make the preliminary steps for addition
	LDR		R0,=Result          
	LDR		R1,=Number1         
	LDR		R2,=Number2         ;load Address of result and the first and second number
	MOVS	R3,#WORD_COUNT      
	BL      AddIntMultiU 
	BCS     AddOverflow		;check for invalid addition

	BL		NewLine

	LDR		R0,=SumString       ;load address of Sum
	MOVS	R1,#MAX_STRING
	BL 		PutStringSB

	LDR		R0,=Result
	MOVS	R1,#WORD_COUNT      
	BL		PutHexIntMulti
	BL		NewLine
	B		Restart

AddOverflow ; Show the Overflow case message
	BL      NewLine
	LDR		R0,=OverflowOutp
	MOVS	R1,#MAX_STRING
	BL		PutStringSB
	BL      NewLine
	B		Restart


OneInvalid 
    BL		NewLine
	LDR		R0,=PromptInvalid
	MOVS	R1,#MAX_STRING
	BL		PutStringSB
	B		FirstPr

TwoInvalid 
    BL		NewLine
	LDR		R0,=PromptInvalid
	MOVS	R1,#MAX_STRING
	BL		PutStringSB
	B		SecndPr


;>>>>>   end main program code <<<<<
;Stay here
            B       .
            ENDP

;---------- for multiprecision arithmetic
			LTORG

;>>>>> begin subroutine code <<<<<

Init_UART0_Polling  PROC  {R0-R14}

	PUSH    {R0, R1, R2}
	LDR     R0,=SIM_SOPT2                           ;connect Sources
	LDR     R1,=SIM_SOPT2_UART0SRC_MASK             
	LDR     R2,[R0,#0]                              ;current SIM_SOPT2 value
	BICS    R2,R2,R1                                ;clear bits of UART0SRC
	LDR     R1,=SIM_SOPT2_UART0SRC_MCGFLLCLK        
	ORRS    R2,R2,R1                                ;UART0 bits changed
	STR     R2,[R0,#0]                              ;update SIM_SOPT2
	
	LDR     R0,=SIM_SOPT5                           ;set SIM_SOPT5 for UART0 external
	LDR     R1,= SIM_SOPT5_UART0_EXTERN_MASK_CLEAR
	LDR     R2,[R0,#0]                              ;current SIM_SOPT5 value
	BICS    R2,R2,R1                                ;UART0 bits cleared
	STR     R2,[R0,#0]                              ;update SIM_SOPT5
	
	LDR     R0,=SIM_SCGC4                           ;enable SIM_SCGC4 as clock
	LDR     R1,=SIM_SCGC4_UART0_MASK                
	LDR     R2,[R0,#0]                              ;current SIM_SCGC4 value
	ORRS    R2,R2,R1                                ;only UART0 bit set
	STR     R2,[R0,#0]                              ;update SIM_SCGC4

	LDR     R0,=SIM_SCGC5                           ;enable clock
	LDR     R1,= SIM_SCGC5_PORTB_MASK 
	LDR     R2,[R0,#0]                              ;current SIM_SCGC5 value
	ORRS    R2,R2,R1                                ;only PORTB bit set
	STR     R2,[R0,#0]                              ;update SIM_SCGC5
	
	LDR     R0,=PORTB_PCR2
	LDR     R1,=PORT_PCR_SET_PTB2_UART0_RX
	STR     R1,[R0,#0]                              ;port B pin 2 connects to UART0 Rx
	 
	LDR     R0,=PORTB_PCR1
	LDR     R1,=PORT_PCR_SET_PTB1_UART0_TX
	STR     R1,[R0,#0]                              ;port B pin 1 connects to UART0 Tx

;---------- load base address
	LDR     R0,=UART0_BASE
	 
;---------- disable UART0
	MOVS    R1,#UART0_C2_T_R
	LDRB    R2,[R0,#UART0_C2_OFFSET]
	BICS    R2,R2,R1
	STRB    R2,[R0,#UART0_C2_OFFSET]
	
;---------- set UART0 baud rate
	MOVS    R1,#UART0_BDH_9600
	STRB    R1,[R0,#UART0_BDH_OFFSET]
	MOVS    R1,#UART0_BDL_9600
	STRB    R1,[R0,#UART0_BDL_OFFSET]

;---------- set UART0 char format for serial bit stream and clear flags
	MOVS    R1,#UART0_C1_8N1
	STRB    R1,[R0,#UART0_C1_OFFSET]
	MOVS    R1,#UART0_C3_NO_TXINV
	STRB    R1,[R0,#UART0_C3_OFFSET]
	MOVS    R1,#UART0_C4_NO_MATCH_OSR_16
	STRB    R1,[R0,#UART0_C4_OFFSET]
	MOVS    R1,#UART0_C5_NO_DMA_SSR_SYNC
	STRB    R1,[R0,#UART0_C5_OFFSET]
	MOVS    R1,#UART0_S1_CLEAR_FLAGS
	STRB    R1,[R0,#UART0_S1_OFFSET]
	MOVS    R1,#UART0_S2_NO_RXINV_BRK10_NO_LBKDETECT_CLEAR_FLAGS
	STRB    R1,[R0,#UART0_S2_OFFSET]
	
;---------- enable UART0
	MOVS    R1,#UART0_C2_T_R
	STRB    R1,[R0,#UART0_C2_OFFSET]
	
	POP     {R0,R1,R2}
	BX      LR
	ENDP    

;---------------------------------------------------------------
;Print a character in the terminal
;---------------------------------------------------------------
PutChar		PROC	{R0-R14}
	PUSH	{R1, R2, R3} 
	
	
	LDR R1,=UART0_BASE			
	MOVS R2,#UART0_S1_TDRE_MASK
	
PollTx 		LDRB R3,[R1,#UART0_S1_OFFSET]

	ANDS R3,R3, R2
	
	BEQ PollTx
	
	;Transmit character stored in R0
	STRB R0,[R1,#UART0_D_OFFSET]
	POP		{R1, R2, R3}
	
	BX    LR                
	ENDP

;---------------------------------------------------------------
;Get a character from the terminal
;---------------------------------------------------------------
GetChar		PROC {R0-R13}

	PUSH 	{R1-R7}
	LDR 	R1,=UART0_BASE 
	MOVS 	R2,#UART0_S1_RDRF_MASK 
	
PollRx 		LDRB 	R3,[R1,#UART0_S1_OFFSET] 
	ANDS 	R3,R3,R2
	BEQ 	PollRx 
	LDRB 	R0,[R1,#UART0_D_OFFSET] 
	POP		{R1-R7}
	BX 		LR
	ENDP
	
		
				
;-------------------------------------------- 
; GetStringSB subroutine, get a character input from user
;--------------------------------------------

GetStringSB PROC    {R1-R14}

	PUSH    {R0,R1,R2,R3,LR}           ;push to stack
	MOVS    R2,#0                      ;initalize string offset
	MOVS    R3,R0 					   ;save input char 	 
	
Input       BL      GetChar                    ;get next char of string

	CMP     R0,#CR                     ;check for carrige return
	BEQ     EndGetStringSB             ;end

	CMP     R1,#1                      ;check if string ended
	BEQ     Input			                                   
	
	CMP     R0,#BS                     ;check for backspace
	BEQ     Input_BS
	
	BL      PutChar                    ;echo to terminal
	STRB    R0,[R3,R2]                 ;store input char in string array
	
	SUBS    R1,R1,#1                   ;decrement number of chars left to read
	
	ADDS	R2,R2,#1                   ;add string's offset index			
	B       Input                      ;loop

Input_BS    CMP     R2,#0
	BEQ     Input
	SUBS    R2,R2,#1                   ;decrease offset
	B       Input

EndGetStringSB
	MOVS    R0,#0                      ;null termination
	STRB    R0,[R3,R2]
	POP     {R0,R1,R2,R3,PC}           ;for nested subroutine
	ENDP



;-------------------------------------------- 
; PutStringSB shows a string to terminal
;--------------------------------------------

PutStringSB PROC    {R0-R14}
            
			PUSH    {R0-R2, LR}                ;for nested subroutine
			CMP     R1,#0                      ;if all characters have been processed
            BEQ     EndPutStringSB             ;end
            
			ADDS    R1,R1,R0
            MOVS    R2, R0                     ;save R0 to R2

ReadChar    LDRB    R0,[R2,#0]
            
			CMP     R0,#NULL                   ;if none
            BEQ     EndPutStringSB             ;end
            BL      PutChar                    ;echo to terminal
            
			ADDS    R2, R2, #1                 ;point to next value
            CMP     R2,R1
            BNE     ReadChar                   ;loop
			
EndPutStringSB
            POP     {R0-R2, PC}                ;for nested subroutine
            ENDP
			


;-------------------------------------------- 
; uses Putcar to Display the text decimal representation of word value in R0
;--------------------------------------------

PutNumU   	PROC    {R0-R14}
	PUSH    {R0,R1,R2,LR}              ;for nested subroutine
	MOVS    R2,#0                      ;initalize array offset
   
DIV10       
	CMP     R0,#10                     ;check if num < 10
	BLT     EndPutNumU
   
	MOVS    R1,R0                      ;dividend in R1
	MOVS    R0,#10                     ;divisor is 10
	BL      DIVU                       ;divide
   
	PUSH    {R0}
	LDR     R0,=putUvar

	STRB    R1,[R0,R2]
	ADDS    R2,R2,#1
	POP     {R0}
	B       DIV10                      ;keep diving by 10 until it can't
   
EndPutNumU  
	ADDS    R0,R0,#'0'                 ;convert to ascii
	BL      PutChar                    ;echo to terminal
	SUBS    R2,R2,#1                   ;decrement string array

PrintChar   
	LDR     R0,=putUvar         ;array iteration
	CMP     R2,#0
	BLT     EndPutNum

	LDRB    R1,[R0,R2]
	MOVS    R0,R1

	ADDS    R0,R0,#'0'                 ;convert to ascii
	BL      PutChar                    ;echo to terminal

	SUBS    R2,R2,#1
	B       PrintChar
   
EndPutNum   
	POP     {R0-R2,PC}              ;for nested subroutine
	ENDP




;---------------------------------------------------------------
;Prints to the terminal screen the text decimal representation of the
;unsigned byte value in R0. (For example, if R0 contains 0x003021101, then 1 should
;print on the terminal. Note: 001 would also be acceptable, as the text decimal
;representation of 0x01.) (Hint: use a mask to preserve only the least byte of the word
;in R0, and call your PutNumU subroutine from Lab Exercise Six.)
;---------------------------------------------------------------
PutNumUB PROC
	PUSH {R0}

	LDR R0,[R0,#0]
	MOVS R4,#0xFF
	ANDS R0,R0,R4
	BL PutNumU
	
	
	POP {R0}
	ENDP
		
	


;--------------------------------------------
; Performs integer division of R1/R0, if divided by 0, C is set
;--------------------------------------------

DIVU 		PROC 	{R2-R14} ; 
	PUSH{R2, R3, R4}
	
	CMP R0, #0 ; Check if R0 is = 0
	
	BEQ DivB0

	
	MOVS R2, #0 ;

WhileLoopD
	CMP R1, R0				; while(R0 > R1){
	BLO Remainder  
	
	ADDS R2, R2, #1 		; Quotient++
	SUBS R1, R1, R0 		; Divisor -= Dividend 
	
	B WhileLoopD			;}
	
	
Remainder	
	
	MOVS R0, R2 ; New Quotient 
	
	MRS R3, APSR
	MOVS R4, #0x20
	LSLS R4, R4, #24
	BICS R3, R3, R4
	MSR APSR, R3
	
	B EndDivu
	
DivB0			 
	MRS R3, APSR
	MOVS R4, #0x20
	LSLS R4, R4, #24
	ORRS R3, R3, R4
	MSR APSR, R3
	
	
EndDivu
	
	POP{R2, R3, R4}
	BX	LR
	ENDP 
			

;-------------------------------------------- 
; NewLine subroutine Print a new line to the terminal
;--------------------------------------------

NewLine  PROC    {R0-R14}
	PUSH    {R0,LR}                    ;for nested subroutine
   
	MOVS    R0,#CR
	BL      PutChar
	MOVS    R0,#LF
	BL      PutChar

	POP	    {R0,PC}                    ;for nested subroutine
	ENDP				


;---------------------------------------------------------------
;Prints to the terminal screen the text hexadecimal representation of the
;unsigned word value in R0. (For example, if R0 contains 0x000012FF, then 000012FF
;should print on the terminal. Note: 12FF would not be acceptable. Do not use division
;to determine the hexadecimal digit values�use bit masks and shifts.)
;---------------------------------------------------------------
PutNumHex PROC

  PUSH {R1-R4,LR}  ; Save registers
  
  ; Initialize ShiftAmount to 28
  MOVS R3, #28

ForHex
  CMP R3, #0      ; Check if ShiftAmount >= 0
  BLT EndForHex   ; If not, exit loop


  ; Shift and mask

  MOVS R4, #0x0F   ; Mask for lower 4 bits
  
  MOVS R2,R0
  LSRS R2, R2, R3  ; Shift right by 4 bits
  ANDS R2, R2, R4  ; Mask out upper bits

  CMP R2,#10
  BGE     ConvertToAF                ;if ASCII value >= 10,
  ADDS    R2,#'0'                    ;else, add 0 to ASCII
  B       PrintX
  CMP R3, #0      ; Check if ShiftAmount >= 0
  BLT EndForHex   ; If not, exit loop

ConvertToAF
  ADDS    R2,R2,#55 ;Convert to uppercase
  
PrintX
	PUSH    {R0}                       ;keep input unchanged
	MOVS    R0,R2
	BL      PutChar
	POP     {R0}
	
	MOVS    R2,#0                     ;reset R4
	
	SUBS    R3,R3,#4                  ;decrement loop counter
	B       ForHex             ;loop


EndForHex
  POP {R1-R4,PC}  ; Restore registers and return
  ENDP

	



; -------------------------------------------
; AddIntMultiU subroutine 
; -------------------------------------------


AddIntMultiU  PROC    {R1-R14}
    PUSH    {R0-R7,LR}			

;  Clear C
    MOVS    R4,#0
    LSRS    R4,R4,#1
    
; Preserve flags
    MRS     R7,APSR

AddWord
; check for end of loop
    CMP 	R3,#0
    BEQ 	AddIntMultiUEnd
    
; load words into registers one at a time
    LDM 	R1!,{R5}
    LDM 	R2!,{R6}
    MSR     APSR,R7
        
    ADCS	R5,R5,R6       ;Carry plus 1st and 2nd word
    STM		R0!,{R5}       ;store addition and carry
    MRS     R7,APSR        
    SUBS 	R3,R3,#1       ;decrememnt word count
    
    B 		AddWord        ;continue adding word

AddIntMultiUEnd 
    MSR 	APSR,R7
    POP 	{R0-R7,PC}
    ENDP




; -------------------------------------------
; GetHexIntMulti subroutine
; -------------------------------------------

GetHexIntMulti  PROC 	{R0-R14}
    PUSH	{R0-R7,LR}
    
; convert buffer  to bytes
    LSLS    R1,#3
    ADDS    R1,R1,#1
    
    MOVS	R7,R0            ;store
    
Repeat		    LDR		R0,=StringBuffer
    BL		GetStringSB
    
Proceed			MOVS 	R2,#0
    SUBS	R1,R1,#1         ;decrement 
    
;-------------- get numbers from mem
Convert 		
    LDRB	R3,[R0,R2] 
    CMP		R3,#'9' 
    BHI     Higher
    CMP		R3,#'0'
    BLO		GetHexInvalid
    SUBS	R3,R3,#'0'	     ;get decimal value
    B		GetHexStore

Higher			
    CMP		R3,#'A' ; For uppercase case
    BLO     GetHexInvalid
    CMP		R3,#'F'
    BHI		Lower
    SUBS    R3,R3,#('A'-10)	 
    B		GetHexStore
    
Lower			
    CMP		R3,#'a'  ;for lowercase case
    BLO     GetHexInvalid
    CMP		R3,#'f'
    BHI		GetHexInvalid
    SUBS    R3,R3,#('a'-10)	
    
GetHexStore		
    STRB 	R3,[R0,R2]		
    ADDS	R2,R2,#1		 
    CMP		R2,R1		 ;check if at end of input	       
    BNE     Convert


; offset counter
PreparePack     
    SUBS	R1,R1,#1         
    MOVS    R2,#0            
    MOVS	R5,R7

PackLP	     	
    CMP		R1,#0		     ;check if at end of value
    BLE		DonePack

    LDRB	R6,[R0,R1]       
    SUBS	R1,R1,#1 
    LDRB	R7,[R0,R1]      

    LSLS	R7,#4
    ADDS	R6,R6,R7         ;concatenate
    STRB	R6,[R5,R2]       ;store number
    ADDS	R2,R2,#1         ;upcounter++
    SUBS	R1,R1,#1         ;decrement offset counter
    B		PackLP

; carry set
GetHexInvalid	MOVS    R4,#1
    LSRS    R4,R4,#1
    B		GetHexEnd

; carry cleared	
DonePack		MOVS    R4,#0
    LSRS    R4,R4,#1
    
GetHexEnd		POP		{R0-R7,PC}
    ENDP

; -------------------------------------------
; PutHexIntMulti subroutine
; -------------------------------------------
PutHexIntMulti	PROC    {R0-R14}
    PUSH	{R0-R2,LR}
    
    MOVS	R2,R0 ; Input from R0
                        
    LSLS	R1,R1,#2 ;number of bytes		
    SUBS	R1,R1,#4 
    
Loop			LDR		R0,[R2,R1]				
    BL		PutNumHex
    
    SUBS	R1,R1,#4 ;previous word
    BHS		Loop
    
    POP		{R0-R2,PC}
    ENDP

    
					
;>>>>>   end subroutine code <<<<<
            ALIGN
;****************************************************************
;Vector Table Mapped to Address 0 at Reset
;Linker requires __Vectors to be exported
            AREA    RESET, DATA, READONLY
            EXPORT  __Vectors
            EXPORT  __Vectors_End
            EXPORT  __Vectors_Size
            IMPORT  __initial_sp
            IMPORT  Dummy_Handler
            IMPORT  HardFault_Handler
__Vectors 
                                      ;ARM core vectors
            DCD    __initial_sp       ;00:end of stack
            DCD    Reset_Handler      ;01:reset vector
            DCD    Dummy_Handler      ;02:NMI
            DCD    HardFault_Handler  ;03:hard fault
            DCD    Dummy_Handler      ;04:(reserved)
            DCD    Dummy_Handler      ;05:(reserved)
            DCD    Dummy_Handler      ;06:(reserved)
            DCD    Dummy_Handler      ;07:(reserved)
            DCD    Dummy_Handler      ;08:(reserved)
            DCD    Dummy_Handler      ;09:(reserved)
            DCD    Dummy_Handler      ;10:(reserved)
            DCD    Dummy_Handler      ;11:SVCall (supervisor call)
            DCD    Dummy_Handler      ;12:(reserved)
            DCD    Dummy_Handler      ;13:(reserved)
            DCD    Dummy_Handler      ;14:PendSV (PendableSrvReq)
                                      ;   pendable request 
                                      ;   for system service)
            DCD    Dummy_Handler      ;15:SysTick (system tick timer)
            DCD    Dummy_Handler      ;16:DMA channel 0 transfer 
                                      ;   complete/error
            DCD    Dummy_Handler      ;17:DMA channel 1 transfer
                                      ;   complete/error
            DCD    Dummy_Handler      ;18:DMA channel 2 transfer
                                      ;   complete/error
            DCD    Dummy_Handler      ;19:DMA channel 3 transfer
                                      ;   complete/error
            DCD    Dummy_Handler      ;20:(reserved)
            DCD    Dummy_Handler      ;21:FTFA command complete/
                                      ;   read collision
            DCD    Dummy_Handler      ;22:low-voltage detect;
                                      ;   low-voltage warning
            DCD    Dummy_Handler      ;23:low leakage wakeup
            DCD    Dummy_Handler      ;24:I2C0
            DCD    Dummy_Handler      ;25:(reserved)
            DCD    Dummy_Handler      ;26:SPI0
            DCD    Dummy_Handler      ;27:(reserved)
            DCD    Dummy_Handler      ;28:UART0 (status; error)
            DCD    Dummy_Handler      ;29:(reserved)
            DCD    Dummy_Handler      ;30:(reserved)
            DCD    Dummy_Handler      ;31:ADC0
            DCD    Dummy_Handler      ;32:CMP0
            DCD    Dummy_Handler      ;33:TPM0
            DCD    Dummy_Handler      ;34:TPM1
            DCD    Dummy_Handler      ;35:(reserved)
            DCD    Dummy_Handler      ;36:RTC (alarm)
            DCD    Dummy_Handler      ;37:RTC (seconds)
            DCD    Dummy_Handler      ;38:PIT
            DCD    Dummy_Handler      ;39:(reserved)
            DCD    Dummy_Handler      ;40:(reserved)
            DCD    Dummy_Handler      ;41:DAC0
            DCD    Dummy_Handler      ;42:TSI0
            DCD    Dummy_Handler      ;43:MCG
            DCD    Dummy_Handler      ;44:LPTMR0
            DCD    Dummy_Handler      ;45:(reserved)
            DCD    Dummy_Handler      ;46:PORTA
            DCD    Dummy_Handler      ;47:PORTB
__Vectors_End
__Vectors_Size  EQU     __Vectors_End - __Vectors
                ALIGN
;****************************************************************
;Constants
                AREA    MyConst,DATA,READONLY
;>>>>> begin constants here <<<<<

FirstPromptP         DCB	    " Enter first 96-bit hex number: 0x",NULL				
SecondPromptP         DCB	    "Enter 96-bit hex number to add: 0x",NULL				
PromptInvalid   DCB	    "      Invalid Number--try again: 0x",NULL				
SumString       DCB 	"                            Sum: 0x",NULL				
OverflowOutp  DCB 	"                            Sum: 0xOVERFLOW",NULL
				
						
;>>>>>   end constants here <<<<<
                ALIGN
;****************************************************************
;Variables
                AREA    MyData,DATA,READWRITE
;>>>>> begin variables here <<<<<

;--- queue structures 
QBuffer         SPACE    Q_BUF_SZ  ;queue contents 
QRecord         SPACE    Q_REC_SZ  ;queue management record
	
;--- from exercise 06
StringBuffer    SPACE    MAX_STRING
putUvar  SPACE    MAX_STRING
	
;--- for exercise 08 multiprecision arithmetic
;Multiprecision Arithemtic
Number1	        SPACE    NUM_SIZE
Number2	        SPACE    NUM_SIZE 
Result          SPACE    NUM_SIZE

;>>>>>   end variables here <<<<<
                ALIGN
                END

