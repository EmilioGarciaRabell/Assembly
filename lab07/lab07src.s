            TTL Program Title for Listing Header Goes Here
;****************************************************************
;Descriptive comment header goes here.
; Polled Serial I/O
;Name:  Emilio Garcia Rabell webos xd
;Date:  -/-/-
;Class:  CMPE-250
;Section:  1
;---------------------------------------------------------------
;Keil Template for KL05
;R. W. Melton
;September 13, 2020
;****************************************************************
;Assembler directives
            THUMB
            OPT    64  ;Turn on listing macro expansions
;****************************************************************
;Include files
            GET  MKL05Z4.s     ;Included by start.s
            OPT  1   ;Turn on listing
;****************************************************************
;EQUates
;Characters
CR          EQU  0x0D
LF          EQU  0x0A
NULL        EQU  0x00
	
; Queue management record field offsets
IN_PTR      EQU   0
OUT_PTR     EQU   4
BUF_STRT    EQU   8
BUF_PAST    EQU   12
BUF_SIZE    EQU   16
NUM_ENQD    EQU   17
; Queue structure sizes
Q_BUF_SZ    EQU   4   ;Queue buffer contents
Q_REC_SZ    EQU   18  ;Queue management record
	

	
; Queue delimiters for printed output
Q_BEGIN_CH  EQU   '>'
Q_END_CH    EQU   '<'
	
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

MAX_STRING 				EQU 79
;---------------------------------------------------------------
;Program
;Linker requires Reset_Handler
            AREA    MyCode,CODE,READONLY
            ENTRY
            EXPORT  Reset_Handler
            IMPORT  Startup
			EXPORT PutChar
	
			
Reset_Handler  PROC  {}
main
;---------------------------------------------------------------
;Mask interrupts
            CPSID   I
;KL05 system startup with 48-MHz system clock
            BL      Startup
;---------------------------------------------------------------
;>>>>> begin main program code <<<<<


			BL Init_UART0_Polling  ; Initialize program
			;LDR R0, =string
			;MOVS R1, #MAX_STRING 	
			
		
			BL InitQueue ; Initialize the queue

Restart
			BL NewLine
			LDR 	R0,=PutPrompt
			BL		PutStringSB

IgnoreChar

			BL GetChar ; get a queue command

interpretChar


			;  dynamic input

			CMP R0, #96 ;Compare r0 to 97
			BHI HandleConditions ; if the number is higher than 96, then the value is lower case

			ADDS R0, R0, #32 ;if the value is uppercase then conver it to lowercase, just add 32

HandleConditions ; Verify which letter was entered
			CMP R0, #'d'
			BEQ handleD
			
			CMP R0, #'e'
			BEQ handleE
			
			CMP R0, #'h'
			BEQ handleH
			
			CMP R0, #'p'
			BEQ handleP
			
			CMP R0, #'s'
			BEQ handleS
			
			B IgnoreChar ; this executes whenever an invalid character is inputted

handleD ; Dequeue a character from the queue
	BL PutChar
	BL NewLine

	LDR R0,=QBuffer
	LDR R1,=QRecord
	MOVS R2,#Q_BUF_SZ

	BL Dequeue ; call the method to remove from queue

	; verify if the event was successful
	BCS UnsuccessD
	
	LDR R0,=SuccessPrompt ;Show the success message
	BL PutStringSB

	LDR R0,=QRecord ; load queue record
	BL StatusC ; show the status

	B Restart ; prompt for another command

UnsuccessD
	BL PutChar 
	LDR R0,=FailurePrompt
	BL PutStringSB

	LDR R0,=QRecord
	BL StatusC
	
	B Restart

handleE ;Enqueue a character to the queue, prompt to enter a character and then enqueue it
	BL PutChar
	BL NewLine
	
	LDR R0,=EnqueuePrompt
	BL PutStringSB ; prompt for a character 
	
	BL GetChar; get the character 
	
	BL NewLine
	BL Enqueue; enqueue the character
	
	BCS UnsuccessE ; Check if C flag is set
	
	LDR R0,=SuccessPrompt ;Show the success message
	BL PutStringSB
	B Restart

UnsuccessE
	BL PutChar
	
	LDR R0,=FailurePrompt
	BL PutStringSB
	
	B Restart
	
handleH ; Help: List the commands
	BL PutChar
	BL NewLine
	
	LDR R0,=HelpPrompt
	BL PutStringSB
	
	B Restart
	
handleP ;Print the queued characters from the queue buffer to the terminal /
		;screen, in order from first in to last in.
	BL PutChar
	BL NewLine
	
	B Restart
	
handleS ; Status: print the queue�s current InPointer, OutPointer, and NumberEnqueued
	BL PutChar
	BL NewLine
	
	LDR R0,=StatusPrompt
	BL PutStringSB
	; Put the address
	;Print 0x
	LDR R0,=QRecord ; Show the start of the hex num
	BL StatusC
	BL NewLine
	
	B Restart 

;>>>>>   end main program code <<<<<
;Stay here
            B       .
            ENDP
;>>>>> begin subroutine code <<<<<
;---------------------------------------------------------------
;---------------------------------------------------------------
StatusC PROC
	PUSH{R0-R3,LR}
       
	LDR     R1,[R0,#IN_PTR]
	LDR     R2,[R0,#OUT_PTR]
	LDRB    R3,[R0,#NUM_ENQD]

;---------- print "   In="
	LDR     R0,=Spaces
	BL      PutStringSB
	LDR     R0,=PrintInHex
	BL      PutStringSB

;---------- print IN_PTR address of queue
	MOVS    R0,R1
	BL      PutNumHex

;---------- print "   Out="
	LDR     R0,=Spaces
	BL      PutStringSB
	LDR     R0,=PrintOutHex
	BL      PutStringSB

;---------- print OUT_PTR address of queue
	MOVS    R0, R2
	BL      PutNumHex

;---------- print "   Num="
	LDR     R0,=Spaces
	BL      PutStringSB
	LDR     R0,=PrintNum
	BL      PutStringSB

;---------- print queue length
	MOVS    R0,R3
	BL      PutNumU
	

	POP     {R0-R3,PC}
	ENDP
	


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
	

;---------------------------------------------------------------
;---------------------------------------------------------------			
GetStringSB		PROC	{R0-R14}
	
			PUSH	{LR, R0, R1, R2, R3}	; Push LR to call subroutines, and registers 
			
			MOVS R2, R0 		; address of the first chartarcter in string
			MOVS R3, #0			; counter for next string
			
			CMP R1, #0			; check if R1 has space left
			BEQ	endGetString
			
			MOVS R0, #'<' ; Show the startof the input line
			
			BL PutChar 
			
Loop
			
			BL GetChar 
			
			; End cases
			CMP R0, #10
			BEQ endGetString 
			
			CMP R0, #13
			BEQ endGetString ; end the get string
			
			CMP R1, #1 ; check if there is space left 
			BEQ endGetString  ; restart
			
			STRB R0, [R2, R3] ; store 
			
			BL PutChar ; space left, put char 
			
			; Increment R3 and R1
			ADDS R3, R3, #1
			SUBS R1, R1, #1
			
			B Loop
			
endGetString
			
			MOVS R0, #'>' ; Show the end of the input line
			
			BL PutChar 
			
			MOVS R0, #NULL 
			BL NewLine 
					
			STRB R0, [R2, R3] 		
			
			POP		{PC, R0, R1, R2, R3}			
			BX    	LR                            
            ENDP

;---------------------------------------------------------------
;---------------------------------------------------------------


PutStringSB		PROC	{R0-R14}
			PUSH	{LR, R0-R7}
			
			MOVS R2, R0
			MOVS R3, #0
			
putLoop		LDRB R0, [R2, R3]
			CMP R0, #NULL
			BEQ endL
			
			BL PutChar
			ADDS R3, R3, #1
			CMP R3, R1
			BLE putLoop
			
			
endL		
			POP{PC, R0-R7}
			BX    	LR                
            ENDP


;---------------------------------------------------------------
;Use the queue
;management record structure in the class notes�including the field order and field offset
;names�so that the subroutines will work with any compatible circular FIFO queue. e
;subroutines must not use any temporary variables; the stack is the only area of memory
;that may be accessed other than the queue. Additionally, you must write the subroutines
;so that the no registers other than PSR and any output parameter(s) have changed values
;after return, (e.g., Dequeue uses R0 for an output parameter)
;---------------------------------------------------------------


;---------------------------------------------------------------
;Initializes the queue record structure at the address in R1 for the empty
;queue buffer at the address in R0 of size given in R2, (i.e., character capacity).
;---------------------------------------------------------------
InitQueue
	LDR R0, =QBuffer
	LDR R1, =QRecord
	STR R0, [R1, #IN_PTR]
	STR R0, [R1, #OUT_PTR]
	STR R0, [R1, #BUF_STRT]
	MOVS R2, #Q_BUF_SZ
	ADDS R0, R0, R2
	STR R0, [R1, #BUF_PAST]
	STRB R2, [R1, #BUF_SIZE] 
	MOVS R0, #0
	STRB R0, [R1, #NUM_ENQD]


;---------------------------------------------------------------
;Attempts to get a character from the queue whose record structure�s
;address is in R1: if the queue is not empty, dequeues a single character from the queue
;to R0, and returns with the PSR C bit cleared, (i.e., 0), to report dequeue success;
;otherwise, returns with the PSR C bit set, (i.e., 1), to report dequeue failure.
;---------------------------------------------------------------
Dequeue PROC
	
;int Dequeue (char *Character, qRecord *Queue) {
;/***************************************************************/
;/* Dequeues Character from queue if queue is not empty.        */
;/* If queue is empty, Character is not changed.                */
;/* Returns:  1 if failure; 0 otherwise                         */
;/***************************************************************/
  ;int Failure = TRUE; the subroutine will return C flag set for false and 0 for succesful
  ;if (Queue->NumberEnqueued) {
    ;*Character = *(Queue->OutPointer++);
    ;(Queue->NumberEnqueued)--;
    ;if (Queue->OutPointer >= Queue->BufferPast) {
     ;Queue->OutPointer = Queue->BufferStart;
    ;}
    ;Failure = FALSE;
  ;}
  ;return (Failure);}
  
  PUSH{R2-R7,LR}
    ; Check if it is empty
	LDRB R6,[R1,#NUM_ENQD] 
	
	CMP R6,#NULL
	BLE EndDeqUnSuccess
	
	LDR R2,[R1,#OUT_PTR]; Queue->OutPointer
	LDRB R3,[R2,#0]; Queue->BufferPast, place item into R0
	
	;*Character = *(Queue->OutPointer++);
    ;(Queue->NumberEnqueued)--;
	
	LDRB R0,[R2,#0]
	
	LDRB R4,[R1,#NUM_ENQD]
	SUBS R3, R3, #1; reduce number of elements in the queue
	STRB R3,[R1,#NUM_ENQD]
	ADDS R2,R2,#1 ; 

	LDR R3,[R1,#BUF_PAST]
	CMP R3,R2	;compare out pointer and buffer past
	BLO EndDeqUnSuccess
	
	
	LDR R3,[R1,#BUF_STRT] 
	STR R3,[R1,#OUT_PTR] ;Queue->OutPointer = Queue->BufferStart; 
	
	STR R2,[R1,#OUT_PTR]


	; Load new values
	;LDR R5,[R1,#OUT_PTR]; Queue->OutPointer
	
;	LDRB R6,[R1,#NUM_ENQD]; Queue->BufferPast
	
;	STR R2,[R1,R5] ; Update R1
;	STR R3,[R1,R6]
	
;	LDR R5,[R1,#BUF_STRT]
	


	BL EndDeqSuccess

;return C bit cleareed if succes set C otherwise
EndDeqUnSuccess
	;CLRC
	MRS R3, APSR
	MOVS R4, #0x20
	LSLS R4, R4, #24
	ORRS R3, R3, R4
	MSR APSR, R3
	B EndProgramDeq

EndDeqSuccess
		;clear c flag
	MRS R7,APSR
	MOVS R6,#0x20
	LSLS R6,R6,#24
	BICS R7,R7,R6
	MSR APSR,R7
EndProgramDeq
	STR R2,[R1,#OUT_PTR]
	POP{R2-R7, PC}
	ENDP  


;---------------------------------------------------------------
;Attempts to put a character in the queue whose queue record structure�s
;address is in R1: if the queue is not full, enqueues the single character from R0 to the
;queue, and returns with the PSR C bit cleared to report enqueue success; otherwise,
;returns with the PSR C bit set to report enqueue failure.
;---------------------------------------------------------------
Enqueue PROC
;/***************************************************************/
;/* Enqueues Character to queue if queue is not full.           */
;/* Returns:  1 if failure; 0 otherwise                         */
;/***************************************************************/

;int Enqueue (char Character, qRecord *Queue) {
  ;int Failure = TRUE;

  ;if (Queue->NumberEnqueued < Queue->BufferSize) {
    ;*(Queue->InPointer++) = Character;
    ;(Queue->NumberEnqueued)++;
    ;if (Queue->InPointer >= Queue->BufferPast) {
      ;Queue->InPointer = Queue->BufferStart;
    ;}
    ;Failure = FALSE;
  ;}
  ;return (Failure);}
 
  PUSH{R2-R7,LR}

	; Check if it is full
	LDRB R6,[R1,#NUM_ENQD] ; number enqued
	LDR R7,[R1,#BUF_SIZE] ; Bufer size
	
	CMP R6,R7
	BGE EndEnqUnSuccess ; if  NUM_ENQD >= BUF_SIZE the queue is full

	;if (Queue->InPointer >= Queue->BufferPast) {
	LDR R2,[R1,#IN_PTR]; Queue->InPointer
	STRB R0,[R2,#0] l; store address to enqueue 

	ADDS R2,R2,#1 ; increment IN_PTR
	STR R2,[R1,#IN_PTR]

	LDRB R3,[R1,#NUM_ENQD]; ; Increment the number of elements
	ADDS R3,R3,#1
	STRB R3,[R1#NUM_ENQD]
	
	LDR R3,[R1, #IN_PTR]
	LDR R4,[R1, #BUF_PAST]
	CMP R3,R4 ; if IN_ptr >= buf_past 
	BGE WrapCir
	B EndEnqSuccess
	
	
WrapCir
	LDR R2,[R1,#BUF_STRT]
	STR R2,[R1,#IN_PTR]
	B EndEnqSuccess
	

;return C bit cleareed if succes set C otherwise
EndEnqUnSuccess
	MRS R3, APSR
	MOVS R4, #0x20
	LSLS R4, R4, #24
	ORRS R3, R3, R4
	MSR APSR, R3
	B EndSubEnq

EndEnqSuccess
;clear c flag
	MRS R7,APSR
	MOVS R6,#0x20
	LSLS R6,R6,#24
	BICS R7,R7,R6
	MSR APSR,R7
EndSubEnq
	POP{R2-R7, PC}
	BX	LR
	ENDP  



;---------------------------------------------------------------
;Prints to the terminal screen the text hexadecimal representation of the
;unsigned word value in R0. (For example, if R0 contains 0x000012FF, then 000012FF
;should print on the terminal. Note: 12FF would not be acceptable. Do not use division
;to determine the hexadecimal digit values�use bit masks and shifts.)
;---------------------------------------------------------------
PutNumHex PROC
;void PutNumHex (uint32_t Number) {
;/***************************************************************/
;/* Prints hex representation of unsigned word (32-bit) number. */
;/* Uses PutNumHexB                                             */
;/***************************************************************/
 ; unsigned int ShiftAmount;
  
  ;for (ShiftAmount = 28; ShiftAmount > 0; ShiftAmount -= 4) {
   ; PutChar ((char) HEXN2ASCII((Number >> ShiftAmount) & 0x0F));
  ;}
  ;PutChar ((char) HEXN2ASCII(Number & 0x0F));
;}

;for (ShiftAmount = 28; ShiftAmount > 0; ShiftAmount -= 4) {

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

  CMP R2,#4
  BGE     ConvertToAF                ;if ASCII value >= 10,
  ADDS    R2,#'0'                    ;else, add 0 to ASCII
  B       PrintX


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



;/* Convert binary value of nibble to ASCII character */
;#define HEXN2ASCII(NIBBLE) ((NIBBLE) < 10 ? ((NIBBLE) + '0') : ((NIBBLE) - 10 + 'A'))
 ;MOVE THIS TO THE MACRO SECTION on top of the program

;end macro

;---------------------------------------------------------------
;Prints to the terminal screen the text decimal representation of the
;unsigned byte value in R0. (For example, if R0 contains 0x003021101, then 1 should
;print on the terminal. Note: 001 would also be acceptable, as the text decimal
;representation of 0x01.) (Hint: use a mask to preserve only the least byte of the word
;in R0, and call your PutNumU subroutine from Lab Exercise Six.)
;---------------------------------------------------------------
PutNumUB PROC
;/***************************************************************/
;/* Prints text representation of unsigned word (32-bit) in a   */
;/* minimum number of characters.                               */
;/* number.                                                     */
;/* Uses:  PutString                                            */
;/***************************************************************/
  ;/* String for number digits up to 4 billion */
  ;char String[MAX_WORD_DECIMAL_DIGITS + 1];
  ;char *StringPtr;

  ;StringPtr = &(String[MAX_WORD_DECIMAL_DIGITS]);
  ;*StringPtr = 0;
  
  ;do {
    ;/* next least significant digit is remainder of division by 10 */
    ;*(--StringPtr) = ((char) (Number % 10u)) + '0';
    ;/* rest of number to print */
    ;Number /= 10u; 
  ;} while (Number > 0);
  ;/* print text digits of number */
  ;PutStringSB (StringPtr, (MAX_WORD_DECIMAL_DIGITS + 1));
;} /* PutNumU */
	PUSH {R0-R5}
	MOVS R4,#0xFF
	ANDS R0,R0,R4
	MOVS R3,R5
	BL PutNumU
	
	
	POP {R0-R5,PC}
	ENDP

;---------------------------------------------------------------
;---------------------------------------------------------------


PutNumU		PROC {R0-R14}
			
			PUSH {R0-R7,LR}      
            LDR R3,=putUvar ; get a variable to store the value that divu outputs
			
            MOVS R4,#0
            STRB R4,[R3,R4]
			
                        
DIVFirstLoop
            MOVS R1,R0            
            MOVS R0,#10     
			
            BL DIVU
endLoop
            STRB R1,[R3,R4]       
            CMP R0,#0            
            BEQ ForNum ; Skip to the for loop
            ADDS R4,R4,#1       ; R5 will contain the ammount of characters
            B DIVFirstLoop        


ForNum
            CMP R4,#0        ; Check when R5 is 0
            BEQ lastSection        
            LDRB R0,[R3,R4]        
            ADDS R0,R0,#'0' ;add '0' or 0x30 to show the correct ascii value
            BL PutChar           
            SUBS R4,R4,#1   ;reduce  R5 
            B ForNum        
lastSection            
            LDRB R0,[R3,R4]
            ADDS R0,R0,#'0'
            BL PutChar    
            POP {R0-R7, PC}           
            ENDP

;---------------------------------------------------------------

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
				
				
				
				
Init_UART0_Polling		PROC     {R3-R15} 			; initialize UART0 polling 
	
			PUSH		{R0, R1, R2}  
		
;Select MCGFLLCLK as UART0 clock source
										
			LDR R0,=SIM_SOPT2
			LDR R1,=SIM_SOPT2_UART0SRC_MASK
			LDR R2,[R0,#0]
			BICS R2,R2,R1
			LDR R1,=SIM_SOPT2_UART0SRC_MCGFLLCLK
			ORRS R2,R2,R1
			STR R2,[R0,#0]
			
;Set UART0 for external connection

			LDR R0,=SIM_SOPT5
			LDR R1,=SIM_SOPT5_UART0_EXTERN_MASK_CLEAR
			LDR R2,[R0,#0]
			BICS R2,R2,R1
			STR R2,[R0,#0]
			
;Enable UART0 module clock

			LDR R0,=SIM_SCGC4
			LDR R1,=SIM_SCGC4_UART0_MASK
			LDR R2,[R0,#0]
			ORRS R2,R2,R1
			STR R2,[R0,#0]
;Enable PORT B module clock
			LDR R0,=SIM_SCGC5
			LDR R1,=SIM_SCGC5_PORTB_MASK
			LDR R2,[R0,#0]
			ORRS R2,R2,R1
			STR R2,[R0,#0]
			
;Select PORT B Pin 2 (D0) for UART0 RX (J8 Pin 01)

			LDR R0,=PORTB_PCR2
			LDR R1,=PORT_PCR_SET_PTB2_UART0_RX
			STR R1,[R0,#0]
			
; Select PORT B Pin 1 (D1) for UART0 TX (J8 Pin 02)
			
			LDR R0,=PORTB_PCR1
			LDR R1,=PORT_PCR_SET_PTB1_UART0_TX
			STR R1,[R0,#0]

;Disable UART0 receiver and transmitter

			LDR R0,=UART0_BASE
			MOVS R1,#UART0_C2_T_R
			LDRB R2,[R0,#UART0_C2_OFFSET]
			BICS R2,R2,R1
			STRB R2,[R0,#UART0_C2_OFFSET]

;Set UART0 for 9600 baud, 8N1 protocol

			MOVS R1,#UART0_BDH_9600
			STRB R1,[R0,#UART0_BDH_OFFSET]
			MOVS R1,#UART0_BDL_9600
			STRB R1,[R0,#UART0_BDL_OFFSET]
			MOVS R1,#UART0_C1_8N1
			STRB R1,[R0,#UART0_C1_OFFSET]
			MOVS R1,#UART0_C3_NO_TXINV
			STRB R1,[R0,#UART0_C3_OFFSET]
			MOVS R1,#UART0_C4_NO_MATCH_OSR_16
			STRB R1,[R0,#UART0_C4_OFFSET]
			MOVS R1,#UART0_C5_NO_DMA_SSR_SYNC
			STRB R1,[R0,#UART0_C5_OFFSET]
			MOVS R1,#UART0_S1_CLEAR_FLAGS
			STRB R1,[R0,#UART0_S1_OFFSET]
			MOVS R1, #UART0_S2_NO_RXINV_BRK10_NO_LBKDETECT_CLEAR_FLAGS  
			STRB R1,[R0,#UART0_S2_OFFSET] 
			
;Enable UART0 receiver and transmitter

			MOVS R1,#UART0_C2_T_R
			STRB R1,[R0,#UART0_C2_OFFSET] 
            
			POP		{R0, R1, R2}        
            
            BX    LR                
            
            ENDP			; end initialize UART0 polling subroutine 
				
;---------------------------------------------------------------
;---------------------------------------------------------------

NewLine		PROC 	{R0, R14}
			PUSH 	{R0-R7, LR} 
			
			MOVS 	R0,#CR
			BL		PutChar
			
			MOVS 	R0,#LF
			BL		PutChar
			
			POP 	{R0-R7, PC}
			BX		LR
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
PutPrompt	DCB		"Type queue command (D,E,H,P,S):",0;
SuccessPrompt DCB 	"Success: ",0
FailurePrompt DCB 	"Failure: ",0
EnqueuePrompt DCB 	"Character to enqueue: ",0
HelpPrompt DCB "D (dequeue), E (enqueue), H (help), P (print), S (status)",0
StatusPrompt DCB "Status: ",0
PrintInHex DCB "In=0x",0
PrintOutHex DCB "Out=0x",0
PrintNum DCB "Num=",0
Spaces          DCB      "   ",0
;>>>>>   end constants here <<<<<
            ALIGN
;****************************************************************
;Variables
            AREA    MyData,DATA,READWRITE
			;Queue structures
QBuffer    SPACE   Q_BUF_SZ
QRecord    SPACE   Q_REC_SZ
;>>>>> begin variables here <<<<<

string 		SPACE MAX_STRING
putUvar 	SPACE 2
;>>>>>   end variables here <<<<<
            ALIGN
            END