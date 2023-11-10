            TTL CMPE 250 Exercise 09
;****************************************************************
;Exercise 09 - Serial I/O Driver
;The objective of this exercise is to implement interrupt-based serial
;communication with the KL05 UART using circular FIFO queues for receiving
;and transmitting serial data
;Name: Emilio Garcia Rabell
;Class: CMPE-250
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

;-- queue management record field offsets
IN_PTR      EQU   0
OUT_PTR     EQU   4
BUF_STRT    EQU   8
BUF_PAST    EQU   12
BUF_SIZE    EQU   16
NUM_ENQD    EQU   17

;--- queue structure sizes
Q_BUF_SZ    EQU   4   ;queue contents
Q_REC_SZ    EQU   18  ;queue management record
X_BUF_SZ    EQU   80  ;for transmit and receive queue

;--- from exercise 06
MAX_STRING    EQU  79
CR          EQU  0x0D
LF          EQU  0x0A
NULL        EQU  0x00
BS          EQU  0x08

;--- for C flag
C_MASK              EQU     0x20
C_SHIFT             EQU     24

;--- for PutNumHex subroutine
RIGHT_NIBBLE_MASK   EQU  0x0F

;--- for PutNumUB subroutine
PutNumUB_MASK       EQU  0xFF

;---------------------------------------------------------------
;NVIC_ICER
;31-00:CLRENA=masks for HW IRQ sources;
;             read:   0 = unmasked;   1 = masked
;             write:  0 = no effect;  1 = mask
;12:UART0 IRQ mask
NVIC_ICER_UART0_MASK  EQU  UART0_IRQ_MASK
;---------------------------------------------------------------
;NVIC_ICPR
;31-00:CLRPEND=pending status for HW IRQ sources;
;             read:   0 = not pending;  1 = pending
;             write:  0 = no effect;
;                     1 = change status to not pending
;12:UART0 IRQ pending status
NVIC_ICPR_UART0_MASK  EQU  UART0_IRQ_MASK
;---------------------------------------------------------------
;NVIC_IPR0-NVIC_IPR7
;2-bit priority:  00 = highest; 11 = lowest
UART0_IRQ_PRIORITY    EQU  3
NVIC_IPR_UART0_MASK   EQU (3 << UART0_PRI_POS)
NVIC_IPR_UART0_PRI_3  EQU (UART0_IRQ_PRIORITY << UART0_PRI_POS)
;---------------------------------------------------------------
;NVIC_ISER
;31-00:SETENA=masks for HW IRQ sources;
;             read:   0 = masked;     1 = unmasked
;             write:  0 = no effect;  1 = unmask
;12:UART0 IRQ mask
NVIC_ISER_UART0_MASK  EQU  UART0_IRQ_MASK
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
UART0_C2_T_R    EQU  (UART0_C2_TE_MASK :OR: UART0_C2_RE_MASK)
UART0_C2_T_RI   EQU  (UART0_C2_RIE_MASK :OR: UART0_C2_T_R)
UART0_C2_TI_RI  EQU  (UART0_C2_TIE_MASK :OR: UART0_C2_T_RI)
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
            
Reset_Handler  PROC  {}
main
;---------------------------------------------------------------
;Mask interrupts
            CPSID   I
;KL05 system startup with 48-MHz system clock
            BL      Startup
;---------------------------------------------------------------

;>>>>> begin main program code <<<<<
        BL      Init_UART0_IRQ      ;initialize UART0 

        LDR     R0,=QBuffer         ;load queue buffer
        LDR     R1,=QRecord         ;load queue record
        MOVS    R2,#Q_BUF_SZ        ;load queue buffer size
        BL      InitQueue           ;initialize queue structure

        EchoPrompt  LDR     R0,=Prompt          ;load prompt constant
        MOVS    R1,#MAX_STRING
        BL      PutStringSB         ;print prompt

        MainPrompt  BL      GetChar             ;get input     
        BL      PutChar             ;echo input

        ;---------- convert input char to uppercase
        CMP     R0,#'a'
        BLO     ValidInput
        CMP     R0,#'z'
        BHI     ValidInput
        SUBS    R0,R0,#'a'          ;remove ASCII of lowercase letter
        ADDS    R0,R0,#'A'          ;add ASCII of uppercase letter

        ;---------- check if input character is valid
        ValidInput  CMP     R0,#'D'             ;dequeue if entered D
        BEQ     command_D
        CMP     R0,#'E'             ;enqueue if entered E
        BEQ     command_E
        CMP     R0,#'H'             ;display help if entered H
        BEQ     command_H
        CMP     R0,#'P'             ;print queue contents if entered P
        BEQ     command_P
        CMP     R0,#'S'             ;print queue status if entered S
        BEQ     command_S
        BL      MoveCursor          ;add newline
        B       EchoPrompt          ;repeat prompt if invalid


        ;--- perform dequeue operation
        command_D   BL      MoveCursor

        ;---------- load input parameters
        LDR     R0,=QBuffer
        LDR     R1,=QRecord
        MOVS    R2,#Q_BUF_SZ

        BL      Dequeue             ;remove from queue
        BCS     DequeueError        ;dequeue failed if flag is set

        BL      PutChar             ;print retrieved value
        MOVS    R0,#":"
        BL      PutChar

        LDR     R0,=QRecord         ;load queue record
        BL      DequeueStatus       ;display status
        BL      MoveCursor          ;go to next line
        B       EchoPrompt          ;echo prompt again

        ;---------- echo dequeue failed and restart
        DequeueError
        LDR     R0,=Failure         ;load failure message
        BL      PutStringSB         ;echo message
        LDR     R0,=QRecord         ;load queue record
        BL      QueueStatus         ;display status
        BL      MoveCursor          ;go to next line
        B       EchoPrompt          ;echo prompt again


        ;--- perform enqueue operation
        command_E   BL      MoveCursor          ;move to next line
        LDR     R0,=Prompt_E        ;load prompt
        BL      PutStringSB         ;echo prompt

        ;---------- load input params
        LDR     R0,=QBuffer
        LDR     R1,=QRecord
        MOVS    R2,#Q_BUF_SZ         

        BL      GetChar             ;get input char
        BL      PutChar             ;echo char
        BL      Enqueue             ;add to queue
        BCS     EnqueueError        ;enqueue failed if flag is set

        ;---------- print enqueue status in a new line
        BL      MoveCursor
        LDR     R0,=Success
        BL      PutStringSB

        ;---------- print queue record and repeat prompt
        LDR     R0,=QRecord
        BL      QueueStatus
        BL      MoveCursor
        B       EchoPrompt

        ;---------- echo enqueue failed and restart
        EnqueueError
        BL      MoveCursor
        LDR     R0,=Failure
        BL      PutStringSB
        LDR     R0,=QRecord
        BL      QueueStatus
        BL      MoveCursor
        B       EchoPrompt


        ;--- display help options
        command_H   BL      MoveCursor           ;go to next line
        LDR     R0,=Help             ;load help message
        BL      PutStringSB          ;display message
        BL      MoveCursor           ;go to next line
        B       EchoPrompt           ;echo prompt

        ;--- display queue status
        command_S   BL      MoveCursor
        LDR     R0,=Status
        BL      PutStringSB
        LDR     R0,=QRecord
        BL      QueueStatus
        BL      MoveCursor
        B       EchoPrompt

        ;--- print queued characters
        command_P   BL      MoveCursor
        PUSH    {R0-R4}
        MOVS    R0,#'>'
        BL      PutChar

        ;---------- load parameters
        LDR     R1,=QRecord
        LDR     R0,=QBuffer
        LDRB    R2,[R1,#NUM_ENQD]
        LDR     R3,[R1,#OUT_PTR]    

        PrintNext   CMP     R2,#0                 ;if end of buffer,
        BEQ     Quit                  ;then quit

        ;---------- print queue characters
        LDRB    R4,[R3,#0]
        PUSH    {R0}
        MOVS    R0,R4                 ;move char to R0 for printing
        BL      PutChar
        POP     {R0}

        SUBS    R2,R2,#1              ;decrement queue chars left to read
        ADDS    R3,R3,#1              ;increment OUT_PTR  

        LDR     R4,[R1,#BUF_PAST]
        CMP     R3,R4                 ;compare OUT_PTR and BUF_PAST
        BEQ     WrapQueue             ;wrap queue if OUT_PTR >= BUF_PAST
        B       PrintNext

        WrapQueue   LDR     R3,[R1,#BUF_STRT]
        B       PrintNext

        Quit        MOVS    R0,#'<'               ;move delimeter to R0
        BL      PutChar               ;print delimeter
        BL      MoveCursor            ;go to next line
        POP     {R0-R4}
        B       EchoPrompt

;-------------------------------------------
;>>>>>   end main program code <<<<<
;Stay here
            B       .
            ENDP
            LTORG                          ;fixed the LTORG error
            
;>>>>> begin subroutine code <<<<<


;-------------------------------------------- 
; UART_ISR subroutine to handle UART0 transmit and interrupts
;--------------------------------------------

UART0_ISR   PROC    
	CPSID   I           ;mask  interrupts
	PUSH    {LR}    
	LDR     R0,=UART0_BASE  ;load UART0_BASE
	
;---if(TxInterrupt)
	LDRB    R1,[R0,#UART0_C2_OFFSET]
	MOVS    R2,#0x80
	ANDS    R1,R1,R2
	CMP     R1,#0
	BNE     TxInterruptEnabled ; go to TxInterruptEnabled

;--If(Not TxInterrupt) { check for Rx Interrupt
CheckTxInterrupt
	LDRB    R1,[R0,#UART0_S1_OFFSET]
	MOVS    R2,#0x10
	ANDS    R1,R1,R2
	CMP     R1,#0
	BEQ     End
	
;---------- store received character in R0
	LDR     R0,=UART0_BASE
	LDRB    R3,[R0,#UART0_D_OFFSET]
	
;---------- enqueue stored character
	LDR     R1,=RxQRecord           ;load input param for queue
	MOVS    R0,R3
	BL      Enqueue
	BL      End
	
;---------- Tx Interrupt is enabled
TxInterruptEnabled
	LDRB    R1,[R0,#UART0_S1_OFFSET]
	MOVS    R2,#0x80
	ANDS    R1,R1,R2
	CMP     R1,#0
	BEQ     CheckTxInterrupt
	
;---------- dequeue character from Tx queue
	LDR     R1,=TxQRecord           ;load input param for queue
	MOVS    R2,#Q_BUF_SZ
	BL      Dequeue
	
;---------- if dequeue is unsuccessul, disable Tx interrupt
	BCS     DisableTxInterrupt
	
;---------- if dequeue is successful, write char to UART0_D
	LDR     R1,=UART0_BASE
	STRB    R0,[R1,#UART0_D_OFFSET] ;store transmit char in R0
	B       End

;---------- Tx Interrupt is disabled
DisableTxInterrupt
	MOVS    R1,#UART0_C2_T_RI
	STRB    R1,[R0,#UART0_C2_OFFSET]
	B       End
	
;---------- unmask interrupts and return
End         CPSIE   I
	POP     {PC}
	ENDP



;-------------------------------------------- 
; Init_UART0_IRQ subroutine
;
; Initializes KL05 for interrupt-based serial I/O
; with UART0 through Port A pins 1 and 2
; 
; Input : None
; Output: None
;--------------------------------------------

Init_UART0_IRQ  PROC  {R0-R14}
            PUSH {R0-R2,LR}

;---------- initialize Rx queue buffer
            LDR     R1,=RxQRecord
            LDR     R0,=RxQBuffer
            MOVS    R2,#Q_BUF_SZ
            BL      InitQueue

;---------- initialize Tx queue buffer
            LDR     R1,=TxQRecord
            LDR     R0,=TxQBuffer
            MOVS    R2,#Q_BUF_SZ
            BL      InitQueue

            LDR     R0,=SIM_SOPT2                           ;connect sources
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
            
;---------- set UART0_IRQ priority
            LDR     R0,=UART0_IPR
            LDR     R1,=NVIC_IPR_UART0_MASK
            LDR     R2,=NVIC_IPR_UART0_PRI_3
            LDR     R3,[R0,#0]
            BICS    R3,R3,R1
            ORRS    R3,R3,R2
            STR     R3,[R0,#0]

;---------- clear pending UART0 Interrupts
            LDR     R0,=NVIC_ICPR
            LDR     R1,=NVIC_ICPR_UART0_MASK
            STR     R1,[R0,#0]

;---------- unmask UART0 interrupts
            LDR     R0,=NVIC_ISER
            LDR     R1,=NVIC_ISER_UART0_MASK
            STR     R1,[R0,#0]

;---------- initialize UART0 for 8N1 format at 9600 baud rate
            LDR     R0,=UART0_BASE
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
            
            POP     {R0-R2,PC}
            ENDP 

 
                 
;-------------------------------------------- 
; GetChar subroutine for dequeueing characters
; from receive queue.
;
; Input : None
; Output: R0 - Character dequeued from receive queue 
;--------------------------------------------

GetChar     PROC    {R1-R14}
            PUSH    {R1,LR}
            LDR     R1,=RxQRecord

GetCharLoop CPSID   I                       ;mask interrupts
            BL      Dequeue                 ;perform dequeue operation
            CPSIE   I                       ;enable interrupts
            BCS     GetCharLoop
            
            POP     {R1,PC}
            ENDP



;-------------------------------------------- 
; PutChar subroutine for enqueueing characters
; to transmit queue.
;
; Input : R0 - Character to enqueue to transmit queue
; Output: None 
;--------------------------------------------

PutChar     PROC    {R0-R14}
            PUSH    {R0,R1,LR}
            LDR     R1,=TxQRecord

PutCharLoop CPSID   I                       ;mask interrupts
            BL      Enqueue                 ;perform enqueue operation
            CPSIE   I                       ;enable interrupts
            BCS     PutCharLoop
            
;---------- enable Tx interrupts
            LDR     R0,=UART0_BASE
            MOVS    R1,#UART0_C2_TI_RI
            STRB    R1,[R0,#UART0_C2_OFFSET]
            
            POP     {R0,R1,PC}
            ENDP



;-------------------------------------------- 
; InitQueue subroutine for initializing circular
; FIFO queue structure
; 
; Input : R0 - Memory location of queue buffer
;         R1 - Address to place Queue record structure
;         R2 - Size of queue structure
; Output: R1 - Queue record structure
;--------------------------------------------

InitQueue   PROC    {R0-R14}
            PUSH    {R0-R2}
            
            STR     R0,[R1,#IN_PTR]            ;store mem address of front of queue in buffer
            STR     R0,[R1,#OUT_PTR]           ;store mem address in OUT_PTR
            STR     R0,[R1,#BUF_STRT]          ;store mem address in BUF_START
            
            ADDS    R0,R0,R2                   ;last buffer slot
            STR     R0,[R1,#BUF_PAST]          ;store BUF_PAST in last buffer slot

            STRB    R2,[R1,#BUF_SIZE]          ;store BUF_SIZE with size in R2

            MOVS    R0,#0                     ;initialize NUM_ENQD to zero
            STRB    R0,[R1,#NUM_ENQD]         ;store in 6th slot of buffer
            
            POP     {R0-R2}
            BX      LR
            ENDP



;-------------------------------------------- 
; Dequeue subroutine
;
; Remove an element from the circular FIFO Queue
; 
; Input : R1 - Address of Queue record structure
; Output: R0 - Character that has been dequeued
;         R1 - Queue record structure
;         C  - 1 (failure) or 0 (success)
;--------------------------------------------

Dequeue     PROC    {R1-R14}
            PUSH    {R1-R4}
            
            LDRB    R3,[R1,#NUM_ENQD]          ;load enqueued number in R3
            CMP     R3,#0                      ;if 0, set PSR C flag
            BLE     DequeueFailed
             
            LDR     R4,[R1,#OUT_PTR]           ;load OUT_PTR in R4
            LDRB    R0,[R4,#0]                 ;place removed item in R0
            
            LDRB    R3,[R1,#NUM_ENQD]          ;load enqueued number
            SUBS    R3,R3,#1                   ;decrement num of enqueued element
            STRB    R3,[R1,#NUM_ENQD]          ;store element to buffer
            
            ADDS    R4,R4,#1                   ;increment OUT_PTR location
            
            LDR     R3,[R1,#BUF_PAST]
            CMP     R3,R4                      ;compare OUT_PTR to BUF_PAST
            BEQ     DequeueWrapBuffer          ;if OUT_PTR >= BUF_PAST, wrap the queue
            
            STR     R4,[R1,#OUT_PTR] 
            B       DequeueSuccess
          
;---------- wrap around the circular queue
DequeueWrapBuffer
            LDR     R3,[R1,#BUF_STRT]
            STR     R3,[R1,#OUT_PTR]

;---------- clear C flag because it successfully dequeued
DequeueSuccess
            MRS     R1,APSR
            MOVS    R3,#C_MASK
            LSLS    R1,R1,#C_SHIFT
            BICS    R1,R1,R3
            MSR     APSR,R1
            B       DequeueEnd

;---------- set C flag because it failed to dequeue
DequeueFailed
            MRS     R1,APSR
            MOVS    R3,#C_MASK  
            LSLS    R3,R3,#C_SHIFT 
            ORRS    R1,R1,R3
            MSR     APSR,R1
            B       DequeueEnd

;---------- end Dequeue subroutine
DequeueEnd  POP     {R1-R4}
            BX      LR
            ENDP
                
                

;-------------------------------------------- 
; Enqueue subroutine
;
; Put an element into the circular FIFO Queue
; 
; Input : R0 - Character to enqueue
;         R1 - Address of the queue record structure
; Output: R1 - Queue record structure
;         C  - 1 (failure) or 0 (success)
;--------------------------------------------

Enqueue     PROC    {R0-R14}
            PUSH    {R1-R4}
            
            LDRB    R3,[R1,#NUM_ENQD]          ;load enqueued num          
            LDRB    R4,[R1,#BUF_SIZE]          ;load buffer size
            CMP     R3,R4                      ;compare enqueued num with size
            BGE     EnqueueFailed              ;if NUM_ENQD >= BUF_SIZE, queue is full
            
            LDR     R3,[R1,#IN_PTR]            ;load mem address of in_ptr
            STRB    R0,[R3,#0]                 ;store the item to be enqueued in mem address

;---------- increment IN_PTR
            ADDS    R3,R3,#1
            STR     R3,[R1,#IN_PTR]
            
;---------- increment enqueued elements number
            LDRB    R3,[R1,#NUM_ENQD]
            ADDS    R3,R3,#1
            STRB    R3,[R1,#NUM_ENQD]
            
;---------- if IN_PTR >= BUF_PAST, wrap around buffer
            LDR     R3,[R1,#IN_PTR]
            LDR     R4,[R1,#BUF_PAST]
            CMP     R3,R4
            BGE     EnqueueWrapBuffer
            B       EnqueueSuccess

;---------- wrap around the circular queue
EnqueueWrapBuffer
            LDR     R2,[R1,#BUF_STRT]
            STR     R2,[R1,#IN_PTR]            ;store IN_PTR to the front of queue
            B       EnqueueSuccess
            
;---------- clear C flag because it successfully enqueued
EnqueueSuccess
            MRS     R2,APSR
            MOVS    R3,#C_MASK
            LSLS    R2,R2,#C_SHIFT
            BICS    R2,R2,R3
            MSR     APSR,R2
            B       EnqueueEnd

;---------- set C flag because it failed to enqueue
EnqueueFailed
            MRS     R1,APSR
            MOVS    R3,#C_MASK
            LSLS    R3,R3,#C_SHIFT
            ORRS    R1,R1,R3
            MSR     APSR,R1
            B       EnqueueEnd

;---------- end Enqueue subroutine
EnqueueEnd  POP     {R1-R4}
            BX      LR
            ENDP



;-------------------------------------------- 
; QueueStatus subroutine
;
; Prints inpointer, outpointer, and number of elements in queue
; 
; Input : R0 - Address of queue record
; Output: None
;--------------------------------------------

QueueStatus PROC    {R1-R14}
            PUSH    {R0-R3,LR}

            LDR     R1,[R0,#IN_PTR]
            LDR     R2,[R0,#OUT_PTR]
            LDRB    R3,[R0,#NUM_ENQD]

;---------- print "   In="
            LDR     R0,=Spaces
            BL      PutStringSB
            LDR     R0,=In
            BL      PutStringSB

;---------- print IN_PTR address of queue
            MOVS    R0,R1
            BL      PutNumHex

;---------- print "   Out="
            LDR     R0,=Spaces
            BL      PutStringSB
            LDR     R0,=Out
            BL      PutStringSB

;---------- print OUT_PTR address of queue
            MOVS    R0, R2
            BL      PutNumHex

;---------- print "   Num="
            LDR     R0,=Spaces
            BL      PutStringSB
            LDR     R0,=Num
            BL      PutStringSB

;---------- print queue length
            MOVS    R0,R3
            BL      PutNumU

            POP     {R0-R3,PC}
            ENDP



;-------------------------------------------- 
; DequeueStatus subroutine
;
; Prints inpointer, outpointer, and number of elements in queue
; with extra spaces after "In: "
;
; Input : R0 - Address of queue record
; Output: None
;--------------------------------------------

DequeueStatus  PROC    {R1-R14}
            PUSH    {R0-R3,LR}

            LDR     R1,[R0,#IN_PTR]
            LDR     R2,[R0,#OUT_PTR]
            LDRB    R3,[R0,#NUM_ENQD]

;---------- print "   In="
            LDR     R0,=Spaces_DQ
            BL      PutStringSB
            LDR     R0,=In
            BL      PutStringSB

;---------- print IN_PTR address of queue
            MOVS    R0,R1
            BL      PutNumHex

;---------- print "   Out="
            LDR     R0,=Spaces
            BL      PutStringSB
            LDR     R0,=Out
            BL      PutStringSB

;---------- print OUT_PTR address of queue
            MOVS    R0, R2
            BL      PutNumHex

;---------- print "   Num="
            LDR     R0,=Spaces
            BL      PutStringSB
            LDR     R0,=Num
            BL      PutStringSB

;---------- print queue length
            MOVS    R0,R3
            BL      PutNumU

            POP     {R0-R3,PC}
            ENDP



;-------------------------------------------- 
; PutNumHex subroutine
;
; Prints hexadecimal representation of value to terminal
; 
; Input : R0 - Number to print in hexadecimal
; Output: None
;--------------------------------------------

PutNumHex   PROC    {R0-R14}
            PUSH    {R2-R4,LR}
            MOVS    R2,#32
            
;---------- loop for printing each digit stored in a register
PutNumHexLoop
            CMP     R2,#0                      ;check if last value to print
            BLT     PutNumHexEnd               ;end if notthing to print
            
            MOVS    R3,R0                      ;move print value to R3
            MOVS    R4,#RIGHT_NIBBLE_MASK      ;move shift value to R4
            LSRS    R3,R2                      ;shift current nibble
            ANDS    R4,R4,R3                   ;to print to the right side of register
            
            CMP     R4,#10                     ;compare ASCII value to 10
            BGE     PrintLetter                ;if ASCII value >= 10, print letter
            ADDS    R4,#'0'                    ;else, add 0 to ASCII
            B       PrintHex                   ;and print Hexadecimal value

;---------- for ASCII values above 9, letter A to F
PrintLetter ADDS    R4,R4,#55                  ;convert to uppercase

;---------- for ASCII values below 10, digits 0 to 9
PrintHex    PUSH    {R0}                       ;keep input unchanged
            MOVS    R0,R4
            BL      PutChar
            POP     {R0}
            
            MOVS    R4,#0                     ;reset R4
            SUBS    R2,R2,#4                  ;decrement loop counter
            B       PutNumHexLoop             ;loop

;---------- end PutNumHex subroutine
PutNumHexEnd
            POP     {R2-R4,PC}
            ENDP



;-------------------------------------------- 
; PutNumUB subroutine
;
; Prints decimal representation of value to terminal
; 
; Input : R0 - Number to print in decimal
; Output: None
;--------------------------------------------

PutNumUB    PROC    {R0-R14}
            PUSH    {R0}                      ;for register retention
            
            LDR     R0,[R0,#0]                ;load the value to print
            MOVS    R1,#PutNumUB_MASK         ;move mask value to R1
            ANDS    R0,R0,R1                  ;for getting only the printable value
            BL      PutNumU                   ;print to terminal
            
            POP     {R0}
            ENDP



;-------------------------------------------- 
; GetStringSB subroutine
;
; Prevents overrun of the buffer capacity specified in R1, inputs a string from the
; terminal keyboard to memory starting at the address in R0 and adds null termination
; 
; Input : R0 - Address in string buffer, 
;         R1 - Bytes in string buffer
; Output: R0 - String buffer in memory
;--------------------------------------------

GetStringSB PROC    {R1-R14}
            PUSH    {R0-R3,LR}
            
            MOVS    R2,#0                     ;initalize string offset
            MOVS    R3,R0                     ;save input char      
            
Input       BL      GetChar                   ;get next char of string

            CMP     R0,#CR                    ;check for carrige return
            BEQ     EndGetStringSB            ;end

            CMP     R1,#1                     ;check if string ended
            BEQ     Input                                              

            CMP     R0,#BS                    ;check for backspace
            BEQ     Input_BS

            BL      PutChar                   ;echo to terminal
            STRB    R0,[R3,R2]                ;store input char in string array

            SUBS    R1,R1,#1                  ;decrement number of chars left to read

            ADDS    R2,R2,#1                  ;add string's offset index           
            B       Input                     ;loop

Input_BS    CMP     R2,#0
            BEQ     Input
            SUBS    R2,R2,#1                  ;decrease offset
            B       Input

EndGetStringSB
            MOVS    R0,#0                     ;null termination
            STRB    R0,[R3,R2]
            POP     {R0-R3,PC}                ;for nested subroutine
            ENDP



;-------------------------------------------- 
; PutStringSB subroutine
;
; Prevents overrun of the buffer capacity specified in R1, displays a null-terminated
; string to the terminal screen from memory starting at the address in R0.
; 
; Input : R0 - String buffer in memory
;         R1 - Bytes in string buffer
; Output: None
;--------------------------------------------

PutStringSB PROC    {R0-R14}
            PUSH    {R0-R2,LR}
            
            CMP     R1,#0                     ;if all characters have been processed
            BEQ     EndPutStringSB            ;end
            
            ADDS    R1,R1,R0
            MOVS    R2,R0                     ;save R0 to R2

ReadChar    LDRB    R0,[R2,#0]
            
            CMP     R0,#NULL                  ;if none
            BEQ     EndPutStringSB            ;end
            BL      PutChar                   ;echo to terminal
            
            ADDS    R2,R2,#1                  ;point to next value
            CMP     R2,R1
            BNE     ReadChar                  ;loop
            
EndPutStringSB
            POP     {R0-R2,PC}                ;for nested subroutine
            ENDP



;-------------------------------------------- 
; PutNumU subroutine
;
; Displays the text decimal representation to the terminal screen of the 
; unsigned word value in R0, using PutChar to output each decimal digit character
;
; Input : R0 - Number for output to terminal
; Output: None
;--------------------------------------------

PutNumU     PROC    {R0-R14}
            PUSH    {R0-R2,LR}                ;for nested subroutine
            MOVS    R2,#0                     ;initalize array offset
           
DIV10       CMP     R0,#10                    ;check if num < 10
            BLT     EndPutNumU
           
            MOVS    R1,R0                     ;dividend in R1
            MOVS    R0,#10                    ;divisor is 10
            BL      DIVU                      ;divide
           
            PUSH    {R0}
            LDR     R0,=StringReversal

            STRB    R1,[R0,R2]
            ADDS    R2,R2,#1
            POP     {R0}
            B       DIV10                     ;keep diving by 10 until it can't
           
EndPutNumU  ADDS    R0,R0,#'0'                ;convert to ascii
            BL      PutChar                   ;echo to terminal
            SUBS    R2,R2,#1                  ;decrement string array

PrintChar   LDR     R0,=StringReversal        ;array iteration
            CMP     R2,#0
            BLT     EndPutNum

            LDRB    R1,[R0,R2]
            MOVS    R0,R1

            ADDS    R0,R0,#'0'                ;convert to ascii
            BL      PutChar                   ;echo to terminal

            SUBS    R2,R2,#1
            B       PrintChar
           
EndPutNum   POP     {R0-R2,PC}                ;for nested subroutine
            ENDP




;--------------------------------------------
; R1 / R0 = R0 remainder R1
;
; Performs the unsigned integer division of the dividend in R1 by 
; the divisor in R0. When divided by 0, register R0 and R1 stays
; the same but C flag gets set.
;
; Input : R0, R1
; Output: R0, R1
;--------------------------------------------

DIVU        PROC    {R2-R14}     ;only R0 and R1 changes
            PUSH    {R2-R3}      ;push to stack temporarily
            CMP     R0,#0        ;if divided by 0
            BEQ     DIVISOR_0    ;branch to DIVISOR_0
            CMP     R1,#0        ;if division of 0          
            BEQ     DIVIDEND_0   ;branch to DIVIDEND_0
            MOVS    R2,R0        ;R2 <- dividend
            MOVS    R0,#0        ;R0 <- quotient

WHILE       CMP     R1,R2        ;if divisor <= dividend
            BLO     DIV_END      ;branch to DIV_END
            SUBS    R1,R1,R2     ;R1 <- dividend -= divisor
            ADDS    R0,R0,#1     ;R0 <- quotient += 1
            B       WHILE

DIVISOR_0   MRS     R2,APSR      ;clear flags
            MOVS    R3,#0x20     ;R3 <- mask
            LSLS    R3,R3,#24    ;R4 <- left shit to MSB
            ORRS    R2,R3        ;R2 <- R3 or R2
            MSR     APSR,R2      ;set C flag
            B       DONE         ;branch to DONE

DIVIDEND_0  MOVS    R0,#0        ;R0 <- quotient = 0
            MOVS    R1,#0        ;R1 <- remainder = 0
            B       DIV_END
            
DIV_END     MRS     R2,APSR      ;clear flags
            MOVS    R3,#0x20     ;R3 <- mask
            LSLS    R3,R3,#24    ;R3 <- left shift to MSB
            BICS    R2,R3        ;clear bits
            MSR     APSR,R2      ;set C flag

DONE        POP     {R2-R3}      ;remove from stack
            BX      LR           ;return from subroutine
            ENDP                 ;end process




;-------------------------------------------- 
; MoveCursor subroutine
;
; Echo character with a carriage return
; and move the cursor to the next line
;--------------------------------------------

MoveCursor  PROC    {R0-R14}
            PUSH    {R0,LR}                    ;for nested subroutine
           
            MOVS    R0,#CR
            BL      PutChar
            MOVS    R0,#LF
            BL      PutChar
 
            POP     {R0,PC}                    ;for nested subroutine
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
            DCD    UART0_ISR          ;28:UART0 (status; error)
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

Prompt          DCB      "Type a queue command (D, E, H, P, S): ",NULL
                ALIGN
Prompt_E        DCB      "Character to enqueue: ",NULL
                ALIGN
Success         DCB      "Success: ",NULL
                ALIGN
Failure         DCB      "Failure: ",NULL
                ALIGN
Help            DCB      "D (dequeue), E (enqueue), H (help}, P (print), S (status)",NULL
                ALIGN
Status          DCB      "Status: ",NULL
                ALIGN
In              DCB      "In=0x",NULL
                ALIGN
Out             DCB      "Out=0x",NULL
                ALIGN
Num             DCB      "Num= ",NULL
                ALIGN
Spaces          DCB      "   ",NULL
                ALIGN
Spaces_DQ       DCB      "          ",NULL
                ALIGN

;>>>>>   end constants here <<<<<
                ALIGN
;****************************************************************
;Variables
                AREA    MyData,DATA,READWRITE
;>>>>> begin variables here <<<<<

;--- queue structures
                ALIGN
QBuffer         SPACE    Q_BUF_SZ  ;queue contents 
QRecord         SPACE    Q_REC_SZ  ;queue management record
    
;--- from exercise 06
StringBuffer    SPACE    MAX_STRING
StringReversal  SPACE    MAX_STRING
        
;--- for exercise 09
                ALIGN
RxQBuffer       SPACE    X_BUF_SZ
RxQRecord       SPACE    Q_REC_SZ
                ALIGN
TxQBuffer       SPACE    X_BUF_SZ
TxQRecord       SPACE    Q_REC_SZ
                
;>>>>>   end variables here <<<<<
                ALIGN
                END