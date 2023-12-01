            TTL Program Title for Listing Header Goes Here
;****************************************************************
;Descriptive comment header goes here.
;(What does the program do?)
;Name:  <Your name here>
;Date:  <Date completed here>
;Class:  CMPE-250
;Section:  <Your lab section, day, and time here>
;---------------------------------------------------------------
;Keil Template for KL05 Assembly with Keil C startup
;R. W. Melton
;November 3, 2020
;****************************************************************
;Assembler directives
            THUMB
            GBLL  MIXED_ASM_C
MIXED_ASM_C SETL  {TRUE}
            OPT   64  ;Turn on listing macro `ansions
;****************************************************************
;Include files
            GET  MKL05Z4.s
            OPT  1          ;Turn on listing
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
X_BUF_SZ    EQU   80  ;for transmit and receive queue

BS          EQU  0x08

C_MASK              EQU     0x20
C_SHIFT             EQU     24
	
; Queue delimiters for printed output
Q_BEGIN_CH  EQU   '>'
Q_END_CH    EQU   '<'
	
MAX_STRING 	EQU 79



;---------------------------------------------------------------
;DAC0
DAC0_BITS   EQU   12
DAC0_STEPS  EQU   4096
DAC0_0V     EQU   0x00
;---------------------------------------------------------------
;Servo
SERVO_POSITIONS  EQU  5
;---------------------------------------------------------------
PWM_FREQ          EQU  50
;TPM_SOURCE_FREQ  EQU  48000000
TPM_SOURCE_FREQ   EQU  47972352
TPM_SC_PS_VAL     EQU  4

;PWM_DUTY_5       EQU  (PWM_PERIOD / 20)  ;  5% duty cycle
;PWM_DUTY_10      EQU  (PWM_PERIOD / 10)  ; 10% duty cycle
PWM_PERIOD        EQU  60000
PWM_DUTY_10       EQU  6000
PWM_DUTY_5        EQU  3000
;---------------------------------------------------------------
;Number output characteristics

;****************************************************************
;MACROs
;****************************************************************



	
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




;---------------------------------------------------------------
;NVIC_ICER
;31-00:CLRENA=masks for HW IRQ sources;
;             read:   0 = unmasked;   1 = masked
;             write:  0 = no effect;  1 = mask
;22:PIT IRQ mask
;12:UART0 IRQ mask
NVIC_ICER_PIT_MASK    EQU  PIT_IRQ_MASK
NVIC_ICER_UART0_MASK  EQU  UART0_IRQ_MASK
;---------------------------------------------------------------
;NVIC_ICPR
;31-00:CLRPEND=pending status for HW IRQ sources;
;             read:   0 = not pending;  1 = pending
;             write:  0 = no effect;
;                     1 = change status to not pending
;22:PIT IRQ pending status
;12:UART0 IRQ pending status
NVIC_ICPR_PIT_MASK    EQU  PIT_IRQ_MASK
NVIC_ICPR_UART0_MASK  EQU  UART0_IRQ_MASK
;---------------------------------------------------------------
;NVIC_IPR0-NVIC_IPR7
;2-bit priority:  00 = highest; 11 = lowest
;--PIT--------------------
PIT_IRQ_PRIORITY    EQU  0
NVIC_IPR_PIT_MASK   EQU  (3 << PIT_PRI_POS)
NVIC_IPR_PIT_PRI_0  EQU  (PIT_IRQ_PRIORITY << PIT_PRI_POS)
;--UART0--------------------
UART0_IRQ_PRIORITY    EQU  3
NVIC_IPR_UART0_MASK   EQU (3 << UART0_PRI_POS)
NVIC_IPR_UART0_PRI_3  EQU (UART0_IRQ_PRIORITY << UART0_PRI_POS)
;---------------------------------------------------------------
;NVIC_ISER
;31-00:SETENA=masks for HW IRQ sources;
;             read:   0 = masked;     1 = unmasked
;             write:  0 = no effect;  1 = unmask
;22:PIT IRQ mask
;12:UART0 IRQ mask
NVIC_ISER_PIT_MASK    EQU  PIT_IRQ_MASK
NVIC_ISER_UART0_MASK  EQU  UART0_IRQ_MASK
;---------------------------------------------------------------
;PIT_LDVALn:  PIT load value register n
;31-00:TSV=timer start value (period in clock cycles - 1)
;Clock ticks for 0.01 s at ~24 MHz count rate
;0.01 s * ~24,000,000 Hz = ~240,000
;TSV = ~240,000 - 1
;Clock ticks for 0.01 s at 23,986,176 Hz count rate
;0.01 s * 23,986,176 Hz = 239,862
;TSV = 239,862 - 1
PIT_LDVAL_10ms  EQU  239861
;---------------------------------------------------------------
;PIT_MCR:  PIT module control register
;1-->    0:FRZ=freeze (continue'/stop in debug mode)
;0-->    1:MDIS=module disable (PIT section)
;               RTI timer not affected
;               must be enabled before any other PIT setup
PIT_MCR_EN_FRZ  EQU  PIT_MCR_FRZ_MASK
;---------------------------------------------------------------
;PIT_TCTRL:  timer control register
;0-->   2:CHN=chain mode (enable)
;1-->   1:TIE=timer interrupt enable
;1-->   0:TEN=timer enable
PIT_TCTRL_CH_IE  EQU  (PIT_TCTRL_TEN_MASK :OR: PIT_TCTRL_TIE_MASK)
;---------------------------------------------------------------
;PORTx_PCRn (Port x pin control register n [for pin n])
;___->10-08:Pin mux control (select 0 to 8)
;Use provided PORT_PCR_MUX_SELECT_2_MASK
;---------------------------------------------------------------

;---------------------------------------------------------------
;SIM_SCGC4
;1->10:UART0 clock gate control (enabled)
;Use provided SIM_SCGC4_UART0_MASK
;---------------------------------------------------------------
;SIM_SCGC5
;1->09:Port B clock gate control (enabled)
;Use provided SIM_SCGC5_PORTB_MASK
;---------------------------------------------------------------
;SIM_SCGC6
;1->23:PIT clock gate control (enabled)
;Use provided SIM_SCGC6_PIT_MASK
;---------------------------------------------------------------
;SIM_SOPT2
;01=27-26:UART0SRC=UART0 clock source select (MCGFLLCLK)
;---------------------------------------------------------------

;---------------------------------------------------------------
;SIM_SOPT5
; 0->   16:UART0 open drain enable (disabled)
; 0->   02:UART0 receive data select (UART0_RX)
;00->01-00:UART0 transmit data select source (UART0_TX)

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

;---------------------------------------------------------------
;UART0_BDL
;26->7-0:SBR[7:0] (UART0CLK / [9600 * (OSR + 1)])
;UART0CLK is MCGPLLCLK/2
;MCGPLLCLK is 96 MHz
;MCGPLLCLK/2 is 48 MHz
;SBR = 48 MHz / (9600 * 16) = 312.5 --> 312 = 0x138

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

;---------------------------------------------------------------
;UART0_C4
;    0-->  7:MAEN1=match address mode enable 1 (disabled)
;    0-->  6:MAEN2=match address mode enable 2 (disabled)
;    0-->  5:M10=10-bit mode select (not selected)
;01111-->4-0:OSR=over sampling ratio (16)
;               = 1 + OSR for 3 <= OSR <= 31
;               = 16 for 0 <= OSR <= 2 (invalid values)
;------------------------
;UART0_S1
;0-->7:TDRE=transmit data register empty flag; read-only
;0-->6:TC=transmission complete flag; read-only
;0-->5:RDRF=receive data register full flag; read-only
;1-->4:IDLE=idle line flag; write 1 to clear (clear)
;1-->3:OR=receiver overrun flag; write 1 to clear (clear)
;1-->2:NF=noise flag; write 1 to clear (clear)
;1-->1:FE=framing error flag; write 1 to clear (clear)
;1-->0:PF=parity error flag; write 1 to clear (clear)

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




;Program
;C source will contain main ()
;Only subroutines and ISRs in this assembly source
            AREA    MyCode,CODE,READONLY

			; Export subroutines

			EXPORT Init_UART0_IRQ
			EXPORT GetChar
			EXPORT PutChar
			EXPORT GetStringSB
			EXPORT PutStringSB
			EXPORT NewLine
			EXPORT InitQueue
			EXPORT UART0_IRQHandler
			EXPORT Dequeue
			EXPORT Enqueue
			AREA    MyCode,CODE,READONLY
			ALIGN
;>>>>> begin subroutine code <<<<<

;-------------------------------------------------
; Init_UART0_IRQ Initializes KL05 for 
; serial communication using UART0.
;-------------------------------------------------

Init_UART0_IRQ  PROC  {R0-R14}
        PUSH {R0-R2,LR}

        ; Initialize Rx and Tx queue buffers
        LDR     R1,=RxQRecord
        LDR     R0,=RxQBuffer
        MOVS    R2,#Q_BUF_SZ
        BL      InitQueue

        LDR     R1,=TxQRecord
        LDR     R0,=TxQBuffer
        MOVS    R2,#Q_BUF_SZ
        BL      InitQueue

        ; Configure UART0 pins and clock
        LDR     R0,=SIM_SOPT2
        LDR     R1,=SIM_SOPT2_UART0SRC_MCGFLLCLK
        LDR     R2,[R0,#0]
        BICS    R2,R2,R1
        ORRS    R2,R2,R1
        STR     R2,[R0,#0]

        LDR     R0,=SIM_SOPT5
        LDR     R1,= SIM_SOPT5_UART0_EXTERN_MASK_CLEAR
        LDR     R2,[R0,#0]
        BICS    R2,R2,R1
        STR     R2,[R0,#0]

        LDR     R0,=SIM_SCGC4
        LDR     R1,=SIM_SCGC4_UART0_MASK
        LDR     R2,[R0,#0]
        ORRS    R2,R2,R1
        STR     R2,[R0,#0]

        LDR     R0,=SIM_SCGC5
        LDR     R1,= SIM_SCGC5_PORTB_MASK 
        LDR     R2,[R0,#0]
        ORRS    R2,R2,R1
        STR     R2,[R0,#0]

        LDR     R0,=PORTB_PCR2
        LDR     R1,=PORT_PCR_SET_PTB2_UART0_RX
        STR     R1,[R0,#0]

        LDR     R0,=PORTB_PCR1
        LDR     R1,=PORT_PCR_SET_PTB1_UART0_TX
        STR     R1,[R0,#0]

        ; Load base address
        LDR     R0,=UART0_BASE

        ; Disable UART0
        MOVS    R1,#UART0_C2_T_R
        LDRB    R2,[R0,#UART0_C2_OFFSET]
        BICS    R2,R2,R1
        STRB    R2,[R0,#UART0_C2_OFFSET]

        ; Set UART0_IRQ priority
        LDR     R0,=UART0_IPR
        LDR     R1,=NVIC_IPR_UART0_MASK
        LDR     R2,=NVIC_IPR_UART0_PRI_3
        LDR     R3,[R0,#0]
        BICS    R3,R3,R1
        ORRS    R3,R3,R2
        STR     R3,[R0,#0]

        ; Clear pending UART0 Interrupts
        LDR     R0,=NVIC_ICPR
        LDR     R1,=NVIC_ICPR_UART0_MASK
        STR     R1,[R0,#0]

        ; Unmask UART0 interrupts
        LDR     R0,=NVIC_ISER
        LDR     R1,=NVIC_ISER_UART0_MASK
        STR     R1,[R0,#0]

        ; Initialize UART0 for 8N1 format at 9600 baud rate
        LDR     R0,=UART0_BASE
        MOVS    R1,#UART0_BDH_9600
        STRB    R1,[R0,#UART0_BDH_OFFSET]
        MOVS    R1,#UART0_BDL_9600
        STRB    R1,[R0,#UART0_BDL_OFFSET]

        ; Set UART0 char format and clear flags
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

        ; Enable UART0
        MOVS    R1,#UART0_C2_T_R
        STRB    R1,[R0,#UART0_C2_OFFSET]

        POP     {R0-R2,PC}
        ENDP

 
           
;-------------------------------------------- 
; GetChar subroutine for dequeueing characters
; from the receive queue.
;--------------------------------------------

GetChar     PROC  
        PUSH    {R1,LR}
        LDR     R1,=RxQRecord

GetCharLoop 
        CPSID   I                       ; Mask interrupts
        BL      Dequeue                 ; Perform dequeue operation
        CPSIE   I                       ; Enable interrupts
        BCS     GetCharLoop             ; Retry if dequeue was unsuccessful
        
        POP     {R1,PC}
        ENDP


;-------------------------------------------- 
; PutChar subroutine for enqueueing characters into the transmit queue
;--------------------------------------------

PutChar     PROC   
        PUSH    {R1-R3,LR}
        LDR     R1,=TxQRecord

PutCharLoop 
		CPSID   I                       ; Mask interrupts
        BL      Enqueue                 ; Perform enqueue operation
        CPSIE   I                       ; Enable interrupts
        BCS     PutCharLoop             ; Retry if enqueue was unsuccessful
        
; Enable Tx interrupts
        LDR     R3,=UART0_BASE
        MOVS    R2,#UART0_C2_TI_RI
        STRB    R2,[R3,#UART0_C2_OFFSET]
        
        POP     {R1-R3,PC}
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
  MOVS R3, #32

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



;---------------------------------------------------------------
;---------------------------------------------------------------

PutNumU     PROC    
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
                
EndPutNumU  ADDS    R0,R0,#'0'                 ;convert to ascii
        BL      PutChar                    ;echo to terminal
        SUBS    R2,R2,#1                   ;decrement string array

PrintChar   LDR     R0,=putUvar         ;array iteration
        CMP     R2,#0
        BLT     EndPutNum

        LDRB    R1,[R0,R2]
        MOVS    R0,R1

        ADDS    R0,R0,#'0'                 ;convert to ascii
        BL      PutChar                    ;echo to terminal

        SUBS    R2,R2,#1
        B       PrintChar
                
EndPutNum   
        POP     {R0,R1,R2,PC}              ;for nested subroutine
        ENDP




;--------------------------------------------
; Perfdorms integer division
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

            BL      PutChar                   ;show to terminal
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
            BL      PutChar                   ;show to terminal
            
            ADDS    R2,R2,#1                  ;point to next value
            CMP     R2,R1
            BEQ     EndPutStringSB                  ;loop
			B 		ReadChar
            
EndPutStringSB
            POP     {R0-R2,PC}                ;for nested subroutine
            ENDP




;-------------------------------------------- 
; NewLine subroutine
;
; show character with a carriage return
; and move the cursor to the next line
;--------------------------------------------

NewLine     PROC    {R0-R14}
            PUSH    {R0,LR}                    ;for nested subroutine
           
            MOVS    R0,#CR
            BL      PutChar
            MOVS    R0,#LF
            BL      PutChar
 
            POP     {R0,PC}                    ;for nested subroutine
            ENDP





;-------------------------------------------- 
; InitQueue subroutine for initializing FIFO queue 
;
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
; UART_ISR subroutine to handle UART0 transmit and interrupts
;--------------------------------------------
UART0_IRQHandler
UART0_ISR   PROC    
    CPSID   I               ; Mask interrupts
    PUSH    {LR}            ; Push link register onto the stack
    LDR     R0,=UART0_BASE  ; Load UART0_BASE address
    
    ; Check if TxInterrupt is enabled
    LDRB    R1,[R0,#UART0_C2_OFFSET]
    MOVS    R2,#0x80
    ANDS    R1,R1,R2
    CMP     R1,#0
    BNE     TxIntEnbld      ; Go to TxIntEnbld if TxInterrupt is enabled

CheckTxInterrupt
    ; Check for Rx Interrupt
    LDRB    R1,[R0,#UART0_S1_OFFSET]
    MOVS    R2,#0x10
    ANDS    R1,R1,R2
    CMP     R1,#0
    BEQ     End              ; Exit if no Rx Interrupt

    ; Store received character in R0
    LDR     R0,=UART0_BASE
    LDRB    R3,[R0,#UART0_D_OFFSET]
    
    ; Enqueue stored character
    LDR     R1,=RxQRecord    ; Load input param for queue
    MOVS    R0,R3
    BL      Enqueue
    BL      End
    
TxIntEnbld
    ; Check if Tx Interrupt is enabled
    LDRB    R1,[R0,#UART0_S1_OFFSET]
    MOVS    R2,#0x80
    ANDS    R1,R1,R2
    CMP     R1,#0
    BEQ     CheckTxInterrupt
    
    ; Dequeue character from Tx queue
    LDR     R1,=TxQRecord    ; Load input param for queue
    MOVS    R2,#Q_BUF_SZ
    BL      Dequeue
    
    ; If dequeue is unsuccessful, disable Tx interrupt
    BCS     TxIntDislbd
    
    ; If dequeue is successful, write char to UART0_D
    LDR     R1,=UART0_BASE
    STRB    R0,[R1,#UART0_D_OFFSET] ; Store transmit char in R0
    B       End

TxIntDislbd
    MOVS    R1,#UART0_C2_T_RI
    STRB    R1,[R0,#UART0_C2_OFFSET] ; Disable Tx interrupt
    B       End
    
End
    CPSIE   I               ; Unmask interrupts
    POP     {PC}            ; Pop program counter from the stack
    ENDP                    ; End procedure



;-------------------------------------------- 
; Dequeue subroutine
; Remove an element from the Queue
;--------------------------------------------

Dequeue     PROC    {R1-R14}
        PUSH    {R1-R4}
        
        LDRB    R3,[R1,#NUM_ENQD]      ; Load enqueued number
        CMP     R3,#0                  ; If 0, set PSR C flag
        BLE     DequeueFailure
        
        LDR     R4,[R1,#OUT_PTR]       ; Load OUT_PTR
        LDRB    R0,[R4,#0]             ; Place removed item in R0
        
        LDRB    R3,[R1,#NUM_ENQD]      ; Load enqueued number
        SUBS    R3,R3,#1               ; Decrement num of enqueued elements
        STRB    R3,[R1,#NUM_ENQD]      ; Store updated count
        
        ADDS    R4,R4,#1               ; Increment OUT_PTR location
        
        LDR     R3,[R1,#BUF_PAST]
        CMP     R3,R4                  ; Compare OUT_PTR to BUF_PAST
        BEQ     DequeueWrpB      ; If OUT_PTR >= BUF_PAST, wrap the queue
        
        STR     R4,[R1,#OUT_PTR] 
        B       DequeueSucessfull
        
; Wrap around the circular queue
DequeueWrpB
        LDR     R3,[R1,#BUF_STRT]
        STR     R3,[R1,#OUT_PTR]

; Clear C flag because it successfully dequeued
DequeueSucessfull
        MRS     R1,APSR
        MOVS    R3,#C_MASK
        LSLS    R1,R1,#C_SHIFT
        BICS    R1,R1,R3
        MSR     APSR,R1
        B       DequeueEnd

; Set C flag because it failed to dequeue
DequeueFailure
        MRS     R1,APSR
        MOVS    R3,#C_MASK  
        LSLS    R3,R3,#C_SHIFT 
        ORRS    R1,R1,R3
        MSR     APSR,R1
        B       DequeueEnd

; End Dequeue subroutine
DequeueEnd  
        POP     {R1-R4}
        BX      LR
        ENDP        

;-------------------------------------------- 
; Enqueue subroutine
; 
; Description:
; Put an element into the  Queue
; 
;--------------------------------------------

Enqueue     PROC    
        PUSH    {R1-R4}
        
        LDRB    R3,[R1,#NUM_ENQD]      ; Load enqueued number
        LDRB    R4,[R1,#BUF_SIZE]      ; Load buffer size
        CMP     R3,R4                  ; Compare enqueued number with size
        BGE     EnqueueFailure          ; If NUM_ENQD >= BUF_SIZE, queue is full
        
        LDR     R3,[R1,#IN_PTR]        ; Load memory address of IN_PTR
        STRB    R0,[R3,#0]             ; Store the item to be enqueued in the memory address

; Increment IN_PTR
        ADDS    R3,R3,#1
        STR     R3,[R1,#IN_PTR]
        
; Increment enqueued elements number
        LDRB    R3,[R1,#NUM_ENQD]
        ADDS    R3,R3,#1
        STRB    R3,[R1,#NUM_ENQD]
        
; Wrap around buffer if IN_PTR >= BUF_PAST
        LDR     R3,[R1,#IN_PTR]
        LDR     R4,[R1,#BUF_PAST]
        CMP     R3,R4
        BGE     EnqueueWrpB
        B       EnqueueSuccessfull

; Wrap around the circular queue
EnqueueWrpB
        LDR     R2,[R1,#BUF_STRT]
        STR     R2,[R1,#IN_PTR]        ; Store IN_PTR to the front of the queue
        B       EnqueueSuccessfull
        
; Clear C flag because it successfully enqueued
EnqueueSuccessfull
		MRS     R2,APSR
        MOVS    R3,#C_MASK
        LSLS    R2,R2,#C_SHIFT
        BICS    R2,R2,R3
        MSR     APSR,R2


        B       EnqueueEnd

; Set C flag because it failed to enqueue
EnqueueFailure
        MRS     R1,APSR
        MOVS    R3,#C_MASK
        LSLS    R3,R3,#C_SHIFT
        ORRS    R1,R1,R3
        MSR     APSR,R1
        B       EnqueueEnd

; End Enqueue subroutine
EnqueueEnd  POP     {R1-R4}
        BX      LR
        ENDP
			

;>>>>>   end subroutine code <<<<<
            ALIGN
;**********************************************************************
;Constants
            AREA    MyConst,DATA,READONLY
;>>>>> begin constants here <<<<<
DAC0_table_0
DAC0_table
            DCW ((DAC0_STEPS - 1) / (SERVO_POSITIONS * 2))
            DCW (((DAC0_STEPS - 1) * 3) / (SERVO_POSITIONS * 2)) 
            DCW (((DAC0_STEPS - 1) * 5) / (SERVO_POSITIONS * 2)) 
            DCW (((DAC0_STEPS - 1) * 7) / (SERVO_POSITIONS * 2))
            DCW (((DAC0_STEPS - 1) * 9) / (SERVO_POSITIONS * 2))

PWM_duty_table
PWM_duty_table_0
            DCW        (PWM_DUTY_10 - 1)
            DCW        (((3 * (PWM_DUTY_10 - PWM_DUTY_5) / 4) + PWM_DUTY_5) - 1)
            DCW        ((((PWM_DUTY_10 - PWM_DUTY_5) / 2) + PWM_DUTY_5) - 1)
            DCW        ((((PWM_DUTY_10 - PWM_DUTY_5) / 4) + PWM_DUTY_5) - 1)
            DCW        (PWM_DUTY_5 - 1)

;>>>>>   end constants here <<<<<
;**********************************************************************
;>>>>> begin variables here <<<<<
;Variables

 AREA    MyData,DATA,READWRITE
QBuffer    SPACE   Q_BUF_SZ
QRecord    SPACE   Q_REC_SZ
;>>>>> begin variables here <<<<<

string 		SPACE MAX_STRING
putUvar 	SPACE 2
			ALIGN
Count		SPACE 8
			ALIGN
RunStopWatch SPACE 1
	
;--- queue structures
				ALIGN

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
            END