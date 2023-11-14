            TTL Program Title for Listing Header Goes Here
;****************************************************************
;Descriptive comment header goes here.
; Polled Serial I/O
;Name:  Emilio Garcia Rabell 
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
;---------------------------------------------------------------
;Program
;Linker requires Reset_Handler
            AREA    MyCode,CODE,READONLY
            ENTRY
            EXPORT  Reset_Handler
            IMPORT  Startup
			;EXPORT PutChar
	
			
Reset_Handler  PROC  {}
main
;---------------------------------------------------------------
;Mask interrupts
            CPSID   I
;KL05 system startup with 48-MHz system clock
            BL      Startup
;---------------------------------------------------------------
;>>>>> begin main program code <<<<<
    BL      Init_UART0_IRQ


;2. Initialize the (byte) Boolean stopwatch run variable RunStopWatch to zero (0).
;3. Initialize the (word) stopwatch clock count variable Count to zero (0).
    BL      Init_PIT_IRQ ;4. Use Init_PIT_IRQ from prelab work to initialize the KL05 PIT to generate an interrupt from channel zero every 0.01s.


;5. Print the character string shown below to the terminal.
;Press key for stopwatch command (C,D,H,P,T)
    
Restart  
	LDR     R0,=MainPrompt          ;load prompt constant
	MOVS    R1,#MAX_STRING
	BL      PutStringSB         ;print prompt

    BL      NewLine
    MOVS    R0,'>'
    BL      PutChar

MainPrompt  
	BL      GetChar             ;get input     
	BL      PutChar             ;show input

	;---Dynamic input 

	CMP R0, #96 ;Compare r0 to 97
	BHI HandleConditions ; if the number is higher than 96, then the value is lower case

	ADDS R0, R0, #32 ;if the value is uppercase then conver it to lowercase, just add 32

HandleConditions
	;-- check  input character 
	CMP     R0,#'c'             ;dequeue if entered D
	BEQ     handleC
	CMP     R0,#'d'             ;enqueue if entered E
	BEQ     handleD
	CMP     R0,#'h'             ;display help if entered H
	BEQ     handleH
	CMP     R0,#'p'             ;print queue contents if entered P
	BEQ     handleP
	CMP     R0,#'t'             ;print queue status if entered S
	BEQ     handleT
	BL      NewLine          ;add newline
	B       Restart          ;repeat prompt if invalid

handleC
    ;Clear—set the stopwatch clock count variable Count to zero (0)
    LDR     R0,=Count
    LDR     R0,[R0,#0] ; get count value
    MOVS    R1,#0
    STR     R1,[R0,#0] ; Clear the count
    B       Restart

handleD
    ;Display—print the current value of the stopwatch clock count variable: >d: 2023 x 0.01 s
    MOVS    R2,R0 ; Hold the character before showing it

    MOVS    R0,#'>'
    BL      PutChar

    MOVS    R0,R2
    BL      PutChar

    MOVS    R0,#':'
    BL      PutChar

    LDR     R0,=Spaces
    BL      PutStringSB

    LDR     R0,=Count
    BL      PutnumUB

    LDR     R0,=PrintDCase
    BL      PutStringSB



handleH
    ;C(lear),D(isplay),H(elp),P(ause),T(ime)
    LDR     R0,HelpPrompt
    BL      PutStringSB
    B       Restart
handleP
    ;Pause—clear the Boolean stopwatch variable RunStopWatch to zero
    LDR     R0,=RunStopWatch
    LDRB     R0,[R0,#0] ; get count value
    MOVS    R1,#0
    STRB     R1,[R0,#0] ; Clear RunStopwatch
    B       Restart
handleT
    ;Time—set the Boolean stopwatch variable RunStopWatch to one 
    LDR     R0,=RunStopWatch
    LDR     R0,[R0,#0] ; get count value
    MOVS    R1,#1
    STR     R1,[R0,#0] ; set  RunStopwatch
    B       Restart
;>>>>>   end main program code <<<<<
;Stay here
            B       .
            ENDP
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
; GetStringSB subroutine gets a string from user 
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
; PutStringSB subroutine displays a string to the terminal
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
            BNE     ReadChar                  ;loop
            
EndPutStringSB
            POP     {R0-R2,PC}                ;for nested subroutine
            ENDP




;-------------------------------------------- 
; NewLine subroutine
;
; show character with a carriage return
; and move the cursor to the next line
;--------------------------------------------

NewLine  PROC    {R0-R14}
            PUSH    {R0,LR}                    ;for nested subroutine
           
            MOVS    R0,#CR
            BL      PutChar
            MOVS    R0,#LF
            BL      PutChar
 
            POP     {R0,PC}                    ;for nested subroutine
            ENDP



 
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

Enqueue     PROC    {R0-R14}
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
		


; Initialize PIT and install PIT_ISR


; Initialize PIT IRQ

Init_PIT_IRQ   PROC {R0-R14}
    PUSH  {R0-R7,LR}
    ; Enable clock for PIT module
    LDR R0,=SIM_SCGC6
    LDR R1,=SIM_SCGC6_PIT_MASK
    LDR R2,[R0,#0]
    ORRS R2,R2,R1
    STR R2,[R0,#0]

    ; Disable PIT timer 0
    LDR R0,=PIT_CH0_BASE
    LDR R1,=PIT_TCTRL_TEN_MASK
    LDR R2,[R0,#PIT_TCTRL_OFFSET]
    BICS R2,R2,R1
    STR R2,[R0,#PIT_TCTRL_OFFSET]

    ; Set PIT interrupt priority
    LDR R0,=NVIC_IPR
    LDR R1,=NVIC_IPR_PIT_MASK
    LDR R3,[R0,#0]
    BICS R3,R3,R1
    STR R3,[R0,#0]

    ; Clear any pending PIT interrupts
    LDR R0,=NVIC_ICPR
    LDR R1,=NVIC_ICPR_PIT_MASK
    STR R1,[R0,#0]

    ; Unmask PIT interrupts
    LDR R0,=NVIC_ISER
    LDR R1,=NVIC_ISER_PIT_MASK
    STR R1,[R0,#0]

    ; Enable PIT module
    LDR R0,=PIT_BASE
    LDR R1,=PIT_MCR_EN_FRZ
    STR R1,[R0,#PIT_MCR_OFFSET]

    ; Set PIT timer 0 period for 0.01 s
    LDR R0,=PIT_CH0_BASE
    LDR R1,=PIT_LDVAL_10ms
    STR R1,[R0,#PIT_LDVAL_OFFSET]

    ; Enable PIT timer 0 interrupt
    LDR R1,=PIT_TCTRL_CH_IE
    STR R1,[R0,#PIT_TCTRL_OFFSET]

    POP {R0-R7,PC}
    ENDP

; PIT ISR

PIT_ISR   PROC {R0-R14}
    PUSH {R0-R7,LR}
    CPSID I        ; masks interrupts 
    LDR  R1,=RunStopWatch        
    LDR  R1,[R1,#0] ; Get the value of the stopwatch
    
    CMP  R1,#0        ; Check if it is not 0
    BEQ  ISREnd
    
    LDR  R2,=Count
    LDR  R3,[R2,#0]        
    ADDS R3,R3,#1    ; Increment the count
    STR  R3,[R2,#0]
    
ISREnd    
    CPSIE I        ; unmasks interrupts
    POP {R0-R7,PC}
    ENDP

; Vector table "install"
DCD    PIT_ISR      ; 38: PIT
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
MainPrompt	DCB		"Press key for stopwatch command (C,D,H,P,T)",0;
SuccessPrompt DCB 	"Success: ",0
FailurePrompt DCB 	"Failure: ",0
EnqueuePrompt DCB 	"Character to enqueue: ",0
HelpPrompt DCB "D (dequeue), E (enqueue), H (help), P (print), S (status)",0
StatusPrompt DCB "Status: ",0
PrintInHex DCB "In=0x",0
PrintOutHex DCB "Out=0x",0
PrintNum DCB "Num=",0
PrintDCase DCB  " x 0.01 S"
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