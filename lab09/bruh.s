

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

 