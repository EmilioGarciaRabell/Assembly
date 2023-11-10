;--------------------------------------------------------------------------
; Emilio Garcia Rabell HW 4
;--------------------------------------------------------------------------
1.
; Define constants for columns, rows, and word size
COLS EQU 32 
ROWS EQU 24 
WORD_SIZE EQU 4 

; Define memory space for matrices
MatrixF SPACE (ROWS * COLS * WORD_SIZE)
MatrixP SPACE (ROWS * COLS * WORD_SIZE)
MatrixQ SPACE (ROWS * COLS * WORD_SIZE)

	; Load addresses of matrices into registers
	LDR R0, =MatrixP       ; R0 = Address of MatrixP
	LDR R1, =MatrixQ       ; R1 = Address of MatrixQ
	LDR R2, =MatrixF       ; R2 = Address of MatrixF

	; Load values of Rows and Cols into registers
	LDR R3, =ROWS          ; R3 = Address of Rows
	LDR R4, =COLS          ; R4 = Address of Cols

	; Combine Rows and Cols into a single register
	LSLS R3, R3, #0x10     ; Move rows to upper 16 bits
	ORRS R3, R3, R4       ; Or in columns to lower 16 bits, store in R3

	; Call the MatrixSub subroutine
	BL MatrixSub

	; Check N flag in APSR (Negative flag)
	MRS R5, APSR
	BMI Failed            ; If N flag is set, jump to Failed

	; If subtraction was successful, continue with the code


	; Return from program
	BX LR

; Subroutine to handle failed subtraction
Failed
	BL Error

; Subroutine to perform matrix subtraction
MatrixSub PROC

	BX LR ; Return
ENDP

; Subroutine for handling error
Error PROC
	; Add error handling code here
	BX LR ; Return from error handling subroutine
ENDP

/*
2. Describe the details of initializing UART0 to generate interrupt requests, as used in lab. Be
sure to describe all control register bits that must be set and/or cleared to enable interrupts and
why each bit/field must have the value that you indicate. (Omit details of any UART0
initialization not specifically associated with enabling the interrupts used in lab.) 
 */

To enable UART0 interrupts for the lab, perform the following steps:

1. Enable clock control register.
2. Set UART0_BDH and UART0_BDL for a baud rate of 9600.
3. Set UART0_C2_TIE (5th bit) to enable transmit interrupt requests on empty buffer.
4. Set UART0_C2_RIE (7th bit) to enable receive interrupt requests on new data.
5. Clear flags in UART0_S1 and UART0_S2 status registers for proper interrupt handling.



;3. For each of the following aspects of execution, explain how the specified types of code differ. (A,B,C)

3A.

i. Subroutine Invocation:

Triggered by a BL (branch with link) instruction within the main program, allowing for explicit calls to the subroutine.
ii. ISR Invocation:

Asynchronously initiated by hardware or external events, interrupting the normal program flow.
iii. Exception Handler Invocation:

Responds to predefined exceptional conditions, such as attempting an illegal operation (e.g., division by zero).
3B.

i. Subroutine Conditions for Invocation:

Main program explicitly calls the subroutine using a BL instruction.
ii. ISR Conditions for Invocation:

External events or hardware interrupts trigger ISR execution.
iii. Exception Handler Conditions for Invocation:

Predefined exceptional conditions in the program's execution flow lead to the invocation of an exception handler.
3C.

i. Actions After Subroutine Invocation:

Save return address, execute subroutine, and then resume from the saved address.
ii. Actions After ISR and Exception Handler Invocation:

Save current program counter, execute ISR/exception, and then resume from the original program counter.

/*
4. Unlike the Cortex-M0+, some CPUs have a specific instruction to return from an interrupt,
(e.g., Atmel/Microchip AVR RETI, IBM Power RFI, Intel 80x86 IRET, Motorola 68xxx RTS,
Motorola/Freescale/NXP 68HCxxx RTI). However the Cortex-M0+, uses the same
instructions to return from an interrupt that are used to return from a subroutine, (i.e., BX LR
or POP {PC}). Since returning from an interrupt requires more actions than just changing PC,
how is the Cortex-M0+ able to distinguish between returning from a subroutine and returning
from an interrupt so that all of the necessary actions are performed?
 */
The Cortex-M0+ manages context saving and restoration through the Nested Vectored Interrupt Controller (NVIC).
The identical return instructions (BX LR or POP {PC}) can be used for both subroutines and interrupts since
the NVIC makes sure that the required steps are taken when returning from an interrupt.


;5. Write an assembly language macro (called COUNT) according to the specifications:

MACRO
    COUNT $Point,$Word,$Return
;----------------------------------------------------
; Description: Counts the number of nonzero elements in 
; an array of words
; 
; Inputs:
;   $Point  - Points to the array of words (Arbitrary low register)
;   $Word   - Number of array elements
;
; Outputs:
;   $Return - Number of non-zero elements
; Registers:
;   $Reg    - Temporary register for processing
;----------------------------------------------------

    ASRS $Reg,$Word,$Log2Divisor
    ; $Reg = $Word / (2^$Log2Divisor)
    MEND

PUSH {LR} ; Save the link register

    ; Initialize loop counter
    MOVS $Return, #0

    ; Loop array elements
    Loop
        ; Load the word from memory at pointer
        LDR $Reg, [$Point, #4]

        ; Check if the word is nonzero element
        CMP $Reg, #0
        BEQ Skip 

        ; Increment counter
        ADDS $Return, $Return, #1

    Skip
        ; Decrement number of words looped over
        SUBS $Word, $Word, #1

        ; Check if all words have been looped over
        BNE Loop ; Repeat loop if not

POP {LR} ; Restore the link register
    MEND




