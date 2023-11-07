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


When initializing the UART0 to generate interrupt requests you would begin the same similar to initializing the UART0 polling by enabling via setting the clock control register and then configure the UART0 to have a baud rate of 9600 by dealing with the UART0_BDH and UART0_BDL registers. You will then set the fifth bit UART0_C2_TIE to 1 enabling the transmit interrupt this must be set to one because it generates an interrupt request to send the next character when the transmit buffer is empty. Next, the 7th bit UART0_C2_RIE is also set to one to enable the recieve interrupt that allows a new interrupt request to be generated when new received data is available in the receive buffer. The status registers UART0_S1 and UART0_S2 clear any flags and status bits, this must be done for UART to handle interrupts.

3A.
Subroutines are specifically invoked via the main program, the programmer specifies where in the program it should be executed. They need a call instruction (BL) to be executed. 

Unlike a subroutine, ISR is asynchronously invoked by a response to an external action event directly form the hardware. 

Seperate from an ISR, an exeception handler is also invoked by a response but this response is to a pre-determined exceptional condition. When code tells the computer to do an illegal operation such as (1/0).

3B.
Subroutine: Invoked at specified points where the programmer calls it in the program. Only executes when the computer encounters a call instruction of that subroutine.

ISR: Invoked when an external  event interrupts the flow of the program. Not executed until computer acknowledges interrupt request signal.

Exception Handler: Invoked when computer hits a pre-determined exceptional situation while executing instructions. Only executes when  an exceptional condition is detected by the computer.

3C.
Subroutine: Computer saves the return address registers (LR) to make sure it can return to the program execution after the subroutine is called and executed. After the subroutine is executed, the computer restores registers saved with their designated data and thr program finishes executing from the point of the return address.

ISR & Exceptions: The computer will save the data at the PC register to load new data intop that is specified for the ISR/Exception, and executes that ISR/Exception. The PC register data is then restored and the execution of the program is resumed from the point where it was interrupted.



