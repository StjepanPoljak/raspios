/* ******* GPIO address space ******* */
.equ GPIOBASE,		0x3f200000

.equ GPFSEL1,		0x04
.equ GPPUD,		0x94
.equ GPPUDCLK0,		0x98

.equ TMRBASE,		0x3f003000
.equ IRQBASE,		0x3f00B200

/* ******* UART address space ******* */
.equ UARTBASE,		0x3f215000

.equ AUX_ENABLE,	0x004
/* mUART, SPI1, SPI2 enable / disable (RW)
 * ----------------------------------
 * 31:3		reserved
 * 2		SPI 2 enable
 * 1		SPI 1 enable
 * 0		mUART enable */

.equ AUX_MU_IO,		0x040
/* mUART data (RW)
 * ---------------------------------
 * 31:8		reserved
 * 7:0		data */

.equ AUX_MU_IER,	0x044
/* mUART IRQ enable (RW)
 * ---------------------------------
 * 31:8		reserved
 * 7:6		reserved
 * 5		low power mode (16750 only)
 * 4		sleep mode (16750 only)
 * 3		modem status register change
 * 2		receiver line status change
 * 1		transmitter holding register empty
 * 0		received data available
 *
 * Note: bits 3:2 are required to receive IRQs */

.equ AUX_MU_IIR,	0x048
/* mUART IRQ identification register
 * (RO except 3:2)
 * ----------------------------------
 * 31:8		reserved
 * 7:6		00 - No FIFO
 *		11 - FIFO enabled
 * 5:4		read as zero
 * 3:1	READ:
 *		000 - no IRQs
 *		001 - transmit holds empty
 *		010 - receiver holds valid byte
 *		011 - line status change
 *		110 - character timeout (16550 only)
 * 3:2
 *	WRITE:
 *		bit 1 set - clear receive FIFO
 *		bit 2 set - clear transmit FIFO
 *
 * 0		0 - interrupt pending */

.equ AUX_MU_LCR,	0x04C
/* mUART line control (RW)
 * ----------------------------------
 * 31:8		reserved
 * 7		DLAB access
 *		0 - RBR, THR, IER accessible
 *		1 - DLL and DLM accessible
 * 6		break signal (0 - disabled)
 * 5:3		xx0 - no parity
 *		001 - odd parity
 *		011 - even parity
 *		101 - high parity
 *		111 - low parity
 * 2		0 - 1 stop bit
 *		1 - 2 stop bits (or 1.5 if 5bit)
 * 0:1		00 - 5bit
 *		01 - 6bit
 *		10 - 7bit
 *		11 - 8bit */

.equ AUX_MU_MCR,	0x050
/* mUART modem control (RW)
 * ----------------------------------
 * 31:8		reserved
 * 7:6		reserved
 * 5		autoflow control (16750 only)
 * 4		loopback mode
 * 3:2		auxiliary output 1 & 2
 * 1		Request To Send
 * 0		reserved */

.equ AUX_MU_LSR,	0x054
/* mUART line status register (RO)
 * ----------------------------------
 * 31:8		reserved
 * 7		erroneous data in FIFO
 * 6		THR is empty, line is idle
 * 5		THR is empty
 * 4		break signal received
 * 3		framing error
 * 2		parity error
 * 1		overrun error
 * 0		data available
 *
 * Note: Here, only 1:0 and 6:5 are relevant. */

.equ AUX_MU_MSR,	0x058
/* mUART modem status register (RO)
 * ----------------------------------
 * 31:8		reserved
 * 7		carrier detect
 * 6		ring indicator
 * 5		data set ready
 * 4		clear to send (CTS)
 * 3		change in carrier detect
 * 2		trailing edge ring indicator
 * 1		change in data set ready
 * 0		change in clear to send
 *
 * Note: Here, only 4 is relevant */

.equ AUX_MU_SCRATCH,	0x05C
/* mUART scratch (RW)
 * 31:8		reserved
 * 7:0		scratch register (1 byte) */

.equ AUX_MU_CNTL,	0x060
/* mUART extra control (RW)
 * -----------------------------------
 * 31:2		set to zero if no extra control
 *		features are needed
 * 7		CTS assert level (1 - low, 0 - high)
 * 6		RTS assert level (1 - low, 0 - high)
 * 5:4		RTS auto flow level; de-assert TS when
 *		00 - FIFO has 3 empty spaces left
 *		01 - FIFO has 2 empty spaces left
 *		10 - FIFO has 1 empty space left
 *		11 - FIFO has 4 empty spaces left
 * 3		enable transmit auto flow-control using CTS
 *		1 - transmitter will stop if CTS is de-asserted
 *		0 - transmitter will ignore the status of CTS line
 * 2		enable receive auto flow-control using RTS
 *		1 - RTS line will de-assert if receive FIFO reaches
 *		    "auto flow" level (RTS behaves like RTR)
 *		0 - RTS line is controlled by bit 1 in MCR register
 * 1		enable receiver
 * 0		enable transmitter
 *
 * NOTE: disable Rx and Tx while setting up mUART */

.equ AUX_MU_STAT,	0x064
/* mUART extra status (RO)
 * -----------------------------------
 * 31:28	reserved
 * 27:24	transmit FIFO fill level
 * 23:20	reserved
 * 19:16	receive FIFO fill level
 * 15:10	reserved
 * 9		transmitter done (bit 2 && bit 8)
 * 8		transmit FIFO is empty
 * 7		CTS line
 * 6		RTS status
 * 5		transmit FIFO is full (!bit 1)
 * 4		receiver overrun (cleared on LSR read)
 * 3		transmitter is idle
 * 2		receiver is idle
 * 1		space available
 *		1 - tramitter FIFO can accept at
 *		    least one more symbol
 * 0		symbol available
 *		1 - receive FIFO contains at least
 *		    one symbol */

.equ AUX_MU_BAUD,	0x068
/* mUART baudrate (RW)
 * -----------------------------------
 * 31:16	reserved
 * 15:0		baudrate (270 = 115200) */

.globl uart_init
uart_init:
	sub	sp, sp, #(16*2)
	stp	x19, x20, [sp]
	stp	x21, x30, [sp, #16]

	ldr	x20, =UARTBASE
	ldr	x21, =GPIOBASE

	/* mUART enable */
	ldr	w19, [x20, AUX_ENABLE]

	orr	w19, w19, #1
	str	w19, [x20, AUX_ENABLE]

	/* disable Rx and Tx */
	mov	w19, #0
	str	w19, [x20, AUX_MU_CNTL]

	/* set 8bit */
	mov	w19, #0x3
	str	w19, [x20, AUX_MU_LCR]

	/* set RTS always high */
	mov	w19, #0
	str	w19, [x20, AUX_MU_MCR]

	/* disable interrupts */
	mov	w19, #0xf
	str	w19, [x20, AUX_MU_IER]

	/* clear FIFO */
	mov	w19, #0x06
	str	w19, [x20, AUX_MU_IIR]

	/* set baudrate */
	mov	w19, #270
	str	w19, [x20, AUX_MU_BAUD]

	/* set ALT0 for GPIO 14 (TX) and 15 (RX) */
	ldr	w3, [x21, GPFSEL1]

	mov	w19, #0x3F
	lsl	w19, w19, #12
	bic	w3, w3, w19

	mov	w19, #0x202
	lsl	w19, w19, #12
	orr	w3, w3, w19

	str	w19, [x21, GPFSEL1]

	/* disable pull up / down */
	mov	w19, #0
	str	w19, [x21, GPPUD]

	/* wait 150 cycles */
	bl gppud_cycle

	/* assert clock on GPIO 14 and 15 */
	mov	w19, #3
	lsl	w19, w19, #14
	str	w19, [x21, GPPUDCLK0]

	/* wait 150 cycles */
	bl gppud_cycle

	/* refresh GPPUDCLK0 */
	mov	w19, #0
	str	w19, [x21, GPPUDCLK0]

	/* enable RX and TX */
	mov	w19, #3
	str	w19, [x20, AUX_MU_CNTL]

	ldp	x19, x20, [sp]
	ldp	x21, x30, [sp, #16]
	add	sp, sp, #(16*2)

	ret

/* wait (at least) 150 cycles */
gppud_cycle:
	sub	sp, sp, #0x10
	stp	x19, x30, [sp]

	mov	w19, #150
gppud_cycle_loop:
	sub	w19, w19, #1
	cbnz	w19, gppud_cycle_loop

	ldp	x19, x30, [sp]
	add	sp, sp, #0x10

	ret

.globl uart_write_char
/* uart_write_char
 * --------------------
 * w0 - char to write */
uart_write_char:
	sub	sp, sp, #(16*2)
	stp	x19, x20, [sp, #16]
	str	x30, [sp, #8]

uart_write_char_loop:
	ldr	x19, =UARTBASE
	ldr	w20, [x19, AUX_MU_LSR]
	and	w20, w20, #0x20
	cbz	w20, uart_write_char_loop

	str	w0, [x19, AUX_MU_IO]

	ldr	x30, [sp, #8]
	ldp	x19, x20, [sp, #16]
	add	sp, sp, #(16*2)

	ret

