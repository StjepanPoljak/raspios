#include <mailbox.h>

#include <log.h>

#define MAILBOX_READ	(mailbox_base | 0x0)	/* will contain pointer to data buffer */
#define MAILBOX_STATUS	(mailbox_base | 0x18)
#define MAILBOX_WRITE	(mailbox_base | 0x20)	/* write here the address of data buffer */

#define MAILBOX_STATUS_FULL	((uint32_t)1 << 31)	/* poll this when writing */
#define MAILBOX_STATUS_EMPTY	((uint32_t)1 << 30)	/* poll this when reading */

#define MAILBOX_RESPONSE	((uint32_t)1 << 31)

#ifdef CONFIG_MBOX_TRACE
#define _mbox_trace(suffix, value) print ## suffix(value)
#define mbox_trace(suffix, value, level) log(suffix, value, level)
#else
#define _mbox_trace(suffix, value) ;;
#define mbox_trace(suffix, value, level) ;;
#endif

addr_t mailbox_base;

void mbox_init(addr_t base) {
	mailbox_base = base;

	mbox_trace(, "Mailbox base set as: ", LOG_INFO);
	_mbox_trace(64, mailbox_base);
	_mbox_trace(ln, "");

	return;
}

#define MBOX_END_TAG		0x0
#define MBOX_REQUEST_CODE	0x0
#define MBOX_PROCESS_REQUEST	0x0

int create_msg(uint16_t buffc, volatile uint32_t* msgv,
	       uint32_t tag_id, uint16_t valc, uint32_t* valv) {
	uint32_t buffb;
	uint16_t i;

	if (buffc < valc)
		return MAILBOX_ERROR_BUFF_SIZE;

	mbox_trace(ln, "Creating message...", LOG_INFO);

	buffb = buffc * sizeof(*msgv);

	msgv[0] = 6 * sizeof(*msgv) + buffb;
	msgv[1] = MBOX_PROCESS_REQUEST;
	msgv[2] = tag_id;
	msgv[3] = buffb;
	msgv[4] = MBOX_REQUEST_CODE;

	for (i = 0; i <= valc; i++)
		msgv[5 + i] = valc == i ? MBOX_END_TAG : valv[i];

	mbox_trace(ln, "Successfully created message.", LOG_INFO);

#ifdef CONFIG_MBOX_TRACE
	for (i = 0; i < msgv[0] / sizeof(uint32_t); i++) {
		_mbox_trace(, "  -> msg[");
		_mbox_trace(64, i);
		_mbox_trace(, "] = ");
		_mbox_trace(64, msgv[i]);
		_mbox_trace(ln, "");
	}
#endif

	return 0;
}

int mbox_write(volatile uint32_t* msg, uint8_t channel) {

	mbox_trace(, "Writing to mbox...", LOG_INFO);

	while((*(volatile uint32_t*)MAILBOX_STATUS)
	       & MAILBOX_STATUS_FULL) {
		asm __volatile__("nop");
		_mbox_trace(, ".");
	}

	_mbox_trace(ln, "");

	*((volatile addr_t*)MAILBOX_WRITE) = (addr_t)msg | channel;

	return 0;
}

int mbox_read(volatile uint32_t* msg, uint8_t channel) {
	volatile addr_t response;
#ifdef CONFIG_MBOX_TRACE
	uint16_t i;
#endif

	do {
		mbox_trace(, "Reading mbox...", LOG_INFO);

		while ((*(volatile uint32_t*)MAILBOX_STATUS)
			& MAILBOX_STATUS_EMPTY) {
			asm __volatile__("nop");
			_mbox_trace(, ".");
		}

		_mbox_trace(ln, "");

		response = (volatile addr_t)
			(*((volatile uint32_t*)MAILBOX_READ));

	} while (response != ((volatile addr_t)msg | channel));

	mbox_trace(ln, "Got response...", LOG_INFO);

#ifdef CONFIG_MBOX_TRACE
	for (i = 0; i < msg[0] / sizeof(uint32_t); i++) {
		_mbox_trace(, "  -> msg[");
		_mbox_trace(64, i);
		_mbox_trace(, "] = ");
		_mbox_trace(64, msg[i]);
		_mbox_trace(ln, "");
	}
#endif

	return msg[1] == MAILBOX_RESPONSE
		       ? MAILBOX_STATUS_SUCCESS
		       : MAILBOX_ERROR_INV_RESPONSE;
}

int mbox_comm(volatile uint32_t* msg, uint8_t channel) {
	int ret;

	/* if mmu enabled, virt_to_phys() is necessary */

	if ((ret = mbox_write(msg, channel)))
		return ret;

	if ((ret = mbox_read(msg, channel)))
		return ret;

	/* if mmu enabled, phys_to_virt() is necessary */

	return MAILBOX_STATUS_SUCCESS;
}

int mbox_get(volatile uint32_t* msg, uint16_t valn, uint32_t* val) {
	uint32_t valc;

	valc = (msg[4] & ~0x80000000) / sizeof(uint32_t);

	if (valn >= valc)
		return MAILBOX_ERROR_OVERFLOW;

	*val = msg[5 + valn];

	return MAILBOX_STATUS_SUCCESS;
}
