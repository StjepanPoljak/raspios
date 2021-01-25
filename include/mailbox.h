#ifndef MAILBOX_H
#define MAILBOX_H

#include <stdint.h>
#include <mem.h>

#define DEFAULT_MAILBOX_BASE 0xb880

#define DEFINE_MAILBOX_BUFFER(NAME, BUFFC) \
	volatile uint32_t __attribute__((aligned(16))) NAME[(BUFFC) + 6]

#define MAILBOX_STATUS_SUCCESS 0
#define MAILBOX_ERROR_BUFF_SIZE -1
#define MAILBOX_ERROR_INV_RESPONSE -2
#define MAILBOX_ERROR_OVERFLOW -3

void mbox_init(addr_t base);
int create_msg(uint16_t buffc, volatile uint32_t* msgv,
	       uint32_t tag_id, uint16_t valc, uint32_t* valv);
int mbox_write(volatile uint32_t* msg, uint8_t channel);
int mbox_read(volatile uint32_t* msg, uint8_t channel);
int mbox_comm(volatile uint32_t* msg, uint8_t channel);
int mbox_get(volatile uint32_t* msg, uint16_t valn, uint32_t* val);

#endif
