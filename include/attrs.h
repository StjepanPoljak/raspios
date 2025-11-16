#ifndef ATTRS_H
#define ATTRS_H
#define __boot __attribute__((section(".text.boot")))
#define __ptable_boot __attribute__((section(".ptables.boot")))
#define __text __attribute__((section(".text")))
#endif
