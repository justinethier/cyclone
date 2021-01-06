#ifndef CYCLONE_CK_POLYFILL_H
#define CYCLONE_CK_POLYFILL_H

#include <stdbool.h>

bool
ck_pr_cas_ptr(void *target, void *old_value, void *new_value);

bool
ck_pr_cas_int(int *target, int old_value, int new_value);

bool
ck_pr_cas_8(uint8_t *target, uint8_t old_value, uint8_t new_value);

#endif                          /* CYCLONE_CK_POLYFILL_H */
