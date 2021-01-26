#include "cyclone/types.h"

// Some custom function to speed-up code
mp_int sub_big_nums(void *data, mp_int bnx, mp_int bny) {
    alloc_bignum(data, bn);
    BIGNUM_CALL(mp_sub(&bnx, &bny, &bignum_value(bn)));

    return bignum_value(bn);
}
