#ifndef SIREUM_ZN_TYPE_H
#define SIREUM_ZN_TYPE_H

#include <inttypes.h>
#include <type.h>

typedef int16_t Z;

#define Z_C(n) INT16_C(n)

#define Z_Min Z_C(INT16_MIN)
#define Z_Max Z_C(INT16_MAX)

#define Z_F "%" PRId16 ""

#endif