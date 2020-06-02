#ifndef SIREUM_ZN_TYPE_H
#define SIREUM_ZN_TYPE_H

#include <inttypes.h>
#include <type.h>

typedef int8_t Z;

#define Z_C(n) INT8_C(n)

#define Z_Min Z_C(INT8_MIN)
#define Z_Max Z_C(INT8_MAX)

#define Z_F "%" PRId32 ""

#endif