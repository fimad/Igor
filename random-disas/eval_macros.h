#ifndef _EVAL_MACROS_H_
#define _EVAL_MACROS_H_

#include "eval.h"

/* Various location related macros. */
#define IS_VALID(x)           ((x).type != LInvalid)
#define INVALID_LOCATION      ((struct Location){LInvalid, 0})
#define REGISTER_LOCATION(r)  ((struct Location){(r), 0})
#define LOCATION_EQ(x,y)      ((x).type == (y).type && (x).extra == (y).extra)

#endif

