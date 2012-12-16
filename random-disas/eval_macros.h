#ifndef _EVAL_MACROS_H_
#define _EVAL_MACROS_H_

#include "eval.h"

/* Location creation macros. */
#define INVALID_LOCATION      ((struct Location){LInvalid, 0})
#define REGISTER_LOCATION(r)  ((struct Location){(r), 0})
#define VARIABLE_LOCATION(id)  ((struct Location){LVariable, (id)})

/* Location operations. */
#define IS_VALID(x)           ((x).type != LInvalid)
#define LOCATION_EQ(x,y)      ((x).type == (y).type && (x).extra == (y).extra)

#endif

