#ifndef _EVAL_H_
#define _EVAL_H_

#include <sys/types.h>
#include <distorm.h>

#include "expr.h"

/*
 * TODO: Currently, there is not an elegant way of noting that the modified
 * locations can be a function of the initial state. For instance, how should
 * the result of the instruction "mov [eax], 0" be represented?
 *
 * Also, because we are modifying a potentially arbitrary memory location, any
 * future memory reads may be overwritten by the instruction and would not
 * report their correct state. In order to fix this, it would be necessary to
 * stop evaluation prior to a another memory operation occurring if a variable
 * memory write has already been encountered.
 *
 * Another problem is that if there are logged location writes and a variable
 * write is encountered, then there is the potential for the variable write to
 * overwrite the value of a previously logged write. This would not be caught
 * though, and there would be two different values reported for the two
 * locations. In order to prevent this from happening, it would be necessary to
 * disallow variable memory writes if there have been any previous memory
 * writes, and to disallow any future writes if one has already occurred.
 *
 * TODO: Currently there is no good distinction between the portions of various
 * registers, e.g. EAX, AX, AH and AL. The current solution is to ignore all
 * registers besides the E* variants.
 *
 * TODO: Also, we need to add support for comparisons. This shouldn't actually
 * be that difficult. It should be possible to implement each bit of the
 * condition register as a unique Location and then using logical expressions
 * express it's state.
 *
 * TODO: Also, signed vs unsigned numbers???? Let's just wave our hands and make
 * everything signed at this point. If this ends up fucking everything up then
 * we can change it...
 */

/* 
 * The state struct represents the internal state of an x86 machine.
 *
 * The values of the state struct should not be directly modified, instead they
 * should be queried using the provided methods at the bottom of this file.
 */
struct State {
  size_t modifiedLocations; /* The number of actual entries in the locationValues array. */
  size_t allocatedLocations; /* The size of the locationValues array. */
  /* An array of location values (pointer to expressions). */
  struct Expression **locationValues;
};

/* 
 * Initializes the passed in state so that every location is mapped to it's
 * unique initial value.
 */
void newState(struct State*);

/*
 * Clears any modifications done to the state and free any memory that has been
 * allocated during evaluation.
 */
void clearState(struct State*);

/*
 * Attempts to evaluate an instruction given a specific state. If successful,
 * the return value is 0 and the state is updated accordingly. In the case of a
 * failure, the return value is an error code, and State is not changed.
 */
int eval(struct State*, _DInst*);

/* 
 * Examines the value of a location under a specific state. On a success the
 * return value is 0 and Expression is set accordingly. If the expression
 * corresponds to an initial state it will be set to NULL, if it's value has
 * been changed then it is set to the point to the instantiated instance of the
 * expression struct describing the value. On an error, the return value will be
 * the error code.
 *
 * Having unchanged locations have a NULL expression simplifies the memory
 * management of the system. Otherwise this method would need to either have a
 * static value that is filled in and returned (making this method
 * non-reentrant) or allocating a new struct for each location (it would be
 * unclear who owns the memory in this instance since there would be no record
 * of it's existing in the State).
 */
int valueOf(struct State*, struct Location, struct Expression**);

/* Returns a list of modified locations. */
void modifiedLocations(struct State*);

#endif

