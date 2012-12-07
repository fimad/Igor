#ifndef _EVAL_H_
#define _EVAL_H_

#include <sys/types.h>
#include <distorm.h>

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

/* The various error codes that can be returned. */
enum Errors {
    Success = 0
  /* The type field of the location struct was invalid. */
  , InvalidLocationType 
  /* The evaluator does not yet know how to interpret the given instruction. */
  , UnsupportedInstruction 
  /* The evaluator does not yet know how to interpret the given operand. */
  , UnsupportedOperand 
  /* 
   * We ran into a problem allocating memory. But really though, this should
   * never happen.
   */
  , OutOfMemory
};

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
 * Locations are an abstraction that encompasses both registers and memory
 * locations. Each corresponds to a unique location, and each location has a
 * corresponding value for any given state. The value of a location will be an
 * Expression which is a combination of initial location values.
 */

enum LocationType {
    LInvalid
  , LMemory
  , LMemoryExpression /* The location in memory is the result of an expression. */
  , LRegEAX
  , LRegEBX
  , LRegECX
  , LRegEDX
  , LRegESP
  , LRegEBP
  , LRegEDI
  , LRegESI
};

struct Location {
  uint8_t type; /* What is the type of the location? */
  /* 
   * The value of the union depends on the type of the location. It is unused
   * for registers and the 32bit memory location of location for memory types.
   */
  union{
    /* The address of an LMemory location. */
    uint32_t address; 
    /* The address of an LMemoryExpression location.*/
    struct Expression *expression;
    /* 
     * Used for comparisons when the semantics of the union is unimportant, and
     * only it's bitwise equality matters.
     */
    uint64_t extra;
  };
};


/*
 * Expressions are the abstract representations of values in this evaluator.
 * The represent the combination of one or more constants and initial location
 * values.
 */

enum ExpressionType {
    EConstantInt
  , ELocation
  , EPlus
  , EMinus
};

struct Expression {
  /* What location does this expression correspond to? */
  struct Location source;
  /* How many references are there to this expression? */
  int32_t references;
  /* What is the type of this expression? */
  uint8_t type;
  /* The interpretation of the 'value' union depends on the value of type. */
  union {
    int64_t constantInt; /* The value of a constant integer expression. */
    struct Location location; /* The value of an initial location expression. */
    /* The children expressions for binary expressions. */
    struct {
      struct Expression *left;
      struct Expression *right;
    };
  } value;
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
int valueOf(struct State*, struct Location, struct Expression*);

/* Returns a list of modified locations. */
void modifiedLocations(struct State*);

#endif

