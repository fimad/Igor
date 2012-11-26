#include <sys/types.h>
#include <distorm.h>

/* The various error codes that can be returned. */
enum Errors {
    Success = 0
  /* The type field of the location struct was invalid. */
  , InvalidLocationType 
  /* The evaluator does not yet know how to interpret the given instruction. */
  , UnsupportedInstruction 
};

/* 
 * The state struct represents the internal state of an x86 machine.
 *
 * The values of the state struct should not be directly modified, instead they
 * should be queried using the provided methods at the bottom of this file.
 */
struct State {
  size_t modifiedLocations;
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
    LMemory
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
   * The value of extra depends on the type of the location. It is unused for
   * registers and the 32bit memory location of location for memory types.
   */
  uint32_t extra; 
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
  struct Location location;
  /* How many references are there to this expression? */
  int32_t references;
  /* What is the type of this expression? */
  uint8_t type;
  /* The interpretation of the 'value' union depends on the value of type. */
  union {
    uint32_t constantInt; /* The value of a constant integer expression. */
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

