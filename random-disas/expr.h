#ifndef _EXPR_H_
#define _EXPR_H_

#include <sys/types.h>
#include <distorm.h>

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
  
  /* Used for matching expressions with gadgets. */
  , LVariable
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
    /* The id of an LVariable location. */
    uint32_t id; 
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
 * Allocates memory for a new expression and fills it with sane defaults. The
 * returned expression has a reference set to 1, so it is unnecessary for the
 * caller to further increment reference.
 *
 * The passed in source parameter sets the source field of the expression. This
 * can be INVALID_LOCATION if the expression is guaranteed to not correspond to
 * a modified location.
 */
struct Expression* newExpression(struct Location source);

/* Increments the reference count for this expression. */
void claimExpression(struct Expression* expr);

/*
 * Frees the memory allocated for a given expression. This method will handle
 * freeing all sub expressions that have been allocated for this expression as
 * well.
 */
void freeExpression(struct Expression* expr);

#endif

