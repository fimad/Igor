#ifndef _ERRORS_H_
#define _ERRORS_H_

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
  /* The state passed in was invalid. Likely it had not been initialized. */
  , InvalidState 
  /* The candidate expression did not match the target. */
  , InvalidMatch
};

#endif

