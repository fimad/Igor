#include <stdlib.h>
#include <mnemonics.h>
#include "eval.h"

void newState(struct State* state) {
  state->modifiedLocations = 0;
  state->locationValues = NULL;
}

/*
 * Frees the memory allocated for a given expression. This method will handle
 * freeing all sub expressions that have been allocated for this expression as
 * well.
 */
void freeExpression(struct Expression* expr){
  /* 
   * Decrement the number of references to this expression and if it hits 0 or
   * below actually free the memory.
   */
  if( --expr->references <= 0 ){
    switch( expr->type ){
      /* No extra work needed for freeing simple expressions. */
      case EConstantInt:
      case ELocation:
        free(expr);
      /* 
       * For binary operations, recursively free each child before freeing the
       * parent.
       */
      case EPlus:
      case EMinus:
        freeExpression(expr->value.left);
        freeExpression(expr->value.right);
        free(expr);
    }
  }
}

void clearState(struct State* state) {
  /* 
   * If there are modified locations free their memory before resetting the
   * state back to it's initial value.
   */
  if( state->locationValues ){
    for( int i=0; i<state->modifiedLocations; i++ ){
      freeExpression( state->locationValues[i] );
    }
    free(state->locationValues);
  }

  newState(state);
}

int eval(struct State* state, _DInst* inst) {
  /* Ensure that the instruction was properly decoded. */
  if( inst->flags != FLAG_NOT_DECODABLE ){
    /* Handle the effect of every known instruction. */
    switch( inst->opcode ){
      case I_NOP :
        return Success;
        
      case I_MOV :
        break;
    }
  }

  /* We did not know how to handle it. */
  return UnsupportedInstruction;
}

int valueOf(struct State* state, struct Location location, struct Expression* expr) {
}
