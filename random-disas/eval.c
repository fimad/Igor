#include <stdlib.h>
#include <mnemonics.h>

#include "eval.h"
#include "eval_macros.h"

void newState(struct State* state) {
  state->modifiedLocations = 0;
  state->allocatedLocations = 0;
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
        break;
      /* 
       * For binary operations, recursively free each child before freeing the
       * parent.
       */
      case EPlus:
      case EMinus:
        freeExpression(expr->value.left);
        freeExpression(expr->value.right);
        free(expr);
        break;
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

/* Decompose an operand into a location struct. */
struct Location getOperandLocation(_DInst* inst, int index){
  if( 3 < index || index < 0 || inst->ops[index].type == O_NONE ){
    return INVALID_LOCATION;
  }

  /* Decompose the operand in the case of a register type. */
  if( inst->ops[index].type == O_REG ){
    switch( inst->ops[index].index ){
      case R_EAX : 
        return REGISTER_LOCATION(LRegEAX);
      case R_ECX : 
        return REGISTER_LOCATION(LRegECX);
      case R_EDX : 
        return REGISTER_LOCATION(LRegEDX);
      case R_EBX : 
        return REGISTER_LOCATION(LRegEBX);
      case R_ESP : 
        return REGISTER_LOCATION(LRegESP);
      case R_EBP : 
        return REGISTER_LOCATION(LRegEBP);
      case R_EDI : 
        return REGISTER_LOCATION(LRegEDI);
      case R_ESI : 
        return REGISTER_LOCATION(LRegESI);
    }
  }
  /* The operand is an immediate */
  else if( inst->ops[index].type == O_IMM ){
    /* 
     * Immediates don't really count as locations. This operand should be
     * queried using the getOperandExpression method instead.
     */
    return INVALID_LOCATION;
  }

  /* The location is of an unsupported type. */
  return INVALID_LOCATION;
}

/* Allocate a new expression. */
struct Expression* newExpression(struct Location source){
  struct Expression *expr = calloc(1, sizeof(struct Expression));
  if( !expr ){
    return NULL;
  }
  expr->source = source;
  expr->references = 1;
  return expr;
}

/* Decompose an operand into a expression struct. */
struct Expression* getOperandExpression(_DInst* inst, int index){
  struct Expression *expr;

  /* Ensure the input is valid */
  if( 3 < index || index < 0 || inst->ops[index].type == O_NONE ){
    return NULL;
  }

  /* Create a new expression. */
  expr = newExpression(INVALID_LOCATION);
  if( !expr ){
    return NULL;
  }

  /* In the event of a register, wrap the register location as an expression. */
  if( inst->ops[index].type == O_REG ){
    expr->type = ELocation;
    expr->value.location = getOperandLocation(inst, index);
    return expr;
  }
  /* The operand is an immediate */
  else if( inst->ops[index].type == O_IMM ){
    expr->type = EConstantInt;
    expr->value.constantInt = inst->imm.sqword;
    return expr;
  }

  /* 
   * If we reach this point, we have not been able to turn the operand into an
   * expression so we will free the memory we have allocated and return NULL to
   * signify our failure.
   */
  freeExpression(expr);
  return NULL;
}

/* 
 * Log in state that the value corresponding to location is now equal to the
 * given expression.
 */
int setLocation(struct State *state, struct Location location, struct Expression *expr){
  /* 
   * Look for location in the locationValues array, and update it's value if
   * found
   */
  for( int i=0; i<state->modifiedLocations; i++ ){
    /* 
     * If we find the location in the array, free the existing expression and
     * replace it with the new value.
     */
    if( LOCATION_EQ(state->locationValues[i]->source, location) ){
      freeExpression(state->locationValues[i]);
      state->locationValues[i] = expr;
      return Success;
    }
  }

  /* 
   * If we reach this point, the location was not previously stored in the
   * state, and we must add it.
   */

  /*Grow the modified locations array if there is no more room. */
  if( state->allocatedLocations == state->modifiedLocations ){
    /* 
     * The new number of allocated locations is twice the number of old
     * allocated locations. If there weren't any previously, we start at 4.
     */
    int newAllocatedLocations =
      (state->allocatedLocations) ? state->allocatedLocations*2 : 4;

    struct Expression **tmp =
      realloc(state->locationValues, sizeof(struct Expression*)*newAllocatedLocations );
    if( tmp ){
      state->locationValues = tmp;
    }else{
      return OutOfMemory;
    }
  }

  /* Append the new value to the state. */
  state->locationValues[state->modifiedLocations++] = expr;
  return Success;
}

int eval(struct State* state, _DInst* inst) {
  /* Grab the first two operands of the instruction. */
  struct Location operand0 = getOperandLocation(inst, 0);
  //struct Location operand1 = getOperandLocation(inst, 1);

  struct Expression *expr;

  /* Ensure that the instruction was properly decoded. */
  if( inst->flags != FLAG_NOT_DECODABLE ){
    /* Handle the effect of every known instruction. */
    switch( inst->opcode ){
      case I_NOP :
        return Success;
        
      case I_MOV :
        if( IS_VALID(operand0) ){
          expr = getOperandExpression(inst, 1);
          if( !expr) return UnsupportedOperand;
          return setLocation(state, operand0, expr);
        }
        break;
    }
  }

  /* We did not know how to handle it. */
  return UnsupportedInstruction;
}

int valueOf(struct State* state, struct Location location, struct Expression* expr) {
}

