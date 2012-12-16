#include <stdlib.h>

#include "expr.h"
#include "errors.h"

/* 
 * Decrements the amount of references to an expression. Once the reference
 * reaches 0 or lower, the resource is freed.
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

/* Increments the reference count for this expression. */
void claimExpression(struct Expression* expr){
  if( expr ){
    expr->references++;
  }
}

/* Allocate a new expression. */
struct Expression* newExpression(struct Location source){
  struct Expression *expr = calloc(1, sizeof(struct Expression));
  if( !expr ){
    return NULL;
  }
  expr->source = source;
  expr->references = 0;
  return expr;
}

