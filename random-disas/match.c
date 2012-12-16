#include <stdlib.h>

#include "match.h"
#include "eval.h"
#include "eval_macros.h"
#include "errors.h"

/*
 * TODO: Currently the matching processes will short circuit after the first
 * match is found. This is likely not a problem as a unique matching processes
 * is run for each gadget type. This means that any missed gadget types will be
 * of the same type as the current.
 */

/*
 * TODO: Currently there is no support for variable constants in expressions.
 * This will almost certainly be needed in the future. Likely, it can be
 * implemented similarly to variable locations. Perhaps, in the target
 * expression, fill in the values of constants with an integer representing
 * their id, and then create an array in the MatchInfo struct that maps from
 * constant id's to their value in the match.
 */

/* 
 * Attempts to assign a given location to the location variable with the given
 * id. If the variable has already been set to a different location, then it
 * will return an error, if it is successful it will return Success (0).
 */
int setLocationVariable(struct MatchInfo *info, uint32_t id, struct Location location){
  int i;
  /* Look for id in the already matched variables. */
  for( i=0; i<info->numVariables; i++ ){
    if( info->variables[i].id == id ){
      /* Ensure that the variable is set to the desired value. */
      if( LOCATION_EQ(info->variables[i].value, location) ){
        return Success;
      }else{
        return InvalidMatch;
      }
    }
  }

  /* The variable was not found we must add it to the list. */
  if( info->numVariables == info->allocatedVariables ){
    /* We need to allocate more space for variables. */
    size_t newSize = (sizeof(struct MatchedLocation))*(info->allocatedVariables ? info->allocatedVariables*2 : 4);
    info->variables = realloc(info->variables, newSize);
  }
  info->variables[info->numVariables++] = ((struct MatchedLocation){id, location});

  return Success;
}

/* 
 * Attempts to match two expression structs, and fills in the variable
 * assignments in info. This method does not fill in the clobbered list.
 */
int matchHelper(struct Expression *target, struct Expression *candidate, struct MatchInfo *info){
  /* Ensure the expression types are equivalent. */
  printf("%d == %d ?....\n", target->type, candidate->type);
  if( target->type == candidate->type ){
    printf("equal types???\n");
    switch( target->type ){
      case EConstantInt :
        /* TODO: pass? */
        printf("const???....\n");
        return Success;
        
      case ELocation :
        printf("hey....\n");
        return setLocationVariable(info, target->value.location.id, candidate->value.location);

      case EPlus :
      case EMinus :
        printf("+/-???....\n");
        if( matchHelper(target->value.left, candidate->value.left, info) == Success &&
            matchHelper(target->value.right, candidate->value.right, info) == Success ){
          return Success;
        }else{
          return InvalidMatch;
        }
    }
  }

  /* Mismatched expression types, impossible to match. */
  return InvalidMatch;
}

/*
 * Attempt to match the candidate expression with the target. If we are * successful, create a MatchInfo struct that contains the mapping for the
 * variable locations. 
 */
int match(struct State *state, struct Expression *target, struct MatchInfo **info){
  int i;

  *info = calloc(sizeof(struct MatchInfo), 1);
  if( *info == NULL ){
    return OutOfMemory;
  }

  for( i=0; i<state->modifiedLocations; i++ ){
    /* Reset the match info before each attempted matching. */
    (*info)->numVariables = 0;
    (*info)->numClobbered = 0;

    /* Attempt to match the current location with the target */
    if( matchHelper(target, state->locationValues[i], *info) == Success ){
      /* TODO: fill in clobbered....*/
      (*info)->source = state->locationValues[i]->source;
      return Success;
    }
  }

  /*
   * If we reached this point, it means that none of the modified locations
   * matched the target expression.
   */
  freeMatchInfo(*info);
  return InvalidMatch;
}


/* Frees memory allocate by a match info struct. */
void freeMatchInfo(struct MatchInfo *info){
  if( info->allocatedVariables ){
    free(info->variables);
  }
  if( info->allocatedClobbered ){
    free(info->clobbered);
  }
  free(info);
}

