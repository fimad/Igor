#ifndef _MATCH_H_
#define _MATCH_H_

#include "expr.h"

struct State;

struct MatchedLocation {
  /* The id of a Variable Location. */
  uint32_t id;
  /*
   * The location that the variable location that was matched in the tested
   * expression.
   */
  struct Location value;
};

/* Contains location information about the result of an expression matching. */
struct MatchInfo {
  /* The location whose expression matches the target. */
  struct Location source;

  /* An array of variable locations that have been matched. */
  uint32_t numVariables;
  uint32_t allocatedVariables;
  struct MatchedLocation *variables;

  /* An array of Locations that are clobbered.  */
  uint32_t numClobbered;
  uint32_t allocatedClobbered;
  struct Location *clobbered;
};

/*
 * Does any modified location in the given state match the given expression? If
 * it does, fill in the given match structure with the corresponding values.
 * 
 * The return value is Success (0) if the match was a success, and the
 * corresponding error otherwise.
 */
int match(struct State *state, struct Expression *target, struct MatchInfo **info);

/* Frees memory allocate by a match info struct. */
void freeMatchInfo(struct MatchInfo *info);

#endif

