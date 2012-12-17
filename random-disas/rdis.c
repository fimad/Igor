#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#include <distorm.h>
#include <mnemonics.h>

#include "eval.h"
#include "match.h"
#include "eval_macros.h"
#include "errors.h"

/*
 * INST_STREAM_MAX gives the maximum length of an instruction stream in bytes,
 * INST_MAX gives the maximum number of instructions in a stream.
 */
#define INST_STREAM_MAX 16
#define INST_MAX        16

/*
 * Fill in a buffer of a given length with random bytes
 */
void randomizeBuffer (void *buffer, size_t length) {
  int tmp;
  /*
   * Copy random integer sized words into the buffer
   */
  while (length > sizeof(int)) {
    tmp = rand();
    *((int*)buffer) = tmp;
    buffer += sizeof(int);
    length -= sizeof(int);
  }
  /*
   * Fill in the remaining portion of the buffer with random bytes
   */
  tmp = rand();
  while (length > 0) {
    *((uint8_t*)buffer) = tmp & 0xFF;
    buffer += 1;
    length -= 1;
    tmp >>= 8;
  }
}

int main (int argc, char **argv) {
  srand(time(NULL));
  uint8_t byteStream[INST_STREAM_MAX];
  _CodeInfo info;
  struct State evalState;
  struct MatchInfo *matchInfo;
  struct Expression movTarget;
  int i,j,k;

  info.codeOffset = 0; 
  info.code = byteStream;
  info.codeLen = INST_STREAM_MAX;
  info.dt = Decode32Bits;
  info.features = 0; /* We don't want to stop on any special opcodes */

  movTarget.source = INVALID_LOCATION;
  movTarget.references = 0;
  movTarget.type = EConstantInt;
  movTarget.value.constantInt = 0;

  newState(&evalState);

  for (i=0; i<1000000; i++) {
    _DecodeResult result;
    _DInst instructions[INST_MAX];
    unsigned int instructionCount;

    /* Grab and decode a random stream of bytes. */
    randomizeBuffer(byteStream, INST_STREAM_MAX);
    result = distorm_decompose(&info, instructions, INST_MAX, &instructionCount);

    /* Start the evaluation with a fresh state. */
    clearState(&evalState);
    /* Attempt to evaluate the effect of each instruction decoded. */
    for( j=0; j<instructionCount; j++ ){
      if( eval(&evalState, &instructions[j]) == Success ){

        //if( instructions[j].opcode == I_MOV ) printf("GOT A REALLY SIMPLY MOV!\n");
        //else if( instructions[j].opcode == I_NOP ) printf("GOT A NOP!\n");
        if( match(&evalState, &movTarget, &matchInfo) == Success ){
          printf("Found a move!\n");
        }

      }else{
        /* 
         * Stop checking if we run into an unknown instruction. This will also
         * catch the end of the instruction stream.
         */
        break; 
      }
    }
  }
}

