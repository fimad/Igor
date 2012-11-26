#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#include <distorm.h>
#include <mnemonics.h>

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
  int i,j,k;

  info.codeOffset = 0; 
  info.code = byteStream;
  info.codeLen = INST_STREAM_MAX;
  info.dt = Decode32Bits;
  info.features = 0; /* We don't want to stop any any special opcodes */


  double average = 0;
  double average2 = 0;
  for (i=0; i<1000000; i++) {
    _DecodeResult result;
    _DInst instructions[INST_MAX];
    unsigned int instructionCount;

    randomizeBuffer(byteStream, INST_STREAM_MAX);
    result = distorm_decompose(&info, instructions, INST_MAX, &instructionCount);

    if( result == DECRES_SUCCESS ){
      average += instructionCount/1000000.0;
      average2 += (instructionCount*instructionCount)/1000000.0;
//      printf("--------------------------------------------------------------------------------\n", instructionCount);
//      for( j=0; j<instructionCount; j++ ){
//        if( instructions[j].opcode == OPCODE_ID_NONE ){
//          break;
//        }else{
//          printf("\t%s ", GET_MNEMONIC_NAME(instructions[j].opcode));
//          for( k=0; k<OPERANDS_NO; k++ ){
//            if( instructions[j].ops[k].type == O_NONE ){
//              break;
//            }else if( instructions[j].ops[k].type == O_REG ){
//              printf("\t%s ", GET_REGISTER_NAME(instructions[j].ops[k].index));
//            }
//          }
//          printf("\n");
//        }
//      }
//      printf("\n");
    }
  }
  printf("Average instructions: %f\n", average);
  printf("Standard Deviation:   %f\n", sqrt(average2 - average*average));
}

