#include <stdio.h>

#define KEY 47

extern int xorEnc(int*, int, int);

int main(int argc, char *argv[]){
    int i;
    int array[] = {1,5,2,4,3,103, 234, 42};
    int length = sizeof(array)/sizeof(int);

    printf("Original:\n");
    for( i=0; i<length; i++ ){
        printf("%d  ", array[i]);
    }

    printf("\nCorrect:\n");
    for( i=0; i<length; i++ ){
        printf("%d  ", KEY ^ array[i]);
    }

    xorEnc(array, length, KEY);
    printf("\nResult:\n");
    for( i=0; i<length; i++ ){
        printf("%d  ", array[i]);
    }
    printf("\n");
}
