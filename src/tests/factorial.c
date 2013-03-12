#include <stdio.h>

extern int factorial(int);

int main(int argc, char *argv[]){
    int result = factorial(10);
    printf("%d\n", result);
}
