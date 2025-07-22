#include <stdio.h>
#include <stdlib.h>

typedef union data {
    int intValue;
    float floatValue;
    char charValue;
} Data;

Data* makeStack(int len) {
    Data* ptr;

    ptr = (Data*)malloc(len*sizeof(Data));
    return ptr;
}; 

typedef enum instruct {
    nop,
    label,
    push,
    pop,
    dup,
    add,
    sub,
    mul,
    divide
} instruction;

typedef struct instructPair {
    int instruction;
    void* op;
} instructionPair;

typedef struct Runtime {
    int* words[];
    int pc; // program counter
    Data* stack;
} runtime;

Data* evaluatePair(runtime r, instructionPair c) {
    switch (c.instruction) {
        case 0:
            return stack;
            break;
        case 1:
            
        default:
            return stack;
    }
}

int main() {
    Data* data = makeStack(100);
    instruction i1 = nop;
    instructionPair ip1;
    ip1.instruction = i1;
    ip1.op = NULL;

    evaluatePair(data,0,ip1);

    free(data);
    return 0;
}
