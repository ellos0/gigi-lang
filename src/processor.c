#include "processor.h"
#include <stdio.h>
#include <stdlib.h>

void writeFile(char[] file, char[] content) {
  FILE* fptr = fopen(*file,"w");
  
  fprintf(fptr, content);
  fclose(fptr)
}

char[] readFile(char[] file) {
  FILE* fptr = fopen(file,"r");
  if (fptr == NULL) {
    printf("File (%s) does not exist", file);
    return NULL;
  }
  
}
