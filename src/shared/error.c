
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "error.h"
void exit2R(void) {
  error("\nRF-SRC:  The application will now exit.\n");
}
void printR(char *format, ...) {
  char *buffer;
  va_list aptr;
  buffer = (char *) malloc(sizeof(char) * 1023);
  va_start(aptr, format);
  vsprintf(buffer, format, aptr);
  va_end(aptr);
  Rprintf(buffer);
  free((char *) buffer);
}
