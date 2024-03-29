#ifndef RF_TERMINAL_H
#define RF_TERMINAL_H
typedef struct terminal Terminal;
struct terminal {
  struct terminalBase base;
  double       **compMeanResponse;
  double        *oobMeanResponse;
  unsigned int   xReleaseCount;
  unsigned int ***complementMCP;
  unsigned int  **oobMCP;
  double         **complementMaxClass;
  double         *oobMaxClass;
};
#endif
