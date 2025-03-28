#ifndef RF_TERMINAL_H
#define RF_TERMINAL_H
typedef struct terminal Terminal;
struct terminal {
  struct terminalBase base;
  unsigned int   xReleaseCount;
  double       **compMeanResponse;
  double        *oobMeanResponse;
  unsigned int ***complementMCP;
  double         **complementMaxClass;
  unsigned int  **oobMCP;
  double         *oobMaxClass;
  double       *complementMortality;
  double        oobMortality;
  uint repMembrCount, oobMembrCount, ibgMembrCount, allMembrCount, testMembrCount;
  uint *repMembrIndx, *oobMembrIndx, *ibgMembrIndx, *allMembrIndx, *testMembrIndx;
};
#endif
