#ifndef RF_TERMINAL_BASE_H
#define RF_TERMINAL_BASE_H
struct nodeBase;
typedef struct terminalBase TerminalBase;
struct terminalBase {
  unsigned int nodeID;
  struct nodeBase *mate;
  unsigned int eTypeSize;
  unsigned int mTimeSize;
  unsigned int sTimeSize;
  unsigned int   rnfCount;
  unsigned int   rfCount;
  unsigned int  *rnfIndex;
  unsigned int  *rfSize;
  unsigned int  *rfIndex;
  double        *meanResponse;
    unsigned int **multiClassProb;
  double        *maxClass;
  double *survival;
  double *nelsonAalen;
  double **csh;
  double **cif;
  double *mortality;
  unsigned int membrCount;
  void *outcome;
};
#endif
