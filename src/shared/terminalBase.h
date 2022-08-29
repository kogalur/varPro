#ifndef RF_TERMINAL_BASE_H
#define RF_TERMINAL_BASE_H
struct nodeBase;
typedef struct terminalBase TerminalBase;
struct terminalBase {
  unsigned int nodeID;
  struct nodeBase *mate;
  unsigned int   rnfCount;
  double        *meanResponse;
  unsigned int   rfCount;
  unsigned int  *rfSize;
  unsigned int **multiClassProb;
  double        *maxClass;
  unsigned int membrCount;
  void *outcome;
};
#endif
