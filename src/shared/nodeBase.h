#ifndef RF_NODE_BASE_H
#define RF_NODE_BASE_H
struct terminalBase;
typedef struct nodeBase NodeBase;
struct nodeBase {
  unsigned int nodeID;
  unsigned int pnodeID;
  unsigned int blnodeID;
  unsigned int brnodeID;
  unsigned int fsrecID;
  struct nodeBase *parent;
  struct nodeBase *left;
  struct nodeBase *right;
  struct terminalBase *mate;
  unsigned int xSize;
  char *permissible;
  uint *permissibleIndx;
  uint  permissibleIndxSize;
  char  permissibleReIndxFlag;
  char  permissibleOwnershipFlag;
  char splitFlag;
  double mean;
  double variance;
  unsigned int depth;
  int *mpSign;
  int *fmpSign;
  struct splitInfo *splitInfo;
  unsigned int *repMembrIndx;
  unsigned int *allMembrIndx;
  unsigned int  repMembrSizeAlloc;
  unsigned int  allMembrSizeAlloc;
  unsigned int  repMembrSize;
  unsigned int  allMembrSize;
  unsigned int  oobMembrSizeAlloc;
  unsigned int  oobMembrSize;
  unsigned int *oobMembrIndx;
  unsigned int lotsSize;
};
#endif
