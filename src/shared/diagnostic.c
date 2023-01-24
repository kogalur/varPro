
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "diagnostic.h"
#include "error.h"
#include "nrutil.h"
void getSplitObjectInfo(SplitInfo *info) {
  RF_nativePrint("\nSplitInfo:  %20x \n", info);
  RF_nativePrint("\n  info -> size        :    %20d", info -> size);
  RF_nativePrint("\n  info -> indicator   : 0x %20x", info -> indicator);
  RF_nativePrint("\n  info -> randomVar   : 0x %20x", info -> randomVar);
  RF_nativePrint("\n  info -> mwcpSizeAbs : 0x %20x", info -> mwcpSizeAbs);
  RF_nativePrint("\n  info -> randomPts   : 0x %20x", info -> randomPts);
  RF_nativePrint("\n   x-variable:   %10d", info -> randomVar[1]);
  RF_nativePrint("\n");
  int covariate = info -> randomVar[1];
  if (info -> mwcpSizeAbs[1] > 0) {
    RF_nativePrint(" (cov = %10d, mwcpPT =", covariate);
    for (uint m = 1; m <= info -> mwcpSizeAbs[1]; m++) {
      RF_nativePrint(" %10x", ((uint *) info -> randomPts[1])[m]);
    }
    RF_nativePrint(")");
  }
  else {
    RF_nativePrint(" (cov = %10d, spltPT = %12.4f) ", covariate, ((double *) info -> randomPts[1])[1]);
  }
  RF_nativePrint("\n");
}
void getNodeBaseInfo(NodeBase *nodePtr) {
  RF_nativePrint("\nNodeBaseInfo:  (address, node) = (%20x, %10d)", nodePtr, nodePtr -> nodeID);
  if (nodePtr -> splitInfo != NULL) {
    getSplitObjectInfo(nodePtr -> splitInfo);
  }
  RF_nativePrint("\n");
}
void getTerminalBaseInfo(TerminalBase *termPtr) {
  RF_nativePrint("\nTerminalBaseInfo:  %20x", termPtr);
  RF_nativePrint("\n  nodeID: %10d", termPtr -> nodeID);
  RF_nativePrint("\n");
  RF_nativePrint("\n membrCount          = %20d", termPtr -> membrCount);
}
void printTreeInfo(uint treeID, NodeBase *parent) {
  if (((parent -> left) != NULL) && ((parent -> right) != NULL)) {
    printTreeInfo(treeID, parent ->  left);
    printTreeInfo(treeID, parent -> right);
  }
}
void initTimer(void) {
}
void printTimer(void) {
}
void printParameters(char mode) {
}
