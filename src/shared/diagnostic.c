
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "diagnostic.h"
#include "error.h"
#include "nrutil.h"
void getSplitObjectInfo(SplitInfoMax *info) {
  RF_nativePrint("\nSplitInfoMax:  %20x \n", info);
  RF_nativePrint("\n  info -> size        :    %20d", info -> size);
  RF_nativePrint("\n  info -> indicator   : 0x %20x", info -> indicator);
  RF_nativePrint("\n   x-variable:   %10d", info -> splitParameter);
  RF_nativePrint("\n");
  int covariate = info -> splitParameter;
  if (info -> splitValueFactSize > 0) {
    RF_nativePrint(" (cov = %10d, mwcpPT =", covariate);
    for (uint m = 1; m <= info -> splitValueFactSize; m++) {
      RF_nativePrint(" %10x", info -> splitValueFactPtr[m]);
    }
    RF_nativePrint(")");
  }
  else {
    RF_nativePrint(" (cov = %10d, spltPT = %12.4f) ", covariate, info -> splitValueCont);
  }
  RF_nativePrint("\n");
}
void getNodeBaseInfo(NodeBase *nodePtr) {
  RF_nativePrint("\nNodeBaseInfo:  (address, node) = (%20x, %10d)", nodePtr, nodePtr -> nodeID);
  if (nodePtr -> splitInfo != NULL) {
    getSplitObjectInfo(nodePtr -> splitInfoMax);
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
