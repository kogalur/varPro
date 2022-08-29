
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
#include           "trace.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "diagnostic.h"
#include "error.h"
#include "nrutil.h"
void getSplitObjectInfo(SplitInfo *info) {
  RF_nativePrint("\nSplitInfo:  %20x \n", info);
  RF_nativePrint("\n  info -> size        :    %20d", info -> size);
  RF_nativePrint("\n  info -> indicator   : 0x %20x", info -> indicator);
  RF_nativePrint("\n  info -> hcDim       :    %20d", info -> hcDim);
  RF_nativePrint("\n  info -> randomVar   : 0x %20x", info -> randomVar);
  RF_nativePrint("\n  info -> mwcpSizeAbs : 0x %20x", info -> mwcpSizeAbs);
  RF_nativePrint("\n  info -> randomPts   : 0x %20x", info -> randomPts);
  RF_nativePrint("\n  info -> randomPtsR  : 0x %20x", info -> randomPtsRight);
  if (info -> hcDim == 0) {
    RF_nativePrint("\n  hcDimension:   %10d", 0);
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
  }
  else {
    RF_nativePrint("\n  hcDimension:  ");
    for (uint ii = 1; ii <= info -> hcDim; ii++) {
      RF_nativePrint(" %10d", ii);
    }
    RF_nativePrint("\n   x-variable:  ");
    for (uint ii = 1; ii <= info -> hcDim; ii++) {
      RF_nativePrint(" %10d", info -> randomVar[ii]);
    }
    RF_nativePrint("\n");
    for (uint ii = 1; ii <= info -> hcDim; ii++) {  
      int covariate = info -> randomVar[ii];
      if (info -> mwcpSizeAbs[ii] > 0) {
        RF_nativePrint(" (cov = %10d, mwcpPT =", covariate);
        for (uint m = 1; m <= info -> mwcpSizeAbs[ii]; m++) {
          RF_nativePrint(" %10x", ((uint *) info -> randomPts[ii])[m]);
        }
        RF_nativePrint(") \n");
      }
      else {
        if ((info -> augmX1 == NULL) && (info -> augmX2 == NULL) && (info -> augmXS == NULL)) {
          RF_nativePrint(" (cov = %10d, spltPT = %12.4f, spltPTright = %12.4f)",
                         covariate,
                         ((double *) info -> randomPts[ii])[1],
                         ((double *) info -> randomPtsRight[ii])[1]);
        }
        else if ((uint) covariate <= RF_xSize) {
          RF_nativePrint(" (cov = %10d, spltPT = %12.4f, spltPTright = %12.4f)",
                         covariate,
                         ((double *) info -> randomPts[ii])[1],
                         ((double *) info -> randomPtsRight[ii])[1]);
        }
        else if ((info -> augmX1 != NULL) && (info -> augmX2 != NULL) && (info -> augmXS == NULL)) {
          RF_nativePrint(" (AugmX1 cov = %10d, AugmX2 cov = %10d, spltPT = %12.4f, spltPTright = %12.4f)", info -> augmX1[ii], info -> augmX2[ii], ((double *) info -> randomPts[ii])[1]);
        }
        else if ((info -> augmX1 == NULL) && (info -> augmX2 == NULL) && (info -> augmXS != NULL)) {
          RF_nativePrint(" (AugmXS cov = %10d, spltPT = %12.4f, spltPTright = %12.4f)", info -> augmXS[ii], ((double *) info -> randomPts[ii])[1]);
        }
        else if ((info -> augmX1 != NULL) && (info -> augmX2 != NULL) && (info -> augmXS == NULL)) {
          RF_nativePrint(" (AugmX1 cov = %10d, AugmX2 cov = %10d, AugmXS cov = %10d, spltPT = %12.4f, spltPTright = %12.4f)", info -> augmX1[ii], info -> augmX2[ii], info -> augmXS[ii], ((double *) info -> randomPts[ii])[1]);
        }
        RF_nativePrint("\n");
      }
    }
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
void initTimer() {
}
void printTimer() {
}
void printParameters(char mode) {
}
