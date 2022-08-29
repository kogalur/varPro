
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
#include           "trace.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "assignTermNodeInfo.h"
#include "regression.h"
#include "classification.h"
void assignTerminalNodeMembership(char             mode,
                                  uint             treeID,
                                  TerminalBase    *parent,
                                  uint            *genMembrIndx,
                                  uint             genMembrSize,
                                  uint            *gmbrIterator,
                                  TerminalBase  ***tTermMembership,
                                  uint           **AMBR_ID_ptr) {
  uint i;
  if (RF_optHigh & OPT_MEMB_INCG) {
    for (i = 1; i <= genMembrSize; i++) {
      ++(*gmbrIterator);
      tTermMembership[treeID][AMBR_ID_ptr[treeID][(*gmbrIterator)]] = parent;
    }
  }
  else {
    for (i = 1; i <= genMembrSize; i++) {
      tTermMembership[treeID][genMembrIndx[i]] = parent;
    }
  }
}
void assignTerminalNodeOutcomes(char       mode,
                                uint       treeID,
                                TerminalBase  *parent,
                                uint       rFactorCount,
                                uint      *rFactorSize,
                                uint       rNonFactorCount) {
  if (FALSE) {
  }
  else {
    if (rFactorCount > 0) {
      assignMultiClassProb(treeID, parent, rFactorCount, rFactorSize, RF_TN_CLAS_ptr[treeID][parent -> nodeID]);
    }
    if (rNonFactorCount > 0) {
      assignMeanResponse(treeID, parent, rNonFactorCount, RF_TN_REGR_ptr[treeID][parent -> nodeID]);
    }
  }
}
