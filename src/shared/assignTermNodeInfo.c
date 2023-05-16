
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "assignTermNodeInfo.h"
#include "regression.h"
#include "classification.h"
#include "survival.h"
void assignTerminalNodeMembership(char             mode,
                                  uint             treeID,
                                  TerminalBase    *parent,
                                  uint            *genMembrIndx,
                                  uint             genMembrSize,
                                  uint            *gmbrIterator,
                                  TerminalBase  ***tTermMembership) {
  uint i;
  if (RF_optHigh & OPT_MEMB_INCG) {
    for (i = 1; i <= genMembrSize; i++) {
      ++(*gmbrIterator);
      tTermMembership[treeID][genMembrIndx[(*gmbrIterator)]] = parent;
    }
  }
  else {
    for (i = 1; i <= genMembrSize; i++) {
      tTermMembership[treeID][genMembrIndx[i]] = parent;
    }
  }
}
void assignTerminalNodeOutcomes(char           mode,
                                uint           treeID,
                                TerminalBase  *parent,
                                uint           startTimeIndex,
                                uint           timeIndex,
                                uint           statusIndex,
                                uint          *rFactorSize) {
  if ((timeIndex > 0) && (statusIndex > 0)) {
    if (startTimeIndex == 0) {
      if (!(RF_opt & OPT_COMP_RISK)) {
        assignSurvival(treeID, parent, RF_TN_SURV_ptr[treeID][parent -> nodeID]);
        assignNelsonAalen(treeID, parent, RF_TN_NLSN_ptr[treeID][parent -> nodeID]);
      }
      else {
        assignCSH(treeID, parent, RF_TN_CSHZ_ptr[treeID][parent -> nodeID]);
        assignCIF(treeID, parent, RF_TN_CIFN_ptr[treeID][parent -> nodeID]);
      }
      assignMortality(treeID, parent, RF_TN_MORT_ptr[treeID][parent -> nodeID]);
    }
  }
  else {
    if (parent -> rfCount > 0) {
      assignMultiClassProb(treeID, parent, RF_TN_CLAS_ptr[treeID][parent -> nodeID]);
    }
    if (parent -> rnfCount > 0) {
      assignMeanResponse(treeID, parent, RF_TN_REGR_ptr[treeID][parent -> nodeID]);
    }
  }
}
