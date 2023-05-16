
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "calculateTermNodeInfo.h"
#include "regression.h"
#include "classification.h"
void calculateTerminalNodeOutcomes(char           mode,
                                   uint           treeID,
                                   TerminalBase  *parent,
                                   uint           rfCount,
                                   uint          *rFactorSize,
                                   uint          *rFactorIndex,
                                   uint          *genMembrIndx,
                                   uint           genMembrSize,
                                   uint          *gmbrIterator) {
  if (FALSE) {
  }
  else {
    if (parent -> rfCount > 0) {
      calculateMultiClassProb(treeID, parent, genMembrIndx, genMembrSize, gmbrIterator);
    }
    if (parent -> rnfCount > 0) {
      calculateMeanResponse(treeID, parent, genMembrIndx, genMembrSize, gmbrIterator);
    }
  }
}
