
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "regression.h"
#include "termBaseOps.h"
void assignMeanResponse(uint           treeID,
                        TerminalBase  *parent,
                        double        *tn_regr_ptr) {
  uint j;
  stackMeanResponse(parent);
  for (j = 1; j <= parent -> rnfCount; j++) {
    (parent -> meanResponse)[j] = tn_regr_ptr[j];
  }
}
void calculateMeanResponse(uint           treeID,
                           TerminalBase  *parent,
                           uint          *membershipIndex,
                           uint           membershipSize,
                           uint          *membershipIterator) {
  uint i, j;
  stackMeanResponse(parent);
  for (j = 1; j <= parent -> rnfCount; j++) {
    (parent -> meanResponse)[j] = 0.0;
  }
  parent -> membrCount = membershipSize;
  for (i = 1; i <= membershipSize; i++) {
    (*membershipIterator) ++;
    for (j = 1; j <= parent -> rnfCount; j++) {
        (parent -> meanResponse)[j] += RF_response[treeID][RF_rNonFactorIndex[j]][membershipIndex[*membershipIterator]];
    }
  }
  if (membershipSize > 0) {
    for (j = 1; j <= parent -> rnfCount; j++) {
      (parent -> meanResponse)[j] = (parent -> meanResponse)[j] / (double) membershipSize;
    }
  }
}
