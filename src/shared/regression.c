
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
#include           "trace.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "regression.h"
#include "termBaseOps.h"
void assignMeanResponse(uint           treeID,
                        TerminalBase  *parent,
                        uint           rNonFactorCount,
                        double        *tn_regr_ptr) {
  uint j;
  stackMeanResponse(parent, rNonFactorCount);
  for (j = 1; j <= rNonFactorCount; j++) {
    (parent -> meanResponse)[j] = tn_regr_ptr[j];
  }
}
