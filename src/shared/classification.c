
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
#include           "trace.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "classification.h"
#include "termBaseOps.h"
void assignMultiClassProb(uint           treeID,
                          TerminalBase  *parent,
                          uint           rFactorCount,
                          uint          *rFactorSize,
                          uint         **tn_clas_ptr) {
  uint j, k;
  double maxValue, maxClass;
  stackMultiClassProb(parent, rFactorCount, rFactorSize);
  for (j = 1; j <= rFactorCount; j++) {
    for (k = 1; k <= rFactorSize[j]; k++) {
      (parent -> multiClassProb)[j][k] = tn_clas_ptr[j][k];
    }
  }
  for (j = 1; j <= rFactorCount; j++) {
    maxValue = 0;
    maxClass = 0;
    for (k=1; k <= rFactorSize[j]; k++) {
      if (maxValue < (double) (parent -> multiClassProb[j][k])) {
        maxValue = (double) parent -> multiClassProb[j][k];
        maxClass = (double) k;
      }
    }
    (parent -> maxClass)[j] = maxClass;
  }
}
