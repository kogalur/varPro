
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "classification.h"
#include "termBaseOps.h"
void assignMultiClassProb(uint           treeID,
                          TerminalBase  *parent,
                          uint          *rFactorSize,
                          uint         **tn_clas_ptr) {
  uint j, k;
  double maxValue, maxClass;
  stackMultiClassProb(parent, rFactorSize);
  for (j = 1; j <= parent -> rfCount; j++) {
    for (k = 1; k <= rFactorSize[j]; k++) {
      (parent -> multiClassProb)[j][k] = tn_clas_ptr[j][k];
    }
  }
  for (j = 1; j <= parent -> rfCount; j++) {
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
void calculateMultiClassProb(uint           treeID,
                             TerminalBase  *parent,
                             uint          *rFactorSize,
                             uint          *membershipIndex,
                             uint           membershipSize,
                             uint          *membershipIterator) {
  uint i, j, k;
  double maxValue, maxClass;
  stackMultiClassProb(parent, rFactorSize);
  for (j = 1; j <= parent -> rfCount; j++) {
    for (k = 1; k <= rFactorSize[j]; k++) {
      (parent -> multiClassProb)[j][k] = 0;
    }
  }
  parent -> membrCount = membershipSize;
  for (i = 1; i <= membershipSize; i++) {
    (*membershipIterator) ++;
    for (j = 1; j <= parent -> rfCount; j++) {
      (parent -> multiClassProb)[j][(uint) RF_response[treeID][RF_rFactorIndex[j]][membershipIndex[*membershipIterator]]] ++;
    }
  }
  for (j = 1; j <= parent -> rfCount; j++) {
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
