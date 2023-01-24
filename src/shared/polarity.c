
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "polarity.h"
#include "splitInfo.h"
#include "factorOps.h"
#include "nodeBase.h"
#include "nrutil.h"
char getDaughterPolarity(uint treeID, SplitInfo *info, uint indv, void *value, ...) {
  char (*getDaughterPolarityGeneric) (uint       treeID,
                                      SplitInfo *info,
                                      uint       indv,
                                      void      *value,
                                      ...);
  void *obsLocal;
  char daughterFlag;
  obsLocal = ((double **) value)[info -> randomVar[1]];
  if (info -> mwcpSizeAbs[1] > 0) {
    getDaughterPolarityGeneric = &getDaughterPolaritySimpleFactor;
  }
  else {
    getDaughterPolarityGeneric = &getDaughterPolaritySimpleNonFactor;
  }
  daughterFlag = getDaughterPolarityGeneric(0, info, indv, obsLocal);
  return daughterFlag;
}
char getDaughterPolaritySimpleFactor(uint treeID, SplitInfo *info, uint indv, void *value, ...) {
  char daughterFlag;
  daughterFlag = splitOnFactor((uint) ((double *) value)[indv], (uint*) info -> randomPts[1]);
  return daughterFlag;
}
char getDaughterPolaritySimpleNonFactor(uint treeID, SplitInfo *info, uint indv, void *value, ...) {
  char daughterFlag;
  daughterFlag =  (( ((double*) info -> randomPts[1])[1] - ((double *) value)[indv]) >= 0.0) ? LEFT : RIGHT;
  return daughterFlag;
}
