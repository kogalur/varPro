
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "polarityNew.h"
#include "splitInfo.h"
#include "factorOps.h"
#include "nodeBase.h"
#include "nrutil.h"
char getDaughterPolarityNew(uint treeID, SplitInfoMax *info, uint indv, void *value, ...) {
  char (*getDaughterPolarityGenericNew) (uint          treeID,
                                         SplitInfoMax *info,
                                         uint          indv,
                                         void         *value,
                                         ...);
  void *obsLocal;
  char daughterFlag;
  obsLocal = ((double **) value)[info -> splitParameter];
  if (info -> splitValueFactSize > 0) {
    getDaughterPolarityGenericNew = &getDaughterPolaritySimpleFactorNew;
  }
  else {
    getDaughterPolarityGenericNew = &getDaughterPolaritySimpleNonFactorNew;
  }
  daughterFlag = getDaughterPolarityGenericNew(treeID, info, indv, obsLocal);
  return daughterFlag;
}
char getDaughterPolaritySimpleFactorNew(uint treeID, SplitInfoMax *info, uint indv, void *value, ...) {
  char daughterFlag;
  daughterFlag = splitOnFactor((uint) ((double *) value)[indv], (uint*) info -> splitValueFactPtr);
  return daughterFlag;
}
char getDaughterPolaritySimpleNonFactorNew(uint treeID, SplitInfoMax *info, uint indv, void *value, ...) {
  char daughterFlag;
  daughterFlag =  (( info -> splitValueCont - ((double *) value)[indv]) >= 0.0) ? LEFT : RIGHT;
  return daughterFlag;
}
