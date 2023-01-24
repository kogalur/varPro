
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "bootstrap.h"
#include "random.h"
#include "nrutil.h"
#include "sampling.h"
#include "error.h"
char bootstrap (char      mode,
                uint      treeID,
                NodeBase *nodePtr,
                uint     *subsetIndex,
                uint      subsetSize,
                uint      indexSize,
                uint    **bootstrapIn,
                uint      subjSize,
                double   *subjWeight,
                uint      subjWeightType,
                uint     *subjWeightSorted,
                uint      subjWeightDensitySize,
                uint      observationSize,
                uint     *index,  
                char    **bootMembershipFlag,
                char    **oobMembershipFlag,
                uint    **bootMembershipCount,
                uint    **bootMembershipIndex,
                uint     *oobSize,
                uint     *ibgSize,
                uint    **ibgMembershipIndex,
                uint    **oobMembershipIndex,
                uint    **BOOT_CT_ptr) {  
  char *permissible;
  char result;
  uint i, j, k;
  result = TRUE;
  if (!(RF_opt & OPT_BOOT_TYP1) && !(RF_opt & OPT_BOOT_TYP2)) {
    for (i=1; i <= subsetSize; i++) {
      index[i] = subsetIndex[i];
    }
  }
  else {
    if (RF_opt & OPT_BOOT_TYP2) {
      i = 0;
      for (k = 1; k <= subjSize; k++) {
        for (j = 1; j <= bootstrapIn[treeID][k]; j++) {
          index[++i] = k;
        }
      }
    }
    else {
      if (subjWeightType == RF_WGHT_UNIFORM) {
        if (RF_optHigh & OPT_BOOT_SWOR) {
          uint *sworVector = uivector(1, subsetSize);
          uint sworVectorSize = subsetSize;
          uint sworIndex;
          for (j = 1; j <= sworVectorSize; j++) {
            sworVector[j] = subsetIndex[j];
          }
          for (j = 1; j <= indexSize; j++) {
            sworIndex = (uint) ceil(ran1A(treeID) * (sworVectorSize * 1.0));
            index[j] = sworVector[sworIndex];
            sworVector[sworIndex] = sworVector[sworVectorSize];
            sworVectorSize --;
          }
          free_uivector (sworVector, 1, subsetSize);
        }
        else {    
          for (i = 1; i <= indexSize; i++) {
            k = (uint) ceil(ran1A(treeID)*(subsetSize * 1.0));
            index[i] = subsetIndex[k];
          }
        }
      }
      else {
        if (subjWeightType != RF_WGHT_UNIFORM) {
          permissible = cvector(1, subjSize);
          for (i = 1; i <= subjSize; i++) {
            permissible[i] = FALSE;
          }
          for (i = 1; i <= subsetSize; i++) {
            permissible[subsetIndex[i]] = TRUE;
          }
        }
        else {
          permissible = NULL;
        }
        DistributionObj *obj = makeDistributionObjRaw();
        obj -> permissibleIndex = (subjWeightType == RF_WGHT_UNIFORM) ? subsetIndex : NULL;
        obj -> permissible       = (subjWeightType == RF_WGHT_UNIFORM) ? NULL : permissible;
        obj -> permissibleSize   = (subjWeightType == RF_WGHT_UNIFORM) ? subsetSize : subjSize;
        obj -> augmentationSize = NULL;
        obj -> weightType = subjWeightType;
        obj -> weight = subjWeight;
        obj -> weightSorted = subjWeightSorted;
        obj -> densityAllocSize = subjWeightDensitySize;
        initializeCDFNew(treeID, obj);
        for (i = 1; i <= indexSize; i++) {
          index[i] = sampleFromCDFNew(ran1A, treeID, obj);
          if (RF_optHigh & OPT_BOOT_SWOR) {
            if (index[i] != 0) {
              updateCDFNew(treeID, obj);
            }
            else {
              RF_nativeError("\nRF-SRC:  *** ERROR *** ");
              RF_nativeError("\nRF-SRC:  No cases left to select for bootstrap SWOR of size:  %10d", indexSize);
              RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
              RF_nativeExit();
            }
          }
        }
        discardCDFNew(treeID, obj);
        freeDistributionObjRaw(obj);
        if (subjWeightType != RF_WGHT_UNIFORM) {
          free_cvector(permissible, 1, subjSize);
        }
      }
    }
  }
  uint iter;
  for (i = 1; i <= observationSize; i++) {
    bootMembershipFlag[treeID][i]  = FALSE;
    oobMembershipFlag[treeID][i]   = TRUE;
    bootMembershipCount[treeID][i] = 0;
  }
  iter = 0;
  for (i = 1; i <= indexSize; i++) {
    bootMembershipIndex[treeID][++iter] = index[i];
    bootMembershipFlag[treeID][index[i]] = TRUE;
    oobMembershipFlag[treeID][index[i]]  = FALSE;
    bootMembershipCount[treeID][index[i]] ++;
    if (RF_optHigh & OPT_MEMB_USER) {
      BOOT_CT_ptr[treeID][index[i]] ++;
    }
  }
  oobSize[treeID] = 0;
  ibgSize[treeID] = 0;
  for (i = 1; i <= observationSize; i++) {
    if (bootMembershipFlag[treeID][i] == FALSE) {
      oobMembershipIndex[treeID][++oobSize[treeID]] = i;
    }
    else {
      ibgMembershipIndex[treeID][++ibgSize[treeID]] = i;
    }
  }
  if (result) {
    result = TRUE;
  }
  else {
  }
  if (result) {
  } 
  else {
  }
  return result;
}
