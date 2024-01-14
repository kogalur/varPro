
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "classification.h"
#include "termBaseOps.h"
#include "nrutil.h"
void assignMultiClassProb(uint           treeID,
                          TerminalBase  *parent,
                          uint         **tn_clas_ptr) {
  uint j, k;
  double maxValue, maxClass;
  stackMultiClassProb(parent);
  for (j = 1; j <= parent -> rfCount; j++) {
    for (k = 1; k <= parent -> rfSize[j]; k++) {
      (parent -> multiClassProb)[j][k] = tn_clas_ptr[j][k];
    }
  }
  for (j = 1; j <= parent -> rfCount; j++) {
    maxValue = 0;
    maxClass = 0;
    for (k=1; k <= parent -> rfSize[j]; k++) {
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
                             uint          *membershipIndex,
                             uint           membershipSize,
                             uint          *membershipIterator) {
  uint i, j, k;
  double maxValue, maxClass;
  stackMultiClassProb(parent);
  for (j = 1; j <= parent -> rfCount; j++) {
    for (k = 1; k <= parent -> rfSize[j]; k++) {
      (parent -> multiClassProb)[j][k] = 0;
    }
  }
  parent -> membrCount = membershipSize;
  for (i = 1; i <= membershipSize; i++) {
    for (j = 1; j <= parent -> rfCount; j++) {
      (parent -> multiClassProb)[j][(uint) RF_response[treeID][parent -> rfIndex[j]][membershipIndex[i]]] ++;
    }
  }
  for (j = 1; j <= parent -> rfCount; j++) {
    maxValue = 0;
    maxClass = 0;
    for (k=1; k <= parent -> rfSize[j]; k++) {
      if (maxValue < (double) (parent -> multiClassProb[j][k])) {
        maxValue = (double) parent -> multiClassProb[j][k];
        maxClass = (double) k;
      }
    }
    (parent -> maxClass)[j] = maxClass;
  }
}
void updateEnsembleClas(char mode, uint treeID) {
  char oobFlag, fullFlag, outcomeFlag;
  TerminalBase ***termMembershipPtr;
  uint    *membershipIndex;
  uint     membershipSize;
  double   ***ensembleCLSnum;
  double     *ensembleDen;
#ifdef _OPENMP
  omp_lock_t   *lockDENptr;
#endif
  ensembleCLSnum = NULL;  
  ensembleDen    = NULL;  
  oobFlag = fullFlag = FALSE;
  switch (mode) {
  case RF_PRED:
    if (RF_opt & OPT_FENS) {
      fullFlag = TRUE;
    }
    termMembershipPtr = RF_ftTermMembership;
    break;
  default:
    if (RF_opt & OPT_OENS) {
      if (RF_oobSize[treeID] > 0) {
        oobFlag = TRUE;
      }
    }
    if (RF_opt & OPT_IENS) {
      fullFlag = TRUE;
    }
    termMembershipPtr = RF_tTermMembership;
    break;
  }
  outcomeFlag = TRUE;
  while ((oobFlag == TRUE) || (fullFlag == TRUE)) {
    if (oobFlag == TRUE) {
      ensembleCLSnum = RF_oobEnsembleCLSnum;
      ensembleDen    = RF_oobEnsembleDen;
      membershipSize  = RF_oobSize[treeID];
      membershipIndex = RF_oobMembershipIndex[treeID];
#ifdef _OPENMP
      lockDENptr      = RF_lockDENoens;
#endif
    }
    else {
      ensembleCLSnum = RF_fullEnsembleCLSnum;
      ensembleDen    = RF_fullEnsembleDen;
      switch (mode) {
      case RF_PRED:
        membershipSize = RF_fobservationSize;
        membershipIndex = RF_fidentityMembershipIndex;
        break;
      default:
        membershipSize  = RF_observationSize;
        membershipIndex = RF_ibgMembershipIndex[treeID];
        break;
      }
#ifdef _OPENMP
      lockDENptr      = RF_lockDENfens;
#endif
    }
    for (uint i = 1; i <= membershipSize; i++) {
      TerminalBase *parent;
      uint j, k, ii;
      ii = membershipIndex[i];
      parent = termMembershipPtr[treeID][ii];
#ifdef _OPENMP        
      omp_set_lock(&(lockDENptr[ii]));
#endif
      ensembleDen[ii] ++;          
      for (j = 1; j <= RF_rTargetFactorCount; j++) {
        for (k = 1; k <= RF_rFactorSize[RF_rFactorMap[RF_rTargetFactor[j]]]; k++) {
          ensembleCLSnum[j][k][ii] += (double) (parent -> multiClassProb)[RF_rFactorMap[RF_rTargetFactor[j]]][k] / (double) (parent -> membrCount);
        }
      }
#ifdef _OPENMP
      omp_unset_lock(&(lockDENptr[ii]));
#endif
    }  
    if (outcomeFlag == TRUE) {
      outcomeFlag = FALSE;
    }
    if (oobFlag == TRUE) {
      oobFlag = FALSE;
    }
    else {
      fullFlag = FALSE;
    }
  }  
}
double getClassificationIndex(uint     size,
                              double  *responsePtr,
                              double  *denomCount,
                              double  *maxVote) {
  uint i;
  uint cumDenomCount;
  double result;
  cumDenomCount = 0;
  result = 0.0;
  for (i=1; i <= size; i++) {
    if (denomCount[i] > 0) {
      cumDenomCount += 1;
      if (responsePtr[i] == maxVote[i]) {
        result += 1.0;
      }
    }
    else {
      maxVote[i] = RF_nativeNaN;
    }
  }  
  if (cumDenomCount == 0) {
    result = RF_nativeNaN;
  }
  else {
    result = 1.0 - result / (double) cumDenomCount;
  }
  return result;
}
void getConditionalClassificationIndex(uint     size,
                                       uint     rfSizeElement,
                                       double  *responsePtr,
                                       double **outcomeCLS,
                                       double  *maxVote,
                                       double  *denomCount,
                                       double  *cpv) {
  uint i, k;
  uint cumDenomCount;
  uint *condClassificationCount;
  cumDenomCount = 0;
  condClassificationCount = uivector(1, rfSizeElement);
  for (k=1; k <= rfSizeElement; k++) {
    cpv[k] = condClassificationCount[k] = 0;
  }
  for (i = 1; i <= size; i++) {
    condClassificationCount[(uint) responsePtr[i]] ++;
    if (denomCount[i] != 0) {
      cumDenomCount += 1;
      if (responsePtr[i] == maxVote[i]) {
        cpv[(uint) responsePtr[i]] += 1.0;
      }
    }
  }  
  if (cumDenomCount == 0) {
    for (k=1; k <= rfSizeElement; k++) {
      cpv[k] = RF_nativeNaN;
    }
  }
  else {
    for (k=1; k <= rfSizeElement; k++) {
      if (condClassificationCount[k] != 0) {
        cpv[k] = 1.0 - cpv[k] / (double) condClassificationCount[k];
      }
      else {
        cpv[k] = RF_nativeNaN;
      }
    }
  }
  free_uivector(condClassificationCount, 1, rfSizeElement);
  return;
}
void getMaxVote(uint     size,
                uint    rfSizeElement,
                double **outcomeCLS,
                double  *denomCount,
                double  *maxVote) {
  uint i,k;
  double maxValue, maxClass;
  for (i = 1; i <= size; i++) {
    if (denomCount[i] > 0) {
      maxValue = 0.0;
      maxClass = 0.0;
      for (k = 1; k <= rfSizeElement; k++) {
        if (maxValue <= outcomeCLS[k][i]) {
          maxValue = outcomeCLS[k][i];
          maxClass = (double) k;
        }
      }
      maxVote[i] = maxClass;
    }
    else {
      maxVote[i] = RF_nativeNaN;
    }
  }  
}
