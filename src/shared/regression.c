
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "regression.h"
#include "termBaseOps.h"
#include "nrutil.h"
void assignAllRegressionOutcomes(char          mode,
                                 uint          treeID,
                                 TerminalBase *term) {
  TerminalRegression *parent;
  parent = term -> regressionBase;
  assignMeanResponse(treeID, parent, RF_TN_REGR_ptr[treeID][term -> nodeID]);
}
void calculateAllRegressionOutcomes(char          mode,
                                    uint          treeID,
                                    TerminalBase *term) {
  TerminalRegression *parent;
  parent = term -> regressionBase;
  calculateMeanResponse(treeID, parent);
}
void assignMeanResponse(uint                 treeID,
                        TerminalRegression  *parent,
                        double              *tn_regr_ptr) {
  uint j;
  stackMeanResponse(parent);
  for (j = 1; j <= parent -> rnfCount; j++) {
    (parent -> meanResponse)[j] = tn_regr_ptr[j];
  }
}
void calculateMeanResponse(uint                treeID,
                           TerminalRegression *parent) {
  uint  membrSize;
  uint *membrIndx;
  uint i, j;
  stackMeanResponse(parent);
  for (j = 1; j <= parent -> rnfCount; j++) {
    (parent -> meanResponse)[j] = 0.0;
  }
  membrSize = parent -> base -> membrCount = parent -> base -> mate -> repMembrSize;
  membrIndx = parent -> base -> mate -> repMembrIndx;
  for (i = 1; i <= membrSize; i++) {
    for (j = 1; j <= parent -> rnfCount; j++) {
      (parent -> meanResponse)[j] += RF_response[treeID][parent -> rnfIndex[j]][membrIndx[i]];
    }
  }
  if (membrSize > 0) {
    for (j = 1; j <= parent -> rnfCount; j++) {
      (parent -> meanResponse)[j] = (parent -> meanResponse)[j] / (double) membrSize;
    }
  }
}
void updateEnsembleMean(char mode, uint treeID) {
  char oobFlag, fullFlag, outcomeFlag;
  TerminalBase ***termMembershipPtr;
  TerminalRegression *parent;
  uint    *membershipIndex;
  uint     membershipSize;
  double    **ensembleRGRnum;
  double     *ensembleDen;
#ifdef _OPENMP
  omp_lock_t   *lockDENptr;
#endif
  ensembleRGRnum = NULL;  
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
      ensembleRGRnum = RF_oobEnsembleRGRnum;
      ensembleDen    = RF_oobEnsembleDen;
      membershipSize  = RF_oobSize[treeID];
      membershipIndex = RF_oobMembershipIndex[treeID];
#ifdef _OPENMP
      lockDENptr      = RF_lockDENoens;
#endif
    }
    else {
      ensembleRGRnum = RF_fullEnsembleRGRnum;
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
      uint j, ii;
      ii = membershipIndex[i];
      parent = termMembershipPtr[treeID][ii] -> regressionBase;
#ifdef _OPENMP
      omp_set_lock(&(lockDENptr[ii]));
#endif
      ensembleDen[ii] ++;          
      for (j = 1; j <= RF_rTargetNonFactorCount; j++) {
        ensembleRGRnum[j][ii] += (parent -> meanResponse)[RF_rNonFactorMap[RF_rTargetNonFactor[j]]];
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
double getMeanSquareError(uint    size,
                          double *responsePtr,
                          double *predictedOutcome,
                          double *denomCount) {
  uint i;
  uint cumDenomCount;
  double result;
  cumDenomCount = 0;
  result = 0.0;
  for (i = 1; i <= size; i++) {
    if (denomCount[i] != 0) {
      cumDenomCount += 1;
      result += pow (responsePtr[i] - predictedOutcome[i], 2.0);
    }
  }  
  if (cumDenomCount == 0) {
    result = RF_nativeNaN;
  }
  else {
    result = result / (double) cumDenomCount;
  }
  return result;
}
