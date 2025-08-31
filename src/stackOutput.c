
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "stackOutput.h"
#include "terminal.h"
#include "shared/nrutil.h"
#include "shared/error.h"
char getStrengthTreeCount(char       mode,
                          uint       ntree,
                          uint       maxTree,
                          uint      *tLeafCount,
                          uint      *oobSZ,
                          uint      *strengthTreeCount) {
  uint i;
  uint nonRejectedTrees;
  uint rejectedTrees;
  char result;
  char flag;
  result = TRUE;
  rejectedTrees = 0;
  for(i = 1; i <= ntree; i++) {
    flag = TRUE;
    if (oobSZ != NULL) {
      if (oobSZ[i] < 1) {
        flag = FALSE;
      }
    }
    if ((tLeafCount[i] <= 1) || (flag == FALSE)) {
      rejectedTrees++;
    }
  }
  nonRejectedTrees = ntree - rejectedTrees;
  *strengthTreeCount = (maxTree > nonRejectedTrees) ? nonRejectedTrees : maxTree;
  if(*strengthTreeCount == 0) {
    RF_nativeError("\nVARPRO:  *** ERROR *** ");
    RF_nativeError("\nVARPRO:  Insufficient trees for analysis. All trees are stumped.");
    result = FALSE;
  }
  return result;
}
void stackStrengthObjectsPtrOnly(char       mode,
                                 uint      strengthTreeCount,
                                 uint     **strengthTreeID,
                                 uint     **branchCount,
                                 uint    ***branchID,
                                 uint    ***genericCount,
                                 uint    ***xReleaseCount,
                                 uint   ****xReleaseIDArray,
                                 uint   ****complementCount,
                                 uint   ****genericMembers,
                                 uint  *****complementMembers,
                                 uint    ***proxyIndv,
                                 uint    ***proxyIndvDepth) {
  uint i;
  *strengthTreeID = uivector(1, strengthTreeCount);
  *branchCount = uivector(1, strengthTreeCount);
  *branchID  = (uint **)  new_vvector(1, strengthTreeCount, NRUTIL_UPTR);
  *genericCount  = (uint **)  new_vvector(1, strengthTreeCount, NRUTIL_UPTR);
  *xReleaseCount  = (uint **)  new_vvector(1, strengthTreeCount, NRUTIL_UPTR);
  *xReleaseIDArray  = (uint ***)  new_vvector(1, strengthTreeCount, NRUTIL_UPTR2);
  *complementCount  = (uint ***)  new_vvector(1, strengthTreeCount, NRUTIL_UPTR2);
  *genericMembers  = (uint ***)  new_vvector(1, strengthTreeCount, NRUTIL_UPTR2);
  *complementMembers  = (uint ****)  new_vvector(1, strengthTreeCount, NRUTIL_UPTR3);
  *proxyIndv  = (uint **)  new_vvector(1, strengthTreeCount, NRUTIL_UPTR);
  *proxyIndvDepth  = (uint **)  new_vvector(1, strengthTreeCount, NRUTIL_UPTR);
  for (i = 1; i <= strengthTreeCount; i++) {
    (*branchID)[i]          = NULL;
    (*genericCount)[i]      = NULL;
    (*xReleaseCount)[i]     = NULL;
    (*xReleaseIDArray)[i]   = NULL;
    (*complementCount)[i]   = NULL;
    (*genericMembers)[i]    = NULL;
    (*complementMembers)[i] = NULL;
    (*proxyIndv)[i]         = NULL;
    (*proxyIndvDepth)[i]    = NULL;
  }
}
void unstackStrengthObjectsPtrOnly(char      mode,
                                   uint      strengthTreeCount,
                                   uint     *strengthTreeID,
                                   uint     *branchCount,
                                   uint    **branchID,
                                   uint    **genericCount,
                                   uint    **xReleaseCount,
                                   uint   ***xReleaseIDArray,
                                   uint   ***complementCount,
                                   uint   ***genericMembers,
                                   uint  ****complementMembers,
                                   uint    **proxyIndv,
                                   uint    **proxyIndvDepth) {
  free_uivector(strengthTreeID,      1, strengthTreeCount);
  free_uivector(branchCount,         1, strengthTreeCount);
  free_new_vvector(branchID,         1, strengthTreeCount, NRUTIL_UPTR);
  free_new_vvector(genericCount,     1, strengthTreeCount, NRUTIL_UPTR);
  free_new_vvector(xReleaseCount,    1, strengthTreeCount, NRUTIL_UPTR);
  free_new_vvector(xReleaseIDArray,  1, strengthTreeCount, NRUTIL_UPTR2);
  free_new_vvector(complementCount,  1, strengthTreeCount, NRUTIL_UPTR2);
  free_new_vvector(genericMembers,   1, strengthTreeCount, NRUTIL_UPTR2);
  free_new_vvector(complementMembers,1, strengthTreeCount, NRUTIL_UPTR3);
  free_new_vvector(proxyIndv,        1, strengthTreeCount, NRUTIL_UPTR);
  free_new_vvector(proxyIndvDepth,   1, strengthTreeCount, NRUTIL_UPTR);            
}
void selectTrees(uint    ntree,
                 uint    strengthTreeCount,
                 uint   *tLeafCount,
                 uint   *oobSZ,
                 uint   *strengthTreeID) {
  uint  j, i;
  uint  rejectedTrees;
  uint *sworVector;
  uint  sworVectorSize;
  uint  sworIndex;
  char  flag;
  rejectedTrees = 0;
  if(ntree == strengthTreeCount) {
    j = 0;
    for (i = 1; i <= ntree; i++) {
      flag = TRUE;
      if (oobSZ != NULL) {
        if (oobSZ[i] < 1) {
          flag = FALSE;
        }
      }
      if ((tLeafCount[i] <= 1) || (flag == FALSE)) {
      }
      else {
        strengthTreeID[++j] = i;
      }
    }
  }
  else {
    j = 0;
    sworVector = uivector(1, ntree);
    for (i = 1; i <= ntree; i++) {
      flag = TRUE;
      if (oobSZ != NULL) {
        if (oobSZ[i] < 1) {
          flag = FALSE;
        }
      }
      if ((tLeafCount[i] <= 1) || (flag == FALSE)) {
        rejectedTrees++;
      }
      else {
        sworVector[++j] = i;
      }
    }
    sworVectorSize = ntree - rejectedTrees;
    for (j = 1; j <= strengthTreeCount; j++) {
      sworIndex = (uint) ceil(ran1B(1) * (sworVectorSize * 1.0));
      strengthTreeID[j] = sworVector[sworIndex];
      sworVector[sworIndex] = sworVector[sworVectorSize];
      sworVectorSize --;
    }
    free_uivector (sworVector, 1, ntree);
  }
}
void selectBranches(uint    b,
                    uint    treeID,
                    uint    maxRulesTree,
                    uint    leafCount,
                    uint    *branchCount,
                    uint   **branchID,
                    uint   **oobCount,
                    uint   **xReleaseCount,
                    uint  ***xReleaseIDArray,
                    uint  ***complementCount,
                    uint  ***oobMembers,
                    uint ****complementMembers,
                    uint   **proxyIndv,
                    uint   **proxyIndvDepth) {
  uint k;
  uint currentBranchCount;
  uint  *sworVectorB;
  uint   sworVectorSizeB;
  uint   sworIndexB;
  if(leafCount == 1) {
    *branchCount = 0;
    RF_nativeError("\nVARPRO:  *** ERROR *** ");
    RF_nativeError("\nVARPRO:  Stump encountered for (strength, tree):  (%10d, %10d).", b, treeID);
    RF_nativeError("\nVARPRO:  Please Contact Technical Support.");
    RF_nativeExit();
  }
  else {
    *branchCount = (maxRulesTree > leafCount) ? leafCount : maxRulesTree;
    currentBranchCount = *branchCount;
    (*branchID) = uivector(1, currentBranchCount);
    (*oobCount) = uivector(1, currentBranchCount);
    (*xReleaseCount)     = uivector(1, currentBranchCount);
    (*xReleaseIDArray)   = (uint **)  new_vvector(1, currentBranchCount, NRUTIL_UPTR);
    (*complementCount)   = (uint **)  new_vvector(1, currentBranchCount, NRUTIL_UPTR);
    (*oobMembers)        = (uint **)  new_vvector(1, currentBranchCount, NRUTIL_UPTR);
    (*complementMembers) = (uint ***) new_vvector(1, currentBranchCount, NRUTIL_UPTR2);
    (*proxyIndv)      = uivector(1, currentBranchCount);
    (*proxyIndvDepth) = uivector(1, currentBranchCount);
    if(currentBranchCount == leafCount) {
      for (k = 1; k <= leafCount; k++) {
        (*branchID)[k] = k;
      }
    }
    else {
      sworVectorB = uivector(1, leafCount);
      for (k = 1; k <= leafCount; k++) {
        sworVectorB[k] = k;
      }
      sworVectorSizeB = leafCount;
      for (k = 1; k <= currentBranchCount; k++) {
        sworIndexB = (uint) ceil(ran1B(treeID) * (sworVectorSizeB * 1.0));
        (*branchID)[k] = sworVectorB[sworIndexB];
        sworVectorB[sworIndexB] = sworVectorB[sworVectorSizeB];
        sworVectorSizeB --;
      }
      free_uivector (sworVectorB, 1, leafCount);
    }
  }
}
void selectBranchesTrain(uint    b,
                         uint    treeID,
                         uint    maxRulesTree,
                         uint    leafCount,
                         TerminalBase **termMembership,
                         uint     membershipSize,
                         uint    *targMembershipIndx,
                         uint     targMembershipSize,
                         uint    *branchCount,
                         uint   **branchID,
                         uint   **oobCount,
                         uint   **xReleaseCount,
                         uint  ***xReleaseIDArray,
                         uint  ***complementCount,
                         uint  ***oobMembers,
                         uint ****complementMembers,
                         uint   **proxyIndv,
                         uint   **proxyIndvDepth) {
  uint i, k;
  uint leadingIndex;
  uint thisBranchCount;
  uint *informedBranchID;
  uint  *sworVectorB;
  uint   sworVectorSizeB;
  uint   sworIndexB;
  if(leafCount == 1) {
    *branchCount = 0;
    RF_nativeError("\nVARPRO:  *** ERROR *** ");
    RF_nativeError("\nVARPRO:  Stump encountered for (strength, tree):  (%10d, %10d).", b, treeID);
    RF_nativeError("\nVARPRO:  Please Contact Technical Support.");
    RF_nativeExit();
  }
  else {
    if(targMembershipSize == 0) {
      *branchCount = 0;
      RF_nativeError("\nVARPRO:  *** ERROR *** ");
      RF_nativeError("\nVARPRO:  Zero (0) out-of-bag or in-bag count encountered for (strength, tree):  (%10d, %10d).", b, treeID);
      RF_nativeError("\nVARPRO:  Please Contact Technical Support.");
      RF_nativeExit();
    }
    informedBranchID = uivector(1, targMembershipSize);
    for (i = 1; i <= targMembershipSize; i++) {
      informedBranchID[i] = termMembership[targMembershipIndx[i]] -> nodeID;
    }
    hpsortui(informedBranchID, targMembershipSize);
    leadingIndex = 1;
    for (i = 2; i <= targMembershipSize; i++) {
      if (informedBranchID[i] > informedBranchID[leadingIndex]) {
        leadingIndex++;
        informedBranchID[leadingIndex] = informedBranchID[i];
      }
    }
    thisBranchCount = *branchCount = (maxRulesTree > leadingIndex) ? leadingIndex : maxRulesTree;
    (*branchID) = uivector(1, thisBranchCount);
    (*oobCount) = uivector(1, thisBranchCount);
    (*xReleaseCount)     = uivector(1, thisBranchCount);
    (*xReleaseIDArray)   = (uint **)  new_vvector(1, thisBranchCount, NRUTIL_UPTR);
    (*complementCount)   = (uint **)  new_vvector(1, thisBranchCount, NRUTIL_UPTR);
    (*oobMembers)        = (uint **)  new_vvector(1, thisBranchCount, NRUTIL_UPTR);
    (*complementMembers) = (uint ***) new_vvector(1, thisBranchCount, NRUTIL_UPTR2);
    (*proxyIndv)      = uivector(1, thisBranchCount);
    (*proxyIndvDepth) = uivector(1, thisBranchCount);
    if(thisBranchCount == leadingIndex) {
      for (k = 1; k <= leadingIndex; k++) {
        (*branchID)[k] = informedBranchID[k];
      }
    }
    else {
      sworVectorB = uivector(1, leadingIndex);
      for (k = 1; k <= leadingIndex; k++) {
        sworVectorB[k] = k;
      }
      sworVectorSizeB = leadingIndex;
      for (k = 1; k <= thisBranchCount; k++) {
        sworIndexB = (uint) ceil(ran1B(treeID) * (sworVectorSizeB * 1.0));
        (*branchID)[k] = informedBranchID[sworVectorB[sworIndexB]];
        sworVectorB[sworIndexB] = sworVectorB[sworVectorSizeB];
        sworVectorSizeB --;
      }
      free_uivector (sworVectorB, 1, leadingIndex);
    }
    free_uivector(informedBranchID, 1, targMembershipSize);
  }
}
void selectBranchesTest(uint    b,
                        uint    treeID,
                        uint    maxRulesTree,
                        uint    leafCount,
                        TerminalBase **termMembership,
                        uint     membershipSize,
                        uint    *branchCount,
                        uint   **branchID,
                        uint   **oobCount,
                        uint   **xReleaseCount,
                        uint  ***xReleaseIDArray,
                        uint  ***complementCount,
                        uint  ***oobMembers,
                        uint ****complementMembers,
                        uint   **proxyIndv,
                        uint   **proxyIndvDepth) {
  uint i, k;
  uint leadingIndex;
  uint thisBranchCount;
  uint *informedBranchID;
  uint  *sworVectorB;
  uint   sworVectorSizeB;
  uint   sworIndexB;
  if(leafCount == 1) {
    *branchCount = 0;
    RF_nativeError("\nVARPRO:  *** ERROR *** ");
    RF_nativeError("\nVARPRO:  Stump encountered for (strength, tree):  (%10d, %10d).", b, treeID);
    RF_nativeError("\nVARPRO:  Please Contact Technical Support.");
    RF_nativeExit();
  }
  else {
    informedBranchID = uivector(1, membershipSize);
    for (i = 1; i <= membershipSize; i++) {
      informedBranchID[i] = termMembership[i] -> nodeID;
    }
    hpsortui(informedBranchID, membershipSize);
    leadingIndex = 1;
    for (i = 2; i <= membershipSize; i++) {
      if (informedBranchID[i] > informedBranchID[leadingIndex]) {
        leadingIndex++;
        informedBranchID[leadingIndex] = informedBranchID[i];
      }
    }
    thisBranchCount = *branchCount = (maxRulesTree > leadingIndex) ? leadingIndex : maxRulesTree;
    (*branchID) = uivector(1, thisBranchCount);
    (*oobCount) = uivector(1, thisBranchCount);
    (*xReleaseCount)     = uivector(1, thisBranchCount);
    (*xReleaseIDArray)   = (uint **)  new_vvector(1, thisBranchCount, NRUTIL_UPTR);
    (*complementCount)   = (uint **)  new_vvector(1, thisBranchCount, NRUTIL_UPTR);
    (*oobMembers)        = (uint **)  new_vvector(1, thisBranchCount, NRUTIL_UPTR);
    (*complementMembers) = (uint ***) new_vvector(1, thisBranchCount, NRUTIL_UPTR2);
    (*proxyIndv)      = uivector(1, thisBranchCount);
    (*proxyIndvDepth) = uivector(1, thisBranchCount);
    if(thisBranchCount == leadingIndex) {
      for (k = 1; k <= leadingIndex; k++) {
        (*branchID)[k] = informedBranchID[k];
      }
    }
    else {
      sworVectorB = uivector(1, leadingIndex);
      for (k = 1; k <= leadingIndex; k++) {
        sworVectorB[k] = k;
      }
      sworVectorSizeB = leadingIndex;
      for (k = 1; k <= thisBranchCount; k++) {
        sworIndexB = (uint) ceil(ran1B(treeID) * (sworVectorSizeB * 1.0));
        (*branchID)[k] = informedBranchID[sworVectorB[sworIndexB]];
        sworVectorB[sworIndexB] = sworVectorB[sworVectorSizeB];
        sworVectorSizeB --;
      }
      free_uivector (sworVectorB, 1, leadingIndex);
    }
    free_uivector(informedBranchID, 1, membershipSize);
  }
}
void freeStrengthBranchIDVectors(uint     strengthTreeCount,
                                 uint    *branchCount,
                                 uint   **branchID,
                                 uint   **oobCount,
                                 uint   **xReleaseCount,
                                 uint  ***xReleaseIDArray,
                                 uint  ***complementCount,
                                 uint  ***oobMembers,
                                 uint ****complementMembers,
                                 uint   **proxyIndv,
                                 uint   **proxyIndvDepth) {
  uint thisBranchCount;
  uint b;
  for (b = 1; b <= strengthTreeCount; b++) {
    thisBranchCount = branchCount[b];
    freeReleaseIDArray(thisBranchCount,
                       oobCount[b],
                       xReleaseCount[b],
                       xReleaseIDArray[b],
                       complementCount[b],
                       oobMembers[b],
                       complementMembers[b],
                       proxyIndvDepth[b]);
    free_uivector(branchID[b], 1, thisBranchCount);
    free_uivector(oobCount[b], 1, thisBranchCount);
    free_uivector(xReleaseCount[b], 1, thisBranchCount);
    free_new_vvector(xReleaseIDArray[b], 1, thisBranchCount, NRUTIL_UPTR);
    free_new_vvector(complementCount[b], 1, thisBranchCount, NRUTIL_UPTR);
    free_new_vvector(oobMembers[b], 1, thisBranchCount, NRUTIL_UPTR);
    free_new_vvector(complementMembers[b], 1, thisBranchCount, NRUTIL_UPTR2);
    free_uivector(proxyIndv[b], 1, thisBranchCount);
    free_uivector(proxyIndvDepth[b], 1, thisBranchCount);
  }
}
void freeReleaseIDArray(uint    branchCount,
                        uint   *oobCount,
                        uint   *xReleaseCount,
                        uint  **xReleaseIDArray,
                        uint  **complementCount,
                        uint  **oobMembers,
                        uint ***complementMembers,
                        uint   *proxyIndvDepth) {
  uint j;
  for(j = 1; j <= branchCount; j++) {
    freeComplementMembership(xReleaseCount[j], complementCount[j], complementMembers[j]);
    free_uivector(xReleaseIDArray[j], 1, proxyIndvDepth[j]);
    free_uivector(complementCount[j], 1, xReleaseCount[j]);
    if(oobCount[j] != 0) {
      free_uivector(oobMembers[j], 1, oobCount[j]);
    }
    free_new_vvector(complementMembers[j], 1, xReleaseCount[j], NRUTIL_UPTR);
  }
}
void freeComplementMembership(uint   xReleaseCount,
                              uint  *complementCount,
                              uint **complementMembers) {
  uint k;
  for(k = 1; k <= xReleaseCount; k++) { 
    if(complementCount[k] != 0) {
      free_uivector(complementMembers[k], 1, complementCount[k]);
    }
  }
}
void writeStrengthArray(uint     *strengthTreeID,
                        uint      strengthTreeCount,
                        uint    **branchID,
                        uint     *branchCount,
                        uint    **branchMemberCount,
                        uint   ***complementCount,
                        uint    **xReleaseCount,
                        uint   ***xReleaseIDArray,
                        uint     *treeID,
                        uint     *nodeID,
                        uint     *xReleaseID,
                        void     *brmCT,
                        void     *releaseStat) {
  uint  b,j,k,m,p;
  uint      row;
  Terminal *parent;
  row = 0;
  if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
    double    mortalityResponse;
    double    oobResponse;
    double    compResponse;
    for(b = 1; b <= strengthTreeCount; b++) {
      for(j = 1; j <= branchCount[b]; j++) {
        parent = (Terminal *) RF_tTermList[strengthTreeID[b]][branchID[b][j]];
        for(k = 1; k <= xReleaseCount[b][j]; k++) {
          row++;
          treeID[row]           = strengthTreeID[b];
          nodeID[row]           = branchID[b][j];
          xReleaseID[row]       = xReleaseIDArray[b][j][k];
          ((uint *) brmCT)[row] = branchMemberCount[b][j];
          if (VP_opt & (VP_OPT_CMP | VP_OPT_OOB)) {
          oobResponse = parent -> oobMortality;
          compResponse = (parent -> complementMortality)[k];
          mortalityResponse = 0.0;
          if ((VP_opt & VP_OPT_CMP) && (VP_opt & VP_OPT_OOB)) {
            if(RF_nativeIsNaN(compResponse) || RF_nativeIsNaN(oobResponse)) {
              mortalityResponse = RF_nativeNaN;
            }
            else {
              mortalityResponse = fabs(compResponse - oobResponse);
            }
          }
          else if ((VP_opt & VP_OPT_CMP) && !(VP_opt & VP_OPT_OOB)) {
            if(RF_nativeIsNaN(compResponse)) {
              mortalityResponse = RF_nativeNaN;
            }
            else {
              mortalityResponse = compResponse;
            }
          }
          else if ((VP_opt & VP_OPT_OOB) && !(VP_opt & VP_OPT_CMP)) {
            if(RF_nativeIsNaN(oobResponse)) {
              mortalityResponse = RF_nativeNaN;              
            }
            else {
              mortalityResponse = oobResponse;
            }
          }
          if (releaseStat != NULL) {
            ((double **) releaseStat)[1][row] = mortalityResponse;
          }
          }
        }
      }
    }
  }
  else if(RF_rNonFactorCount > 0) {
    double    meanResponse;
    double    oobResponse;
    double    compResponse;
    for(b = 1; b <= strengthTreeCount; b++) {
      for(j = 1; j <= branchCount[b]; j++) {
        parent = (Terminal *) RF_tTermList[strengthTreeID[b]][branchID[b][j]];
        for(k = 1; k <= xReleaseCount[b][j]; k++) {
          row++;
          treeID[row]           = strengthTreeID[b];
          nodeID[row]           = branchID[b][j];
          xReleaseID[row]       = xReleaseIDArray[b][j][k];
          ((uint *) brmCT)[row] = branchMemberCount[b][j];
          if (VP_opt & (VP_OPT_CMP | VP_OPT_OOB)) {
          for(p = 1; p <= RF_rNonFactorCount; p++) {
            oobResponse = (parent -> oobMeanResponse)[p];
            compResponse = (parent -> compMeanResponse)[k][p];
            meanResponse = 0.0;
            if ((VP_opt & VP_OPT_CMP) && (VP_opt & VP_OPT_OOB)) {
              if(RF_nativeIsNaN(compResponse) || RF_nativeIsNaN(oobResponse)) {
                meanResponse = RF_nativeNaN;
              }
              else {
                meanResponse = fabs(compResponse - oobResponse);
              }
            }
            else if ((VP_opt & VP_OPT_CMP) && !(VP_opt & VP_OPT_OOB)) {
              if(RF_nativeIsNaN(compResponse)) {
                meanResponse = RF_nativeNaN;
              }
              else {
                meanResponse = compResponse;
              }
            }
            else if ((VP_opt & VP_OPT_OOB) && !(VP_opt & VP_OPT_CMP)) {
              if(RF_nativeIsNaN(oobResponse)) {
                meanResponse = RF_nativeNaN;              
              }
              else {
                meanResponse = oobResponse;
              }
            }
            if (releaseStat != NULL) {
              ((double **) releaseStat)[p][row] = meanResponse;
            }
          }            
          }
        }
      }
    }
  }
  else if (RF_rFactorCount > 0) {
    for(b = 1; b <= strengthTreeCount; b++) {
      for(j = 1; j <= branchCount[b]; j++) {
        double *oobProb;
        oobProb = NULL;  
        parent = (Terminal *) RF_tTermList[strengthTreeID[b]][branchID[b][j]];
        if (VP_opt & (VP_OPT_CMP | VP_OPT_OOB)) {
          oobProb = dvector(1, RF_rFactorSize[1]);
          for(m = 1; m <= RF_rFactorSize[1]; m++) {
            if(branchMemberCount[b][j] == 0) {
              oobProb[m] = RF_nativeNaN;
            }
            else {
              oobProb[m] =  (double) parent -> oobMCP[1][m] / branchMemberCount[b][j];
            }
          }
        }
        for(k = 1; k <= xReleaseCount[b][j]; k++) {
          row++;
          treeID[row]      = strengthTreeID[b];
          nodeID[row]      = branchID[b][j];
          xReleaseID[row]  = xReleaseIDArray[b][j][k];
          ((uint ***) brmCT)[1][1][row]         = branchMemberCount[b][j];
          if (VP_opt & (VP_OPT_CMP | VP_OPT_OOB)) {
            double *compProb = dvector(1, RF_rFactorSize[1]);
            double *diffProb = dvector(1, RF_rFactorSize[1]);
            double sum = 0.0;
            double maxValue = 0.0;
            double maxClass = 0.0;
            double duplicatedVal = 0.0;
            for(m = 1; m <= RF_rFactorSize[1]; m++) {
              if(complementCount[b][j][k] == 0) {
                compProb[m] = RF_nativeNaN;
              }
              else {
                compProb[m] =  (double) parent -> complementMCP[k][1][m] / complementCount[b][j][k];
              }
            }
            if ((VP_opt & VP_OPT_CMP) && (VP_opt & VP_OPT_OOB)) {
              if((complementCount[b][j][k] != 0) && (branchMemberCount[b][j] != 0)) {
                for(m = 1; m <= RF_rFactorSize[1]; m++) {
                  sum += fabs(oobProb[m] - compProb[m]);
                  diffProb[m] = fabs(oobProb[m] - compProb[m]);
                  if (maxValue < diffProb[m]) {
                    maxValue = diffProb[m];
                    maxClass = (double) m;
                  }
                  else if (maxValue == diffProb[m]) {
                    duplicatedVal = maxValue;
                  }
                }
                sum = sum / RF_rFactorSize[1];
              }
              else {
                for(m = 1; m <= RF_rFactorSize[1]; m++) {
                  diffProb[m] = RF_nativeNaN;                
                }
                sum = RF_nativeNaN;
              }
            }
            else {
              sum = 0.0;
            }
            ((double ***) releaseStat)[1][1][row] = sum;
            if(duplicatedVal == maxValue && maxClass != 0.0) {
              double *duplicatedClasses = dvector(1, RF_rFactorSize[1]);
              uint dClassSize = 0;
              uint selectedIndex = 0;
              for (m = 1; m <= RF_rFactorSize[1]; m++) {
                if(diffProb[m] == duplicatedVal) {
                  duplicatedClasses[++dClassSize] = (double) m;
                }
              }
              selectedIndex = (uint) ceil(ran1B(strengthTreeID[b]) * (dClassSize * 1.0));
              maxClass = duplicatedClasses[selectedIndex];
              free_dvector(duplicatedClasses, 1, RF_rFactorSize[1]);
            }
            if ((VP_opt & VP_OPT_CMP) && (VP_opt & VP_OPT_OOB)) {
              if((complementCount[b][j][k] != 0) && (branchMemberCount[b][j] != 0)) {
                for (m = 1; m <= RF_rFactorSize[1]; m++) {                                    
                  if(m == maxClass) {
                    ((double ***) releaseStat)[1][m + 1][row] = diffProb[m];
                  }
                  else {
                    ((double ***) releaseStat)[1][m + 1][row] = 0.0;
                  }
                  ((uint ***) brmCT)[1][m + 1][row] = (parent -> oobMCP)[1][m];            
                }
              }
              else {
                for (m = 1; m <= RF_rFactorSize[1]; m++) {                                    
                  ((double ***) releaseStat)[1][m + 1][row] = RF_nativeNaN;
                  ((uint ***) brmCT)[1][m + 1][row] = (parent -> oobMCP)[1][m];
                }
              }
            }
            else if ((VP_opt & VP_OPT_CMP) && !(VP_opt & VP_OPT_OOB)) {
              for (m = 1; m <= RF_rFactorSize[1]; m++) {                                    
                ((double ***) releaseStat)[1][m + 1][row] = compProb[m];
                ((uint ***) brmCT)[1][m + 1][row] = (parent -> oobMCP)[1][m];
              }
            }
            else if ((VP_opt & VP_OPT_OOB) && !(VP_opt & VP_OPT_CMP)) {
              for (m = 1; m <= RF_rFactorSize[1]; m++) {                                    
                ((double ***) releaseStat)[1][m + 1][row] = oobProb[m];
                ((uint ***) brmCT)[1][m + 1][row] = (parent -> oobMCP)[1][m];
              }
            }
            free_dvector(compProb, 1, RF_rFactorSize[1]);
            free_dvector(diffProb, 1, RF_rFactorSize[1]);
          }
        }  
        if (VP_opt & (VP_OPT_CMP | VP_OPT_OOB)) {
          free_dvector(oobProb, 1, RF_rFactorSize[1]);
        }
      }  
    }  
  }
  else {
    for(b = 1; b <= strengthTreeCount; b++) {
      for(j = 1; j <= branchCount[b]; j++) {
        parent = (Terminal *) RF_tTermList[strengthTreeID[b]][branchID[b][j]];
        for(k = 1; k <= xReleaseCount[b][j]; k++) {
          row++;
          treeID[row]           = strengthTreeID[b];
          nodeID[row]           = branchID[b][j];
          xReleaseID[row]       = xReleaseIDArray[b][j][k];
          ((uint *) brmCT)[row] = branchMemberCount[b][j];
        }
      }
    }
  }
}
void writeMembershipArray(uint      strengthTreeCount,
                          uint     *branchCount,
                          uint    **branchMemberCount,
                          uint   ***complementCount,
                          uint    **xReleaseCount,
                          uint   ***branchMembers,
                          uint  ****complementMembers,
                          uint     *complementCT,
                          uint     *branchPopID,
                          uint     *complementID) {
  uint  b,j,k,p;
  uint      row, oobIndex, compIndex;
  row = 0;
  compIndex = 0;
  oobIndex = 0;
  for(b = 1; b <= strengthTreeCount; b++) {
    for(j = 1; j <= branchCount[b]; j++) {
      for(p = 1; p <= branchMemberCount[b][j]; p++) {
        branchPopID[++oobIndex] = branchMembers[b][j][p];
      }
      for(k = 1; k <= xReleaseCount[b][j]; k++) {
        complementCT[++row] = complementCount[b][j][k];
        for(p = 1; p <= complementCount[b][j][k]; p++) {
          complementID[++compIndex] = complementMembers[b][j][k][p];
        }
      }
    }
  }
}
void acquireTwinStat(uint strengthTreeCount,
                     uint *branchCount,
                     uint ***complementCount,
                     uint **xReleaseCount,
                     uint **branchID,
                     uint **testCaseNodeIDptr,
                     uint ***xReleaseIDArray,
                     uint ****complementMembers,
                     uint n,
                     uint xSize,
                     uint  i,
                     uint  neighbourSize,
                     uint  xReduceSize,
                     uint *xReduceIndx,
                     uint   **twinStatID_ptr,
                     double **twinStat_ptr,
                     uint  ***twinFreqTable_ptr) {
  uint  *vtWeight;
  double *vtStat;
  uint  releaseIndxCnt;
  double ratio, prob; 
  char *xReduceFlag = cvector(1, xSize);
  uint *xReduceIndxMap = uivector (1, xSize);
  if (xReduceSize > 0) {
    for (uint i = 1; i <= xSize; i++) {
      xReduceFlag[i] = FALSE;
    }
    uint iter = 0;
    for (uint i = 1; i <= xReduceSize; i++) {
      xReduceFlag[xReduceIndx[i]] = TRUE;
      xReduceIndxMap[xReduceIndx[i]] = (++iter);
    }
  }
  else {
    for (uint i = 1; i <= xSize; i++) {
      xReduceFlag[i] = TRUE;
      xReduceIndxMap[i] = i;      
    }
  }
  char *releaseFlag = cvector(1, xSize);
  uint *releaseIndx = uivector(1, xSize);
  uint *releaseIndxMap = uivector(1, xSize);
  vtWeight = uivector(1, n);
  vtStat  = dvector(1, n);
  for (uint m = 1; m <= xSize; m++) {
    releaseFlag[m] = FALSE;
  }
  for (uint b = 1; b <= strengthTreeCount; b++) {
    for (uint j = 1; j <= branchCount[b]; j++) {
      for (uint k = 1; k <= xReleaseCount[b][j]; k++) {
        if  (xReduceFlag[ xReleaseIDArray[b][j][k] ] == TRUE) {
          if (branchID[b][j] == testCaseNodeIDptr[b][i]) {
            releaseFlag[xReleaseIDArray[b][j][k]] = TRUE;
          }
        }
      }
    }
  }
  releaseIndxCnt = 0;
  for (uint m = 1; m <= xSize; m++) {
    releaseIndxMap[m] = 0;
    if (releaseFlag[m] == TRUE) {
      releaseIndx[++releaseIndxCnt] = m;
      releaseIndxMap[m] = releaseIndxCnt;
    }
  }
  if (releaseIndxCnt > 0) {
    uint **vtTable = uimatrix(1, releaseIndxCnt, 1, n);
    for (uint m = 1; m <= releaseIndxCnt; m++) {
      for (uint j = 1; j <= n; j++) {
        vtTable[m][j] = 0;
      }
    }
    for (uint b = 1; b <= strengthTreeCount; b++) {
      for (uint j = 1; j <= branchCount[b]; j++) {
        for (uint k = 1; k <= xReleaseCount[b][j]; k++) {
          if  (xReduceFlag[ xReleaseIDArray[b][j][k] ] == TRUE) {
            if (branchID[b][j] == testCaseNodeIDptr[b][i]) {
              for (uint p = 1; p <= complementCount[b][j][k]; p++) {
                vtTable[ releaseIndxMap[ xReleaseIDArray[b][j][k]] ]  [ complementMembers[b][j][k][p] ] ++;
              }
            }
          }
        }
      }
    }
    for (uint j = 1; j <= n; j++) {                
      vtWeight[j] = 0;
      for (uint m = 1; m <= releaseIndxCnt; m++) {                
        vtWeight[j] += vtTable[m][j];
      }
    }
    for (uint j = 1; j <= n; j++) {                
      prob = 0.0;
      for (uint m = 1; m <= releaseIndxCnt; m++) {                
        if (vtWeight[j] > 0) {
          ratio = ((double) vtTable[m][j]) / vtWeight[j];
          ratio = ratio * (1.0 - ratio);
          prob += ratio;
        }
      }
      vtStat[j] = prob / releaseIndxCnt;
    }
    for (uint j = 1; j <= n; j++) {
      vtStat[j] = vtStat[j] * vtWeight[j];
    }
    uint *neighbourIndx = uivector(1, neighbourSize);
    getMinHeap(i, neighbourSize, n, vtStat, neighbourIndx);
    for (uint j = 1; j <= neighbourSize; j++) {
      twinStat_ptr[i][j] = vtStat[neighbourIndx[neighbourSize - j + 1]];
    }
    for (uint j = 1; j <= neighbourSize; j++) {
      twinStatID_ptr[i][j] = neighbourIndx[neighbourSize - j + 1];
    }
    if (VP_opt & VP_OPT_FRQ) {
      for (uint j = 1; j <= neighbourSize; j++) {
        for (uint k = 1; k <= releaseIndxCnt; k++) {
          uint xTarget = releaseIndx[k];
          if (releaseFlag[xTarget] == TRUE) {
            twinFreqTable_ptr[i][j][ xReduceIndxMap[xTarget] ] = vtTable[k] [neighbourIndx[neighbourSize - j + 1]];
          }
        }
      }
    }
    free_uivector(neighbourIndx, 1, neighbourSize);    
    free_uimatrix(vtTable, 1, releaseIndxCnt, 1, n);
  }
  else {
    for (uint j = 1; j <= neighbourSize; j++) {
      VP_twinStat_ptr[i][j] = 0;
    }
  }
  free_uivector(vtWeight, 1, n);
  free_dvector(vtStat, 1, n);
  free_uivector(releaseIndxMap, 1, xSize);
  free_uivector(releaseIndx, 1, xSize);
  free_cvector(releaseFlag, 1, xSize);
  free_uivector(xReduceIndxMap, 1, xSize);
  free_cvector(xReduceFlag, 1, xSize);
}
void getMinHeap(uint twin, uint m, uint n, double *value, uint *minHeapIndx) {
  double temp;
  uint tempIndx;
  uint iter;
  char flip;
  double *neighbour = dvector(1, m);
  for (uint j = 1; j <= m; j++) {
    iter = j;
    neighbour[j] = value[j];
    minHeapIndx[j] = j;
    while(iter > 1) {
      if (neighbour[iter] < neighbour[iter-1]) {
        temp = neighbour[iter];
        neighbour[iter] = neighbour[iter-1];
        neighbour[iter-1] = temp;
        tempIndx = minHeapIndx[iter];
        minHeapIndx[iter] = minHeapIndx[iter-1];
        minHeapIndx[iter-1] = tempIndx;
        iter--;
      }
      else {
        break;
      }
    }
  }
  for (uint j = m; j <= n; j++) {
    iter = 1;
    if (value[j] >= neighbour[1]) {
      if (value[j] == neighbour[1]) {
        flip = (ran1C(twin) >= 0.5);
      }
      else {
        flip = TRUE;
      }
      if (flip) {
        neighbour[1] = value[j];
        minHeapIndx[1] = j;
        while(iter < m) {
          if (neighbour[iter] > neighbour[iter+1]) {
            temp = neighbour[iter];
            neighbour[iter] = neighbour[iter+1];
            neighbour[iter+1] = temp;
            tempIndx = minHeapIndx[iter];
            minHeapIndx[iter] = minHeapIndx[iter+1];
            minHeapIndx[iter+1] = tempIndx;
            iter++;
          }
          else {
            break;
          }
        }
      }
    }
    else {
    }
  }
  free_dvector(neighbour, 1, m);
}
void testTwinMembership() {
  uint iter, treeID, nodeID, cellTreeID, cellNodeID;
  for (uint ii = 1; ii <= RF_fobservationSize; ii++) {
    for (uint bb = 1; bb <= VP_strengthTreeCount; bb++) {
      iter = 0;
      cellTreeID = VP_strengthTreeID_[bb];
      cellNodeID = VP_testCaseNodeIDptr[bb][ii]; 
      char flag = TRUE;
      while (flag) {
        iter ++;
        treeID = VP_treeID_[iter];
        nodeID = VP_nodeID_[iter];
        if ((cellTreeID == treeID) && (cellNodeID == nodeID)) {
        }
        else {
          if (treeID > cellTreeID) {
            flag = FALSE;
            iter --;
          }
          if (iter >= VP_totalRecordCount) {
            flag = FALSE;
          }
        }
      }
    }
  }
}
void testMinHeap() {
  double *testValue = dvector(1, 20);
  for (uint j = 1; j <= 20; j++) {
    testValue[j] = ran1D(1) * 50;
  }
  uint *neighbourIndx = uivector(1, 5);
  getMinHeap (1, 5, 20, testValue, neighbourIndx);
  free_uivector(neighbourIndx, 1, 5);
  free_dvector(testValue, 1, 20);
}
