
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "shared/trace.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "stackOutput.h"
#include "terminal.h"
#include "shared/nrutil.h"
#include "shared/error.h"
void stackStrengthObjectsPtrOnly(char       mode,
                                 uint       ntree,
                                 uint       maxTree,
                                 uint      *tLeafCount,
                                 uint      *strengthTreeCount,
                                 uint     **strengthTreeID,
                                 uint     **branchCount,
                                 uint    ***branchID,
                                 uint    ***oobCount,
                                 uint    ***xReleaseCount,
                                 uint   ****xReleaseIDArray,
                                 uint   ****complementCount,
                                 uint    ***proxyIndv,
                                 uint    ***proxyIndvDepth) {
  uint i;
  uint nonRejectedTrees;
  uint rejectedTrees;
  rejectedTrees = 0;
  for(i = 1; i <= ntree; i++) {
    if(tLeafCount[i] <= 1) {
      rejectedTrees++;
    }
  }
  nonRejectedTrees = ntree - rejectedTrees;
  *strengthTreeCount = (maxTree > nonRejectedTrees) ? nonRejectedTrees : maxTree;
  if(*strengthTreeCount == 0) {
    RF_nativeError("\nVARPRO:  *** ERROR *** ");
    RF_nativeError("\nVARPRO:  Insufficient trees for analysis. All trees are stumped.");
    RF_nativeError("\nVARPRO:  The application will now exit.\n");    
    RF_nativeExit();
  }
  *strengthTreeID = uivector(1, *strengthTreeCount);
  *branchCount = uivector(1, *strengthTreeCount);
  *branchID  = (uint **)  new_vvector(1, *strengthTreeCount, NRUTIL_UPTR);
  *oobCount  = (uint **)  new_vvector(1, *strengthTreeCount, NRUTIL_UPTR);
  *xReleaseCount  = (uint **)  new_vvector(1, *strengthTreeCount, NRUTIL_UPTR);
  *xReleaseIDArray  = (uint ***)  new_vvector(1, *strengthTreeCount, NRUTIL_UPTR2);
  *complementCount  = (uint ***)  new_vvector(1, *strengthTreeCount, NRUTIL_UPTR2);
  *proxyIndv  = (uint **)  new_vvector(1, *strengthTreeCount, NRUTIL_UPTR);
  *proxyIndvDepth  = (uint **)  new_vvector(1, *strengthTreeCount, NRUTIL_UPTR);
  for (i = 1; i <= *strengthTreeCount; i++) {
    (*branchID)[i]        = NULL;
    (*oobCount)[i]        = NULL;
    (*xReleaseCount)[i]   = NULL;
    (*xReleaseIDArray)[i] = NULL;
    (*complementCount)[i] = NULL;
    (*proxyIndv)[i]       = NULL;
    (*proxyIndvDepth)[i]  = NULL;
  }
}
void unstackStrengthObjectsPtrOnly(char      mode,
                                   uint      strengthTreeCount,
                                   uint     *strengthTreeID,
                                   uint     *branchCount,
                                   uint    **branchID,
                                   uint    **oobCount,
                                   uint    **xReleaseCount,
                                   uint   ***xReleaseIDArray,
                                   uint   ***complementCount,
                                   uint    **proxyIndv,
                                   uint    **proxyIndvDepth) {
  free_uivector(strengthTreeID,      1, strengthTreeCount);
  free_uivector(branchCount,         1, strengthTreeCount);
  free_new_vvector(branchID,         1, strengthTreeCount, NRUTIL_UPTR);
  free_new_vvector(oobCount,         1, strengthTreeCount, NRUTIL_UPTR);
  free_new_vvector(xReleaseCount,    1, strengthTreeCount, NRUTIL_UPTR);
  free_new_vvector(xReleaseIDArray,  1, strengthTreeCount, NRUTIL_UPTR2);
  free_new_vvector(complementCount,  1, strengthTreeCount, NRUTIL_UPTR2);
  free_new_vvector(proxyIndv,        1, strengthTreeCount, NRUTIL_UPTR);
  free_new_vvector(proxyIndvDepth,   1, strengthTreeCount, NRUTIL_UPTR);            
}
void selectTrees(uint    ntree,
                 uint    strengthTreeCount,
                 uint   *tLeafCount,
                 uint   *strengthTreeID) {
  uint j, i;
  uint rejectedTrees;
  uint *sworVector;
  uint  sworVectorSize;
  uint  sworIndex;
  rejectedTrees = 0;
  if(ntree == strengthTreeCount) {
    i = 0;
    for (j = 1; j <= ntree; j++) {
      if(tLeafCount[j] <= 1) {
        RF_nativeError("\nVARPRO:  *** ERROR *** ");
        RF_nativeError("\nVARPRO:  Stump encountered for tree: %10d.", j);
        RF_nativeError("\nVARPRO:  Please Contact Technical Support.");
        RF_nativeExit();
      }
      else {
        strengthTreeID[++i] = j;
      }
    }
  }
  else {
    i = 0;
    sworVector = uivector(1, ntree);
    for (j = 1; j <= ntree; j++) {
      if(tLeafCount[j] <= 1) {
        rejectedTrees++;
        RF_nativeError("\nVARPRO:  *** ERROR *** ");
        RF_nativeError("\nVARPRO:  Stump encountered for tree: %10d.", j);
        RF_nativeError("\nVARPRO:  Please Contact Technical Support.");
        RF_nativeExit();
      }
      else {
        sworVector[++i] = j;
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
                    uint   *branchCount,
                    uint  **branchID,
                    uint  **oobCount,
                    uint  **xReleaseCount,
                    uint ***xReleaseIDArray,
                    uint ***complementCount,
                    uint  **proxyIndv,
                    uint  **proxyIndvDepth) {
  uint k;
  uint currentBranchCount;
  uint  *sworVectorB;
  uint   sworVectorSizeB;
  uint   sworIndexB;
  if(leafCount == 1) {
    *branchCount = 0;
    RF_nativeError("\nVARPRO:  *** ERROR *** ");
    RF_nativeError("\nVARPRO:  Stump encountered for tree: %10d.", treeID);
    RF_nativeError("\nVARPRO:  Please Contact Technical Support.");
    RF_nativeExit();
  }
  else {
    *branchCount = (maxRulesTree > leafCount) ? leafCount : maxRulesTree;
    currentBranchCount = *branchCount;
    (*branchID) = uivector(1, currentBranchCount);
    (*oobCount) = uivector(1, currentBranchCount);
    (*xReleaseCount)    = uivector(1, currentBranchCount);
    (*xReleaseIDArray)  = (uint **) new_vvector(1, currentBranchCount, NRUTIL_UPTR);
    (*complementCount)  = (uint **) new_vvector(1, currentBranchCount, NRUTIL_UPTR);
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
void freeStrengthBranchIDVectors(uint    strengthTreeCount,
                                 uint   *branchCount,
                                 uint  **branchID,
                                 uint  **oobCount,
                                 uint  **xReleaseCount,
                                 uint ***xReleaseIDArray,
                                 uint ***complementCount,
                                 uint  **proxyIndv,
                                 uint  **proxyIndvDepth) {
  uint currentBranchCount;
  uint b;
  for (b = 1; b <= strengthTreeCount; b++) {
    currentBranchCount = branchCount[b];
    freeReleaseIDArray(currentBranchCount, xReleaseCount[b], xReleaseIDArray[b], complementCount[b], proxyIndvDepth[b]);
    free_uivector(branchID[b], 1, currentBranchCount);
    free_uivector(oobCount[b], 1, currentBranchCount);
    free_uivector(xReleaseCount[b], 1, currentBranchCount);
    free_new_vvector(xReleaseIDArray[b], 1, currentBranchCount, NRUTIL_UPTR);
    free_new_vvector(complementCount[b], 1, currentBranchCount, NRUTIL_UPTR);
    free_uivector(proxyIndv[b], 1, currentBranchCount);
    free_uivector(proxyIndvDepth[b], 1, currentBranchCount);
  }
}
void freeReleaseIDArray(uint   branchCount,
                        uint  *xReleaseCount,
                        uint **xReleaseIDArray,
                        uint **complementCount,
                        uint  *proxyIndvDepth) {
  uint j;
  for(j = 1; j <= branchCount; j++) { 
    free_uivector(xReleaseIDArray[j], 1, proxyIndvDepth[j]);
    free_uivector(complementCount[j], 1, xReleaseCount[j]);
  }
}
void writeStrengthArray(uint     *strengthTreeID,
                        uint      strengthTreeCount,
                        uint    **branchID,
                        uint     *branchCount,
                        uint    **oobCount,
                        uint   ***complementCount,
                        uint    **xReleaseCount,
                        uint   ***xReleaseIDArray,
                        uint     *treeID,
                        uint     *nodeID,
                        uint     *xReleaseID,
                        void     *oobCT,
                        void     *releaseStat) {
  uint  b,j,k,m,p;
  uint      row;
  Terminal *parent;
  row = 0;
  if(RF_rNonFactorCount > 0) {
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
          ((uint *) oobCT)[row] = oobCount[b][j];
          for(p = 1; p <= RF_rNonFactorCount; p++) {
            oobResponse = (parent -> oobMeanResponse)[p];
            compResponse = (parent -> compMeanResponse)[k][p];
            meanResponse = 0.0;
            if (!(VP_opt & (VP_OPT_CMP | VP_OPT_OOB))) {
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
            ((double **) releaseStat)[p][row] = meanResponse;
          }            
        }
      }
    }
  }
  else if (RF_rFactorCount > 0) {
    for(b = 1; b <= strengthTreeCount; b++) {
      for(j = 1; j <= branchCount[b]; j++) {
        parent = (Terminal *) RF_tTermList[strengthTreeID[b]][branchID[b][j]];
        double *oobProb = dvector(1, RF_rFactorSize[1]);
        for(m = 1; m <= RF_rFactorSize[1]; m++) {
          if(oobCount[b][j] == 0) {
            oobProb[m] = RF_nativeNaN;
          }
          else {
            oobProb[m] =  (double) parent -> oobMCP[1][m] / oobCount[b][j];
          }
        }
        for(k = 1; k <= xReleaseCount[b][j]; k++) {
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
          if (!(VP_opt & (VP_OPT_CMP | VP_OPT_OOB))) {
            if((complementCount[b][j][k] != 0) && (oobCount[b][j] != 0)) {
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
          row++;
          treeID[row]      = strengthTreeID[b];
          nodeID[row]      = branchID[b][j];
          xReleaseID[row]  = xReleaseIDArray[b][j][k];
          ((uint ***) oobCT)[1][1][row]         = oobCount[b][j];
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
          if (!(VP_opt & (VP_OPT_CMP | VP_OPT_OOB))) {
            if((complementCount[b][j][k] != 0) && (oobCount[b][j] != 0)) {
              for (m = 1; m <= RF_rFactorSize[1]; m++) {                                    
                if(m == maxClass) {
                  ((double ***) releaseStat)[1][m + 1][row] = diffProb[m];
                }
                else {
                  ((double ***) releaseStat)[1][m + 1][row] = 0.0;
                }
                ((uint ***) oobCT)[1][m + 1][row] = (parent -> oobMCP)[1][m];            
              }
            }
            else {
              for (m = 1; m <= RF_rFactorSize[1]; m++) {                                    
                ((double ***) releaseStat)[1][m + 1][row] = RF_nativeNaN;
                ((uint ***) oobCT)[1][m + 1][row] = (parent -> oobMCP)[1][m];
              }
            }
          }
          else if ((VP_opt & VP_OPT_CMP) && !(VP_opt & VP_OPT_OOB)) {
            for (m = 1; m <= RF_rFactorSize[1]; m++) {                                    
              ((double ***) releaseStat)[1][m + 1][row] = compProb[m];
              ((uint ***) oobCT)[1][m + 1][row] = (parent -> oobMCP)[1][m];
            }
          }
          else if ((VP_opt & VP_OPT_OOB) && !(VP_opt & VP_OPT_CMP)) {
            for (m = 1; m <= RF_rFactorSize[1]; m++) {                                    
              ((double ***) releaseStat)[1][m + 1][row] = oobProb[m];
              ((uint ***) oobCT)[1][m + 1][row] = (parent -> oobMCP)[1][m];
            }
          }
          free_dvector(compProb, 1, RF_rFactorSize[1]);
          free_dvector(diffProb, 1, RF_rFactorSize[1]);
        }
        free_dvector(oobProb, 1, RF_rFactorSize[1]);
      }
    }
  }
}
