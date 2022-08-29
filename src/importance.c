
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "shared/trace.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "importance.h"
#include "terminal.h"
#include "termOps.h"
#include "shared/nrutil.h"
void getMeanResponse(uint       treeID,
                     Terminal  *parent,
                     uint      *membershipIndex,
                     uint       membershipSize,
                     uint       xReleaseIndx,
                     char       oob) {
  uint    i, j;
  double *mean;
  mean = dvector(1, RF_rNonFactorCount);
  if(oob) {
    stackOobMeanResponse(parent, RF_rNonFactorCount);
  }
  else {
    stackCompMeanResponseInner(parent, xReleaseIndx, RF_rNonFactorCount);
  }
  if (membershipSize == 0) {
    for (j = 1; j <= RF_rNonFactorCount; j++) {
      mean[j] = RF_nativeNaN;
    }
  }
  else {
    for (j=1; j <= RF_rNonFactorCount; j++) {
      mean[j] = 0.0;
    }
    for (i = 1; i <= membershipSize; i++) {
      for (j = 1; j <= RF_rNonFactorCount; j++) {
        mean[j] += RF_response[treeID][RF_rNonFactorIndex[j]][membershipIndex[i]];
      }
    }
    if (membershipSize > 0) {
      for (j = 1; j <= RF_rNonFactorCount; j++) {
        mean[j] = mean[j] / (double) membershipSize;        
      }
    }
  }
  if(oob) {
    for (j=1; j <= RF_rNonFactorCount; j++) {
      (parent -> oobMeanResponse)[j] = mean[j];
    }
  }
  else {
    for (j=1; j <= RF_rNonFactorCount; j++) {
      (parent -> compMeanResponse[xReleaseIndx])[j] = mean[j];
    }
  }
  free_dvector(mean, 1, RF_rNonFactorCount);
}
void getMultiClassProb (uint       treeID,
                        Terminal  *parent,
                        uint      *membershipIndex,
                        uint       membershipSize,
                        uint       xReleaseIndx,
                        char       oob) {
  uint i, j, k;
  double maxValue, maxClass;
  uint **mcp;
  mcp = (uint **)  new_vvector(1, RF_rFactorCount , NRUTIL_UPTR);
  for (j=1; j <= RF_rFactorCount; j++) {
    mcp[j] = uivector(1, RF_rFactorSize[j]);
  }
  if(oob) {
    stackOobMultiClass(parent, RF_rFactorCount, RF_rFactorSize);
  }
  else {
    stackCompMultiClassInner(parent, xReleaseIndx, RF_rFactorCount, RF_rFactorSize);
  }
  if (membershipSize == 0) {
    for (j=1; j <= RF_rFactorCount; j++) {
      for (k=1; k <= RF_rFactorSize[j]; k++) {
        mcp[j][k] = 0;
      }
    }
  }
  else {
    for (j=1; j <= RF_rFactorCount; j++) {
      for (k=1; k <= RF_rFactorSize[j]; k++) {
        mcp[j][k] = 0;
      }
    }
    for (i = 1; i <= membershipSize; i++) {
      for (j=1; j <= RF_rFactorCount; j++) {
        mcp[j][(uint) RF_response[treeID][RF_rFactorIndex[j]][membershipIndex[i]]] ++;
      }
    }
  }
  if(oob) {
    for (j=1; j <= RF_rFactorCount; j++) {
      maxValue = 0;
      maxClass = 0;
      for (k=1; k <= RF_rFactorSize[j]; k++) {
        (parent -> oobMCP)[j][k] = mcp[j][k];
        if (maxValue < (double) (parent -> oobMCP[j][k])) {
          maxValue = (double) parent -> oobMCP[j][k];
          maxClass = (double) k;
        }
      }
      (parent -> oobMaxClass)[j] = maxClass;
    }
  }
  else {
    for (j=1; j <= RF_rFactorCount; j++) {
      maxValue = 0;
      maxClass = 0;
      for (k=1; k <= RF_rFactorSize[j]; k++) {
        (parent -> complementMCP)[xReleaseIndx][j][k] = mcp[j][k];
        if (maxValue < (double) (parent -> complementMCP[xReleaseIndx][j][k])) {
          maxValue = (double) parent -> complementMCP[xReleaseIndx][j][k];
          maxClass = (double) k;
        }
      }
      (parent -> complementMaxClass)[xReleaseIndx][j] = maxClass;
    }
  }
  for (j=1; j <= RF_rFactorCount; j++) {
    free_uivector(mcp[j], 1, RF_rFactorSize[j]);
  }
  free_new_vvector(mcp, 1, RF_rFactorCount , NRUTIL_UPTR);
}
