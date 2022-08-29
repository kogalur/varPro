
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
#include           "trace.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "splitInfo.h"
#include "nrutil.h"
SplitInfo *makeSplitInfo(uint size) {
  SplitInfo *info = (SplitInfo*) gblock((size_t) sizeof(SplitInfo));
  info -> size = size;
  if (size > 0) {
   info -> indicator = cvector(1, size);
  }
  else {
    info -> indicator = NULL;
  }
  info -> hcDim          = 0;
  info -> mwcpSizeAbs    = NULL;
  info -> randomVar      = NULL;
  info -> randomPts      = NULL;
  info -> randomPtsRight = NULL;
  info -> pairCT         = 0;
  info -> augmX1         = NULL;
  info -> augmX2         = NULL;
  info -> augmXS         = NULL;
  info -> sythFlag = FALSE;
  info -> timeCutLeft    = RF_nativeNaN;
  info -> timeCutRight   = RF_nativeNaN;
  info -> splitRank      = 0;
  return info;
}
void freeSplitInfo(SplitInfo *info) {
  uint adj;
  uint j;
  if (info -> size > 0) {
    if(info -> indicator != NULL) {
      free_cvector(info -> indicator, 1, info -> size);
    }
  }
  if (info -> hcDim == 0) {
    adj = 1;
  }
  else {
    adj = info -> hcDim;
  }
  if (info -> mwcpSizeAbs != NULL) {
    for (j = 1; j <= adj; j++) {
      if (info -> mwcpSizeAbs[j] > 0) {
        free_uivector((uint *) ((info -> randomPts)[j]), 1, info -> mwcpSizeAbs[j]);
      }
      else {
        free_dvector((double *) ((info -> randomPts)[j]), 1, 1);
        if (info -> hcDim > 0) {
          free_dvector((double *) ((info -> randomPtsRight)[j]), 1, 1);
        }
      }
    }
    free_uivector(info -> mwcpSizeAbs, 1, adj);
    free_ivector(info -> randomVar, 1, adj);
    free_new_vvector(info -> randomPts, 1, adj, NRUTIL_VPTR);
    if (info -> hcDim > 0) {
      free_new_vvector(info -> randomPtsRight, 1, adj, NRUTIL_VPTR);
    }
    if (info -> augmX1 != NULL) free_ivector(info -> augmX1, 1, adj);
    if (info -> augmX2 != NULL) free_ivector(info -> augmX2, 1, adj);
    if (info -> augmXS != NULL) free_ivector(info -> augmXS, 1, adj);
  }
  free_gblock(info, (size_t) sizeof(SplitInfo));
}
SplitInfoMax *makeSplitInfoMax(uint size) {
  SplitInfoMax *info = (SplitInfoMax*) gblock((size_t) sizeof(SplitInfoMax));
  info -> size = size;
  if (size > 0) {
   info -> indicator = cvector(1, size);
  }
  else {
    info -> indicator = NULL;
  }
  info -> deltaMax              = RF_nativeNaN;
  info -> splitParameterMax     = 0;
  info -> splitValueMaxCont     = RF_nativeNaN;
  info -> splitValueMaxFactSize = 0;
  info -> splitValueMaxFactPtr  = NULL;
  info -> splitAugmMaxPairOne   = 0;
  info -> splitAugmMaxPairTwo   = 0;
  info -> splitAugmMaxSyth      = 0;
  info -> splitRank             = 0;
  info -> splitStatistic        = RF_nativeNaN;
  return info;
}
void freeSplitInfoMax(SplitInfoMax *info) {
  if (info -> size > 0) {
    if(info -> indicator != NULL) {
      free_cvector(info -> indicator, 1, info -> size);
    }
  }
  if (info -> splitValueMaxFactSize > 0) {
    free_uivector(info -> splitValueMaxFactPtr, 1, info -> splitValueMaxFactSize);
  }
  free_gblock(info, (size_t) sizeof(SplitInfoMax));
}
