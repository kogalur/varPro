
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "splitInfo.h"
#include "nrutil.h"
SplitInfo *makeSplitInfo(uint size) {
  SplitInfo *info = (SplitInfo *) gblock((size_t) sizeof(SplitInfo));
  initSplitInfo(info, size);
  return info;
}
void freeSplitInfo(SplitInfo *info) {
  deinitSplitInfo(info);
  free_gblock(info, (size_t) sizeof(SplitInfo));
}
void initSplitInfo(SplitInfo *info, uint size) {
  info -> size = size;
  if (size > 0) {
   info -> indicator = cvector(1, size);
  }
  else {
    info -> indicator = NULL;
  }
  info -> mwcpSizeAbs    = NULL;
  info -> randomVar      = NULL;
  info -> randomPts      = NULL;
}
void deinitSplitInfo(SplitInfo *info) {
  if (info -> size > 0) {
    if(info -> indicator != NULL) {
      free_cvector(info -> indicator, 1, info -> size);
    }
  }
  if (info -> mwcpSizeAbs != NULL) {
    if (info -> mwcpSizeAbs[1] > 0) {
      free_uivector((uint *) ((info -> randomPts)[1]), 1, info -> mwcpSizeAbs[1]);
    }
    else {
      free_dvector((double *) ((info -> randomPts)[1]), 1, 1);
    }
    free_uivector(info -> mwcpSizeAbs, 1, 1);
    free_ivector(info -> randomVar, 1, 1);
    free_new_vvector(info -> randomPts, 1, 1, NRUTIL_VPTR);
  }
}
SplitInfoMax *makeSplitInfoMax(uint size) {
  SplitInfoMax *info = (SplitInfoMax*) gblock((size_t) sizeof(SplitInfoMax));
  initSplitInfoMax(info, size);
  return info;
}
void freeSplitInfoMax(SplitInfoMax *info) {
  deinitSplitInfoMax(info);
  free_gblock(info, (size_t) sizeof(SplitInfoMax));
}
void initSplitInfoMax(SplitInfoMax *info, uint size) {
  info -> size = size;
  if (size > 0) {
   info -> indicator = cvector(1, size);
  }
  else {
    info -> indicator = NULL;
  }
  info -> delta              = RF_nativeNaN;
  info -> splitParameter     = 0;
  info -> splitValueCont     = RF_nativeNaN;
  info -> splitValueFactSize = 0;
  info -> splitValueFactPtr  = NULL;
  info -> splitStatistic        = RF_nativeNaN;
}
void deinitSplitInfoMax(SplitInfoMax *info) {
  if (info -> size > 0) {
    if(info -> indicator != NULL) {
      free_cvector(info -> indicator, 1, info -> size);
    }
  }
  if (info -> splitValueFactSize > 0) {
    free_uivector(info -> splitValueFactPtr, 1, info -> splitValueFactSize);
  }
}
