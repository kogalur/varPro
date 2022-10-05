
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "shared/trace.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "termOps.h"
#include "shared/termBaseOps.h"
#include "shared/nrutil.h"
#include "terminal.h"
#include "shared/error.h"
void *makeTerminalDerived() {
  Terminal *parent = (Terminal*) gblock((size_t) sizeof(Terminal));
  preInitTerminalBase((TerminalBase *) parent);
  parent -> xReleaseCount     = 0;
  parent -> compMeanResponse  = NULL;
  parent -> oobMeanResponse   = NULL;
  parent -> complementMaxClass = NULL;
  parent -> oobMaxClass        = NULL;
  parent -> complementMCP      = NULL;
  parent -> oobMCP             = NULL;
  return parent;
}
void freeTerminalDerived(void *parent) {
  if(((TerminalBase *) parent) -> rnfCount > 0) {
    unstackMeanResponseTerm(parent);
  }
  else if(((TerminalBase *) parent) -> rfCount > 0) {
    unstackMultiClassTerm(parent);
  }
  deinitTerminalBase((TerminalBase *) parent);
  free_gblock(parent, (size_t) sizeof(Terminal));
}
void stackCompMeanResponseOuter(Terminal *tTerm, unsigned int xReleaseCount) {
  uint k;
  if (tTerm -> xReleaseCount > 0) {
    if (tTerm -> xReleaseCount != xReleaseCount) {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  xReleaseCount has been previously defined:  %10d vs %10d", tTerm -> xReleaseCount, xReleaseCount);
      RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
      RF_nativeExit();
    }
  }
  else {
    tTerm -> xReleaseCount = xReleaseCount;
  }
  tTerm -> compMeanResponse = (double **)  new_vvector(1, tTerm -> xReleaseCount, NRUTIL_DPTR);
  for(k = 1; k <= (tTerm -> xReleaseCount); k++) {
    tTerm -> compMeanResponse[k] = NULL;
  }
}
void stackCompMeanResponseInner(Terminal *tTerm, unsigned int xReleaseIndx) {
  if (((TerminalBase *) tTerm) -> rnfCount > 0) {
    (tTerm -> compMeanResponse)[xReleaseIndx] = dvector(1, ((TerminalBase  *) tTerm) -> rnfCount);
  }
}
void stackOobMeanResponse(Terminal *tTerm) {
  if (((TerminalBase  *) tTerm) -> rnfCount > 0) {
    tTerm -> oobMeanResponse = dvector(1, ((TerminalBase  *) tTerm) -> rnfCount);
  }
}
void unstackMeanResponseTerm(Terminal *tTerm) {
  uint k;
  if (tTerm -> oobMeanResponse != NULL) {
    free_dvector(tTerm -> oobMeanResponse, 1, ((TerminalBase  *) tTerm) -> rnfCount);
    tTerm -> oobMeanResponse = NULL;
  }
  for(k = 1; k <= (tTerm -> xReleaseCount); k++) {
    if(tTerm -> compMeanResponse[k] != NULL) {
      free_dvector(tTerm -> compMeanResponse[k], 1, ((TerminalBase  *) tTerm) -> rnfCount);
      tTerm -> compMeanResponse[k] = NULL;
    }
  }
  if (tTerm -> compMeanResponse != NULL) {
    free_new_vvector(tTerm -> compMeanResponse, 1, tTerm -> xReleaseCount, NRUTIL_DPTR);
    tTerm -> compMeanResponse = NULL;
  }
}
void stackCompMultiClassOuter(Terminal *tTerm, unsigned int xReleaseCount) {
  uint k;
  if (tTerm -> xReleaseCount > 0) {
    if (tTerm -> xReleaseCount != xReleaseCount) {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  xReleaseCount has been previously defined:  %10d vs %10d", tTerm -> xReleaseCount, xReleaseCount);
      RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
      RF_nativeExit();
    }
  }
  else {
    tTerm -> xReleaseCount = xReleaseCount;
  }
  tTerm -> complementMCP = (unsigned int ***) new_vvector(1, tTerm -> xReleaseCount, NRUTIL_UPTR2);
  tTerm -> complementMaxClass = (double **) new_vvector(1, tTerm -> xReleaseCount, NRUTIL_DPTR);
  for(k = 1; k <= (tTerm -> xReleaseCount); k++) {
    tTerm -> complementMCP[k] = NULL;
    tTerm -> complementMaxClass[k] = NULL;
  }
}
void stackCompMultiClassInner(Terminal *tTerm, unsigned int xReleaseIndx) {
  unsigned int j;
  if (((TerminalBase *) tTerm) -> rfCount > 0) {
    (tTerm -> complementMCP)[xReleaseIndx] = (unsigned int **) new_vvector(1, ((TerminalBase *) tTerm) -> rfCount, NRUTIL_UPTR);
    for (j = 1; j <= ((TerminalBase *) tTerm) -> rfCount; j++) {
      (tTerm -> complementMCP)[xReleaseIndx][j] = uivector(1, (((TerminalBase *) tTerm) -> rfSize)[j]);
    }
    (tTerm -> complementMaxClass)[xReleaseIndx] = dvector(1, ((TerminalBase *) tTerm) -> rfCount);
  }
}
void stackOobMultiClass(Terminal *tTerm) {
  unsigned int j;
  if (((TerminalBase *) tTerm) -> rfCount > 0) {  
    tTerm -> oobMCP = (unsigned int **) new_vvector(1, ((TerminalBase *) tTerm) -> rfCount, NRUTIL_UPTR);
    for (j = 1; j <= ((TerminalBase *) tTerm) -> rfCount; j++) {
      (tTerm -> oobMCP)[j] = uivector(1, (((TerminalBase *) tTerm) -> rfSize)[j]);
    }
    (tTerm -> oobMaxClass) = dvector(1, ((TerminalBase *) tTerm) -> rfCount);
  }
}
void unstackMultiClassTerm(Terminal *tTerm) {
  unsigned int j, i;
  if (((TerminalBase *) tTerm) -> rfCount > 0) {
    if (((TerminalBase *) tTerm) -> rfSize != NULL) {
      if (tTerm -> complementMCP != NULL) {
        for (i = 1; i <= tTerm -> xReleaseCount; i++) {  
          for (j = 1; j <= ((TerminalBase *) tTerm) -> rfCount; j++) {
            if (tTerm -> complementMCP[i][j] != NULL) {
              free_uivector(tTerm -> complementMCP[i][j], 1, ((TerminalBase *) tTerm) -> rfSize[j]);
              tTerm -> complementMCP[i][j] = NULL;
            }
          }
          if(tTerm -> complementMCP[i] != NULL) {
            free_new_vvector(tTerm -> complementMCP[i], 1, ((TerminalBase *) tTerm) -> rfCount, NRUTIL_UPTR);
            tTerm -> complementMCP[i] = NULL;
          }
        }
        free_new_vvector(tTerm -> complementMCP, 1, tTerm -> xReleaseCount, NRUTIL_UPTR2);
        tTerm -> complementMCP = NULL;
      }
      if (tTerm -> oobMCP != NULL) {
        for (j = 1; j <= ((TerminalBase *) tTerm) -> rfCount; j++) {
          if (tTerm -> oobMCP[j] != NULL) {
            free_uivector(tTerm -> oobMCP[j], 1, ((TerminalBase *) tTerm) -> rfSize[j]);
            tTerm -> oobMCP[j] = NULL;
          }
        }
        free_new_vvector(tTerm -> oobMCP, 1, ((TerminalBase *) tTerm) -> rfCount, NRUTIL_UPTR);
        tTerm -> oobMCP = NULL;
      }
    }
  }
  if (((TerminalBase *) tTerm) -> rfCount > 0) {
    if (tTerm -> complementMaxClass != NULL) {
      for (i = 1; i <= tTerm -> xReleaseCount; i++) {
        if(tTerm -> complementMaxClass[i] != NULL) {
          free_dvector(tTerm -> complementMaxClass[i], 1, ((TerminalBase *) tTerm) -> rfCount);
          tTerm -> complementMaxClass[i] = NULL;
        }
      }
      free_new_vvector(tTerm -> complementMaxClass, 1, tTerm -> xReleaseCount, NRUTIL_DPTR);
      tTerm -> complementMaxClass = NULL;
    }
    if (tTerm -> oobMaxClass != NULL) {
      free_dvector(tTerm -> oobMaxClass, 1, ((TerminalBase *) tTerm) -> rfCount);
      tTerm -> oobMaxClass = NULL;
    }
  }
}
