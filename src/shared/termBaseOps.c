
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
#include           "trace.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "termBaseOps.h"
#include "nrutil.h"
#include "error.h"
TerminalBase *makeTerminalBase() {
  TerminalBase *parent = (TerminalBase*) gblock((size_t) sizeof(TerminalBase));
  initTerminalBase(parent);
  return parent;
}
void initTerminalBase(TerminalBase *parent) {
  parent -> nodeID       = 0;
  parent -> mate         = NULL;
  parent -> rnfCount     = 0;
  parent -> meanResponse = NULL;
  parent -> membrCount   = 0;
  parent -> outcome      = NULL;
  parent -> rfCount              = 0;
  parent -> rfSize               = NULL;
  parent -> multiClassProb       = NULL;
  parent -> maxClass             = NULL;
}
void deinitTerminalBase(TerminalBase *parent) {
  freeTerminalBaseNonSurvivalStructures(parent);
}
void freeTerminalBase(TerminalBase *parent) {
  deinitTerminalBase(parent);
  free_gblock(parent, (size_t) sizeof(TerminalBase));
}
void freeTerminalBaseNonSurvivalStructures(TerminalBase *tTerm) {
  unstackMeanResponse(tTerm);
  unstackMultiClassProb(tTerm);
}
void stackMeanResponse(TerminalBase *tTerm, unsigned int rnfCount) {
  if (tTerm -> rnfCount > 0) {
    if (tTerm -> rnfCount != rnfCount) {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  rnfCount has been previously defined:  %10d vs %10d", tTerm -> rnfCount, rnfCount);
      RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
      RF_nativeExit();
    }
  }
  else {
    tTerm -> rnfCount = rnfCount;
  }
  tTerm -> meanResponse = dvector(1, tTerm -> rnfCount);
}
void unstackMeanResponse(TerminalBase *tTerm) {
  if (tTerm -> rnfCount > 0) {
    if (tTerm -> meanResponse != NULL) {
      free_dvector(tTerm -> meanResponse, 1, tTerm -> rnfCount);
      tTerm -> meanResponse = NULL;
    }
  }
}
void stackMultiClassProb(TerminalBase *tTerm, unsigned int rfCount, unsigned int *rfSize) {
  unsigned int j;
  if (tTerm -> rfCount > 0) {
    if (tTerm -> rfCount != rfCount) {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  rfCount has been previously defined:  %10d vs %10d", tTerm -> rfCount, rfCount);
      RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
      RF_nativeExit();
    }
  }
  else {
    tTerm -> rfCount = rfCount;
  }
  tTerm -> rfSize = uivector(1, tTerm -> rfCount);
  tTerm -> multiClassProb = (unsigned int **) new_vvector(1, tTerm -> rfCount, NRUTIL_UPTR);
  for (j = 1; j <= tTerm -> rfCount; j++) {
    (tTerm -> rfSize)[j] = rfSize[j];
    (tTerm -> multiClassProb)[j] = uivector(1, (tTerm -> rfSize)[j]);
  }
  tTerm -> maxClass = dvector(1, tTerm -> rfCount);
}
void unstackMultiClassProb(TerminalBase *tTerm) {
  unsigned int j;
  if (tTerm -> rfCount > 0) {
    if (tTerm -> rfSize != NULL) {
      if (tTerm -> multiClassProb != NULL) {
        for (j = 1; j <= tTerm -> rfCount; j++) {
          if (tTerm -> multiClassProb[j] != NULL) {
            free_uivector(tTerm -> multiClassProb[j], 1, tTerm -> rfSize[j]);
            tTerm -> multiClassProb[j] = NULL;
          }
        }
        free_new_vvector(tTerm -> multiClassProb, 1, tTerm -> rfCount, NRUTIL_UPTR);
        tTerm -> multiClassProb = NULL;
      }
      free_uivector(tTerm -> rfSize, 1, tTerm -> rfCount);
      tTerm -> rfSize = NULL;
    }
  }
  if (tTerm -> rfCount > 0) {
    if (tTerm -> maxClass != NULL) {
      free_dvector(tTerm -> maxClass, 1, tTerm -> rfCount);
      tTerm -> maxClass = NULL;
    }
  }
}
