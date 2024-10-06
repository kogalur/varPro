
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "termOps.h"
#include "shared/termBaseOps.h"
#include "shared/classification.h"
#include "shared/regression.h"
#include "shared/survival.h"
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
  parent -> complementMortality = NULL;
  parent -> oobMortality        = 0;
  return parent;
}
void freeTerminalDerived(void *parent) {
  if (((TerminalBase *) parent) -> survivalBase != NULL) {
    unstackMortalityTerm((Terminal *) parent);
  }
  else if (((TerminalBase *) parent) -> regressionBase != NULL) {  
    unstackMeanResponseTerm((Terminal *) parent);
  }
  else if(((TerminalBase *) parent) -> classificationBase != NULL) {
    unstackMultiClassTerm((Terminal *) parent);
  }
  deinitTerminalBase((TerminalBase*) parent);
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
  TerminalRegression *parent;
  parent = ((TerminalBase *) tTerm) -> regressionBase;
  if (parent != NULL) {
    if (parent -> rnfCount > 0) {
      (tTerm -> compMeanResponse)[xReleaseIndx] = dvector(1, parent -> rnfCount);
    }
  }
}
void stackOobMeanResponse(Terminal *tTerm) {
  TerminalRegression *parent;
  parent = ((TerminalBase *) tTerm) -> regressionBase;
  if (parent != NULL) {
    if (parent -> rnfCount > 0) {
      tTerm -> oobMeanResponse = dvector(1, parent -> rnfCount);
    }
  }
}
void unstackMeanResponseTerm(Terminal *tTerm) {
  TerminalRegression *parent;
  uint k;
  parent = ((TerminalBase *) tTerm) -> regressionBase;
  if (tTerm -> oobMeanResponse != NULL) {
    free_dvector(tTerm -> oobMeanResponse, 1, parent -> rnfCount);
    tTerm -> oobMeanResponse = NULL;
  }
  for(k = 1; k <= (tTerm -> xReleaseCount); k++) {
    if(tTerm -> compMeanResponse[k] != NULL) {
      free_dvector(tTerm -> compMeanResponse[k], 1, parent -> rnfCount);
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
  TerminalClassification *parent;
  unsigned int j;
  parent = ((TerminalBase *) tTerm) -> classificationBase;
  if (parent != NULL) {
    if (parent -> rfCount > 0) {
      (tTerm -> complementMCP)[xReleaseIndx] = (unsigned int **) new_vvector(1, parent -> rfCount, NRUTIL_UPTR);
      for (j = 1; j <= parent -> rfCount; j++) {
        (tTerm -> complementMCP)[xReleaseIndx][j] = uivector(1, parent -> rfSize[j]);
      }
      (tTerm -> complementMaxClass)[xReleaseIndx] = dvector(1, parent -> rfCount);
    }
  }
}
void stackOobMultiClass(Terminal *tTerm) {
  TerminalClassification *parent;
  unsigned int j;
  parent = ((TerminalBase *) tTerm) -> classificationBase;
  if (parent != NULL) {
    if (parent -> rfCount > 0) {
      tTerm -> oobMCP = (unsigned int **) new_vvector(1, parent -> rfCount, NRUTIL_UPTR);
      for (j = 1; j <= parent -> rfCount; j++) {
        (tTerm -> oobMCP)[j] = uivector(1, parent -> rfSize[j]);
      }
      (tTerm -> oobMaxClass) = dvector(1, parent ->rfCount);
    }
  }
}
void unstackMultiClassTerm(Terminal *tTerm) {
  TerminalClassification *parent;
  unsigned int j, i;
  parent = ((TerminalBase *) tTerm) -> classificationBase;
  if (tTerm -> complementMCP != NULL) {
    for (i = 1; i <= tTerm -> xReleaseCount; i++) {
      if (tTerm -> complementMCP[i] != NULL) {      
        for (j = 1; j <= parent -> rfCount; j++) {
          free_uivector(tTerm -> complementMCP[i][j], 1, parent -> rfSize[j]);
          tTerm -> complementMCP[i][j] = NULL;
        }
        free_new_vvector(tTerm -> complementMCP[i], 1, parent -> rfCount, NRUTIL_UPTR);
        tTerm -> complementMCP[i] = NULL;
      }
    }
    free_new_vvector(tTerm -> complementMCP, 1, tTerm -> xReleaseCount, NRUTIL_UPTR2);
    tTerm -> complementMCP = NULL;
    if (tTerm -> oobMCP != NULL) {
      for (j = 1; j <= parent -> rfCount; j++) {
        free_uivector(tTerm -> oobMCP[j], 1, parent -> rfSize[j]);
        tTerm -> oobMCP[j] = NULL;
      }
      free_new_vvector(tTerm -> oobMCP, 1, parent -> rfCount, NRUTIL_UPTR);
      tTerm -> oobMCP = NULL;
    }
  }
  if (tTerm -> complementMaxClass != NULL) {
    for (i = 1; i <= tTerm -> xReleaseCount; i++) {
      if(tTerm -> complementMaxClass[i] != NULL) {
        free_dvector(tTerm -> complementMaxClass[i], 1, parent -> rfCount);
        tTerm -> complementMaxClass[i] = NULL;
      }
    }
    free_new_vvector(tTerm -> complementMaxClass, 1, tTerm -> xReleaseCount, NRUTIL_DPTR);
    tTerm -> complementMaxClass = NULL;
  }
  if (tTerm -> oobMaxClass != NULL) {
    free_dvector(tTerm -> oobMaxClass, 1, parent -> rfCount);
    tTerm -> oobMaxClass = NULL;
  }
}
void stackCompMortalityOuter(Terminal *tTerm, unsigned int xReleaseCount) {
  TerminalSurvival *parent;
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
  parent = ((TerminalBase *) tTerm) -> survivalBase;
  if (parent != NULL) {
    tTerm -> complementMortality = dvector(1, xReleaseCount);
  }
}
void unstackMortalityTerm(Terminal *tTerm) {
  TerminalSurvival *parent;
  parent = ((TerminalBase *) tTerm) -> survivalBase;
  if (parent != NULL) {
    free_dvector(tTerm -> complementMortality, 1, tTerm -> xReleaseCount);
  }
}
void restoreTerminalNodeOutcomesVarPro(uint treeID, Terminal *term) {
  TerminalBase *parent;
  parent = (TerminalBase *) term;
  if (FALSE) {
    if (parent -> classificationBase != NULL) {
      assignAllClassificationOutcomes(0, treeID, parent);
    }
    else if (parent -> regressionBase != NULL) {
      assignAllRegressionOutcomes(0, treeID, parent);
    }
    else if (parent -> survivalBase != NULL) {
      assignAllSurvivalOutcomes(0, treeID, parent);
    }
    else if (parent -> competingRiskBase != NULL) {
      assignAllCompetingRiskOutcomes(0, treeID, parent);
    }
  }
}
