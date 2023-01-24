
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "termBaseOps.h"
#include "nrutil.h"
#include "error.h"
TerminalBase *makeTerminalBase(void ) {
  TerminalBase *parent = (TerminalBase*) gblock((size_t) sizeof(TerminalBase));
  preInitTerminalBase(parent);
  return parent;
}
void preInitTerminalBase(TerminalBase *parent) {
  parent -> nodeID       = 0;
  parent -> mate         = NULL;
  parent -> eTypeSize    = 0;
  parent -> mTimeSize    = 0;
  parent -> sTimeSize    = 0;
  parent -> rnfCount     = 0;
  parent -> rfCount      = 0;
  parent -> meanResponse = NULL;
  parent -> rfSize         = NULL;
  parent -> multiClassProb = NULL;
  parent -> maxClass       = NULL;
  parent -> survival       = NULL;
  parent -> nelsonAalen    = NULL;
  parent -> csh            = NULL;
  parent -> cif            = NULL;
  parent -> mortality      = NULL;
  parent -> membrCount   = 0;
  parent -> outcome      = NULL;
}
void initTerminalBase(TerminalBase *parent,
                      uint         eTypeSize,
                      uint         mTimeSize,
                      uint         sTimeSize,
                      uint         rnfCount,
                      uint         rfCount) {
  parent -> eTypeSize    = eTypeSize;
  parent -> mTimeSize    = mTimeSize;
  parent -> sTimeSize    = sTimeSize;
  parent -> rnfCount     = rnfCount;
  parent -> rfCount      = rfCount;
}
void deinitTerminalBase(TerminalBase *parent) {
  if ((parent -> rfCount > 0) || (parent -> rnfCount > 0)) {
    freeTerminalBaseNonSurvivalStructures(parent);
  }
  if (parent -> sTimeSize > 0) {
    freeTerminalBaseSurvivalStructures(parent);
  }
}
void freeTerminalBase(TerminalBase *parent) {
  deinitTerminalBase(parent);
  free_gblock(parent, (size_t) sizeof(TerminalBase));
}
void freeTerminalBaseNonSurvivalStructures(TerminalBase *tTerm) {
  unstackMeanResponse(tTerm);
  unstackMultiClassProb(tTerm);
}
void freeTerminalBaseSurvivalStructures(TerminalBase *tTerm) {
  unstackSurvival(tTerm);
  unstackNelsonAalen(tTerm);
  unstackCSH(tTerm);
  unstackCIF(tTerm);
  unstackMortality(tTerm);
}
void stackMeanResponse(TerminalBase *tTerm) {
  if (tTerm -> rnfCount > 0) {
    tTerm -> meanResponse = dvector(1, tTerm -> rnfCount);
  }
}
void unstackMeanResponse(TerminalBase *tTerm) {
  if (tTerm -> rnfCount > 0) {
    if (tTerm -> meanResponse != NULL) {
      free_dvector(tTerm -> meanResponse, 1, tTerm -> rnfCount);
      tTerm -> meanResponse = NULL;
    }
  }
}
void stackMultiClassProb(TerminalBase *tTerm, unsigned int *rfSize) {
  unsigned int j;
  if (tTerm -> rfCount) {
    tTerm -> rfSize = uivector(1, tTerm -> rfCount);
    tTerm -> multiClassProb = (unsigned int **) new_vvector(1, tTerm -> rfCount, NRUTIL_UPTR);
    for (j = 1; j <= tTerm -> rfCount; j++) {
      (tTerm -> rfSize)[j] = rfSize[j];
      (tTerm -> multiClassProb)[j] = uivector(1, (tTerm -> rfSize)[j]);
    }
    tTerm -> maxClass = dvector(1, tTerm -> rfCount);
  }
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
void stackSurvival(TerminalBase *tTerm) {
  if (tTerm -> sTimeSize > 0) {
    tTerm -> survival = dvector(1, tTerm -> sTimeSize);
  }
}
void unstackSurvival(TerminalBase *tTerm) {
  if(tTerm -> sTimeSize > 0) {
    if (tTerm -> survival != NULL) {
      free_dvector(tTerm -> survival, 1, tTerm -> sTimeSize);
      tTerm -> survival = NULL;
    }
  }
}
void stackNelsonAalen(TerminalBase *tTerm) {
  if (tTerm -> sTimeSize > 0) {
    tTerm -> nelsonAalen = dvector(1, tTerm -> sTimeSize);
  }
}
void unstackNelsonAalen(TerminalBase *tTerm) {
  if(tTerm -> sTimeSize > 0) {
    if (tTerm -> nelsonAalen != NULL) {
      free_dvector(tTerm -> nelsonAalen, 1, tTerm -> sTimeSize);
      tTerm -> nelsonAalen = NULL;
    }
  }
}
void stackCSH(TerminalBase *tTerm) {
  if (tTerm -> eTypeSize > 1) {
    tTerm -> csh = dmatrix(1, tTerm -> eTypeSize, 1, tTerm -> sTimeSize);
  }
}
void unstackCSH(TerminalBase *tTerm) {
  if(tTerm -> eTypeSize > 1) {
    if (tTerm -> csh != NULL) {
      free_dmatrix(tTerm -> csh, 1, tTerm -> eTypeSize, 1, tTerm -> sTimeSize);
      tTerm -> csh = NULL;
    }
  }
}
void stackCIF(TerminalBase *tTerm) {
  if (tTerm -> eTypeSize > 1) {
    tTerm -> cif = dmatrix(1, tTerm -> eTypeSize, 1, tTerm -> sTimeSize);
  }
}
void unstackCIF(TerminalBase *tTerm) {
  if(tTerm -> eTypeSize > 1) {
    if (tTerm -> cif != NULL) {
      free_dmatrix(tTerm -> cif, 1, tTerm -> eTypeSize, 1, tTerm -> sTimeSize);
      tTerm -> cif = NULL;
    }
  }
}
void stackMortality(TerminalBase *tTerm) {
  if (tTerm -> eTypeSize > 0) {
    tTerm -> mortality = dvector(1, tTerm -> eTypeSize);
  }
}
void unstackMortality(TerminalBase *tTerm) {
  if(tTerm -> eTypeSize > 0) {
    if (tTerm -> mortality != NULL) {
      free_dvector(tTerm -> mortality, 1, tTerm -> eTypeSize);
      tTerm -> mortality = NULL;
    }
  }
}
