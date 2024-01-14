
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "termBaseOps.h"
#include "classification.h"
#include "regression.h"
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
  parent -> rfCount      = 0;
  parent -> rfSize       = 0;
  parent -> rfIndex      = NULL;
  parent -> rnfCount     = 0;
  parent -> rnfIndex     = NULL;
  parent -> meanResponse = NULL;
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
                      uint  eTypeSize,
                      uint  mTimeSize,
                      uint  sTimeSize,
                      uint  rnfCount,
                      uint  rfCount,
                      uint *rfSize,
                      uint *rfIndex,
                      uint *rnfIndex) {
  parent -> eTypeSize    = eTypeSize;
  parent -> mTimeSize    = mTimeSize;
  parent -> sTimeSize    = sTimeSize;
  parent -> rnfCount     = rnfCount;
  parent -> rfCount      = rfCount;
  parent -> rfSize       = rfSize;
  parent -> rfIndex      = rfIndex;
  parent -> rnfIndex     = rnfIndex;
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
void stackMultiClassProb(TerminalBase *tTerm) {
  unsigned int j;
  if (tTerm -> rfCount) {
    tTerm -> multiClassProb = (unsigned int **) new_vvector(1, tTerm -> rfCount, NRUTIL_UPTR);
    for (j = 1; j <= tTerm -> rfCount; j++) {
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
void updateTerminalNodeOutcomes(uint treeID, TerminalBase  *parent) {
  uint      *repMembrIndx;
  uint       repMembrSize;
  uint      *allMembrIndx;
  uint       allMembrSize;
  uint i;
  repMembrIndx = parent -> mate -> repMembrIndx;
  repMembrSize = parent -> mate -> repMembrSize;
  allMembrIndx = parent -> mate -> allMembrIndx;
  allMembrSize = parent -> mate -> allMembrSize;
  for (i = 1; i <= allMembrSize; i++) {
    RF_tTermMembership[treeID][allMembrIndx[i]] = parent;
  }
  if ((RF_opt & OPT_PERF) ||
      (RF_opt & OPT_OENS) ||
      (RF_opt & OPT_IENS) ||
      (RF_opt & OPT_FENS)) {
    if (RF_rFactorCount > 0) {
      calculateMultiClassProb(treeID, parent, repMembrIndx, repMembrSize, NULL);
    }
    if (RF_rNonFactorCount > 0) {
      calculateMeanResponse(treeID, parent, repMembrIndx, repMembrSize, NULL);
    }
  }
  else {
    getMembrCountOnly(treeID, parent, repMembrIndx, repMembrSize, allMembrIndx, allMembrSize);
  }
}
void getMembrCountOnly (uint       treeID,
                        TerminalBase  *parent,
                        uint      *repMembrIndx,
                        uint       repMembrSize,
                        uint      *genMembrIndx,
                        uint       genMembrSize) {
  if ( !(RF_opt & OPT_BOOT_TYP1) && (RF_opt & OPT_BOOT_TYP2) ) {
    parent -> membrCount = genMembrSize;
  }
  else {
    parent -> membrCount = repMembrSize;
  }
  if ((parent -> membrCount) == 0) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Zero node count encountered in (tree, leaf) = (%10d, %10d)  \n", treeID, parent -> nodeID);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  }
}
