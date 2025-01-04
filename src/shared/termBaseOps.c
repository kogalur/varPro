
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "termBaseOps.h"
#include "classification.h"
#include "regression.h"
#include "survival.h"
#include "nrutil.h"
#include "error.h"
TerminalBase *makeTerminalBase(void) {
  TerminalBase *parent = (TerminalBase*) gblock((size_t) sizeof(TerminalBase));
  preInitTerminalBase(parent);
  return parent;
}
void preInitTerminalBase(TerminalBase *parent) {
  parent -> nodeID       = 0;
  parent -> mate         = NULL;
  parent -> membrCount   = 0;
  parent -> outcome      = NULL;
  parent -> regressionBase     = NULL;
  parent -> classificationBase = NULL;
  parent -> survivalBase       = NULL;
  parent -> competingRiskBase  = NULL;
}
void initTerminalBase(TerminalBase *parent,
                      uint  eTypeSize,
                      uint  mTimeSize,
                      uint  aeTimeSize,
                      uint  sTimeSize,
                      uint  rnfCount,
                      uint *rnfIndex,
                      uint  rfCount,
                      uint *rfIndex,
                      uint *rfSize) {
  if (eTypeSize > 0) {
    if (eTypeSize == 1) {
      parent -> survivalBase = makeSurvivalBase(0, mTimeSize, aeTimeSize, sTimeSize);
      parent -> survivalBase -> base = parent;
    }
    else {
      parent -> competingRiskBase = makeCompetingRiskBase(eTypeSize, mTimeSize, sTimeSize);
      parent -> competingRiskBase -> base = parent;
    }
  }
  else if (rnfCount > 0) {
    parent -> regressionBase = makeRegressionBase(rnfCount, rnfIndex, NULL);
    parent -> regressionBase -> base = parent;
  }
  else if (rfCount > 0) {
    parent -> classificationBase = makeClassificationBase(rfCount, rfIndex, rfSize);
    parent -> classificationBase -> base = parent;
  }
}
TerminalRegression *makeRegressionBase(uint rnfCount, uint *rnfIndex, uint *dummy) {
  TerminalRegression *parent = (TerminalRegression*) gblock((size_t) sizeof(TerminalRegression));
  parent -> rnfCount = rnfCount;
  parent -> rnfIndex = rnfIndex;
  parent -> meanResponse = NULL;
  return parent;
}
TerminalClassification *makeClassificationBase(uint rfCount, uint *rfIndex, uint *rfSize) {
  TerminalClassification *parent = (TerminalClassification*) gblock((size_t) sizeof(TerminalClassification));
  parent -> rfCount = rfCount;
  parent -> rfIndex = rfIndex;
  parent -> rfSize  = rfSize;
  parent -> multiClassProb = NULL;
  parent -> maxClass = NULL;
  return parent;
}
TerminalSurvival *makeSurvivalBase(uint dummy, uint mTimeSize, uint aeTimeSize, uint sTimeSize) {
  TerminalSurvival *parent = (TerminalSurvival*) gblock((size_t) sizeof(TerminalSurvival));
  parent -> eTypeSize = 1;
  parent -> mTimeSize = mTimeSize;
  parent -> aeTimeSize = aeTimeSize;
  parent -> sTimeSize = sTimeSize;
  parent -> atRiskCount = NULL;
  parent -> atRiskTime = NULL;
  parent -> eventCount = NULL;
  parent -> eventTimeIndex = NULL;
  parent -> localRatio = NULL;
  parent -> localSurvival = NULL;
  parent -> localNelsonAalen = NULL;
  parent -> localHazard = NULL;
  parent -> allNelsonAalen = NULL;
  parent -> nelsonAalen = NULL;
  parent -> allHazard = NULL;
  parent -> hazard = NULL;
  parent -> survival = NULL;
  parent -> outcome = NULL;
  return parent;
}
TerminalCompetingRisk *makeCompetingRiskBase(uint eTypeSize, uint mTimeSize, uint sTimeSize) {
  TerminalCompetingRisk *parent = (TerminalCompetingRisk*) gblock((size_t) sizeof(TerminalSurvival));
  parent -> eTypeSize = eTypeSize;
  parent -> mTimeSize = mTimeSize;
  parent -> sTimeSize = sTimeSize;
  parent -> atRiskCount = NULL;
  parent -> atRiskTime = NULL;
  parent -> eventCount = NULL;
  parent -> eventTimeIndex = NULL;
  parent -> localRatio = NULL;
  parent -> localCSH = NULL;
  parent -> localCIF = NULL;
  parent -> CSH = NULL;
  parent -> CIF = NULL;
  parent -> outcome = NULL;
  return parent;
}
void stackMeanResponse(TerminalRegression *tTerm) {
  tTerm -> meanResponse = dvector(1, tTerm -> rnfCount);
}
void unstackMeanResponse(TerminalRegression *tTerm) {
  if (tTerm -> meanResponse != NULL) {
    free_dvector(tTerm -> meanResponse, 1, tTerm -> rnfCount);
    tTerm -> meanResponse = NULL;
  }
}
void stackMultiClassProb(TerminalClassification *tTerm) {
  unsigned int j;
  tTerm -> multiClassProb = (unsigned int **) new_vvector(1, tTerm -> rfCount, NRUTIL_UPTR);
  for (j = 1; j <= tTerm -> rfCount; j++) {
    (tTerm -> multiClassProb)[j] = uivector(1, (tTerm -> rfSize)[j]);
  }
  tTerm -> maxClass = dvector(1, tTerm -> rfCount);
}
void unstackMultiClassProb(TerminalClassification *tTerm) {
  unsigned int j;
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
  if (tTerm -> maxClass != NULL) {
    free_dvector(tTerm -> maxClass, 1, tTerm -> rfCount);
    tTerm -> maxClass = NULL;
  }
}
void stackAllNelsonAalen(TerminalSurvival *tTerm) {
  tTerm -> allNelsonAalen = dvector(1, tTerm -> aeTimeSize);
}
void unstackAllNelsonAalen(TerminalSurvival *tTerm) {
  if (tTerm -> allNelsonAalen != NULL) {
    free_dvector(tTerm -> allNelsonAalen, 1, tTerm -> aeTimeSize);
    tTerm -> allNelsonAalen = NULL;
  }
}
void stackNelsonAalen(TerminalSurvival *tTerm) {
  tTerm -> nelsonAalen = dvector(1, tTerm -> sTimeSize);
}
void unstackNelsonAalen(TerminalSurvival *tTerm) {
  if (tTerm -> nelsonAalen != NULL) {
    free_dvector(tTerm -> nelsonAalen, 1, tTerm -> sTimeSize);
    tTerm -> nelsonAalen = NULL;
  }
}
void stackAllHazard(TerminalSurvival *tTerm) {
  tTerm -> allHazard = dvector(1, tTerm -> aeTimeSize);
}
void unstackAllHazard(TerminalSurvival *tTerm) {
  if (tTerm -> allHazard != NULL) {
    free_dvector(tTerm -> allHazard, 1, tTerm -> aeTimeSize);
    tTerm -> allHazard = NULL;
  }
}
void stackHazard(TerminalSurvival *tTerm) {
  tTerm -> hazard = dvector(1, tTerm -> sTimeSize);
}
void unstackHazard(TerminalSurvival *tTerm) {
  if (tTerm -> hazard != NULL) {
    free_dvector(tTerm -> hazard, 1, tTerm -> sTimeSize);
    tTerm -> hazard = NULL;
  }
}
void stackSurvival(TerminalSurvival *tTerm) {
  tTerm -> survival = dvector(1, tTerm -> sTimeSize);
}
void unstackSurvival(TerminalSurvival *tTerm) {
  if (tTerm -> survival != NULL) {
    free_dvector(tTerm -> survival, 1, tTerm -> sTimeSize);
    tTerm -> survival = NULL;
  }
}
void stackSurvivalOutcome(TerminalSurvival *tTerm) {
  tTerm -> outcome = dvector(1, 1);
}
void unstackSurvivalOutcome(TerminalSurvival *tTerm) {
  if (tTerm -> outcome != NULL) {
    free_dvector(tTerm -> outcome, 1, 1);
    tTerm -> outcome = NULL;
  }
}
void stackCSH(TerminalCompetingRisk *tTerm) {
  tTerm -> CSH = dmatrix(1, tTerm -> eTypeSize, 1, tTerm -> sTimeSize);
}
void unstackCSH(TerminalCompetingRisk *tTerm) {
  if (tTerm -> CSH != NULL) {
    free_dmatrix(tTerm -> CSH, 1, tTerm -> eTypeSize, 1, tTerm -> sTimeSize);
    tTerm -> CSH = NULL;
  }
}
void stackCIF(TerminalCompetingRisk *tTerm) {
  tTerm -> CIF = dmatrix(1, tTerm -> eTypeSize, 1, tTerm -> sTimeSize);
}
void unstackCIF(TerminalCompetingRisk *tTerm) {
  if (tTerm -> CIF != NULL) {
    free_dmatrix(tTerm -> CIF, 1, tTerm -> eTypeSize, 1, tTerm -> sTimeSize);
    tTerm -> CIF = NULL;
  }
}
void stackCompetingRiskOutcome(TerminalCompetingRisk *tTerm) {
  tTerm -> outcome = dvector(1, tTerm -> eTypeSize);
}
void unstackCompetingRiskOutcome(TerminalCompetingRisk *tTerm) {
  if (tTerm -> outcome != NULL) {
    free_dvector(tTerm -> outcome, 1, tTerm -> eTypeSize);
    tTerm -> outcome = NULL;
  }
}
void stackAtRiskAndEventCount(TerminalSurvival *tTerm) {
  tTerm -> atRiskCount     = uivector(1, tTerm -> mTimeSize);
  tTerm -> eventCount      = uimatrix(1, tTerm -> eTypeSize, 1, tTerm -> mTimeSize);
}
void unstackAtRiskAndEventCount(TerminalSurvival *tTerm) {
  if (tTerm -> atRiskCount != NULL) {
    free_uivector(tTerm -> atRiskCount, 1, tTerm -> mTimeSize);
    tTerm -> atRiskCount = NULL;
  }
  if (tTerm -> eventCount != NULL) {
    free_uimatrix(tTerm -> eventCount, 1, tTerm -> eTypeSize, 1, tTerm -> mTimeSize);
    tTerm -> eventCount = NULL;
  }
}
void stackEventTimeIndex(TerminalSurvival *tTerm) {
  tTerm -> eventTimeIndex  = uivector(1, tTerm -> eTimeSize + 1);
}
void unstackEventTimeIndex(TerminalSurvival *tTerm) {
  if (tTerm -> eventTimeIndex != NULL) {
    free_uivector(tTerm -> eventTimeIndex, 1, tTerm -> eTimeSize + 1);
    tTerm -> eventTimeIndex = NULL;
  }
}
void stackLocalRatio(TerminalSurvival *tTerm) {
  tTerm -> localRatio = dmatrix(1, tTerm -> eTypeSize, 1, tTerm -> eTimeSize);
}
void unstackLocalRatio(TerminalSurvival *tTerm) {
  if (tTerm -> localRatio != NULL) {
    free_dmatrix(tTerm -> localRatio, 1, tTerm -> eTypeSize, 1, tTerm -> eTimeSize);
    tTerm -> localRatio = NULL;
  }
}
void stackLocalNelsonAalen(TerminalSurvival *tTerm) {
  tTerm -> localNelsonAalen = dvector(1, tTerm -> eTimeSize);
}
void unstackLocalNelsonAalen(TerminalSurvival *tTerm) {
  if (tTerm -> localNelsonAalen != NULL) {
    free_dvector(tTerm -> localNelsonAalen, 1, tTerm -> eTimeSize);
    tTerm -> localNelsonAalen = NULL;
  }
}
void stackLocalHazard(TerminalSurvival *tTerm) {
  tTerm -> localHazard = dvector(1, tTerm -> eTimeSize);
}
void unstackLocalHazard(TerminalSurvival *tTerm) {
  if (tTerm -> localHazard != NULL) {
    free_dvector(tTerm -> localHazard, 1, tTerm -> eTimeSize);
    tTerm -> localHazard = NULL;
  }
}
void freeTerminalBase(TerminalBase *parent) {
  deinitTerminalBase(parent);
  free_gblock(parent, (size_t) sizeof(TerminalBase));
}
void deinitTerminalBase(TerminalBase *parent) {
  if (parent -> survivalBase != NULL) {
    freeSurvivalBase(parent -> survivalBase);
    parent -> survivalBase = NULL;
  }
  else if (parent -> competingRiskBase != NULL) {
    freeCompetingRiskBase(parent -> competingRiskBase);
    parent -> competingRiskBase = NULL;
  }
  else if (parent -> classificationBase != NULL) {
    freeClassificationBase(parent -> classificationBase);
    parent -> classificationBase = NULL;
  }
  else if (parent -> regressionBase != NULL) {
    freeRegressionBase(parent -> regressionBase);
    parent -> regressionBase = NULL;
  }
}
void freeRegressionBase(TerminalRegression *parent) {
  if (parent != NULL) {
    unstackMeanResponse(parent);
    free_gblock(parent, (size_t) sizeof(TerminalRegression));
  }
}
void freeClassificationBase(TerminalClassification *parent) {
  if (parent != NULL) {
    unstackMultiClassProb(parent);
    free_gblock(parent, (size_t) sizeof(TerminalClassification));    
  }
}
void freeSurvivalBase(TerminalSurvival *parent) {
  if (parent != NULL) {
    unstackAllNelsonAalen(parent);
    unstackNelsonAalen(parent);
    unstackAllHazard(parent);
    unstackHazard(parent);
    unstackSurvival(parent);
    unstackSurvivalOutcome(parent);
    free_gblock(parent, (size_t) sizeof(TerminalSurvival));
  }
}
void freeCompetingRiskBase(TerminalCompetingRisk *parent) {
  if (parent != NULL) {
    unstackCSH(parent);
    unstackCIF(parent);
    unstackCompetingRiskOutcome(parent);
    free_gblock(parent, (size_t) sizeof(TerminalCompetingRisk));    
  }
}
void assignTerminalNodeMembership(char             mode,
                                  uint             treeID,
                                  TerminalBase    *parent,
                                  uint            *genMembrIndx,
                                  uint             genMembrSize,
                                  uint            *gmbrIterator,
                                  TerminalBase  ***tTermMembership) {
  uint i;
  if (RF_optHigh & OPT_MEMB_INCG) {
    for (i = 1; i <= genMembrSize; i++) {
      ++(*gmbrIterator);
      tTermMembership[treeID][genMembrIndx[(*gmbrIterator)]] = parent;
    }
  }
  else {
    for (i = 1; i <= genMembrSize; i++) {
      tTermMembership[treeID][genMembrIndx[i]] = parent;
    }
  }
}
