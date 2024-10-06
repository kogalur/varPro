
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "survivalUtil.h"
#include "shared/termBaseOps.h"
#include "shared/nrutil.h"
#include "shared/error.h"
void getAtRiskAndEventCount(uint treeID,
                            TerminalSurvival *parent,
                            uint *membrIndx,
                            uint membrSize,
                            double *statusPtr) {
  uint i, j, k;
  uint ii;
  char eventFlag;
  stackAtRiskAndEventCount(parent);
  for (j = 1; j <= parent -> mTimeSize; j++) {
    (parent -> atRiskCount)[j] = 0;
    for (k = 1; k <= parent -> eTypeSize; k++) {
      (parent -> eventCount)[k][j] = 0;
    }
  }
  for (i = 1; i <= membrSize; i++) {
    ii = membrIndx[i];
    for (j = 1; j <= RF_masterTimeIndex[treeID][ii]; j++) {
      (parent -> atRiskCount)[j] ++;
    }
    if (statusPtr[ii] > 0) {
      if (parent -> eTypeSize > 1) {
        k = RF_eventTypeIndex[(uint) statusPtr[ii]];
      }
      else {
        k = 1;
      }
      (parent -> eventCount)[k][RF_masterTimeIndex[treeID][ii]] ++;
    }
  }
  uint *tempEventTimeIndex = uivector(1, parent -> mTimeSize);
  parent -> eTimeSize = 0;
  i = 0;    
  for (j = 1; j <= parent -> mTimeSize; j++) {
    eventFlag = FALSE;
    for (k = 1; k <= parent -> eTypeSize; k++) {
      if ((parent -> eventCount)[k][j] > 0) {
        eventFlag = TRUE;
        k = parent -> eTypeSize;
      }
    }
    if (eventFlag == TRUE) {
      tempEventTimeIndex[++i] = j;        
      (parent -> eTimeSize)++;
    }
  }
  stackEventTimeIndex(parent);
  for (j = 1; j <= parent -> eTimeSize; j++) {
    (parent -> eventTimeIndex)[j] = tempEventTimeIndex[j];
  }
  free_uivector(tempEventTimeIndex, 1, parent -> mTimeSize);
}
void getLocalRatio(uint treeID, TerminalSurvival *parent) {
  uint q;
  if(parent -> eTimeSize > 0) {
    stackLocalRatio(parent);
    for (q = 1; q <= parent -> eTimeSize; q++) {
      if ((parent -> eventCount)[1][(parent -> eventTimeIndex)[q]] > 0) {
        if ((parent -> atRiskCount)[(parent -> eventTimeIndex)[q]] >= 1) {
          (parent -> localRatio)[1][q] = (double) ((parent -> eventCount)[1][(parent -> eventTimeIndex)[q]]) / (parent -> atRiskCount)[(parent -> eventTimeIndex)[q]];
        }
        else {
          RF_nativeError("\nRF-SRC:  *** ERROR *** ");
          RF_nativeError("\nRF-SRC:  Zero At Risk Count encountered in local ratio calculation for (tree, leaf) = (%10d, %10d)", treeID, parent -> base -> nodeID);
          RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
          RF_nativeExit();
        }
      }
      else {
        (parent -> localRatio)[1][q] = 0.0;
      }
    }
  }
}
void getLocalNelsonAalen(uint treeID, TerminalSurvival *parent) {
  uint q;
  if (parent -> eTimeSize > 0) {
    stackLocalNelsonAalen(parent);
    for (q = 1; q <= parent -> eTimeSize; q++) {
      (parent -> localNelsonAalen)[q] = (parent -> localRatio)[1][q];
    }
    for (q = 2; q <= parent -> eTimeSize; q++) {
      (parent -> localNelsonAalen)[q] += (parent -> localNelsonAalen)[q-1];
    }
  }
}
void getNelsonAalen(uint treeID, TerminalSurvival *parent) {
  uint k;
  stackNelsonAalen(parent);
  for (k = 1; k <= parent -> sTimeSize; k++) {
    (parent -> nelsonAalen)[k] = 0.0;
  }
  mapLocalToTimeInterest(treeID,
                         parent,
                         parent -> localNelsonAalen,
                         parent -> nelsonAalen);
}
void mapLocalToTimeInterest(uint              treeID,
                            TerminalSurvival *parent,
                            double   *genericLocal,
                            double   *genericGlobal) {
  uint itIndex, etIndex, lookAheadIndex;
  char mapFlag, transitFlag;
  if ((parent -> eTimeSize) > 0) {
    itIndex = 1;
    etIndex = 1;
    mapFlag = TRUE;
    while(mapFlag) {
      if (RF_timeInterest[itIndex] < RF_masterTime[(parent -> eventTimeIndex)[etIndex]] ) {
        if (itIndex > 1) {
          genericGlobal[itIndex] = genericGlobal[itIndex-1];
        }
        itIndex++;
      }
      else {
        lookAheadIndex = etIndex;
        transitFlag = TRUE;
        while (transitFlag) {
          if (RF_timeInterest[itIndex] >= RF_masterTime[(parent -> eventTimeIndex)[lookAheadIndex]] ) {
            genericGlobal[itIndex] = genericLocal[lookAheadIndex];
            lookAheadIndex++;
            if (lookAheadIndex > (parent -> eTimeSize)) {
              transitFlag = FALSE;
            }
          }
          else {
            transitFlag = FALSE;
          }
        }
        itIndex++;
        etIndex = lookAheadIndex;
      }
      if(etIndex > (parent -> eTimeSize)) {
        while(itIndex <= parent -> sTimeSize) {
          genericGlobal[itIndex] = genericGlobal[itIndex-1];
          itIndex++;
        }
      }
      if(itIndex > parent -> sTimeSize) {
        mapFlag = FALSE;
      }
    }
  }
}
void getSurvivalOutcome(uint treeID, TerminalSurvival *parent) {
  uint q;
  stackSurvivalOutcome(parent);
  parent -> outcome[1] = 0.0;
  for (q = 1; q <= parent -> sTimeSize; q++) {
    parent -> outcome[1] += parent -> nelsonAalen[q];
  }
}
