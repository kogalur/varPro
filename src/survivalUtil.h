#ifndef RF_SURV_UTIL_H
#define RF_SURV_UTIL_H
#include "shared/terminalBase.h"
void getAtRiskAndEventCount(uint treeID,
                            TerminalSurvival *parent,
                            uint *membrIndx,
                            uint membrSize,
                            double *statusPtr);
void getLocalRatio(uint treeID, TerminalSurvival *parent);
void getLocalNelsonAalen(uint treeID, TerminalSurvival *parent);
void getNelsonAalen(uint treeID, TerminalSurvival *parent);
void mapLocalToTimeInterest(uint treeID,
                            TerminalSurvival *parent,
                            double *genericLocal,
                            double *genericGlobal);
void getSurvivalOutcome(uint treeID, TerminalSurvival *parent);
#endif
