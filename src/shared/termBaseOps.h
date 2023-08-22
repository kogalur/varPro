#ifndef  RF_TERM_BASE_OPS_H
#define  RF_TERM_BASE_OPS_H
#include "terminalBase.h"
TerminalBase *makeTerminalBase(void);
void preInitTerminalBase(TerminalBase *parent);
void initTerminalBase(TerminalBase *parent,
                      uint  eTypeSize,
                      uint  mTimeSize,
                      uint  sTimeSize,
                      uint  rnfCount,
                      uint  rfCount,
                      uint *rfSize,
                      uint *rfIndex,
                      uint *rnfIndex);
void deinitTerminalBase(TerminalBase *parent);
void freeTerminalBase(TerminalBase *parent);
void freeTerminalBaseNonSurvivalStructures(TerminalBase *tTerm);
void freeTerminalBaseSurvivalStructures(TerminalBase *tTerm);
void stackMeanResponse(TerminalBase *tTerm);
void unstackMeanResponse(TerminalBase *tTerm);
void stackMultiClassProb(TerminalBase *tTerm);
void unstackMultiClassProb(TerminalBase *tTerm);
void stackSurvival(TerminalBase *tTerm);
void unstackSurvival(TerminalBase *tTerm);
void stackNelsonAalen(TerminalBase *tTerm);
void unstackNelsonAalen(TerminalBase *tTerm);
void stackCSH(TerminalBase *tTerm);
void unstackCSH(TerminalBase *tTerm);
void stackCIF(TerminalBase *tTerm);
void unstackCIF(TerminalBase *tTerm);
void stackMortality(TerminalBase *tTerm);
void unstackMortality(TerminalBase *tTerm);
void updateTerminalNodeOutcomes(uint treeID, TerminalBase  *parent);
void getMembrCountOnly (uint       treeID,
                        TerminalBase  *parent,
                        uint      *repMembrIndx,
                        uint       repMembrSize,
                        uint      *genMembrIndx,
                        uint       genMembrSize);
#endif
