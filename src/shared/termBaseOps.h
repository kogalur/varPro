#ifndef  RF_TERM_BASE_OPS_H
#define  RF_TERM_BASE_OPS_H
#include "terminalBase.h"
TerminalBase *makeTerminalBase();
void preInitTerminalBase(TerminalBase *parent);
void initTerminalBase(TerminalBase *parent,
                      uint         eTypeSize,
                      uint         mTimeSize,
                      uint         sTimeSize,
                      uint         rnfCount,
                      uint         rfCount);
void deinitTerminalBase(TerminalBase *parent);
void freeTerminalBase(TerminalBase *parent);
void freeTerminalBaseNonSurvivalStructures(TerminalBase *tTerm);
void freeTerminalBaseSurvivalStructures(TerminalBase *tTerm);
void stackMeanResponse(TerminalBase *tTerm);
void unstackMeanResponse(TerminalBase *tTerm);
void stackMultiClassProb(TerminalBase *tTerm, unsigned int *rfSize);
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
#endif