#ifndef  RF_TERM_BASE_OPS_H
#define  RF_TERM_BASE_OPS_H
#include "terminalBase.h"
TerminalBase *makeTerminalBase();
void initTerminalBase(TerminalBase *parent);
void deinitTerminalBase(TerminalBase *parent);
void freeTerminalBase(TerminalBase *parent);
void freeTerminalBaseNonSurvivalStructures(TerminalBase *tTerm);
void stackMeanResponse(TerminalBase *tTerm, unsigned int rnfCount);
void unstackMeanResponse(TerminalBase *tTerm);
void stackMultiClassProb(TerminalBase *tTerm, unsigned int rfCount, unsigned int *rfSize);
void unstackMultiClassProb(TerminalBase *tTerm);
#endif
