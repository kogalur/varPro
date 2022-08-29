#ifndef RF_TERM_OPS_H
#define RF_TERM_OPS_H
#include "terminal.h"
void *makeTerminalDerived();
void  freeTerminalDerived(void *parent);
void stackCompMeanResponseOuter(Terminal *tTerm, unsigned int xReleaseCount);
void stackCompMeanResponseInner(Terminal *tTerm, unsigned int xReleaseIndx, unsigned int rnfCount);
void stackOobMeanResponse(Terminal *tTerm, unsigned int rnfCount);
void unstackMeanResponseTerm(Terminal *tTerm);
void stackCompMultiClassOuter(Terminal *tTerm, unsigned int xReleaseCount);
void stackCompMultiClassInner(Terminal *tTerm, unsigned int xReleaseIndx, unsigned int rfCount, unsigned int *rfSize);
void stackOobMultiClass(Terminal *tTerm, unsigned int rfCount, unsigned int *rfSize);
void unstackMultiClassTerm(Terminal *tTerm);
#endif 
