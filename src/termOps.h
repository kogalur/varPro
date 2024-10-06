#ifndef RF_TERM_OPS_H
#define RF_TERM_OPS_H
#include "terminal.h"
void *makeTerminalDerived();
void  freeTerminalDerived(void *parent);
void stackCompMeanResponseOuter(Terminal *tTerm, unsigned int xReleaseCount);
void stackCompMeanResponseInner(Terminal *tTerm, unsigned int xReleaseIndx);
void stackOobMeanResponse(Terminal *tTerm);
void unstackMeanResponseTerm(Terminal *tTerm);
void stackCompMultiClassOuter(Terminal *tTerm, unsigned int xReleaseCount);
void stackCompMultiClassInner(Terminal *tTerm, unsigned int xReleaseIndx);
void stackOobMultiClass(Terminal *tTerm);
void unstackMultiClassTerm(Terminal *tTerm);
void stackCompMortalityOuter(Terminal *tTerm, unsigned int xReleaseCount);
void unstackMortalityTerm(Terminal *tTerm);
void restoreTerminalNodeOutcomesVarPro(uint treeID, Terminal *term);
#endif 
