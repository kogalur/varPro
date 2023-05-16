#ifndef  RF_CALCULATE_TERM_NODE_INFO_H
#define  RF_CALCULATE_TERM_NODE_INFO_H
#include "terminalBase.h"
void calculateTerminalNodeOutcomes(char           mode,
                                   uint           treeID,
                                   TerminalBase  *parent,
                                   uint           rfCount,
                                   uint          *rFactorSize,
                                   uint          *rFactorIndex,
                                   uint          *genMembrIndx,
                                   uint           genMembrSize,
                                   uint          *gmbrIterator);
#endif
