#ifndef  RF_ASSIGN_TERM_NODE_INFO_H
#define  RF_ASSIGN_TERM_NODE_INFO_H
#include "terminalBase.h"
void assignTerminalNodeMembership(char             mode,
                                  uint             treeID,
                                  TerminalBase    *parent,
                                  uint            *genMembrIndx,
                                  uint             genMembrSize,
                                  uint            *gmbrIterator,
                                  TerminalBase  ***tTermMembership,
                                  uint           **AMBR_ID_ptr);
void assignTerminalNodeOutcomes(char           mode,
                                uint           treeID,
                                TerminalBase  *parent,
                                uint           rFactorCount,
                                uint          *rFactorSize,
                                uint           rNonFactorCount);
#endif
