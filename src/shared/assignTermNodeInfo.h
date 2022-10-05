#ifndef  RF_ASSIGN_TERM_NODE_INFO_H
#define  RF_ASSIGN_TERM_NODE_INFO_H
#include "terminalBase.h"
void assignTerminalNodeMembership(char             mode,
                                  uint             treeID,
                                  TerminalBase    *parent,
                                  uint            *genMembrIndx,
                                  uint             genMembrSize,
                                  uint            *gmbrIterator,
                                  TerminalBase  ***tTermMembership);
void assignTerminalNodeOutcomes(char           mode,
                                uint           treeID,
                                TerminalBase  *parent,
                                uint           startTimeIndex,
                                uint           timeIndex,
                                uint           statusIndex,
                                uint          *rFactorSize);
#endif
