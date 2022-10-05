#ifndef  RF_REGRESSION_H
#define  RF_REGRESSION_H
#include "terminalBase.h"
void assignMeanResponse(uint           treeID,
                        TerminalBase  *parent,
                        double        *tn_regr_ptr);
void calculateMeanResponse(uint           treeID,
                           TerminalBase  *parent,
                           uint          *membershipIndex,
                           uint           membershipSize,
                           uint          *membershipIterator);
#endif
