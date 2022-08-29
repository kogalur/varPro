#ifndef  RF_REGRESSION_H
#define  RF_REGRESSION_H
#include "terminalBase.h"
void assignMeanResponse(uint           treeID,
                        TerminalBase  *parent,
                        uint           rNonFactorCount,
                        double        *tn_regr_ptr);
#endif
