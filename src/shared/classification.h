#ifndef  RF_CLASSIFICATION_H
#define  RF_CLASSIFICATION_H
#include "terminalBase.h"
void assignMultiClassProb(uint           treeID,
                          TerminalBase  *parent,
                          uint           rFactorCount,
                          uint          *rFactorSize,
                          uint         **tn_clas_ptr);
#endif
