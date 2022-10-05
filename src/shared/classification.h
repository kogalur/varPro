#ifndef  RF_CLASSIFICATION_H
#define  RF_CLASSIFICATION_H
#include "terminalBase.h"
void assignMultiClassProb(uint           treeID,
                          TerminalBase  *parent,
                          uint          *rFactorSize,
                          uint         **tn_clas_ptr);
void calculateMultiClassProb(uint           treeID,
                             TerminalBase  *parent,
                             uint          *rFactorSize,
                             uint          *membershipIndex,
                             uint           membershipSize,
                             uint          *membershipIterator);
#endif
