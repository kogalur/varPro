#ifndef  RF_CLASSIFICATION_H
#define  RF_CLASSIFICATION_H
#include "terminalBase.h"
void assignMultiClassProb(uint           treeID,
                          TerminalBase  *parent,
                          uint         **tn_clas_ptr);
void calculateMultiClassProb(uint           treeID,
                             TerminalBase  *parent,
                             uint          *membershipIndex,
                             uint           membershipSize,
                             uint          *membershipIterator);
void updateEnsembleClas(char mode, uint treeID);
double getClassificationIndex(uint     size,
                              double  *responsePtr,
                              double  *denomCount,
                              double  *maxVote);
void getConditionalClassificationIndex(uint     size,
                                       uint     rfSizeElement,
                                       double  *responsePtr,
                                       double **outcomeCLS,
                                       double  *maxVote,
                                       double  *denomCount,
                                       double  *cpv);
void getMaxVote(uint     size,
                uint    rfSizeElement,
                double **outcomeCLS,
                double  *denomCount,
                double  *maxVote);
#endif
