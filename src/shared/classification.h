#ifndef  RF_CLASSIFICATION_H
#define  RF_CLASSIFICATION_H
#include "terminalBase.h"
void assignAllClassificationOutcomes(char          mode,
                                     uint          treeID,
                                     TerminalBase *term);
void assignMultiClassProb(uint                    treeID,
                          TerminalClassification *parent,
                          uint                  **tn_clas_ptr);
void calculateMultiClassProb(uint                    treeID,
                             TerminalClassification *parent);
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
