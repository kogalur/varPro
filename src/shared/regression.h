#ifndef  RF_REGRESSION_H
#define  RF_REGRESSION_H
#include "terminalBase.h"
void assignAllRegressionOutcomes(char mode, uint treeID, TerminalBase *term);
void calculateAllRegressionOutcomes(char mode, uint treeID, TerminalBase *term);
void assignMeanResponse(uint                 treeID,
                        TerminalRegression  *parent,
                        double              *tn_regr_ptr);
void calculateMeanResponse(uint                treeID,
                           TerminalRegression *parent);
void updateEnsembleMean(char mode, uint treeID);
double getMeanSquareError(uint    size,
                          double *responsePtr,
                          double *predictedOutcome,
                          double *denomCount);
#endif
