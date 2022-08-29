#ifndef RF_IMPORTANCE_H
#define RF_IMPORTANCE_H
#include "terminal.h"
void getMeanResponse(uint       treeID,
                     Terminal  *parent,
                     uint      *membershipIndex,
                     uint       membershipSize,
                     uint       xReleaseIndx,
                     char       oob);
void getMultiClassProb(uint       treeID,
                       Terminal  *parent,
                       uint      *membershipIndex,
                       uint       membershipSize,
                       uint       xReleaseIndx,
                       char       oob);
#endif
