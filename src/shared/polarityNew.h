#ifndef RF_POLARITY_NEW_H
#define RF_POLARITY_NEW_H
#include "splitInfo.h"
char getDaughterPolaritySimpleFactorNew    (uint treeID, SplitInfoMax *info, uint index, void *value, ...);
char getDaughterPolaritySimpleNonFactorNew (uint treeID, SplitInfoMax *info, uint index, void *value, ...);
char getDaughterPolarityNew                (uint treeID, SplitInfoMax *info, uint index, void *value, ...);
#endif
