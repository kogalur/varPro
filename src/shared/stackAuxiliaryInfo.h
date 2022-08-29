#ifndef  RF_STACK_AUXILIARY_INFO_H
#define  RF_STACK_AUXILIARY_INFO_H
#include "snpAuxiliaryInfo.h"
void stackAuxiliaryInfoList(SNPAuxiliaryInfo ***list, uint count);
void unstackAuxiliaryInfoAndList(AuxiliaryDimensionConstants *dimConst, char targetFlag, SNPAuxiliaryInfo **list, uint count);
void allocateAuxiliaryInfo(AuxiliaryDimensionConstants *dimConst,
                           char   targetFlag,
                           char   type,
                           char  *stringIdentifier,
                           SNPAuxiliaryInfo **list,
                           uint   slot,
                           void  *snpPtr,
                           void  *auxiliaryArrayPtr,
                           uint   dimSize,
                           int   *dim);
uint getAuxDim(char flag, int *dim, uint preIndex, uint postIndex, AuxiliaryDimensionConstants *dimConst);
AuxiliaryDimensionConstants *makeAuxDimConsts(uint *rFactorSize,
                                              uint  rFactorCount,
                                              uint *rFactorMap,
                                              uint *rTargetFactor,
                                              uint  rTargetFactorCount,
                                              uint *tLeafCount,
                                              uint *holdBLKptr);
void freeAuxDimConsts(AuxiliaryDimensionConstants *obj);
#endif
