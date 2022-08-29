#ifndef RF_FACTOR_OPS_H
#define RF_FACTOR_OPS_H
#include "factor.h"
struct factor *makeFactor(uint r, char bookFlag);
void freeFactor(struct factor *f);
char bookFactor(struct factor *f);
char unbookFactor(struct factor *f);
void bookPair (uint    levelCount,
               uint    groupIndex,
               uint    levelIndex,
               uint   *row,
               uint   *level,
               struct factor *f);
void nChooseK (uint n, uint r, char type, void *result);
char reduceFraction(uint *numerator, uint *denominator);
char splitOnFactor(uint level, uint *mwcp);
#endif
