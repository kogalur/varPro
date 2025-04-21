
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "random.h"
#include "nrutil.h"
#define IA      16807
#define IM      2147483647
#define AM      (1.0/IM)
#define IQ      127773
#define IR      2836
#define NTAB    32
#define NDIV    (1+(IM-1)/NTAB)
#define EPS     1.2e-7
#define RNMX    (1.0-EPS)
#define LCG_IM  714025
#define LCG_IA  1366
#define LCG_IC  150889
int  *ran1A_iy;
int **ran1A_iv;
int  *ran1B_iy;
int **ran1B_iv;
int  *ran1C_iy;
int **ran1C_iv;
int  *ran1D_iy;
int **ran1D_iv;
int      *seed1AValue;
int      *seed1BValue;
int      *seed1CValue;
int      *seed1DValue;
void stackRandom(uint aSize, uint bSize, uint cSize, uint dSize) {
  uint r;
  if (aSize > 0) {
    ran1A_iy = ivector(1, aSize);
    ran1A_iv = imatrix(1, aSize, 1, NTAB);
    seed1AValue = ivector(1, aSize);
  }
  if (bSize > 0) {
    ran1B_iy = ivector(1, bSize);
    ran1B_iv = imatrix(1, bSize, 1, NTAB);
    seed1BValue = ivector(1, bSize);
  }
  if (cSize > 0) {
    ran1C_iy = ivector(1, cSize);
    ran1C_iv = imatrix(1, cSize, 1, NTAB);
    seed1CValue = ivector(1, cSize);
  }
  if (dSize > 0) {
    ran1D_iy = ivector(1, dSize);
    ran1D_iv = imatrix(1, dSize, 1, NTAB);
    seed1DValue = ivector(1, dSize);
  }
  for (r = 1; r <= aSize; r++) {
    ran1A_iy[r] = 0;
  }
  for (r = 1; r <= bSize; r++) {
    ran1B_iy[r] = 0;
  }
  for (r = 1; r <= cSize; r++) {
    ran1C_iy[r] = 0;
  }
  for (r = 1; r <= dSize; r++) {
    ran1D_iy[r] = 0;
  }
}
void unstackRandom(uint aSize, uint bSize, uint cSize, uint dSize) {
  if (aSize > 0) {  
    free_ivector(ran1A_iy, 1, aSize);
    free_imatrix(ran1A_iv, 1, aSize, 1, NTAB);
    free_ivector(seed1AValue, 1, aSize);
  }
  if (bSize > 0) {
    free_ivector(ran1B_iy, 1, bSize);
    free_imatrix(ran1B_iv, 1, bSize, 1, NTAB);
    free_ivector(seed1BValue, 1, bSize);
  }
  if (cSize > 0) {
    free_ivector(ran1C_iy, 1, cSize);
    free_imatrix(ran1C_iv, 1, cSize, 1, NTAB);
    free_ivector(seed1CValue, 1, cSize);
  }
  if (dSize > 0) {
    free_ivector(ran1D_iy, 1, dSize);
    free_imatrix(ran1D_iv, 1, dSize, 1, NTAB);
    free_ivector(seed1DValue, 1, dSize);
  }
}
void randomSetChainParallelA(uint r, int value) {
  seed1AValue[r] = value;
}
void randomSetChainParallelB(uint r, int value) {
  seed1BValue[r] = value;
}
void randomSetChainParallelC(uint r, int value) {
  seed1CValue[r] = value;
}
void randomSetChainParallelD(uint r, int value) {
  seed1DValue[r] = value;
}
int randomGetChainParallelA(uint r) {
  return seed1AValue[r];
}
int randomGetChainParallelB(uint r) {
  return seed1BValue[r];
}
int randomGetChainParallelC(uint r) {
  return seed1CValue[r];
}
int randomGetChainParallelD(uint r) {
  return seed1DValue[r];
}
float randomChainParallelA(uint r) {
  return  ran1_generic(& ran1A_iy[r], ran1A_iv[r], & seed1AValue[r]);
}
float randomChainParallelB(uint r) {
  return  ran1_generic(& ran1B_iy[r], ran1B_iv[r], & seed1BValue[r]);
}
float randomChainParallelC(uint r) {
  return  ran1_generic(& ran1C_iy[r], ran1C_iv[r], & seed1CValue[r]);
}
float randomChainParallelD(uint r) {
  return  ran1_generic(& ran1D_iy[r], ran1D_iv[r], & seed1DValue[r]);
}
float ran1_generic(int *iy, int *iv, int *idum) {
  int j, k;
  float temp;
  if (*idum <= 0 || !(*iy)) {
    if (-(*idum) < 1) {
      *idum = 1;
    }
    else {
      *idum = -(*idum);
    }
    for (j = NTAB+7; j >= 0; j--) {
      k = (*idum) / IQ;
      *idum = IA * (*idum - k * IQ) - IR * k;
      if (*idum < 0) *idum += IM;
      if (j < NTAB) iv[j] = *idum;
    }
    (*iy) = iv[1];
  }
  k = (*idum) / IQ;
  *idum = IA * (*idum - k * IQ) - IR * k;
  if (*idum < 0) *idum += IM;
  j = (*iy) / NDIV;
  (*iy) = iv[j];
  iv[j] = *idum;
  if ((temp = AM * (*iy)) > RNMX) {
    return RNMX;
  }
  else {
    return temp;
  }
}
void lcgenerator(unsigned int *seed, unsigned char reset) {
  if (reset) {
    if (*seed >= LCG_IM) (*seed) %= LCG_IM;
  }
  else {
    *seed = (LCG_IA * (*seed) + LCG_IC) % LCG_IM;
  }
}
float ran1_original(int *idum) {
  int j;
  int k;
  static int iy = 0;
  static int iv[NTAB];
  float temp;
  if (*idum <= 0 || !iy) {
    if (-(*idum) < 1) {
      *idum = 1;
    }
    else {
      *idum = -(*idum);
    }
    for (j = NTAB+7; j >= 0; j--) {
      k = (*idum) / IQ;
      *idum = IA * (*idum - k * IQ) - IR * k;
      if (*idum < 0) *idum += IM;
      if (j < NTAB) iv[j] = *idum;
    }
    iy = iv[0];
  }
  k = (*idum) / IQ;
  *idum = IA * (*idum - k * IQ) - IR * k;
  if (*idum < 0) *idum += IM;
  j = iy / NDIV;
  iy = iv[j];
  iv[j] = *idum;
  if ((temp = AM * iy) > RNMX) {
    return RNMX;
  }
  else {
    return temp;
  }
}
#undef IA
#undef IM
#undef AM
#undef IQ
#undef IR
#undef NTAB
#undef NDIV
#undef EPS
#undef RNMX
