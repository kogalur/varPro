#ifndef  RF_RANDOM_H
#define  RF_RANDOM_H
void stackRandom(uint aSize, uint bSize, uint dSize, uint cSize);
void unstackRandom(uint aSize, uint bSize, uint dSize, uint cSize);
void randomSetChainParallelA(uint r, int value);
void randomSetChainParallelB(uint r, int value);
void randomSetChainParallelC(uint r, int value);
void randomSetChainParallelD(uint r, int value);
int randomGetChainParallelA(uint r);
int randomGetChainParallelB(uint r);
int randomGetChainParallelC(uint r);
int randomGetChainParallelD(uint r);
float randomChainParallelA(uint r);
float randomChainParallelB(uint r);
float randomChainParallelC(uint r);
float randomChainParallelD(uint r);
float ran1_generic(int *iy, int *iv, int *idum);
void lcgenerator(unsigned int *seed, unsigned char reset);
float ran1_original(int *idum);
#endif
