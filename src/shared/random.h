#ifndef  RF_RANDOM_H
#define  RF_RANDOM_H
void stackRandom(uint bSize);
void unstackRandom(uint bSize);
void randomSetChainParallel(uint b, int value);
void randomSetChainParallel2(uint b, int value);
void randomSetChainParallel3(uint p, int value);
int randomGetChainParallel(uint b);
int randomGetChainParallel2(uint b);
int randomGetChainParallel3(uint p);
float randomChainParallel(uint b);
float randomChainParallel2(uint b);
float randomChainParallel3(uint p);
float ran1_generic(int *iy, int *iv, int *idum);
void lcgenerator(unsigned int *seed, unsigned char reset);
float ran1_original(int *idum);
#endif
