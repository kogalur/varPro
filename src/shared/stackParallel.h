#ifndef RF_STACK_PARALLEL_H
#define RF_STACK_PARALLEL_H
void stackLocksOpenMP(char mode);
void unstackLocksOpenMP(char mode);
void stackLocksPosix(char mode);
void unstackLocksPosix(char mode);
#endif
