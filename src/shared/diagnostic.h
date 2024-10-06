#ifndef  RF_DIAGNOSTIC_H
#define  RF_DIAGNOSTIC_H
#include "splitInfo.h"
#include "nodeBase.h"
#include "terminalBase.h"
void getSplitObjectInfo(SplitInfoMax *info);
void getNodeBaseInfo(NodeBase *nodePtr);
void getTerminalBaseInfo(TerminalBase *termPtr);
void printTreeInfo(uint treeID, NodeBase *parent);
void initTimer(void);
void printTimer(void);
void printParameters(char mode);
#endif
