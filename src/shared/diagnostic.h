#ifndef  RF_DIAGNOSTIC_H
#define  RF_DIAGNOSTIC_H
#include "splitInfo.h"
#include "nodeBase.h"
#include "terminalBase.h"
void getSplitObjectInfo(SplitInfo *info);
void getNodeBaseInfo(NodeBase *nodePtr);
void getTerminalBaseInfo(TerminalBase *termPtr);
void printTreeInfo(uint treeID, NodeBase *parent);
void initTimer();
void printTimer();
void printParameters(char mode);
#endif
