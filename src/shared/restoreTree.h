#ifndef  RF_RESTORE_TREE_H
#define  RF_RESTORE_TREE_H
#include "nodeBase.h"
void restoreTree(char mode, uint b, NodeBase *parent);
char restoreNodeMembership(char      mode,
                           char      rootFlag,
                           uint      treeID,
                           NodeBase *parent,
                           uint     *repMembrIndx,
                           uint      repMembrSize,
                           uint     *allMembrIndx,
                           uint      allMembrSize,
                           uint     *rmbrIterator,
                           uint     *ambrIterator);
#endif
