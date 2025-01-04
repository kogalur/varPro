
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "restoreTree.h"
#include "terminalBase.h"
#include "splitInfo.h"
#include "error.h"
#include "nrutil.h"
#include "nodeBaseOps.h"
#include "termBaseOps.h"
#include "leafLink.h"
void restoreTree(char mode, uint treeID, NodeBase *parent) {
  ulong *offset;
  SplitInfoMax *info;
  uint i;
  offset = &RF_restoreTreeOffset[treeID];
  if (treeID != RF_treeID_[*offset]) {
    RF_nativeError("\nRF-SRC:  Diagnostic Trace of Tree Record:  \n");
    RF_nativeError("\nRF-SRC:      treeID     nodeID ");
    RF_nativeError("\nRF-SRC:  %10d %10d \n", RF_treeID_[*offset], RF_nodeID_[*offset]);
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Invalid forest input record in tree:  %10d", treeID);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  }
  if (parent -> parent != NULL) {
    parent -> depth = (parent -> parent) -> depth + 1;
  }
  parent -> left  = NULL;
  parent -> right = NULL;
  parent -> splitFlag = FALSE;
  parent -> nodeID = RF_nodeID_[*offset];
  parent -> repMembrSize = RF_nodeSZ_[*offset];
  if (RF_parmID_[1][*offset] != 0) {
    info = parent -> splitInfoMax = makeSplitInfoMax(0);
    info -> splitParameter = RF_parmID_[1][*offset];
    info -> splitValueFactSize = RF_mwcpSZ_[1][*offset];
    if (RF_mwcpSZ_[1][*offset] > 0) {
      info -> splitValueFactPtr = uivector(1, RF_mwcpSZ_[1][*offset]);
      for (i = 1; i <= RF_mwcpSZ_[1][*offset]; i++) {
        RF_restoreMWCPoffset[1][treeID] ++;
        info -> splitValueFactPtr[i] = RF_mwcpPT_[1][RF_restoreMWCPoffset[1][treeID]];
      }
    }
    else {
      info -> splitValueCont =  RF_contPT_[1][*offset];
    }
  }
  else {
    parent -> splitInfoMax = NULL;
  }
  (*offset) ++;
  if (parent -> splitInfoMax != NULL) {
    parent -> left  = (NodeBase *) makeNode(0);
    setParent(parent ->  left, parent);
    restoreTree(mode, treeID, parent -> left);
    parent -> right = (NodeBase *) makeNode(0);
    setParent(parent -> right, parent);
    restoreTree(mode, treeID, parent -> right);
  }
  else {
    RF_leafLinkedObjTail[treeID] = makeAndSpliceLeafLinkedObj(RF_leafLinkedObjTail[treeID]);
    RF_leafLinkedObjTail[treeID] -> nodePtr = parent;
    TerminalBase *termBasePtr = (TerminalBase *) makeTerminal();
    RF_leafLinkedObjTail[treeID] -> termPtr = termBasePtr;
    initTerminalBase(termBasePtr,
                     RF_eventTypeSize,
                     RF_masterTimeSize,
                     0,
                     RF_sortedTimeInterestSize,
                     RF_rNonFactorCount,
                     RF_rNonFactorIndex,
                     RF_rFactorCount,
                     RF_rFactorIndex,
                     RF_rFactorSize);
    parent -> mate = termBasePtr;
    termBasePtr -> mate = parent;
    RF_leafLinkedObjTail[treeID] -> nodeID = (RF_leafLinkedObjTail[treeID] -> termPtr) -> nodeID = parent -> nodeID;
  }
}
char restoreNodeMembership(char      mode,
                           char      rootFlag,
                           uint      treeID,
                           NodeBase *parent,
                           uint     *repMembrIndx,
                           uint      repMembrSize,
                           uint     *allMembrIndx,
                           uint      allMembrSize,
                           uint     *rmbrIterator,
                           uint     *ambrIterator) {
  char  bootResult;
  char leftResult, rghtResult;
  char terminalFlag;
  uint *leftRepMembrIndx;
  uint *rghtRepMembrIndx;
  uint *leftAllMembrIndx;
  uint *rghtAllMembrIndx;
  uint leftRepMembrSize, rghtRepMembrSize;
  uint leftAllMembrSize;
  uint rghtAllMembrSize;
  bootResult = TRUE;
  terminalFlag = TRUE;
  if (((parent -> left) != NULL) && ((parent -> right) != NULL)) {
    terminalFlag = FALSE;
    leftAllMembrIndx = rghtAllMembrIndx = NULL;
    leftAllMembrSize = rghtAllMembrSize = 0;
    leftRepMembrIndx = rghtRepMembrIndx = NULL;
    leftRepMembrSize = rghtRepMembrSize = 0;
    if (RF_optHigh & OPT_MEMB_INCG) {
    }
    leftResult = restoreNodeMembership(mode,
                                       FALSE,
                                       treeID,
                                       parent -> left,
                                       leftRepMembrIndx,
                                       leftRepMembrSize,
                                       leftAllMembrIndx,
                                       leftAllMembrSize,
                                       rmbrIterator,
                                       ambrIterator);
    if(!leftResult) {
    }
    rghtResult = restoreNodeMembership(mode,
                                       FALSE,
                                       treeID,
                                       parent -> right,
                                       rghtRepMembrIndx,
                                       rghtRepMembrSize,
                                       rghtAllMembrIndx,
                                       rghtAllMembrSize,
                                       rmbrIterator,
                                       ambrIterator);
    if(!rghtResult) {
    }
    if (RF_optHigh & OPT_MEMB_INCG) {
    }
  }  
  else {
  }
  if (terminalFlag) {
    if (RF_optHigh & OPT_MEMB_INCG) {
      RF_leafLinkedObjTail[treeID] = makeAndSpliceLeafLinkedObj(RF_leafLinkedObjTail[treeID]);
      assignTerminalNodeMembership(mode,
                                   treeID,
                                   RF_leafLinkedObjTail[treeID] -> termPtr,
                                   RF_AMBR_ID_ptr[treeID],
                                   RF_TN_ACNT_ptr[treeID][((TerminalBase*) RF_leafLinkedObjTail[treeID] -> termPtr) -> nodeID],
                                   ambrIterator,
                                   RF_tTermMembership);
    }
    else {
      assignTerminalNodeMembership(mode,
                                   treeID,
                                   RF_leafLinkedObjTail[treeID] -> termPtr,
                                   allMembrIndx,
                                   allMembrSize,
                                   ambrIterator,
                                   RF_tTermMembership);
    }
  }  
  return bootResult;
}  
