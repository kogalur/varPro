
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
#include           "trace.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "restoreTree.h"
#include "terminalBase.h"
#include "splitInfo.h"
#include "error.h"
#include "nrutil.h"
#include "nodeBaseOps.h"
#include "leafLink.h"
#include "assignTermNodeInfo.h"
void restoreTree(char mode, uint treeID, NodeBase *parent) {
  ulong *offset;
  SplitInfo *info;
  uint adj;
  uint i, k;
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
    info = parent -> splitInfo = makeSplitInfo(0);
    if (RF_hdim == 0) {
      info -> hcDim = 0;
      adj = 1;
    }
    else {
      info -> hcDim = RF_hcDim_[*offset];      
      adj = RF_hcDim_[*offset];
    }
    if (RF_baseLearnDepthINTR > 1) {
      info -> pairCT = RF_pairCT_[*offset];
    }
    if (RF_baseLearnDepthSYTH > 1) {
      if (RF_lotsSZ_[*offset] > 0) {
        info -> sythFlag = TRUE;
        parent -> lotsSize = RF_lotsSZ_[*offset];
      }
    }
    info -> mwcpSizeAbs = uivector(1, adj);
    info -> randomVar   = ivector(1, adj);
    info -> randomPts   = new_vvector(1, adj, NRUTIL_VPTR);
    if (RF_baseLearnDepthINTR > 1) {
      info -> augmX1    = ivector(1, adj);
      info -> augmX2    = ivector(1, adj);
    }
    if (RF_baseLearnDepthSYTH > 1) {
      info -> augmXS    = ivector(1, adj);
    }
    if (RF_hdim > 0) {
      info -> randomPtsRight   = new_vvector(1, adj, NRUTIL_VPTR);
    }
    for (k = 1; k <= adj; k++) {
      info -> randomVar[k] = RF_parmID_[k][*offset];
      info -> mwcpSizeAbs[k] = RF_mwcpSZ_[k][*offset];
      if (RF_mwcpSZ_[k][*offset] > 0) {
        info -> randomPts[k] = uivector(1, RF_mwcpSZ_[k][*offset]);
        for (i = 1; i <= RF_mwcpSZ_[k][*offset]; i++) {
          RF_restoreMWCPoffset[k][treeID] ++;
          ((uint *) info -> randomPts[k])[i] = RF_mwcpPT_[k][RF_restoreMWCPoffset[k][treeID]];
        }
      }
      else {
        info -> randomPts[k] = dvector(1, 1);
        ((double *) info -> randomPts[k])[1] =  RF_contPT_[k][*offset];
        if (RF_hdim > 0) {
          info -> randomPtsRight[k] = dvector(1, 1);
          ((double *) info -> randomPtsRight[k])[1] =  RF_contPTR_[k][*offset];
        }
      }
      if (RF_baseLearnDepthINTR > 1) {
        info -> augmX1[k] = RF_augmX1_[k][*offset];
        info -> augmX2[k] = RF_augmX2_[k][*offset];
      }
      if (RF_baseLearnDepthSYTH > 1) {
        info -> augmXS[k] = RF_augmXS_[k][*offset];
      }
    }
  }
  else {
    parent -> splitInfo = NULL;
  }
  (*offset) ++;
  if (parent -> splitInfo != NULL) {
    parent -> left  = (NodeBase *) makeNode(0);
    setParent(parent ->  left, parent);
    restoreTree(mode, treeID, parent -> left);
    parent -> right = (NodeBase *) makeNode(0);
    setParent(parent -> right, parent);
    restoreTree(mode, treeID, parent -> right);
  }
  else {
    if (RF_optHigh & OPT_MEMB_INCG) {
      RF_leafLinkedObjTail[treeID] = makeAndSpliceLeafLinkedObj(RF_leafLinkedObjTail[treeID]);
      RF_leafLinkedObjTail[treeID] -> nodePtr = (NodeBase *) parent;
      RF_leafLinkedObjTail[treeID] -> termPtr = (TerminalBase *) makeTerminal();
      parent -> mate = RF_leafLinkedObjTail[treeID] -> termPtr;
      (RF_leafLinkedObjTail[treeID] -> termPtr) -> mate = parent;
      RF_leafLinkedObjTail[treeID] -> nodeID = (RF_leafLinkedObjTail[treeID] -> termPtr) -> nodeID = parent -> nodeID;
    }  
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
  uint i;
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
    }
    if (RF_optHigh & OPT_MEMB_USER) {
      if (RF_optHigh & OPT_MEMB_INCG) {
        uint userIterator = *ambrIterator;
        for (i = 1; i <= RF_TN_ACNT_ptr[treeID][parent -> nodeID]; i++) {
          ++(userIterator);
          RF_MEMB_ID_ptr[treeID][RF_AMBR_ID_ptr[treeID][(userIterator)]] = parent -> nodeID;
        }
      }
      else {
        for (i = 1; i <= allMembrSize; i++) {
          RF_MEMB_ID_ptr[treeID][allMembrIndx[i]] = parent -> nodeID;
        }
      }
    }  
    assignTerminalNodeMembership(mode,
                                 treeID,
                                 RF_leafLinkedObjTail[treeID] -> termPtr,
                                 allMembrIndx,
                                 allMembrSize,
                                 ambrIterator,
                                 RF_tTermMembership,
                                 RF_AMBR_ID_ptr);
  }  
  return bootResult;
}  
