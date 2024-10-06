
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "treeOps.h"
#include "terminal.h"
#include "node.h"
#include "stackOutput.h"
#include "varProMain.h"
#include "importance.h"
#include "termOps.h"
#include "shared/stackMembershipVectors.h"
#include "shared/terminalBase.h"
#include "shared/nrutil.h"
#include "shared/restoreTree.h"
#include "shared/bootstrap.h"
#include "shared/nodeBaseOps.h"
#include "shared/termBaseOps.h"
#include "shared/polarityNew.h"
void acquireTree(char mode, uint b) {
  Node     *root;
  NodeBase *rootBase;
  uint nSize, xSize;
  uint  treeID;
  uint  ambrIterator;
  uint *bootMembrIndx;
  uint  bootMembrSize;
  char  bootResult;
  LeafLinkedObj *leafLinkedObjectPtr;
  uint branchCount;
  uint branchID;
  uint  i, k, j, p;
  treeID = VP_strengthTreeID[b];
  selectBranches(b,
                 treeID,
                 VP_maxRulesTree,
                 RF_tLeafCount_[treeID],
                 & VP_branchCount[b],
                 & VP_branchID[b],
                 & VP_oobCount[b],
                 & VP_xReleaseCount[b],
                 & VP_xReleaseIDArray[b],
                 & VP_complementCount[b],
                 & VP_oobMembers[b],
                 & VP_complementMembers[b],
                 & VP_proxyIndv[b],
                 & VP_proxyIndvDepth[b]);
#ifdef _OPENMP
#endif
  nSize = RF_observationSize;
  xSize = RF_xSize;
  stackMembershipVectors(nSize,
                         &RF_bootMembershipFlag[treeID],
                         &RF_oobMembershipFlag[treeID],
                         &RF_bootMembershipCount[treeID],
                         &RF_ibgMembershipIndex[treeID],
                         &RF_oobMembershipIndex[treeID]);
  RF_tTermMembership[treeID] = (TerminalBase **) new_vvector(1, nSize, NRUTIL_TPTR);
  RF_leafLinkedObjHead[treeID] = RF_leafLinkedObjTail[treeID] = makeLeafLinkedObj();
  root = makeNode(xSize);
  root -> nSize = nSize;
  rootBase = (NodeBase *) root;
  RF_root[treeID] = rootBase;
  rootBase -> parent = NULL;
  rootBase -> nodeID = 1;
  for (k = 1; k <= rootBase -> xSize; k++) {
    rootBase -> permissible[k] = TRUE;
  }
  rootBase -> allMembrSizeAlloc = rootBase -> allMembrSize = nSize;
  rootBase -> allMembrIndx = uivector(1, rootBase -> allMembrSizeAlloc);
  for (i = 1; i <= nSize; i++) {
    rootBase -> allMembrIndx[i] = i;
  }
  if ( (RF_opt & OPT_BOOT_TYP1) || (RF_opt & OPT_BOOT_TYP2)) {
    bootMembrSize = RF_bootstrapSize;
  }
  else {
    bootMembrSize = nSize;
  }
  bootMembrIndx  = uivector(1, bootMembrSize);
  bootResult = bootstrap (mode,
                          treeID,
                          rootBase, 
                          rootBase -> allMembrIndx,
                          rootBase -> allMembrSize,
                          bootMembrSize,
                          RF_bootstrapIn,
                          RF_subjSize,
                          RF_subjWeight,
                          RF_subjWeightType,
                          RF_subjWeightSorted,
                          RF_subjWeightDensitySize,
                          nSize,
                          bootMembrIndx,
                          RF_bootMembershipFlag,
                          RF_oobMembershipFlag,
                          RF_bootMembershipCount,
                          RF_oobSize,
                          RF_ibgSize,
                          RF_ibgMembershipIndex,
                          RF_oobMembershipIndex,
                          RF_BOOT_CT_ptr);
  if (bootResult) {
    rootBase -> repMembrIndx = bootMembrIndx;
    rootBase -> repMembrSize = rootBase -> repMembrSizeAlloc = bootMembrSize;
    restoreTree(mode, treeID, RF_root[treeID]);
    if (RF_optHigh & OPT_MEMB_INCG) {
      ambrIterator = 0;
      leafLinkedObjectPtr = RF_leafLinkedObjHead[treeID];
      for(i = 1; i <= RF_tLeafCount_[treeID]; i++) {
        leafLinkedObjectPtr = leafLinkedObjectPtr -> fwdLink;
        assignTerminalNodeMembership(mode,
                                     treeID,
                                     leafLinkedObjectPtr -> termPtr,
                                     RF_AMBR_ID_ptr[treeID],
                                     RF_TN_ACNT_ptr[treeID][((TerminalBase*) leafLinkedObjectPtr -> termPtr) -> nodeID],
                                     &ambrIterator,
                                     RF_tTermMembership);
      }
    }
    if (RF_optHigh & OPT_TERM_INCG) {
      ambrIterator = 0;
      leafLinkedObjectPtr = RF_leafLinkedObjHead[treeID];
      for(i = 1; i <= RF_tLeafCount_[treeID]; i++) {
        leafLinkedObjectPtr = leafLinkedObjectPtr -> fwdLink;
        restoreTerminalNodeOutcomesVarPro(treeID, (Terminal *) (leafLinkedObjectPtr -> termPtr));
      }
    }
    RF_tTermList[treeID] = (TerminalBase **) new_vvector(1, RF_tLeafCount_[treeID], NRUTIL_TPTR);
    leafLinkedObjectPtr = RF_leafLinkedObjHead[treeID] -> fwdLink;
    while (leafLinkedObjectPtr != NULL) {
      RF_tTermList[treeID][(leafLinkedObjectPtr -> termPtr) -> nodeID] = leafLinkedObjectPtr -> termPtr;
      leafLinkedObjectPtr = leafLinkedObjectPtr -> fwdLink;
    }
    char **pathPolarity;
    char **releaseFlag;
    branchCount = VP_branchCount[b];
    pathPolarity           = (char **) new_vvector(1, branchCount, NRUTIL_CPTR);
    releaseFlag            = (char **) new_vvector(1, branchCount, NRUTIL_CPTR);
    for(j = 1; j <= branchCount; j++) { 
      branchID = VP_branchID[b][j];
      uint *branchMembers;
      branchMembers = uivector(1, RF_oobSize[treeID]);
      VP_oobCount[b][j] = 0;
      acquireProxyIndv(treeID,
                       branchID,
                       RF_oobMembershipIndex[treeID],
                       RF_oobSize[treeID],
                       RF_ibgMembershipIndex[treeID],
                       RF_ibgSize[treeID],
                       branchMembers,
                       & VP_oobCount[b][j],
                       & VP_proxyIndv[b][j],
                       & VP_proxyIndvDepth[b][j]);
      if(VP_oobCount[b][j] != 0) {
        VP_oobMembers[b][j]      = uivector(1, VP_oobCount[b][j]);
        for(p = 1; p <= VP_oobCount[b][j]; p++) {
          VP_oobMembers[b][j][p] = branchMembers[p];
        }
      }
      else {
        VP_oobMembers[b][j]      = NULL;
      }
      if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
        getMortality(treeID,
                     (Terminal *) RF_tTermList[treeID][branchID],
                     VP_oobMembers[b][j],
                     VP_oobCount[b][j],
                     0,
                     TRUE);
      }
      else if (RF_rNonFactorCount > 0) {
        getMeanResponse(treeID,
                        (Terminal *) RF_tTermList[treeID][branchID],
                        VP_oobMembers[b][j],
                        VP_oobCount[b][j],
                        0,
                        TRUE);
      }
      else if (RF_rFactorCount > 0) {
        getMultiClassProb(treeID,
                          (Terminal *) RF_tTermList[treeID][branchID],
                          VP_oobMembers[b][j],
                          VP_oobCount[b][j],
                          0,
                          TRUE);
      }
      VP_xReleaseIDArray[b][j] = uivector(1, VP_proxyIndvDepth[b][j]);
      pathPolarity[j] = cvector(1, VP_proxyIndvDepth[b][j]);
      releaseFlag[j]  = cvector(1, RF_xSize);
      for (i = 1; i <= RF_xSize; i++) {
        releaseFlag[j][i] = FALSE;
      }
      VP_xReleaseCount[b][j] = 0;
      acquireBranch(b,
                    treeID,
                    branchID,
                    VP_proxyIndv[b][j],
                    VP_proxyIndvDepth[b][j],
                    RF_root[treeID],
                    RF_observation[treeID],
                    VP_xReleaseIDArray[b][j],
                    pathPolarity[j],
                    releaseFlag[j],
                    & VP_xReleaseCount[b][j]);
      VP_complementCount[b][j] = uivector(1, VP_xReleaseCount[b][j]);
      VP_complementMembers[b][j] = (uint **) new_vvector(1, VP_xReleaseCount[b][j], NRUTIL_UPTR);
      if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
        stackCompMortalityOuter((Terminal *) RF_tTermList[treeID][branchID], VP_xReleaseCount[b][j]);
      }        
      else if (RF_rNonFactorCount > 0) {
        stackCompMeanResponseOuter((Terminal *) RF_tTermList[treeID][branchID], VP_xReleaseCount[b][j]);
      }
      else if(RF_rFactorCount > 0) {
        stackCompMultiClassOuter((Terminal *) RF_tTermList[treeID][branchID], VP_xReleaseCount[b][j]);
      }
      for(k = 1; k <= VP_xReleaseCount[b][j]; k++) {
        uint *membershipReleased;
        uint  membershipReleasedCount;
        uint  membershipCountAlloc;
        membershipCountAlloc = 0;
        acquireReleasedMembership(b,
                                  treeID,
                                  branchID, 
                                  RF_root[treeID],
                                  RF_observation[treeID],
                                  RF_oobMembershipIndex[treeID],
                                  RF_oobMembershipIndex[treeID],
                                  RF_oobSize[treeID],
                                  & (pathPolarity[j][1]),
                                  VP_xReleaseIDArray[b][j][k],
                                  & membershipCountAlloc,
                                  & membershipReleased,
                                  & membershipReleasedCount);
        if(VP_oobCount[b][j] != 0) {
          VP_complementCount[b][j][k] = membershipReleasedCount - VP_oobCount[b][j];
          if(VP_complementCount[b][j][k] != 0) {
            VP_complementMembers[b][j][k] = uivector(1, VP_complementCount[b][j][k]);
            complement(VP_oobCount[b][j],
                       VP_oobMembers[b][j],
                       membershipReleasedCount,
                       membershipReleased,
                       VP_complementMembers[b][j][k]);
          }
          else {
            VP_complementMembers[b][j][k] = NULL;
          }
          if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
            getMortality(treeID,
                         (Terminal *) RF_tTermList[treeID][branchID],
                         VP_complementMembers[b][j][k],
                         VP_complementCount[b][j][k],
                         k,
                         FALSE);
          }
          else if (RF_rNonFactorCount > 0) {
            getMeanResponse(treeID,
                            (Terminal *) RF_tTermList[treeID][branchID],
                            VP_complementMembers[b][j][k],
                            VP_complementCount[b][j][k],
                            k,
                            FALSE);
          }
          else if (RF_rFactorCount > 0) {
            getMultiClassProb(treeID,
                              (Terminal *) RF_tTermList[treeID][branchID],
                              VP_complementMembers[b][j][k],
                              VP_complementCount[b][j][k],
                              k,
                              FALSE);
          }
        }
        else {
          VP_complementCount[b][j][k] = membershipReleasedCount;
          if(VP_complementCount[b][j][k] != 0) {
            VP_complementMembers[b][j][k] = uivector(1, VP_complementCount[b][j][k]);
            for(p = 1; p <= VP_complementCount[b][j][k]; p++) {
              VP_complementMembers[b][j][k][p] = membershipReleased[p];
            }
          }
          else {
            VP_complementMembers[b][j][k] = NULL;
          }
          if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
            getMortality(treeID,
                         (Terminal *) RF_tTermList[treeID][branchID],
                         VP_complementMembers[b][j][k],
                         VP_complementCount[b][j][k],
                         k,
                         FALSE);
          }
          else if (RF_rNonFactorCount > 0) {
            getMeanResponse(treeID,
                            (Terminal *) RF_tTermList[treeID][branchID],
                            VP_complementMembers[b][j][k],
                            VP_complementCount[b][j][k],
                            k,
                            FALSE);
          }
          else if (RF_rFactorCount > 0) {
            getMultiClassProb(treeID,
                              (Terminal *) RF_tTermList[treeID][branchID],
                              VP_complementMembers[b][j][k],
                              VP_complementCount[b][j][k],
                              k,
                              FALSE);
          }
        }
        if(membershipReleased != RF_oobMembershipIndex[treeID]) {
          free_uivector(membershipReleased, 1, membershipCountAlloc);
        }
      }
      free_cvector(pathPolarity[j], 1, VP_proxyIndvDepth[b][j]);
      free_cvector(releaseFlag[j], 1, RF_xSize);
      free_uivector(branchMembers, 1, RF_oobSize[treeID]);
    }
    free_new_vvector(pathPolarity, 1, branchCount, NRUTIL_CPTR);
    free_new_vvector(releaseFlag, 1, branchCount, NRUTIL_CPTR);
  }
  else {
  }
  free_uivector(bootMembrIndx, 1, bootMembrSize);
  rootBase -> repMembrIndx = NULL;
  rootBase -> repMembrSize = rootBase -> repMembrSizeAlloc = 0;
  unstackMembershipVectors(nSize,
                           RF_bootMembershipFlag[treeID],
                           RF_oobMembershipFlag[treeID],
                           RF_bootMembershipCount[treeID],
                           RF_ibgMembershipIndex[treeID],
                           RF_oobMembershipIndex[treeID]);
  freeTree(treeID, (NodeBase*) RF_root[treeID]);
}
void freeTree(uint treeID, NodeBase *parent) {
  if (parent != NULL) {
    if ((parent -> left) != NULL) {
      freeTree(treeID, parent -> left);
    }
    if ((parent -> right) != NULL) {
      freeTree(treeID, parent -> right);
    }
    freeNode((Node*) parent);
  }
}
void acquireProxyIndv(uint  treeID,
                      uint  branchID,
                      uint *genMembership,
                      uint  genMembershipSize,
                      uint *ibgMembership,
                      uint  ibgMembershipSize,
                      uint *branchMembers,
                      uint *branchMemberCount,
                      uint *indv,
                      uint *indvDepth) {
  uint  i;
  char result;
  result = FALSE;
  for (i = 1; i <= genMembershipSize; i++) {
    if((RF_tTermMembership[treeID][genMembership[i]] -> nodeID) == branchID) {
      if(result == FALSE) {
        *indv = genMembership[i];
        *indvDepth = RF_tTermMembership[treeID][genMembership[i]] -> mate -> depth;
        result = TRUE;
      }
      branchMembers[++(*branchMemberCount)] = genMembership[i];
    }
  }
  if(result == FALSE){
    for (i = 1; i <= ibgMembershipSize; i++) {
      if((RF_tTermMembership[treeID][ibgMembership[i]] -> nodeID) == branchID) {
        *indv = ibgMembership[i];
        *indvDepth = RF_tTermMembership[treeID][ibgMembership[i]] -> mate -> depth;
        break;
      }
    }
  }
}
void acquireBranch(uint      b,
                   uint      treeID,
                   uint      branchID,
                   uint      indv,
                   uint      nodeDepth,
                   NodeBase *parent,
                   double  **xArray,
                   uint     *xReleaseID,
                   char     *pathPolarity,
                   char     *releaseFlag,
                   uint     *xReleaseCount) {
  uint depth;
  SplitInfoMax *info;
  char parseFlag;
  char daughterFlag;
  void *gobsLocal;
  uint xvar;
  parseFlag = TRUE;
  depth = 0;
  while (parseFlag) {
    info = parent -> splitInfoMax;
    if (info != NULL) {
      gobsLocal = (double **) xArray;
      daughterFlag = getDaughterPolarityNew(treeID,
                                            info,
                                            indv,
                                            gobsLocal);
      if (daughterFlag == LEFT) {
        pathPolarity[++depth] = LEFT;
        parent = parent -> left;
      }
      else {
        pathPolarity[++depth] = RIGHT;
        parent = parent -> right;
      }
      xvar = info -> splitParameter;
      if(releaseFlag[xvar] == FALSE) {
        releaseFlag[xvar] = TRUE;
        xReleaseID[++(*xReleaseCount)] = xvar;
      }
    }  
    else {
      parseFlag = FALSE;
    }
  }  
}
void acquireReleasedMembership(uint      b,
                               uint      treeID,
                               uint      branchID, 
                               NodeBase *parent,
                               double  **xArray,
                               uint     *membershipStatic,
                               uint     *membership,
                               uint      membershipCount,
                               char     *pathPolarity,
                               uint      xReleaseID,
                               uint     *membershipCountAlloc,
                               uint    **membershipReleased,
                               uint     *membershipReleasedCount) {
  uint *newMembership;
  uint  newMembershipCount;
  SplitInfoMax *info;
  char daughterFlag;
  void *gobsLocal;
  uint i;
  info = parent -> splitInfoMax;
  if (info != NULL) {
    if(info -> splitParameter == xReleaseID) {
      newMembership = membership;
      newMembershipCount = membershipCount;
    }
    else {
      if(membershipCount != 0) {
        newMembership = uivector(1, membershipCount);
        newMembershipCount = 0;
        gobsLocal = (double **) xArray;
        for(i = 1; i <= membershipCount; i++) {
          daughterFlag = getDaughterPolarityNew(treeID,
                                                info,
                                                membership[i],
                                                gobsLocal);
          if((*pathPolarity) == daughterFlag) {
            newMembership[++newMembershipCount] = membership[i];
          }
        }
        if(membership != membershipStatic) {
          free_uivector(membership, 1, *membershipCountAlloc);
        }
        *membershipCountAlloc = membershipCount;
      }
      else {
        newMembership = membership;
        newMembershipCount = membershipCount;      
      }
    }
    if((*pathPolarity) == LEFT) {
      parent = parent -> left;
    }
    else {
      parent = parent -> right;
    }
    acquireReleasedMembership(b,
                              treeID,
                              branchID, 
                              parent,
                              xArray,
                              membershipStatic,
                              newMembership,
                              newMembershipCount,
                              ++pathPolarity,
                              xReleaseID,
                              membershipCountAlloc,
                              membershipReleased,
                              membershipReleasedCount);
  }
  else {
    *membershipReleased = membership;
    *membershipReleasedCount = membershipCount;
  }
}
