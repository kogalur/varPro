
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
#include "shared/error.h"
void acquireTree(char mode, uint b) {
  Node     *root;
  NodeBase *rootBase;
  uint nSize, xSize;
  uint  treeID;
  ulong rmbrIterator;
  ulong ambrIterator;
  uint  gmbrIterator;
  uint *bootMembrIndx;
  uint  bootMembrSize;
  char  bootResult;
  LeafLinkedObj *leafLinkedObjectPtr;
  uint branchCount;
  uint branchID;
  uint  i, k, j, p;
  treeID = VP_strengthTreeID[b];
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
                          RF_identityMembershipIndex, 
                          (RF_startTimeIndex == 0) ? RF_observationSize : RF_subjCount,  
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
    if (RF_startTimeIndex == 0) {
      rootBase -> repMembrSizeAlloc = rootBase -> repMembrSize = bootMembrSize;
      rootBase -> repMembrIndx = uivector(1, rootBase -> repMembrSizeAlloc);
      for (i = 1; i <= bootMembrSize; i++) {
        rootBase -> repMembrIndx[i] = bootMembrIndx[i];
      }
    }
    else {
      rootBase -> repMembrSizeAlloc = 0;
      for (i = 1; i <= bootMembrSize; i++) {
        k = bootMembrIndx[i];
        rootBase -> repMembrSizeAlloc += RF_subjSlotCount[k];
      }
      rootBase -> repMembrIndx = uivector(1, rootBase -> repMembrSizeAlloc);
      rootBase -> repMembrSize = 0;
      for (i = 1; i <= bootMembrSize; i++) {
        k = bootMembrIndx[i];
        for (j = 1; j <= RF_subjSlotCount[k]; j++) {
          rootBase -> repMembrIndx[++(rootBase -> repMembrSize)] = RF_subjList[k][j];
        }
      }
      SG_bootstrapSizeCase[treeID] = rootBase -> repMembrSize;
      SG_bootMembershipFlagCase[treeID] = cvector(1, nSize);
      SG_oobMembershipFlagCase[treeID] = cvector(1, nSize);
      for (i = 1; i <= nSize; i++) {
        SG_bootMembershipFlagCase[treeID][i]  = FALSE;
        SG_oobMembershipFlagCase[treeID][i]   = TRUE;
      }
      SG_ibgMembershipIndexCase[treeID] = uivector(1, nSize);
      SG_oobMembershipIndexCase[treeID] = uivector(1, nSize);
      for (i = 1; i <= rootBase -> repMembrSize; i++) {
        SG_bootMembershipFlagCase[treeID][rootBase -> repMembrIndx[i]] =  TRUE;
        SG_oobMembershipFlagCase[treeID][rootBase -> repMembrIndx[i]] =  FALSE;
      }
      SG_oobSizeCase[treeID] = 0;
      SG_ibgSizeCase[treeID] = 0;
      for (i = 1; i <= nSize; i++) {
        if (SG_bootMembershipFlagCase[treeID][i] == FALSE) {
          SG_oobMembershipIndexCase[treeID][++SG_oobSizeCase[treeID]] = i;
        }
        else {
          SG_ibgMembershipIndexCase[treeID][++SG_ibgSizeCase[treeID]] = i;
        }
      }
      if (RF_OOB_SZ_[treeID] != SG_oobSizeCase[treeID]) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  OOB terminal qualitative and RHF calculated size mismatch in tree %10d:  OOB_SZ=%10d  oobSize=%10d",
                       treeID, RF_OOB_SZ_[treeID], SG_oobSizeCase[treeID]);
        RF_nativeExit();
      }
      if (RF_OOB_SZ_[treeID] != SG_oobSizeCase[treeID]) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  IBG terminal qualitative and RHF calculated size mismatch in tree %10d:  IBG_SZ=%10d  ibgSize=%10d",
                       treeID, RF_IBG_SZ_[treeID], SG_ibgSizeCase[treeID]);
        RF_nativeExit();
      }
    }
    restoreTree(mode, treeID, RF_root[treeID]);
    if (RF_optHigh & OPT_MEMB_INCG) {
      uint *oobNodeOffset;
      uint *ibgNodeOffset;
      Terminal *termPtr;
      uint leafID;
      uint oobOffset;
      uint ibgOffset;
      oobNodeOffset = uivector(1, RF_tLeafCount_[treeID]);
      ibgNodeOffset = uivector(1, RF_tLeafCount_[treeID]);
      oobOffset = 0;
      ibgOffset = 0;
      for (i = 1; i <= RF_tLeafCount_[treeID]; i++) {
        oobNodeOffset[i] = oobOffset;
        ibgNodeOffset[i] = ibgOffset;
        oobOffset += VP_TN_OCNT_ptr[treeID][i];
        ibgOffset += VP_TN_ICNT_ptr[treeID][i];
      }
      if ((RF_OOB_SZ_ != NULL) && (oobOffset != RF_OOB_SZ_[treeID])) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  OOB offset mismatch in tree %10d:  offset=%10d  oobSZ=%10d",
                       treeID, oobOffset, RF_OOB_SZ_[treeID]);
        RF_nativeExit();
      }
      if ((RF_IBG_SZ_ != NULL) && (ibgOffset != RF_IBG_SZ_[treeID])) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  IBG offset mismatch in tree %10d:  offset=%10d  ibgSZ=%10d",
                       treeID, ibgOffset, RF_IBG_SZ_[treeID]);
        RF_nativeExit();
      }
      rmbrIterator = 0;
      ambrIterator = 0;
      leafLinkedObjectPtr = RF_leafLinkedObjHead[treeID];
      for(i = 1; i <= RF_tLeafCount_[treeID]; i++) {
        leafLinkedObjectPtr = leafLinkedObjectPtr -> fwdLink;
        termPtr = (Terminal *) (leafLinkedObjectPtr -> termPtr);
        leafID = ((TerminalBase *) termPtr) -> nodeID;
        termPtr -> repMembrCount = RF_TN_RCNT_ptr[treeID][leafID];
        termPtr -> repMembrIndx  = RF_RMBR_ID_ptr[treeID] + rmbrIterator;
        termPtr -> oobMembrCount = VP_TN_OCNT_ptr[treeID][leafID];
        termPtr -> oobMembrIndx  = (termPtr -> oobMembrCount > 0) ?
          (VP_OMBR_ID_ptr[treeID] + oobNodeOffset[leafID]) : NULL;
        termPtr -> ibgMembrCount = VP_TN_ICNT_ptr[treeID][leafID];
        termPtr -> ibgMembrIndx  = (termPtr -> ibgMembrCount > 0) ?
          (VP_IMBR_ID_ptr[treeID] + ibgNodeOffset[leafID]) : NULL;
        rmbrIterator += (termPtr -> repMembrCount);
        ambrIterator += (termPtr -> allMembrCount);
      }
      gmbrIterator = 0;
      leafLinkedObjectPtr = RF_leafLinkedObjHead[treeID];
      for(i = 1; i <= RF_tLeafCount_[treeID]; i++) {
        leafLinkedObjectPtr = leafLinkedObjectPtr -> fwdLink;
        termPtr = (Terminal *) (leafLinkedObjectPtr -> termPtr);
        leafID = ((TerminalBase *) termPtr) -> nodeID;
        gmbrIterator = 0;
        assignTerminalNodeMembership(mode,
                                     treeID,
                                     leafLinkedObjectPtr -> termPtr,
                                     termPtr -> ibgMembrIndx,
                                     termPtr -> ibgMembrCount,
                                     &gmbrIterator,
                                     RF_tTermMembership);
        gmbrIterator = 0;
        assignTerminalNodeMembership(mode,
                                     treeID,
                                     leafLinkedObjectPtr -> termPtr,
                                     termPtr -> oobMembrIndx,
                                     termPtr -> oobMembrCount,
                                     &gmbrIterator,
                                     RF_tTermMembership);
      }
      free_uivector(oobNodeOffset, 1, RF_tLeafCount_[treeID]);
      free_uivector(ibgNodeOffset, 1, RF_tLeafCount_[treeID]);      
    }
    if (VP_opt & ((VP_OPT_CMP | VP_OPT_OOB))) {
      if (RF_optHigh & OPT_TERM_INCG) {
        leafLinkedObjectPtr = RF_leafLinkedObjHead[treeID];
        for(i = 1; i <= RF_tLeafCount_[treeID]; i++) {
          leafLinkedObjectPtr = leafLinkedObjectPtr -> fwdLink;
          restoreTerminalNodeOutcomesVarPro(treeID, (Terminal *) (leafLinkedObjectPtr -> termPtr));
        }
      }
    }
    RF_tTermList[treeID] = (TerminalBase **) new_vvector(1, RF_tLeafCount_[treeID], NRUTIL_TPTR);
    leafLinkedObjectPtr = RF_leafLinkedObjHead[treeID] -> fwdLink;
    while (leafLinkedObjectPtr != NULL) {
      RF_tTermList[treeID][(leafLinkedObjectPtr -> termPtr) -> nodeID] = leafLinkedObjectPtr -> termPtr;
      leafLinkedObjectPtr = leafLinkedObjectPtr -> fwdLink;
    }
    if (mode  == RF_REST) {
      selectBranchesTrain(b,
                          treeID,
                          VP_maxRulesTree,
                          RF_tLeafCount_[treeID],
                          RF_tTermMembership[treeID],
                          RF_observationSize,
                          (VP_opt & VP_OPT_IBG) ? RF_ibgMembershipIndex[treeID] : RF_oobMembershipIndex[treeID],
                          (VP_opt & VP_OPT_IBG) ? RF_ibgSize[treeID] : RF_oobSize[treeID],
                          & VP_branchCount[b],
                          & VP_branchID[b],
                          & VP_branchMemberCount[b],
                          & VP_xReleaseCount[b],
                          & VP_xReleaseIDArray[b],
                          & VP_complementCount[b],
                          & VP_branchMembers[b],
                          & VP_complementMembers[b],
                          & VP_proxyIndv[b],
                          & VP_proxyIndvDepth[b]);
    }
    else {
      RF_ftTermMembership[treeID] = (TerminalBase **) new_vvector(1, RF_fobservationSize, NRUTIL_TPTR);
      getTestMembershipOldSchool(treeID, RF_fobservation[treeID], root, RF_fidentityMembershipIndex, RF_fidentityMembershipIndexSize, RF_ftTermMembership[treeID]);
      selectBranchesTest(b,
                         treeID,
                         VP_maxRulesTree,
                         RF_tLeafCount_[treeID],
                         RF_ftTermMembership[treeID],
                         RF_fobservationSize,
                         & VP_branchCount[b],
                         & VP_branchID[b],
                         & VP_branchMemberCount[b],
                         & VP_xReleaseCount[b],
                         & VP_xReleaseIDArray[b],
                         & VP_complementCount[b],
                         & VP_branchMembers[b],
                         & VP_complementMembers[b],
                         & VP_proxyIndv[b],
                         & VP_proxyIndvDepth[b]);
    }
    char **pathPolarity;
    char **releaseFlag;
    branchCount = VP_branchCount[b];
    pathPolarity           = (char **) new_vvector(1, branchCount, NRUTIL_CPTR);
    releaseFlag            = (char **) new_vvector(1, branchCount, NRUTIL_CPTR);
    for(j = 1; j <= branchCount; j++) { 
      branchID = VP_branchID[b][j];
      uint *branchMembers;
      acquireProxyIndv(mode,
                       treeID,
                       branchID,
                       & VP_proxyIndv[b][j],
                       & VP_proxyIndvDepth[b][j],
                       & VP_branchMemberCount[b][j],
                       & branchMembers);
      if(VP_branchMemberCount[b][j] > 0) {
        VP_branchMembers[b][j]      = uivector(1, VP_branchMemberCount[b][j]);
        for(p = 1; p <= VP_branchMemberCount[b][j]; p++) {
          VP_branchMembers[b][j][p] = branchMembers[p];
        }
      }
      else {
        VP_branchMembers[b][j]      = NULL;
      }
      if (VP_opt & ((VP_OPT_CMP | VP_OPT_OOB))) {
        if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
          getMortality(treeID,
                       (Terminal *) RF_tTermList[treeID][branchID],
                       VP_branchMembers[b][j],
                       VP_branchMemberCount[b][j],
                       0,
                       TRUE);
        }
        else if (RF_rNonFactorCount > 0) {
          getMeanResponse(treeID,
                          (Terminal *) RF_tTermList[treeID][branchID],
                          VP_branchMembers[b][j],
                          VP_branchMemberCount[b][j],
                          0,
                          TRUE);
        }
        else if (RF_rFactorCount > 0) {
          getMultiClassProb(treeID,
                            (Terminal *) RF_tTermList[treeID][branchID],
                            VP_branchMembers[b][j],
                            VP_branchMemberCount[b][j],
                            0,
                            TRUE);
        }
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
      if (VP_opt & ((VP_OPT_CMP | VP_OPT_OOB))) {
        if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
          stackCompMortalityOuter((Terminal *) RF_tTermList[treeID][branchID], VP_xReleaseCount[b][j]);
        }        
        else if (RF_rNonFactorCount > 0) {
          stackCompMeanResponseOuter((Terminal *) RF_tTermList[treeID][branchID], VP_xReleaseCount[b][j]);
        }
        else if(RF_rFactorCount > 0) {
          stackCompMultiClassOuter((Terminal *) RF_tTermList[treeID][branchID], VP_xReleaseCount[b][j]);
        }
      }
      double **xArray;
      uint *membrIndexStatic;
      uint *membrIndex;
      uint  subsetSize;
      if (VP_opt & VP_OPT_IBG) {
        xArray = RF_observation[treeID];
        if (RF_startTimeIndex == 0) {
          membrIndexStatic = RF_ibgMembershipIndex[treeID];
          membrIndex       = RF_ibgMembershipIndex[treeID];
          subsetSize       = RF_ibgSize[treeID];
        }
        else {
          membrIndexStatic = SG_ibgMembershipIndexCase[treeID];
          membrIndex       = SG_ibgMembershipIndexCase[treeID];
          subsetSize       = SG_ibgSizeCase[treeID];
        }
      }
      else {
        xArray = RF_observation[treeID];
        if (RF_startTimeIndex == 0) {
          membrIndexStatic = RF_oobMembershipIndex[treeID];
          membrIndex       = RF_oobMembershipIndex[treeID];
          subsetSize       = RF_oobSize[treeID];
        }
        else {
          membrIndexStatic = SG_oobMembershipIndexCase[treeID];
          membrIndex       = SG_oobMembershipIndexCase[treeID];
          subsetSize       = SG_oobSizeCase[treeID];
        }
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
                                  xArray,
                                  membrIndexStatic,
                                  membrIndex,
                                  subsetSize,
                                  & (pathPolarity[j][1]),
                                  VP_xReleaseIDArray[b][j][k],
                                  & membershipCountAlloc,
                                  & membershipReleased,
                                  & membershipReleasedCount);
        if (VP_branchMemberCount[b][j] > membershipReleasedCount) {
          RF_nativeError("\nVARPRO:  *** ERROR *** ");
          RF_nativeError("\nVARPRO:  Released count less than branch count:  (%10d < %10d) for (branch, tree) = (%10d, %10d).", membershipReleasedCount, VP_branchMemberCount[b][j], j, b);
          RF_nativeError("\nVARPRO:  Please Contact Technical Support.");
          RF_nativeExit();
        }
        if(VP_branchMemberCount[b][j] > 0) {
          VP_complementCount[b][j][k] = membershipReleasedCount - VP_branchMemberCount[b][j];
          if(VP_complementCount[b][j][k] != 0) {
            VP_complementMembers[b][j][k] = uivector(1, VP_complementCount[b][j][k]);
            complement(VP_branchMemberCount[b][j],
                       VP_branchMembers[b][j],
                       membershipReleasedCount,
                       membershipReleased,
                       VP_complementMembers[b][j][k]);
          }
          else {
            VP_complementMembers[b][j][k] = NULL;
          }
          if (VP_opt & ((VP_OPT_CMP | VP_OPT_OOB))) {
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
          if (VP_opt & ((VP_OPT_CMP | VP_OPT_OOB))) {
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
        }
        if(membershipReleased != membrIndexStatic) {
          free_uivector(membershipReleased, 1, membershipCountAlloc);
        }
      }
      free_cvector(pathPolarity[j], 1, VP_proxyIndvDepth[b][j]);
      free_cvector(releaseFlag[j], 1, RF_xSize);
      if (VP_branchMemberCount[b][j] > 0) {
        free_uivector(branchMembers, 1, VP_branchMemberCount[b][j]);
      }
    }
    free_new_vvector(pathPolarity, 1, branchCount, NRUTIL_CPTR);
    free_new_vvector(releaseFlag, 1, branchCount, NRUTIL_CPTR);
    if (RF_startTimeIndex > 0) {
      free_cvector(SG_bootMembershipFlagCase[treeID], 1, nSize);
      free_cvector(SG_oobMembershipFlagCase[treeID], 1, nSize);
      free_uivector(SG_oobMembershipIndexCase[treeID], 1, nSize);
      free_uivector(SG_ibgMembershipIndexCase[treeID], 1, nSize);
    }
  }
  else {
  }
  free_uivector(bootMembrIndx, 1, bootMembrSize);
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
void acquireProxyIndv(char  mode,
                      uint  treeID,
                      uint  branchID,
                      uint  *indv,
                      uint  *indvDepth,
                      uint  *branchMemberCount,
                      uint **branchMembers) {
  uint  i;
  Terminal *termPtr = (Terminal *) RF_tTermList[treeID][branchID];
  if (termPtr -> ibgMembrIndx != NULL) {
    *indv = termPtr -> ibgMembrIndx[1];
  }
  else if (termPtr -> oobMembrIndx != NULL) {
    *indv = termPtr -> oobMembrIndx[1];
  }
  else {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  IBG and OOB proxy member not found for (treeID, branchID, termID) = (%10d, %10d %10d)",
                       treeID, branchID, RF_tTermList[treeID][branchID] -> nodeID);
        RF_nativeExit();
  }
  *indvDepth = ((TerminalBase *) termPtr) -> mate -> depth;
  *branchMemberCount = 0;
  if (VP_opt & VP_OPT_IBG) {
    *branchMembers = uivector(1, termPtr -> ibgMembrCount);
    for (i = 1; i <= termPtr -> ibgMembrCount; i++) {
      (*branchMembers)[++(*branchMemberCount)] = termPtr -> ibgMembrIndx[i];
    }
  }
  else {
    if (termPtr -> oobMembrCount > 0) {
      *branchMembers = uivector(1, termPtr -> oobMembrCount);
      for (i = 1; i <= termPtr -> oobMembrCount; i++) {
        (*branchMembers)[++(*branchMemberCount)] = termPtr -> oobMembrIndx[i];
      }
    }
    else {
      *branchMembers = NULL;
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
void getTestMembershipOldSchool(uint treeID, double **xArray, Node *parent, uint *testMembrIndx, uint testMembrSize, TerminalBase **ftTermMembership) {
  NodeBase *parentBase;
  char daughterFlag;
  Terminal *termPtr;
  TerminalBase *termBasePtr;
  uint leftTestMembrSize, rghtTestMembrSize;
  uint *leftTestMembrIndx, *rghtTestMembrIndx;
  uint i;
  leftTestMembrIndx = rghtTestMembrIndx = NULL; 
  leftTestMembrSize = rghtTestMembrSize = 0;
  parentBase = (NodeBase *) parent;
  if (parentBase -> splitInfoMax != NULL) {
    leftTestMembrIndx   = uivector(1, testMembrSize);
    rghtTestMembrIndx   = uivector(1, testMembrSize);
    for (i = 1; i <= testMembrSize; i++) {
      daughterFlag = getDaughterPolarityNew(treeID,
                                            parentBase -> splitInfoMax,
                                            testMembrIndx[i],
                                            xArray);
      if (daughterFlag == LEFT) {
        leftTestMembrIndx[++leftTestMembrSize] = testMembrIndx[i];
      }
      else {
        rghtTestMembrIndx[++rghtTestMembrSize] = testMembrIndx[i];
      }
    }
  }
  else {
    termBasePtr = parentBase -> mate;
    termPtr = (Terminal *) termBasePtr;
    termPtr -> testMembrCount = testMembrSize;
    termPtr -> testMembrIndx = uivector(1, termPtr -> testMembrCount);
    for (i = 1; i <= termPtr -> testMembrCount; i++) {
      termPtr -> testMembrIndx[i] = testMembrIndx[i];
      ftTermMembership[testMembrIndx[i]] = termBasePtr;
    }
  }
  if (parentBase -> splitInfoMax != NULL) {
    if (leftTestMembrSize > 0) {
      getTestMembershipOldSchool(treeID, xArray, (Node*) (((NodeBase*) parent) -> left), leftTestMembrIndx, leftTestMembrSize, ftTermMembership);
    }
    if (rghtTestMembrSize > 0) {
      getTestMembershipOldSchool(treeID, xArray, (Node*) (((NodeBase*) parent) -> right), rghtTestMembrIndx, rghtTestMembrSize, ftTermMembership);
    }
    free_uivector(leftTestMembrIndx, 1, testMembrSize);
    free_uivector(rghtTestMembrIndx, 1, testMembrSize);
  }
}
