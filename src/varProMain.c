
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "varProMain.h"
#include "shared/error.h"
#include "shared/random.h"
#include "shared/stackPreDefined.h"
#include "shared/stackAuxiliaryInfo.h"
#include "shared/preprocessForestRecord.h"
#include "shared/stackIncoming.h"
#include "shared/nrutil.h"
#include "shared/nativeUtil.h"
#include "shared/sexpIO.h"
#include "shared/stack.h"
#include "varProAux.h"
#include "stackOutput.h"
#include "treeOps.h"
#include "nodeOps.h"
#include "termOps.h"
#include "sexpOutgoing.h"
char varProMain(char mode, int seedValue) {
  uint b, j, k;
  uint seedValueLC;
  char result;
  result = TRUE;
  seedValueLC    = 0; 
  if (seedValue >= 0) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Parameter verification failed.");
    RF_nativeError("\nRF-SRC:  Random seed must be less than zero.  \n");
    result = FALSE;
  }
  if (RF_ntree < 1) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Parameter verification failed.");
    RF_nativeError("\nRF-SRC:  Number of bootstrap iterations must be greater than zero:  %10d \n", RF_ntree);
    result = FALSE;
  }
  if (RF_observationSize < 1) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Parameter verification failed.");
    RF_nativeError("\nRF-SRC:  Number of individuals must be greater than one:  %10d \n", RF_observationSize);
    result = FALSE;
  }
  if (RF_xSize < 1) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Parameter verification failed.");
    RF_nativeError("\nRF-SRC:  Number of parameters must be greater than zero:  %10d \n", RF_xSize);
    result = FALSE;
  }
#ifdef _OPENMP
  if (RF_numThreads < 0) {
    RF_numThreads = omp_get_max_threads();
  }
  else if (RF_numThreads == 0) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Parameter verification failed.");
    RF_nativeError("\nRF-SRC:  Number of threads must not be zero:  %10d \n", RF_numThreads);
    result = FALSE;
  }
  else {
    RF_numThreads = (RF_numThreads < omp_get_max_threads()) ? (RF_numThreads) : (omp_get_max_threads());
  }
#endif
  if (result) {
    result = getStrengthTreeCount(mode,
                                  RF_ntree,
                                  VP_maxTree,
                                  RF_tLeafCount_,
                                  (VP_opt & VP_OPT_EXP2) ? NULL : RF_OOB_SZ_,
                                  & VP_strengthTreeCount);
  }
  if (result) {
    ran1A = &randomChainParallelA;
    ran1B = &randomChainParallelB;
    ran1C = &randomChainParallelC;
    randomSetChainA    = &randomSetChainParallelA;
    randomSetChainB    = &randomSetChainParallelB;
    randomSetChainC    = &randomSetChainParallelC;
    randomGetChainA    = &randomGetChainParallelA;
    randomGetChainB    = &randomGetChainParallelB;
    randomGetChainC    = &randomGetChainParallelC;
    stackRandom(RF_ntree, RF_ntree, RF_fobservationSize, 0);
    for (b = 1; b <= RF_ntree; b++) {
      randomSetChainA(b , RF_seed_[b]);
    }
    seedValueLC = abs(seedValue);
    lcgenerator(&seedValueLC, TRUE);
    for (b = 1; b <= RF_ntree; b++) {
      lcgenerator(&seedValueLC, FALSE);
      lcgenerator(&seedValueLC, FALSE);
      while(seedValueLC == 0) {
        lcgenerator(&seedValueLC, FALSE);
      }
      randomSetChainB(b, -seedValueLC);
    }
    for (uint r = 1; r <= RF_fobservationSize; r++) {
      lcgenerator(&seedValueLC, FALSE);
      lcgenerator(&seedValueLC, FALSE);
      while(seedValueLC == 0) {
        lcgenerator(&seedValueLC, FALSE);
      }
      randomSetChainC(r, -seedValueLC);
    }
    result = stackIncomingArrays(mode,
                                 RF_ntree,
                                 RF_timeInterestSize,
                                 RF_ytry,
                                 RF_mtry,
                                 RF_xWeight,
                                 RF_yWeight,
                                 RF_subjSize,
                                 RF_subjWeight,
                                 RF_xWeightStat,
                                 RF_nodeSize,
                                 RF_bootstrapSize,
                                 RF_splitRule,
                                 RF_quantileSize,
                                 RF_quantile,
                                 RF_ySize,
                                 RF_rType,
                                 RF_frSize,
                                 RF_subjIn,
                                 RF_observationSize,
                                 RF_responseIn,
                                 RF_fresponseIn,
                                 RF_xSize,
                                 RF_xType,
                                 RF_fobservationSize,
                                 RF_observationIn,
                                 RF_fobservationIn,
                                 &RF_yIndex,
                                 &RF_yIndexZero,
                                 &RF_timeIndex,
                                 &RF_startTimeIndex,
                                 &RF_statusIndex,
                                 &RF_masterTime,
                                 &RF_masterTimeSize,
                                 &RF_sortedTimeInterestSize,
                                 &RF_startMasterTimeIndexIn,
                                 &RF_masterTimeIndexIn,
                                 &RF_ptnCount,
                                 &RF_ySizeProxy,
                                 &RF_yIndexZeroSize);
    if (result) {
      result = stackPreDefinedCommonArrays(mode,
                                           RF_ntree,
                                           RF_subjWeight,
                                           RF_timeIndex,
                                           RF_startTimeIndex,
                                           RF_statusIndex,
                                           RF_bootstrapSize,
                                           RF_bootstrapIn,
                                           RF_subjSize,
                                           RF_ptnCount,
                                           RF_getTree,
                                           RF_observationSize,
                                           &RF_nodeMembership,
                                           &RF_tTermMembership,
                                           &RF_pNodeMembership,
                                           &RF_pTermMembership,
                                           &RF_hTermMembership,
                                           &RF_tTermList,
                                           &RF_pNodeList,
                                           &RF_pTermList,
                                           &RF_bootMembershipFlag,
                                           &RF_oobMembershipFlag,
                                           &RF_bootMembershipCount,
                                           &RF_ibgMembershipIndex,
                                           &RF_oobMembershipIndex,
                                           &RF_oobSize,
                                           &RF_ibgSize,
                                           &RF_bootMembershipIndex,
                                           &RF_maxDepth,
                                           &RF_orderedTreeIndex,
                                           &RF_serialTreeIndex,
                                           &RF_root,
                                           &RF_nodeCount,
                                           &RF_leafLinkedObjHead,
                                           &RF_leafLinkedObjTail,
                                           &RF_pLeafCount,
                                           &RF_getTreeIndex,
                                           &RF_getTreeCount,
                                           &RF_subjWeightType,
                                           &RF_subjWeightSorted,
                                           &RF_subjWeightDensitySize,
                                           &RF_identityMembershipIndexSize,
                                           &RF_identityMembershipIndex);
        if (result) {
          result = stackAndInitializeTimeAndSubjectArrays(mode,
                                                          RF_startTimeIndex,
                                                          RF_observationSize,
                                                          RF_responseIn,
                                                          RF_timeIndex,
                                                          RF_timeInterestSize,
                                                          RF_subjIn,
                                                          &RF_subjSize,
                                                          &RF_masterTime,
                                                          &RF_masterTimeIndexIn,
                                                          &RF_startMasterTimeIndexIn,
                                                          &RF_timeInterest,
                                                          &RF_masterTimeSize,
                                                          &RF_sortedTimeInterestSize,
                                                          &RF_masterToInterestTimeMap,
                                                          &RF_subjSlot,
                                                          &RF_subjSlotCount,
                                                          &RF_subjList,
                                                          &RF_caseMap,
                                                          &RF_subjMap,
                                                          &RF_subjCount);
          if (result) {
            stackFactorArrays(mode,
                              RF_rType,
                              RF_xType,
                              RF_ySize,
                              RF_xSize,
                              RF_xLevelsCnt,
                              RF_rTarget,
                              RF_rTargetCount,
                              RF_timeIndex,
                              RF_statusIndex,
                              &RF_rFactorCount,
                              &RF_xFactorCount,
                              &RF_rFactorMap,
                              &RF_xFactorMap,
                              &RF_rFactorIndex,
                              &RF_xFactorIndex,
                              &RF_rFactorSize,
                              &RF_xFactorSize,
                              &RF_rNonFactorCount,
                              &RF_xNonFactorCount,
                              &RF_rNonFactorMap,
                              &RF_xNonFactorMap,
                              &RF_rNonFactorIndex,
                              &RF_xNonFactorIndex,
                              &RF_rTargetFactor,
                              &RF_rTargetNonFactor,
                              &RF_rTargetFactorCount,
                              &RF_rTargetNonFactorCount,
                              &RF_xLevels);
            initializeFactorArrays(mode,
                                   RF_rFactorCount,
                                   RF_xFactorCount,
                                   RF_rFactorIndex,
                                   RF_xFactorIndex,
                                   RF_rLevelsMax,
                                   RF_xLevelsMax,
                                   RF_rLevelsCnt,
                                   RF_xLevelsCnt,
                                   RF_ntree,
                                   RF_rFactorSize,
                                   RF_xFactorSize,
                                   &RF_rMaxFactorLevel,
                                   &RF_xMaxFactorLevel,
                                   &RF_maxFactorLevel,
                                   &RF_factorList);
            stackTrainingDataArraysWithPass(mode,
                                            RF_ySize,
                                            RF_ntree,
                                            RF_responseIn,
                                            RF_startTimeIndex,
                                            RF_statusIndex,
                                            RF_timeIndex,
                                            RF_startMasterTimeIndexIn,
                                            RF_masterTimeIndexIn,
                                            RF_observationSize,
                                            RF_observationIn,
                                            & RF_response,
                                            & RF_time,
                                            & RF_startTime,
                                            & RF_startMasterTimeIndex,
                                            & RF_masterTimeIndex,
                                            & RF_status,
                                            & RF_observation,
                                            & RF_mStatusFlag,
                                            & RF_mTimeFlag,
                                            & RF_mResponseFlag,
                                            & RF_mPredictorFlag,
                                            & RF_mRecordSize,
                                            & RF_mRecordMap);
            if (mode == RF_PRED) {
              stackPreDefinedPredictArrays(RF_ntree,
                                           RF_fobservationSize,
                                           &RF_fidentityMembershipIndexSize,
                                           &RF_fidentityMembershipIndex,
                                           &RF_fnodeMembership,
                                           &RF_ftTermMembership);
              stackTestDataArraysWithPass(mode,
                                          RF_frSize,
                                          RF_ntree,
                                          RF_fresponseIn,
                                          RF_fobservationSize,
                                          RF_fobservationIn,
                                          &RF_fresponse,
                                          &RF_fobservation);
            }
            if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
              stackCompetingArrays(mode,
                                   RF_statusIndex,
                                   RF_splitRule,
                                   RF_eventTypeSize,
                                   RF_eventType,
                                   RF_crWeightSize,
                                   RF_crWeight,
                                   RF_frSize,
                                   RF_observationSize,
                                   RF_fobservationSize,
                                   RF_responseIn,
                                   RF_fresponseIn,
                                   RF_mRecordMap,
                                   RF_fmRecordMap,
                                   RF_mRecordSize,
                                   RF_fmRecordSize,
                                   RF_mpSign,
                                   RF_fmpSign,
                                   &RF_eventTypeIndex,
                                   &RF_feventTypeSize,
                                   &RF_mStatusSize,
                                   &RF_eIndividualSize,
                                   &RF_eIndividualIn);
            }
            if (RF_rFactorCount > 0) {
              stackClassificationArrays(mode,
                                        RF_rFactorSize,
                                        RF_rLevelsCnt,
                                        RF_rFactorCount,
                                        RF_observationSize,
                                        RF_responseIn,
                                        RF_rFactorIndex,
                                        RF_frSize,
                                        RF_fresponseIn,
                                        RF_fobservationSize,
                                        &RF_rLevels,
                                        &RF_classLevelSize,
                                        &RF_classLevel,
                                        &RF_classLevelIndex,
                                        &RF_rFactorThreshold,
                                        &RF_rFactorMinority,
                                        &RF_rFactorMajority,
                                        &RF_rFactorMinorityFlag);
            }
            RF_auxDimConsts = makeAuxDimConsts(RF_rFactorSize,
                                               RF_rFactorCount,
                                               RF_rFactorMap,
                                               RF_rTargetFactor,
                                               RF_rTargetFactorCount,
                                               RF_tLeafCount_,
                                               RF_holdBLKptr);
            preprocessForestRecord(RF_ntree,
                                   RF_totalNodeCount_,
                                   RF_treeID_,
                                   RF_nodeID_,
                                   RF_parmID_,
                                   RF_mwcpSZ_,
                                   RF_tLeafCount_,
                                   RF_nodeSZ_,
                                   RF_restoreTreeID,
                                   RF_restoreTreeOffset,
                                   RF_nodeCount,
                                   RF_mwcpCT,
                                   RF_restoreMWCPoffset,
                                   & RF_totalTermCount);
            if ((RF_optHigh & OPT_MEMB_INCG) || (RF_optHigh & OPT_TERM_INCG)) {
              RF_incomingStackCount = 0;
              stackAuxiliaryInfoList(&RF_incomingAuxiliaryInfoList, 8);
              if (RF_optHigh & OPT_MEMB_INCG) {
                stackTNQualitativeIncoming(mode,
                                           RF_auxDimConsts,
                                           RF_incomingAuxiliaryInfoList,
                                           RF_ntree,
                                           RF_bootstrapSize,
                                           RF_observationSize,
                                           RF_sexpStringIO,
                                           RF_RMBR_ID_,
                                           RF_AMBR_ID_,
                                           RF_TN_RCNT_,
                                           RF_TN_ACNT_,
                                           &RF_incomingStackCount,
                                           &RF_RMBR_ID_ptr,
                                           &RF_AMBR_ID_ptr,
                                           &RF_TN_RCNT_ptr,
                                           &RF_TN_ACNT_ptr);
              }
              else {
                RF_nativeError("\nRF-SRC:  *** ERROR *** ");
                RF_nativeError("\nRF-SRC:  VarPro now needs OPT_MEMB_INCG asserted, as a minimum.");
                RF_nativeError("\nRF-SRC:  These are contained in obj$terminal.qualts.");
                RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
                RF_nativeExit();
              }
              if (RF_optHigh & OPT_TERM_INCG) {              
                stackTNQuantitativeIncoming(mode,
                                            RF_auxDimConsts,
                                            RF_incomingAuxiliaryInfoList,
                                            RF_ntree,
                                            RF_sexpStringIO,
                                            RF_timeIndex,
                                            RF_startTimeIndex,
                                            RF_statusIndex,
                                            RF_rFactorCount,
                                            RF_rNonFactorCount,
                                            RF_eventTypeSize,
                                            RF_sortedTimeInterestSize,
                                            RF_tLeafCount_,
                                            RF_TN_MORT_,
                                            RF_TN_SURV_,
                                            RF_TN_NLSN_,
                                            RF_TN_CSHZ_,
                                            RF_TN_CIFN_,
                                            RF_TN_KHZF_,
                                            RF_TN_REGR_,
                                            RF_TN_CLAS_,
                                            &RF_incomingStackCount,
                                            &RF_TN_MORT_ptr,
                                            &RF_TN_SURV_ptr,
                                            &RF_TN_NLSN_ptr,
                                            &RF_TN_CSHZ_ptr,
                                            &RF_TN_CIFN_ptr,
                                            &RF_TN_KHZF_ptr,
                                            &RF_TN_REGR_ptr,
                                            &RF_TN_CLAS_ptr);
              }
            }
            else {
              RF_nativeError("\nRF-SRC:  *** ERROR *** ");
              RF_nativeError("\nRF-SRC:  VarPro now needs OPT_MEMB_INCG asserted, as a minimum.");
              RF_nativeError("\nRF-SRC:  These are contained in obj$terminal.qualts.");
              RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
              RF_nativeExit();
            }
            stackStrengthObjectsPtrOnly(mode,
                                        VP_strengthTreeCount,                              
                                        & VP_strengthTreeID,
                                        & VP_branchCount,
                                        & VP_branchID,
                                        & VP_branchMemberCount,
                                        & VP_xReleaseCount,
                                        & VP_xReleaseIDArray,
                                        & VP_complementCount,
                                        & VP_branchMembers,
                                        & VP_complementMembers,
                                        & VP_proxyIndv,
                                        & VP_proxyIndvDepth);
            selectTrees(RF_ntree,
                        VP_strengthTreeCount,
                        RF_tLeafCount_,
                        RF_OOB_SZ_,
                        VP_strengthTreeID);
            makeNode = & makeNodeDerived;
            freeNode = & freeNodeDerived;
            makeTerminal = & makeTerminalDerived;
            freeTerminal = & freeTerminalDerived;
            stackTNQualitativeIncomingVP(mode,
                                         RF_tLeafCount_,
                                         RF_RMBR_ID_,
                                         RF_OMBR_ID_,
                                         RF_IMBR_ID_,
                                         RF_TN_RCNT_,
                                         RF_TN_OCNT_,
                                         RF_TN_ICNT_,
                                         & VP_RMBR_ID_ptr,
                                         & VP_OMBR_ID_ptr,
                                         & VP_IMBR_ID_ptr,
                                         & VP_TN_RCNT_ptr,
                                         & VP_TN_OCNT_ptr,
                                         & VP_TN_ICNT_ptr);
#ifdef _OPENMP
#pragma omp parallel for num_threads(RF_numThreads)
#endif
            for (b = 1; b <= VP_strengthTreeCount; b++) {
              acquireTree(mode, b);
            }
            unstackTNQualitativeIncomingVP(mode,
                                           RF_tLeafCount_,
                                           VP_RMBR_ID_ptr,
                                           VP_OMBR_ID_ptr,
                                           VP_IMBR_ID_ptr,
                                           VP_TN_RCNT_ptr,
                                           VP_TN_OCNT_ptr,
                                           VP_TN_ICNT_ptr);
            if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
              RF_stackCount = 9;
              if (!(VP_opt & (VP_OPT_CMP | VP_OPT_OOB))) {
                RF_stackCount --;
              }
            }
            else if (RF_rNonFactorCount > 0) {
              RF_stackCount = 9;
              if (!(VP_opt & (VP_OPT_CMP | VP_OPT_OOB))) {
                RF_stackCount --;
              }
            }
            else if (RF_rFactorCount > 0) {
              RF_stackCount = 9;    
              if (!(VP_opt & (VP_OPT_CMP | VP_OPT_OOB))) {
                RF_stackCount --;
              }
            }
            else {
              RF_stackCount = 8;
            }
            if (mode == RF_PRED) {
              RF_stackCount += 3;
            }
            RF_stackCount++;
            initProtect(RF_stackCount);
            stackAuxiliaryInfoList(&RF_snpAuxiliaryInfoList, RF_stackCount);
            VP_cpuTime_ = (double*) stackAndProtect(RF_auxDimConsts,
                                                    mode,
                                                    &RF_nativeIndex,
                                                    NATIVE_TYPE_NUMERIC,
                                                    RF_CPU_TIME,
                                                    1,
                                                    0,
                                                    RF_sexpStringIO,
                                                    NULL,
                                                    1,
                                                    1);
            VP_cpuTime_ --;
            VP_totalRecordCount = 0;
            for(b = 1; b <= VP_strengthTreeCount; b++) {
              for(j = 1; j <= VP_branchCount[b]; j++) {
                for(k = 1; k <= VP_xReleaseCount[b][j]; k++) {
                  VP_totalRecordCount++;
                }
              }
            }
            VP_treeID_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                 mode,
                                                 &RF_nativeIndex,
                                                 NATIVE_TYPE_INTEGER,
                                                 VP_TREE_ID,
                                                 VP_totalRecordCount,
                                                 0,
                                                 VP_sexpStringOutgoing,
                                                 NULL,
                                                 1,
                                                 VP_totalRecordCount);
            VP_treeID_ --;
            VP_nodeID_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                 mode,
                                                 &RF_nativeIndex,
                                                 NATIVE_TYPE_INTEGER,
                                                 VP_NODE_ID,
                                                 VP_totalRecordCount,
                                                 0,
                                                 VP_sexpStringOutgoing,
                                                 NULL,
                                                 1,
                                                 VP_totalRecordCount);
            VP_nodeID_ --;
            VP_xReleaseID_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                     mode,
                                                     &RF_nativeIndex,
                                                     NATIVE_TYPE_INTEGER,
                                                     VP_XVAR_ID,
                                                     VP_totalRecordCount,
                                                     0,
                                                     VP_sexpStringOutgoing,
                                                     NULL,
                                                     1,
                                                     VP_totalRecordCount);
            VP_xReleaseID_ --;
            VP_strengthTreeID_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                         mode,
                                                         &RF_nativeIndex,
                                                         NATIVE_TYPE_INTEGER,
                                                         VP_STRN_TID,
                                                         VP_strengthTreeCount,
                                                         0,
                                                         VP_sexpStringOutgoing,
                                                         NULL,
                                                         1,
                                                         VP_strengthTreeCount);
            VP_strengthTreeID_ --;
            for(b = 1; b <= VP_strengthTreeCount; b++) {
              VP_strengthTreeID_[b] = VP_strengthTreeID[b];
            }
            uint localSize;
            uint membershipSize;
            if (mode == RF_PRED) {
              localSize = VP_strengthTreeCount * RF_fobservationSize;
              VP_testCaseNodeID_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                           mode,
                                                           &RF_nativeIndex,
                                                           NATIVE_TYPE_INTEGER,
                                                           VP_TWIN_TID,
                                                           localSize,
                                                           0,
                                                           VP_sexpStringOutgoing,
                                                           &VP_testCaseNodeIDptr,
                                                           2,
                                                           VP_strengthTreeCount,
                                                           RF_fobservationSize);
              VP_testCaseNodeID_ --;
              for (uint b = 1; b <= VP_strengthTreeCount; b++) {
                for (uint i = 1; i <= RF_fobservationSize; i++) {
                  VP_testCaseNodeIDptr[b][i] = RF_ftTermMembership[VP_strengthTreeID[b]][i] -> nodeID;
                }
              }
              if ((VP_neighbourSize < 1) || (VP_neighbourSize > RF_observationSize)) {
                RF_nativeError("\nRF-SRC:  *** ERROR *** ");
                RF_nativeError("\nRF-SRC:  Parameter verification failed.");
                RF_nativeError("\nRF-SRC:  Neighbour size must be greater than zero and less than n:  %10d \n", VP_neighbourSize);
                RF_nativeExit();
              }
              localSize = RF_fobservationSize * VP_neighbourSize;
              VP_twinStat_ = (double*) stackAndProtect(RF_auxDimConsts,
                                                       mode,
                                                       &RF_nativeIndex,
                                                       NATIVE_TYPE_NUMERIC,
                                                       VP_TWIN_STAT,
                                                       localSize,
                                                       0,
                                                       VP_sexpStringOutgoing,
                                                       & VP_twinStat_ptr,
                                                       2,
                                                       RF_fobservationSize,
                                                       VP_neighbourSize);
              localSize = RF_fobservationSize * VP_neighbourSize;
              VP_twinStatID_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                     mode,
                                                     &RF_nativeIndex,
                                                     NATIVE_TYPE_INTEGER,
                                                     VP_TWIN_STAT_ID,
                                                     localSize,
                                                     0,
                                                     VP_sexpStringOutgoing,
                                                     & VP_twinStatID_ptr,
                                                     2,
                                                     RF_fobservationSize,
                                                     VP_neighbourSize);
              char *xReduceFlag = cvector(1, RF_xSize);
              if (VP_xReduceSize > 0) {
                for (uint i = 1; i <= RF_xSize; i++) {
                  xReduceFlag[i] = FALSE;
                }
                for (uint i = 1; i <= VP_xReduceSize; i++) {
                  xReduceFlag[VP_xReduceIndx[i]] = TRUE;
                }
              }
              else {
                for (uint i = 1; i <= RF_xSize; i++) {
                  xReduceFlag[i] = TRUE;
                }
              }
#ifdef _OPENMP
#pragma omp parallel for num_threads(RF_numThreads)
#endif
              for (b = 1; b <= RF_fobservationSize; b++) {
                acquireTwinStat(VP_strengthTreeCount,
                                VP_branchCount,
                                VP_complementCount,
                                VP_xReleaseCount,
                                VP_branchID,
                                VP_testCaseNodeIDptr,
                                VP_xReleaseIDArray,
                                VP_complementMembers,
                                RF_observationSize,
                                RF_xSize,
                                b,
                                VP_neighbourSize,
                                xReduceFlag,
                                VP_twinStat_ptr,
                                VP_twinStatID_ptr);
              }
              free_cvector(xReduceFlag, 1, RF_xSize);
            }
            if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
              localSize = VP_totalRecordCount * 1;
              VP_dimImpSRVptr = NULL;
              VP_brmCT_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                  mode,
                                                  &RF_nativeIndex,
                                                  NATIVE_TYPE_INTEGER,
                                                  VP_BRAN_CT,
                                                  VP_totalRecordCount,
                                                  0,
                                                  VP_sexpStringOutgoing,
                                                  NULL,
                                                  1,
                                                  VP_totalRecordCount);
              VP_brmCT_ --;
              if ((VP_opt & VP_OPT_CMP) && (VP_opt & VP_OPT_OOB)) {
                VP_importance_ = (double*) stackAndProtect(RF_auxDimConsts,
                                                           mode,
                                                           &RF_nativeIndex,
                                                           NATIVE_TYPE_NUMERIC,
                                                           VP_STAT_IMP,
                                                           localSize,
                                                           0,
                                                           VP_sexpStringOutgoing,
                                                           & VP_dimImpSRVptr,
                                                           2,
                                                           1,
                                                           VP_totalRecordCount);
                VP_importance_ --;
              }
              else if ((VP_opt & VP_OPT_CMP) && !(VP_opt & VP_OPT_OOB)) {
                VP_complementStat_ = (double*) stackAndProtect(RF_auxDimConsts,
                                                               mode,
                                                               &RF_nativeIndex,
                                                               NATIVE_TYPE_NUMERIC,
                                                               VP_STAT_CMP,
                                                               localSize,
                                                               0,
                                                               VP_sexpStringOutgoing,
                                                               & VP_dimImpSRVptr,
                                                               2,
                                                               1,
                                                               VP_totalRecordCount);
                VP_complementStat_ --;
              }
              else if ((VP_opt & VP_OPT_OOB) && !(VP_opt & VP_OPT_CMP)) {
                VP_oobStat_ = (double*) stackAndProtect(RF_auxDimConsts,
                                                        mode,
                                                        &RF_nativeIndex,
                                                        NATIVE_TYPE_NUMERIC,
                                                        VP_STAT_OOB,
                                                        localSize,
                                                        0,
                                                        VP_sexpStringOutgoing,
                                                        & VP_dimImpSRVptr,
                                                        2,
                                                        RF_rNonFactorCount,
                                                        VP_totalRecordCount);
                VP_oobStat_ --;
              }
              writeStrengthArray(VP_strengthTreeID,
                                 VP_strengthTreeCount,
                                 VP_branchID,
                                 VP_branchCount,
                                 VP_branchMemberCount,
                                 VP_complementCount,
                                 VP_xReleaseCount,
                                 VP_xReleaseIDArray,
                                 VP_treeID_,
                                 VP_nodeID_,
                                 VP_xReleaseID_,
                                 VP_brmCT_,
                                 VP_dimImpSRVptr);
            }
            else if (RF_rNonFactorCount > 0) {
              localSize = VP_totalRecordCount * RF_rTargetNonFactorCount;
              VP_dimImpRGRptr = NULL;
              VP_brmCT_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                  mode,
                                                  &RF_nativeIndex,
                                                  NATIVE_TYPE_INTEGER,
                                                  VP_BRAN_CT,
                                                  VP_totalRecordCount,
                                                  0,
                                                  VP_sexpStringOutgoing,
                                                  NULL,
                                                  1,
                                                  VP_totalRecordCount);
              VP_brmCT_ --;
              if ((VP_opt & VP_OPT_CMP) && (VP_opt & VP_OPT_OOB)) {
                VP_importance_ = (double*) stackAndProtect(RF_auxDimConsts,
                                                           mode,
                                                           &RF_nativeIndex,
                                                           NATIVE_TYPE_NUMERIC,
                                                           VP_STAT_IMP,
                                                           localSize,
                                                           0,
                                                           VP_sexpStringOutgoing,
                                                           & VP_dimImpRGRptr,
                                                           2,
                                                           RF_rNonFactorCount,
                                                           VP_totalRecordCount);
                VP_importance_ --;
              }
              else if ((VP_opt & VP_OPT_CMP) && !(VP_opt & VP_OPT_OOB)) {
                VP_complementStat_ = (double*) stackAndProtect(RF_auxDimConsts,
                                                               mode,
                                                               &RF_nativeIndex,
                                                               NATIVE_TYPE_NUMERIC,
                                                               VP_STAT_CMP,
                                                               localSize,
                                                               0,
                                                               VP_sexpStringOutgoing,
                                                               & VP_dimImpRGRptr,
                                                               2,
                                                               RF_rNonFactorCount,
                                                               VP_totalRecordCount);
                VP_complementStat_ --;
              }
              else if ((VP_opt & VP_OPT_OOB) && !(VP_opt & VP_OPT_CMP)) {
                VP_oobStat_ = (double*) stackAndProtect(RF_auxDimConsts,
                                                        mode,
                                                        &RF_nativeIndex,
                                                        NATIVE_TYPE_NUMERIC,
                                                        VP_STAT_OOB,
                                                        localSize,
                                                        0,
                                                        VP_sexpStringOutgoing,
                                                        & VP_dimImpRGRptr,
                                                        2,
                                                        RF_rNonFactorCount,
                                                        VP_totalRecordCount);
                VP_oobStat_ --;
              }
              writeStrengthArray(VP_strengthTreeID,
                                 VP_strengthTreeCount,
                                 VP_branchID,
                                 VP_branchCount,
                                 VP_branchMemberCount,
                                 VP_complementCount,
                                 VP_xReleaseCount,
                                 VP_xReleaseIDArray,
                                 VP_treeID_,
                                 VP_nodeID_,
                                 VP_xReleaseID_,
                                 VP_brmCT_,
                                 VP_dimImpRGRptr);
            }
            else if (RF_rFactorCount > 0) {
              localSize = VP_totalRecordCount;
              for (j = 1; j <= RF_rTargetFactorCount; j++) {
                for (k = 1; k <= RF_rFactorSize[RF_rFactorMap[RF_rTargetFactor[j]]]; k++) {
                  localSize += VP_totalRecordCount;
                }
              }
              VP_dimImpCLSptr = NULL;
              VP_brmCT_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                  mode,
                                                  &RF_nativeIndex,
                                                  NATIVE_TYPE_INTEGER,
                                                  VP_BRAN_CT,
                                                  localSize,
                                                  0,
                                                  VP_sexpStringOutgoing,
                                                  & VP_brmCTptr,
                                                  3,
                                                  1,
                                                  -1,
                                                  VP_totalRecordCount);
              VP_brmCT_ --;
              if ((VP_opt & VP_OPT_CMP) && (VP_opt & VP_OPT_OOB)) {
                VP_importance_ = (double*) stackAndProtect(RF_auxDimConsts,
                                                           mode,
                                                           &RF_nativeIndex,
                                                           NATIVE_TYPE_NUMERIC,
                                                           VP_STAT_IMP,
                                                           localSize,
                                                           0,
                                                           VP_sexpStringOutgoing,
                                                           & VP_dimImpCLSptr,
                                                           3,
                                                           1,
                                                           -1,
                                                           VP_totalRecordCount);
                VP_importance_ --;
              }
              else if ((VP_opt & VP_OPT_CMP) && !(VP_opt & VP_OPT_OOB)) {
                VP_complementStat_ = (double*) stackAndProtect(RF_auxDimConsts,
                                                               mode,
                                                               &RF_nativeIndex,
                                                               NATIVE_TYPE_NUMERIC,
                                                               VP_STAT_CMP,
                                                               localSize,
                                                               0,
                                                               VP_sexpStringOutgoing,
                                                               & VP_dimImpCLSptr,
                                                               3,
                                                               1,
                                                               -1,
                                                               VP_totalRecordCount);
                VP_complementStat_ --;
              }
              else if ((VP_opt & VP_OPT_OOB) && !(VP_opt & VP_OPT_CMP)) {
                VP_oobStat_ = (double*) stackAndProtect(RF_auxDimConsts,
                                                        mode,
                                                        &RF_nativeIndex,
                                                        NATIVE_TYPE_NUMERIC,
                                                        VP_STAT_OOB,
                                                        localSize,
                                                        0,
                                                        VP_sexpStringOutgoing,
                                                        & VP_dimImpCLSptr,
                                                        3,
                                                        1,
                                                        -1,
                                                        VP_totalRecordCount);
                VP_oobStat_ --;
              }
              writeStrengthArray(VP_strengthTreeID,
                                 VP_strengthTreeCount,
                                 VP_branchID,
                                 VP_branchCount,
                                 VP_branchMemberCount,
                                 VP_complementCount,
                                 VP_xReleaseCount,
                                 VP_xReleaseIDArray,
                                 VP_treeID_,
                                 VP_nodeID_,
                                 VP_xReleaseID_,
                                 VP_brmCTptr,
                                 VP_dimImpCLSptr);
            }
            else {
              VP_brmCT_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                  mode,
                                                  &RF_nativeIndex,
                                                  NATIVE_TYPE_INTEGER,
                                                  VP_BRAN_CT,
                                                  VP_totalRecordCount,
                                                  0,
                                                  VP_sexpStringOutgoing,
                                                  NULL,
                                                  1,
                                                  VP_totalRecordCount);
              VP_brmCT_ --;
              writeStrengthArray(VP_strengthTreeID,
                                 VP_strengthTreeCount,
                                 VP_branchID,
                                 VP_branchCount,
                                 VP_branchMemberCount,
                                 VP_complementCount,
                                 VP_xReleaseCount,
                                 VP_xReleaseIDArray,
                                 VP_treeID_,
                                 VP_nodeID_,
                                 VP_xReleaseID_,
                                 VP_brmCT_,
                                 NULL);
            }
            VP_complementCT_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                       mode,
                                                       &RF_nativeIndex,
                                                       NATIVE_TYPE_INTEGER,
                                                       VP_COMP_CT,
                                                       VP_totalRecordCount,
                                                       0,
                                                       VP_sexpStringOutgoing,
                                                       NULL,
                                                       1,
                                                       VP_totalRecordCount);
            VP_complementCT_ --;
            membershipSize = 0;
            for (b = 1; b <= VP_strengthTreeCount; b++) {
              for(j = 1; j <= VP_branchCount[b]; j++) {
                membershipSize += VP_branchMemberCount[b][j];
              }
            }
            if (membershipSize == 0) {
              membershipSize ++;
            }
            VP_branchPopID_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                      mode,
                                                      &RF_nativeIndex,
                                                      NATIVE_TYPE_INTEGER,
                                                      VP_BRAN_MEM,
                                                      membershipSize,
                                                      0,
                                                      VP_sexpStringOutgoing,
                                                      NULL,
                                                      1,
                                                      membershipSize);
            VP_branchPopID_ --;
            membershipSize = 0;
            for (b = 1; b <= VP_strengthTreeCount; b++) {
              for(j = 1; j <= VP_branchCount[b]; j++) {
                for(k = 1; k <= VP_xReleaseCount[b][j]; k++) {
                  membershipSize += VP_complementCount[b][j][k];
                }
              }
            }
            if (membershipSize == 0) {
              membershipSize ++;
            }
            VP_complementID_ = (uint*) stackAndProtect(RF_auxDimConsts,
                                                       mode,
                                                       &RF_nativeIndex,
                                                       NATIVE_TYPE_INTEGER,
                                                       VP_COMP_MEM,
                                                       membershipSize,
                                                       0,
                                                       VP_sexpStringOutgoing,
                                                       NULL,
                                                       1,
                                                       membershipSize);
            VP_complementID_ --;
            writeMembershipArray(VP_strengthTreeCount,
                                 VP_branchCount,
                                 VP_branchMemberCount,
                                 VP_complementCount,
                                 VP_xReleaseCount,
                                 VP_branchMembers,
                                 VP_complementMembers,
                                 VP_complementCT_,
                                 VP_branchPopID_,
                                 VP_complementID_);
            for (uint bb = 1; bb <= VP_strengthTreeCount; bb++) {
              if(RF_tTermList[VP_strengthTreeID[bb]] != NULL) {
                free_new_vvector(RF_tTermList[VP_strengthTreeID[bb]], 1, RF_tLeafCount_[VP_strengthTreeID[bb]], NRUTIL_TPTR);
              }
              freeLeafLinkedObjList(RF_leafLinkedObjHead[VP_strengthTreeID[bb]]);
              free_new_vvector(RF_tTermMembership[VP_strengthTreeID[bb]], 1, RF_observationSize, NRUTIL_TPTR);
              if (mode == RF_PRED) {
                free_new_vvector(RF_ftTermMembership[VP_strengthTreeID[bb]], 1, RF_fobservationSize, NRUTIL_TPTR);
              }
            }
            freeStrengthBranchIDVectors(VP_strengthTreeCount,
                                        VP_branchCount,
                                        VP_branchID,
                                        VP_branchMemberCount,
                                        VP_xReleaseCount,
                                        VP_xReleaseIDArray,
                                        VP_complementCount,
                                        VP_branchMembers,
                                        VP_complementMembers,
                                        VP_proxyIndv,
                                        VP_proxyIndvDepth);
            unstackStrengthObjectsPtrOnly(mode,
                                          VP_strengthTreeCount,
                                          VP_strengthTreeID,
                                          VP_branchCount,
                                          VP_branchID,
                                          VP_branchMemberCount,
                                          VP_xReleaseCount,
                                          VP_xReleaseIDArray,
                                          VP_complementCount,
                                          VP_branchMembers,
                                          VP_complementMembers,
                                          VP_proxyIndv,
                                          VP_proxyIndvDepth);
            unstackAuxiliaryInfoAndList(RF_auxDimConsts, TRUE, RF_snpAuxiliaryInfoList, RF_stackCount);
            if ((RF_optHigh & OPT_MEMB_INCG) || (RF_optHigh & OPT_TERM_INCG)) {
              unstackAuxiliaryInfoAndList(RF_auxDimConsts, FALSE, RF_incomingAuxiliaryInfoList, 8);
            }
            freeAuxDimConsts(RF_auxDimConsts);
            if (RF_rFactorCount > 0) {
              unstackClassificationArrays(mode,
                                          RF_rFactorSize,
                                          RF_rFactorCount,
                                          RF_rLevels,
                                          RF_classLevelSize,
                                          RF_classLevel,
                                          RF_classLevelIndex,
                                          RF_rFactorThreshold,
                                          RF_rFactorMinority,
                                          RF_rFactorMajority,
                                          RF_rFactorMinorityFlag);
            }
            if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
              unstackCompetingArrays(mode,
                                     RF_statusIndex,
                                     RF_eventTypeSize,
                                     RF_eventType,
                                     RF_feventTypeSize,
                                     RF_eventTypeIndex,
                                     RF_mStatusSize,
                                     RF_eIndividualSize,
                                     RF_eIndividualIn);
            }
            unstackFactorArrays(mode,
                                RF_ntree,
                                RF_ySize,
                                RF_xSize,
                                RF_rTarget,
                                RF_rTargetCount,
                                RF_rTargetFactor,
                                RF_rTargetNonFactor,
                                RF_timeIndex,
                                RF_statusIndex,
                                RF_rFactorCount,
                                RF_xFactorCount,
                                RF_rFactorMap,
                                RF_xFactorMap,
                                RF_rFactorIndex,
                                RF_xFactorIndex,
                                RF_rFactorSize,
                                RF_xFactorSize,
                                RF_rNonFactorCount,
                                RF_xNonFactorCount,
                                RF_rNonFactorMap,
                                RF_xNonFactorMap,
                                RF_rNonFactorIndex,
                                RF_xNonFactorIndex,
                                RF_xLevels,
                                RF_factorList);
            unstackTrainingDataArraysWithPass(mode,
                                              RF_ySize,
                                              RF_ntree,
                                              RF_timeIndex,
                                              RF_statusIndex,
                                              RF_startTimeIndex,
                                              RF_response,
                                              RF_time,
                                              RF_masterTimeIndex,
                                              RF_startTime,
                                              RF_startMasterTimeIndex,
                                              RF_status,
                                              RF_observation);
            if (mode == RF_PRED) {
              unstackPreDefinedPredictArrays(RF_ntree,
                                             RF_fobservationSize,
                                             RF_fidentityMembershipIndexSize,
                                             RF_fidentityMembershipIndex,
                                             RF_fnodeMembership,
                                             RF_ftTermMembership);
              unstackTestDataArraysWithPass(mode,
                                            RF_ntree,
                                            RF_fresponse,
                                            RF_fobservation);
            }
          }
          if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
            unstackTimeAndSubjectArrays(mode,
                                        RF_startTimeIndex,
                                        RF_observationSize,
                                        RF_masterTime,
                                        RF_masterTimeIndexIn,
                                        RF_masterTimeSize,
                                        RF_startMasterTimeIndexIn,
                                        RF_masterToInterestTimeMap,
                                        RF_subjSlot,
                                        RF_subjSlotCount,
                                        RF_subjList,
                                        RF_caseMap,
                                        RF_subjMap,
                                        RF_subjCount);
          }
        }
        unstackPreDefinedCommonArrays(mode,
                                    RF_ntree,
                                    RF_timeIndex,
                                    RF_startTimeIndex,
                                    RF_statusIndex,
                                    RF_subjSize,
                                    RF_ptnCount,
                                    RF_nodeMembership,
                                    RF_tTermMembership,
                                    RF_pNodeMembership,
                                    RF_pTermMembership,
                                    RF_hTermMembership,
                                    RF_tTermList,
                                    RF_pNodeList,
                                    RF_pTermList,
                                    RF_bootMembershipFlag,
                                    RF_oobMembershipFlag,
                                    RF_bootMembershipCount,
                                    RF_ibgMembershipIndex,
                                    RF_oobMembershipIndex,
                                    RF_oobSize,
                                    RF_ibgSize,
                                    RF_bootMembershipIndex,
                                    RF_maxDepth,
                                    RF_orderedTreeIndex,
                                    RF_serialTreeIndex,
                                    RF_root,
                                    RF_nodeCount,
                                    RF_leafLinkedObjHead,
                                    RF_leafLinkedObjTail,
                                    RF_pLeafCount,
                                    RF_getTreeIndex,
                                    RF_subjWeightType,
                                    RF_subjWeightSorted,
                                    RF_identityMembershipIndexSize,
                                    RF_identityMembershipIndex);
    }
    unstackIncomingArrays(mode,
                          RF_ySize,
                          RF_yIndex,
                          RF_yIndexZero);
    unstackRandom(RF_ntree, RF_ntree, RF_fobservationSize, 0);
  }
  return result;
}
void complement(uint    originalMemberSize,
                uint   *originalMembers,
                uint    releasedMemberSize,
                uint   *releasedMembers,
                uint   *complementMembers) {
  uint *originalMembersIndx = uivector(1,originalMemberSize);
  uint *releasedMembersIndx = uivector(1,releasedMemberSize);
  uint o = 1;
  uint r = 1;
  uint curOrg;
  uint curRel;
  uint count = 0;
  indexxui(originalMemberSize, originalMembers, originalMembersIndx); 
  indexxui(releasedMemberSize, releasedMembers, releasedMembersIndx);
  count = 0;
  while(o <= originalMemberSize && r <= releasedMemberSize)
    {
      curOrg = originalMembers[originalMembersIndx[o]];
      curRel = releasedMembers[releasedMembersIndx[r]];
      if(curRel < curOrg) 
        {
          complementMembers[++(count)] = curRel;
          r++;
        }
      else if(curRel > curOrg)
        {
          o++;
        }
      else if(curRel == curOrg)
        {
          r++;
          o++;
        }
    }
  while(r <= releasedMemberSize) 
    {
      curRel = releasedMembers[releasedMembersIndx[r]];
      complementMembers[++(count)] = curRel;
      r++;
    }
  free_uivector(originalMembersIndx, 1, originalMemberSize);
  free_uivector(releasedMembersIndx, 1, releasedMemberSize);
}
void test()
{
  uint n = 20;
  uint m = 50;
  uint *ibgOrg = uivector(1, n);
  uint *ibgRel = uivector(1, m);
  uint randNumber;
  for(uint i = 1; i <= n; i++)
    {
      randNumber = ceil(ran1D(1) * 100);
      ibgOrg[i] = randNumber;
      for(uint k = 1; k < i; k++)
        {
          if(ibgOrg[k] == randNumber)
            {
              i--;
              break;
            }
        }
    }
  for(uint i =1; i<=m; i++)
    {
      randNumber = ceil(ran1D(1) * 100);
      ibgRel[i] = randNumber;
      for(uint k = 1; k < i; k++)
        {
          if(ibgRel[k] == randNumber)
            {
              i--;
              break;
            }
        }
    }
  free_uivector(ibgOrg, 1, n);
  free_uivector(ibgRel, 1, m);
} 
