
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "entry.h"
#include "shared/nrutil.h"
#include "shared/stackForestObjects.h"
#include "shared/nativeUtil.h"
#include "shared/error.h"
#include "varProMain.h"
#include "entryGeneric.h"
SEXP varProStrength(SEXP traceFlag,
                    SEXP seedPtr,
                    SEXP optLow,
                    SEXP optHigh,
                    SEXP optVarPro,
                    SEXP ntree,
                    SEXP observationSize,
                    SEXP sampleInfo,
                    SEXP yTarget,
                    SEXP yInfo,
                    SEXP yLevels,
                    SEXP yData,
                    SEXP xInfo,
                    SEXP xLevels,
                    SEXP xData,
                    SEXP timeInterest,
                    SEXP fobservationSize,
                    SEXP fyData,
                    SEXP fxData,
                    SEXP scoreInfo,
                    SEXP totalNodeCount,
                    SEXP tLeafCount,
                    SEXP seedInfo,
                    SEXP hdim,
                    SEXP treeID,
                    SEXP nodeID,
                    SEXP nodeSZ,
                    SEXP brnodeID,
                    SEXP hc_zero,
                    SEXP tnRMBR,
                    SEXP tnAMBR,
                    SEXP tnOMBR,
                    SEXP tnIMBR,
                    SEXP tnRCNT,
                    SEXP tnACNT,
                    SEXP tnOCNT,
                    SEXP tnICNT,
                    SEXP oobSZ,
                    SEXP ibgSZ,
                    SEXP tnSURV,
                    SEXP tnMORT,
                    SEXP tnNLSN,
                    SEXP tnCSHZ,
                    SEXP tnCIFN,
                    SEXP tnREGR,
                    SEXP tnCLAS,
                    SEXP maxRulesTree,
                    SEXP maxTree,
                    SEXP getTree,
                    SEXP numThreads) {
  char mode;
  char result;
  clock_t cpuTimeStart = clock();
  setUserTraceFlag(INTEGER(traceFlag)[0]);
  setNativeGlobalEnv(&RF_nativeIndex, &RF_stackCount);
  int seedValue           = INTEGER(seedPtr)[0];
  RF_opt                  = INTEGER(optLow)[0];
  RF_optHigh              = INTEGER(optHigh)[0];
  VP_opt                  = INTEGER(optVarPro)[0];
  RF_ntree                = INTEGER(ntree)[0];
  RF_observationSize      = INTEGER(observationSize)[0];
  RF_ySize                = INTEGER(VECTOR_ELT(yInfo, 0))[0];
  if(VECTOR_ELT(yInfo, 1) != R_NilValue) {
    RF_rType = (char *) copy1DObject(VECTOR_ELT(yInfo, 1), NATIVE_TYPE_CHARACTER, RF_ySize, TRUE);
  }
  else {
    RF_rType = NULL;
  }
  if(VECTOR_ELT(yInfo, 2) != R_NilValue) {
    RF_rLevelsMax         = (uint *) INTEGER(VECTOR_ELT(yInfo, 2)); RF_rLevelsMax--;
  }
  else {
    RF_rLevelsMax         = NULL;
  }
  if(VECTOR_ELT(yInfo, 3) != R_NilValue) {
    RF_rLevelsCnt         = (uint *) INTEGER(VECTOR_ELT(yInfo, 3)); RF_rLevelsCnt--;
  }
  else {
    RF_rLevelsCnt = NULL;
  }
  if(VECTOR_ELT(yInfo, 4) != R_NilValue) {
    RF_subjIn             =  (uint *) INTEGER(VECTOR_ELT(yInfo, 4));  RF_subjIn --;
  }
  else {
    RF_subjIn             = NULL;
  }
  if(VECTOR_ELT(yInfo, 5) != R_NilValue) {
    RF_eventTypeSize      =  INTEGER(VECTOR_ELT(yInfo, 5))[0];
  }
  else {
    RF_eventTypeSize      = 0;
  }
  if(VECTOR_ELT(yInfo, 6) != R_NilValue) {
    if (RF_eventTypeSize > 0) {
      RF_eventType        =  (uint *) INTEGER(VECTOR_ELT(yInfo, 6));  RF_eventType --;
    }
    else {
      RF_eventType        = NULL;
    }
  }
  else {
    RF_eventType          = NULL;
  }
  RF_rLevelsSEXP = yLevels;
  RF_rLevels = NULL;
  if(RF_ySize > 0) {
    RF_responseIn           = (double **) copy2DObject(yData, NATIVE_TYPE_NUMERIC, TRUE, RF_ySize, RF_observationSize);
  }
  else {
    RF_responseIn = NULL;
  }
  RF_xSize                 = INTEGER(VECTOR_ELT(xInfo, 0))[0];
  if(VECTOR_ELT(xInfo, 1) != R_NilValue) {
    RF_xType                = (char *) copy1DObject(VECTOR_ELT(xInfo, 1), NATIVE_TYPE_CHARACTER, RF_xSize, TRUE);
  }
  else {
    RF_xType = NULL;
  }
  if(VECTOR_ELT(xInfo, 2) != R_NilValue) {  
    RF_xLevelsMax           = (uint *) INTEGER(VECTOR_ELT(xInfo, 2)); RF_xLevelsMax--;
  }
  else {
    RF_xLevelsMax = NULL;
  }
  if(VECTOR_ELT(xInfo, 3) != R_NilValue) {
    RF_xLevelsCnt           = (uint *) INTEGER(VECTOR_ELT(xInfo, 3)); RF_xLevelsCnt --;
  }
  else {
    RF_xLevelsCnt = NULL;
  }
  RF_xtType = NULL;
  RF_stType = NULL;
  RF_xLevelsSEXP = xLevels;
  RF_xLevels = NULL;
  if (RF_xSize > 0) {
    RF_observationIn      = (double **) copy2DObject(xData, NATIVE_TYPE_NUMERIC, TRUE, RF_xSize, RF_observationSize);
  }
  else {
    RF_observationIn = NULL;
  }
  RF_subjSize             = 0;
  RF_subjWeight           = NULL;
  RF_bootstrapSize        = 0;
  RF_bootstrapIn          = NULL;
  if(VECTOR_ELT(sampleInfo, 0) != R_NilValue) {
    RF_subjSize = INTEGER(VECTOR_ELT(sampleInfo, 0))[0];
    if(VECTOR_ELT(sampleInfo, 1) != R_NilValue) {
      RF_subjWeight = REAL(VECTOR_ELT(sampleInfo, 1)); RF_subjWeight--;
    }
    if(VECTOR_ELT(sampleInfo, 2) != R_NilValue) {  
      RF_bootstrapSize        = INTEGER(VECTOR_ELT(sampleInfo, 2))[0];
      if(VECTOR_ELT(sampleInfo, 3) != R_NilValue) {
        RF_bootstrapIn = (uint **) copy2DObject(VECTOR_ELT(sampleInfo, 3), NATIVE_TYPE_INTEGER, (RF_opt & OPT_BOOT_TYP2), RF_ntree, RF_subjSize);
      }
    }
    else {
      RF_bootstrapSize        = RF_subjSize;
    }
  }
  RF_timeInterestSize = INTEGER(VECTOR_ELT(timeInterest, 0))[0];
  if (VECTOR_ELT(timeInterest, 1) != R_NilValue) {
    RF_timeInterest         = (double *) REAL(VECTOR_ELT(timeInterest, 1));
    RF_timeInterest --;
  }
  else {
    RF_timeInterest = NULL;
  }
  RF_fobservationSize      = INTEGER(fobservationSize)[0];
  RF_frSize = 0;
  RF_fresponseIn = NULL;
  RF_fobservationIn = NULL;
  VP_neighbourSize = 0;
  VP_xReduceSize = 0;
  VP_xReduceIndx = NULL;
  RF_fnodeMembership = NULL;
  if (RF_fobservationSize == 0) {
    mode = RF_REST;  
  }
  else {
    mode = RF_PRED;
    if (fyData != R_NilValue) {    
      RF_fresponseIn           = (double **) copy2DObject(fyData, NATIVE_TYPE_NUMERIC, TRUE, RF_ySize, RF_fobservationSize);
      RF_frSize = RF_ySize;
    }
    if (fxData != R_NilValue) {
      RF_fobservationIn      = (double **) copy2DObject(fxData, NATIVE_TYPE_NUMERIC, TRUE, RF_xSize, RF_fobservationSize);
    }
    if (scoreInfo != R_NilValue) {
      VP_neighbourSize = INTEGER(VECTOR_ELT(scoreInfo, 0))[0];
      VP_xReduceSize   = INTEGER(VECTOR_ELT(scoreInfo, 1))[0];
      if (VP_xReduceSize > 0) {
        VP_xReduceIndx = (uint *) INTEGER(VECTOR_ELT(scoreInfo, 2)); VP_xReduceIndx --;
      }
    }
    VP_opt = VP_opt & (~(VP_OPT_CMP | VP_OPT_OOB));
  }
  RF_totalNodeCount_      = INTEGER(totalNodeCount)[0];
  RF_tLeafCount_          = (uint *) INTEGER(tLeafCount); RF_tLeafCount_ --;
  RF_seed_                = (int *) INTEGER(VECTOR_ELT(seedInfo, 0)); RF_seed_ --;
  RF_treeID_              = (uint *) INTEGER(treeID);   RF_treeID_ --;
  RF_nodeID_              = (uint *) INTEGER(nodeID);   RF_nodeID_ --;
  RF_nodeSZ_              = (uint *) INTEGER(nodeSZ);   RF_nodeSZ_ --;
  RF_brnodeID_            = (uint *) INTEGER(brnodeID); RF_brnodeID_ --;
  RF_RMBR_ID_             = (uint *) INTEGER(tnRMBR);
  RF_AMBR_ID_             = (uint *) INTEGER(tnAMBR);
  RF_OMBR_ID_             = (uint *) INTEGER(tnOMBR);
  RF_IMBR_ID_             = (uint *) INTEGER(tnIMBR);  
  RF_TN_RCNT_             = (uint *) INTEGER(tnRCNT);
  RF_TN_ACNT_             = (uint *) INTEGER(tnACNT);
  RF_TN_OCNT_             = (uint *) INTEGER(tnOCNT);
  RF_TN_ICNT_             = (uint *) INTEGER(tnICNT);
  RF_OOB_SZ_              = (uint *) INTEGER(oobSZ);  RF_OOB_SZ_ --;
  RF_IBG_SZ_              = (uint *) INTEGER(ibgSZ);  RF_IBG_SZ_ --;
  RF_quantileSize = 0;
  RF_quantile = NULL;
  RF_qEpsilon = 0;
  RF_numThreads           = INTEGER(numThreads)[0];
  RF_ptnCount   =   0;
  RF_rTargetCount         = INTEGER(VECTOR_ELT(yTarget, 0))[0];
  if (VECTOR_ELT(yTarget, 1) != R_NilValue) {
    RF_rTarget         = (uint *) INTEGER(VECTOR_ELT(yTarget, 1));
    RF_rTarget --;
  }
  else {
    RF_rTarget = NULL;
  }
  RF_intrPredictorSize = 0;
  RF_intrPredictor = NULL;
  RF_getTree = (uint *) INTEGER(getTree);  RF_getTree --;
  RF_TN_SURV_ = REAL(tnSURV);
  RF_TN_MORT_ = REAL(tnMORT);
  RF_TN_NLSN_ = REAL(tnNLSN);
  RF_TN_CSHZ_ = REAL(tnCSHZ);
  RF_TN_CIFN_ = REAL(tnCIFN);
  RF_TN_REGR_ = REAL(tnREGR);  
  RF_TN_CLAS_ = (uint *) INTEGER(tnCLAS);
  VP_maxRulesTree = INTEGER(maxRulesTree)[0];
  VP_maxTree = INTEGER(maxTree)[0];
  stackForestObjectsAuxOnly(mode,
                            RF_ntree,
                            &RF_restoreTreeID,
                            &RF_restoreTreeOffset,
                            &RF_restoreMWCPoffset,
                            &RF_parmID_,
                            &RF_contPT_,
                            &RF_mwcpSZ_,
                            &RF_fsrecID_,
                            &RF_mwcpPT_,
                            &RF_mwcpCT);
  RF_parmID_[1]              = (int *) INTEGER(VECTOR_ELT(hc_zero, 0));   RF_parmID_[1]  --;
  RF_contPT_[1]              =             REAL(VECTOR_ELT(hc_zero, 1));  RF_contPT_[1]  --;
  RF_mwcpSZ_[1]              = (uint *) INTEGER(VECTOR_ELT(hc_zero, 2));  RF_mwcpSZ_[1]  --;
  RF_fsrecID_[1]             = (uint *) INTEGER(VECTOR_ELT(hc_zero, 3));  RF_fsrecID_[1] --;
  if (VECTOR_ELT(hc_zero, 4) != R_NilValue) {
    RF_mwcpPT_[1]            = (uint *) INTEGER(VECTOR_ELT(hc_zero, 4));  RF_mwcpPT_[1]  --;
  }
  else {
    RF_mwcpPT_[1] = NULL;
  }
  result = varProMain(mode, seedValue);
  unstackForestObjectsAuxOnly(mode,
                              RF_ntree,
                              RF_restoreTreeID,
                              RF_restoreTreeOffset,
                              RF_restoreMWCPoffset,
                              RF_parmID_,
                              RF_contPT_,
                              RF_mwcpSZ_,
                              RF_fsrecID_,
                              RF_mwcpPT_,
                              RF_mwcpCT);
  if (RF_rType != NULL) free_1DObject(RF_rType, NATIVE_TYPE_CHARACTER, RF_ySize);
  if (RF_xType != NULL) free_1DObject(RF_xType, NATIVE_TYPE_CHARACTER, RF_xSize);
  if (RF_responseIn != NULL) free_2DObject(RF_responseIn, NATIVE_TYPE_NUMERIC, RF_ySize > 0, RF_ySize, RF_observationSize);
  if (RF_observationIn != NULL) free_2DObject(RF_observationIn, NATIVE_TYPE_NUMERIC, TRUE, RF_xSize, RF_observationSize);
  if (RF_fresponseIn != NULL) free_2DObject(RF_fresponseIn, NATIVE_TYPE_NUMERIC, TRUE, RF_ySize, RF_fobservationSize);
  if (RF_fobservationIn != NULL) free_2DObject(RF_fobservationIn, NATIVE_TYPE_NUMERIC, TRUE, RF_xSize, RF_fobservationSize);
  if(VECTOR_ELT(sampleInfo, 0) != R_NilValue) {
    if(VECTOR_ELT(sampleInfo, 2) != R_NilValue) {  
      if(VECTOR_ELT(sampleInfo, 3) != R_NilValue) {
        free_2DObject(RF_bootstrapIn, NATIVE_TYPE_INTEGER, (RF_opt & OPT_BOOT_TYP2), RF_ntree, RF_subjSize);
      }
    }
  }
  if (RF_nativeIndex != RF_stackCount) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Stack imbalance in PROTECT/UNPROTECT:  %10d versus %10d  ", RF_nativeIndex, RF_stackCount);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  }
  if (result && (RF_stackCount > 0)) {
    VP_cpuTime_[1] = (double) (clock() - cpuTimeStart) / CLOCKS_PER_SEC;
    R_ReleaseObject(RF_sexpVector[RF_OUTP_ID]);
    R_ReleaseObject(RF_sexpVector[RF_STRG_ID]);
    return RF_sexpVector[RF_OUTP_ID];
  }
  else {
    return NULL;
  }
}
