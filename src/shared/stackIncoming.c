
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "stackIncoming.h"
#include "snpAuxiliaryInfo.h"
#include "stackAuxiliaryInfo.h"
#include "nrutil.h"
void stackTNQualitativeIncoming(char      mode,
                                AuxiliaryDimensionConstants *dimConst,
                                SNPAuxiliaryInfo **incomingAuxiliaryInfoList,
                                uint      ntree,
                                uint      bootstrapSize,
                                uint      observationSize,
                                char    **sexpStringIO,
                                uint     *rmbr_id_,
                                uint     *ambr_id,
                                uint     *tn_rcnt_,
                                uint     *tn_acnt_,
                                uint     *incomingStackCount,
                                uint   ***rmbr_id_ptr,
                                uint   ***ambr_id_ptr,
                                uint   ***tn_rcnt_ptr,
                                uint   ***tn_acnt_ptr) {
 if (RF_optHigh & OPT_MEMB_INCG) {
    const AuxiliaryDimension rmbrDim[] = {
      [1] = {RF_AUX_DIM_FIXED, ntree},
      [2] = {RF_AUX_DIM_FIXED, bootstrapSize}
    };
    allocateAuxiliaryInfo(dimConst,
                          FALSE,
                          NATIVE_TYPE_INTEGER,
                          sexpStringIO[RF_RMBR_ID],
                          incomingAuxiliaryInfoList,
                          *incomingStackCount,
                          rmbr_id_,
                          rmbr_id_ptr,
                          2,
                          rmbrDim);
    (*incomingStackCount) ++;
    const AuxiliaryDimension ambrDim[] = {
      [1] = {RF_AUX_DIM_FIXED, ntree},
      [2] = {RF_AUX_DIM_FIXED, observationSize}
    };
    allocateAuxiliaryInfo(dimConst,
                          FALSE,
                          NATIVE_TYPE_INTEGER,
                          sexpStringIO[RF_AMBR_ID],
                          incomingAuxiliaryInfoList,
                          *incomingStackCount,                          
                          ambr_id,
                          ambr_id_ptr,
                          2,
                          ambrDim);
    (*incomingStackCount) ++;
    const AuxiliaryDimension countDim[] = {
      [1] = {RF_AUX_DIM_FIXED, ntree},
      [2] = {RF_AUX_DIM_LEAF_COUNT, 0}
    };
    allocateAuxiliaryInfo(dimConst,
                          FALSE,
                          NATIVE_TYPE_INTEGER,
                          sexpStringIO[RF_TN_RCNT],
                          incomingAuxiliaryInfoList,
                          *incomingStackCount,
                          tn_rcnt_,
                          tn_rcnt_ptr,
                          2,
                          countDim);
    (*incomingStackCount) ++;
    allocateAuxiliaryInfo(dimConst,
                          FALSE,
                          NATIVE_TYPE_INTEGER,
                          sexpStringIO[RF_TN_ACNT],
                          incomingAuxiliaryInfoList,
                          *incomingStackCount,
                          tn_acnt_,
                          tn_acnt_ptr,
                          2,
                          countDim);
    (*incomingStackCount) ++;
  }
}
void stackTNQuantitativeIncoming(char         mode,
                                 AuxiliaryDimensionConstants *dimConst,
                                 SNPAuxiliaryInfo **incomingAuxiliaryInfoList,
                                 uint        ntree,
                                 char      **sexpStringIO,
                                 uint        timeIndex,
                                 uint        startTimeIndex,
                                 uint        statusIndex,
                                 uint        rFactorCount,
                                 uint        rNonFactorCount,
                                 uint        eventTypeSize,
                                 uint        sortedTimeInterestSize,
                                 uint       *tLeafCount,
                                 double     *tn_mort_,
                                 double     *tn_surv_,
                                 double     *tn_nlsn_,
                                 double     *tn_cshz_,
                                 double     *tn_cifn_,
                                 double     *tn_khzf_,
                                 double     *tn_regr_,
                                 uint       *tn_clas_,
                                 uint       *incomingStackCount,
                                 double  ****tn_mort_ptr,
                                 double  ****tn_surv_ptr,
                                 double  ****tn_nlsn_ptr,
                                 double *****tn_cshz_ptr,
                                 double *****tn_cifn_ptr,
                                 double  ****tn_khzf_ptr,
                                 double  ****tn_regr_ptr,
                                 uint   *****tn_clas_ptr) {
  if (RF_optHigh & OPT_TERM_INCG) {
    if ((timeIndex > 0) && (statusIndex > 0)) {
      if (startTimeIndex == 0) {
        const AuxiliaryDimension mortDim[] = {
          [1] = {RF_AUX_DIM_FIXED, ntree},
          [2] = {RF_AUX_DIM_LEAF_COUNT, 0},
          [3] = {RF_AUX_DIM_FIXED, eventTypeSize}
        };
        allocateAuxiliaryInfo(dimConst,
                              FALSE,
                              NATIVE_TYPE_NUMERIC,
                              sexpStringIO[RF_TN_MORT],
                              incomingAuxiliaryInfoList,
                              *incomingStackCount,
                              tn_mort_,
                              tn_mort_ptr,
                              3,
                              mortDim);
        (*incomingStackCount) ++;
        if (!(RF_opt & OPT_COMP_RISK)) {
          const AuxiliaryDimension survDim[] = {
            [1] = {RF_AUX_DIM_FIXED, ntree},
            [2] = {RF_AUX_DIM_LEAF_COUNT, 0},
            [3] = {RF_AUX_DIM_FIXED, sortedTimeInterestSize}
          };
          allocateAuxiliaryInfo(dimConst,
                                FALSE,
                                NATIVE_TYPE_NUMERIC,
                                sexpStringIO[RF_TN_SURV],
                                incomingAuxiliaryInfoList,
                                *incomingStackCount,
                                tn_surv_,
                                tn_surv_ptr,
                                3,
                                survDim);
          (*incomingStackCount) ++;
          allocateAuxiliaryInfo(dimConst,
                                FALSE,
                                NATIVE_TYPE_NUMERIC,
                                sexpStringIO[RF_TN_NLSN],
                                incomingAuxiliaryInfoList,
                                *incomingStackCount,
                                tn_nlsn_,
                                tn_nlsn_ptr,
                                3,
                                survDim);
          (*incomingStackCount) ++;
        }
        else {
          const AuxiliaryDimension riskDim[] = {
            [1] = {RF_AUX_DIM_FIXED, ntree},
            [2] = {RF_AUX_DIM_LEAF_COUNT, 0},
            [3] = {RF_AUX_DIM_FIXED, eventTypeSize},
            [4] = {RF_AUX_DIM_FIXED, sortedTimeInterestSize}
          };
          allocateAuxiliaryInfo(dimConst,
                                FALSE,
                                NATIVE_TYPE_NUMERIC,
                                sexpStringIO[RF_TN_CSHZ],
                                incomingAuxiliaryInfoList,
                                *incomingStackCount,
                                tn_cshz_,
                                tn_cshz_ptr,
                                4,
                                riskDim);
          (*incomingStackCount) ++;
          allocateAuxiliaryInfo(dimConst,
                                FALSE,
                                NATIVE_TYPE_NUMERIC,
                                sexpStringIO[RF_TN_CIFN],
                                incomingAuxiliaryInfoList,
                                *incomingStackCount,
                                tn_cifn_,
                                tn_cifn_ptr,
                                4,
                                riskDim);
          (*incomingStackCount) ++;
        }
      }
      else {
        if (FALSE) {
          const AuxiliaryDimension khzfDim[] = {
            [1] = {RF_AUX_DIM_FIXED, ntree},
            [2] = {RF_AUX_DIM_LEAF_COUNT, 0},
            [3] = {RF_AUX_DIM_FIXED, 1}
          };
          allocateAuxiliaryInfo(dimConst,
                                FALSE,
                                NATIVE_TYPE_NUMERIC,
                                sexpStringIO[RF_TN_KHZF],
                                incomingAuxiliaryInfoList,
                                *incomingStackCount,
                                tn_khzf_,
                                tn_khzf_ptr,
                                3,
                                khzfDim);
          (*incomingStackCount) ++;
        }
      }
    }
    else {
      if (rNonFactorCount > 0) {
        const AuxiliaryDimension regrDim[] = {
          [1] = {RF_AUX_DIM_FIXED, ntree},
          [2] = {RF_AUX_DIM_LEAF_COUNT, 0},
          [3] = {RF_AUX_DIM_FIXED, rNonFactorCount}
        };
        allocateAuxiliaryInfo(dimConst,
                              FALSE,
                              NATIVE_TYPE_NUMERIC,
                              sexpStringIO[RF_TN_REGR],
                              incomingAuxiliaryInfoList,
                              *incomingStackCount,
                              tn_regr_,
                              tn_regr_ptr,
                              3,
                              regrDim);
        (*incomingStackCount) ++;
      }
      if (rFactorCount > 0) {
        const AuxiliaryDimension clasDim[] = {
          [1] = {RF_AUX_DIM_FIXED, ntree},
          [2] = {RF_AUX_DIM_LEAF_COUNT, 0},
          [3] = {RF_AUX_DIM_FIXED, rFactorCount},
          [4] = {RF_AUX_DIM_FACTOR_SIZE, 0}
        };
        allocateAuxiliaryInfo(dimConst,
                              FALSE,
                              NATIVE_TYPE_INTEGER,
                              sexpStringIO[RF_TN_CLAS],
                              incomingAuxiliaryInfoList,
                              *incomingStackCount,
                              tn_clas_,
                              tn_clas_ptr,
                              4,
                              clasDim);
        (*incomingStackCount) ++;
      }
    }
  }
}
