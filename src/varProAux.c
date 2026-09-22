
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "varProAux.h"
#include "shared/stackAuxiliaryInfo.h"
#include "shared/nrutil.h"
#include "shared/error.h"
void stackTNQualitativeIncomingVP(char      mode,
                                  AuxiliaryDimensionConstants *dimConst,
                                  SNPAuxiliaryInfo           **incomingAuxiliaryInfoList,
                                  uint                        ntree,
                                  uint                       *ombr_id_,
                                  uint                       *imbr_id_,
                                  uint                       *tn_ocnt_,
                                  uint                       *tn_icnt_,
                                  uint                       *incomingStackCount,
                                  uint                      ***ombr_id_ptr,
                                  uint                      ***imbr_id_ptr,
                                  uint                      ***tn_ocnt_ptr,
                                  uint                      ***tn_icnt_ptr) {
  uint treeID;
  uint leafID;
  if (RF_optHigh & OPT_MEMB_INCG) {
    const AuxiliaryDimension membershipDim[] = {
      [1] = {RF_AUX_DIM_FIXED, ntree},
      [2] = {RF_AUX_DIM_CUSTOM_COUNT, 0}
    };
    ulong cntOffset;
    uint *oobBlk = uivector(1, ntree);
    uint *ibgBlk = uivector(1, ntree);
    cntOffset = 0;
    for (treeID = 1; treeID <= ntree; treeID++) {
      oobBlk[treeID] = 0;
      ibgBlk[treeID] = 0;
      for (leafID = 1; leafID <= dimConst->tLeafCount[treeID]; leafID++) {
        oobBlk[treeID] += tn_ocnt_[cntOffset + leafID - 1];
        ibgBlk[treeID] += tn_icnt_[cntOffset + leafID - 1];
      }
      if ((RF_OOB_SZ_ != NULL) && (oobBlk[treeID] != RF_OOB_SZ_[treeID])) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  OOB block mismatch in tree %10d:  counts=%10d  oobSZ=%10d",
                       treeID, oobBlk[treeID], RF_OOB_SZ_[treeID]);
        RF_nativeExit();
      }
      if ((RF_IBG_SZ_ != NULL) && (ibgBlk[treeID] != RF_IBG_SZ_[treeID])) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  IBG block mismatch in tree %10d:  counts=%10d  ibgSZ=%10d",
                       treeID, ibgBlk[treeID], RF_IBG_SZ_[treeID]);
        RF_nativeExit();
      }
      cntOffset += dimConst->tLeafCount[treeID];
    }
    AuxiliaryDimensionConstants customDimConst = *dimConst;
    customDimConst.customSize = oobBlk;
    allocateAuxiliaryInfo(&customDimConst,
                          FALSE,
                          NATIVE_TYPE_INTEGER,
                          "tnOMBR",
                          incomingAuxiliaryInfoList,
                          *incomingStackCount,
                          ombr_id_,
                          ombr_id_ptr,
                          2,
                          membershipDim);
    (*incomingStackCount)++;
    customDimConst.customSize = ibgBlk;
    allocateAuxiliaryInfo(&customDimConst,
                          FALSE,
                          NATIVE_TYPE_INTEGER,
                          "tnIMBR",
                          incomingAuxiliaryInfoList,
                          *incomingStackCount,
                          imbr_id_,
                          imbr_id_ptr,
                          2,
                          membershipDim);
    (*incomingStackCount)++;
    free_uivector(oobBlk, 1, ntree);
    free_uivector(ibgBlk, 1, ntree);
    const AuxiliaryDimension countDim[] = {
      [1] = {RF_AUX_DIM_FIXED, ntree},
      [2] = {RF_AUX_DIM_LEAF_COUNT, 0}
    };
    allocateAuxiliaryInfo(dimConst,
                          FALSE,
                          NATIVE_TYPE_INTEGER,
                          "tnOCNT",  
                          incomingAuxiliaryInfoList,
                          *incomingStackCount,
                          tn_ocnt_,
                          tn_ocnt_ptr,
                          2,
                          countDim);
    (*incomingStackCount)++;
    allocateAuxiliaryInfo(dimConst,
                          FALSE,
                          NATIVE_TYPE_INTEGER,
                          "tnICNT",
                          incomingAuxiliaryInfoList,
                          *incomingStackCount,
                          tn_icnt_,
                          tn_icnt_ptr,
                          2,
                          countDim);
    (*incomingStackCount)++;
  }
}
