
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
                                  uint     *tLeafCount,
                                  uint     *rmbr_id_,
                                  uint     *ombr_id_,
                                  uint     *imbr_id_,
                                  uint     *tn_rcnt_,
                                  uint     *tn_ocnt_,
                                  uint     *tn_icnt_,
                                  uint  ****rmbr_id_ptr,
                                  uint  ****ombr_id_ptr,
                                  uint  ****imbr_id_ptr,
                                  uint   ***tn_rcnt_ptr,
                                  uint   ***tn_ocnt_ptr,
                                  uint   ***tn_icnt_ptr) {  
  uint ombrIterator;
  uint imbrIterator;
  uint cntIterator;
  uint treeID;
  uint i;
  if (RF_optHigh & OPT_MEMB_INCG) {
    ombrIterator = 0;
    imbrIterator = 0;
    cntIterator = 0;
    *ombr_id_ptr = (uint ***) new_vvector(1, RF_ntree, NRUTIL_UPTR2);
    *imbr_id_ptr = (uint ***) new_vvector(1, RF_ntree, NRUTIL_UPTR2);
    *tn_ocnt_ptr = (uint **) new_vvector(1, RF_ntree, NRUTIL_UPTR);
    *tn_icnt_ptr = (uint **) new_vvector(1, RF_ntree, NRUTIL_UPTR);
    ombr_id_ --;
    imbr_id_ --;
    tn_ocnt_ --;
    tn_icnt_ --;
    for (treeID = 1; treeID <= RF_ntree; treeID++) {
      (*ombr_id_ptr)[treeID] = (uint **) new_vvector(1, tLeafCount[treeID], NRUTIL_UPTR); 
      (*imbr_id_ptr)[treeID] = (uint **) new_vvector(1, tLeafCount[treeID], NRUTIL_UPTR); 
      (*tn_ocnt_ptr)[treeID] = tn_ocnt_ + cntIterator;
      (*tn_icnt_ptr)[treeID] = tn_icnt_ + cntIterator;
      for(i = 1; i <= tLeafCount[treeID]; i++) {
        (*ombr_id_ptr)[treeID][i] = ombr_id_ + ombrIterator;
        ombrIterator += (*tn_ocnt_ptr)[treeID][i];
        (*imbr_id_ptr)[treeID][i] = imbr_id_ + imbrIterator;
        imbrIterator += (*tn_icnt_ptr)[treeID][i];
      }
      cntIterator += tLeafCount[treeID];
    }
  }
}
void unstackTNQualitativeIncomingVP(char      mode,
                                    uint     *tLeafCount,
                                    uint   ***rmbr_id_ptr,
                                    uint   ***ombr_id_ptr,
                                    uint   ***imbr_id_ptr,
                                    uint    **tn_rcnt_ptr,
                                    uint    **tn_ocnt_ptr,
                                    uint    **tn_icnt_ptr) {
  uint treeID;
  if (RF_optHigh & OPT_MEMB_INCG) {
    for (treeID = 1; treeID <= RF_ntree; treeID++) {
     free_new_vvector((ombr_id_ptr)[treeID], 1, tLeafCount[treeID], NRUTIL_UPTR);
     free_new_vvector((imbr_id_ptr)[treeID], 1, tLeafCount[treeID], NRUTIL_UPTR);
    }
    free_new_vvector(ombr_id_ptr, 1, RF_ntree, NRUTIL_UPTR2);
    free_new_vvector(imbr_id_ptr, 1, RF_ntree, NRUTIL_UPTR2);
    free_new_vvector(tn_ocnt_ptr, 1, RF_ntree, NRUTIL_UPTR);
    free_new_vvector(tn_icnt_ptr, 1, RF_ntree, NRUTIL_UPTR);
  }
}
void stackTNQualitativeIncomingVPnew(char      mode,
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
    int *dim = ivector(1, 2);
    dim[1] = ntree;
    dim[2] = -3;
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
    AuxiliaryDimensionConstants *dimConstHack;
    dimConstHack =
      makeAuxDimConsts(dimConst -> rFactorSize,
                       0,
                       dimConst -> rFactorMap,
                       dimConst -> rTargetFactor,
                       0,
                       dimConst -> tLeafCount,
                       oobBlk);    
    allocateAuxiliaryInfo(dimConstHack,
                          FALSE,
                          NATIVE_TYPE_INTEGER,
                          "tnOMBR",
                          incomingAuxiliaryInfoList,
                          *incomingStackCount,
                          ombr_id_,
                          ombr_id_ptr,
                          2,
                          dim);
    (*incomingStackCount)++;
    freeAuxDimConsts(dimConstHack);
    dimConstHack =
      makeAuxDimConsts(dimConst -> rFactorSize,
                       0,
                       dimConst -> rFactorMap,
                       dimConst -> rTargetFactor,
                       0,
                       dimConst -> tLeafCount,
                       ibgBlk);    
    allocateAuxiliaryInfo(dimConstHack,
                          FALSE,
                          NATIVE_TYPE_INTEGER,
                          "tnIMBR",
                          incomingAuxiliaryInfoList,
                          *incomingStackCount,
                          imbr_id_,
                          imbr_id_ptr,
                          2,
                          dim);
    (*incomingStackCount)++;
    freeAuxDimConsts(dimConstHack);
    free_uivector(oobBlk, 1, ntree);
    free_uivector(ibgBlk, 1, ntree);
    dim[1] = ntree;
    dim[2] = -2;
    allocateAuxiliaryInfo(dimConst,
                          FALSE,
                          NATIVE_TYPE_INTEGER,
                          "tnOCNT",  
                          incomingAuxiliaryInfoList,
                          *incomingStackCount,
                          tn_ocnt_,
                          tn_ocnt_ptr,
                          2,
                          dim);
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
                          dim);
    (*incomingStackCount)++;
    free_ivector(dim, 1, 2);
  }
}
