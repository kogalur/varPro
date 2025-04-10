
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "varProAux.h"
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
