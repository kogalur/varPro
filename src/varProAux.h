#ifndef RF_VAR_PRO_AUX_H
#define RF_VAR_PRO_AUX_H
#include "shared/snpAuxiliaryInfo.h"
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
                                  uint                      ***tn_icnt_ptr);
#endif
