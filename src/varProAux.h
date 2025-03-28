#ifndef RF_VAR_PRO_AUX_H
#define RF_VAR_PRO_AUX_H
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
                                  uint   ***tn_icnt_ptr);
void unstackTNQualitativeIncomingVP(char      mode,
                                    uint     *tLeafCount,
                                    uint   ***rmbr_id_ptr,
                                    uint   ***ombr_id_ptr,
                                    uint   ***imbr_id_ptr,
                                    uint    **tn_rcnt_ptr,
                                    uint    **tn_ocnt_ptr,
                                    uint    **tn_icnt_ptr);
#endif
