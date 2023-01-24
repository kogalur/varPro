#ifndef  RF_BOOTSTRAP_H
#define  RF_BOOTSTRAP_H
#include "nodeBase.h"
char bootstrap (char      mode,
                uint      treeID,
                NodeBase *nodePtr,
                uint     *subsetIndex,
                uint      subsetSize,
                uint      indexSize,
                uint    **bootstrapIn,
                uint      subjSize,
                double   *subjWeight,
                uint      subjWeightType,
                uint     *subjWeightSorted,
                uint      subjWeightDensitySize,
                uint      observationSize,
                uint     *index,  
                char    **bootMembershipFlag,
                char    **oobMembershipFlag,
                uint    **bootMembershipCount,
                uint    **bootMembershipIndex,
                uint     *oobSize,
                uint     *ibgSize,
                uint    **ibgMembershipIndex,
                uint    **oobMembershipIndex,
                uint    **BOOT_CT_ptr);
#endif
