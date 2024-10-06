
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "stackMembershipVectors.h"
#include "nrutil.h"
void stackMembershipVectors(uint size,
                            char **bootMembershipFlag,
                            char **oobMembershipFlag,
                            uint **bootMembershipCount,
                            uint **ibgMembershipIndex,
                            uint **oobMembershipIndex) {
  if(bootMembershipFlag != NULL) {
    *bootMembershipFlag = cvector(1, size);
  }
  if(oobMembershipFlag != NULL) {
    *oobMembershipFlag = cvector(1, size);
  }
  if(bootMembershipCount != NULL) {
    *bootMembershipCount = uivector(1, size);
  }
  if(ibgMembershipIndex != NULL) {
    *ibgMembershipIndex = uivector(1, size);
  }
  if(oobMembershipIndex != NULL) {
    *oobMembershipIndex = uivector(1, size);
  }
}
void unstackMembershipVectors(uint size,
                              char *bootMembershipFlag,
                              char *oobMembershipFlag,
                              uint *bootMembershipCount,
                              uint *ibgMembershipIndex,
                              uint *oobMembershipIndex) {
  if(bootMembershipFlag != NULL) {
    free_cvector(bootMembershipFlag, 1, size);
  }
  if(oobMembershipFlag != NULL) {
    free_cvector(oobMembershipFlag, 1, size);
  }
  if(bootMembershipCount != NULL) {
    free_uivector(bootMembershipCount, 1, size);
  }
  if(ibgMembershipIndex != NULL) {
    free_uivector(ibgMembershipIndex, 1, size);
  }
  if(oobMembershipIndex != NULL) {
    free_uivector(oobMembershipIndex, 1, size);
  }
}
