
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "stackMembershipVectors.h"
#include "nrutil.h"
void stackMembershipVectors(uint observationSize,
                            uint identityMembershipIndexSize,
                            char **bootMembershipFlag,
                            char **oobMembershipFlag,
                            uint **bootMembershipCount,
                            uint **ibgMembershipIndex,
                            uint **oobMembershipIndex,
                            uint **bootMembershipIndex) {
  if(bootMembershipFlag != NULL) {
    *bootMembershipFlag = cvector(1, observationSize);
  }
  if(oobMembershipFlag != NULL) {
    *oobMembershipFlag = cvector(1, observationSize);
  }
  if(bootMembershipCount != NULL) {
    *bootMembershipCount = uivector(1, observationSize);
  }
  if(ibgMembershipIndex != NULL) {
    *ibgMembershipIndex = uivector(1, observationSize);
  }
  if(oobMembershipIndex != NULL) {
    *oobMembershipIndex = uivector(1, observationSize);
  }
  if(bootMembershipIndex != NULL && identityMembershipIndexSize > 0) {
    *bootMembershipIndex = uivector(1, identityMembershipIndexSize);  
  }
}
void unstackMembershipVectors(uint observationSize,
                              uint identityMembershipIndexSize,
                              char *bootMembershipFlag,
                              char *oobMembershipFlag,
                              uint *bootMembershipCount,
                              uint *ibgMembershipIndex,
                              uint *oobMembershipIndex,
                              uint *bootMembershipIndex) {
  if(bootMembershipFlag != NULL) {
    free_cvector(bootMembershipFlag, 1, observationSize);
  }
  if(oobMembershipFlag != NULL) {
    free_cvector(oobMembershipFlag, 1, observationSize);
  }
  if(bootMembershipCount != NULL) {
    free_uivector(bootMembershipCount, 1, observationSize);
  }
  if(ibgMembershipIndex != NULL) {
    free_uivector(ibgMembershipIndex, 1, observationSize);
  }
  if(oobMembershipIndex != NULL) {
    free_uivector(oobMembershipIndex, 1, observationSize);
  }
  if(bootMembershipIndex != NULL && identityMembershipIndexSize > 0) {
    free_uivector(bootMembershipIndex, 1, identityMembershipIndexSize);
  }
}
