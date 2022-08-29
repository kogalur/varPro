
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
#include           "trace.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "stackMembershipVectors.h"
#include "nrutil.h"
void stackMembershipVectors(uint observationSize,
                            uint identityMembershipIndexSize,
                            char **bootMembershipFlag_treeID,
                            char **oobMembershipFlag_treeID,
                            uint **bootMembershipCount_treeID,
                            uint **ibgMembershipIndex_treeID,
                            uint **oobMembershipIndex_treeID,
                            uint **bootMembershipIndex_treeID) {
  if(bootMembershipFlag_treeID != NULL) {
    *bootMembershipFlag_treeID = cvector(1, observationSize);
  }
  if(oobMembershipFlag_treeID != NULL) {
    *oobMembershipFlag_treeID = cvector(1, observationSize);
  }
  if(bootMembershipCount_treeID != NULL) {
    *bootMembershipCount_treeID = uivector(1, observationSize);
  }
  if(ibgMembershipIndex_treeID != NULL) {
    *ibgMembershipIndex_treeID = uivector(1, observationSize);
  }
  if(oobMembershipIndex_treeID != NULL) {
    *oobMembershipIndex_treeID = uivector(1, observationSize);
  }
  if(bootMembershipIndex_treeID != NULL && identityMembershipIndexSize > 0) {
    *bootMembershipIndex_treeID = uivector(1, identityMembershipIndexSize);  
  }
}
void unstackMembershipVectors(uint observationSize,
                              uint identityMembershipIndexSize,
                              char *bootMembershipFlag_treeID,
                              char *oobMembershipFlag_treeID,
                              uint *bootMembershipCount_treeID,
                              uint *ibgMembershipIndex_treeID,
                              uint *oobMembershipIndex_treeID,
                              uint *bootMembershipIndex_treeID) {
  if(bootMembershipFlag_treeID != NULL) {
    free_cvector(bootMembershipFlag_treeID, 1, observationSize);
  }
  if(oobMembershipFlag_treeID != NULL) {
    free_cvector(oobMembershipFlag_treeID, 1, observationSize);
  }
  if(bootMembershipCount_treeID != NULL) {
    free_uivector(bootMembershipCount_treeID, 1, observationSize);
  }
  if(ibgMembershipIndex_treeID != NULL) {
    free_uivector(ibgMembershipIndex_treeID, 1, observationSize);
  }
  if(oobMembershipIndex_treeID != NULL) {
    free_uivector(oobMembershipIndex_treeID, 1, observationSize);
  }
  if(bootMembershipIndex_treeID != NULL && identityMembershipIndexSize > 0) {
    free_uivector(bootMembershipIndex_treeID, 1, identityMembershipIndexSize);
  }
}
