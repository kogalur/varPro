#ifndef RF_STACK_MEMBERSHIP_VECTORS_H
#define RF_STACK_MEMBERSHIP_VECTORS_H
void stackMembershipVectors(uint observationSize,
                            uint identityMembershipIndexSize,
                            char **bootMembershipFlag_treeID,
                            char **oobMembershipFlag_treeID,
                            uint **bootMembershipCount_treeID,
                            uint **ibgMembershipIndex_treeID,
                            uint **oobMembershipIndex_treeID,
                            uint **bootMembershipIndex_treeID);
void unstackMembershipVectors(uint observationSize,
                              uint identityMembershipIndexSize,
                              char *bootMembershipFlag_treeID,
                              char *oobMembershipFlag_treeID,
                              uint *bootMembershipCount_treeID,
                              uint *ibgMembershipIndex_treeID,
                              uint *oobMembershipIndex_treeID,
                              uint *bootMembershipIndex_treeID);
#endif
