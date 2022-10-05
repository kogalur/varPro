#ifndef RF_STACK_MEMBERSHIP_VECTORS_H
#define RF_STACK_MEMBERSHIP_VECTORS_H
void stackMembershipVectors(uint observationSize,
                            uint identityMembershipIndexSize,
                            char **bootMembershipFlag,
                            char **oobMembershipFlag,
                            uint **bootMembershipCount,
                            uint **ibgMembershipIndex,
                            uint **oobMembershipIndex,
                            uint **bootMembershipIndex);
void unstackMembershipVectors(uint observationSize,
                              uint identityMembershipIndexSize,
                              char *bootMembershipFlag,
                              char *oobMembershipFlag,
                              uint *bootMembershipCount,
                              uint *ibgMembershipIndex,
                              uint *oobMembershipIndex,
                              uint *bootMembershipIndex);
#endif
