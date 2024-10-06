#ifndef RF_STACK_MEMBERSHIP_VECTORS_H
#define RF_STACK_MEMBERSHIP_VECTORS_H
void stackMembershipVectors(uint size,
                            char **bootMembershipFlag,
                            char **oobMembershipFlag,
                            uint **bootMembershipCount,
                            uint **ibgMembershipIndex,
                            uint **oobMembershipIndex);
void unstackMembershipVectors(uint size,
                              char *bootMembershipFlag,
                              char *oobMembershipFlag,
                              uint *bootMembershipCount,
                              uint *ibgMembershipIndex,
                              uint *oobMembershipIndex);
#endif
