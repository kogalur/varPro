#ifndef RF_TREE_OPS_H
#define RF_TREE_OPS_H
#include "node.h"
void acquireTree(char mode, uint b);
void freeTree(uint treeID, NodeBase *parent);
void acquireProxyIndv(uint  treeID,
                      uint  branchID,
                      uint *genMembership,
                      uint  genMembershipSize,
                      uint *ibgMembership,
                      uint  ibgMembershipSize,
                      uint *branchMembers,
                      uint *branchMemberCount,
                      uint *indv,
                      uint *indvDepth);
void acquireBranch(uint      b,
                   uint      treeID,
                   uint      branchID,    
                   uint      indv,
                   uint      nodeDepth,
                   NodeBase *parent,
                   double  **xArray,
                   uint     *xReleaseID,
                   char     *pathPolarity,
                   char     *releaseFlag,
                   uint     *xReleaseCount);
void acquireReleasedMembership(uint      b,
                               uint      treeID,
                               uint      branchID, 
                               NodeBase *parent,
                               double  **xArray,
                               uint     *membershipIncoming,
                               uint     *membership,
                               uint      membershipCount,
                               char     *pathPolarity,
                               uint      xReleaseID,
                               uint     *membershipCountAlloc,
                               uint    **membershipReleased,
                               uint     *membershipReleasedCount);
#endif
