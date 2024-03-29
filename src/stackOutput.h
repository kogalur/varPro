#ifndef RF_STACK_OUTPUT_H
#define RF_STACK_OUTPUT_H
char getStrengthTreeCount(char       mode,
                          uint       ntree,
                          uint       maxTree,
                          uint      *tLeafCount,
                          uint      *strengthTreeCount);
void stackStrengthObjectsPtrOnly(char       mode,
                                 uint      *strengthTreeCount,
                                 uint     **strengthTreeID,
                                 uint     **branchCount,
                                 uint    ***branchID,
                                 uint    ***oobCount,
                                 uint    ***xReleaseCount,
                                 uint   ****xReleaseIDArray,
                                 uint   ****complementCount,
                                 uint   ****oobMembers,
                                 uint  *****complementMembers,
                                 uint    ***proxyIndv,
                                 uint    ***proxyIndvDepth);
void unstackStrengthObjectsPtrOnly(char      mode,
                                   uint      strengthTreeCount,
                                   uint     *strengthTreeID,
                                   uint     *branchCount,
                                   uint    **branchID,
                                   uint    **oobCount,
                                   uint    **xReleaseCount,
                                   uint   ***xReleaseIDArray,
                                   uint   ***complementCount,
                                   uint   ***oobMembers,
                                   uint  ****complementMembers,
                                   uint    **proxyIndv,
                                   uint    **proxyIndvDepth);
void selectTrees(uint    ntree,
                 uint    strengthTreeCount,
                 uint   *tLeafCount,
                 uint   *strengthTreeID);
void selectBranches(uint    b,
                    uint    treeID,
                    uint    maxRulesTree,
                    uint    leafCount,
                    uint    *branchCount,
                    uint   **branchID,
                    uint   **oobCount,
                    uint   **xReleaseCount,
                    uint  ***xReleaseIDArray,
                    uint  ***complementCount,
                    uint  ***oobMembers,
                    uint ****complementMembers,
                    uint   **proxyIndv,
                    uint   **proxyIndvDepth);
void freeStrengthBranchIDVectors(uint     strengthTreeCount,
                                 uint    *branchCount,
                                 uint   **branchID,
                                 uint   **oobCount,
                                 uint   **xReleaseCount,
                                 uint  ***xReleaseIDArray,
                                 uint  ***complementCount,
                                 uint  ***oobMembers,
                                 uint ****complementMembers,
                                 uint   **proxyIndv,
                                 uint   **proxyIndvDepth);
void freeReleaseIDArray(uint    branchCount,
                        uint   *oobCount,
                        uint   *xReleaseCount,
                        uint  **xReleaseIDArray,
                        uint  **complementCount,
                        uint  **oobMembers,
                        uint ***complementMembers,
                        uint   *proxyIndvDepth);
void freeComplementMembership(uint   xReleaseCount,
                              uint  *complementCount,
                              uint **complementMembers);
void writeStrengthArray(uint     *strengthTreeID,
                        uint      strengthTreeCount,
                        uint    **branchID,
                        uint     *branchCount,
                        uint    **oobCount,
                        uint   ***complementCount,
                        uint    **xReleaseCount,
                        uint   ***xReleaseIDArray,
                        uint     *treeID,
                        uint     *nodeID,
                        uint     *xReleaseID,
                        void     *oobCT,
                        void     *releaseStat);
void writeMembershipArray(uint      strengthTreeCount,
                          uint     *branchCount,
                          uint    **oobCount,
                          uint   ***complementCount,
                          uint    **xReleaseCount,
                          uint   ***oobMembers,
                          uint  ****complementMembers,
                          uint     *complementCT,
                          uint     *oobID,
                          uint     *complementID);
#endif
