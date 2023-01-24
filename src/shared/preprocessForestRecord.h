#ifndef  RF_PREPROCESS_FOREST_RECORD_H
#define  RF_PREPROCESS_FOREST_RECORD_H
void preprocessForestRecord(uint    ntree,
                            uint    totalNodeCount,
                            uint   *treeID,
                            uint   *nodeID,
                            int   **parmID,
                            uint  **mwcpSZ,
                            uint   *tLeafCount,
                            uint   *nodeSZ,
                            uint   *restoreTreeID,
                            ulong  *restoreTreeOffset,
                            uint   *nodeCount,
                            uint  **mwcpCT,
                            ulong **restoreMWCPoffset,
                            ulong  *totalTerminalCount);
void preprocessForestRecordSynthetic();
#endif
