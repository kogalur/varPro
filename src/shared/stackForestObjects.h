#ifndef  RF_STACK_FOREST_OBJECTS_H
#define  RF_STACK_FOREST_OBJECTS_H
void stackForestObjectsAuxOnly(char        mode,
                               uint        ntree,
                               uint      **restoreTreeID,
                               ulong     **restoreTreeOffset,
                               ulong    ***restoreMWCPoffset,
                               int      ***parmID,
                               double   ***contPT,
                               uint     ***mwcpSZ,
                               uint     ***fsrecID,
                               uint     ***mwcpPT,
                               uint     ***mwcpCT);
void unstackForestObjectsAuxOnly(char     mode,
                                 uint     ntree,
                                 uint    *restoreTreeID,
                                 ulong   *restoreTreeOffset,
                                 ulong  **restoreMWCPoffset,
                                 int    **parmID,
                                 double **contPT,
                                 uint   **mwcpSZ,
                                 uint   **fsrecID,
                                 uint   **mwcpPT,
                                 uint   **mwcpCT);
#endif
