
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "preprocessForestRecord.h"
#include "nrutil.h"
#include "error.h"
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
                            ulong  *totalTerminalCount) {
  ulong *mwcpOffset;
  uint previousTreeID;
  uint b;
  mwcpOffset = ulvector(1, 1);
  mwcpOffset[1] = 0;
  previousTreeID = b = 0;
  for (ulong ui = 1; ui <= totalNodeCount; ui++) {
    if ((treeID[ui] > 0) && (treeID[ui] <= ntree)) {
      if (treeID[ui] != previousTreeID) {
        previousTreeID = restoreTreeID[++b] = treeID[ui];
        restoreTreeOffset[treeID[ui]] = ui;
      }
      nodeCount[treeID[ui]] ++;
      mwcpCT[1][treeID[ui]] += mwcpSZ[1][ui];
    }
    else {
      RF_nativeError("\nRF-SRC:  Diagnostic Trace of Tree Record:  \n");
      RF_nativeError("\nRF-SRC:      treeID     nodeID ");
      RF_nativeError("\nRF-SRC:  %10d %10d \n", treeID[ui], nodeID[ui]);
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  Invalid forest input record at line:  %20lu", ui);
      RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
      RF_nativeExit();
    }
  }
  for (b = 1; b <= ntree; b++) {
    if (mwcpCT[1][restoreTreeID[b]] > 0) {
      restoreMWCPoffset[1][restoreTreeID[b]] = mwcpOffset[1];
      mwcpOffset[1] = mwcpOffset[1] + mwcpCT[1][restoreTreeID[b]];
    }
    else {
      restoreMWCPoffset[1][restoreTreeID[b]] = 0;
    }
  }
  free_ulvector(mwcpOffset, 1, 1);
  *totalTerminalCount = 0;
  for (b = 1; b <= ntree; b++) {
    (*totalTerminalCount) += (ulong) tLeafCount[b];
  }
}
void preprocessForestRecordSynthetic() {
}
