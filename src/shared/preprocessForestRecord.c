
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
#include           "trace.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "preprocessForestRecord.h"
#include "nrutil.h"
#include "error.h"
void preprocessForestRecord(uint    ntree,
                            uint    hdim,
                            uint    totalNodeCount,
                            uint   *treeID,
                            uint   *nodeID,
                            int   **parmID,
                            uint  **mwcpSZ,
                            uint   *hcDim,
                            uint   *tLeafCount,
                            uint   *nodeSZ,
                            uint   *restoreTreeID,
                            ulong  *restoreTreeOffset,
                            uint   *nodeCount,
                            uint  **mwcpCT,
                            ulong **restoreMWCPoffset,
                            ulong  *totalTerminalCount) {
  uint   adj;
  ulong *mwcpOffset;
  uint previousTreeID;
  uint b;
  if (hdim == 0) {
    adj = 1;
  }
  else {
    adj = hdim;
  }
  mwcpOffset = ulvector(1, adj);
  for (uint j = 1; j <= adj; j++) {
    mwcpOffset[j] = 0;
  }
  previousTreeID = b = 0;
  for (ulong ui = 1; ui <= totalNodeCount; ui++) {
    if ((treeID[ui] > 0) && (treeID[ui] <= ntree)) {
      if (treeID[ui] != previousTreeID) {
        previousTreeID = restoreTreeID[++b] = treeID[ui];
        restoreTreeOffset[treeID[ui]] = ui;
      }
      nodeCount[treeID[ui]] ++;
      for (uint j = 1; j <= adj; j++) {
        mwcpCT[j][treeID[ui]] += mwcpSZ[j][ui];
      }
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
    for (uint j = 1; j <= adj; j++) {
      if (mwcpCT[j][restoreTreeID[b]] > 0) {
        restoreMWCPoffset[j][restoreTreeID[b]] = mwcpOffset[j];
        mwcpOffset[j] = mwcpOffset[j] + mwcpCT[j][restoreTreeID[b]];
      }
      else {
        restoreMWCPoffset[j][restoreTreeID[b]] = 0;
      }
    }
  }
  free_ulvector(mwcpOffset, 1, adj);
  *totalTerminalCount = 0;
  for (b = 1; b <= ntree; b++) {
    (*totalTerminalCount) += (ulong) tLeafCount[b];
  }
}
void preprocessForestRecordSynthetic() {
}
