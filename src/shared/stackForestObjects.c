
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
#include           "trace.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "stackForestObjects.h"
#include "nrutil.h"
void stackForestObjectsAuxOnly(char        mode,
                               uint        hdim,
                               uint        ntree,
                               uint      **restoreTreeID,
                               ulong     **restoreTreeOffset,
                               ulong    ***restoreMWCPoffset,
                               int      ***parmID,
                               double   ***contPT,
                               double   ***contPTR,
                               uint     ***mwcpSZ,
                               uint     ***fsrecID,
                               uint     ***mwcpPT,
                               uint     ***mwcpCT) {
    uint i, k;
  if (mode != RF_GROW) {
    if (hdim == 0) {    
      *parmID   = (int **)    new_vvector(1, 1, NRUTIL_UPTR);
      *contPT   = (double **) new_vvector(1, 1, NRUTIL_DPTR);
      *mwcpSZ   = (uint **)   new_vvector(1, 1, NRUTIL_UPTR);
      *fsrecID  = (uint **)  new_vvector(1, 1, NRUTIL_UPTR);
      *mwcpPT   = (uint **)   new_vvector(1, 1, NRUTIL_UPTR);
      *mwcpCT   = (uint **)   new_vvector(1, 1, NRUTIL_UPTR);
    }
    else {
      *parmID   = (int **)    new_vvector(1, hdim, NRUTIL_UPTR);
      *contPT   = (double **) new_vvector(1, hdim, NRUTIL_DPTR);
      *contPTR  = (double **) new_vvector(1, hdim, NRUTIL_DPTR); 
      *mwcpSZ   = (uint **)   new_vvector(1, hdim, NRUTIL_UPTR);
      *fsrecID  = (uint **)   new_vvector(1, hdim, NRUTIL_UPTR);
      *mwcpPT   = (uint **)   new_vvector(1, hdim, NRUTIL_UPTR);
      *mwcpCT   = (uint **)   new_vvector(1, hdim, NRUTIL_UPTR);
    }
    *restoreTreeID = uivector(1, ntree);
    *restoreTreeOffset = ulvector(1, ntree);
    for (i = 1; i <= ntree; i++) {
      (*restoreTreeID)[i] = 0;
      (*restoreTreeOffset)[i] = 0;
    }
    if (hdim == 0) {    
      *restoreMWCPoffset = new_vvector(1, 1, NRUTIL_LPTR);
      (*restoreMWCPoffset)[1] = ulvector(1, ntree);
      for (i = 1; i <= ntree; i++) {
        (*restoreMWCPoffset)[1][i] = 0;
      }
      (*mwcpCT)[1] = uivector(1, ntree);
      for (i = 1; i <= ntree; i++) {
        (*mwcpCT)[1][i] = 0;
      }
    }
    else {
      *restoreMWCPoffset = new_vvector(1, hdim, NRUTIL_LPTR);
      for (k = 1; k <= hdim; k++) {
        (*restoreMWCPoffset)[k] = ulvector(1, ntree);
        for (i = 1; i <= ntree; i++) {
          (*restoreMWCPoffset)[k][i] = 0;
        }
        (*mwcpCT)[k] = uivector(1, ntree);
        for (i = 1; i <= ntree; i++) {
          (*mwcpCT)[k][i] = 0;
        }
      }
    }
  }
}
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
                                 uint   **mwcpCT) {
  free_new_vvector(parmID,  1, 1, NRUTIL_IPTR);
  free_new_vvector(contPT,  1, 1, NRUTIL_DPTR);
  free_new_vvector(mwcpSZ,  1, 1, NRUTIL_UPTR);
  free_new_vvector(fsrecID, 1, 1, NRUTIL_UPTR);
  free_new_vvector(mwcpPT,  1, 1, NRUTIL_UPTR);
  free_uivector(restoreTreeID, 1, ntree);
  free_ulvector(restoreTreeOffset, 1, ntree);
  free_ulvector(restoreMWCPoffset[1], 1, ntree);
  free_new_vvector(restoreMWCPoffset, 1, 1, NRUTIL_LPTR);
  free_uivector(mwcpCT[1], 1, ntree);
  free_new_vvector(mwcpCT, 1, 1, NRUTIL_UPTR);
}
