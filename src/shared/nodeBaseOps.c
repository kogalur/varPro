
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "nodeBaseOps.h"
#include "splitInfo.h"
#include "nrutil.h"
#include "error.h"
NodeBase *makeNodeBase(uint xSize) {
  NodeBase *parent = (NodeBase*) gblock((size_t) sizeof(NodeBase));
  initNodeBase(parent, xSize);
  return parent;
}
void initNodeBase(NodeBase *parent, unsigned int xSize) {
  parent -> nodeID               = 0;
  parent -> pnodeID              = 0;
  parent -> blnodeID             = 0;
  parent -> brnodeID             = 0;
  parent -> fsrecID              = 0;
  parent -> parent = NULL;
  parent -> left               = NULL;
  parent -> right              = NULL;
  parent -> mate               = NULL;
  parent -> xSize = xSize;
  if (xSize > 0) {
    parent -> permissible = cvector(1, xSize);
    parent -> permissibleIndx = uivector(1, xSize);
    parent -> permissibleIndxSize = xSize;
    parent -> permissibleReIndxFlag = FALSE;
    parent -> permissibleOwnershipFlag = TRUE;
  }
  else {
    parent -> permissible = NULL;
    parent -> permissibleIndx = NULL;
    parent -> permissibleIndxSize = 0;
    parent -> permissibleReIndxFlag = FALSE;
    parent -> permissibleOwnershipFlag = FALSE;
  }
  parent -> splitFlag            = TRUE;
  parent -> variance             = RF_nativeNaN;
  parent -> mean                 = RF_nativeNaN;
  parent -> depth                = 0;
  parent -> splitInfo = NULL;
  parent -> splitInfoMax = NULL;
  parent -> repMembrIndx = NULL;
  parent -> allMembrIndx = NULL;
  parent -> repMembrSizeAlloc = parent -> repMembrSize = 0;
  parent -> allMembrSizeAlloc = parent -> allMembrSize = 0;
  parent -> oobMembrSizeAlloc = parent -> oobMembrSize = 0;
  parent -> oobMembrIndx = NULL;
}
void deinitNodeBase(NodeBase *parent) {
  if (parent -> xSize > 0) {
    if (parent -> permissibleOwnershipFlag) {
      if (parent -> permissible != NULL) {
        free_cvector(parent -> permissible, 1, parent -> xSize);
      }
      else {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  parent -> permissible is NULL:  parent %20x of size %10d", parent, parent -> xSize);
        RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
        RF_nativeExit();
      }
      if (parent -> permissibleIndx != NULL) {
        free_uivector(parent -> permissibleIndx, 1, parent -> xSize);
      }
      else {
      }
    }
    parent -> permissible = NULL;
    parent -> permissibleIndx = NULL;
    parent -> permissibleIndxSize = 0;
  }
  if (parent -> splitInfo != NULL) {
    freeSplitInfo(parent -> splitInfo);
    parent -> splitInfo = NULL;
  }
  if (parent -> splitInfoMax != NULL) {
    freeSplitInfoMax(parent -> splitInfoMax);
    parent -> splitInfoMax = NULL;
  }
  if (parent -> repMembrSizeAlloc > 0) {
    if (parent -> repMembrIndx != NULL) {
      free_uivector(parent -> repMembrIndx, 1, parent -> repMembrSizeAlloc);
      parent -> repMembrIndx = NULL;
    }
  }
  if (parent -> allMembrSizeAlloc > 0) {
    if (parent -> allMembrIndx != NULL) {
      free_uivector(parent -> allMembrIndx, 1, parent -> allMembrSizeAlloc);
      parent -> allMembrIndx = NULL;
    }
  }
  if (parent -> oobMembrSizeAlloc > 0) {
    if (parent -> oobMembrIndx != NULL) {
      free_uivector(parent -> oobMembrIndx, 1, parent -> oobMembrSizeAlloc);
      parent -> oobMembrIndx = NULL;
    }
  }
}
void freeNodeBase(NodeBase *parent) {
  deinitNodeBase(parent);
  free_gblock(parent, (size_t) sizeof(NodeBase));
}
void setParent(NodeBase *daughter, NodeBase *parent) {
  daughter -> parent = parent;
}
void setLeftDaughter(NodeBase *daughter, NodeBase *parent) {
  parent -> left = daughter;
}
void setRightDaughter(NodeBase *daughter, NodeBase *parent) {
  parent -> right = daughter;
}
