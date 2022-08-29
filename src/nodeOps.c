
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "shared/globalCore.h"
#include           "shared/externalCore.h"
#include           "shared/trace.h"
#include           "global.h"
#include           "external.h"

// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "nodeOps.h"
#include "node.h"
#include "shared/nrutil.h"
#include "shared/nodeBaseOps.h"
void *makeNodeDerived(uint xSize) {
  Node *parent = (Node*) gblock((size_t) sizeof(Node));
  initNodeBase((NodeBase*) parent, xSize);
  return parent;
}
void freeNodeDerived(void *parent) {
  deinitNodeBase((NodeBase*) parent);
  free_gblock(parent, (size_t) sizeof(Node));
}
