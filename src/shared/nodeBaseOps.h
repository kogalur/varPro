#ifndef RF_NODE_BASE_OPS_H
#define RF_NODE_BASE_OPS_H
#include "nodeBase.h"
NodeBase *makeNodeBase(uint xSize);
void initNodeBase(NodeBase *parent, unsigned int xSize);
void deinitNodeBase(NodeBase *parent);
void freeNodeBase(NodeBase *parent);
void setParent(
  NodeBase *daughter,
  NodeBase *parent
);
void setLeftDaughter(
   NodeBase *daughter,
   NodeBase *parent
);
void setRightDaughter(
  NodeBase *daughter,
  NodeBase *parent
);
#endif
