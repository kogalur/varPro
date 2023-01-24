#ifndef  RF_LEAF_LINK_H
#define  RF_LEAF_LINK_H
#include "terminalBase.h"
typedef struct leafLinkedObj LeafLinkedObj;
struct leafLinkedObj {
  struct leafLinkedObj *fwdLink;
  struct leafLinkedObj *bakLink;
  void *nodePtr;
  TerminalBase *termPtr;
  uint nodeID;  
};
LeafLinkedObj *makeLeafLinkedObj(void);
LeafLinkedObj *makeAndSpliceLeafLinkedObj(LeafLinkedObj *tail);
void freeLeafLinkedObj(LeafLinkedObj *obj);
void freeLeafLinkedObjList(LeafLinkedObj *obj);
void freeLeafLinkedObjListRev(LeafLinkedObj *obj);
#endif
