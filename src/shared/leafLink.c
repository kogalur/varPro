
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "leafLink.h"
#include "nrutil.h"
#include "terminalBase.h"
LeafLinkedObj *makeLeafLinkedObj(void) {
  LeafLinkedObj *obj = (LeafLinkedObj*) gblock((size_t) sizeof(LeafLinkedObj));
  obj -> fwdLink = NULL;
  obj -> bakLink = NULL;
  obj -> nodePtr = NULL;
  obj -> termPtr = NULL;
  obj -> nodeID = 0;
  return obj;
}
LeafLinkedObj *makeAndSpliceLeafLinkedObj(LeafLinkedObj *tail) {
  LeafLinkedObj *obj = makeLeafLinkedObj();
  tail -> fwdLink = obj;
  obj -> bakLink = tail;
  return obj;
}
void freeLeafLinkedObj(LeafLinkedObj *obj) {
  if (obj -> termPtr != NULL) {
    freeTerminal(obj -> termPtr);
    obj -> termPtr = NULL;
  }
  free_gblock(obj, (size_t) sizeof(LeafLinkedObj));
}
void freeLeafLinkedObjList(LeafLinkedObj *obj) {
  if (obj -> fwdLink != NULL) {
    freeLeafLinkedObjList(obj -> fwdLink);
  }
  freeLeafLinkedObj(obj);
}
void freeLeafLinkedObjListRev(LeafLinkedObj *obj) {
  if (obj -> bakLink != NULL) {
    freeLeafLinkedObjListRev(obj -> bakLink);
  }
  freeLeafLinkedObj(obj);
}
