#ifndef  RF_SORTED_LINKED_H
#define  RF_SORTED_LINKED_H
typedef struct sortedLinkedObj SortedLinkedObj;
struct sortedLinkedObj {
  struct sortedLinkedObj *fwdLink;
  struct sortedLinkedObj *bakLink;
  uint rank;
  uint indx;
};
struct sortedLinkedObj *makeSortedLinkedObj(void);
void makeAndSpliceSortedLinkedObj(uint treeID,
                                  struct sortedLinkedObj **headPtr,
                                  struct sortedLinkedObj **tailPtr,
                                  uint *listLength,
                                  uint rank, uint indx);
void freeSortedLinkedObjList(struct sortedLinkedObj *obj);
void freeSortedLinkedObj(struct sortedLinkedObj *obj);
#endif
