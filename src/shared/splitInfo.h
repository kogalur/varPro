#ifndef  RF_SPLIT_INFO_H
#define  RF_SPLIT_INFO_H
typedef struct splitInfo SplitInfo;
struct splitInfo {
  uint     size;
  char    *indicator;
  int    *randomVar;
  uint    *mwcpSizeAbs;
  void   **randomPts;
};  
typedef struct splitInfoMax SplitInfoMax;
struct splitInfoMax {
  uint   size;
  char  *indicator;
  double delta;
  int    splitParameter;
  double splitValueCont;
  uint   splitValueFactSize;
  uint  *splitValueFactPtr;
  double splitStatistic;
};
SplitInfo *makeSplitInfo(uint size);
void freeSplitInfo(SplitInfo *info);
void initSplitInfo(SplitInfo *info, uint size);
void deinitSplitInfo(SplitInfo *info);
SplitInfoMax *makeSplitInfoMax(uint size);
void freeSplitInfoMax(SplitInfoMax *info);
void initSplitInfoMax(SplitInfoMax *info, uint size);
void deinitSplitInfoMax(SplitInfoMax *info);
#endif
