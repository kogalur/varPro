#ifndef  RF_SPLIT_INFO_H
#define  RF_SPLIT_INFO_H
typedef struct splitInfo SplitInfo;
struct splitInfo {
  uint     size;
  char    *indicator;
  uint     hcDim;
  int    *randomVar;
  uint    *mwcpSizeAbs;
  void   **randomPts;
  void   **randomPtsRight;
  uint  pairCT;  
  int  *augmX1;  
  int  *augmX2;  
  int  *augmXS;  
  char  sythFlag;
  uint *sythIndx;
  double timeCutLeft;
  double timeCutRight;
  uint splitRank;
};  
typedef struct splitInfoMax SplitInfoMax;
struct splitInfoMax {
  uint   size;
char  *indicator;
  double deltaMax;
  int    splitParameterMax;
  double splitValueMaxCont;
  uint   splitValueMaxFactSize;
  uint  *splitValueMaxFactPtr;
  uint   splitAugmMaxPairOne;
  uint   splitAugmMaxPairTwo;
  uint   splitAugmMaxSyth;
  uint splitRank;
  double splitStatistic;
};
SplitInfo *makeSplitInfo(uint size);
void freeSplitInfo(SplitInfo *info);
SplitInfoMax *makeSplitInfoMax(uint size);
void freeSplitInfoMax(SplitInfoMax *info);
#endif
