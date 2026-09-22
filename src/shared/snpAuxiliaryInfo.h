#ifndef  RF_SNP_AUXILIARY_INFO_H
#define  RF_SNP_AUXILIARY_INFO_H
typedef enum auxiliaryDimensionType {
  RF_AUX_DIM_FIXED                =  1,
  RF_AUX_DIM_FACTOR_SIZE          = -1,
  RF_AUX_DIM_FACTOR_SIZE_PLUS_ONE = -2,
  RF_AUX_DIM_LEAF_COUNT           = -3,
  RF_AUX_DIM_BLOCK_COUNT          = -4,
  RF_AUX_DIM_CUSTOM_COUNT         = -5
} AuxiliaryDimensionType;
typedef struct auxiliaryDimension AuxiliaryDimension;
struct auxiliaryDimension {
  AuxiliaryDimensionType type;
  ulong value;
};
typedef struct snpAuxiliaryInfo SNPAuxiliaryInfo;
struct snpAuxiliaryInfo {
  char type;
  char *identity;
  uint slot;
  ulong linearSize;
  void *snpPtr;
  void *auxiliaryArrayPtr;
  uint dimSize;
  AuxiliaryDimension *dim;
};
typedef struct auxiliaryDimensionConstants AuxiliaryDimensionConstants;
struct auxiliaryDimensionConstants {
  uint *rFactorSize;
  uint *rFactorMap;
  uint *rTargetFactor;
  uint *tLeafCount;
  uint *holdBLKptr;
  uint *customSize;
};
#endif
