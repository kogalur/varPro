
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "stackAuxiliaryInfo.h"
#include "nrutil.h"
#include "error.h"
void stackAuxiliaryInfoList(SNPAuxiliaryInfo ***list, uint count) {
   *list = new_vvector(0, count, NRUTIL_XPTR);
   for (uint i = 0; i <= count; i++) {
     (*list)[i] = NULL;
  }
}
void unstackAuxiliaryInfoAndList(AuxiliaryDimensionConstants *dimConst, char targetFlag, SNPAuxiliaryInfo **list, uint count) {
  SNPAuxiliaryInfo *auxInfoPtr;
  const AuxiliaryDimension *dim;
  uint  dimSize;
  ulong dim1, dim2, dim3;
  uint stringLength;
  for (uint ii = 0; ii < count; ii++) {
     auxInfoPtr = list[ii];
     if (auxInfoPtr != NULL) {
       dim = auxInfoPtr -> dim;
       dimSize = auxInfoPtr -> dimSize;
       stringLength = strlen(auxInfoPtr -> identity) + 1;
       free_cvector(auxInfoPtr -> identity, 1, stringLength);
       switch(auxInfoPtr -> type) {
       case NATIVE_TYPE_NUMERIC:
         if ((auxInfoPtr -> auxiliaryArrayPtr) == NULL) {
         }
         else if (dimSize == 4) {
           dim1 = getAuxDim(targetFlag, dim, 0 , 1, dimConst);
           for (ulong i = 1; i <= dim1; i++) {
             dim2 = getAuxDim(targetFlag, dim, i , 2, dimConst);
             if (dim2 > 0) {
               for (ulong j = 1; j <= dim2; j++) {
                 dim3 = getAuxDim(targetFlag, dim, j , 3, dimConst);
                 free_new_vvector((*((double *****) (auxInfoPtr -> auxiliaryArrayPtr)))[i][j], 1, dim3, NRUTIL_DPTR);
               }
               free_new_vvector((*((double *****) (auxInfoPtr -> auxiliaryArrayPtr)))[i], 1, dim2, NRUTIL_DPTR2);
             }
           }
           free_new_vvector((*((double *****) (auxInfoPtr -> auxiliaryArrayPtr))), 1, dim1, NRUTIL_DPTR3);
         }
         else if (dimSize == 3) {
           dim1 = getAuxDim(targetFlag, dim, 0 , 1, dimConst);
           for (ulong i = 1; i <= dim1; i++) {
             dim2 = getAuxDim(targetFlag, dim, i , 2, dimConst);
             if (dim2 > 0) {
               free_new_vvector((*((double ****) (auxInfoPtr -> auxiliaryArrayPtr)))[i], 1, dim2, NRUTIL_DPTR);
             }
           }
           free_new_vvector((*((double ****) (auxInfoPtr -> auxiliaryArrayPtr))), 1, dim1, NRUTIL_DPTR2);
         }
         else if (dimSize == 2) {
           dim1 = getAuxDim(targetFlag, dim, 0 , 1, dimConst);
           free_new_vvector((*((double ***) (auxInfoPtr -> auxiliaryArrayPtr))), 1, dim1, NRUTIL_DPTR);
         }
         else if (dimSize == 1) {
         }
         break;
       case NATIVE_TYPE_INTEGER:
         if ((auxInfoPtr -> auxiliaryArrayPtr) == NULL) {
         }
         else if (dimSize == 4) {
           dim1 = getAuxDim(targetFlag, dim, 0 , 1, dimConst);
           for (ulong i = 1; i <= dim1; i++) {
             dim2 = getAuxDim(targetFlag, dim, i , 2, dimConst);
             for (ulong j = 1; j <= dim2; j++) {
               dim3 = getAuxDim(targetFlag, dim, j , 3, dimConst);
               free_new_vvector((*((uint *****) (auxInfoPtr -> auxiliaryArrayPtr)))[i][j], 1, dim3, NRUTIL_UPTR);
             }
             free_new_vvector((*((uint *****) (auxInfoPtr -> auxiliaryArrayPtr)))[i], 1, dim2, NRUTIL_UPTR2);
           }
           free_new_vvector((*((uint *****) (auxInfoPtr -> auxiliaryArrayPtr))), 1,  dim1, NRUTIL_UPTR3);
         }
         else if (dimSize == 3) {
           dim1 = getAuxDim(targetFlag, dim, 0 , 1, dimConst);
           for (ulong i = 1; i <= dim1; i++) {
             dim2 = getAuxDim(targetFlag, dim, i , 2, dimConst);             
             free_new_vvector((*((uint ****) (auxInfoPtr -> auxiliaryArrayPtr)))[i], 1, dim2, NRUTIL_UPTR);
           }
           free_new_vvector((*((uint ****) (auxInfoPtr -> auxiliaryArrayPtr))), 1, dim1, NRUTIL_UPTR2);
         }
         else if (dimSize == 2) {
           dim1 = getAuxDim(targetFlag, dim, 0 , 1, dimConst);
           free_new_vvector((*((uint ***) (auxInfoPtr -> auxiliaryArrayPtr))), 1, dim1, NRUTIL_UPTR);
         }
         else if (dimSize == 1) {
         }
         break;
       }
       free_gblock(auxInfoPtr -> dim, ((size_t) dimSize + 1) * sizeof(AuxiliaryDimension));
       free_gblock(auxInfoPtr, sizeof(SNPAuxiliaryInfo));
     }
   }
  free_new_vvector(list, 0, count, NRUTIL_XPTR);
}
void allocateAuxiliaryInfo(AuxiliaryDimensionConstants *dimConst,
                           char   targetFlag,
                           char   type,
                           char  *stringIdentifier,
                           SNPAuxiliaryInfo **list,
                           uint   slot,
                           void  *snpPtr,
                           void  *auxiliaryArrayPtr,
                           uint   dimSize,
                           const AuxiliaryDimension *dim) {
  ulong dim1, dim2, dim3, dim4;
  ulong offset;
  uint stringLength;
  SNPAuxiliaryInfo *auxInfoPtr = (SNPAuxiliaryInfo*) gblock((size_t) sizeof(SNPAuxiliaryInfo));
  list[slot] = auxInfoPtr;
  auxInfoPtr -> slot = slot;
  auxInfoPtr -> type = type;
  stringLength = strlen(stringIdentifier) + 1;
  auxInfoPtr -> identity = cvector(1, stringLength);
  strcpy(auxInfoPtr -> identity, stringIdentifier);
  auxInfoPtr -> snpPtr = snpPtr;
  auxInfoPtr -> auxiliaryArrayPtr = auxiliaryArrayPtr;
  auxInfoPtr -> dimSize = dimSize;
  auxInfoPtr -> dim = (AuxiliaryDimension*) gblock(((size_t) dimSize + 1) * sizeof(AuxiliaryDimension));
  for (uint i = 1; i <= dimSize; i++) {
    (auxInfoPtr -> dim)[i] = dim[i];
  }
  switch(type) {
  case NATIVE_TYPE_NUMERIC:
    if (auxiliaryArrayPtr == NULL) {
    }
    else if (dimSize == 4) {
      offset = 0;
      dim1 = getAuxDim(targetFlag, dim, 0 , 1, dimConst);
      *((double *****) auxiliaryArrayPtr) = (double ****) new_vvector(1, dim1, NRUTIL_DPTR3);
      for (ulong i = 1; i <= dim1; i++) {
        dim2 = getAuxDim(targetFlag, dim, i , 2, dimConst);
        if (dim2 > 0) {
          (*((double *****) auxiliaryArrayPtr))[i] = (double ***) new_vvector(1, dim2, NRUTIL_DPTR2);
          for (ulong j = 1; j <= dim2; j++) {
            dim3 = getAuxDim(targetFlag, dim, j , 3, dimConst);
            (*((double *****) auxiliaryArrayPtr))[i][j] = (double **) new_vvector(1, dim3, NRUTIL_DPTR);
            for (ulong k = 1; k <= dim3; k++) {
              dim4 = getAuxDim(targetFlag, dim, k , 4, dimConst);
              (*((double *****) auxiliaryArrayPtr))[i][j][k] = (double *) snpPtr + offset - 1;
              offset += dim4;
            }
          }
        }
      }
    }
    else if (dimSize == 3) {
      offset = 0;      
      dim1 = getAuxDim(targetFlag, dim, 0 , 1, dimConst);
      *((double ****) auxiliaryArrayPtr) = (double ***) new_vvector(1, dim1, NRUTIL_DPTR2);
      for (ulong i = 1; i <= dim1; i++) {
        dim2 = getAuxDim(targetFlag, dim, i , 2, dimConst);
        if (dim2 > 0) {
          (*((double ****) auxiliaryArrayPtr))[i] = (double **) new_vvector(1, dim2, NRUTIL_DPTR);
          for (ulong j = 1; j <= dim2; j++) {
            dim3 = getAuxDim(targetFlag, dim, j , 3, dimConst);
            (*((double ****) auxiliaryArrayPtr))[i][j] = (double *) snpPtr + offset - 1;
            offset += dim3;
          }
        }
      }
    }
    else if (dimSize == 2) {
      offset = 0;
      dim1 = getAuxDim(targetFlag, dim, 0 , 1, dimConst);
      *((double ***) auxiliaryArrayPtr) = (double **) new_vvector(1, dim1, NRUTIL_DPTR);
      for (ulong i = 1; i <= dim1; i++) {
        dim2 = getAuxDim(targetFlag, dim, i , 2, dimConst);
        (*((double ***) auxiliaryArrayPtr))[i] = (double *) snpPtr + offset - 1;
          offset += dim2;
      }
    }
    else if (dimSize == 1) {
      *((double **) auxiliaryArrayPtr) = (double *) snpPtr - 1;
    }
    else {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  Invalid ( > 4 ) dimension in stackAndProtect() auxiliary arrays:  %10d", dimSize);
      RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
      RF_nativeExit();
    }
    break;
  case NATIVE_TYPE_INTEGER:
    if (auxiliaryArrayPtr == NULL) {
    }
    else if (dimSize == 4) {
      offset = 0;
      dim1 = getAuxDim(targetFlag, dim, 0 , 1, dimConst);
      *((uint *****) auxiliaryArrayPtr) = (uint ****) new_vvector(1, dim1, NRUTIL_UPTR3);
      for (ulong i = 1; i <= dim1; i++) {
        dim2 = getAuxDim(targetFlag, dim, i , 2, dimConst);
        (*((uint *****) auxiliaryArrayPtr))[i] = (uint ***) new_vvector(1, dim2, NRUTIL_UPTR2);
        for (ulong j = 1; j <= dim2; j++) {
          dim3 = getAuxDim(targetFlag, dim, j , 3, dimConst);
          (*((uint *****) auxiliaryArrayPtr))[i][j] = (uint **) new_vvector(1, dim3, NRUTIL_UPTR);
          for (ulong k = 1; k <= dim3; k++) {
            dim4 = getAuxDim(targetFlag, dim, k , 4, dimConst);
            (*((uint *****) auxiliaryArrayPtr))[i][j][k] = (uint *) snpPtr + offset - 1;
            offset += dim4;
          }
        }
      }
    }
    else if (dimSize == 3) {
      offset = 0;
      dim1 = getAuxDim(targetFlag, dim, 0 , 1, dimConst);
      *((uint ****) auxiliaryArrayPtr) = (uint ***) new_vvector(1, dim1, NRUTIL_UPTR2);
      for (ulong i = 1; i <= dim1; i++) {
        dim2 = getAuxDim(targetFlag, dim, i , 2, dimConst);
        (*((uint ****) auxiliaryArrayPtr))[i] = (uint **) new_vvector(1, dim2, NRUTIL_UPTR);
        for (ulong j = 1; j <= dim2; j++) {
          dim3 = getAuxDim(targetFlag, dim, j , 3, dimConst);
          (*((uint ****) auxiliaryArrayPtr))[i][j] = (uint *) snpPtr + offset - 1;
            offset += dim3;
        }
      }
    }
    else if (dimSize == 2) {
      offset = 0;
      dim1 = getAuxDim(targetFlag, dim, 0 , 1, dimConst);
      *((uint ***) auxiliaryArrayPtr) = (uint **) new_vvector(1, dim1, NRUTIL_UPTR);
      for (ulong i = 1; i <= dim1; i++) {
        dim2 = getAuxDim(targetFlag, dim, i , 2, dimConst);
        (*((uint ***) auxiliaryArrayPtr))[i] = (uint *) snpPtr + offset - 1;
          offset += dim2;
      }
    }
    else if (dimSize == 1) {
      *((uint **) auxiliaryArrayPtr) = (uint *) snpPtr - 1;
    }
    else {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  Invalid ( > 4 ) dimension in stackAndProtect() auxiliary arrays:  %10d", dimSize);
      RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
      RF_nativeExit();
    }
    break;
  }
}
ulong getAuxDim(char flag, const AuxiliaryDimension *dim, ulong iterIndex, uint slot, AuxiliaryDimensionConstants *dimConst) {
  ulong result = 0;
  uint *rFactorSize    = dimConst -> rFactorSize;
  uint *rFactorMap     = dimConst -> rFactorMap;
  uint *rTargetFactor  = dimConst -> rTargetFactor;
  uint *tLeafCount     = dimConst -> tLeafCount;
  uint *holdBLKptr     = dimConst -> holdBLKptr;
  uint *customSize     = dimConst -> customSize;
  if ((slot == 1) && (dim[slot].type != RF_AUX_DIM_FIXED)) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  First auxiliary dimension must be fixed in getAuxDim():  %10d", dim[slot].type);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  }
  switch (dim[slot].type) {
  case RF_AUX_DIM_FIXED:
    result = dim[slot].value;
    break;
  case RF_AUX_DIM_FACTOR_SIZE:
    if (flag) {
      result = rFactorSize[rFactorMap[rTargetFactor[iterIndex]]];
    }
    else {
      result = rFactorSize[iterIndex];
    }
    break;
  case RF_AUX_DIM_FACTOR_SIZE_PLUS_ONE:
    if (flag) {
      result = (ulong) rFactorSize[rFactorMap[rTargetFactor[iterIndex]]] + 1;
    }
    else {
      result = (ulong) rFactorSize[iterIndex] + 1;
    }
    break;
  case RF_AUX_DIM_LEAF_COUNT:
    result = tLeafCount[iterIndex];
    break;
  case RF_AUX_DIM_BLOCK_COUNT:
    result = holdBLKptr[iterIndex];
    break;
  case RF_AUX_DIM_CUSTOM_COUNT:
    result = customSize[iterIndex];
    break;
  default:
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Inconsistent internal dimension type in getAuxDim():  %10d", dim[slot].type);
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
    break;
  }
  return result;
}
AuxiliaryDimensionConstants *makeAuxDimConsts(uint *rFactorSize,
                                              uint  rFactorCount,
                                              uint *rFactorMap,
                                              uint *rTargetFactor,
                                              uint  rTargetFactorCount,
                                              uint *tLeafCount,
                                              uint *holdBLKptr,
                                              uint *customSize) {
  AuxiliaryDimensionConstants *obj = (AuxiliaryDimensionConstants*) gblock((size_t) sizeof(AuxiliaryDimensionConstants));
  obj -> rFactorSize = rFactorSize;
  obj -> rFactorMap = rFactorMap;
  obj -> rTargetFactor = rTargetFactor;
  obj -> tLeafCount = tLeafCount;
  obj -> holdBLKptr = holdBLKptr;
  obj -> customSize = customSize;
  return obj;
}
void freeAuxDimConsts(AuxiliaryDimensionConstants *obj) {
    free_gblock(obj, (size_t) sizeof(AuxiliaryDimensionConstants));
}
