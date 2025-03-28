
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "stackPreDefined.h"
#include "factor.h"
#include "factorOps.h"
#include "leafLink.h"
#include "nrutil.h"
#include "error.h"
char stackIncomingArrays(char     mode,
                         uint     ntree,
                         uint     timeInterestSize,
                         uint     ytry,
                         uint     mtry,
                         double  *xWeight,
                         double  *yWeight,
                         uint     subjSize,
                         double  *subjWeight,
                         double  *xWeightStat,
                         uint     nodeSize,
                         uint     bootstrapSize,
                         uint     splitRule,
                         uint     quantileSize,
                         double  *quantile,
                         uint     ySize,
                         char    *rType,
                         uint     frSize,
                         uint    *subjIn,
                         uint     observationSize,
                         double **responseIn,
                         double **fresponseIn,
                         uint     xSize,
                         char    *xType,
                         uint     fobservationSize,
                         double **observationIn,
                         double **fobservationIn,
                         uint   **yIndex,
                         uint   **yIndexZero,
                         uint    *timeIndex,
                         uint    *startTimeIndex,
                         uint    *statusIndex,
                         double **masterTime,
                         uint    *masterTimeSize,
                         uint    *sortedTimeInterestSize,
                         uint   **startMasterTimeIndexIn,
                         uint   **masterTimeIndexIn,
                         uint    *ptnCount,
                         uint    *ySizeProxy,
                         uint    *yIndexZeroSize) {
  uint i;
  char result;
  result = TRUE;
  stackIncomingResponseArrays(mode,
                              ySize,
                              rType,
                              subjIn,
                              frSize,
                              observationSize,
                              fobservationSize,
                              responseIn,
                              fresponseIn,
                              yIndex,
                              yIndexZero,
                              timeIndex,
                              startTimeIndex,
                              statusIndex,
                              masterTime,
                              masterTimeSize,
                              sortedTimeInterestSize,
                              startMasterTimeIndexIn,
                              masterTimeIndexIn,
                              ptnCount,
                              ySizeProxy,
                              yIndexZeroSize);
  if (mode == RF_GROW) {
    if (nodeSize < 1) {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  Parameter verification failed.");
      RF_nativeError("\nRF-SRC:  Minimum node size must be greater than zero:  %10d \n", nodeSize);
      result = FALSE;
    }
    if (bootstrapSize < 1) {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  Parameter verification failed.");
      RF_nativeError("\nRF-SRC:  Bootstrap size must be greater than zero:  %12d \n", bootstrapSize);
      result = FALSE;
    }
    if ( splitRule > MAXM_SPLIT) {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  Parameter verification failed.");
      RF_nativeError("\nRF-SRC:  Invalid split rule:  %10d \n", splitRule);
      result = FALSE;
    }
    if (splitRule == USPV_SPLIT) {
      if (xSize < 2) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Parameter verification failed.");
        RF_nativeError("\nRF-SRC:  Number of covariates must be greater than or equal to two (2) with specified split rule:  %10d \n", xSize);
        result = FALSE;
      }
      if ( ((int) (xSize - ytry) < 1) || (mtry > xSize) ) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Parameter verification failed.");
        RF_nativeError("\nRF-SRC:  ytry and mtry must be within range:  %10d %10d \n", ytry,  mtry);
        result = FALSE;
      }
    }
    else {
      if (ySize == 0) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Parameter verification failed.");
        RF_nativeError("\nRF-SRC:  Number of response variables must be greater than zero:  %10d \n", ySize);
        result = FALSE;
      }
      if ( ((mtry < 1) || (mtry > xSize)) ) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Parameter verification failed.");
        RF_nativeError("\nRF-SRC:  Number of random covariate parameters must be greater");
        RF_nativeError("\nRF-SRC:  than zero and less than or equal to the total number of covariates:  %10d \n", mtry);
        result = FALSE;
      }
    }
    if (splitRule != USPV_SPLIT) {
      if ((*timeIndex != 0) && (*statusIndex != 0)) {
      }
      else {
        if (*ySizeProxy == 0) {
          RF_nativeError("\nRF-SRC:  *** ERROR *** ");
          RF_nativeError("\nRF-SRC:  No non-[S] and non-[C] responses found.");
          result = FALSE;
        }
        if (ytry > *ySizeProxy) {
          RF_nativeError("\nRF-SRC:  *** ERROR *** ");
          RF_nativeError("\nRF-SRC:  Parameter verification failed.");
          RF_nativeError("\nRF-SRC:  ytry must be within range:  %10d \n", ytry);
          result = FALSE;
        }
      }
    }
    if(xWeightStat != NULL) {
      for (uint i = 1; i <= xSize; i++) {
        if(xWeightStat[i] < 0) {
          RF_nativeError("\nRF-SRC:  *** ERROR *** ");
          RF_nativeError("\nRF-SRC:  Parameter verification failed.");
          RF_nativeError("\nRF-SRC:  Split statistical weight elements must be greater than or equal to zero:  %12.4f \n", xWeightStat[i]);
          result = FALSE;
        }
      }
    }
    if(ySize > 0) {
      if(yWeight != NULL) {
        for (uint i = 1; i <= ySize; i++) {
          if(yWeight[i] < 0) {
            RF_nativeError("\nRF-SRC:  *** ERROR *** ");
            RF_nativeError("\nRF-SRC:  Parameter verification failed.");
            RF_nativeError("\nRF-SRC:  Y-weight elements must be greater than or equal to zero:  %12.4f \n", yWeight[i]);
            result = FALSE;
          }
        }
      }
    }
    if(xWeight != NULL) {
      for (uint i = 1; i <= xSize; i++) {
        if(xWeight[i] < 0) {
          RF_nativeError("\nRF-SRC:  *** ERROR *** ");
          RF_nativeError("\nRF-SRC:  Parameter verification failed.");
          RF_nativeError("\nRF-SRC:  X-weight elements must be greater than or equal to zero:  %12.4f \n", xWeight[i]);
          result = FALSE;
        }
      }
    }
    if ((*timeIndex == 0) && (*statusIndex == 0)) {
      if  ((splitRule == REGR_QUANT) || (splitRule == LARG_QUANT)) {
        if (quantileSize > 0) {
        }
        else {
          RF_nativeError("\nRF-SRC:  *** ERROR *** ");
          RF_nativeError("\nRF-SRC:  Quantile regression split rules require the presence of a probability vector.");
          result = FALSE;
        }
      }
      if  (splitRule == MAHALANOBIS) {
      }
    }
    else if ((*timeIndex != 0) && (*statusIndex != 0)) {
      if (*startTimeIndex == 0) {
      }
      else {
      }
    }
    else {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  Data set contains mixed outcomes with no comatible split rule.");
      RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
      result = FALSE;
    }
  }
  if (quantileSize > 0) {
    for (uint i = 1; i <= quantileSize; i++) {
      if ((0 < quantile[i]) && (quantile[i] <= 1.0)) {
      }
      else {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Parameter verification failed.");
        RF_nativeError("\nRF-SRC:  Quantile value is out of range (0, 1):  %.10e ", quantile[i]);
        result = FALSE;
      }
    }
  }
  for (i = 1; i <= xSize; i++) {
    if ((xType[i] != 'B') &&
        (xType[i] != 'R') &&
        (xType[i] != 'I') &&
        (xType[i] != 'C')) {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  Invalid type:  [%10d] = %2c", i, xType[i]);
      RF_nativeError("\nRF-SRC:  Variables must be [B], [R], [I] or [C].");
      result = FALSE;
    }
  }
  return result;
}
void unstackIncomingArrays(char    mode,
                           uint    ySize,
                           uint   *yIndex,
                           uint   *yIndexZero) {
  unstackIncomingResponseArrays(mode,
                                ySize,
                                yIndex,
                                yIndexZero);
}
char stackIncomingResponseArrays(char     mode,
                                 uint     ySize,
                                 char    *rType,
                                 uint    *subjIn,
                                 uint     frSize,
                                 uint     observationSize,
                                 uint     fobservationSize,
                                 double **responseIn,
                                 double **fresponseIn,
                                 uint   **yIndex,
                                 uint   **yIndexZero,
                                 uint    *timeIndex,
                                 uint    *startTimeIndex,
                                 uint    *statusIndex,
                                 double **masterTime,
                                 uint    *masterTimeSize,
                                 uint    *sortedTimeInterestSize,
                                 uint   **startMasterTimeIndexIn,
                                 uint   **masterTimeIndexIn,
                                 uint    *ptnCount,
                                 uint    *ySizeProxy,
                                 uint    *yIndexZeroSize) {
  uint i, j;
  char result;
  result = TRUE;
  *startTimeIndex = *timeIndex = *statusIndex = 0;
  *masterTime = NULL;
  *masterTimeSize = 0;
  *sortedTimeInterestSize = 0;
  *startMasterTimeIndexIn = *masterTimeIndexIn = NULL;
  *yIndex = *yIndexZero = NULL;
  *ySizeProxy = *yIndexZeroSize = 0;
  if (ySize > 0) {
    *yIndex = uivector(1, ySize);
    *yIndexZero = uivector(1, ySize);
    j = 0;
    for (i = 1; i <= ySize; i++) {
      if ((rType[i] != 'B') &&
          (rType[i] != 'R') &&
          (rType[i] != 'I') &&
          (rType[i] != 'C') &&
          (rType[i] != 't') &&
          (rType[i] != 'T') &&
          (rType[i] != 'S')) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Invalid type:  [%10d] = %2c", i, rType[i]);
        RF_nativeError("\nRF-SRC:  Variables must be [B], [R], [I], [C], [t], [T], [S].");
        result = FALSE;
      }
      if (result) {
        (*yIndex)[i] = (*yIndexZero)[i] = 0;
        if (rType[i] == 'T') {
          *timeIndex = i;
        }
        if (rType[i] == 't') {
          *startTimeIndex = i;
        }
        else if (rType[i] == 'S') {
          *statusIndex = i;
        }
        else {
          (*yIndex)[++j] = i;
        }
      }
    }
    if (mode == RF_PRED) {
      if (frSize > 0) {
        if (ySize != frSize) {
          RF_nativeError("\nRF-SRC:  *** ERROR *** ");
          RF_nativeError("\nRF-SRC:  train and test outcome/response matrices must be of the same dimension.  ");
          RF_nativeError("\nRF-SRC:  train vs test:  %10d vs %10d  ", ySize, frSize);
          result = FALSE;
        }
      }
      else {
        if ((RF_opt & OPT_PERF) | (RF_opt & OPT_VIMP)) {
          RF_nativeError("\nRF-SRC:  *** ERROR *** ");
          RF_nativeError("\nRF-SRC:  test outcome/response matrix must be present when PERF or VIMP is requested.  ");
          result = FALSE;
        }
      }
    }
    if ((*timeIndex > 0) && (*statusIndex > 0)) {
      *ptnCount = 0;
    }
    *ySizeProxy = ySize - ((*startTimeIndex == 0) ? 0:1) - ((*timeIndex == 0) ? 0:1) - ((*statusIndex == 0) ? 0:1);
    *yIndexZeroSize = 0;
  }
  else {
    *ySizeProxy = 0;
    *yIndexZeroSize = 0;
  }
  if (RF_opt & OPT_ANON) {
    if (mode != RF_PRED) {
      RF_opt = RF_opt & (~OPT_PERF);
      RF_opt = RF_opt & (~OPT_VIMP);
    }
  }
  return result;
}
void unstackIncomingResponseArrays(char  mode,
                                   uint  ySize,
                                   uint *yIndex,
                                   uint *yIndexZero) {
  if (ySize > 0) {
    if (yIndex != NULL) {
      free_uivector(yIndex, 1, ySize);
    }
    if (yIndexZero != NULL) {
      free_uivector(yIndexZero, 1, ySize);
    }
  }
}
char stackPreDefinedCommonArrays(char          mode,
                                 uint          ntree,
                                 double       *subjWeight,
                                 uint          timeIndex,
                                 uint          startTimeIndex,
                                 uint          statusIndex,
                                 uint          bootstrapSize,
                                 uint        **bootstrapIn,
                                 uint          subjSize,
                                 uint          ptnCount,
                                 uint         *getTree,
                                 uint          observationSize,
                                 NodeBase      ****nodeMembership,
                                 TerminalBase  ****tTermMembership,
                                 NodeBase      ****pNodeMembership,
                                 TerminalBase  ****pTermMembership,
                                 LeafLinkedObj ****hTermMembership,
                                 TerminalBase  ****tTermList,
                                 NodeBase      ****pNodeList,
                                 TerminalBase  ****pTermList,
                                 char       ***bootMembershipFlag,
                                 char       ***oobMembershipFlag,
                                 uint       ***bootMembershipCount,
                                 uint       ***ibgMembershipIndex,
                                 uint       ***oobMembershipIndex,
                                 uint        **oobSize,
                                 uint        **ibgSize,
                                 uint       ***bootMembershipIndex,
                                 uint        **maxDepth,
                                 uint        **orderedTreeIndex,
                                 uint        **serialTreeIndex,
                                 NodeBase       ***root,
                                 uint        **nodeCount,
                                 LeafLinkedObj ***leafLinkedObjHead,
                                 LeafLinkedObj ***leafLinkedObjTail,
                                 uint        **pLeafCount,
                                 uint        **getTreeIndex,
                                 uint         *getTreeCount,
                                 uint         *subjWeightType,
                                 uint        **subjWeightSorted,
                                 uint         *subjWeightDensitySize,
                                 uint         *identityMembershipIndexSize,
                                 uint        **identityMembershipIndex) {
  uint i, j, k;
  char result;
  result = TRUE;
  *nodeMembership = (NodeBase ***)     new_vvector(1, ntree, NRUTIL_NPTR2);
  *tTermMembership = (TerminalBase ***) new_vvector(1, ntree, NRUTIL_TPTR2);
  if ((startTimeIndex > 0) && (timeIndex > 0) && (statusIndex > 0)) {
    *hTermMembership = (LeafLinkedObj ***) new_vvector(1, ntree, NRUTIL_LEAFPTR2);
  }
  *tTermList = (TerminalBase ***) new_vvector(1, ntree, NRUTIL_NPTR2);
  *leafLinkedObjHead = (LeafLinkedObj **) new_vvector(1, ntree, NRUTIL_LEAFPTR);
  *leafLinkedObjTail = (LeafLinkedObj **) new_vvector(1, ntree, NRUTIL_LEAFPTR);
  *nodeCount = uivector(1, ntree);
  for (i = 1; i <= ntree; i++) {
    (*tTermList)[i] = NULL;
    (*nodeCount)[i] = 0;
    (*leafLinkedObjHead)[i] = NULL;
    (*leafLinkedObjHead)[i] = NULL;
  }
  *bootMembershipIndex = (uint **) new_vvector(1, ntree, NRUTIL_UPTR);
  *bootMembershipFlag = (char **) new_vvector(1, ntree, NRUTIL_CPTR);
  *bootMembershipCount = (uint **) new_vvector(1, ntree, NRUTIL_UPTR);
  *oobMembershipFlag = (char **) new_vvector(1, ntree, NRUTIL_CPTR);
  *ibgMembershipIndex = (uint **) new_vvector(1, ntree, NRUTIL_UPTR);
  *oobMembershipIndex = (uint **) new_vvector(1, ntree, NRUTIL_UPTR);
  *oobSize = uivector(1, ntree);
  *ibgSize = uivector(1, ntree);
  *maxDepth = uivector(1, ntree);
  *orderedTreeIndex = uivector(1, ntree);
  for (i = 1; i <= ntree; i++) {
    (*orderedTreeIndex)[i] = i;
  }
  *serialTreeIndex = uivector(1, ntree);
  *root = (NodeBase **) new_vvector(1, ntree, NRUTIL_NPTR);
  for (i = 1; i <= ntree; i++) {
    (*root)[i] = NULL;
  }
  if (ptnCount > 0) {
    *pNodeMembership = (NodeBase ***)     new_vvector(1, ntree, NRUTIL_NPTR2);
    *pTermMembership = (TerminalBase ***) new_vvector(1, ntree, NRUTIL_NPTR2);
    *pNodeList = (NodeBase ***)     new_vvector(1, ntree, NRUTIL_NPTR2);
    *pTermList = (TerminalBase ***) new_vvector(1, ntree, NRUTIL_NPTR2);
    *pLeafCount = uivector(1, ntree);
  }
  if ((RF_opt & OPT_BOOT_TYP1) || (RF_opt & OPT_BOOT_TYP2)) {
    for (i = 1; i <= subjSize; i++) {
      if(subjWeight[i] < 0) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Parameter verification failed.");
        RF_nativeError("\nRF-SRC:  Subject-weight elements must be greater than or equal to zero:  %12.4f \n", subjWeight[i]);
        RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
        RF_nativeExit();
      }
    }
    stackWeights(subjWeight,
                 subjSize,
                 subjWeightType,
                 subjWeightSorted,
                 subjWeightDensitySize); 
  }
  *getTreeIndex = uivector(1, ntree);
  if ((mode == RF_GROW) || (getTree == NULL)) {
    for (i = 1; i <= ntree; i++) {
      (*getTreeIndex)[i] = i;
    }
    *getTreeCount = ntree;
  }
  else {
    *getTreeCount = 0;    
    for (i = 1; i <= ntree; i++) {
      if (getTree[i] != 0) {
        (*getTreeIndex)[++(*getTreeCount)] = i;
      }
    }
  }
  *identityMembershipIndexSize = (bootstrapSize > observationSize ) ? bootstrapSize : observationSize;
  *identityMembershipIndex = uivector(1, *identityMembershipIndexSize);
  for (i = 1; i <= *identityMembershipIndexSize; i++) {
    (*identityMembershipIndex)[i] = i;
  }
  if (RF_opt & OPT_BOOT_TYP2) {
    for (i = 1; i <= ntree; i++) {
      k = 0;
      for (j = 1; j <= subjSize; j++) {
        k += bootstrapIn[i][j];
      }
      if(k != bootstrapSize) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Parameter verification failed.");
        RF_nativeError("\nRF-SRC:  Bootstrap size implied by samp matrix inconsistent:  %12d found vs. %12d specified \n", k, bootstrapSize);
        result = FALSE;
      }
    }
  }
  return result;
}
void unstackPreDefinedCommonArrays(char             mode,
                                   uint             ntree,
                                   uint             timeIndex,
                                   uint             startTimeIndex,
                                   uint             statusIndex,
                                   uint             subjSize,
                                   uint             ptnCount,
                                   NodeBase          ***nodeMembership,
                                   TerminalBase      ***tTermMembership,
                                   NodeBase          ***pNodeMembership,
                                   TerminalBase      ***pTermMembership,
                                   LeafLinkedObj ***hTermMembership,
                                   TerminalBase      ***tTermList,
                                   NodeBase          ***pNodeList,
                                   TerminalBase      ***pTermList,
                                   char           **bootMembershipFlag,
                                   char           **oobMembershipFlag,
                                   uint           **bootMembershipCount,
                                   uint           **ibgMembershipIndex,
                                   uint           **oobMembershipIndex,
                                   uint            *oobSize,
                                   uint            *ibgSize,
                                   uint           **bootMembershipIndex,
                                   uint            *maxDepth,
                                   uint            *orderedTreeIndex,
                                   uint            *serialTreeIndex,
                                   NodeBase           **root,
                                   uint            *nodeCount,
                                   LeafLinkedObj  **leafLinkedObjHead,
                                   LeafLinkedObj  **leafLinkedObjTail,
                                   uint            *pLeafCount,
                                   uint            *getTreeIndex,
                                   uint             subjWeightType,
                                   uint            *subjWeightSorted,
                                   uint             identityMembershipIndexSize,
                                   uint            *identityMembershipIndex) {
  free_new_vvector(nodeMembership, 1, ntree, NRUTIL_NPTR2);
  free_new_vvector(tTermMembership, 1, ntree, NRUTIL_TPTR2);
  if ((startTimeIndex > 0) && (timeIndex > 0) && (statusIndex > 0)) {
    free_new_vvector(hTermMembership, 1, ntree, NRUTIL_LEAFPTR2);
  }
  free_new_vvector(tTermList, 1, ntree, NRUTIL_TPTR2);
  free_new_vvector(leafLinkedObjHead, 1, ntree, NRUTIL_LEAFPTR);
  free_new_vvector(leafLinkedObjTail, 1, ntree, NRUTIL_LEAFPTR);
  free_uivector(nodeCount, 1, ntree);
  free_new_vvector(bootMembershipIndex, 1, ntree, NRUTIL_UPTR);
  if (RF_opt & OPT_BOOT_TYP2) {
  }
  free_new_vvector(bootMembershipFlag, 1, ntree, NRUTIL_CPTR);
  free_new_vvector(bootMembershipCount, 1, ntree, NRUTIL_UPTR);
  free_new_vvector(oobMembershipFlag, 1, ntree, NRUTIL_CPTR);
  free_new_vvector(ibgMembershipIndex, 1, ntree, NRUTIL_UPTR);
  free_new_vvector(oobMembershipIndex, 1, ntree, NRUTIL_UPTR);
  free_uivector(oobSize, 1, ntree);
  free_uivector(ibgSize, 1, ntree);
  free_uivector(maxDepth, 1, ntree);
  free_uivector(orderedTreeIndex, 1, ntree);
  free_uivector(serialTreeIndex, 1, ntree);
  free_new_vvector(root, 1, ntree, NRUTIL_NPTR);
  if (ptnCount > 0) {
    free_new_vvector(pNodeMembership, 1, ntree, NRUTIL_NPTR2);
    free_new_vvector(pTermMembership, 1, ntree, NRUTIL_NPTR2);
    free_new_vvector(pNodeList, 1, ntree, NRUTIL_NPTR2);
    free_new_vvector(pTermList, 1, ntree, NRUTIL_NPTR2);
    free_uivector(pLeafCount, 1, ntree);
  }
  if ((RF_opt & OPT_BOOT_TYP1) || (RF_opt & OPT_BOOT_TYP2)) {
    unstackWeights(subjWeightType, subjSize, subjWeightSorted); 
  }
  if ((startTimeIndex > 0) && (timeIndex > 0) && (statusIndex > 0)) {
  }
  else {
  }
  free_uivector(getTreeIndex, 1, ntree);
  free_uivector(identityMembershipIndex, 1, identityMembershipIndexSize);
}
char stackPreDefinedRestoreArrays(uint   xSize,
                                  uint  *intrPredictor,
                                  uint   intrPredictorSize,
                                  char **importanceFlag) {
  uint i;
  char result;
  result = TRUE;
  if (RF_opt & OPT_VIMP) {
    *importanceFlag = cvector(1, xSize);
    result = checkInteraction(xSize,
                              intrPredictor,
                              intrPredictorSize);
    if (result) {
      for (i = 1; i <= xSize; i++) {
        (*importanceFlag)[i] = FALSE;
      }
      for (i = 1; i <= intrPredictorSize; i++) {
        (*importanceFlag)[intrPredictor[i]] = TRUE;
      }
    }
  }
  return result;
}
void unstackPreDefinedRestoreArrays(uint  xSize, char *importanceFlag) {
  if (RF_opt & OPT_VIMP) {
    free_cvector(importanceFlag, 1, xSize);
  }
}
void stackPreDefinedPredictArrays(uint   ntree,
                                  uint   observationSize,
                                  uint  *identityMembershipIndexSize,
                                  uint **identityMembershipIndex,
                                  NodeBase      ****nodeMembership,
                                  TerminalBase  ****tTermMembership) {
  uint i;
  *nodeMembership = (NodeBase ***)      new_vvector(1, RF_ntree, NRUTIL_NPTR2);
  *tTermMembership = (TerminalBase ***) new_vvector(1, RF_ntree, NRUTIL_TPTR2);
  *identityMembershipIndex = uivector(1, observationSize);
  *identityMembershipIndexSize = observationSize;
  for (i = 1; i <= *identityMembershipIndexSize; i++) {
    (*identityMembershipIndex)[i] = i;
  }
}
void unstackPreDefinedPredictArrays(uint   ntree,
                                    uint   observationSize,
                                    uint  identityMembershipIndexSize,
                                    uint *identityMembershipIndex,
                                    NodeBase      ***nodeMembership,
                                    TerminalBase  ***tTermMembership) {
  free_new_vvector(nodeMembership, 1, RF_ntree, NRUTIL_NPTR2);
  free_new_vvector(tTermMembership, 1, RF_ntree, NRUTIL_TPTR2);
  free_uivector(identityMembershipIndex, 1, identityMembershipIndexSize);
}
char checkInteraction(uint  xSize,
                      uint *intrPredictor,
                      uint  intrPredictorSize) {
  uint leadingIndex, i;
  char result;
  result = TRUE;
  if((intrPredictorSize <= 0) || (intrPredictorSize > xSize)) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Parameter verification failed.");
    RF_nativeError("\nRF-SRC:  Number of predictors to be perturbed must be greater than zero and less than or equal to %10d:  %10d \n", xSize, intrPredictorSize);
    result = FALSE;
  }
  if (result) {
    uint *intrPredictorCopy = uivector(1, intrPredictorSize);
    for (i=1; i <= intrPredictorSize; i++) {
      intrPredictorCopy[i] = intrPredictor[i];
    }
    hpsortui(intrPredictorCopy, intrPredictorSize);
    leadingIndex = 1;
    for (i=2; i <= intrPredictorSize; i++) {
      if (intrPredictorCopy[i] > intrPredictorCopy[leadingIndex]) {
        leadingIndex++;
      }
    }
    free_uivector(intrPredictorCopy, 1, intrPredictorSize);
    if (intrPredictorSize != leadingIndex) {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  Parameter verification failed.");
      RF_nativeError("\nRF-SRC:  Interaction terms are not unique.");
      RF_nativeError("\nRF-SRC:  Only %10d of %10d are unique.", leadingIndex, intrPredictorSize);
      result = FALSE;
    }
    if (result) {
      for (i = 1; i <= intrPredictorSize; i++) {
        if (intrPredictor[i] > xSize) {
          RF_nativeError("\nRF-SRC:  *** ERROR *** ");
          RF_nativeError("\nRF-SRC:  Parameter verification failed.");
          RF_nativeError("\nRF-SRC:  Interaction terms are not coherent.");
          RF_nativeError("\nRF-SRC:  Predictor encountered is %10d, maximum allowable is %10d.", intrPredictor[i], xSize);
          result = FALSE;
        }
      }
    }
  }
  return result;
}
void stackWeights(double *weight,
                  uint    size,
                  uint   *weightType,
                  uint  **weightSorted,
                  uint   *weightDensitySize) {
  char uniformFlag, integerFlag;
  double meanWeight;
  uint i;
  *weightSorted      = NULL;
  *weightDensitySize = 0;
  meanWeight = getMeanValue(weight, size);
  uniformFlag = TRUE;
  i = 0;
  while (uniformFlag && (i < size)) {
    ++i;
    if (fabs(weight[i] - meanWeight) > 0) {
      uniformFlag = FALSE;
    }
  }
  if (uniformFlag) {
    *weightType = RF_WGHT_UNIFORM;
  } 
  else {
    integerFlag = TRUE;
    i = 0;
    while (integerFlag && (i < size)) {
      i++;
      if (fabs(round(weight[i]) - weight[i]) > 0.0) {
        integerFlag = FALSE;
      }
    }
    if(integerFlag) {
      *weightType = RF_WGHT_INTEGER;
    }
    else {
      *weightType = RF_WGHT_GENERIC;
    }
  }
  switch (*weightType) {
  case RF_WGHT_UNIFORM:
    break;
  case RF_WGHT_INTEGER:
    *weightSorted = uivector(1, size);
    indexx(size, weight, *weightSorted);
    *weightDensitySize = 0;
    for (i = 1; i <= size; i++) {
      (*weightDensitySize) += (uint) weight[i];
    }
    break;
  case RF_WGHT_GENERIC:
    *weightSorted = uivector(1, size);
    indexx(size, weight, *weightSorted);
    break;
  }
}
void unstackWeights(uint    weightType,
                    uint    size,
                    uint   *weightSorted) {
  switch (weightType) {
  case RF_WGHT_UNIFORM:
    break;
  case RF_WGHT_INTEGER:
    free_uivector(weightSorted, 1, size);
    break;
  case RF_WGHT_GENERIC:
    free_uivector(weightSorted, 1, size);
    break;
  }
}
double getMeanValue(double *value, uint size) {
  double result;
  uint j;
  result = 0.0;
  for (j = 1; j <= size; j++) {
    result = result + value[j];
  }
  result = result / size;
  return result;
}
char stackAndInitializeTimeAndSubjectArrays(char     mode,
                                            uint     startTimeIndex,
                                            uint     observationSize,
                                            double **responseIn,
                                            uint     timeIndex,
                                            uint     timeInterestSize,
                                            uint    *subjIn,
                                            uint    *subjSize,
                                            double **masterTime,
                                            uint   **masterTimeIndexIn,
                                            uint   **startMasterTimeIndexIn,
                                            double **timeInterest,
                                            uint    *masterTimeSize,
                                            uint    *sortedTimeInterestSize,
                                            uint   **masterToInterestTimeMap,
                                            uint   **subjSlot,
                                            uint   **subjSlotCount,
                                            uint  ***subjList,
                                            uint   **caseMap,
                                            uint   **subjMap,
                                            uint    *subjCount) {
  uint i, j;
  uint leadingIndex;
  uint adjObsSize;
  char result;
  result = TRUE;
  *subjList = NULL;
  if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
    if (!(RF_opt & OPT_ANON)) {
      if (startTimeIndex == 0) {
        *masterTime  = dvector(1, observationSize);
        *masterTimeIndexIn  = uivector(1, observationSize);
        *masterTimeSize = 0;
        for (j = 1; j <= observationSize; j++) {
          if (!RF_nativeIsNaN(responseIn[timeIndex][j])) {
            (*masterTimeSize) ++;
            (*masterTime)[*masterTimeSize] = responseIn[timeIndex][j];
          }
        }
        adjObsSize = observationSize;
      }
      else {
        RF_opt                  = RF_opt & (~OPT_PERF);
        RF_opt                  = RF_opt & (~OPT_VIMP);
        *masterTime  = dvector(1, 2 * observationSize);
        *startMasterTimeIndexIn = uivector(1, observationSize);
        *masterTimeIndexIn      = uivector(1, observationSize);
        *masterTimeSize = 0;
        for (j = 1; j <= observationSize; j++) {
          if (!RF_nativeIsNaN(responseIn[startTimeIndex][j])) {
            (*masterTimeSize) ++;
            (*masterTime)[*masterTimeSize] = responseIn[startTimeIndex][j];
          }
          if (!RF_nativeIsNaN(responseIn[timeIndex][j])) {
            (*masterTimeSize) ++;
            (*masterTime)[*masterTimeSize] = responseIn[timeIndex][j];
          }
        }
        adjObsSize = 2 * observationSize;      
      }
      qksort(*masterTime, *masterTimeSize);
      leadingIndex = 1;
      for (i=2; i <= *masterTimeSize; i++) {
        if ((*masterTime)[i] > (*masterTime)[leadingIndex]) {
          leadingIndex++;
          (*masterTime)[leadingIndex] = (*masterTime)[i];
        }
      }
      *masterTimeSize = leadingIndex;
      for (i= (*masterTimeSize) + 1; i <= adjObsSize; i++) {
        (*masterTime)[i] = 0;
      }
      if (startTimeIndex > 0) {
        *masterToInterestTimeMap = uivector(1, *masterTimeSize);
        *subjSlot = uivector(1, observationSize);
        *subjSlotCount = uivector(1, observationSize);
        *caseMap = uivector(1, observationSize);
        double *copySubjIn = dvector(1, observationSize);
        uint   *sortedIdx = uivector(1, observationSize);
        for (i = 1; i <= observationSize; i++) {
          (*subjSlotCount)[i] = 0;
          copySubjIn[i] = (double) subjIn[i];
        }
        indexx(observationSize, copySubjIn, sortedIdx);
        *subjCount = 1;
        (*subjSlotCount)[1] = 1;      
        (*subjSlot)[1] = subjIn[sortedIdx[1]]; 
        (*caseMap)[sortedIdx[1]] = 1;
        for (i = 2; i <= observationSize; i++) {
          if (subjIn[sortedIdx[i]] > (*subjSlot)[*subjCount]) {
            (*subjCount) ++;
            (*subjSlot)[*subjCount] = subjIn[sortedIdx[i]];
          }
          (*subjSlotCount)[*subjCount] ++;
          (*caseMap)[sortedIdx[i]] = *subjCount;
        }
        for (i = (*subjCount) + 1; i <= observationSize; i++) {
          (*subjSlot)[i] = 0;
        }
        *subjMap = uivector(1, (*subjSlot)[*subjCount]);
        for (i = 1; i <= (*subjSlot)[*subjCount]; i++) {
          (*subjMap)[i] = 0;
        }
        for (i = 1; i <= *subjCount; i++) {
          (*subjMap)[(*subjSlot)[i]] = i;
        }
        if (*subjSize == 0) {
          *subjSize = *subjCount;
        }
        if (*subjCount != *subjSize) {
          RF_nativeError("\nRF-SRC: *** ERROR *** ");
          RF_nativeError("\nRF-SRC: Subject count found in cases inconsistent with incoming subject size:  %10d vs %10d", *subjCount, subjSize);
          result = FALSE;
        }
        else {
          *subjList = (uint **) new_vvector(1, *subjCount, NRUTIL_UPTR);
          uint *tempSubjIter = uivector(1, *subjCount);
          for (i = 1; i <= *subjCount; i++) {
            (*subjList)[i] = uivector(1, (*subjSlotCount)[i]);
            tempSubjIter[i] = 0;
          }
          for (i = 1; i <= observationSize; i++) {
            (*subjList)[(*caseMap)[i]][++tempSubjIter[(*caseMap)[i]]] = i;
          }
          free_uivector(tempSubjIter, 1, *subjCount);
        }
        free_uivector(sortedIdx, 1, observationSize);
        free_dvector(copySubjIn, 1, observationSize);
      }
    }
    if (!(RF_opt & OPT_IMPU_ONLY)) {
      qksort(*timeInterest, timeInterestSize);
      *sortedTimeInterestSize = 1;
      for (i=2; i <= timeInterestSize; i++) {
        if ((*timeInterest)[i] > (*timeInterest)[*sortedTimeInterestSize]) {
          (*sortedTimeInterestSize) ++;
          (*timeInterest)[*sortedTimeInterestSize] = (*timeInterest)[i];
        }
      }
      if (*sortedTimeInterestSize != timeInterestSize) {
        RF_nativePrint("\nRF-SRC:  *** WARNING *** ");
        RF_nativePrint("\nRF-SRC:  Time points of interest are not unique.");
        RF_nativePrint("\nRF-SRC:  Any ensemble matricies will be");
        RF_nativePrint("\nRF-SRC:  resized as [N'] x [n], where N' is the");
        RF_nativePrint("\nRF-SRC:  unique time points of interest and n is");
        RF_nativePrint("\nRF-SRC:  number of observations in the data.");
      }
      for (i = (*sortedTimeInterestSize) + 1; i <= timeInterestSize; i++) {
        (*timeInterest)[i] = 0;
      }
      if (startTimeIndex > 0) {
        i = j = 1;
        while (i <= *masterTimeSize) {
          while (((*timeInterest)[j] >= (*masterTime)[i]) && (i <= *masterTimeSize)) {
            (*masterToInterestTimeMap)[i] = j;
            i++;
          }
          if (j < *sortedTimeInterestSize) {
            j++;
          }
          else {
            if (i <= *masterTimeSize) {
              (*masterToInterestTimeMap)[i] = timeInterestSize;
              i++;
            }
          }
        }
      }
    }  
  }
  return result;
}
void unstackTimeAndSubjectArrays(char     mode,
                                 uint     startTimeIndex,
                                 uint     observationSize,
                                 double  *masterTime,
                                 uint    *masterTimeIndexIn,
                                 uint     masterTimeSize,
                                 uint    *startMasterTimeIndexIn,
                                 uint    *masterToInterestTimeMap,
                                 uint    *subjSlot,
                                 uint    *subjSlotCount,
                                 uint   **subjList,
                                 uint    *caseMap,
                                 uint    *subjMap,
                                 uint     subjCount) {
  uint i;
  if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
    if (!(RF_opt & OPT_ANON)) {
      if (startTimeIndex == 0) {
        free_dvector(masterTime, 1, observationSize);
        free_uivector(masterTimeIndexIn, 1, observationSize);
      }
      else {
        free_dvector(masterTime, 1, 2 * observationSize);
        free_uivector(startMasterTimeIndexIn, 1, observationSize);
        free_uivector(masterTimeIndexIn, 1, observationSize);
      }
    }
    if (startTimeIndex > 0) {
      free_uivector(subjMap, 1, subjSlot[subjCount]);
      free_uivector(subjSlot, 1, observationSize);
      free_uivector(caseMap, 1, observationSize);
      if (subjList != NULL) {
        for (i = 1; i <= subjCount; i++) {
          free_uivector(subjList[i], 1, subjSlotCount[i]);
        }
        free_uivector(subjSlotCount, 1, observationSize);
        free_new_vvector(subjList, 1, subjCount, NRUTIL_UPTR);
      }
    }
    if (!(RF_opt & OPT_IMPU_ONLY)) {
      if (startTimeIndex > 0) {    
        free_uivector(masterToInterestTimeMap, 1, masterTimeSize);    
      }
    }
  }
}
void stackFactorArrays(char    mode,
                       char   *rType,
                       char   *xType,
                       uint    ySize,
                       uint    xSize,
                       uint   *xLevelsCnt,
                       uint   *rTarget,
                       uint    rTargetCount,
                       uint    timeIndex,
                       uint    statusIndex,
                       uint   *rFactorCount,
                       uint   *xFactorCount,
                       uint  **rFactorMap,
                       uint  **xFactorMap,
                       uint  **rFactorIndex,
                       uint  **xFactorIndex,
                       uint  **rFactorSize,
                       uint  **xFactorSize,
                       uint   *rNonFactorCount,
                       uint   *xNonFactorCount,
                       uint  **rNonFactorMap,
                       uint  **xNonFactorMap,
                       uint  **rNonFactorIndex,
                       uint  **xNonFactorIndex,
                       uint  **rTargetFactor,
                       uint  **rTargetNonFactor,
                       uint   *rTargetFactorCount,
                       uint   *rTargetNonFactorCount,
                       uint ***xLevels) {
  uint i, k;
  stackFactorGeneric(TRUE,
                     ySize,
                     rType,
                     rFactorMap,
                     rFactorCount,
                     rFactorIndex,
                     rFactorSize,
                     rNonFactorMap,
                     rNonFactorCount,
                     rNonFactorIndex);
  stackFactorGeneric(FALSE,
                     xSize,
                     xType,
                     xFactorMap,
                     xFactorCount,
                     xFactorIndex,
                     xFactorSize,
                     xNonFactorMap,
                     xNonFactorCount,
                     xNonFactorIndex);
  if (*xFactorCount > 0) {
    
    *xLevels = (uint **) new_vvector(1, *xFactorCount, NRUTIL_UPTR);  
    for (k = 1; k <= *xFactorCount; k++) {
      if (xLevelsCnt[k] > 0) {
        (*xLevels)[k] = (uint *) INTEGER(VECTOR_ELT(RF_xLevelsSEXP, k-1));
        (*xLevels)[k] --;
      }
      else {
        RF_nativeError("\nRF-SRC: *** ERROR *** ");
        RF_nativeError("\nRF-SRC: Inconsistent zero-level count in factor:  compressed-index = %10d, x-index = %10d", k, (*xFactorIndex)[k]);
        RF_nativeError("\nRF-SRC: Please Contact Technical Support.");
        RF_nativeExit();
      }
    }
    
         
  }
  if (ySize == 0) {
  }
  else {
    if ((timeIndex > 0) && (statusIndex > 0)) {
    }
    else {
      if (mode == RF_GROW) {
      }
      else {
      }
      *rTargetFactor    = uivector(1, rTargetCount);
      *rTargetNonFactor = uivector(1, rTargetCount);
      *rTargetFactorCount = *rTargetNonFactorCount = 0;
      for (i = 1; i <= rTargetCount; i++) {
        if ((rTarget[i] < 1) || (rTarget[i] > ySize)) {
          RF_nativeError("\nRF-SRC:  *** ERROR *** ");
          RF_nativeError("\nRF-SRC:  Target response is out of range for [C+], [R+], [M+]:  %10d %10d ", i, rTarget[i]);
          RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
          RF_nativeExit();
        }
        if ((rType[rTarget[i]] == 'B') ||
            (rType[rTarget[i]] == 'I') ||
            (rType[rTarget[i]] == 'C')) {
          (*rTargetFactor)[++(*rTargetFactorCount)] = rTarget[i];
        }
        else {
          (*rTargetNonFactor)[++(*rTargetNonFactorCount)] = rTarget[i];
        }
      }
    }  
  }  
}
void stackFactorGeneric(char    respFlag,
                        uint    size,
                        char   *type,
                        uint  **factorMap,
                        uint   *factorCount,
                        uint  **factorIndex,
                        uint  **factorSize,
                        uint  **nonfactorMap,
                        uint   *nonfactorCount,
                        uint  **nonfactorIndex) {
  uint i, j;
  if (size > 0) {
    *factorMap    = uivector(1, size);
    *nonfactorMap = uivector(1, size);
    *factorCount    = 0;
    *nonfactorCount = 0;
    for (i = 1; i <= size; i++) {
      (*factorMap)[i]    = 0;
      (*nonfactorMap)[i] = 0;
      if ((type[i] == 'B') ||
          ((type[i] == 'I') && respFlag) ||
          (type[i] == 'C')) {
        (*factorCount) ++;
        (*factorMap)[i] = *factorCount;
      }
      else {
        (*nonfactorCount) ++;
        (*nonfactorMap)[i] = *nonfactorCount;
      }
    }
    if (*factorCount > 0) {
      *factorIndex = uivector(1, *factorCount);
      j = 0;
      for (i = 1; i <= size; i++) {
        if ((*factorMap)[i] > 0) {
          (*factorIndex)[++j] = i;
        }
      }
      *factorSize = uivector(1, *factorCount);
    }
    if (*nonfactorCount > 0) {
      *nonfactorIndex = uivector(1, *nonfactorCount);
      j = 0;
      for (i = 1; i <= size; i++) {
        if ((*nonfactorMap)[i] > 0) {
          (*nonfactorIndex)[++j] = i;
        }
      }
    }
  }
  else {
    *factorCount    = 0;
    *nonfactorCount = 0;
  }
}
void unstackFactorArrays(char     mode,
                         uint     ntree,
                         uint     ySize,
                         uint     xSize,
                         uint    *rTarget,
                         uint     rTargetCount,
                         uint    *rTargetFactor,
                         uint    *rTargetNonFactor,
                         uint     timeIndex,
                         uint     statusIndex,
                         uint     rFactorCount,
                         uint     xFactorCount,
                         uint    *rFactorMap,
                         uint    *xFactorMap,
                         uint    *rFactorIndex,
                         uint    *xFactorIndex,
                         uint    *rFactorSize,
                         uint    *xFactorSize,
                         uint     rNonFactorCount,
                         uint     xNonFactorCount,
                         uint    *rNonFactorMap,
                         uint    *xNonFactorMap,
                         uint    *rNonFactorIndex,
                         uint    *xNonFactorIndex,
                         uint    **xLevels,
                         Factor ***factorList) {
  if (ySize > 0) {
    free_uivector(rFactorMap, 1, ySize);
    if (rFactorCount > 0) {
      free_uivector(rFactorIndex, 1, rFactorCount);
      free_uivector(rFactorSize, 1, rFactorCount);
    }
    free_uivector(rNonFactorMap, 1, ySize);
    if (rNonFactorCount > 0) {
      free_uivector(rNonFactorIndex, 1, rNonFactorCount);
    }
  }
  free_uivector(xFactorMap, 1, xSize);
  if (xFactorCount > 0) {
    free_uivector(xFactorIndex, 1, xFactorCount);
    free_uivector(xFactorSize, 1, xFactorCount);
    free_new_vvector(xLevels, 1, xFactorCount, NRUTIL_UPTR);  
  }
  free_uivector(xNonFactorMap, 1, xSize);
  if (xNonFactorCount > 0) {
    free_uivector(xNonFactorIndex, 1, xNonFactorCount);
  }
  if ((rFactorCount + xFactorCount) > 0) {
    if (ntree > 0) {
      free_new_vvector(factorList, 1, ntree, NRUTIL_FPTR2);
    }
  }
  if (ySize == 0) {
  }
  else {
    if ((timeIndex > 0) && (statusIndex > 0)) {
    }
    else {
      free_uivector(rTargetFactor, 1, rTargetCount);
      free_uivector(rTargetNonFactor, 1, rTargetCount);
    }
  }
}
void initializeFactorArrays(char  mode,
                            uint  rFactorCount,
                            uint  xFactorCount,
                            uint *rFactorIndex,
                            uint *xFactorIndex,
                            uint *rLevelsMax,
                            uint *xLevelsMax,
                            uint *rLevelsCnt,
                            uint *xLevelsCnt,
                            uint  ntree,
                            uint  *rFactorSize,
                            uint  *xFactorSize,
                            uint  *rMaxFactorLevel,
                            uint  *xMaxFactorLevel,
                            uint  *maxFactorLevel,
                            Factor ****factorList) {
  uint j;
  if (rFactorCount + xFactorCount > 0) {
    *rMaxFactorLevel = 0;
    for (j = 1; j <= rFactorCount; j++) {
      rFactorSize[j] = rLevelsMax[rFactorIndex[j]];
      if (*rMaxFactorLevel < rFactorSize[j]) {
        *rMaxFactorLevel = rFactorSize[j];
      }
    }
    *xMaxFactorLevel = 0;
    for (j = 1; j <= xFactorCount; j++) {
      xFactorSize[j] = xLevelsMax[xFactorIndex[j]];
      if (*xMaxFactorLevel < xFactorSize[j]) {
        *xMaxFactorLevel = xFactorSize[j];
      }
    }
    *maxFactorLevel = (*xMaxFactorLevel > *rMaxFactorLevel) ? *xMaxFactorLevel : *rMaxFactorLevel;
    if (ntree > 0 ) {
      *factorList = (Factor ***) new_vvector(1, ntree, NRUTIL_FPTR2);
      for (j = 1; j <= ntree; j++) {
        (*factorList)[j] = NULL;
      }
    }
  }
}
char stackCompetingArrays(char     mode,
                          uint     statusIndex,
                          uint     splitRule,
                          uint     eventTypeSize,
                          uint    *eventType,
                          uint     crWeightSize,
                          double  *crWeight,
                          uint     frSize,
                          uint     observationSize,
                          uint     fobservationSize,
                          double **responseIn,
                          double **fresponseIn,
                          uint    *dmRecordMap,
                          uint    *fmRecordMap,
                          uint     dmRecordSize,
                          uint     fmRecordSize,
                          int    **dmpSign,
                          int    **fmpSign,
                          uint   **eventTypeIndex,
                          uint    *feventTypeSize,
                          uint    *mStatusSize,
                          uint   **eIndividualSize,
                          uint  ***eIndividualIn) {
  uint obsSize;
  double  *statusPtr;
  uint    *mRecordMap;
  int    **mpSign;
  uint     mRecordSize;
  char eventSubsetFlag;
  char statusFlag;
  uint *eventCounter;
  uint i, j;
  if (statusIndex == 0) {
    RF_nativeError("\nRF-SRC:  *** ERROR *** ");
    RF_nativeError("\nRF-SRC:  Attempt to stack competing risk structures in the absence of SURV data.");
    RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
    RF_nativeExit();
  }
  switch (mode) {
  case RF_GROW:
    if ((splitRule == SURV_CR_LAU) || (splitRule == SURV_CR_GEN)) {
      if (eventTypeSize > 1) {
        RF_opt = RF_opt | OPT_COMP_RISK;
      }
      else {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Parameter verification failed.");
        RF_nativeError("\nRF-SRC:  Competing Risk analysis has been requested.");
        RF_nativeError("\nRF-SRC:  The train data set does not contain competing risks.");
        RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
        RF_nativeExit();
      }
    }
    else {
      if (splitRule == CUST_SPLIT) {
        if (eventTypeSize > 1) {
          RF_opt = RF_opt | OPT_COMP_RISK;
        }
        else {
          RF_opt = RF_opt & (~OPT_COMP_RISK);
        }
      }
      else {
        RF_opt = RF_opt & (~OPT_COMP_RISK);
      }
    }
    break;
  default:
    break;
  }
  if (eventTypeSize == 0) {
    if ((RF_opt & OPT_OUTC_TYPE) && !(RF_opt & OPT_PERF) && !(RF_opt & OPT_VIMP)) {
      RF_opt                  = RF_opt & (~OPT_OENS);
      RF_opt                  = RF_opt & (~OPT_IENS);
    }
    else {
      RF_nativeError("\nRF-SRC:  *** ERROR *** ");
      RF_nativeError("\nRF-SRC:  Parameter verification failed.");
      RF_nativeError("\nRF-SRC:  Performance or vimp has been requested.");
      RF_nativeError("\nRF-SRC:  The train or pseudo-train data set does not contain any events.");
      RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
      RF_nativeExit();
    }
  }
  else {
    hpsortui(eventType, eventTypeSize);
    *eventTypeIndex  = uivector(1, eventType[eventTypeSize]);
    for (j = 1; j <= eventType[eventTypeSize]; j++) {
      (*eventTypeIndex)[j] = 0;
    }
    for (j = 1; j <= eventTypeSize; j++) {
      (*eventTypeIndex)[eventType[j]] = j;
    }
  }
  switch (mode) {
  case RF_GROW:
    if (splitRule == RAND_SPLIT) {
      if (eventTypeSize == 1) {
      }
      else {
        RF_opt = RF_opt | OPT_COMP_RISK;
      }
    }
    if (splitRule == SURV_CR_LAU) {
      if (eventTypeSize == 1) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Split rule specified is for Competing Risk scenarios only.");
        RF_nativeError("\nRF-SRC:  The data set does not contain multiple events.");
        RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
        RF_nativeExit();
      }
      if(crWeightSize != eventTypeSize) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Parameter verification failed.");
        RF_nativeError("\nRF-SRC:  Competing risk weight vector must be of size equal to number of event types:  %12d != %12d \n", crWeightSize, eventTypeSize);
        RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
        RF_nativeExit();
      }
      i = 0;
      for (j = 1; j <= eventTypeSize; j++) {
        if(crWeight[j] < 0) {
          RF_nativeError("\nRF-SRC:  *** ERROR *** ");
          RF_nativeError("\nRF-SRC:  Parameter verification failed.");
          RF_nativeError("\nRF-SRC:  Competing risk weight elements must be greater than or equal to zero:  %12.4f \n", crWeight[j]);
          RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
          RF_nativeExit();
        }
        else {
          if(crWeight[j] == 0) {
            i ++;
          }
        }
      }
      if (i == eventTypeSize) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Parameter verification failed.");
        RF_nativeError("\nRF-SRC:  Competing risk weight elements are all zero. \n");
        RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
        RF_nativeExit();
      }
    }
    break;
  default:
    if (RF_opt & OPT_COMP_RISK) {
      if (eventTypeSize == 1) {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  CR analysis has been specified in !GROW mode.");
        RF_nativeError("\nRF-SRC:  However, the GROW data set does not contain multiple events.");
        RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
        RF_nativeExit();
      }
    }
    break;
  }
  switch (mode) {
  case RF_PRED:
    if (frSize > 0) {
      getEventInfo(mode,
                   statusIndex,
                   observationSize,
                   fobservationSize,
                   responseIn,
                   fresponseIn,
                   eventTypeSize,
                   eventType,
                   dmRecordMap,
                   fmRecordMap,
                   dmRecordSize,
                   fmRecordSize,
                   dmpSign,
                   fmpSign,
                   mStatusSize,
                   feventTypeSize);
    }
    else {
      *feventTypeSize = *mStatusSize = 0;
    }
    break;
  default:
    getEventInfo(mode,
                 statusIndex,
                 observationSize,
                 fobservationSize,
                 responseIn,
                 fresponseIn,
                 eventTypeSize,
                 eventType,
                 dmRecordMap,
                 fmRecordMap,
                 dmRecordSize,
                 fmRecordSize,
                 dmpSign,
                 fmpSign,
                 mStatusSize,
                 feventTypeSize);
    break;
  } 
  if (eventTypeSize > 1) {
    if (mode == RF_PRED) {
      if (*feventTypeSize > 0) {
        eventSubsetFlag = TRUE;
      }
      else {
        eventSubsetFlag = FALSE;
      }
    }
    else if (mode == RF_REST) {
      if (!(RF_opt & OPT_ANON)) {
        eventSubsetFlag = TRUE;
      }
      else {
        eventSubsetFlag = FALSE;
      }
    }
    else {
      eventSubsetFlag = TRUE;        
    }
  }
  else {
    eventSubsetFlag = FALSE;
  }
  if (eventSubsetFlag == TRUE) {
    switch (mode) {
    case RF_PRED:
      obsSize    = fobservationSize;
      statusPtr  = fresponseIn[statusIndex];
      mpSign     = fmpSign;
      mRecordMap = fmRecordMap;
      mRecordSize = fmRecordSize;
      break;
    default:
      obsSize    = observationSize;
      statusPtr  = responseIn[statusIndex];
      mpSign     = dmpSign;
      mRecordMap = dmRecordMap;
      mRecordSize = dmRecordSize;
      break;
    }
    *eIndividualSize = uivector(1, eventTypeSize);
    for (j = 1; j <= eventTypeSize; j++) {
      (*eIndividualSize)[j] = 0;
    }
    if (mRecordSize > 0) {
      for (i = 1; i <= obsSize; i++) {
        statusFlag = FALSE;
        if (mRecordMap[i] == 0) {
          statusFlag = TRUE;
        }
        else {
          if (mpSign[statusIndex][mRecordMap[i]] == 0) {
            statusFlag = TRUE;
          }
        }
        if (statusFlag == TRUE) {
          if ((uint) statusPtr[i] > 0) {
            (*eIndividualSize)[(*eventTypeIndex)[(uint) statusPtr[i]]] ++;
          }
          else {
            for (j=1; j <= eventTypeSize; j++) {
              (*eIndividualSize)[j] ++;
            }
          }
        }
      } 
    }
    else {
      for (i = 1; i <= obsSize; i++) {
        if ((uint) statusPtr[i] > 0) {
          (*eIndividualSize)[(*eventTypeIndex)[(uint) statusPtr[i]]] ++;
        }
        else {
          for (j=1; j <= eventTypeSize; j++) {
            (*eIndividualSize)[j] ++;
          }
        }
      } 
    }
    *eIndividualIn = (uint **) new_vvector(1, eventTypeSize, NRUTIL_UPTR);
    for (j = 1; j <= eventTypeSize; j++) {
      (*eIndividualIn)[j] = uivector(1, (*eIndividualSize)[j] + *mStatusSize + 1);
    }
    eventCounter = uivector(1, eventTypeSize);
    for (j = 1; j <= eventTypeSize; j++) {
      eventCounter[j] = 0;
    }
    if (mRecordSize > 0) {    
      for (i = 1; i <= obsSize; i++) {
        statusFlag = FALSE;
        if (mRecordMap[i] == 0) {
          statusFlag = TRUE;
        }
        else {
          if (mpSign[statusIndex][mRecordMap[i]] == 0) {
            statusFlag = TRUE;
          }
        }
        if (statusFlag == TRUE) {
          if ((uint) statusPtr[i] > 0) {
            j = (*eventTypeIndex)[(uint) statusPtr[i]];
            eventCounter[j] ++;
            (*eIndividualIn)[j][eventCounter[j]] = i;
          }
          else {
            for (j=1; j <= eventTypeSize; j++) {
              eventCounter[j] ++;
              (*eIndividualIn)[j][eventCounter[j]] = i;
            }
          }
        }
      }
    }
    else {
      for (i = 1; i <= obsSize; i++) {
        if ((uint) statusPtr[i] > 0) {
          j = (*eventTypeIndex)[(uint) statusPtr[i]];
          eventCounter[j] ++;
          (*eIndividualIn)[j][eventCounter[j]] = i;
        }
        else {
          for (j=1; j <= eventTypeSize; j++) {
            eventCounter[j] ++;
            (*eIndividualIn)[j][eventCounter[j]] = i;
          }
        }
      }
    }
    free_uivector(eventCounter, 1, eventTypeSize);
  }  
  return TRUE;
}
void getEventInfo(char     mode,
                  uint     statusIndex,
                  uint     observationSize,
                  uint     fobservationSize,
                  double **responseIn,
                  double **fresponseIn,
                  uint     eventTypeSize,
                  uint    *eventType,
                  uint    *dmRecordMap,
                  uint    *fmRecordMap,
                  uint     dmRecordSize,
                  uint     fmRecordSize,
                  int    **dmpSign,
                  int    **fmpSign,
                  uint    *mStatusSize,
                  uint    *feventTypeSize) {
  uint    obsSize;
  double *status;
  uint   *mRecordMap;
  int   **mpSign;
  uint    mRecordSize;
  uint statusFlag;
  uint leadingIndex;
  uint i, j;
  uint jgrow;
  if (statusIndex == 0) {
    RF_nativeError("\nRF-SRC: *** ERROR *** ");
    RF_nativeError("\nRF-SRC: Attempt to stack competing risk structures in the absence of SURV data.");
    RF_nativeError("\nRF-SRC: Please Contact Technical Support.");
    RF_nativeExit();
  }
  if (mode == RF_PRED) {
    obsSize    = fobservationSize;
    status     = fresponseIn[statusIndex];
    mRecordMap = fmRecordMap;
    mpSign     = fmpSign;
    mRecordSize = fmRecordSize;
  }
  else {
    obsSize    = observationSize;
    status     = responseIn[statusIndex];
    mRecordMap = dmRecordMap;
    mpSign     = dmpSign;
    mRecordSize = dmRecordSize;    
  }
  *mStatusSize = 0;
  uint *eventTypeLocal = uivector(1, obsSize);
  uint eventTypeSizeLocal = 0;
  if (mRecordSize > 0) {
    for (i = 1; i <= obsSize; i++) {
      eventTypeLocal[i] = 0;
      statusFlag = FALSE;
      if (mRecordMap[i] == 0) {
        statusFlag = TRUE;
      }
      else {
        if (mpSign[statusIndex][mRecordMap[i]] == 0) {
          statusFlag = TRUE;
        }
      }
      if (statusFlag == TRUE) {
        if ((uint) status[i] > 0) {
          eventTypeSizeLocal ++;
          eventTypeLocal[eventTypeSizeLocal] = (uint) status[i];
        } 
        else {
        }
      }
      else {
        (*mStatusSize) ++;
      }
    }  
  }
  else {
    for (i = 1; i <= obsSize; i++) {
      eventTypeLocal[i] = 0;
      if ((uint) status[i] > 0) {
        eventTypeSizeLocal ++;
        eventTypeLocal[eventTypeSizeLocal] = (uint) status[i];
      } 
      else {
      }
    }
  }
  if (mode == RF_PRED) {
    if(eventTypeSizeLocal > 0) {
      hpsortui(eventTypeLocal, eventTypeSizeLocal);
      leadingIndex = 1;
      for (i = 2; i <= eventTypeSizeLocal; i++) {
        if (eventTypeLocal[i] > eventTypeLocal[leadingIndex]) {
          leadingIndex++;
          eventTypeLocal[leadingIndex] = eventTypeLocal[i];
        }
      }
      eventTypeSizeLocal = leadingIndex;
    }
    if (eventTypeSizeLocal > 0) {
      *feventTypeSize = eventTypeSizeLocal;
    }
    else {
      *feventTypeSize = 0;
    }
    if (*feventTypeSize == 0) {
      if (!(RF_opt & OPT_PERF) && !(RF_opt & OPT_VIMP)) {
      }
      else {
        RF_nativeError("\nRF-SRC:  *** ERROR *** ");
        RF_nativeError("\nRF-SRC:  Parameter verification failed.");
        RF_nativeError("\nRF-SRC:  Performance or vimp has been requested.");
        RF_nativeError("\nRF-SRC:  The test or pseudo-train data set does not contain any events.");
        RF_nativeError("\nRF-SRC:  Please Contact Technical Support.");
        RF_nativeExit();
      }
    }
    else {
      char consistencyFlag = TRUE;
      if (eventTypeSize > 1) {
        for (j = 1; j <= *feventTypeSize; j++) {
          for (jgrow = 1; jgrow <= eventTypeSize; jgrow++) {
            if (eventTypeLocal[j] != eventType[jgrow]) {
              if (jgrow == eventTypeSize) {
                consistencyFlag = FALSE;
              }
            }
            else {
              jgrow = eventTypeSize;
            }
          }
        }
      }
      if (consistencyFlag == FALSE) {
        RF_nativeError("\nRF-SRC: *** ERROR *** ");
        RF_nativeError("\nRF-SRC: Unknown event type encountered in PRED mode. ");
        RF_nativeError("\nRF-SRC: Please Contact Technical Support.");
        RF_nativeExit();
      }
    }
  }
  free_uivector(eventTypeLocal, 1, obsSize);
}
void unstackCompetingArrays(char   mode,
                            uint   statusIndex,
                            uint   eventTypeSize,
                            uint  *eventType,
                            uint   feventTypeSize,
                            uint  *eventTypeIndex,
                            uint   mStatusSize,
                            uint  *eIndividualSize,
                            uint **eIndividualIn) {
  char eventSubsetFlag;
  uint j;
    if (statusIndex == 0) {
      RF_nativeError("\nRF-SRC: *** ERROR *** ");
      RF_nativeError("\nRF-SRC: Attempt to unstack competing risk structures in the absence of SURV data.");
      RF_nativeError("\nRF-SRC: Please Contact Technical Support.");
      RF_nativeExit();
    }
    if (eventTypeSize == 0) {
    }
    else {
      free_uivector(eventTypeIndex, 1, eventType[eventTypeSize]);
    }
    if (eventTypeSize > 1) {
      if (mode == RF_PRED) {
        if (feventTypeSize > 0) {
          eventSubsetFlag = TRUE;
        }
        else {
          eventSubsetFlag = FALSE;
        }
      }
      else if (mode == RF_REST) {
        if (!(RF_opt & OPT_ANON)) {
          eventSubsetFlag = TRUE;
        }
        else {
          eventSubsetFlag = FALSE;
        }
      }
      else {
        eventSubsetFlag = TRUE;        
      }
    }
    else {
      eventSubsetFlag = FALSE;
    }
    if (eventSubsetFlag == TRUE) {
      for (j = 1; j <= eventTypeSize; j++) {
        free_uivector(eIndividualIn[j], 1, eIndividualSize[j] + mStatusSize + 1);
      }
      free_new_vvector(eIndividualIn, 1, eventTypeSize, NRUTIL_UPTR);
      free_uivector(eIndividualSize, 1, eventTypeSize);
    }  
}
char stackClassificationArrays(char     mode,
                               uint    *rFactorSize,
                               uint    *rLevelsCnt,
                               uint     rFactorCount,
                               uint     observationSize,
                               double **responseIn,
                               uint    *rFactorIndex,
                               uint     frSize,
                               double **fresponseIn,
                               uint     fobservationSize,
                               uint  ***rLevels,
                               uint   **classLevelSize,
                               uint  ***classLevel,
                               uint  ***classLevelIndex,
                               double **rFactorThreshold,
                               uint   **rFactorMinority,
                               uint   **rFactorMajority,
                               char   **rFactorMinorityFlag) {
  uint  minorityClassID, minorityClassCnt;
  uint  majorityClassID, majorityClassCnt;
  uint i, j, k;
  if (rFactorCount == 0) {
    RF_nativeError("\nRF-SRC: *** ERROR *** ");
    RF_nativeError("\nRF-SRC: Attempt to stack classification structures in the absence of CLAS data.");
    RF_nativeError("\nRF-SRC: Please Contact Technical Support.");
    RF_nativeExit();
  }
  *classLevelSize = uivector(1, rFactorCount);
  *classLevel = (uint **) new_vvector(1, rFactorCount, NRUTIL_UPTR);
  *rFactorMinorityFlag = cvector(1, rFactorCount);
  
  *rLevels = (uint **) new_vvector(1, rFactorCount, NRUTIL_UPTR);  
  for (k = 1; k <= rFactorCount; k++) {
    if (rLevelsCnt[k] > 0) {
      (*classLevelSize)[k] = rLevelsCnt[k];
      (*rLevels)[k] = (uint *) INTEGER(VECTOR_ELT(RF_rLevelsSEXP, k-1));
      (*rLevels)[k] --;
      (*classLevel)[k] = (*rLevels)[k];
    }
    else {
      RF_nativeError("\nRF-SRC: *** ERROR *** ");
      RF_nativeError("\nRF-SRC: Inconsistent zero-level count in factor:  compressed-index = %10d, y-index = %10d", k, rFactorIndex[k]);
      RF_nativeError("\nRF-SRC: Please Contact Technical Support.");
      RF_nativeExit();
    }
  }
  
   
  *classLevelIndex = (uint **) new_vvector(1, rFactorCount, NRUTIL_UPTR);
  for (k = 1; k <= rFactorCount; k++) {
    (*rFactorMinorityFlag)[k] = FALSE;
    (*classLevelIndex)[k] = uivector(1, rFactorSize[k]);
    for (j = 1; j <= rFactorSize[k]; j++) {
      (*classLevelIndex)[k][j] = 0;
    }
    for (j = 1; j <= (*classLevelSize)[k]; j++) {
      (*classLevelIndex)[k][(*classLevel)[k][j]] = j;
    }
  }  
  if (RF_opt & OPT_PERF) {
    if (RF_opt & OPT_CLAS_RFQ) {
      *rFactorMinority = uivector(1, rFactorCount);
      *rFactorMajority = uivector(1, rFactorCount);
      *rFactorThreshold = dvector(1, rFactorCount);
      uint totalCount;
      for (j = 1; j <= rFactorCount; j++) {
        uint *levelCount = uivector(1, rFactorSize[j]);
        totalCount = 0;
        for (k = 1; k <= rFactorSize[j]; k++) {
          levelCount[k] = 0;
        }
        for (i = 1; i <= observationSize; i++) {
          if (!RF_nativeIsNaN(responseIn[rFactorIndex[j]][i])) {
            levelCount[(uint) responseIn[rFactorIndex[j]][i]] ++;
            totalCount ++;
          }
        }
        minorityClassCnt = levelCount[1];
        minorityClassID = 1;
        for (k = 1; k <= rFactorSize[j]; k++) {
          if (levelCount[k] < minorityClassCnt) {
            minorityClassCnt = levelCount[k];
            minorityClassID = k;
          }
        }
        (*rFactorMinority)[j] = minorityClassID;
        majorityClassCnt = levelCount[1];
        majorityClassID = 1;
        for (k = 1; k <= rFactorSize[j]; k++) {
          if (levelCount[k] >= majorityClassCnt) {
            majorityClassCnt = levelCount[k];
            majorityClassID = k;
          }
        }
        (*rFactorMajority)[j] = majorityClassID;
        (*rFactorThreshold)[j] = (double) levelCount[(*rFactorMinority)[j]] / totalCount;
        free_uivector(levelCount, 1, rFactorSize[j]);
      }
    }
    for (j = 1; j <= rFactorCount; j++) {
      if (rFactorSize[j] == 2) {
        (*rFactorMinorityFlag)[j] = TRUE;
      }
    }
  }
  if (mode == RF_PRED) {
    if (frSize > 0) {
      for (k = 1; k <= rFactorCount; k++) {
        for (i = 1; i <= fobservationSize; i++) {
          if (!RF_nativeIsNaN(fresponseIn[rFactorIndex[k]][i])) {
            if ((uint) fresponseIn[rFactorIndex[k]][i] > (*classLevelSize)[k]) {
              RF_nativeError("\nRF-SRC: *** ERROR *** ");
              RF_nativeError("\nRF-SRC: Inconsistent test response in factor:  compressed-index = %10d, y-index = %10d", k, rFactorIndex[k]);
              RF_nativeError("\nRF-SRC: Level countered versus class size:  test level = %10d, class size = %10d", (uint) fresponseIn[rFactorIndex[k]][i], (*classLevelSize)[k]);
              RF_nativeError("\nRF-SRC: Please Contact Technical Support.");
              RF_nativeExit();
            }
          }
        }
      }
    }
  }
  return TRUE;
}
void unstackClassificationArrays(char    mode,
                                 uint   *rFactorSize,
                                 uint    rFactorCount,
                                 uint  **rLevels,
                                 uint   *classLevelSize,
                                 uint  **classLevel,
                                 uint  **classLevelIndex,
                                 double *rFactorThreshold,
                                 uint   *rFactorMinority,
                                 uint   *rFactorMajority,
                                 char   *rFactorMinorityFlag) {
  uint k;
  if (rFactorCount == 0) {
    RF_nativeError("\nRF-SRC: *** ERROR *** ");
    RF_nativeError("\nRF-SRC: Attempt to unstack classification structures in the absence of CLAS data.");
    RF_nativeError("\nRF-SRC: Please Contact Technical Support.");
    RF_nativeExit();
  }
  for (k = 1; k <= rFactorCount; k++) {
    free_uivector(classLevelIndex[k], 1, rFactorSize[k]);
  }
  free_new_vvector(classLevelIndex, 1, rFactorCount, NRUTIL_UPTR);
  free_uivector(classLevelSize, 1, rFactorCount);
  free_new_vvector(classLevel, 1, rFactorCount, NRUTIL_UPTR);
  free_cvector(rFactorMinorityFlag, 1, rFactorCount);
  
  free_new_vvector(rLevels, 1, rFactorCount, NRUTIL_UPTR);  
  
  if (RF_opt & OPT_PERF) {
    if (RF_opt & OPT_CLAS_RFQ) {
      free_dvector(rFactorThreshold, 1, rFactorCount);
      free_uivector(rFactorMinority, 1, rFactorCount);
      free_uivector(rFactorMajority, 1, rFactorCount);
    }
  }
}
void stackFactorInSitu(uint  treeID,
                       uint  rFactorCount,
                       uint  xFactorCount,
                       uint  maxFactorLevel,
                       uint *rFactorSize,
                       uint *xFactorSize,
                       Factor ***factorList) {
  uint j;
  if (rFactorCount + xFactorCount > 0) {
    factorList[treeID] = (Factor **) new_vvector(1, maxFactorLevel, NRUTIL_FPTR);
    for (j = 1; j <= maxFactorLevel; j++) {
      factorList[treeID][j] = NULL;
    }
    for (j = 1; j <= xFactorCount; j++) {
      if (factorList[treeID][xFactorSize[j]] == NULL) {
        factorList[treeID][xFactorSize[j]] = makeFactor(xFactorSize[j], FALSE);
      }
    }
    for (j = 1; j <= rFactorCount; j++) {
      if (factorList[treeID][rFactorSize[j]] == NULL) {
        factorList[treeID][rFactorSize[j]] = makeFactor(rFactorSize[j], FALSE);
      }
    }
  }
}
void unstackFactorInSitu(uint  treeID,
                         uint  rFactorCount,
                         uint  xFactorCount,
                         uint  maxFactorLevel,
                         Factor ***factorList) {
  uint j;
  if (rFactorCount + xFactorCount > 0) {
    if (factorList[treeID] != NULL) {
      for (j = 1; j <= maxFactorLevel; j++) {
        if (factorList[treeID][j] != NULL) {
          freeFactor(factorList[treeID][j]);
        }
      }
      free_new_vvector(factorList[treeID], 1, maxFactorLevel, NRUTIL_FPTR);
      factorList[treeID] = NULL;
    }
  }
}
