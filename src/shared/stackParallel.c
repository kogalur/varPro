
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "stackParallel.h"
#include "nrutil.h"
#ifdef _OPENMP
void stackLocksOpenMP(char mode) {
  uint growSize, predSize;
  uint i;
  omp_init_lock(&RF_lockEnsbUpdtCount);
  omp_init_lock(&RF_lockPerf);
  if (RF_statusIndex > 0) {
    if (RF_startTimeIndex > 0) {
      growSize = RF_subjSize;
      predSize = RF_subjSize; 
    }
    else {
      growSize = RF_observationSize;
      predSize = RF_fobservationSize;
    }
  }
  else {
      growSize = RF_observationSize;
      predSize = RF_fobservationSize;
  }
  omp_lock_t   **lockDENptr;
  uint size;  
  char oobFlag, fullFlag;
  oobFlag = fullFlag = FALSE;
  switch (mode) {
  case RF_PRED:
    if (RF_opt & OPT_FENS) {
      fullFlag = TRUE;
    }
    break;
  default:
    if (RF_opt & OPT_OENS) {
      oobFlag = TRUE;
    }
    if (RF_opt & OPT_IENS) {
      fullFlag = TRUE;
    }
    break;
  }
  while ((oobFlag == TRUE) || (fullFlag == TRUE)) {
    if (oobFlag == TRUE) {
      lockDENptr = &RF_lockDENoens;
      size = growSize;
    }
    else {
      lockDENptr = &RF_lockDENfens;
      size = (mode == RF_PRED) ? predSize : growSize;
    }
    *lockDENptr = ompvector(1, size);
    for (i = 1; i <= size; i++) {
      omp_init_lock(&((*lockDENptr)[i]));
    }
    if (oobFlag == TRUE) {
      oobFlag = FALSE;
    }
    else {
      fullFlag = FALSE;
    }
  }
}
#else
void stackLocksOpenMP(char mode) { }
#endif
#ifdef _OPENMP
void unstackLocksOpenMP(char mode) {
  uint growSize, predSize;
  uint i;
  omp_destroy_lock(&RF_lockEnsbUpdtCount);
  omp_destroy_lock(&RF_lockPerf);
  if (RF_statusIndex > 0) {
    if (RF_startTimeIndex > 0) {
      growSize = RF_subjSize;
      predSize = RF_subjSize; 
    }
    else {
      growSize = RF_observationSize;
      predSize = RF_fobservationSize;
    }
  }
  else {
    growSize = RF_observationSize;
    predSize = RF_fobservationSize;
  }
  omp_lock_t   *lockDENptr;
  uint size;  
  char oobFlag, fullFlag;
  oobFlag = fullFlag = FALSE;
  switch (mode) {
  case RF_PRED:
    if (RF_opt & OPT_FENS) {
      fullFlag = TRUE;
    }
    break;
  default:
    if (RF_opt & OPT_OENS) {
      oobFlag = TRUE;
    }
    if (RF_opt & OPT_IENS) {
      fullFlag = TRUE;
    }
    break;
  }
  while ((oobFlag == TRUE) || (fullFlag == TRUE)) {
    if (oobFlag == TRUE) {
      lockDENptr = RF_lockDENoens;
      size = growSize;
    }
    else {
      lockDENptr = RF_lockDENfens;
      size = (mode == RF_PRED) ? predSize : growSize;
    }
    for (i = 1; i <= size; i++) {
      omp_destroy_lock(&(lockDENptr[i]));
    }
    free_ompvector(lockDENptr, 1, size);
    if (oobFlag == TRUE) {
      oobFlag = FALSE;
    }
    else {
      fullFlag = FALSE;
    }
  }
}
#else
void unstackLocksOpenMP(char mode) { }
#endif
#ifdef NOT_HAVE_PTHREAD
void stackLocksPosix(char mode) {
  uint i, j;
  pthread_mutex_init(&RF_lockEnsbUpdtCountPosix, NULL);
  pthread_mutex_init(&RF_lockPerfPosix, NULL);
  char  potentiallyMixedMultivariate = FALSE;
  if (RF_rTargetFactorCount > 0) {
    pthread_mutex_t   **lockDENptr;
    uint obsSize;  
    char oobFlag, fullFlag;
    oobFlag = fullFlag = FALSE;
    switch (mode) {
    case RF_PRED:
      if (RF_opt & OPT_FENS) {
        fullFlag = TRUE;
      }
      break;
    default:
      if (RF_opt & OPT_OENS) {
        oobFlag = TRUE;
      }
      if (RF_opt & OPT_IENS) {
        fullFlag = TRUE;
      }
      break;
    }
    while ((oobFlag == TRUE) || (fullFlag == TRUE)) {
      if (oobFlag == TRUE) {
        lockDENptr = &RF_lockDENoensPosix;
        obsSize = RF_observationSize;
      }
      else {
        lockDENptr = &RF_lockDENfensPosix;
        obsSize = (mode == RF_PRED) ? RF_fobservationSize : RF_observationSize;
      }
      if (!potentiallyMixedMultivariate) {
        *lockDENptr = ptmvector(1, obsSize);
        for (i = 1; i <= obsSize; i++) {
          pthread_mutex_init(&((*lockDENptr)[i]), NULL);
        }
      }
      if (oobFlag == TRUE) {
        oobFlag = FALSE;
      }
      else {
        fullFlag = FALSE;
      }
    }
    potentiallyMixedMultivariate = TRUE;
  }
  if (RF_rTargetNonFactorCount > 0) {
    pthread_mutex_t   **lockDENptr;
    pthread_mutex_t   **lockQNTptr;
    uint obsSize;  
    char oobFlag, fullFlag;
    oobFlag = fullFlag = FALSE;
    switch (mode) {
    case RF_PRED:
      if (RF_opt & OPT_FENS) {
        fullFlag = TRUE;
      }
      break;
    default:
      if (RF_opt & OPT_OENS) {
        oobFlag = TRUE;
      }
      if (RF_opt & OPT_IENS) {
        fullFlag = TRUE;
      }
      break;
    }
    while ((oobFlag == TRUE) || (fullFlag == TRUE)) {
      if (oobFlag == TRUE) {
        lockDENptr = &RF_lockDENoensPosix;
        lockQNTptr = &RF_lockQNToensPosix;
        obsSize = RF_observationSize;
      }
      else {
        lockDENptr = &RF_lockDENfensPosix;
        lockQNTptr = &RF_lockQNTfensPosix;
        obsSize = (mode == RF_PRED) ? RF_fobservationSize : RF_observationSize;
      }
      if (!potentiallyMixedMultivariate) {
        *lockDENptr = ptmvector(1, obsSize);
        for (i = 1; i <= obsSize; i++) {
          pthread_mutex_init(&((*lockDENptr)[i]), NULL);
        }
      }
      *lockQNTptr = ptmvector(1, obsSize);        
      for (i = 1; i <= obsSize; i++) {
        pthread_mutex_init(&((*lockQNTptr)[i]), NULL);
      }
      if (oobFlag == TRUE) {
        oobFlag = FALSE;
      }
      else {
        fullFlag = FALSE;
      }
    }
    potentiallyMixedMultivariate = TRUE;
  }
}
#else
void stackLocksPosix(char mode) { }
#endif
#ifdef NOT_HAVE_PTHREAD
void unstackLocksPosix(char mode) {
  uint i, j;
  pthread_mutex_destroy(&RF_lockEnsbUpdtCountPosix);
  pthread_mutex_destroy(&RF_lockPerfPosix);
  if (RF_optHigh & OPT_PART_PLOT) {
    for (i = 1; i <= RF_observationSize; i++) {
      pthread_mutex_destroy(&(RF_lockPartialPosix[i]));
    }
    free_ptmvector(RF_lockPartialPosix, 1, RF_observationSize);
  }
  if (RF_optHigh & OPT_WGHT) {
    uint gMembershipSize;
    if((RF_optHigh & OPT_WGHT_IBG) && (RF_optHigh & OPT_WGHT_OOB)) {
      switch (mode) {
      case RF_PRED:
        gMembershipSize = RF_fobservationSize;
        break;
      default:
        gMembershipSize = RF_observationSize;
        break;
      }
    }
    else {
      gMembershipSize  = RF_observationSize;
    }
    for (i = 1; i <= gMembershipSize; i++) {
      for (j = 1; j <= RF_observationSize; j++) {
        pthread_mutex_destroy(&(RF_lockWeightPosix[i][j]));
      }
    }
    for (i = 1; i <= gMembershipSize; i++) {    
      free_ptmvector(RF_lockWeightPosix[i], 1, RF_observationSize);
    }
    free_new_vvector(RF_lockWeightPosix, 1, gMembershipSize, NRUTIL_PTMLPTR);
    for (i = 1; i <= gMembershipSize; i++) {
      pthread_mutex_destroy(&(RF_lockWeightRowPosix[i]));
    }
    free_ptmvector(RF_lockWeightRowPosix, 1, gMembershipSize);
  }
  if (RF_opt & OPT_VIMP) {
    uint obsSize, xVimpSize;
    if (RF_opt & OPT_VIMP_JOIN) {
      xVimpSize = 1;
    }
    else {
      xVimpSize = RF_intrPredictorSize;
    }
    switch (mode) {
    case RF_PRED:
      obsSize = RF_fobservationSize;
      break;
    default:
      obsSize  = RF_observationSize;
      break;
    }
    for (i = 1; i <= xVimpSize; i++) {
      for (j = 1; j <= obsSize; j++) {
        pthread_mutex_destroy(&(RF_lockVimpPosix[i][j]));
      }
    }
    for (i = 1; i <= xVimpSize; i++) {
      free_ptmvector(RF_lockVimpPosix[i], 1, obsSize);
    }
    free_new_vvector(RF_lockVimpPosix, 1, xVimpSize, NRUTIL_PTMLPTR);
    for (i = 1; i <= obsSize; i++) {
      pthread_mutex_destroy(&(RF_lockVimpRowPosix[i]));
    }
    free_ptmvector(RF_lockVimpRowPosix, 1, obsSize);
    for (i = 1; i <= xVimpSize; i++) {
      pthread_mutex_destroy(&(RF_lockVimpColPosix[i]));
    }
    free_ptmvector(RF_lockVimpColPosix, 1, xVimpSize);
  }
  if ((RF_vtry > 0) && (RF_vtryMode != RF_VTRY_NULL)) {
    uint xVimpSize;
    xVimpSize = RF_xSize;
    for (i = 1; i <= xVimpSize; i++) {
      if (RF_holdBLKptr[i] > 0) {
        for (j = 1; j <= RF_holdBLKptr[i]; j++) {
          pthread_mutex_destroy(&(RF_lockVimpHoldoutPosix[i][j]));
        }
        free_ptmvector(RF_lockVimpHoldoutPosix[i], 1, RF_holdBLKptr[i]);
      }
    }
    free_new_vvector(RF_lockVimpHoldoutPosix, 1, xVimpSize, NRUTIL_PTMLPTR);    
  }
  if ((RF_timeIndex > 0) && (RF_statusIndex > 0)) {
    pthread_mutex_t   *lockDENptr;
    uint obsSize;  
    char oobFlag, fullFlag;
    oobFlag = fullFlag = FALSE;
    switch (mode) {
    case RF_PRED:
      if (RF_opt & OPT_FENS) {
        fullFlag = TRUE;
      }
      break;
    default:
      if (RF_opt & OPT_OENS) {
        oobFlag = TRUE;
      }
      if (RF_opt & OPT_IENS) {
        fullFlag = TRUE;
      }
      break;
    }
    while ((oobFlag == TRUE) || (fullFlag == TRUE)) {
      if (oobFlag == TRUE) {
        lockDENptr = RF_lockDENoensPosix;
        obsSize = RF_observationSize;
      }
      else {
        lockDENptr = RF_lockDENfensPosix;
        obsSize = (mode == RF_PRED) ? RF_fobservationSize : RF_observationSize;
      }
      for (i = 1; i <= obsSize; i++) {
        pthread_mutex_destroy(&(lockDENptr[i]));
      }
      free_ptmvector(lockDENptr, 1, obsSize);
      if (oobFlag == TRUE) {
        oobFlag = FALSE;
      }
      else {
        fullFlag = FALSE;
      }
    }
  }
  else {
    char  potentiallyMixedMultivariate = FALSE;
    if (RF_rTargetFactorCount > 0) {
      pthread_mutex_t   *lockDENptr;
      uint obsSize;  
      char oobFlag, fullFlag;
      oobFlag = fullFlag = FALSE;
      switch (mode) {
      case RF_PRED:
        if (RF_opt & OPT_FENS) {
          fullFlag = TRUE;
        }
        break;
      default:
        if (RF_opt & OPT_OENS) {
          oobFlag = TRUE;
        }
        if (RF_opt & OPT_IENS) {
          fullFlag = TRUE;
        }
        break;
      }
      while ((oobFlag == TRUE) || (fullFlag == TRUE)) {
        if (oobFlag == TRUE) {
          lockDENptr = RF_lockDENoensPosix;
          obsSize = RF_observationSize;
        }
        else {
          lockDENptr = RF_lockDENfensPosix;
          obsSize = (mode == RF_PRED) ? RF_fobservationSize : RF_observationSize;
        }
        if (!potentiallyMixedMultivariate) {
          for (i = 1; i <= obsSize; i++) {
            pthread_mutex_destroy(&(lockDENptr[i]));
          }
          free_ptmvector(lockDENptr, 1, obsSize);
        }
        if (oobFlag == TRUE) {
          oobFlag = FALSE;
        }
        else {
          fullFlag = FALSE;
        }
      }
      potentiallyMixedMultivariate = TRUE;
    }
    if (RF_rTargetNonFactorCount > 0) {
      pthread_mutex_t   *lockDENptr;
      pthread_mutex_t   *lockQNTptr;
      uint obsSize;  
      char oobFlag, fullFlag;
      oobFlag = fullFlag = FALSE;
      switch (mode) {
      case RF_PRED:
        if (RF_opt & OPT_FENS) {
          fullFlag = TRUE;
        }
        break;
      default:
        if (RF_opt & OPT_OENS) {
          oobFlag = TRUE;
        }
        if (RF_opt & OPT_IENS) {
          fullFlag = TRUE;
        }
        break;
      }
      while ((oobFlag == TRUE) || (fullFlag == TRUE)) {
        if (oobFlag == TRUE) {
          lockDENptr = RF_lockDENoensPosix;
          lockQNTptr = RF_lockQNToensPosix;
          obsSize = RF_observationSize;
        }
        else {
          lockDENptr = RF_lockDENfensPosix;
          lockQNTptr = RF_lockQNTfensPosix;
          obsSize = (mode == RF_PRED) ? RF_fobservationSize : RF_observationSize;
        }
        if (!potentiallyMixedMultivariate) {
          for (i = 1; i <= obsSize; i++) {
            pthread_mutex_destroy(&(lockDENptr[i]));
          }
          free_ptmvector(lockDENptr, 1, obsSize);
        }
        for (i = 1; i <= obsSize; i++) {
          pthread_mutex_destroy(&(lockQNTptr[i]));
        }
        free_ptmvector(lockQNTptr, 1, obsSize);
        if (oobFlag == TRUE) {
          oobFlag = FALSE;
        }
        else {
          fullFlag = FALSE;
        }
      }
      potentiallyMixedMultivariate = TRUE;
    }
  }
}
#else
void unstackLocksPosix(char mode) { }
#endif
