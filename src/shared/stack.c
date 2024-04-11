
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "stack.h"
#include "nrutil.h"
#include "error.h"
char stackTrainingDataArraysWithPass(char      mode,
                                     uint      ySize,
                                     uint      ntree,
                                     double  **responseIn,
                                     uint      startTimeIndex,
                                     uint      statusIndex,
                                     uint      timeIndex,
                                     uint     *startMasterTimeIndexIn,
                                     uint     *masterTimeIndexIn,
                                     uint      observationSize,
                                     double  **observationIn,
                                     double ****response,
                                     double  ***time,
                                     double  ***startTime,
                                     uint    ***startMasterTimeIndex,
                                     uint    ***masterTimeIndex,
                                     double  ***status,
                                     double ****observation,
                                     char      *mStatusFlag,
                                     char      *mTimeFlag,
                                     char      *mResponseFlag,
                                     char      *mPredictorFlag,
                                     uint      *mRecordSize,
                                     uint     **mRecordMap) {
  char result;
  uint i;
  result = TRUE;
  if (mode != RF_NONE) {
    *response = (double ***) new_vvector(1, ntree, NRUTIL_DPTR2);
    if (ySize > 0) {
      for (i = 1 ; i <= ntree; i++) {
        (*response)[i] = responseIn;
      }
      *startTime = *time = NULL;
      *startMasterTimeIndex = *masterTimeIndex = NULL;
      if (statusIndex > 0) {
        if (startTimeIndex == 0) {
          *time = (double **) new_vvector(1, ntree, NRUTIL_DPTR);
          *masterTimeIndex = (uint **) new_vvector(1, ntree, NRUTIL_UPTR);
          for (i = 1 ; i <= ntree; i++) {
            (*time)[i] = responseIn[timeIndex];
            (*masterTimeIndex)[i] = masterTimeIndexIn;
          }
        }
        else {
          *startTime = (double **) new_vvector(1, ntree, NRUTIL_DPTR);
          *time      = (double **) new_vvector(1, ntree, NRUTIL_DPTR);
          *startMasterTimeIndex = (uint **) new_vvector(1, ntree, NRUTIL_UPTR);
          *masterTimeIndex      = (uint **) new_vvector(1, ntree, NRUTIL_UPTR);
          for (i = 1 ; i <= ntree; i++) {
            (*startTime)[i] = responseIn[startTimeIndex];
            (*time)[i] = responseIn[timeIndex];
            (*startMasterTimeIndex)[i] = startMasterTimeIndexIn;
            (*masterTimeIndex)[i] = masterTimeIndexIn;
          }
        }
        *status = (double **) new_vvector(1, ntree, NRUTIL_DPTR);
        for (i = 1 ; i <= ntree; i++) {
          (*status)[i] = responseIn[statusIndex];
        }
      }
    }
    else {
      for (i = 1 ; i <= ntree; i++) {
        (*response)[i] = NULL;
      }
    }
    *observation = (double ***) new_vvector(1, ntree, NRUTIL_DPTR2);
    for (i = 1 ; i <= ntree; i++) {
      (*observation)[i] = observationIn;
    }
  }
  *mStatusFlag = *mTimeFlag = *mResponseFlag = *mPredictorFlag = FALSE;
  *mRecordSize = 0;
  *mRecordMap = NULL;
  return result;
}
void unstackTrainingDataArraysWithPass(char      mode,
                                       uint      ySize,
                                       uint      ntree,
                                       uint      timeIndex,
                                       uint      statusIndex,
                                       uint      startTimeIndex,
                                       double ***response,
                                       double  **time,
                                       uint    **masterTimeIndex,
                                       double  **startTime,
                                       uint    **startMasterTimeIndex,
                                       double  **status,
                                       double ***observation) {
  if (mode != RF_NONE) {  
    free_new_vvector(response, 1, ntree, NRUTIL_DPTR2);
    if (ySize > 0) {
      if ((timeIndex > 0) && (statusIndex > 0)) {
        if (startTimeIndex == 0) {
          free_new_vvector(time, 1, ntree, NRUTIL_DPTR);
          free_new_vvector(masterTimeIndex, 1, ntree, NRUTIL_UPTR);
        }
        else {
          free_new_vvector(startTime, 1, ntree, NRUTIL_DPTR);
          free_new_vvector(time, 1, ntree, NRUTIL_DPTR);
          free_new_vvector(startMasterTimeIndex, 1, ntree, NRUTIL_UPTR);
          free_new_vvector(masterTimeIndex, 1, ntree, NRUTIL_UPTR);
        }
        free_new_vvector(status, 1, ntree, NRUTIL_DPTR);
      }
    }
    free_new_vvector(observation, 1, ntree, NRUTIL_DPTR2);    
  }
}
char stackTrainingDataArraysWithoutPass(char mode){
  return TRUE;
}
char unstackTrainingDataArraysWithoutPass(char mode){
  return TRUE;
}
char stackTestDataArraysWithPass (char mode,
                                  uint frSize,
                                  uint ntree,
                                  double **fresponseIn,
                                  uint    fobservationSize,
                                  double **fobservationIn,
                                  double ****fresponse,
                                  double ****fobservation){
  char result;
  uint i;
  result = TRUE;
  if (mode == RF_PRED) {
    *fresponse = (double ***) new_vvector(1, ntree, NRUTIL_DPTR2);
    if (frSize > 0) {
      for (i = 1 ; i <= ntree; i++) {
        (*fresponse)[i] = fresponseIn;
      }
    }
    else {
      for (i = 1 ; i <= ntree; i++) {
        (*fresponse)[i] = NULL;
      }
    }
    *fobservation = (double ***) new_vvector(1, ntree, NRUTIL_DPTR2);
    for (i = 1 ; i <= ntree; i++) {
      (*fobservation)[i] = fobservationIn;
    }
  }
  return result;
}
char unstackTestDataArraysWithPass (char mode,
                                    uint ntree,
                                    double ***fresponse,
                                    double ***fobservation){
  char result;
  result = TRUE;
  if (mode == RF_PRED) {
    free_new_vvector(fresponse, 1, ntree, NRUTIL_DPTR2);
    free_new_vvector(fobservation, 1, ntree, NRUTIL_DPTR2);
  }
  return result;
}
char stackTestDataArraysWithoutPass    (char mode){
  return TRUE;
}
char unstackTestDataArraysWithoutPass    (char mode){
  return TRUE;
}
