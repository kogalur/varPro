#ifndef RF_STACK_H
#define RF_STACK_H
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
                                     uint     **mRecordMap);
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
                                       double ***observation);
char stackTrainingDataArraysWithoutPass(char mode);
char stackTestDataArraysWithPass       (char mode);
char stackTestDataArraysWithoutPass    (char mode);
#endif
