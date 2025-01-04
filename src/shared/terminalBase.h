#ifndef RF_TERMINAL_BASE_H
#define RF_TERMINAL_BASE_H
struct nodeBase;
typedef struct terminalBase TerminalBase;
struct terminalBase {
  unsigned int nodeID;
  struct nodeBase *mate;
  unsigned int membrCount;
  void *outcome;
  struct terminalRegression *regressionBase;
  struct terminalClassification *classificationBase;
  struct terminalSurvival *survivalBase;
  struct terminalCompetingRisk *competingRiskBase;
};
typedef struct terminalRegression TerminalRegression;
struct terminalRegression {
  struct terminalBase *base;
  unsigned int rnfCount;
  unsigned int *rnfIndex;
  double *meanResponse;
};
typedef struct terminalClassification TerminalClassification;
struct terminalClassification {
  struct terminalBase *base;
  unsigned int   rfCount;
  unsigned int *rfIndex;
  unsigned int  *rfSize;
  unsigned int **multiClassProb;
  double        *maxClass;
};
typedef struct terminalSurvival TerminalSurvival;
struct terminalSurvival {
  struct terminalBase *base;
  unsigned int eTypeSize;  
  unsigned int eTimeSize;
  unsigned int mTimeSize;
  unsigned int aeTimeSize;
  unsigned int sTimeSize;
  unsigned int *atRiskCount;
  double *atRiskTime;
  unsigned int **eventCount;
  unsigned int *eventTimeIndex;
  double **localRatio;
  double *localSurvival;
  double *localNelsonAalen;
  double *localHazard;
  double *allNelsonAalen;
  double *allHazard;
  double *nelsonAalen;
  double *hazard;
  double *survival;
  double *outcome;
};
typedef struct terminalCompetingRisk TerminalCompetingRisk;
struct terminalCompetingRisk {
  struct terminalBase *base;
  unsigned int eTypeSize;
  unsigned int mTimeSize;
  unsigned int sTimeSize;
  unsigned int eTimeSize;
  unsigned int *atRiskCount;
  double *atRiskTime;
  unsigned int **eventCount;
  unsigned int *eventTimeIndex;
  double **localRatio;
  double **localCSH;
  double **localCIF;
  double **CSH;
  double **CIF;
  double *outcome;
};
#endif
