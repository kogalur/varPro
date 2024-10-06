
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "survival.h"
#include "termBaseOps.h"
void assignAllSurvivalOutcomes(char mode,
                               uint treeID,
                               TerminalBase *term) {
  TerminalSurvival *parent;
  parent = term -> survivalBase;
  assignSurvival(treeID, parent, RF_TN_SURV_ptr[treeID][term -> nodeID]);
  assignNelsonAalen(treeID, parent, RF_TN_NLSN_ptr[treeID][term -> nodeID]);
  assignSurvivalOutcome(treeID, parent, RF_TN_MORT_ptr[treeID][term -> nodeID]);
}
void assignAllCompetingRiskOutcomes(char mode,
                                    uint treeID,
                                    TerminalBase *term) {
  TerminalCompetingRisk *parent;
  parent = term -> competingRiskBase;
  assignCSH(treeID, parent, RF_TN_CSHZ_ptr[treeID][term -> nodeID]);
  assignCIF(treeID, parent, RF_TN_CIFN_ptr[treeID][term -> nodeID]);
  assignCompetingRiskOutcome(treeID, parent, RF_TN_MORT_ptr[treeID][term -> nodeID]);        
}
void assignSurvival(uint              treeID,
                    TerminalSurvival *parent,
                    double           *tn_surv_ptr) {
  uint k;
  stackSurvival(parent);
  for (k = 1; k <= parent -> sTimeSize; k++) {
    (parent -> survival)[k] = tn_surv_ptr[k];
  }
}
void assignNelsonAalen(uint              treeID,
                       TerminalSurvival *parent,
                       double           *tn_nlsn_ptr) {
  uint k;
  stackNelsonAalen(parent);
  for (k = 1; k <= parent -> sTimeSize; k++) {
    (parent -> nelsonAalen)[k] = tn_nlsn_ptr[k];
  }
}
void assignSurvivalOutcome(uint              treeID,
                           TerminalSurvival *parent,
                           double           *tn_mort_ptr) {
  uint j;
  stackSurvivalOutcome(parent);
  for (j = 1; j <= parent -> eTypeSize; j++) {
    (parent -> outcome)[j] = tn_mort_ptr[j];
  }
}
void assignCSH(uint                   treeID,
               TerminalCompetingRisk *parent,
               double               **tn_cshz_ptr) {
  uint j, k;
  stackCSH(parent);
  for (j = 1; j <= parent -> eTypeSize; j++) {
    for (k = 1; k <= parent -> sTimeSize; k++) {
      (parent -> CSH)[j][k] = tn_cshz_ptr[j][k];
    }
  }
}
void assignCIF(uint                   treeID,
               TerminalCompetingRisk *parent,
               double               **tn_cifn_ptr) {
  uint j, k;
  stackCIF(parent);
  for (j = 1; j <= parent -> eTypeSize; j++) {
    for (k = 1; k <= parent -> sTimeSize; k++) {
      (parent -> CIF)[j][k] = tn_cifn_ptr[j][k];
    }
  }
}
void assignCompetingRiskOutcome(uint                   treeID,
                                TerminalCompetingRisk *parent,
                                double                *tn_mort_ptr) {
  uint j;
  stackCompetingRiskOutcome(parent);
  for (j = 1; j <= parent -> eTypeSize; j++) {
    (parent -> outcome)[j] = tn_mort_ptr[j];
  }
}
