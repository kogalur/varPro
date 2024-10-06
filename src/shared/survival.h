#ifndef RF_SURVIVAL_H
#define RF_SURVIVAL_H
#include "terminalBase.h"
void assignAllSurvivalOutcomes(char mode, uint treeID, TerminalBase *term);
void assignAllCompetingRiskOutcomes(char mode, uint treeID, TerminalBase *term);
void assignSurvival(uint              treeID,
                    TerminalSurvival *parent,
                    double           *tn_surv_ptr);
void assignNelsonAalen(uint              treeID,
                       TerminalSurvival *parent,
                       double           *tn_nlsn_ptr);
void assignSurvivalOutcome(uint              treeID,
                           TerminalSurvival *parent,
                           double           *tn_mort_ptr);
void assignCSH(uint                   treeID,
               TerminalCompetingRisk *parent,
               double               **tn_cshz_ptr);
void assignCIF(uint                   treeID,
               TerminalCompetingRisk *parent,
               double               **tn_cifn_ptr);
void assignCompetingRiskOutcome(uint                   treeID,
                                TerminalCompetingRisk *parent,
                                double                *tn_mort_ptr);
#endif
