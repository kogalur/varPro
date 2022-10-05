#ifndef RF_SURVIVAL_H
#define RF_SURVIVAL_H
#include "terminalBase.h"
void assignSurvival(uint          treeID,
                    TerminalBase *parent,
                    double       *tn_surv_ptr);
void assignNelsonAalen(uint          treeID,
                       TerminalBase *parent,
                       double       *tn_nlsn_ptr);
void assignCSH(uint          treeID,
               TerminalBase *parent,
               double      **tn_cshz_ptr);
void assignCIF(uint          treeID,
               TerminalBase *parent,
               double      **tn_cifn_ptr);
void assignMortality(uint          treeID,
                     TerminalBase *parent,
                     double       *tn_mort_ptr);
#endif
