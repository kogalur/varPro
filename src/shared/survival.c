
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***
#include           "globalCore.h"
#include           "externalCore.h"
// *** THIS HEADER IS AUTO GENERATED. DO NOT EDIT IT ***

      
    

#include "survival.h"
#include "termBaseOps.h"
void assignSurvival(uint          treeID,
                    TerminalBase *parent,
                    double       *tn_surv_ptr) {
  uint k;
  stackSurvival(parent);
  for (k = 1; k <= parent -> sTimeSize; k++) {
    (parent -> survival)[k] = tn_surv_ptr[k];
  }
}
void assignNelsonAalen(uint          treeID,
                       TerminalBase *parent,
                       double       *tn_nlsn_ptr) {
  uint k;
  stackNelsonAalen(parent);
  for (k = 1; k <= parent -> sTimeSize; k++) {
    (parent -> nelsonAalen)[k] = tn_nlsn_ptr[k];
  }
}
void assignCSH(uint          treeID,
               TerminalBase *parent,
               double      **tn_cshz_ptr) {
  uint j, k;
  stackCSH(parent);
  for (j = 1; j <= parent -> eTypeSize; j++) {
    for (k = 1; k <= parent -> sTimeSize; k++) {
      (parent -> csh)[j][k] = tn_cshz_ptr[j][k];
    }
  }
}
void assignCIF(uint          treeID,
               TerminalBase *parent,
               double      **tn_cifn_ptr) {
  uint j, k;
  stackCIF(parent);
  for (j = 1; j <= parent -> eTypeSize; j++) {
    for (k = 1; k <= parent -> sTimeSize; k++) {
      (parent -> cif)[j][k] = tn_cifn_ptr[j][k];
    }
  }
}
void assignMortality(uint treeID,
                     TerminalBase *parent,
                     double       *tn_mort_ptr) {
  uint j;
  stackMortality(parent);
  for (j = 1; j <= parent -> eTypeSize; j++) {
    (parent -> mortality)[j] = tn_mort_ptr[j];
  }
}
