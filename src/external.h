// *** THIS FILE IS AUTO GENERATED. DO NOT EDIT IT ***
#ifndef RF_EXTERNAL_H
#define RF_EXTERNAL_H

#include "shared/globalCore.h"
extern uint    *VP_treeID_;
extern uint    *VP_nodeID_;
extern uint    *VP_xReleaseID_;
extern uint    *VP_oobCT_;
extern double  *VP_importance_;
extern double  *VP_complementStat_;
extern double  *VP_oobStat_;
extern uint    *VP_oobID_;
extern uint    *VP_complementID_;
extern uint    *VP_complementCT_;
extern double  **VP_dimImpSRVptr;
extern double  **VP_dimImpRGRptr;
extern double  ***VP_dimImpCLSptr;
extern uint    ***VP_oobCTptr;
extern uint     VP_strengthTreeCount;
extern uint    *VP_strengthTreeID; 
extern uint    *VP_branchCount;
extern uint   **VP_branchID;
extern uint  **VP_oobCount;
extern uint   **VP_xReleaseCount;
extern uint  ***VP_xReleaseIDArray;
extern uint  ***VP_complementCount;
extern uint  ***VP_oobMembers;
extern uint  ****VP_complementMembers;
extern uint   **VP_proxyIndv;
extern uint   **VP_proxyIndvDepth;
extern double   *VP_cpuTime_;
extern uint   VP_totalRecordCount;
extern uint      VP_maxRulesTree;
extern uint      VP_maxTree;
extern uint      VP_opt;

#endif
