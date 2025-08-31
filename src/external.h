// *** THIS FILE IS AUTO GENERATED. DO NOT EDIT IT ***
#ifndef RF_EXTERNAL_H
#define RF_EXTERNAL_H

#include "shared/globalCore.h"
#include "shared/leafLink.h"
extern uint    *VP_treeID_;
extern uint    *VP_nodeID_;
extern uint    *VP_xReleaseID_;
extern uint    *VP_brmCT_;
extern double  *VP_importance_;
extern double  *VP_complementStat_;
extern double  *VP_oobStat_;
extern uint    *VP_branchPopID_;
extern uint    *VP_complementID_;
extern uint    *VP_complementCT_;
extern uint    *VP_testCaseNodeID_;
extern uint    *VP_strengthTreeID_;
extern double  *VP_twinStat_;
extern uint    *VP_twinStatID_;
extern uint    *VP_twinFreqTable_;
extern double  **VP_dimImpSRVptr;
extern double  **VP_dimImpRGRptr;
extern double  ***VP_dimImpCLSptr;
extern uint    ***VP_brmCTptr;
extern uint **VP_testCaseNodeIDptr;
extern double **VP_twinStat_ptr;
extern uint   **VP_twinStatID_ptr;
extern uint  ***VP_twinFreqTable_ptr;
extern uint     VP_strengthTreeCount;
extern uint    *VP_strengthTreeID; 
extern uint    *VP_branchCount;
extern uint   **VP_branchID;
extern uint  **VP_branchMemberCount;
extern uint   **VP_xReleaseCount;
extern uint  ***VP_xReleaseIDArray;
extern uint  ***VP_complementCount;
extern uint  ***VP_branchMembers;
extern uint  ****VP_complementMembers;
extern uint   **VP_proxyIndv;
extern uint   **VP_proxyIndvDepth;
extern uint ***VP_RMBR_ID_ptr;
extern uint ***VP_OMBR_ID_ptr;
extern uint ***VP_IMBR_ID_ptr;
extern uint  **VP_TN_RCNT_ptr;
extern uint  **VP_TN_OCNT_ptr;
extern uint  **VP_TN_ICNT_ptr;
extern double   *VP_cpuTime_;
extern uint   VP_totalRecordCount;
extern uint      VP_maxRulesTree;
extern uint      VP_maxTree;
extern uint      VP_neighbourSize;
extern uint      VP_xReduceSize;
extern uint     *VP_xReduceIndx;
extern uint      VP_opt;

#endif
