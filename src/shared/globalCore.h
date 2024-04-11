#ifndef  RF_GLOBAL_CORE_H
#define  RF_GLOBAL_CORE_H
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include <math.h>
#include <time.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#ifdef NOT_HAVE_PTHREAD
#include <pthread.h>
#endif

#include <R_ext/Print.h>
#include <Rdefines.h>
#define RF_nativePrint printR
#define RF_nativeError printR
#define RF_nativeExit  exit2R
#define RF_nativeNaN NA_REAL
#define RF_nativeIsNaN ISNA

 
#ifndef NULL
#define NULL 0
#endif
#ifndef TRUE
#define TRUE      0x01
#endif
#ifndef FALSE
#define FALSE     0x00
#endif
#ifndef uint
typedef unsigned int  uint;
#endif
#ifndef ulong
typedef unsigned long ulong;
#endif
#define RF_SEXP_ASCII_SIZE 16
#define RF_OUTP_ID   0  
#define RF_STRG_ID   1  
#define RF_ASRG_ID   2  
#define RF_OSRG_ID   3  
#define RF_ACIF_ID   4  
#define RF_OCIF_ID   5  
#define RF_ASRV_ID   6  
#define RF_OSRV_ID   7  
#define RF_AMRT_ID   8  
#define RF_OMRT_ID   9  
#define RF_ACLS_ID  10  
#define RF_OCLS_ID  11  
#define RF_ARGR_ID  12  
#define RF_ORGR_ID  13  
#define RF_ER_SURV  14  
#define RF_ER_CLAS  15  
#define RF_ER_REGR  16  
#define RF_PROX_ID  17  
#define RF_LEAF_CT  18  
#define RF_SEED_ID  19  
#define RF_SEED_VM  20  
#define RF_AQNT_ID  21  
#define RF_OQNT_ID  22  
#define RF_BLK_SRG  23  
#define RF_BLK_CLS  24  
#define RF_BLK_RGR  25  
#define RF_VMP_SRG  26  
#define RF_VMP_CLS  27  
#define RF_VMP_RGR  28  
#define RF_MISS_ID  29  
#define RF_XXXX_30  30  
#define RF_VUSE_ID  31  
#define RF_DPTH_ID  32  
#define RF_MEMB_ID  33  
#define RF_PRUN_ID  34  
#define RF_BOOT_CT  35  
#define RF_RMBR_ID  36  
#define RF_AMBR_ID  37  
#define RF_TN_RCNT  38  
#define RF_TN_ACNT  39  
#define RF_SPLT_ST  40  
#define RF_DPTH_ST  41  
#define RF_WGHT_ID  42  
#define RF_TN_SURV  43  
#define RF_TN_MORT  44  
#define RF_TN_NLSN  45  
#define RF_TN_CSHZ  46  
#define RF_TN_CIFN  47  
#define RF_TN_REGR  48  
#define RF_TN_CLAS  49  
#define RF_TN_KHZF  50  
#define RF_CSE_RGR  51  
#define RF_CSV_RGR  52  
#define RF_CSE_CLS  53  
#define RF_CSV_CLS  54  
#define RF_XXXX_55  55  
#define RF_PART_SR   56  
#define RF_PART_CL   57  
#define RF_PART_RG   58  
#define RF_DIST_ID   59  
#define RF_TREE_ID   60  
#define RF_NODE_ID   61  
#define RF_PARM_ID   62  
#define RF_CONT_PT   63  
#define RF_MWCP_SZ   64  
#define RF_MWCP_PT   65  
#define RF_MWCP_CT   66  
#define RF_HC_DIM    67  
#define RF_CONT_PTR  68  
#define RF_PAIR_CT   69  
#define RF_AUGM_X1   70  
#define RF_AUGM_X2   71  
#define RF_AUGM_XS   72  
#define RF_EMP_RSK     73  
#define RF_OEMP_RSK    74  
#define RF_STAT_LOT    75  
#define RF_HLDOUT_BLK  76  
#define RF_HLDOUT_SRG  77  
#define RF_HLDOUT_CLS  78  
#define RF_HLDOUT_RGR  79  
#define RF_AKHZ_ID       80  
#define RF_OKHZ_ID       81  
#define RF_SYTH_SZ       82  
#define RF_SYTH_TREE_ID  83  
#define RF_SYTH_NODE_ID  84  
#define RF_SYTH_PARM_ID  85  
#define RF_SYTH_CONT_PT  86  
#define RF_SYTH_MWCP_SZ  87  
#define RF_SYTH_MWCP_PT  88  
#define RF_SYTH_MWCP_CT  89  
#define RF_SYTH_HC_DIM   90  
#define RF_SYTH_CONT_PTR 91  
#define RF_SYTH_NODE_CT  92  
#define RF_TDC_MEMB_ID   93  
#define RF_CSE_DEN       94  
#define RF_CSV_DEN       95  
#define RF_BL_NODE_ID     96  
#define RF_BR_NODE_ID     97  
#define RF_FS_REC_ID      98  
#define RF_NODE_SZ        99  
#define RF_CASE_DEPTH    100  
#define RF_OPT_LO_GROW   101  
#define RF_OPT_HI_GROW   102  
#define RF_CPU_TIME      103  
#define RF_SEXP_CNT      104  
#define OPT_FENS      0x00000001 
#define OPT_IENS      0x00000001 
#define OPT_OENS      0x00000002 
#define OPT_PERF      0x00000004 
#define OPT_PERF_CALB 0x00000008 
#define OPT_LEAF      0x00000010 
#define OPT_TREE      0x00000020 
#define OPT_SEED      0x00000040 
#define OPT_MISS_OUT  0x00000080 
#define OPT_VIMP_TYP1 0x00000100 
#define OPT_VIMP_TYP2 0x00000200 
#define OPT_VIMP_JOIN 0x00000400 
#define OPT_VARUSED_F 0x00001000 
#define OPT_VARUSED_T 0x00002000 
#define OPT_PERF_GMN2 0x00004000 
#define OPT_CLAS_RFQ  0x00008000 
#define OPT_IMPU_ONLY 0x00010000 
#define OPT_OUTC_TYPE 0x00020000 
#define OPT_EMPR_RISK 0x00040000 
#define OPT_BOOT_TYP1 0x00080000 
#define OPT_BOOT_TYP2 0x00100000 
#define OPT_COMP_RISK 0x00200000 
#define OPT_SPLDPTH_1 0x00400000 
#define OPT_SPLDPTH_2 0x00800000 
#define OPT_QUANTLE   0x01000000 
#define OPT_VIMP      0x02000000 
#define OPT_ANON      0x04000000 
#define OPT_NODE_STAT 0x08000000 
#define OPT_PROX      0x10000000 
#define OPT_PROX_IBG  0x20000000 
#define OPT_PROX_OOB  0x40000000 
#define OPT_PROX_FUL  0x60000000 
#define OPT_WGHT      0x00000001 
#define OPT_WGHT_IBG  0x00000002 
#define OPT_WGHT_OOB  0x00000004 
#define OPT_WGHT_FUL  0x00000006 
#define OPT_MISS_SKIP 0x00000010 
#define OPT_MEMB_PRUN 0x00000020 
#define OPT_MEMB_USER 0x00000040 
#define OPT_SPLT_CUST 0x00000F00 
#define OPT_BOOT_SWOR 0x00001000 
#define OPT_TREE_ERR  0x00002000 
#define OPT_PART_PLOT 0x00004000 
#define OPT_DATA_PASG 0x00008000 
#define OPT_MEMB_OUTG 0x00010000 
#define OPT_MEMB_INCG 0x00020000 
#define OPT_TERM_OUTG 0x00040000 
#define OPT_TERM_INCG 0x00080000 
#define OPT_DIST      0x00100000 
#define OPT_DIST_IBG  0x00200000 
#define OPT_DIST_OOB  0x00400000 
#define OPT_DIST_FUL  0x00600000 
#define OPT_JIT_TOP   0x00800000 
#define OPT_TDC_BIT1  0x01000000 
#define OPT_TDC_BIT2  0x02000000 
#define OPT_TDC_BIT3  0x04000000 
#define OPT_DATA_PASP 0x08000000 
#define OPT_CSE       0x10000000 
#define OPT_CSV       0x20000000 
#define ACTIVE    0x02
#define LEFT      0x01
#define RIGHT     0x02
#define NEITHER   0x00
#define BOTH      0x03
#define EPSILON 1.0e-9
#define RF_GROW   0x01
#define RF_PRED   0x02
#define RF_REST   0x04
#define RF_NONE   0x08
#define RF_REAL   0x10
#define CLAS_FAM     0
#define REGR_FAM     1
#define SURV_FAM     2
#define CRSK_FAM     3
#define APROX 0
#define EXACT 1
#define SURV_LGRNK   1
#define SURV_LRSCR   2
#define SURV_CR_LAU  3
#define RAND_SPLIT   4
#define REGR_NRM     5
#define CLAS_NRM     6
#define USPV_SPLIT   7
#define MVRG_SPLIT   8 
#define MVCL_SPLIT   9 
#define MVMX_SPLIT  10 
#define CUST_SPLIT  11
#define REGR_QUANT  12
#define LARG_QUANT  13
#define SURV_BSG1   14
#define CLAS_AU_ROC 15
#define CLAS_ENTROP 16
#define SURV_TDC    20
#define MAHALANOBIS 21
#define SURV_CR_GEN 22
#define MAXM_SPLIT  22 
#define AUGT_INTR_NONE      0
#define AUGT_INTR_MULT      1
#define AUGT_INTR_DIVS      2
#define AUGT_INTR_ADDT      3
#define AUGT_INTR_SUBT      4
#define MAX_EXACT_LEVEL sizeof(uint) * 8
#define SAFE_FACTOR_SIZE 16
#define RF_WGHT_UNIFORM 1
#define RF_WGHT_INTEGER 2
#define RF_WGHT_GENERIC 3
#define RF_DISTANCE_EUCLIDEAN 1
#define NATIVE_TYPE_CHARACTER 0
#define NATIVE_TYPE_INTEGER   1
#define NATIVE_TYPE_NUMERIC   2
#define NATIVE_TYPE_LIST      3
#endif
