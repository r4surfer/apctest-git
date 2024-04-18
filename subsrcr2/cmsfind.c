/*
 cmsfind: Tue Jun  1 14:30:29 2021
 */

/* CALLED SUBPROGRAM */
#include "wvsb.h"

CMSFIND (pufb1, pufb2, KEY_TS, DESCR_TS, ALTKEYDESCR_DS, SLIB_TM, SEC_TS, F1_IS)
struct ufb *pufb1;
struct ufb *pufb2;
struct symbol *KEY_TS;
struct symbol *DESCR_TS;
struct symbol *ALTKEYDESCR_DS;
struct symbol *SLIB_TM;
struct symbol *SEC_TS;
struct symbol *F1_IS;
{

#include "cmsfind.h"

/* Declare and initialize the local variables */
	int g_ret;
	int g_argcnt = 0;

static char BFAC_ts[1];
static struct symbol BFAC_TS ={(long) 1, (char *)  BFAC_ts, 1, 0, 0, 0, 1, 1};
static char BLANKLINE_ts[79];
static struct symbol BLANKLINE_TS ={(long) 1, (char *)  BLANKLINE_ts, 79, 0, 0, 0, 79, 79};
static char BLINE_ts[79];
static struct symbol BLINE_TS ={(long) 1, (char *)  BLINE_ts, 79, 0, 0, 0, 79, 79};
static long C_iv[2];
static struct symbol C_IV ={(long) 4, (char *)  C_iv, 4, 1, 2, 0, 8, 8};
static char FAC_tv[16][1];
static struct symbol FAC_TV ={(long) 1, (char *)  FAC_tv, 1, 1, 16, 0, 16, 16};
static long F1_iv[2];
static struct symbol F1_IV ={(long) 4, (char *)  F1_iv, 4, 1, 2, 0, 8, 8};
static char FLAG_tv[16][1];
static struct symbol FLAG_TV ={(long) 1, (char *)  FLAG_tv, 1, 1, 16, 0, 16, 16};
static char I_tv[24][80];
static struct symbol I_TV ={(long) 1, (char *)  I_tv, 80, 1, 24, 0, 1920, 1920};
static char PGMSEC_ts[5];
static struct symbol PGMSEC_TS ={(long) 1, (char *)  PGMSEC_ts, 5, 0, 0, 0, 5, 5};
static char KEYS_ts[16];
static struct symbol KEYS_TS ={(long) 1, (char *)  KEYS_ts, 16, 0, 0, 0, 16, 16};
static char READKEY_ts[99];
static struct symbol READKEY_TS ={(long) 1, (char *)  READKEY_ts, 99, 0, 0, 0, 99, 99};
static char SCREEN_tv[16][77];
static struct symbol SCREEN_TV ={(long) 1, (char *)  SCREEN_tv, 77, 1, 16, 0, 1232, 1232};
static long P_iv[2];
static struct symbol P_IV ={(long) 4, (char *)  P_iv, 4, 1, 2, 0, 8, 8};
static char PRIMARY_tm[30][1];
static struct symbol PRIMARY_TM ={(long) 1, (char *)  PRIMARY_tm, 1, 2, 30, 1, 30, 30};
static char PFAC_ts[1];
static struct symbol PFAC_TS ={(long) 1, (char *)  PFAC_ts, 1, 0, 0, 0, 1, 1};
static char PLOWKEY_ts[99];
static struct symbol PLOWKEY_TS ={(long) 1, (char *)  PLOWKEY_ts, 99, 0, 0, 0, 99, 99};
static char OBS_ts[1];
static struct symbol OBS_TS ={(long) 1, (char *)  OBS_ts, 1, 0, 0, 0, 1, 1};
static char LAST_VERSION_ts[6];
static struct symbol LAST_VERSION_TS ={(long) 1, (char *)  LAST_VERSION_ts, 6, 0, 0, 0, 6, 6};
static char WORK_ts[100];
static struct symbol WORK_TS ={(long) 1, (char *)  WORK_ts, 100, 0, 0, 0, 100, 100};
static char SEARCH_ts[30];
static struct symbol SEARCH_TS ={(long) 1, (char *)  SEARCH_ts, 30, 0, 0, 0, 30, 30};
static char SEQNR_ts[3];
static struct symbol SEQNR_TS ={(long) 1, (char *)  SEQNR_ts, 3, 0, 0, 0, 3, 3};
static char TEXT_ts[50];
static struct symbol TEXT_TS ={(long) 1, (char *)  TEXT_ts, 50, 0, 0, 0, 50, 50};
static char TITLE_ts[79];
static struct symbol TITLE_TS ={(long) 1, (char *)  TITLE_ts, 79, 0, 0, 0, 79, 79};
static char TYPE_ts[16];
static struct symbol TYPE_TS ={(long) 1, (char *)  TYPE_ts, 16, 0, 0, 0, 16, 16};
static char UFBKL_ts[1];
static struct symbol UFBKL_TS ={(long) 1, (char *)  UFBKL_ts, 1, 0, 0, 0, 1, 1};
static char UFBKD_ts[2];
static struct symbol UFBKD_TS ={(long) 1, (char *)  UFBKD_ts, 2, 0, 0, 0, 2, 2};
static char FILE_ts[8];
static struct symbol FILE_TS ={(long) 1, (char *)  FILE_ts, 8, 0, 0, 0, 8, 8};
static char LIB_ts[8];
static struct symbol LIB_TS ={(long) 1, (char *)  LIB_ts, 8, 0, 0, 0, 8, 8};
static char VOL_ts[6];
static struct symbol VOL_TS ={(long) 1, (char *)  VOL_ts, 6, 0, 0, 0, 6, 6};
static char CMS2V_ts[50];
static struct symbol CMS2V_TS ={(long) 1, (char *)  CMS2V_ts, 50, 0, 0, 0, 50, 50};
static long DISP_is;
static struct symbol DISP_IS ={(long) 4, (char *) &DISP_is, 4, 0, 0, 0, 4, 4};
static long KEY_is;
static struct symbol KEY_IS ={(long) 4, (char *) &KEY_is, 4, 0, 0, 0, 4, 4};
static long BREAKP_is;
static struct symbol BREAKP_IS ={(long) 4, (char *) &BREAKP_is, 4, 0, 0, 0, 4, 4};
static long X_is;
static struct symbol X_IS ={(long) 4, (char *) &X_is, 4, 0, 0, 0, 4, 4};
static long F2_is;
static struct symbol F2_IS ={(long) 4, (char *) &F2_is, 4, 0, 0, 0, 4, 4};
static long SUBLEN_is;
static struct symbol SUBLEN_IS ={(long) 4, (char *) &SUBLEN_is, 4, 0, 0, 0, 4, 4};
static long ANYHITS_is;
static struct symbol ANYHITS_IS ={(long) 4, (char *) &ANYHITS_is, 4, 0, 0, 0, 4, 4};
static long C_is;
static struct symbol C_IS ={(long) 4, (char *) &C_is, 4, 0, 0, 0, 4, 4};
static long SEACHED_ONCE_is;
static struct symbol SEACHED_ONCE_IS ={(long) 4, (char *) &SEACHED_ONCE_is, 4, 0, 0, 0, 4, 4};
static long I_is;
static struct symbol I_IS ={(long) 4, (char *) &I_is, 4, 0, 0, 0, 4, 4};
static long SLEN_is;
static struct symbol SLEN_IS ={(long) 4, (char *) &SLEN_is, 4, 0, 0, 0, 4, 4};
static long Z_is;
static struct symbol Z_IS ={(long) 4, (char *) &Z_is, 4, 0, 0, 0, 4, 4};
static long KEYHIT_is;
static struct symbol KEYHIT_IS ={(long) 4, (char *) &KEYHIT_is, 4, 0, 0, 0, 4, 4};
static struct symbol *allvar[]={
&BFAC_TS,
&BLANKLINE_TS,
&BLINE_TS,
&C_IV,
&FAC_TV,
&F1_IV,
&FLAG_TV,
&I_TV,
&PGMSEC_TS,
&KEYS_TS,
&READKEY_TS,
&SCREEN_TV,
&P_IV,
&PRIMARY_TM,
&PFAC_TS,
&PLOWKEY_TS,
&OBS_TS,
&LAST_VERSION_TS,
&WORK_TS,
&SEARCH_TS,
&SEQNR_TS,
&TEXT_TS,
&TITLE_TS,
&TYPE_TS,
&UFBKL_TS,
&UFBKD_TS,
&FILE_TS,
&LIB_TS,
&VOL_TS,
&CMS2V_TS,
&DISP_IS,
&KEY_IS,
&BREAKP_IS,
&X_IS,
&F2_IS,
&SUBLEN_IS,
&ANYHITS_IS,
&C_IS,
&SEACHED_ONCE_IS,
&I_IS,
&SLEN_IS,
&Z_IS,
&KEYHIT_IS,
NULL};
static struct ufb *ufbptr[64]={
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL};

static int Iflag = 1;
       int G_hold;

extern int Call_args;	/* count of callers args */
int CMS_nargs = Call_args;	/* Local var to keep nargs */

/* local BCD & int pointers to callers parms */
	long *ALTKEYDESCR_ds;
	long *F1_is;

/* set the Str COMs */

	g_cidx = 0L;

/* Set pointers to callers parms */
	if ((Call_args >= 5) && (ALTKEYDESCR_DS != NULL))
		ALTKEYDESCR_ds = (long *) *( (long *) ALTKEYDESCR_DS + 1);
	if ((Call_args >= 8) && (F1_IS != NULL))
		F1_is = (long *) *( (long *) F1_IS + 1);


ufbptr[0] = pufb1;
ufbptr[1] = pufb2;
	wb_init (&Iflag, allvar, NULL);	/* init for sub program */
	G_hold = G_scnt;		/* hold the stack count */
/* 000001 */
/* 000029 */
/* 000094 */
/* 000098 */
	str_asgn ("04.16.07 01/05/87 OS 7.10 Compatibility           ", (long) 50 , 
		CMS2V_ts, (long) 50 );
/* 000099 */
/* 000100 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("00", g_astr0);
	wb_vinit (g_astr0, &UFBKL_TS, &UFBKD_TS, NULL);
/* 000101 */
	str_asgn (" ", (long) 1 , 
		FILE_ts, (long) 8 );
/* 000102 */
	*(F1_is) =  0;
/* 000103 */
	Call_args = 4;
	GETNAMES (ufbptr[1-1], FILE_ts
, LIB_ts
, VOL_ts
);
/* 000104 */
	Call_args = 2;
	GETUFBKL (ufbptr[1-1], UFBKL_ts
);
/* 000105 */
	Call_args = 2;
	GETUFBDK (ufbptr[1-1], UFBKD_ts
);
/* 000106 */
	g_ind0 = wb_val (UFBKL_ts,  1 , 1);
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	l_BCD (g_ind0, g_exp1);
	addl ('+', g_exp0, g_exp1, g_exp2);
	g_ind1 = 2;
	g_ind2 = wb_val (UFBKD_ts,  2 , g_ind1);
	l_BCD (g_ind2, g_exp3);
	addl ('+', g_exp2, g_exp3, g_exp4);
	BCD_l (g_exp4, &g_ind3);
	 (DISP_is) =  g_ind3;
/* 000107 */
	g_ind0 = ( (DISP_is) -  468);
	g_ind1 = (g_ind0 > 0);
	if (g_ind1)
	  {
		G_scnt = G_hold;	/* restore stack count */
		return (0);
	  }
/* 000108 */
	wb_int ((long) 8, ALTKEYDESCR_ds, g_exp0);
	BCD_l (g_exp0, &g_ind0);
	 (KEY_is) =  g_ind0;
/* 000109 */
	g_exp0[0] = 0x43100000L;
	g_exp0[1] = 0L;
	multl (ALTKEYDESCR_ds, g_exp0, g_exp1);
	g_exp2[0] = 0x43100000L;
	g_exp2[1] = 0L;
	wb_mod (g_exp1, g_exp2, g_exp3);
	g_exp4[0] = 0x40100000L;
	g_exp4[1] = 0L;
	addl ('+', g_exp3, g_exp4, g_exp5);
	/* Make MIN/MAX Gmind = 0 */
	g_m[0] = (long) BCDCONST;
	g_m[1] = g_exp5[0];
	g_m[2] = g_exp5[1];
	g_m[3] = (long) BCDCONST;
	g_m[4] = 1112539136L;
	g_m[5] = 0L;
	g_m[6] = 0L;
	wb_min (0, 2, g_exp6);
	BCD_l (g_exp6, &g_ind0);
	 (BREAKP_is) =  g_ind0;
/* 000110 */
	g_ind0 = ( (BREAKP_is) +  1);
	g_astr0 = (char *) (char *)(KEY_TS->p);
	g_astr0 += ( g_ind0-1);
	g_ind1 = ((KEY_TS->len) - ( g_ind0-1));
	g_ind2 = wb_str (g_astr0,  g_ind1 , " ",  1 );
	g_ind3 = (g_ind2 == 0);
	if (g_ind3)
	  {
		goto lL01380;
	  }
/* 000113 */
	Call_args = 4;
	REDALT0 (ufbptr[1-1], KEY_TS, &KEY_IS, F1_IS);
/* 000114 */
	g_ind0 = (*(F1_is) -  0);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL01380;
	  }
/* 000115 */
	g_ind0 = wb_str (SLIB_TM->p, (SLIB_TM->tbyte), " ",  1 );
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lALL_DONE;
	  }
/* 000116 */
/* 000117 */
	/* For loop */
	      /* assign initial value to loop index */
	 (X_is) =  1;
	g_ind0 = wb_dim ( SLIB_TM, 1);
	g_ima0 = g_ind0;	/* assign Max value to loop term variable */
 	g_ist0 = 1;
	g_retlab0 = 0;	/* assign return label number variable */
 ret_000:	;  /* loop top label */
/* 000118 */
	g_astr0 = (char *) (SLIB_TM->p) + (( (X_is)-1) * (SLIB_TM->len));
	g_ind0 = SLIB_TM->len;
	g_ind1 = ( (BREAKP_is) +  1);
	g_astr1 = (char *) (char *)(KEY_TS->p);
	g_astr1 += ( g_ind1-1);
	g_ind2 = min ((KEY_TS->len) - ( g_ind1-1),  (16));
	g_ind3 =  g_ind0 ;	 /* full length */
	g_astr2 = g_str0;
	g_ind4 = wb_cat (g_astr1,  g_ind2 , 20,
		g_astr0,  g_ind3 , 20, g_astr2);
	str_asgn (g_astr2, (long) g_ind4 , 
		WORK_ts, (long) 100 );
/* 000119 */
	INT_ISD0.p = (char *) INT_IS0;
	INT_IS0[0] = 2;
	INT_ISD1.p = (char *) INT_IS1;
	INT_IS1[0] = 22;
	Call_args = 5;
	PLOWALTS (ufbptr[2-1], &WORK_TS, &INT_ISD0, &INT_ISD1, &F2_IS);
/* 000120 */
	g_ind0 = ( (F2_is) -  0);
	g_ind1 = (g_ind0 > 0);
	if (g_ind1)
	  {
		goto lALL_DONE;
	  }
/* 000121 */
	g_ret = wb_nexti( (X_is), g_ima0, g_ist0);
	if (g_ret != 1)
	  {  /* return to loop top */
	     (X_is) =  (X_is) + g_ist0;
	    G_stack[++G_scnt] = g_retlab0;
	    goto Go_Sub_Ret;  /* use return stack */
	  }
	else
	  {  /* end of For loop : clear step */
	    g_ist0 = 0;
	  }
/* 000122 */
lL01380:	;
/* 000122 */
/* 000129 */
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	addl ('-', ALTKEYDESCR_ds, g_exp0, g_exp1);
	g_ind0 = cmp_NO(g_exp1,0);
	g_exp2[0] = 0x42170000L;
	g_exp2[1] = 0L;
	addl ('-', ALTKEYDESCR_ds, g_exp2, g_exp3);
	g_ind1 = cmp_NO(g_exp3,8);
	g_ind2 = (g_ind0 || g_ind1);
	if (g_ind2)
	  {
		goto lALL_DONE;
	  }
/* 000130 */
	g_ind0 =  ((KEY_is));
	g_ind1 = 1;
	g_ind2 = wb_key (2, ufbptr[ g_ind1-1],  g_ind0, &g_astr0);
	g_ind3 =  g_ind2 ;	 /* full length */
	g_ind4 = wb_len ((long)1, g_astr0,  g_ind3 );
	g_ind5 = ( g_ind4 -  (BREAKP_is));
	 (SUBLEN_is) =  g_ind5;
/* 000131 */
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	 (SEACHED_ONCE_is) =  g_ind0;
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind1);
	/* BCD temp will be reused */
	 (C_is) =  g_ind1;
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind2);
	/* BCD temp will be reused */
	 (ANYHITS_is) =  g_ind2;
/* 000132 */
	str_asgn (" ", (long) 1 , 
		TEXT_ts, (long) 50 );
	str_asgn (" ", (long) 1 , 
		SCREEN_tv, (long)(SCREEN_TV.tbyte));
/* 000133 */
	str_asgn ("Active P.F. Keys Are :", (long) 22 , 
		BLANKLINE_ts, (long) 79 );
/* 000134 */
	g_astr0 = (char *) (char *)(KEY_TS->p);
	g_ind0 = min ((KEY_TS->len),  ((BREAKP_is)));
	str_asgn (g_astr0, (long) g_ind0 , 
		TYPE_ts, (long) 16 );
/* 000135 */
	/* For loop */
	      /* assign initial value to loop index */
	g_exp0[0] = 0x41200000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	 (I_is) =  g_ind0;
	g_ima1 = (BREAKP_is);	/* assign Max value to loop term variable */
 	g_ist1 = 1;
	g_retlab1 = 1;	/* assign return label number variable */
 ret_001:	;  /* loop top label */
/* 000136 */
	g_astr0 = (char *) (char *)(KEY_TS->p);
	g_astr0 += ( (I_is)-1);
	g_ind0 = min ((KEY_TS->len) - ( (I_is)-1),  (1));
	g_ind1 = wb_str (g_astr0,  g_ind0 , "A",  1 );
	g_ind2 = (g_ind1 < 0);
	g_astr1 = (char *) (char *)(KEY_TS->p);
	g_astr1 += ( (I_is)-1);
	g_ind3 = min ((KEY_TS->len) - ( (I_is)-1),  (1));
	g_ind4 = wb_str (g_astr1,  g_ind3 , "Z",  1 );
	g_ind5 = (g_ind4 > 0);
	g_ind6 = (g_ind2 || g_ind5);
	if (g_ind6)
	  {
		goto lL01700;
	  }
/* 000137 */
	g_astr0 = (char *) TYPE_ts;
	g_astr0 += ( (I_is)-1);
	g_ind0 = min ( 16  - ( (I_is)-1),  (1));
	g_astr1 = (char *) (char *)(KEY_TS->p);
	g_astr1 += ( (I_is)-1);
	g_ind1 = min ((KEY_TS->len) - ( (I_is)-1),  (1));
	str_asgn (g_astr1, (long) g_ind1 ,
	g_astr0, (long) g_ind0 );
	g_ind2 = wb_or (g_astr0, (long) g_ind0 , " ", (long) 1 );
/* 000138 */
lL01700:	;
/* 000138 */
	g_ret = wb_nexti( (I_is), g_ima1, g_ist1);
	if (g_ret != 1)
	  {  /* return to loop top */
	     (I_is) =  (I_is) + g_ist1;
	    G_stack[++G_scnt] = g_retlab1;
	    goto Go_Sub_Ret;  /* use return stack */
	  }
	else
	  {  /* end of For loop : clear step */
	    g_ist1 = 0;
	  }
/* 000139 */
	g_astr0 = "(8)Search For ";
	g_ind0 = 14;
	g_astr1 = g_str0;
	g_ind1 = wb_cat (g_astr0,  g_ind0 , 20,
		TYPE_ts,  16 , 17, g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		SEARCH_ts, (long) 30 );
/* 000140 */
	g_astr0 = ":";
	g_ind0 = 1;
	g_astr1 = g_str0;
	g_ind1 = wb_cat (SEARCH_ts,  30 , 17,
		g_astr0,  g_ind0 , 20, g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		SEARCH_ts, (long) 30 );
/* 000141 */
	g_ind0 = wb_len ((long)0, SEARCH_ts,  30 );
	 (SLEN_is) =  g_ind0;
/* 000142 */
	str_asgn (" PF Key 16 may be used to end display and return with no selection.", (long) 67 , 
		BLINE_ts, (long) 79 );
/* 000144 */
	g_ind0 = wb_str (SLIB_TM->p, (SLIB_TM->tbyte), " ",  1 );
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL01960;
	  }
/* 000145 */
	g_astr0 = (char *) (char *)(KEY_TS->p);
	g_ind0 = min ((KEY_TS->len),  ((BREAKP_is)));
	g_astr1 = " Shown Below Are ";
	g_ind1 = 17;
	g_astr2 = g_str0;
	g_ind2 = wb_cat (g_astr1,  g_ind1 , 20,
		g_astr0,  g_ind0 , 20, g_astr2);
	str_asgn (g_astr2, (long) g_ind2 , 
		BLINE_ts, (long) 79 );
/* 000146 */
	g_ind0 = wb_dim ( SLIB_TM, 1);
	g_ind1 = ( g_ind0 -  1);
	g_ind2 = (g_ind1 == 0);
	if (g_ind2)
	  {
		g_exp0[0] = 0x41100000L;
		g_exp0[1] = 0L;
		BCD_l (g_exp0, &g_ind0);
		/* BCD temp will be reused */
		g_astr0 = (char *) (SLIB_TM->p) + (( g_ind0-1) * (SLIB_TM->len));
		g_ind1 = SLIB_TM->len;
		g_astr1 = "S Existing In Library:";
		g_ind2 = 22;
		g_astr2 = g_str0;
		g_ind3 = wb_cat (BLINE_ts,  79 , 17,
			g_astr1,  g_ind2 , 20, g_astr2);
		g_astr3 = g_str1;
		g_ind4 = wb_hex ("84", g_astr3);
		g_astr4 = g_str2;
		g_ind5 = wb_cat (g_astr2,  g_ind3 , 20,
			g_astr3,  g_ind4 , 19, g_astr4);
		g_astr5 = g_str3;
		g_ind6 = wb_cat (g_astr4,  g_ind5 , 20,
			g_astr0,  g_ind1 , 19, g_astr5);
		str_asgn (g_astr5, (long) g_ind6 , 
			BLINE_ts, (long) 79 );
	  }
	else
	  {
		g_astr0 = "S Accessible From Your Menu's";
		g_ind0 = 29;
		g_astr1 = g_str0;
		g_ind1 = wb_cat (BLINE_ts,  79 , 17,
			g_astr0,  g_ind0 , 20, g_astr1);
		str_asgn (g_astr1, (long) g_ind1 , 
			BLINE_ts, (long) 79 );
	  }
/* 000151 */
lL01960:	;
/* 000151 */
/* 000152 */
	str_asgn ((char *)(KEY_TS->p), (long)(KEY_TS->len), 
		READKEY_ts, (long) 99 );
/* 000153 */
	g_ind0 = ( (BREAKP_is) +  1);
	g_astr0 = (char *) READKEY_ts;
	g_astr0 += ( g_ind0-1);
	g_ind1 = min ( 99  - ( g_ind0-1),  ((SUBLEN_is)));
	g_ind2 = ( (BREAKP_is) +  1);
	g_astr1 = (char *) (char *)(KEY_TS->p);
	g_astr1 += ( g_ind2-1);
	g_ind3 = min ((KEY_TS->len) - ( g_ind2-1),  ((SUBLEN_is)));
	g_astr2 = g_str0;
	g_ind4 = wb_hex ("ff", g_astr2);
	g_astr3 = g_str1;
	g_ind5 = wb_all (g_astr2, g_astr3);
	str_asgn (g_astr1, (long) g_ind3 ,
	g_astr0, (long) g_ind1 );
	g_ind6 = wb_addc (g_astr0, (long) g_ind1 , g_astr3, (long) g_ind5 );
/* 000156 */
/* 000157 */
	g_f[0] = (long) STRSTRT;
	g_f[1] = (long) &(PRIMARY_TM);
	/* Make MIN/MAX Gmind = 0 */
	g_m[0] = (long) INTCONST;
	g_m[1] = (long) (SUBLEN_is);
	g_m[2] = (long) INTCONST;
	g_m[3] = 30L;
	g_m[4] = 0L;
	wb_min (0, 2, g_exp0);
	BCD_l (g_exp0, &g_ind0);
	g_f[2] = (long) INTCONST;
	g_f[3] = (long) g_ind0;
	g_f[4] = (long) INTCONST;
	g_f[5] = 1L;
	g_f[6] = (long) BCDCONST;
	g_f[7] = 0x41100000;
	g_f[8] = 0x0;
	g_f[9] = 0L;
	wb_redim (1);
/* 000158 */
	g_ind0 = ( (BREAKP_is) +  1);
	g_astr0 = (char *) (char *)(KEY_TS->p);
	g_astr0 += ( g_ind0-1);
	g_ind1 = ((KEY_TS->len) - ( g_ind0-1));
	str_asgn (g_astr0, (long) g_ind1 , 
		PRIMARY_tm, (long)(PRIMARY_TM.tbyte));
/* 000159 */
	g_ind0 = wb_str (TYPE_ts,  16 , "Library",  7 );
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		g_f[0] = (long) STRSTRT;
		g_f[1] = (long) &(PRIMARY_TM);
		g_f[2] = (long) INTCONST;
		g_f[3] = 6L;
		g_f[4] = (long) INTCONST;
		g_f[5] = 1L;
		g_f[6] = (long) INTCONST;
		g_f[7] = 1L;
		g_f[8] = 0L;
		wb_redim (1);
	  }
/* 000161 */
lL02160:	;
/* 000161 */
/* 000163 */
lPLOW_CODES:	;
/* 000164 */
	Call_args = 5;
	PLOWALTS (ufbptr[1-1], &READKEY_TS, &KEY_IS, &BREAKP_IS, F1_IS);
/* 000165 */
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	l_BCD (*(F1_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,5);
	if (g_ind0)
	  {
		goto lSHOW_CODES;
	  }
/* 000167 */
	G_chan = 1 - 1;
	g_ind0 = 882;
	g_f[0] = (long) POS;
	g_f[1] = 1L;
	g_f[2] = (long) g_ind0;
	g_ind1 = 5;
	g_f[3] = (long) CH;
	g_f[4] = 1L;
	g_f[5] = (long) g_ind1;
	g_ind2 = 936;
	g_f[6] = (long) POS;
	g_f[7] = 1L;
	g_f[8] = (long) g_ind2;
	g_ind3 = 1;
	g_f[9] = (long) CH;
	g_f[10] = 1L;
	g_f[11] = (long) g_ind3;
	g_f[12] = (long) STRADR;
	g_f[13] = (long) (PGMSEC_ts);
	g_f[14] = (long)  5 ;
	g_f[15] = (long) STRADR;
	g_f[16] = (long) (OBS_ts);
	g_f[17] = (long)  1 ;
	g_f[18] = 0L;
	/*GETT,UFBPTR[CH],HOLD,KEYIND,KEYREL,RELF,RECF,PIC,CNT*/
	g_ret = wb_rdgt (GET, ufbptr[G_chan], 0, -1,
	0, 0,  0, 4, 2);
	if (g_ret == DATAX)
	  wb_iserr ("DATA", ufbptr[G_chan]->prname);
	else if (g_ret == EODX)
	  wb_iserr ("EOD", ufbptr[G_chan]->prname);
	else if (g_ret == TIMEOX)
	  wb_iserr ("TIMEOUT", ufbptr[G_chan]->prname);
	else if (g_ret == IOERRX)
	  wb_iserr ("IOERR", ufbptr[G_chan]->prname);
/* 000168 */
	g_ind0 = wb_str (OBS_ts,  1 , "Y",  1 );
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lPLOW_CODES;
	  }
/* 000169 */
	g_ind0 = wb_bool (11, PGMSEC_ts, (long) 5 , (char *)(SEC_TS->p), (long)(SEC_TS->len));
/* 000170 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("ff", g_astr0);
	/*LOC = wb_pos(REV,OP,STR1,LEN1,STR2,LEN2);*/
	/*OP:<11,==16,>13,>=19,<=18,<>17,- 7*/
	g_ind1 = wb_pos (0, 17, PGMSEC_ts, (long) 5 , g_astr0, (long) g_ind0 );
	g_ind2 = ( g_ind1 -  0);
	g_ind3 = (g_ind2 > 0);
	if (g_ind3)
	  {
		goto lPLOW_CODES;
	  }
/* 000171 */
	g_ind0 = wb_str (SLIB_TM->p, (SLIB_TM->tbyte), " ",  1 );
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL02800;
	  }
/* 000172 */
/* 000173 */
	g_ind0 = ( (BREAKP_is) +  1);
	g_astr0 = (char *) READKEY_ts;
	g_astr0 += ( g_ind0-1);
	g_ind1 = min ( 99  - ( g_ind0-1),  (16));
	str_asgn (g_astr0, (long) g_ind1 , 
		WORK_ts, (long) 100 );
/* 000174 */
	 (Z_is) =  0;
/* 000175 */
lL02440:	;
/* 000175 */
	INT_ISD0.p = (char *) INT_IS0;
	INT_IS0[0] = 1;
	INT_ISD1.p = (char *) INT_IS1;
	INT_IS1[0] = 16;
	Call_args = 5;
	PLOWALTS (ufbptr[2-1], &WORK_TS, &INT_ISD0, &INT_ISD1, &F2_IS);
/* 000176 */
	g_ind0 = ( (F2_is) -  0);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lPLOW_PARENTS;
	  }
/* 000177 */
	 (Z_is) =  1;
/* 000177 */
	g_ind0 = 0;
	g_ind1 = 2;
	g_ind2 = wb_key (2, ufbptr[ g_ind1-1],  g_ind0, &g_astr0);
	g_astr0 += ( 29-1);
	g_ind3 = min ( g_ind2  - ( 29-1),  (3));
	str_asgn (g_astr0, (long) g_ind3 , 
		SEQNR_ts, (long) 3 );
/* 000178 */
	g_astr0 = (char *) WORK_ts;
	g_astr0 += ( 23-1);
	g_ind0 = min ( 100  - ( 23-1),  (16));
	STR_TSD0.p = g_astr0;
	STR_TSD0.len = g_ind0;
	STR_TSD0.tbyte = g_ind0;
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind1);
	/* BCD temp will be reused */
	g_aind0 = &F1_iv[ g_ind1-1];
	INT_ISD0.p = (char *) g_aind0;
	Call_args = 3;
	READ100 (ufbptr[1-1], &STR_TSD0, &INT_ISD0);
/* 000179 */
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	g_aind0 = &F1_iv[ g_ind0-1];
	g_ind1 = (*(g_aind0) -  0);
	g_ind2 = (g_ind1 == 0);
	if (g_ind2)
	  {
		goto lL02560;
	  }
/* 000180 */
	g_ind0 = 1;
	g_ind1 = 1;
	g_ind2 = wb_key (2, ufbptr[ g_ind1-1],  g_ind0, &g_astr0);
	g_ind3 = min ( g_ind2 ,  (8));
	g_ind4 = wb_str (g_astr0,  g_ind3 , "MENU",  4 );
	g_ind5 = (g_ind4 != 0);
	if (g_ind5)
	  {
		goto lL02440;
	  }
/* 000181 */
lL02560:	;
/* 000181 */
	g_astr0 = (char *) WORK_ts;
	g_astr0 += ( 39-1);
	g_ind0 = min ( 100  - ( 39-1),  (6));
	wb_srch ( 0, 6, SLIB_TM->p, (SLIB_TM->tbyte), g_astr0,  g_ind0 , 1, 4, P_iv, 6, 2);
/* 000182 */
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	g_aind0 = &P_iv[ g_ind0-1];
	g_ind1 = (*(g_aind0) -  0);
	g_ind2 = (g_ind1 == 0);
	if (g_ind2)
	  {
		goto lL02440;
	  }
/* 000183 */
	g_exp0[0] = 0x41200000;
	g_exp0[1] = 0;
	BCD_l (g_exp0, &g_ind0);
	/* Exp variable was tmp & will be reused */
	g_ind1 = 2;
	g_ind2 = wb_key (2, ufbptr[ g_ind1-1],  g_ind0, &g_astr0);
	str_asgn (g_astr0, (long) g_ind2 , 
		PLOWKEY_ts, (long) 99 );
/* 000184 */
	g_astr0 = (char *) PLOWKEY_ts;
	g_astr0 += ( 39-1);
	g_ind0 = min ( 99  - ( 39-1),  (6));
	str_asgn (g_astr0, (long) g_ind0 , 
		LAST_VERSION_ts, (long) 6 );
/* 000185 */
lL02640:	;
/* 000185 */
	INT_ISD0.p = (char *) INT_IS0;
	INT_IS0[0] = 2;
	INT_ISD1.p = (char *) INT_IS1;
	INT_IS1[0] = 38;
	g_exp0[0] = 0x41200000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind2);
	/* BCD temp will be reused */
	g_aind0 = &F1_iv[ g_ind2-1];
	INT_ISD2.p = (char *) g_aind0;
	Call_args = 5;
	PLOWALTS (ufbptr[2-1], &PLOWKEY_TS, &INT_ISD0, &INT_ISD1, &INT_ISD2);
/* 000186 */
	g_exp0[0] = 0x41200000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	g_aind0 = &F1_iv[ g_ind0-1];
	g_ind1 = (*(g_aind0) -  0);
	g_ind2 = (g_ind1 == 0);
	if (g_ind2)
	  {
		goto lL02742;
	  }
/* 000187 */
	g_astr0 = (char *) PLOWKEY_ts;
	g_astr0 += ( 39-1);
	g_ind0 = min ( 99  - ( 39-1),  (6));
	str_asgn (g_astr0, (long) g_ind0 , 
		LAST_VERSION_ts, (long) 6 );
/* 000188 */
	goto lL02640;
/* 000189 */
lL02742:	;
/* 000189 */
	g_astr0 = (char *) WORK_ts;
	g_ind0 = min ( 100 ,  (16));
	g_astr1 = "      ";
	g_ind1 = 6;
	g_astr2 = g_str0;
	g_ind2 = wb_cat (g_astr0,  g_ind0 , 20,
		g_astr1,  g_ind1 , 20, g_astr2);
	g_astr3 = (char *) PLOWKEY_ts;
	g_ind3 = min ( 99 ,  (16));
	g_astr4 = g_str1;
	g_ind4 = wb_cat (g_astr2,  g_ind2 , 20,
		g_astr3,  g_ind3 , 20, g_astr4);
	g_astr5 = (char *) PLOWKEY_ts;
	g_astr5 += ( 17-1);
	g_ind5 = min ( 99  - ( 17-1),  (6));
	g_astr6 = g_str2;
	g_ind6 = wb_cat (g_astr4,  g_ind4 , 20,
		g_astr5,  g_ind5 , 20, g_astr6);
	g_astr7 = (char *) PLOWKEY_ts;
	g_ind7 = min ( 99 ,  (16));
	g_astr8 = g_str3;
	g_ind8 = wb_cat (g_astr6,  g_ind6 , 20,
		g_astr7,  g_ind7 , 20, g_astr8);
	g_astr9 = (char *) LAST_VERSION_ts;
	g_ind9 = min ( 6 ,  (6));
	g_astr10 = g_str4;
	g_ind10 = wb_cat (g_astr8,  g_ind8 , 20,
		g_astr9,  g_ind9 , 20, g_astr10);
	g_astr11 = (char *) PLOWKEY_ts;
	g_astr11 += ( 17-1);
	g_ind11 = min ( 99  - ( 17-1),  (6));
	g_astr12 = g_str5;
	g_ind12 = wb_cat (g_astr10,  g_ind10 , 20,
		g_astr11,  g_ind11 , 20, g_astr12);
	g_astr13 = g_str6;
	g_ind13 = wb_hex ("000000", g_astr13);
	g_astr14 = g_str7;
	g_ind14 = wb_cat (g_astr12,  g_ind12 , 20,
		g_astr13,  g_ind13 , 19, g_astr14);
	str_asgn (g_astr14, (long) g_ind14 , 
		PLOWKEY_ts, (long) 99 );
/* 000193 */
	INT_ISD0.p = (char *) INT_IS0;
	INT_IS0[0] = 1;
	INT_ISD1.p = (char *) INT_IS1;
	INT_IS1[0] = 72;
	g_exp0[0] = 0x41200000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind2);
	/* BCD temp will be reused */
	g_aind0 = &F1_iv[ g_ind2-1];
	INT_ISD2.p = (char *) g_aind0;
	Call_args = 5;
	PLOWALTS (ufbptr[2-1], &PLOWKEY_TS, &INT_ISD0, &INT_ISD1, &INT_ISD2);
/* 000194 */
	g_exp0[0] = 0x41200000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	g_aind0 = &F1_iv[ g_ind0-1];
	g_ind1 = (*(g_aind0) -  0);
	g_ind2 = (g_ind1 == 0);
	if (g_ind2)
	  {
		goto lL02440;
	  }
/* 000195 */
lL02800:	;
/* 000195 */
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	l_BCD ( (C_is), g_exp1);
	addl ('+', g_exp1, g_exp0, g_exp2);
	BCD_l (g_exp2, &g_ind0);
	 (C_is) =  g_ind0;
/* 000196 */
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	 (ANYHITS_is) =  g_ind0;
/* 000197 */
	G_stack[++G_scnt] = 2;
	goto lDESCRIBE_IT;
ret_002:	;
/* 000198 */
	g_astr0 = (char *) (SCREEN_tv) + (( (C_is)-1) * SCREEN_TV.len);
	g_ind0 = SCREEN_TV.len;
	g_ind1 = ( (BREAKP_is) +  1);
	g_astr1 = (char *) READKEY_ts;
	g_astr1 += ( g_ind1-1);
	g_ind2 = min ( 99  - ( g_ind1-1),  ((SUBLEN_is)));
	g_astr2 = "   ";
	g_ind3 = 3;
	g_astr3 = g_str0;
	g_ind4 = wb_cat (g_astr1,  g_ind2 , 20,
		g_astr2,  g_ind3 , 20, g_astr3);
	g_astr4 = g_str1;
	g_ind5 = wb_cat (g_astr3,  g_ind4 , 20,
		TEXT_ts,  50 , 17, g_astr4);
	str_asgn (g_astr4, (long) g_ind5 , 
		g_astr0, (long) g_ind0 );
/* 000199 */
	g_exp0[0] = 0x42160000L;
	g_exp0[1] = 0L;
	l_BCD ( (C_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,5);
	if (g_ind0)
	  {
		goto lSHOW_CODES;
	  }
/* 000200 */
	goto lPLOW_CODES;
/* 000202 */
lPLOW_PARENTS:	;
/* 000203 */
	switch ( (Z_is))
	  {
		case 1: 
		  goto lPLOW_CODES;
		  break;
		default:
		  break;
	  }
/* 000204 */
	g_ind0 = 1;
	g_ind1 = 0;
	g_ind2 = wb_key (1, ufbptr[ g_ind0-1],  g_ind1, &g_astr0);
	g_astr1 = (char *) READKEY_ts;
	g_astr1 += ( 9-1);
	g_ind3 = ( 99  - ( 9-1));
	g_ind4 = wb_str (g_astr0,  g_ind2 , g_astr1,  g_ind3 );
	g_ind5 = (g_ind4 != 0);
	if (g_ind5)
	  {
		g_astr0 = (char *) READKEY_ts;
		g_astr0 += ( 9-1);
		g_ind0 = ( 99  - ( 9-1));
		STR_TSD0.p = g_astr0;
		STR_TSD0.len = g_ind0;
		STR_TSD0.tbyte = g_ind0;
		g_exp0[0] = 0x41100000L;
		g_exp0[1] = 0L;
		BCD_l (g_exp0, &g_ind1);
		/* BCD temp will be reused */
		g_aind0 = &F1_iv[ g_ind1-1];
		INT_ISD0.p = (char *) g_aind0;
		Call_args = 3;
		READ100 (ufbptr[1-1], &STR_TSD0, &INT_ISD0);
	  }
/* 000207 */
	g_ind0 = 1;
	g_ind1 = 1;
	g_ind2 = wb_key (2, ufbptr[ g_ind1-1],  g_ind0, &g_astr0);
	g_ind3 = min ( g_ind2 ,  (8));
	g_ind4 = wb_str (g_astr0,  g_ind3 , "MENU",  4 );
	g_ind5 = (g_ind4 != 0);
	if (g_ind5)
	  {
		goto lPLOW_CODES;
	  }
/* 000208 */
	g_ind0 = ( (BREAKP_is) +  1);
	g_astr0 = (char *) READKEY_ts;
	g_astr0 += ( g_ind0-1);
	g_ind1 = min ( 99  - ( g_ind0-1),  ((SUBLEN_is)));
	str_asgn (g_astr0, (long) g_ind1 , 
		PLOWKEY_ts, (long) 99 );
/* 000209 */
lL03060:	;
/* 000209 */
	INT_ISD0.p = (char *) INT_IS0;
	INT_IS0[0] = 2;
	INT_ISD1.p = (char *) INT_IS1;
	INT_IS1[0] = 16;
	g_exp0[0] = 0x41200000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind2);
	/* BCD temp will be reused */
	g_aind0 = &F1_iv[ g_ind2-1];
	INT_ISD2.p = (char *) g_aind0;
	Call_args = 5;
	PLOWALTS (ufbptr[2-1], &PLOWKEY_TS, &INT_ISD0, &INT_ISD1, &INT_ISD2);
/* 000210 */
	g_exp0[0] = 0x41200000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	g_aind0 = &F1_iv[ g_ind0-1];
	g_ind1 = (*(g_aind0) -  0);
	g_ind2 = (g_ind1 == 0);
	if (g_ind2)
	  {
		goto lPLOW_CODES;
	  }
/* 000211 */
	g_astr0 = (char *) PLOWKEY_ts;
	g_astr0 += ( 17-1);
	g_ind0 = min ( 99  - ( 17-1),  (6));
	wb_srch ( 0, 6, SLIB_TM->p, (SLIB_TM->tbyte), g_astr0,  g_ind0 , 1, 4, P_iv, 6, 2);
/* 000212 */
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	g_aind0 = &P_iv[ g_ind0-1];
	g_ind1 = (*(g_aind0) -  0);
	g_ind2 = (g_ind1 == 0);
	if (g_ind2)
	  {
		goto lL03060;
	  }
/* 000213 */
	goto lL02800;
/* 000215 */
lDESCRIBE_IT:	;
/* 000216 */
	g_astr0 = (char *) READKEY_ts;
	g_astr0 += ( 9-1);
	g_ind0 = ( 99  - ( 9-1));
	STR_TSD0.p = g_astr0;
	STR_TSD0.len = g_ind0;
	STR_TSD0.tbyte = g_ind0;
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind1);
	/* BCD temp will be reused */
	g_aind0 = &F1_iv[ g_ind1-1];
	INT_ISD0.p = (char *) g_aind0;
	Call_args = 3;
	READ100 (ufbptr[1-1], &STR_TSD0, &INT_ISD0);
/* 000217 */
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	g_aind0 = &F1_iv[ g_ind0-1];
	g_ind1 = (*(g_aind0) -  0);
	g_ind2 = (g_ind1 == 0);
	if (g_ind2)
	  {
		goto Go_Sub_Ret;
	  }
/* 000218 */
	G_chan = 1 - 1;
	g_f[0] = (long) STRADR;
	g_f[1] = (long) (WORK_ts);
	g_f[2] = (long)  100 ;
	g_f[3] = 0L;
	/*GETT,UFBPTR[CH],HOLD,KEYIND,KEYREL,RELF,RECF,PIC,CNT*/
	g_ret = wb_rdgt (GET, ufbptr[G_chan], 0, -1,
	0, 0,  0, 0, 1);
	if (g_ret == DATAX)
	  wb_iserr ("DATA", ufbptr[G_chan]->prname);
	else if (g_ret == EODX)
	  wb_iserr ("EOD", ufbptr[G_chan]->prname);
	else if (g_ret == TIMEOX)
	  wb_iserr ("TIMEOUT", ufbptr[G_chan]->prname);
	else if (g_ret == IOERRX)
	  wb_iserr ("IOERR", ufbptr[G_chan]->prname);
/* 000219 */
	g_astr0 = (char *) WORK_ts;
	g_astr0 += ( (DISP_is)-1);
	g_ind0 = min ( 100  - ( (DISP_is)-1),  (30));
	str_asgn (g_astr0, (long) g_ind0 , 
		TEXT_ts, (long) 50 );
/* 000220 */
	g_ind0 = wb_str (FILE_ts,  8 , "INTDOC01",  8 );
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		g_astr0 = (char *) WORK_ts;
		g_astr0 += ( (DISP_is)-1);
		g_ind0 = min ( 100  - ( (DISP_is)-1),  (50));
		str_asgn (g_astr0, (long) g_ind0 , 
			TEXT_ts, (long) 50 );
	  }
/* 000224 */
/* 000226 */
	goto Go_Sub_Ret;
/* 000228 */
lSHOW_CODES:	;
/* 000229 */
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	 (X_is) =  g_ind0;
/* 000230 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("0b", g_astr0);
	g_astr1 = g_str1;
	g_ind1 = wb_all (g_astr0, g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		FLAG_tv, (long)(FLAG_TV.tbyte));
/* 000231 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("9c", g_astr0);
	wb_vinit (g_astr0, &PFAC_TS, &FAC_TV, NULL);
/* 000232 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("ac", g_astr0);
	str_asgn (g_astr0, (long) g_ind0 , 
		BFAC_ts, (long) 1 );
/* 000233 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("000205080f2010", g_astr0);
	str_asgn (g_astr0, (long) g_ind0 , 
		KEYS_ts, (long) 16 );
/* 000234 */
	str_asgn ("Active P.F. Keys Are :", (long) 22 , 
		BLANKLINE_ts, (long) 79 );
/* 000235 */
	g_ind0 = ( (C_is) +  (ANYHITS_is));
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	l_BCD (g_ind0, g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind1 = cmp_NO(g_exp2,2);
	if (g_ind1)
	  {
		goto lL03720;
	  }
/* 000236 */
/* 000237 */
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	l_BCD ( (SEACHED_ONCE_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,5);
	if (g_ind0)
	  {
		goto lALL_DONE;
	  }
/* 000238 */
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	 (SEACHED_ONCE_is) =  g_ind0;
/* 000239 */
	g_ind0 = ( (BREAKP_is) +  1);
	g_astr0 = (char *) (char *)(KEY_TS->p);
	g_astr0 += ( g_ind0-1);
	g_ind1 = ((KEY_TS->len) - ( g_ind0-1));
	g_astr1 = g_str0;
	g_ind2 = wb_hex ("20", g_astr1);
	/*LOC = wb_pos(REV,OP,STR1,LEN1,STR2,LEN2);*/
	/*OP:<11,==16,>13,>=19,<=18,<>17,- 7*/
	g_ind3 = wb_pos (0, 13, g_astr0, (long) g_ind1 , g_astr1, (long) g_ind2 );
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	l_BCD (g_ind3, g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind4 = cmp_NO(g_exp2,6);
	if (g_ind4)
	  {
		goto lL05460;
	  }
/* 000240 */
/* 000240 */
	goto lALL_DONE;
/* 000242 */
lL03720:	;
/* 000242 */
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	l_BCD ( (C_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,5);
	if (g_ind0)
	  {
		g_astr0 = (char *) (SCREEN_tv) + (( 8-1) * SCREEN_TV.len);
		g_ind0 = SCREEN_TV.len;
		g_astr0 += ( 27-1);
		g_ind1 = ( g_ind0  - ( 27-1));
		str_asgn ("****  END OF FILE ****", (long) 22 , 
			g_astr0, (long) g_ind1 );
	  }
	else
	  {
		g_astr0 = (char *) FAC_tv;
		g_ind0 = min ((FAC_TV.tbyte),  ((C_is)));
		g_astr1 = g_str0;
		g_ind1 = wb_hex ("86", g_astr1);
		g_astr2 = g_str1;
		g_ind2 = wb_all (g_astr1, g_astr2);
		str_asgn (g_astr2, (long) g_ind2 , 
			g_astr0, (long) g_ind0 );
	  }
/* 000245 */
/* 000246 */
	g_astr0 = "  ";
	g_ind0 = 2;
	g_astr1 = g_str0;
	g_ind1 = wb_cat (g_astr0,  g_ind0 , 20,
		TYPE_ts,  16 , 17, g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		TITLE_ts, (long) 79 );
/* 000247 */
	g_astr0 = " Name";
	g_ind0 = 5;
	g_astr1 = g_str0;
	g_ind1 = wb_cat (TITLE_ts,  79 , 17,
		g_astr0,  g_ind0 , 20, g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		TITLE_ts, (long) 79 );
/* 000248 */
	g_ind0 = ( 6 +  (SUBLEN_is));
	g_astr0 = (char *) TITLE_ts;
	g_astr0 += ( g_ind0-1);
	g_ind1 = ( 79  - ( g_ind0-1));
	str_asgn (TYPE_ts, (long) 16 , 
		g_astr0, (long) g_ind1 );
/* 000249 */
	g_astr0 = " Description";
	g_ind0 = 12;
	g_astr1 = g_str0;
	g_ind1 = wb_cat (TITLE_ts,  79 , 17,
		g_astr0,  g_ind0 , 20, g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		TITLE_ts, (long) 79 );
/* 000251 */
/* 000252 */
	g_astr0 = (char *) TITLE_ts;
	g_astr0 += ( 66-1);
	g_ind0 = ( 79  - ( 66-1));
	g_astr1 = "FILE: ";
	g_ind1 = 6;
	g_astr2 = g_str0;
	g_ind2 = wb_cat (g_astr1,  g_ind1 , 20,
		FILE_ts,  8 , 17, g_astr2);
	str_asgn (g_astr2, (long) g_ind2 , 
		g_astr0, (long) g_ind0 );
/* 000254 */
lL03960:	;
/* 000254 */
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	g_astr0 = (char *) (FLAG_tv) + (( g_ind0-1) * FLAG_TV.len);
	g_ind1 = FLAG_TV.len;
	g_astr1 = g_str0;
	g_ind2 = wb_hex ("0b", g_astr1);
	str_asgn (g_astr1, (long) g_ind2 , 
		g_astr0, (long) g_ind1 );
/* 000255 */
	g_ind0 = 2;
	g_ind1 = 1;
	g_f[0] = (long) AT;
	g_f[1] = 2;
	g_f[2] = (long) g_ind1;
	g_f[3] = (long) g_ind0;
	g_f[4] = (long) STRCONST;
	g_f[5] = (long) "Postion cursor (Tab) to line and press (RETURN) to return with that code";
	g_f[6] = (long)  72 ;
	g_ind0 = 2;
	g_ind1 = 2;
	g_f[7] = (long) AT;
	g_f[8] = 2;
	g_f[9] = (long) g_ind1;
	g_f[10] = (long) g_ind0;
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("8c", g_astr0);
	/* skip FAC operation */
	g_f[11] = (long) FAC;
	g_f[12] = (long) g_astr0;
	g_f[13] = (long)  g_ind0 ;
	g_f[14] = (long) STRADR;
	g_f[15] = (long) BLINE_ts;
	g_f[16] = (long)  79 ;
	g_ind0 = 2;
	g_ind1 = 3;
	g_f[17] = (long) AT;
	g_f[18] = 2;
	g_f[19] = (long) g_ind1;
	g_f[20] = (long) g_ind0;
	g_astr1 = g_str1;
	g_ind0 = wb_hex ("ac", g_astr1);
	/* skip FAC operation */
	g_f[21] = (long) FAC;
	g_f[22] = (long) g_astr1;
	g_f[23] = (long)  g_ind0 ;
	g_f[24] = (long) STRADR;
	g_f[25] = (long) TITLE_ts;
	g_f[26] = (long)  79 ;
	g_ind0 = 79;
	g_f[27] = (long) CH;
	g_f[28] = 1;
	g_f[29] = (long) g_ind0;
	g_ind0 = 2;
	g_ind1 = 5;
	g_f[30] = (long) AT;
	g_f[31] = 2;
	g_f[32] = (long) g_ind1;
	g_f[33] = (long) g_ind0;
	g_astr2 = g_str2;
	g_ind0 = wb_hex ("82", g_astr2);
	/* skip FAC operation */
	g_f[34] = (long) FAC;
	g_f[35] = (long) g_astr2;
	g_f[36] = (long)  g_ind0 ;
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	g_astr3 = (char *) (FLAG_tv) + (( g_ind0-1) * FLAG_TV.len);
	g_ind1 = FLAG_TV.len;
	g_f[37] = (long) STRADR;
	g_f[38] = (long) g_astr3;
	g_f[39] = (long)  g_ind1 ;
	g_ind1 = 1;
	g_f[40] = (long) CH;
	g_f[41] = 1;
	g_f[42] = (long) g_ind1;
	g_ind1 = 2;
	g_ind2 = 5;
	g_f[43] = (long) AT;
	g_f[44] = 2;
	g_f[45] = (long) g_ind2;
	g_f[46] = (long) g_ind1;
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind1);
	/* BCD temp will be reused */
	g_astr4 = (char *) (FAC_tv) + (( g_ind1-1) * FAC_TV.len);
	g_ind2 = FAC_TV.len;
	/* skip FAC operation */
	g_f[47] = (long) FAC;
	g_f[48] = (long) g_astr4;
	g_f[49] = (long)  g_ind2 ;
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind2);
	/* BCD temp will be reused */
	g_astr5 = (char *) (FLAG_tv) + (( g_ind2-1) * FLAG_TV.len);
	g_ind3 = FLAG_TV.len;
	g_f[50] = (long) STRADR;
	g_f[51] = (long) g_astr5;
	g_f[52] = (long)  g_ind3 ;
	g_ind3 = 1;
	g_f[53] = (long) CH;
	g_f[54] = 1;
	g_f[55] = (long) g_ind3;
	g_ind3 = 2;
	g_ind4 = 6;
	g_f[56] = (long) AT;
	g_f[57] = 2;
	g_f[58] = (long) g_ind4;
	g_f[59] = (long) g_ind3;
	g_exp0[0] = 0x41200000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind3);
	/* BCD temp will be reused */
	g_astr6 = (char *) (FAC_tv) + (( g_ind3-1) * FAC_TV.len);
	g_ind4 = FAC_TV.len;
	/* skip FAC operation */
	g_f[60] = (long) FAC;
	g_f[61] = (long) g_astr6;
	g_f[62] = (long)  g_ind4 ;
	g_exp0[0] = 0x41200000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind4);
	/* BCD temp will be reused */
	g_astr7 = (char *) (FLAG_tv) + (( g_ind4-1) * FLAG_TV.len);
	g_ind5 = FLAG_TV.len;
	g_f[63] = (long) STRADR;
	g_f[64] = (long) g_astr7;
	g_f[65] = (long)  g_ind5 ;
	g_ind5 = 1;
	g_f[66] = (long) CH;
	g_f[67] = 1;
	g_f[68] = (long) g_ind5;
	g_ind5 = 2;
	g_ind6 = 7;
	g_f[69] = (long) AT;
	g_f[70] = 2;
	g_f[71] = (long) g_ind6;
	g_f[72] = (long) g_ind5;
	g_exp0[0] = 0x41300000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind5);
	/* BCD temp will be reused */
	g_astr8 = (char *) (FAC_tv) + (( g_ind5-1) * FAC_TV.len);
	g_ind6 = FAC_TV.len;
	/* skip FAC operation */
	g_f[73] = (long) FAC;
	g_f[74] = (long) g_astr8;
	g_f[75] = (long)  g_ind6 ;
	g_exp0[0] = 0x41300000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind6);
	/* BCD temp will be reused */
	g_astr9 = (char *) (FLAG_tv) + (( g_ind6-1) * FLAG_TV.len);
	g_ind7 = FLAG_TV.len;
	g_f[76] = (long) STRADR;
	g_f[77] = (long) g_astr9;
	g_f[78] = (long)  g_ind7 ;
	g_ind7 = 1;
	g_f[79] = (long) CH;
	g_f[80] = 1;
	g_f[81] = (long) g_ind7;
	g_ind7 = 2;
	g_ind8 = 8;
	g_f[82] = (long) AT;
	g_f[83] = 2;
	g_f[84] = (long) g_ind8;
	g_f[85] = (long) g_ind7;
	g_exp0[0] = 0x41400000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind7);
	/* BCD temp will be reused */
	g_astr10 = (char *) (FAC_tv) + (( g_ind7-1) * FAC_TV.len);
	g_ind8 = FAC_TV.len;
	/* skip FAC operation */
	g_f[86] = (long) FAC;
	g_f[87] = (long) g_astr10;
	g_f[88] = (long)  g_ind8 ;
	g_exp0[0] = 0x41400000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind8);
	/* BCD temp will be reused */
	g_astr11 = (char *) (FLAG_tv) + (( g_ind8-1) * FLAG_TV.len);
	g_ind9 = FLAG_TV.len;
	g_f[89] = (long) STRADR;
	g_f[90] = (long) g_astr11;
	g_f[91] = (long)  g_ind9 ;
	g_ind9 = 1;
	g_f[92] = (long) CH;
	g_f[93] = 1;
	g_f[94] = (long) g_ind9;
	g_ind9 = 2;
	g_ind10 = 9;
	g_f[95] = (long) AT;
	g_f[96] = 2;
	g_f[97] = (long) g_ind10;
	g_f[98] = (long) g_ind9;
	g_exp0[0] = 0x41500000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind9);
	/* BCD temp will be reused */
	g_astr12 = (char *) (FAC_tv) + (( g_ind9-1) * FAC_TV.len);
	g_ind10 = FAC_TV.len;
	/* skip FAC operation */
	g_f[99] = (long) FAC;
	g_f[100] = (long) g_astr12;
	g_f[101] = (long)  g_ind10 ;
	g_exp0[0] = 0x41500000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind10);
	/* BCD temp will be reused */
	g_astr13 = (char *) (FLAG_tv) + (( g_ind10-1) * FLAG_TV.len);
	g_ind11 = FLAG_TV.len;
	g_f[102] = (long) STRADR;
	g_f[103] = (long) g_astr13;
	g_f[104] = (long)  g_ind11 ;
	g_ind11 = 1;
	g_f[105] = (long) CH;
	g_f[106] = 1;
	g_f[107] = (long) g_ind11;
	g_ind11 = 2;
	g_ind12 = 10;
	g_f[108] = (long) AT;
	g_f[109] = 2;
	g_f[110] = (long) g_ind12;
	g_f[111] = (long) g_ind11;
	g_exp0[0] = 0x41600000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind11);
	/* BCD temp will be reused */
	g_astr14 = (char *) (FAC_tv) + (( g_ind11-1) * FAC_TV.len);
	g_ind12 = FAC_TV.len;
	/* skip FAC operation */
	g_f[112] = (long) FAC;
	g_f[113] = (long) g_astr14;
	g_f[114] = (long)  g_ind12 ;
	g_exp0[0] = 0x41600000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind12);
	/* BCD temp will be reused */
	g_astr15 = (char *) (FLAG_tv) + (( g_ind12-1) * FLAG_TV.len);
	g_ind13 = FLAG_TV.len;
	g_f[115] = (long) STRADR;
	g_f[116] = (long) g_astr15;
	g_f[117] = (long)  g_ind13 ;
	g_ind13 = 1;
	g_f[118] = (long) CH;
	g_f[119] = 1;
	g_f[120] = (long) g_ind13;
	g_ind13 = 2;
	g_ind14 = 11;
	g_f[121] = (long) AT;
	g_f[122] = 2;
	g_f[123] = (long) g_ind14;
	g_f[124] = (long) g_ind13;
	g_exp0[0] = 0x41700000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind13);
	/* BCD temp will be reused */
	g_astr16 = (char *) (FAC_tv) + (( g_ind13-1) * FAC_TV.len);
	g_ind14 = FAC_TV.len;
	/* skip FAC operation */
	g_f[125] = (long) FAC;
	g_f[126] = (long) g_astr16;
	g_f[127] = (long)  g_ind14 ;
	g_exp0[0] = 0x41700000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind14);
	/* BCD temp will be reused */
	g_astr17 = (char *) (FLAG_tv) + (( g_ind14-1) * FLAG_TV.len);
	g_ind15 = FLAG_TV.len;
	g_f[128] = (long) STRADR;
	g_f[129] = (long) g_astr17;
	g_f[130] = (long)  g_ind15 ;
	g_ind15 = 1;
	g_f[131] = (long) CH;
	g_f[132] = 1;
	g_f[133] = (long) g_ind15;
	g_ind15 = 2;
	g_ind16 = 12;
	g_f[134] = (long) AT;
	g_f[135] = 2;
	g_f[136] = (long) g_ind16;
	g_f[137] = (long) g_ind15;
	g_exp0[0] = 0x41800000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind15);
	/* BCD temp will be reused */
	g_astr18 = (char *) (FAC_tv) + (( g_ind15-1) * FAC_TV.len);
	g_ind16 = FAC_TV.len;
	/* skip FAC operation */
	g_f[138] = (long) FAC;
	g_f[139] = (long) g_astr18;
	g_f[140] = (long)  g_ind16 ;
	g_exp0[0] = 0x41800000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind16);
	/* BCD temp will be reused */
	g_astr19 = (char *) (FLAG_tv) + (( g_ind16-1) * FLAG_TV.len);
	g_ind17 = FLAG_TV.len;
	g_f[141] = (long) STRADR;
	g_f[142] = (long) g_astr19;
	g_f[143] = (long)  g_ind17 ;
	g_ind17 = 1;
	g_f[144] = (long) CH;
	g_f[145] = 1;
	g_f[146] = (long) g_ind17;
	g_ind17 = 2;
	g_ind18 = 13;
	g_f[147] = (long) AT;
	g_f[148] = 2;
	g_f[149] = (long) g_ind18;
	g_f[150] = (long) g_ind17;
	g_exp0[0] = 0x41900000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind17);
	/* BCD temp will be reused */
	g_astr20 = (char *) (FAC_tv) + (( g_ind17-1) * FAC_TV.len);
	g_ind18 = FAC_TV.len;
	/* skip FAC operation */
	g_f[151] = (long) FAC;
	g_f[152] = (long) g_astr20;
	g_f[153] = (long)  g_ind18 ;
	g_exp0[0] = 0x41900000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind18);
	/* BCD temp will be reused */
	g_astr21 = (char *) (FLAG_tv) + (( g_ind18-1) * FLAG_TV.len);
	g_ind19 = FLAG_TV.len;
	g_f[154] = (long) STRADR;
	g_f[155] = (long) g_astr21;
	g_f[156] = (long)  g_ind19 ;
	g_ind19 = 1;
	g_f[157] = (long) CH;
	g_f[158] = 1;
	g_f[159] = (long) g_ind19;
	g_ind19 = 2;
	g_ind20 = 14;
	g_f[160] = (long) AT;
	g_f[161] = 2;
	g_f[162] = (long) g_ind20;
	g_f[163] = (long) g_ind19;
	g_exp0[0] = 0x42100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind19);
	/* BCD temp will be reused */
	g_astr22 = (char *) (FAC_tv) + (( g_ind19-1) * FAC_TV.len);
	g_ind20 = FAC_TV.len;
	/* skip FAC operation */
	g_f[164] = (long) FAC;
	g_f[165] = (long) g_astr22;
	g_f[166] = (long)  g_ind20 ;
	g_exp0[0] = 0x42100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind20);
	/* BCD temp will be reused */
	g_astr23 = (char *) (FLAG_tv) + (( g_ind20-1) * FLAG_TV.len);
	g_ind21 = FLAG_TV.len;
	g_f[167] = (long) STRADR;
	g_f[168] = (long) g_astr23;
	g_f[169] = (long)  g_ind21 ;
	g_ind21 = 1;
	g_f[170] = (long) CH;
	g_f[171] = 1;
	g_f[172] = (long) g_ind21;
	g_ind21 = 2;
	g_ind22 = 15;
	g_f[173] = (long) AT;
	g_f[174] = 2;
	g_f[175] = (long) g_ind22;
	g_f[176] = (long) g_ind21;
	g_exp0[0] = 0x42110000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind21);
	/* BCD temp will be reused */
	g_astr24 = (char *) (FAC_tv) + (( g_ind21-1) * FAC_TV.len);
	g_ind22 = FAC_TV.len;
	/* skip FAC operation */
	g_f[177] = (long) FAC;
	g_f[178] = (long) g_astr24;
	g_f[179] = (long)  g_ind22 ;
	g_exp0[0] = 0x42110000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind22);
	/* BCD temp will be reused */
	g_astr25 = (char *) (FLAG_tv) + (( g_ind22-1) * FLAG_TV.len);
	g_ind23 = FLAG_TV.len;
	g_f[180] = (long) STRADR;
	g_f[181] = (long) g_astr25;
	g_f[182] = (long)  g_ind23 ;
	g_ind23 = 1;
	g_f[183] = (long) CH;
	g_f[184] = 1;
	g_f[185] = (long) g_ind23;
	g_ind23 = 2;
	g_ind24 = 16;
	g_f[186] = (long) AT;
	g_f[187] = 2;
	g_f[188] = (long) g_ind24;
	g_f[189] = (long) g_ind23;
	g_exp0[0] = 0x42120000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind23);
	/* BCD temp will be reused */
	g_astr26 = (char *) (FAC_tv) + (( g_ind23-1) * FAC_TV.len);
	g_ind24 = FAC_TV.len;
	/* skip FAC operation */
	g_f[190] = (long) FAC;
	g_f[191] = (long) g_astr26;
	g_f[192] = (long)  g_ind24 ;
	g_exp0[0] = 0x42120000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind24);
	/* BCD temp will be reused */
	g_astr27 = (char *) (FLAG_tv) + (( g_ind24-1) * FLAG_TV.len);
	g_ind25 = FLAG_TV.len;
	g_f[193] = (long) STRADR;
	g_f[194] = (long) g_astr27;
	g_f[195] = (long)  g_ind25 ;
	g_ind25 = 1;
	g_f[196] = (long) CH;
	g_f[197] = 1;
	g_f[198] = (long) g_ind25;
	g_ind25 = 2;
	g_ind26 = 17;
	g_f[199] = (long) AT;
	g_f[200] = 2;
	g_f[201] = (long) g_ind26;
	g_f[202] = (long) g_ind25;
	g_exp0[0] = 0x42130000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind25);
	/* BCD temp will be reused */
	g_astr28 = (char *) (FAC_tv) + (( g_ind25-1) * FAC_TV.len);
	g_ind26 = FAC_TV.len;
	/* skip FAC operation */
	g_f[203] = (long) FAC;
	g_f[204] = (long) g_astr28;
	g_f[205] = (long)  g_ind26 ;
	g_exp0[0] = 0x42130000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind26);
	/* BCD temp will be reused */
	g_astr29 = (char *) (FLAG_tv) + (( g_ind26-1) * FLAG_TV.len);
	g_ind27 = FLAG_TV.len;
	g_f[206] = (long) STRADR;
	g_f[207] = (long) g_astr29;
	g_f[208] = (long)  g_ind27 ;
	g_ind27 = 1;
	g_f[209] = (long) CH;
	g_f[210] = 1;
	g_f[211] = (long) g_ind27;
	g_ind27 = 2;
	g_ind28 = 18;
	g_f[212] = (long) AT;
	g_f[213] = 2;
	g_f[214] = (long) g_ind28;
	g_f[215] = (long) g_ind27;
	g_exp0[0] = 0x42140000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind27);
	/* BCD temp will be reused */
	g_astr30 = (char *) (FAC_tv) + (( g_ind27-1) * FAC_TV.len);
	g_ind28 = FAC_TV.len;
	/* skip FAC operation */
	g_f[216] = (long) FAC;
	g_f[217] = (long) g_astr30;
	g_f[218] = (long)  g_ind28 ;
	g_exp0[0] = 0x42140000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind28);
	/* BCD temp will be reused */
	g_astr31 = (char *) (FLAG_tv) + (( g_ind28-1) * FLAG_TV.len);
	g_ind29 = FLAG_TV.len;
	g_f[219] = (long) STRADR;
	g_f[220] = (long) g_astr31;
	g_f[221] = (long)  g_ind29 ;
	g_ind29 = 1;
	g_f[222] = (long) CH;
	g_f[223] = 1;
	g_f[224] = (long) g_ind29;
	g_ind29 = 2;
	g_ind30 = 19;
	g_f[225] = (long) AT;
	g_f[226] = 2;
	g_f[227] = (long) g_ind30;
	g_f[228] = (long) g_ind29;
	g_exp0[0] = 0x42150000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind29);
	/* BCD temp will be reused */
	g_astr32 = (char *) (FAC_tv) + (( g_ind29-1) * FAC_TV.len);
	g_ind30 = FAC_TV.len;
	/* skip FAC operation */
	g_f[229] = (long) FAC;
	g_f[230] = (long) g_astr32;
	g_f[231] = (long)  g_ind30 ;
	g_exp0[0] = 0x42150000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind30);
	/* BCD temp will be reused */
	g_astr33 = (char *) (FLAG_tv) + (( g_ind30-1) * FLAG_TV.len);
	g_ind31 = FLAG_TV.len;
	g_f[232] = (long) STRADR;
	g_f[233] = (long) g_astr33;
	g_f[234] = (long)  g_ind31 ;
	g_ind31 = 1;
	g_f[235] = (long) CH;
	g_f[236] = 1;
	g_f[237] = (long) g_ind31;
	g_ind31 = 2;
	g_ind32 = 20;
	g_f[238] = (long) AT;
	g_f[239] = 2;
	g_f[240] = (long) g_ind32;
	g_f[241] = (long) g_ind31;
	g_exp0[0] = 0x42160000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind31);
	/* BCD temp will be reused */
	g_astr34 = (char *) (FAC_tv) + (( g_ind31-1) * FAC_TV.len);
	g_ind32 = FAC_TV.len;
	/* skip FAC operation */
	g_f[242] = (long) FAC;
	g_f[243] = (long) g_astr34;
	g_f[244] = (long)  g_ind32 ;
	g_exp0[0] = 0x42160000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind32);
	/* BCD temp will be reused */
	g_astr35 = (char *) (FLAG_tv) + (( g_ind32-1) * FLAG_TV.len);
	g_ind33 = FLAG_TV.len;
	g_f[245] = (long) STRADR;
	g_f[246] = (long) g_astr35;
	g_f[247] = (long)  g_ind33 ;
	g_ind33 = 1;
	g_f[248] = (long) CH;
	g_f[249] = 1;
	g_f[250] = (long) g_ind33;
	g_ind33 = 4;
	g_ind34 = 5;
	g_f[251] = (long) AT;
	g_f[252] = 2;
	g_f[253] = (long) g_ind34;
	g_f[254] = (long) g_ind33;
	g_astr36 = g_str3;
	g_ind33 = wb_hex ("8c", g_astr36);
	/* skip FAC operation */
	g_f[255] = (long) FAC;
	g_f[256] = (long) g_astr36;
	g_f[257] = (long)  g_ind33 ;
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind33);
	/* BCD temp will be reused */
	g_astr37 = (char *) (SCREEN_tv) + (( g_ind33-1) * SCREEN_TV.len);
	g_ind34 = SCREEN_TV.len;
	g_f[258] = (long) STRADR;
	g_f[259] = (long) g_astr37;
	g_f[260] = (long)  g_ind34 ;
	g_ind34 = 77;
	g_f[261] = (long) CH;
	g_f[262] = 1;
	g_f[263] = (long) g_ind34;
	g_ind34 = 4;
	g_ind35 = 6;
	g_f[264] = (long) AT;
	g_f[265] = 2;
	g_f[266] = (long) g_ind35;
	g_f[267] = (long) g_ind34;
	g_astr38 = g_str4;
	g_ind34 = wb_hex ("8c", g_astr38);
	/* skip FAC operation */
	g_f[268] = (long) FAC;
	g_f[269] = (long) g_astr38;
	g_f[270] = (long)  g_ind34 ;
	g_exp0[0] = 0x41200000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind34);
	/* BCD temp will be reused */
	g_astr39 = (char *) (SCREEN_tv) + (( g_ind34-1) * SCREEN_TV.len);
	g_ind35 = SCREEN_TV.len;
	g_f[271] = (long) STRADR;
	g_f[272] = (long) g_astr39;
	g_f[273] = (long)  g_ind35 ;
	g_ind35 = 77;
	g_f[274] = (long) CH;
	g_f[275] = 1;
	g_f[276] = (long) g_ind35;
	g_ind35 = 4;
	g_ind36 = 7;
	g_f[277] = (long) AT;
	g_f[278] = 2;
	g_f[279] = (long) g_ind36;
	g_f[280] = (long) g_ind35;
	g_astr40 = g_str5;
	g_ind35 = wb_hex ("8c", g_astr40);
	/* skip FAC operation */
	g_f[281] = (long) FAC;
	g_f[282] = (long) g_astr40;
	g_f[283] = (long)  g_ind35 ;
	g_exp0[0] = 0x41300000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind35);
	/* BCD temp will be reused */
	g_astr41 = (char *) (SCREEN_tv) + (( g_ind35-1) * SCREEN_TV.len);
	g_ind36 = SCREEN_TV.len;
	g_f[284] = (long) STRADR;
	g_f[285] = (long) g_astr41;
	g_f[286] = (long)  g_ind36 ;
	g_ind36 = 77;
	g_f[287] = (long) CH;
	g_f[288] = 1;
	g_f[289] = (long) g_ind36;
	g_ind36 = 4;
	g_ind37 = 8;
	g_f[290] = (long) AT;
	g_f[291] = 2;
	g_f[292] = (long) g_ind37;
	g_f[293] = (long) g_ind36;
	g_astr42 = g_str6;
	g_ind36 = wb_hex ("8c", g_astr42);
	/* skip FAC operation */
	g_f[294] = (long) FAC;
	g_f[295] = (long) g_astr42;
	g_f[296] = (long)  g_ind36 ;
	g_exp0[0] = 0x41400000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind36);
	/* BCD temp will be reused */
	g_astr43 = (char *) (SCREEN_tv) + (( g_ind36-1) * SCREEN_TV.len);
	g_ind37 = SCREEN_TV.len;
	g_f[297] = (long) STRADR;
	g_f[298] = (long) g_astr43;
	g_f[299] = (long)  g_ind37 ;
	g_ind37 = 77;
	g_f[300] = (long) CH;
	g_f[301] = 1;
	g_f[302] = (long) g_ind37;
	g_ind37 = 4;
	g_ind38 = 9;
	g_f[303] = (long) AT;
	g_f[304] = 2;
	g_f[305] = (long) g_ind38;
	g_f[306] = (long) g_ind37;
	g_astr44 = g_str7;
	g_ind37 = wb_hex ("8c", g_astr44);
	/* skip FAC operation */
	g_f[307] = (long) FAC;
	g_f[308] = (long) g_astr44;
	g_f[309] = (long)  g_ind37 ;
	g_exp0[0] = 0x41500000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind37);
	/* BCD temp will be reused */
	g_astr45 = (char *) (SCREEN_tv) + (( g_ind37-1) * SCREEN_TV.len);
	g_ind38 = SCREEN_TV.len;
	g_f[310] = (long) STRADR;
	g_f[311] = (long) g_astr45;
	g_f[312] = (long)  g_ind38 ;
	g_ind38 = 77;
	g_f[313] = (long) CH;
	g_f[314] = 1;
	g_f[315] = (long) g_ind38;
	g_ind38 = 4;
	g_ind39 = 10;
	g_f[316] = (long) AT;
	g_f[317] = 2;
	g_f[318] = (long) g_ind39;
	g_f[319] = (long) g_ind38;
	g_astr46 = g_str8;
	g_ind38 = wb_hex ("8c", g_astr46);
	/* skip FAC operation */
	g_f[320] = (long) FAC;
	g_f[321] = (long) g_astr46;
	g_f[322] = (long)  g_ind38 ;
	g_exp0[0] = 0x41600000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind38);
	/* BCD temp will be reused */
	g_astr47 = (char *) (SCREEN_tv) + (( g_ind38-1) * SCREEN_TV.len);
	g_ind39 = SCREEN_TV.len;
	g_f[323] = (long) STRADR;
	g_f[324] = (long) g_astr47;
	g_f[325] = (long)  g_ind39 ;
	g_ind39 = 77;
	g_f[326] = (long) CH;
	g_f[327] = 1;
	g_f[328] = (long) g_ind39;
	g_ind39 = 4;
	g_ind40 = 11;
	g_f[329] = (long) AT;
	g_f[330] = 2;
	g_f[331] = (long) g_ind40;
	g_f[332] = (long) g_ind39;
	g_astr48 = g_str9;
	g_ind39 = wb_hex ("8c", g_astr48);
	/* skip FAC operation */
	g_f[333] = (long) FAC;
	g_f[334] = (long) g_astr48;
	g_f[335] = (long)  g_ind39 ;
	g_exp0[0] = 0x41700000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind39);
	/* BCD temp will be reused */
	g_astr49 = (char *) (SCREEN_tv) + (( g_ind39-1) * SCREEN_TV.len);
	g_ind40 = SCREEN_TV.len;
	g_f[336] = (long) STRADR;
	g_f[337] = (long) g_astr49;
	g_f[338] = (long)  g_ind40 ;
	g_ind40 = 77;
	g_f[339] = (long) CH;
	g_f[340] = 1;
	g_f[341] = (long) g_ind40;
	g_ind40 = 4;
	g_ind41 = 12;
	g_f[342] = (long) AT;
	g_f[343] = 2;
	g_f[344] = (long) g_ind41;
	g_f[345] = (long) g_ind40;
	g_astr50 = g_str10;
	g_ind40 = wb_hex ("8c", g_astr50);
	/* skip FAC operation */
	g_f[346] = (long) FAC;
	g_f[347] = (long) g_astr50;
	g_f[348] = (long)  g_ind40 ;
	g_exp0[0] = 0x41800000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind40);
	/* BCD temp will be reused */
	g_astr51 = (char *) (SCREEN_tv) + (( g_ind40-1) * SCREEN_TV.len);
	g_ind41 = SCREEN_TV.len;
	g_f[349] = (long) STRADR;
	g_f[350] = (long) g_astr51;
	g_f[351] = (long)  g_ind41 ;
	g_ind41 = 77;
	g_f[352] = (long) CH;
	g_f[353] = 1;
	g_f[354] = (long) g_ind41;
	g_ind41 = 4;
	g_ind42 = 13;
	g_f[355] = (long) AT;
	g_f[356] = 2;
	g_f[357] = (long) g_ind42;
	g_f[358] = (long) g_ind41;
	g_astr52 = g_str11;
	g_ind41 = wb_hex ("8c", g_astr52);
	/* skip FAC operation */
	g_f[359] = (long) FAC;
	g_f[360] = (long) g_astr52;
	g_f[361] = (long)  g_ind41 ;
	g_exp0[0] = 0x41900000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind41);
	/* BCD temp will be reused */
	g_astr53 = (char *) (SCREEN_tv) + (( g_ind41-1) * SCREEN_TV.len);
	g_ind42 = SCREEN_TV.len;
	g_f[362] = (long) STRADR;
	g_f[363] = (long) g_astr53;
	g_f[364] = (long)  g_ind42 ;
	g_ind42 = 77;
	g_f[365] = (long) CH;
	g_f[366] = 1;
	g_f[367] = (long) g_ind42;
	g_ind42 = 4;
	g_ind43 = 14;
	g_f[368] = (long) AT;
	g_f[369] = 2;
	g_f[370] = (long) g_ind43;
	g_f[371] = (long) g_ind42;
	g_astr54 = g_str12;
	g_ind42 = wb_hex ("8c", g_astr54);
	/* skip FAC operation */
	g_f[372] = (long) FAC;
	g_f[373] = (long) g_astr54;
	g_f[374] = (long)  g_ind42 ;
	g_exp0[0] = 0x42100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind42);
	/* BCD temp will be reused */
	g_astr55 = (char *) (SCREEN_tv) + (( g_ind42-1) * SCREEN_TV.len);
	g_ind43 = SCREEN_TV.len;
	g_f[375] = (long) STRADR;
	g_f[376] = (long) g_astr55;
	g_f[377] = (long)  g_ind43 ;
	g_ind43 = 77;
	g_f[378] = (long) CH;
	g_f[379] = 1;
	g_f[380] = (long) g_ind43;
	g_ind43 = 4;
	g_ind44 = 15;
	g_f[381] = (long) AT;
	g_f[382] = 2;
	g_f[383] = (long) g_ind44;
	g_f[384] = (long) g_ind43;
	g_astr56 = g_str13;
	g_ind43 = wb_hex ("8c", g_astr56);
	/* skip FAC operation */
	g_f[385] = (long) FAC;
	g_f[386] = (long) g_astr56;
	g_f[387] = (long)  g_ind43 ;
	g_exp0[0] = 0x42110000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind43);
	/* BCD temp will be reused */
	g_astr57 = (char *) (SCREEN_tv) + (( g_ind43-1) * SCREEN_TV.len);
	g_ind44 = SCREEN_TV.len;
	g_f[388] = (long) STRADR;
	g_f[389] = (long) g_astr57;
	g_f[390] = (long)  g_ind44 ;
	g_ind44 = 77;
	g_f[391] = (long) CH;
	g_f[392] = 1;
	g_f[393] = (long) g_ind44;
	g_ind44 = 4;
	g_ind45 = 16;
	g_f[394] = (long) AT;
	g_f[395] = 2;
	g_f[396] = (long) g_ind45;
	g_f[397] = (long) g_ind44;
	g_astr58 = g_str14;
	g_ind44 = wb_hex ("8c", g_astr58);
	/* skip FAC operation */
	g_f[398] = (long) FAC;
	g_f[399] = (long) g_astr58;
	g_f[400] = (long)  g_ind44 ;
	g_exp0[0] = 0x42120000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind44);
	/* BCD temp will be reused */
	g_astr59 = (char *) (SCREEN_tv) + (( g_ind44-1) * SCREEN_TV.len);
	g_ind45 = SCREEN_TV.len;
	g_f[401] = (long) STRADR;
	g_f[402] = (long) g_astr59;
	g_f[403] = (long)  g_ind45 ;
	g_ind45 = 77;
	g_f[404] = (long) CH;
	g_f[405] = 1;
	g_f[406] = (long) g_ind45;
	g_ind45 = 4;
	g_ind46 = 17;
	g_f[407] = (long) AT;
	g_f[408] = 2;
	g_f[409] = (long) g_ind46;
	g_f[410] = (long) g_ind45;
	g_astr60 = g_str15;
	g_ind45 = wb_hex ("8c", g_astr60);
	/* skip FAC operation */
	g_f[411] = (long) FAC;
	g_f[412] = (long) g_astr60;
	g_f[413] = (long)  g_ind45 ;
	g_exp0[0] = 0x42130000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind45);
	/* BCD temp will be reused */
	g_astr61 = (char *) (SCREEN_tv) + (( g_ind45-1) * SCREEN_TV.len);
	g_ind46 = SCREEN_TV.len;
	g_f[414] = (long) STRADR;
	g_f[415] = (long) g_astr61;
	g_f[416] = (long)  g_ind46 ;
	g_ind46 = 77;
	g_f[417] = (long) CH;
	g_f[418] = 1;
	g_f[419] = (long) g_ind46;
	g_ind46 = 4;
	g_ind47 = 18;
	g_f[420] = (long) AT;
	g_f[421] = 2;
	g_f[422] = (long) g_ind47;
	g_f[423] = (long) g_ind46;
	g_astr62 = g_str16;
	g_ind46 = wb_hex ("8c", g_astr62);
	/* skip FAC operation */
	g_f[424] = (long) FAC;
	g_f[425] = (long) g_astr62;
	g_f[426] = (long)  g_ind46 ;
	g_exp0[0] = 0x42140000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind46);
	/* BCD temp will be reused */
	g_astr63 = (char *) (SCREEN_tv) + (( g_ind46-1) * SCREEN_TV.len);
	g_ind47 = SCREEN_TV.len;
	g_f[427] = (long) STRADR;
	g_f[428] = (long) g_astr63;
	g_f[429] = (long)  g_ind47 ;
	g_ind47 = 77;
	g_f[430] = (long) CH;
	g_f[431] = 1;
	g_f[432] = (long) g_ind47;
	g_ind47 = 4;
	g_ind48 = 19;
	g_f[433] = (long) AT;
	g_f[434] = 2;
	g_f[435] = (long) g_ind48;
	g_f[436] = (long) g_ind47;
	g_astr64 = g_str17;
	g_ind47 = wb_hex ("8c", g_astr64);
	/* skip FAC operation */
	g_f[437] = (long) FAC;
	g_f[438] = (long) g_astr64;
	g_f[439] = (long)  g_ind47 ;
	g_exp0[0] = 0x42150000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind47);
	/* BCD temp will be reused */
	g_astr65 = (char *) (SCREEN_tv) + (( g_ind47-1) * SCREEN_TV.len);
	g_ind48 = SCREEN_TV.len;
	g_f[440] = (long) STRADR;
	g_f[441] = (long) g_astr65;
	g_f[442] = (long)  g_ind48 ;
	g_ind48 = 77;
	g_f[443] = (long) CH;
	g_f[444] = 1;
	g_f[445] = (long) g_ind48;
	g_ind48 = 4;
	g_ind49 = 20;
	g_f[446] = (long) AT;
	g_f[447] = 2;
	g_f[448] = (long) g_ind49;
	g_f[449] = (long) g_ind48;
	g_astr66 = g_str18;
	g_ind48 = wb_hex ("8c", g_astr66);
	/* skip FAC operation */
	g_f[450] = (long) FAC;
	g_f[451] = (long) g_astr66;
	g_f[452] = (long)  g_ind48 ;
	g_exp0[0] = 0x42160000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind48);
	/* BCD temp will be reused */
	g_astr67 = (char *) (SCREEN_tv) + (( g_ind48-1) * SCREEN_TV.len);
	g_ind49 = SCREEN_TV.len;
	g_f[453] = (long) STRADR;
	g_f[454] = (long) g_astr67;
	g_f[455] = (long)  g_ind49 ;
	g_ind49 = 77;
	g_f[456] = (long) CH;
	g_f[457] = 1;
	g_f[458] = (long) g_ind49;
	g_ind49 = 2;
	g_ind50 = 22;
	g_f[459] = (long) AT;
	g_f[460] = 2;
	g_f[461] = (long) g_ind50;
	g_f[462] = (long) g_ind49;
	g_astr68 = g_str19;
	g_ind49 = wb_fac (BFAC_ts, g_astr68);
	g_f[463] = (long) FAC;
	g_f[464] = (long) g_astr68;
	g_f[465] = (long)  g_ind49 ;
	g_f[466] = (long) STRADR;
	g_f[467] = (long) BLANKLINE_ts;
	g_f[468] = (long)  79 ;
	g_ind48 = 79;
	g_f[469] = (long) CH;
	g_f[470] = 1;
	g_f[471] = (long) g_ind48;
	g_ind48 = 2;
	g_ind49 = 23;
	g_f[472] = (long) AT;
	g_f[473] = 2;
	g_f[474] = (long) g_ind49;
	g_f[475] = (long) g_ind48;
	g_f[476] = (long) STRCONST;
	g_f[477] = (long) "(2)First  (5)Next Screen";
	g_f[478] = (long)  24 ;
	g_ind47 = 46;
	g_ind48 = 23;
	g_f[479] = (long) AT;
	g_f[480] = 2;
	g_f[481] = (long) g_ind48;
	g_f[482] = (long) g_ind47;
	g_f[483] = (long) STRCONST;
	g_f[484] = (long) "(15)Print Screen";
	g_f[485] = (long)  16 ;
	g_ind46 = 64;
	g_ind47 = 23;
	g_f[486] = (long) AT;
	g_f[487] = 2;
	g_f[488] = (long) g_ind47;
	g_f[489] = (long) g_ind46;
	g_f[490] = (long) STRCONST;
	g_f[491] = (long) "(16)Cancel & Exit";
	g_f[492] = (long)  17 ;
	g_ind45 = 2;
	g_ind46 = 24;
	g_f[493] = (long) AT;
	g_f[494] = 2;
	g_f[495] = (long) g_ind46;
	g_f[496] = (long) g_ind45;
	g_astr69 = g_str20;
	g_ind45 = wb_hex ("8c", g_astr69);
	/* skip FAC operation */
	g_f[497] = (long) FAC;
	g_f[498] = (long) g_astr69;
	g_f[499] = (long)  g_ind45 ;
	g_astr70 = (char *) SEARCH_ts;
	g_ind45 = min ( 30 ,  ((SLEN_is)));
	g_f[500] = (long) STRADR;
	g_f[501] = (long) g_astr70;
	g_f[502] = (long)  g_ind45 ;
	g_astr71 = g_str21;
	g_ind45 = wb_fac (PFAC_ts, g_astr71);
	g_f[503] = (long) FAC;
	g_f[504] = (long) g_astr71;
	g_f[505] = (long)  g_ind45 ;
	g_astr72 = (char *) PRIMARY_tm;
	g_ind45 = (PRIMARY_TM.tbyte);	 /* full length */
	g_f[506] = (long) STRADR;
	g_f[507] = (long) g_astr72;
	g_f[508] = (long)  g_ind45 ;
	g_ind45 = 48;
	g_ind46 = 24;
	g_f[509] = (long) AT;
	g_f[510] = 2;
	g_f[511] = (long) g_ind46;
	g_f[512] = (long) g_ind45;
	g_f[513] = (long) STRCONST;
	g_f[514] = (long) "Cursor & RETURN: Return With Code";
	g_f[515] = (long)  33 ;
		g_f[516] = (long) STRADR;
	g_f[517] = (long) KEYS_ts;
	g_f[518] = (long)  16 ;
		g_f[519] = (long) INTADR;
	g_f[520] = (long) &((KEYHIT_is));
	g_f[521] = 0L;
	g_ret = wb_accept (158, 1, 1, 0);
/* 000306 */
	g_exp0[0] = 0x42160000L;
	g_exp0[1] = 0L;
	l_BCD ( (KEYHIT_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,5);
	if (g_ind0)
	  {
		goto lALL_DONE;
	  }
/* 000307 */
	g_exp0[0] = 0x42320000L;
	g_exp0[1] = 0L;
	l_BCD ( (KEYHIT_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,5);
	if (g_ind0)
	  {
		goto lALL_DONE;
	  }
/* 000309 */
	g_exp0[0] = 0x42150000L;
	g_exp0[1] = 0L;
	l_BCD ( (KEYHIT_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,6);
	if (g_ind0)
	  {
		goto lL05140;
	  }
/* 000310 */
	PRNTSCRN ();
/* 000311 */
	goto lL03960;
/* 000313 */
lL05140:	;
/* 000313 */
	g_exp0[0] = 0x41800000L;
	g_exp0[1] = 0L;
	l_BCD ( (KEYHIT_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,6);
	if (g_ind0)
	  {
		goto lL05340;
	  }
/* 000314 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("81", g_astr0);
	str_asgn (g_astr0, (long) g_ind0 , 
		PFAC_ts, (long) 1 );
/* 000315 */
	g_astr0 = "Type As Much Of The ";
	g_ind0 = 20;
	g_astr1 = g_str0;
	g_ind1 = wb_cat (g_astr0,  g_ind0 , 20,
		TYPE_ts,  16 , 17, g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		BLANKLINE_ts, (long) 79 );
/* 000316 */
	g_astr0 = " Name As Is Known, Then Press (RETURN)";
	g_ind0 = 38;
	g_astr1 = g_str0;
	g_ind1 = wb_cat (BLANKLINE_ts,  79 , 17,
		g_astr0,  g_ind0 , 20, g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		BLANKLINE_ts, (long) 79 );
/* 000318 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("000d0f10", g_astr0);
	str_asgn (g_astr0, (long) g_ind0 , 
		KEYS_ts, (long) 16 );
/* 000319 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("8c86", g_astr0);
	wb_tran (FAC_tv, (FAC_TV.tbyte), g_astr0,  g_ind0 , 1);
/* 000320 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("a4", g_astr0);
	str_asgn (g_astr0, (long) g_ind0 , 
		BFAC_ts, (long) 1 );
/* 000321 */
	goto lL03960;
/* 000323 */
lL05340:	;
/* 000323 */
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	l_BCD ( (KEYHIT_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,6);
	if (g_ind0)
	  {
		goto lL05460;
	  }
/* 000324 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("81", g_astr0);
	g_ind1 = wb_str (PFAC_ts,  1 , g_astr0,  g_ind0 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lL05460;
	  }
/* 000325 */
	g_ind0 = ( (BREAKP_is) +  1);
	g_astr0 = (char *) PRIMARY_tm;
	g_ind1 = (PRIMARY_TM.tbyte);	 /* full length */
	g_astr1 = g_str0;
	g_ind2 = wb_hex ("00", g_astr1);
	/*LOC = wb_pos(REV,OP,STR1,LEN1,STR2,LEN2);*/
	/*OP:<11,==16,>13,>=19,<=18,<>17,- 7*/
	g_ind3 = wb_pos (1, 19, g_astr0, (long) g_ind1 , g_astr1, (long) g_ind2 );
	g_astr2 = (char *) READKEY_ts;
	g_astr2 += ( g_ind0-1);
	g_ind4 = min ( 99  - ( g_ind0-1),  (g_ind3));
	g_astr3 = (char *) PRIMARY_tm;
	g_ind5 = (PRIMARY_TM.tbyte);	 /* full length */
	g_astr4 = g_str1;
	g_ind6 = wb_hex ("ff", g_astr4);
	g_astr5 = g_str2;
	g_ind7 = wb_all (g_astr4, g_astr5);
	str_asgn (g_astr3, (long) g_ind5 ,
	g_astr2, (long) g_ind4 );
	g_ind8 = wb_addc (g_astr2, (long) g_ind4 , g_astr5, (long) g_ind7 );
/* 000327 */
	goto lL05580;
/* 000329 */
lL05460:	;
/* 000329 */
	g_exp0[0] = 0x41200000L;
	g_exp0[1] = 0L;
	l_BCD ( (KEYHIT_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,5);
	g_exp3[0] = 0L;
	g_exp3[1] = 0L;
	l_BCD ( (C_is), g_exp4);
	addl ('-', g_exp4, g_exp3, g_exp5);
	g_ind1 = cmp_NO(g_exp5,5);
	g_ind2 = (g_ind0 || g_ind1);
	if (g_ind2)
	  {
		g_ind0 = ( (BREAKP_is) +  1);
		g_astr0 = (char *) READKEY_ts;
		g_astr0 += ( g_ind0-1);
		g_ind1 = ( 99  - ( g_ind0-1));
		g_astr1 = g_str0;
		g_ind2 = wb_hex ("00", g_astr1);
		g_astr2 = g_str1;
		g_ind3 = wb_all (g_astr1, g_astr2);
		str_asgn (g_astr2, (long) g_ind3 , 
			g_astr0, (long) g_ind1 );
	  }
/* 000331 */
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	l_BCD ( (C_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,5);
	if (g_ind0)
	  {
		str_asgn (" ", (long) 1 , 
			SCREEN_tv, (long)(SCREEN_TV.tbyte));
	  }
/* 000332 */
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	l_BCD ( (C_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,5);
	if (g_ind0)
	  {
		goto lPLOW_CODES;
	  }
/* 000334 */
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	l_BCD ( (KEYHIT_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind0 = cmp_NO(g_exp2,5);
	if (g_ind0)
	  {
		goto lL05640;
	  }
/* 000335 */
lL05580:	;
/* 000335 */
	str_asgn (" ", (long) 1 , 
		SCREEN_tv, (long)(SCREEN_TV.tbyte));
/* 000335 */
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	 (C_is) =  g_ind0;
/* 000336 */
	goto lPLOW_CODES;
/* 000338 */
lL05640:	;
/* 000338 */
	wb_close (1, WS);
/* 000339 */
wb_wisp = 1;
#ifdef SCREEN
#undef SCREEN
#endif
	str_asgn ("C", 1, STR_TS0, 257);
	STR_TS0[1] = 0x00;
	g_c[g_ctmp+0] = 1;
	g_c[g_ctmp+1] = (long) STR_TS0;
	str_asgn ("I", 1, STR_TS1, 257);
	STR_TS1[1] = 0x00;
	g_c[g_ctmp+2] = 1;
	g_c[g_ctmp+3] = (long) STR_TS1;
	g_argcnt = 5;
	wvaset (&g_argcnt);
	Call_args = 5;
	SCREEN (g_c[g_ctmp+1]
, &(X_is)
, g_c[g_ctmp+3]
, I_tv
, (C_iv)
);
/* 000340 */
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	BCD_l (g_exp0, &g_ind0);
	/* BCD temp will be reused */
	g_aind0 = &C_iv[ g_ind0-1];
	g_exp0[0] = 0x41400000L;
	g_exp0[1] = 0L;
	l_BCD (*(g_aind0), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	BCD_l (g_exp2, &g_ind1);
	 (X_is) =  g_ind1;
/* 000341 */
	g_ind0 = ( (X_is) -  (C_is));
	g_ind1 = (g_ind0 > 0);
	g_exp0[0] = 0x41100000L;
	g_exp0[1] = 0L;
	l_BCD ( (X_is), g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind2 = cmp_NO(g_exp2,0);
	g_ind3 = (g_ind1 || g_ind2);
	if (g_ind3)
	  {
		goto lL03960;
	  }
/* 000343 */
/* 000344 */
	g_astr0 = (char *) (SCREEN_tv) + (( (X_is)-1) * SCREEN_TV.len);
	g_ind0 = SCREEN_TV.len;
	g_ind1 = ( (BREAKP_is) +  1);
	g_astr1 = (char *) (char *)(KEY_TS->p);
	g_astr1 += ( g_ind1-1);
	g_ind2 = ((KEY_TS->len) - ( g_ind1-1));
	g_ind3 = min ( g_ind0 ,  ((SUBLEN_is)));
	str_asgn (g_astr0, (long) g_ind3 , 
		g_astr1, (long) g_ind2 );
/* 000346 */
lALL_DONE:	;
/* 000347 */
	wb_close (1, WS);
/* 000348 */
	Call_args = 4;
	REDALT0 (ufbptr[1-1], KEY_TS, &KEY_IS, F1_IS);
/* 000349 */
	g_ind0 = (*(F1_is) -  0);
	g_ind1 = (g_ind0 > 0);
	if (g_ind1)
	  {
		g_exp0[0] = 0x41300000;
		g_exp0[1] = 0;
		BCD_l (g_exp0, &g_ind0);
		/* Exp variable was tmp & will be reused */
		g_ind1 = 1;
		g_ind2 = wb_key (2, ufbptr[ g_ind1-1],  g_ind0, &g_astr0);
		str_asgn (g_astr0, (long) g_ind2 , 
			(char *)(DESCR_TS->p), (long)(DESCR_TS->len));
	  }
/* 000350 */
	G_scnt = G_hold;	/* restore stack count */
	return (0);
#ifdef END
#undef END
#endif
END:
	G_scnt = G_hold;	/* restore stack count */
	return  (0);

/**** THIS IS THE GOSUB RETURN TABLE ****/
Go_Sub_Ret:

	switch (G_stack[G_scnt--])
	{
	case 0:
		goto ret_000;
	case 1:
		goto ret_001;
	case 2:
		goto ret_002;
	default:
		wb_err("GOSUB TABLE ERROR");
	}
}
/* END OF cmsfind */

