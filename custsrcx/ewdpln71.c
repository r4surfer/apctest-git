/*
 ewdpln71: Mon Oct 19 08:44:05 2020
 */

/* VS-BASIC MAIN PROGRAM */
#include "wvsb0.h"

main (int argc, char *argv[])
{

#include "ewdpln71.h"

/* Declare and initialize the local variables */
	int g_ret;
	int g_argcnt = 0;

static char READKEY_ts[50];
static struct symbol READKEY_TS ={(long) 1, (char *)  READKEY_ts, 50, 0, 0, 0, 50, 50};
static char DESC_ts[30];
static struct symbol DESC_TS ={(long) 1, (char *)  DESC_ts, 30, 0, 0, 0, 30, 30};
static char COUNT_ts[22];
static struct symbol COUNT_TS ={(long) 1, (char *)  COUNT_ts, 22, 0, 0, 0, 22, 22};
static char SC_DEPT_ts[3];
static struct symbol SC_DEPT_TS ={(long) 1, (char *)  SC_DEPT_ts, 3, 0, 0, 0, 3, 3};
static char SC_DEPT_D_ts[30];
static struct symbol SC_DEPT_D_TS ={(long) 1, (char *)  SC_DEPT_D_ts, 30, 0, 0, 0, 30, 30};
static char SC_PRDDATE_ts[10];
static struct symbol SC_PRDDATE_TS ={(long) 1, (char *)  SC_PRDDATE_ts, 10, 0, 0, 0, 10, 10};
static char SC_END_PRDDATE_ts[10];
static struct symbol SC_END_PRDDATE_TS ={(long) 1, (char *)  SC_END_PRDDATE_ts, 10, 0, 0, 0, 10, 10};
static char SC_LOAD_FR_ts[5];
static struct symbol SC_LOAD_FR_TS ={(long) 1, (char *)  SC_LOAD_FR_ts, 5, 0, 0, 0, 5, 5};
static char SC_LOAD_TO_ts[5];
static struct symbol SC_LOAD_TO_TS ={(long) 1, (char *)  SC_LOAD_TO_ts, 5, 0, 0, 0, 5, 5};
static char SC_SHIFT_ts[2];
static struct symbol SC_SHIFT_TS ={(long) 1, (char *)  SC_SHIFT_ts, 2, 0, 0, 0, 2, 2};
static char SC_BARCODE_FR_ts[18];
static struct symbol SC_BARCODE_FR_TS ={(long) 1, (char *)  SC_BARCODE_FR_ts, 18, 0, 0, 0, 18, 18};
static char SC_BARCODE_TO_ts[18];
static struct symbol SC_BARCODE_TO_TS ={(long) 1, (char *)  SC_BARCODE_TO_ts, 18, 0, 0, 0, 18, 18};
static char SC_SEQ_FR_ts[5];
static struct symbol SC_SEQ_FR_TS ={(long) 1, (char *)  SC_SEQ_FR_ts, 5, 0, 0, 0, 5, 5};
static char SC_SEQ_TO_ts[5];
static struct symbol SC_SEQ_TO_TS ={(long) 1, (char *)  SC_SEQ_TO_ts, 5, 0, 0, 0, 5, 5};
static char LB_KEY_ts[35];
static struct symbol LB_KEY_TS ={(long) 1, (char *)  LB_KEY_ts, 35, 0, 0, 0, 35, 35};
static char LB_KEY1_ts[23];
static struct symbol LB_KEY1_TS ={(long) 1, (char *)  LB_KEY1_ts, 23, 0, 0, 0, 23, 23};
static char LB_REC_tv[4][256];
static struct symbol LB_REC_TV ={(long) 1, (char *)  LB_REC_tv, 256, 1, 4, 0, 1024, 1024};
static char LB_PRDDATE_ts[10];
static struct symbol LB_PRDDATE_TS ={(long) 1, (char *)  LB_PRDDATE_ts, 10, 0, 0, 0, 10, 10};
static char LB_DEPT_ts[3];
static struct symbol LB_DEPT_TS ={(long) 1, (char *)  LB_DEPT_ts, 3, 0, 0, 0, 3, 3};
static char LB_SHIFT_ts[2];
static struct symbol LB_SHIFT_TS ={(long) 1, (char *)  LB_SHIFT_ts, 2, 0, 0, 0, 2, 2};
static char LB_LOAD_ts[5];
static struct symbol LB_LOAD_TS ={(long) 1, (char *)  LB_LOAD_ts, 5, 0, 0, 0, 5, 5};
static char LB_BARCODE_ts[18];
static struct symbol LB_BARCODE_TS ={(long) 1, (char *)  LB_BARCODE_ts, 18, 0, 0, 0, 18, 18};
static char LB_FOAM_ts[1];
static struct symbol LB_FOAM_TS ={(long) 1, (char *)  LB_FOAM_ts, 1, 0, 0, 0, 1, 1};
static char LB_SEQ_ts[5];
static struct symbol LB_SEQ_TS ={(long) 1, (char *)  LB_SEQ_ts, 5, 0, 0, 0, 5, 5};
static char FILENAME_ts[8];
static struct symbol FILENAME_TS ={(long) 1, (char *)  FILENAME_ts, 8, 0, 0, 0, 8, 8};
static char HDR_ts[40];
static struct symbol HDR_TS ={(long) 1, (char *)  HDR_ts, 40, 0, 0, 0, 40, 40};
static char MSG_tv[3][79];
static struct symbol MSG_TV ={(long) 1, (char *)  MSG_tv, 79, 1, 3, 0, 237, 237};
static char TIME_ts[8];
static struct symbol TIME_TS ={(long) 1, (char *)  TIME_ts, 8, 0, 0, 0, 8, 8};
static long CURSOR_iv[2];
static struct symbol CURSOR_IV ={(long) 4, (char *)  CURSOR_iv, 4, 1, 2, 0, 8, 8};
static char DATE_ts[8];
static struct symbol DATE_TS ={(long) 1, (char *)  DATE_ts, 8, 0, 0, 0, 8, 8};
static char SCHEMA_ts[8];
static struct symbol SCHEMA_TS ={(long) 1, (char *)  SCHEMA_ts, 8, 0, 0, 0, 8, 8};
static char EDTMESSAGE_ts[79];
static struct symbol EDTMESSAGE_TS ={(long) 1, (char *)  EDTMESSAGE_ts, 79, 0, 0, 0, 79, 79};
static char ERRORMSG_ts[79];
static struct symbol ERRORMSG_TS ={(long) 1, (char *)  ERRORMSG_ts, 79, 0, 0, 0, 79, 79};
static char I_tv[24][80];
static struct symbol I_TV ={(long) 1, (char *)  I_tv, 80, 1, 24, 0, 1920, 1920};
static char INPMESSAGE_ts[79];
static struct symbol INPMESSAGE_TS ={(long) 1, (char *)  INPMESSAGE_ts, 79, 0, 0, 0, 79, 79};
static char LFAC_tv[20][1];
static struct symbol LFAC_TV ={(long) 1, (char *)  LFAC_tv, 1, 1, 20, 0, 20, 20};
static char PF_tv[3][79];
static struct symbol PF_TV ={(long) 1, (char *)  PF_tv, 79, 1, 3, 0, 237, 237};
static char PFKEYS_ts[32];
static struct symbol PFKEYS_TS ={(long) 1, (char *)  PFKEYS_ts, 32, 0, 0, 0, 32, 32};
static char USERID_ts[3];
static struct symbol USERID_TS ={(long) 1, (char *)  USERID_ts, 3, 0, 0, 0, 3, 3};
static char APC_ts[40];
static struct symbol APC_TS ={(long) 1, (char *)  APC_ts, 40, 0, 0, 0, 40, 40};
static char PNAME_ts[21];
static struct symbol PNAME_TS ={(long) 1, (char *)  PNAME_ts, 21, 0, 0, 0, 21, 21};
static long ERR_is;
static struct symbol ERR_IS ={(long) 4, (char *) &ERR_is, 4, 0, 0, 0, 4, 4};
static long SCHEMA_is;
static struct symbol SCHEMA_IS ={(long) 4, (char *) &SCHEMA_is, 4, 0, 0, 0, 4, 4};
static long FIELDNR_is;
static struct symbol FIELDNR_IS ={(long) 4, (char *) &FIELDNR_is, 4, 0, 0, 0, 4, 4};
static long ENABLED_is;
static struct symbol ENABLED_IS ={(long) 4, (char *) &ENABLED_is, 4, 0, 0, 0, 4, 4};
static long KEYHIT_is;
static struct symbol KEYHIT_IS ={(long) 4, (char *) &KEYHIT_is, 4, 0, 0, 0, 4, 4};
static long LASTFIELDNR_is;
static struct symbol LASTFIELDNR_IS ={(long) 4, (char *) &LASTFIELDNR_is, 4, 0, 0, 0, 4, 4};
static long SCRNR_is;
static struct symbol SCRNR_IS ={(long) 4, (char *) &SCRNR_is, 4, 0, 0, 0, 4, 4};
static long U3_is;
static struct symbol U3_IS ={(long) 4, (char *) &U3_is, 4, 0, 0, 0, 4, 4};
static long LBL_is;
static struct symbol LBL_IS ={(long) 4, (char *) &LBL_is, 4, 0, 0, 0, 4, 4};
static long BEEN_HERE_is;
static struct symbol BEEN_HERE_IS ={(long) 4, (char *) &BEEN_HERE_is, 4, 0, 0, 0, 4, 4};
static long LB_FOAM_is;
static struct symbol LB_FOAM_IS ={(long) 4, (char *) &LB_FOAM_is, 4, 0, 0, 0, 4, 4};
static long P_is;
static struct symbol P_IS ={(long) 4, (char *) &P_is, 4, 0, 0, 0, 4, 4};
static long EDIT_is;
static struct symbol EDIT_IS ={(long) 4, (char *) &EDIT_is, 4, 0, 0, 0, 4, 4};
static long DEPT_is;
static struct symbol DEPT_IS ={(long) 4, (char *) &DEPT_is, 4, 0, 0, 0, 4, 4};
static long BARCODE_is;
static struct symbol BARCODE_IS ={(long) 4, (char *) &BARCODE_is, 4, 0, 0, 0, 4, 4};
static long SCSEQ_is;
static struct symbol SCSEQ_IS ={(long) 4, (char *) &SCSEQ_is, 4, 0, 0, 0, 4, 4};
static long COMP_is;
static struct symbol COMP_IS ={(long) 4, (char *) &COMP_is, 4, 0, 0, 0, 4, 4};
static long K_is;
static struct symbol K_IS ={(long) 4, (char *) &K_is, 4, 0, 0, 0, 4, 4};
static struct symbol *allvar[]={
&READKEY_TS,
&DESC_TS,
&COUNT_TS,
&SC_DEPT_TS,
&SC_DEPT_D_TS,
&SC_PRDDATE_TS,
&SC_END_PRDDATE_TS,
&SC_LOAD_FR_TS,
&SC_LOAD_TO_TS,
&SC_SHIFT_TS,
&SC_BARCODE_FR_TS,
&SC_BARCODE_TO_TS,
&SC_SEQ_FR_TS,
&SC_SEQ_TO_TS,
&LB_KEY_TS,
&LB_KEY1_TS,
&LB_REC_TV,
&LB_PRDDATE_TS,
&LB_DEPT_TS,
&LB_SHIFT_TS,
&LB_LOAD_TS,
&LB_BARCODE_TS,
&LB_FOAM_TS,
&LB_SEQ_TS,
&FILENAME_TS,
&HDR_TS,
&MSG_TV,
&TIME_TS,
&CURSOR_IV,
&DATE_TS,
&SCHEMA_TS,
&EDTMESSAGE_TS,
&ERRORMSG_TS,
&I_TV,
&INPMESSAGE_TS,
&LFAC_TV,
&PF_TV,
&PFKEYS_TS,
&USERID_TS,
&APC_TS,
&PNAME_TS,
&ERR_IS,
&SCHEMA_IS,
&FIELDNR_IS,
&ENABLED_IS,
&KEYHIT_IS,
&LASTFIELDNR_IS,
&SCRNR_IS,
&U3_IS,
&LBL_IS,
&BEEN_HERE_IS,
&LB_FOAM_IS,
&P_IS,
&EDIT_IS,
&DEPT_IS,
&BARCODE_IS,
&SCSEQ_IS,
&COMP_IS,
&K_IS,
NULL};
/***   UFB Structure initialization   ***/
/* TYP,IO,EOD,MASK,DUP,KEY,REC,SIZ,ERR, */
/* PRNAME,BUF,FD,CH,FTYP,RECSIZ,POS,LEN,*/
/* USED,SIZE,MODE,IOTYP,EODTYP,         */
/* KEYPOS,KEYLEN, KEYPOS,KEYLEN,.....   */
/* (uninitialized)VOL,FILNAM,PATH,CMASK */
static struct ufb ufb1 = {
	0L, 0L, 0L, -1L, 12L, 0L, 0L, 0L, 0L,
	"APCPLNDT", NULL, -1, 1, 0x22, 256, 24, 23,
	0, 0, 0, 0, 0,
	47,57,	53,51,	1,23,	96,8,	0,0,	0,0,	0,0,	0,0,
	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0};
static struct ufb ufb4 = {
	0L, 0L, 0L, -1L, 0L, 0L, 0L, 0L, 0L,
	"GENCODES", NULL, -1, 4, 0x22, 128, 1, 24,
	0, 0, 0, 0, 0,
	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,
	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0};
static struct ufb ufb5 = {
	0L, 0L, 0L, -1L, 0L, 0L, 0L, 0L, 0L,
	"EWDPRDLB", NULL, -1, 5, 0x22, 1024, 1, 35,
	0, 0, 0, 0, 0,
	278,23,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,
	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0};
static struct ufb ufb6 = {
	0L, 0L, 0L, -1L, 0L, 0L, 0L, 0L, 0L,
	"BCKLINES", NULL, -1, 6, 0x22, 300, 10, 19,
	0, 0, 0, 0, 0,
	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,
	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0};
static struct ufb ufb8 = {
	0L, 0L, 0L, -1L, 7L, 0L, 0L, 0L, 0L,
	"AWDAPPLS", NULL, -1, 8, 0x22, 1024, 1, 20,
	0, 0, 0, 0, 0,
	21,34,	23,32,	56,10,	0,0,	0,0,	0,0,	0,0,	0,0,
	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0};
static struct ufb ufb9 = {
	0L, 0L, 0L, -1L, 2L, 0L, 0L, 0L, 0L,
	"AWDSKUXR", NULL, -1, 9, 0x22, 256, 1, 16,
	0, 0, 0, 0, 0,
	17,20,	37,45,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,
	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0};
static struct ufb ufb16 = {
	0L, 0L, 0L, -1L, 1L, 0L, 0L, 0L, 0L,
	"BCKMASTR", NULL, -1, 16, 0x22, 1000, 1, 25,
	0, 0, 0, 0, 0,
	26,16,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,
	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0};
static struct ufb ufb58 = {
	0L, 0L, 0L, -1L, 0L, 0L, 0L, 0L, 0L,
	"ORADESC2", NULL, -1, 58, 0x22, 1024, 1, 11,
	0, 0, 0, 0, 0,
	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,
	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0};
static struct ufb ufb63 = {
	0L, 0L, 0L, -1L, 3L, 0L, 0L, 0L, 0L,
	"BCKSUBPT", NULL, -1, 63, 0x22, 256, 1, 11,
	0, 0, 0, 0, 0,
	12,11,	23,45,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,
	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0,	0,0};
static struct ufb *ufbptr[64]={
	&ufb1,NULL,NULL,&ufb4,&ufb5,&ufb6,NULL,&ufb8,
	&ufb9,NULL,NULL,NULL,NULL,NULL,NULL,&ufb16,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,
	NULL,&ufb58,NULL,NULL,NULL,NULL,&ufb63,NULL};

static int Iflag = 0;
		
/* set the Str COMs */

	wb_init (&Iflag, allvar, argv[0]);	/* init for main program */
/* 000001 */
/* 000157 */
/* 000168 */
	str_asgn ("Generate Production Labels- 07-27-2006", (long) 38 , 
		APC_ts, (long) 40 );
/* 000169 */
	str_asgn ("EWDPLN71 - Rev: R1.00", (long) 21 , 
		PNAME_ts, (long) 21 );
/* 000171 */
/* 000177 */
/* 000195 */
/* 000204 */
/* 000210 */
/* 000217 */
/* 000221 */
/* 000228 */
/* 000235 */
/* 000243 */
/* 000247 */
/* 000252 */
	str_asgn ("Opening Files, One Moment Please", 32, STR_TS0, 257);
	STR_TSD0.p = STR_TS0;
	STR_TSD0.len = 32;
	STR_TSD0.tbyte = 32;
	Call_args = 1;
	SHOSTAT (&STR_TSD0);
/* 000254 */
	str_asgn ("APCPLNDT", (long) 8 , 
		FILENAME_ts, (long) 8 );
/* 000254 */
	Call_args = 3;
	EWDOPEN (ufbptr[1-1], &FILENAME_TS, &ERR_IS);
/* 000255 */
	g_ind0 = ( (ERR_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 0;
		goto lOPEN_ERROR;
ret_000:	;
	  }
/* 000257 */
	str_asgn ("GENCODES", (long) 8 , 
		FILENAME_ts, (long) 8 );
/* 000257 */
	Call_args = 3;
	EWDOPEN (ufbptr[4-1], &FILENAME_TS, &ERR_IS);
/* 000258 */
	g_ind0 = ( (ERR_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 1;
		goto lOPEN_ERROR;
ret_001:	;
	  }
/* 000259 */
	str_asgn ("EWDPRDLB", (long) 8 , 
		FILENAME_ts, (long) 8 );
/* 000259 */
	Call_args = 3;
	EWDOPEN (ufbptr[5-1], &FILENAME_TS, &ERR_IS);
/* 000260 */
	g_ind0 = ( (ERR_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 2;
		goto lOPEN_ERROR;
ret_002:	;
	  }
/* 000262 */
	str_asgn ("BCKLINES", (long) 8 , 
		FILENAME_ts, (long) 8 );
/* 000262 */
	Call_args = 3;
	EWDOPEN (ufbptr[6-1], &FILENAME_TS, &ERR_IS);
/* 000263 */
	g_ind0 = ( (ERR_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 3;
		goto lOPEN_ERROR;
ret_003:	;
	  }
/* 000265 */
	str_asgn ("AWDAPPLS", (long) 8 , 
		FILENAME_ts, (long) 8 );
/* 000265 */
	Call_args = 3;
	EWDOPEN (ufbptr[8-1], &FILENAME_TS, &ERR_IS);
/* 000266 */
	g_ind0 = ( (ERR_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 4;
		goto lOPEN_ERROR;
ret_004:	;
	  }
/* 000268 */
	str_asgn ("AWDSKUXR", (long) 8 , 
		FILENAME_ts, (long) 8 );
/* 000268 */
	Call_args = 3;
	EWDOPEN (ufbptr[9-1], &FILENAME_TS, &ERR_IS);
/* 000269 */
	g_ind0 = ( (ERR_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 5;
		goto lOPEN_ERROR;
ret_005:	;
	  }
/* 000271 */
	str_asgn ("BCKSUBPT", (long) 8 , 
		FILENAME_ts, (long) 8 );
/* 000271 */
	Call_args = 3;
	EWDOPEN (ufbptr[63-1], &FILENAME_TS, &ERR_IS);
/* 000272 */
	g_ind0 = ( (ERR_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 6;
		goto lOPEN_ERROR;
ret_006:	;
	  }
/* 000274 */
	str_asgn ("ORADESC2", (long) 8 , 
		FILENAME_ts, (long) 8 );
/* 000274 */
	Call_args = 3;
	EWDOPEN (ufbptr[58-1], &FILENAME_TS, &ERR_IS);
/* 000275 */
	g_ind0 = ( (ERR_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 7;
		goto lOPEN_ERROR;
ret_007:	;
	  }
/* 000277 */
	str_asgn ("BCKMASTR", (long) 8 , 
		FILENAME_ts, (long) 8 );
/* 000277 */
	Call_args = 3;
	EWDOPEN (ufbptr[16-1], &FILENAME_TS, &ERR_IS);
/* 000278 */
	g_ind0 = ( (ERR_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 8;
		goto lOPEN_ERROR;
ret_008:	;
	  }
/* 000279 */
/* 000284 */
	str_asgn ("ID", 2, STR_TS0, 257);
	STR_TS0[2] = 0x00;
	g_c[g_ctmp+0] = 1;
	g_c[g_ctmp+1] = (long) STR_TS0;
	g_argcnt = 2;
	wvaset (&g_argcnt);
	Call_args = 2;
	cms_xtract (g_c[g_ctmp+1]
, USERID_ts
);
/* 000285 */
	g_astr0 = g_str0;
	wb_date(g_astr0);
	str_asgn (g_astr0, (long) 6 , 
		DATE_ts, (long) 8 );
/* 000286 */
	Call_args = 1;
	DATEFMT (&DATE_TS);
/* 000287 */
	str_asgn ("To Modify Displayed Values, Position Cursor to Desired Value & Press (RETURN).", (long) 78 , 
		EDTMESSAGE_ts, (long) 79 );
/* 000289 */
#ifdef TIME
#undef TIME
#endif
	Call_args = 1;
	TIME (&TIME_TS);
/* 000291 */
	 (ERR_is) =  0;
/* 000292 */
	Call_args = 4;
	SCHEMA (&SCHEMA_TS, &SCHEMA_IS, ufbptr[4-1], &ERR_IS);
/* 000297 */
/* 000303 */
lINPUTMODE:	;
/* 000304 */
	G_stack[++G_scnt] = 9;
	goto lINITIALIZE_VARIABLES;
ret_009:	;
/* 000306 */
	/* For loop */
	      /* assign initial value to loop index */
	 (FIELDNR_is) =  1;
	g_ima0 = 6;	/* assign Max value to loop term variable */
 	g_ist0 = 1;
	g_retlab0 = 10;	/* assign return label number variable */
 ret_010:	;  /* loop top label */
/* 000307 */
lL10110:	;
/* 000307 */
	/* GOSUB PRIME CALL */
	GSP_param[1] = INTADR;
	GSP_param[2] =  (FIELDNR_is);
	GSP_param[0] = 1;   /* number of params */
	G_stack[++G_scnt] = 11;
	goto f_000051;
ret_011:	;
/* 000308 */
	g_ind0 = ( (ENABLED_is) -  0);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL10230;
	  }
/* 000309 */
lL10130:	;
/* 000309 */
	/* GOSUB PRIME CALL */
	GSP_param[1] = INTADR;
	GSP_param[2] =  (FIELDNR_is);
	GSP_param[3] = INTCONST;
	GSP_param[4] = 1;
	GSP_param[0] = 2;   /* number of params */
	G_stack[++G_scnt] = 12;
	goto f_000101;
ret_012:	;
/* 000310 */
	g_ind0 = ( (KEYHIT_is) -  1);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 13;
		goto lSTARTOVER;
ret_013:	;
	  }
/* 000311 */
	g_ind0 = ( (KEYHIT_is) -  4);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL10215;
	  }
/* 000312 */
lL10160:	;
/* 000312 */
	g_ind0 = ( (FIELDNR_is) -  1);
	/* Make MIN/MAX Gmind = 0 */
	g_m[0] = (long) INTCONST;
	g_m[1] = (long) g_ind0;
	g_m[2] = (long) INTCONST;
	g_m[3] = 1L;
	g_m[4] = 0L;
	wb_max (0, 2, g_exp0);
	BCD_l (g_exp0, &g_ind1);
	 (FIELDNR_is) =  g_ind1;
/* 000313 */
	/* GOSUB PRIME CALL */
	GSP_param[1] = INTADR;
	GSP_param[2] =  (FIELDNR_is);
	GSP_param[0] = 1;   /* number of params */
	G_stack[++G_scnt] = 14;
	goto f_000051;
ret_014:	;
/* 000314 */
	g_ind0 = ( (ENABLED_is) -  1);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL10130;
	  }
/* 000315 */
	g_ind0 = ( (FIELDNR_is) -  1);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL10110;
	  }
/* 000316 */
	goto lL10160;
/* 000317 */
lL10215:	;
/* 000317 */
	g_ind0 = ( (KEYHIT_is) -  16);
	g_ind1 = (g_ind0 == 0);
	g_ind2 = ( (FIELDNR_is) -  1);
	g_ind3 = (g_ind2 == 0);
	g_ind4 = (g_ind1 && g_ind3);
	if (g_ind4)
	  {
		goto lEXIT_PROGRAM;
	  }
/* 000318 */
	g_ind0 = ( (KEYHIT_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL10130;
	  }
/* 000319 */
lL10230:	;
/* 000319 */
	/* GOSUB PRIME CALL */
	GSP_param[1] = INTADR;
	GSP_param[2] =  (FIELDNR_is);
	GSP_param[0] = 1;   /* number of params */
	G_stack[++G_scnt] = 15;
	goto f_000151;
ret_015:	;
/* 000320 */
	g_ind0 = wb_str (ERRORMSG_ts,  79 , " ",  1 );
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL10130;
	  }
/* 000321 */
	g_ret = wb_nexti( (FIELDNR_is), g_ima0, g_ist0);
	if (g_ret != 1)
	  {  /* return to loop top */
	     (FIELDNR_is) =  (FIELDNR_is) + g_ist0;
	    G_stack[++G_scnt] = g_retlab0;
	    goto Go_Sub_Ret;  /* use return stack */
	  }
	else
	  {  /* end of For loop : clear step */
	    g_ist0 = 0;
	  }
/* 000323 */
/* 000329 */
lEDITPG1:	;
/* 000330 */
	 (LASTFIELDNR_is) =  0;
/* 000331 */
	/* GOSUB PRIME CALL */
	GSP_param[1] = INTCONST;
	GSP_param[2] = 0;
	GSP_param[3] = INTCONST;
	GSP_param[4] = 2;
	GSP_param[0] = 2;   /* number of params */
	G_stack[++G_scnt] = 16;
	goto f_000101;
ret_016:	;
/* 000332 */
	g_ind0 = ( (KEYHIT_is) -  1);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 17;
		goto lSTARTOVER;
ret_017:	;
	  }
/* 000333 */
	g_ind0 = ( (KEYHIT_is) -  16);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lDATALOAD;
	  }
/* 000334 */
	g_ind0 = ( (KEYHIT_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lEDITPG1;
	  }
/* 000335 */
lL11120:	;
/* 000335 */
	g_aind0 = &CURSOR_iv[ 1-1];
	g_ind0 = (*(g_aind0) -  2);
	 (FIELDNR_is) =  g_ind0;
/* 000336 */
	g_ind0 = ( (FIELDNR_is) -  1);
	g_ind1 = (g_ind0 < 0);
	g_ind2 = ( (FIELDNR_is) -  6);
	g_ind3 = (g_ind2 > 0);
	g_ind4 = (g_ind1 || g_ind3);
	if (g_ind4)
	  {
		goto lEDITPG1;
	  }
/* 000337 */
	g_ind0 = ( (FIELDNR_is) -  (LASTFIELDNR_is));
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lEDITPG1;
	  }
/* 000338 */
	/* GOSUB PRIME CALL */
	GSP_param[1] = INTADR;
	GSP_param[2] =  (FIELDNR_is);
	GSP_param[0] = 1;   /* number of params */
	G_stack[++G_scnt] = 18;
	goto f_000051;
ret_018:	;
/* 000339 */
	g_ind0 = ( (ENABLED_is) -  0);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lEDITPG1;
	  }
/* 000340 */
lL11170:	;
/* 000340 */
	/* GOSUB PRIME CALL */
	GSP_param[1] = INTADR;
	GSP_param[2] =  (FIELDNR_is);
	GSP_param[3] = INTCONST;
	GSP_param[4] = 2;
	GSP_param[0] = 2;   /* number of params */
	G_stack[++G_scnt] = 19;
	goto f_000101;
ret_019:	;
/* 000341 */
	g_ind0 = ( (KEYHIT_is) -  1);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 20;
		goto lSTARTOVER;
ret_020:	;
	  }
/* 000342 */
	g_ind0 = ( (KEYHIT_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL11170;
	  }
/* 000343 */
	/* GOSUB PRIME CALL */
	GSP_param[1] = INTADR;
	GSP_param[2] =  (FIELDNR_is);
	GSP_param[0] = 1;   /* number of params */
	G_stack[++G_scnt] = 21;
	goto f_000151;
ret_021:	;
/* 000344 */
	g_ind0 = wb_str (ERRORMSG_ts,  79 , " ",  1 );
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL11170;
	  }
/* 000345 */
	 (LASTFIELDNR_is) =  (FIELDNR_is);
/* 000346 */
	goto lL11120;
/* 000348 */
/* 000354 */
	/* DEFFN Prime */
	goto f_s00051;
    f_000051:
	/* Handle parameters to DEFFN */ 
	g_ind0 = 1;  /* GSP_param index */
	              /*start w/ 1 to skip number of params */
	if (!GSP_param[0]--) /* test number of parameters*/
		wb_err("GOSUB' ERROR:Invalid number of parameters");
	/* Integer parameter */ 
	if ((GSP_param[g_ind0] & 0xF0) == 0x40)
	  {  /* passes an integer */
	    (FIELDNR_is) = GSP_param[++g_ind0];
	   g_ind0++;
	  }
	else if ((GSP_param[g_ind0] & 0xF0) == 0x80)
	  {  /* convert from BCD */
	   BCD_l(&GSP_param[++g_ind0], &g_ind1);
	    (FIELDNR_is) = g_ind1;
	   g_ind0 += 2;
	  }
	else 
		wb_err("GOSUB' ERROR:parameter mismatch");
    f_s00051:
	;
/* 000355 */
	 (ENABLED_is) =  1;
/* 000356 */
	goto Go_Sub_Ret;
/* 000358 */
/* 000364 */
	/* DEFFN Prime */
	goto f_s00050;
    f_000050:
	/* Handle parameters to DEFFN */ 
	g_ind0 = 1;  /* GSP_param index */
	              /*start w/ 1 to skip number of params */
	if (!GSP_param[0]--) /* test number of parameters*/
		wb_err("GOSUB' ERROR:Invalid number of parameters");
	/* Integer parameter */ 
	if ((GSP_param[g_ind0] & 0xF0) == 0x40)
	  {  /* passes an integer */
	    (SCRNR_is) = GSP_param[++g_ind0];
	   g_ind0++;
	  }
	else if ((GSP_param[g_ind0] & 0xF0) == 0x80)
	  {  /* convert from BCD */
	   BCD_l(&GSP_param[++g_ind0], &g_ind1);
	    (SCRNR_is) = g_ind1;
	   g_ind0 += 2;
	  }
	else 
		wb_err("GOSUB' ERROR:parameter mismatch");
	if (!GSP_param[0]--) /* test number of parameters*/
		wb_err("GOSUB' ERROR:Invalid number of parameters");
	/* Integer parameter */ 
	if ((GSP_param[g_ind0] & 0xF0) == 0x40)
	  {  /* passes an integer */
	    (FIELDNR_is) = GSP_param[++g_ind0];
	   g_ind0++;
	  }
	else if ((GSP_param[g_ind0] & 0xF0) == 0x80)
	  {  /* convert from BCD */
	   BCD_l(&GSP_param[++g_ind0], &g_ind2);
	    (FIELDNR_is) = g_ind2;
	   g_ind0 += 2;
	  }
	else 
		wb_err("GOSUB' ERROR:parameter mismatch");
    f_s00050:
	;
/* 000365 */
	g_ind0 = ( (FIELDNR_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL28110;
	  }
/* 000366 */
	str_asgn (EDTMESSAGE_ts, (long) 79 , 
		INPMESSAGE_ts, (long) 79 );
/* 000367 */
	goto Go_Sub_Ret;
/* 000369 */
lL28110:	;
/* 000371 */
	g_ind0 = ( (SCRNR_is) -  1);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		g_ret = wb_restore (a_items, &d_items, &c_items, 375L,  (FIELDNR_is));
	  }
/* 000372 */
	g_f[0] = (long) STRADR;
	g_f[1] = (long) (INPMESSAGE_ts);
	g_f[2] = (long)  79 ;
	g_f[3] = 0L;
	/*a_items, d_items, c_items, CNT*/
	g_ret = wb_read (a_items, &d_items, &c_items, 1);
/* 000373 */
	goto Go_Sub_Ret;
/* 000375 */
lSCRN1_MSG:	;
/* 000375 */
/* 000383 */
/* 000387 */
lSTARTOVER:	;
/* 000388 */
	 (U3_is) =  2;
/* 000389 */
	Call_args = 1;
	STARTOVR (&U3_IS);
/* 000390 */
	g_ind0 = ( (U3_is) -  1);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto Go_Sub_Ret;
	  }
/* 000391 */
	G_scnt = 0;	/* RETURN CLEAR ALL */
/* 000392 */
	goto lINPUTMODE;
/* 000394 */
/* 000399 */
lINITIALIZE_VARIABLES:	;
/* 000400 */
	wb_vinit (" ",
	&ERRORMSG_TS,
	&INPMESSAGE_TS,
	&SC_DEPT_TS,
	&SC_DEPT_D_TS,
	&SC_PRDDATE_TS,
	&SC_LOAD_FR_TS,
	&SC_LOAD_TO_TS,
	&SC_SHIFT_TS,
	&SC_BARCODE_FR_TS,
	&SC_BARCODE_TO_TS,
	&LB_BARCODE_TS,
	&SC_END_PRDDATE_TS,
	&SC_SEQ_FR_TS,
	&SC_SEQ_TO_TS,
	NULL);
/* 000405 */
	 (LBL_is) =  0;
/* 000407 */
	goto Go_Sub_Ret;
/* 000409 */
/* 000415 */
lDATALOAD:	;
/* 000416 */
	str_asgn ("Printing Production Labels...", 29, STR_TS0, 257);
	STR_TSD0.p = STR_TS0;
	STR_TSD0.len = 29;
	STR_TSD0.tbyte = 29;
	Call_args = 1;
	SHOSTAT (&STR_TSD0);
/* 000417 */
	str_asgn ("Labels Printed (xxxxx)", (long) 22 , 
		COUNT_ts, (long) 22 );
/* 000418 */
	Call_args = 1;
	DATUFMTC (&SC_PRDDATE_TS);
/* 000420 */
	Call_args = 1;
	DATUFMTC (&SC_END_PRDDATE_TS);
/* 000422 */
	 (BEEN_HERE_is) =  0;
/* 000424 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("00", g_astr0);
	g_astr1 = g_str1;
	g_ind1 = wb_all (g_astr0, g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		LB_KEY_ts, (long) 35 );
/* 000425 */
	g_astr0 = (char *) LB_KEY_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 35  - ( 1-1),  (6));
	str_asgn (SC_PRDDATE_ts, (long) 10 , 
		g_astr0, (long) g_ind0 );
/* 000426 */
lLOAD_NEXT_REC:	;
/* 000427 */
	G_chan = 5 - 1;
	g_ind0 = 256;
	g_ind1 = 0;
	for (g_ind2 = 0; g_ind2 < 4; g_ind2++)
	   { /* fill g_f array for repeated fmt */
		g_f[g_ind1++] = (long) CH;
		g_f[g_ind1++] = 1L;
		g_f[g_ind1++] = (long) g_ind0;
	   }
	g_f[12] = (long) STRCONST;
	g_f[13] = (long) LB_KEY_ts;
	g_f[14] =  35 ;
	g_f[15] = (long) STRSTRT;
	g_f[16] = (long) &LB_REC_TV;
	g_f[17] = 0L;
	/*GETT,UFBPTR[CH],HOLD,KEYIND,KEYREL,RELF,RECF,PIC,CNT*/
	g_ret = wb_rdgt (READ, ufbptr[G_chan], 0, 0,
	62, 1,  0, 4, 1);
	if (g_ret == EODX)
	  goto lLOAD_DONE;
	else if (g_ret == DATAX)
	  wb_iserr ("DATA", ufbptr[G_chan]->prname);
	else if (g_ret == TIMEOX)
	  wb_iserr ("TIMEOUT", ufbptr[G_chan]->prname);
	else if (g_ret == IOERRX)
	  wb_iserr ("IOERR", ufbptr[G_chan]->prname);
/* 000431 */
	g_astr0 = (char *) LB_REC_tv;
	g_astr0 += ( 1-1);
	g_ind0 = min ((LB_REC_TV.tbyte) - ( 1-1),  (35));
	str_asgn (g_astr0, (long) g_ind0 , 
		LB_KEY_ts, (long) 35 );
/* 000433 */
	g_astr0 = (char *) LB_KEY_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 35  - ( 1-1),  (6));
	str_asgn (g_astr0, (long) g_ind0 , 
		LB_PRDDATE_ts, (long) 10 );
/* 000434 */
	g_astr0 = (char *) LB_KEY_ts;
	g_astr0 += ( 12-1);
	g_ind0 = min ( 35  - ( 12-1),  (3));
	str_asgn (g_astr0, (long) g_ind0 , 
		LB_DEPT_ts, (long) 3 );
/* 000436 */
	g_astr0 = (char *) LB_KEY_ts;
	g_astr0 += ( 15-1);
	g_ind0 = min ( 35  - ( 15-1),  (2));
	str_asgn (g_astr0, (long) g_ind0 , 
		LB_SHIFT_ts, (long) 2 );
/* 000437 */
	g_astr0 = (char *) LB_KEY_ts;
	g_astr0 += ( 22-1);
	g_ind0 = min ( 35  - ( 22-1),  (5));
	str_asgn (g_astr0, (long) g_ind0 , 
		LB_LOAD_ts, (long) 5 );
/* 000439 */
	g_astr0 = (char *) LB_REC_tv;
	g_astr0 += ( 278-1);
	g_ind0 = min ((LB_REC_TV.tbyte) - ( 278-1),  (18));
	str_asgn (g_astr0, (long) g_ind0 , 
		LB_BARCODE_ts, (long) 18 );
/* 000441 */
	g_astr0 = (char *) LB_REC_tv;
	g_astr0 += ( 601-1);
	g_ind0 = min ((LB_REC_TV.tbyte) - ( 601-1),  (1));
	str_asgn (g_astr0, (long) g_ind0 , 
		LB_FOAM_ts, (long) 1 );
/* 000444 */
	g_astr0 = (char *) LB_REC_tv;
	g_astr0 += ( 311-1);
	g_ind0 = min ((LB_REC_TV.tbyte) - ( 311-1),  (5));
	str_asgn (g_astr0, (long) g_ind0 , 
		LB_SEQ_ts, (long) 5 );
/* 000446 */
	g_ind0 = wb_str (LB_PRDDATE_ts,  10 , SC_END_PRDDATE_ts,  10 );
	g_ind1 = (g_ind0 > 0);
	if (g_ind1)
	  {
		goto lLOAD_DONE;
	  }
/* 000448 */
	g_ind0 = wb_str (LB_SHIFT_ts,  2 , SC_SHIFT_ts,  2 );
	g_ind1 = (g_ind0 != 0);
	g_ind2 = wb_str (SC_SHIFT_ts,  2 , "AA",  2 );
	g_ind3 = (g_ind2 != 0);
	g_ind4 = (g_ind1 && g_ind3);
	if (g_ind4)
	  {
		goto lLOAD_NEXT_REC;
	  }
/* 000451 */
	g_ind0 = wb_str (SC_DEPT_ts,  3 , "ALL",  3 );
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL30000;
	  }
/* 000452 */
	g_ind0 = wb_str (LB_DEPT_ts,  3 , SC_DEPT_ts,  3 );
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lLOAD_NEXT_REC;
	  }
/* 000454 */
lL30000:	;
/* 000458 */
	g_ind0 = wb_str (SC_DEPT_ts,  3 , "ALL",  3 );
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lSKIP_PRINT_TEST;
	  }
/* 000459 */
	g_ind0 = wb_str (SC_SHIFT_ts,  2 , "AA",  2 );
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lSKIP_PRINT_TEST;
	  }
/* 000460 */
	g_astr0 = (char *) SC_LOAD_FR_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 5  - ( 1-1),  (3));
	g_ind1 = wb_str (g_astr0,  g_ind0 , "ALL",  3 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lSKIP_PRINT_TEST;
	  }
/* 000461 */
	g_astr0 = (char *) SC_BARCODE_FR_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 18  - ( 1-1),  (3));
	g_ind1 = wb_str (g_astr0,  g_ind0 , "ALL",  3 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lSKIP_PRINT_TEST;
	  }
/* 000462 */
	g_astr0 = (char *) SC_SEQ_FR_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 5  - ( 1-1),  (3));
	g_ind1 = wb_str (g_astr0,  g_ind0 , "ALL",  3 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lSKIP_PRINT_TEST;
	  }
/* 000463 */
	wb_vinit (" ", &READKEY_TS, NULL);
/* 000464 */
	g_astr0 = (char *) READKEY_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 50  - ( 1-1),  (9));
	str_asgn ("SKIP PRT ", (long) 9 , 
		g_astr0, (long) g_ind0 );
/* 000465 */
	g_astr0 = (char *) READKEY_ts;
	g_astr0 += ( 10-1);
	g_ind0 = min ( 50  - ( 10-1),  (15));
	str_asgn (LB_DEPT_ts, (long) 3 , 
		g_astr0, (long) g_ind0 );
/* 000466 */
	G_chan = 4 - 1;
	g_f[0] = (long) STRCONST;
	g_f[1] = (long) READKEY_ts;
	g_f[2] =  50 ;
	g_f[3] = 0L;
	/*GETT,UFBPTR[CH],HOLD,KEYIND,KEYREL,RELF,RECF,PIC,CNT*/
	g_ret = wb_rdgt (READ, ufbptr[G_chan], 0, 0,
	61, 1,  0, 0, 0);
	if (g_ret == EODX)
	  goto lSKIP_PRINT_TEST;
	else if (g_ret == DATAX)
	  wb_iserr ("DATA", ufbptr[G_chan]->prname);
	else if (g_ret == TIMEOX)
	  wb_iserr ("TIMEOUT", ufbptr[G_chan]->prname);
	else if (g_ret == IOERRX)
	  wb_iserr ("IOERR", ufbptr[G_chan]->prname);
/* 000467 */
	goto lLOAD_NEXT_REC;
/* 000469 */
lSKIP_PRINT_TEST:	;
/* 000473 */
	g_ind0 = wb_str (SC_DEPT_ts,  3 , "ALL",  3 );
	g_ind1 = (g_ind0 == 0);
	g_ind2 = wb_str (SC_BARCODE_FR_ts,  18 , "ALL",  3 );
	g_ind3 = (g_ind2 == 0);
	g_ind4 = (g_ind1 && g_ind3);
	g_ind5 = wb_str (LB_DEPT_ts,  3 , "101",  3 );
	g_ind6 = (g_ind5 == 0);
	g_ind7 = (g_ind4 && g_ind6);
	g_ind8 = ( (SCHEMA_is) -  2);
	g_ind9 = (g_ind8 == 0);
	g_ind10 = (g_ind7 && g_ind9);
	if (g_ind10)
	  {
		goto lLOAD_NEXT_REC;
	  }
/* 000477 */
	g_astr0 = (char *) SC_LOAD_FR_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 5  - ( 1-1),  (3));
	g_ind1 = wb_str (g_astr0,  g_ind0 , "ALL",  3 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lL30010;
	  }
/* 000479 */
	g_ind0 = ( (SCHEMA_is) -  2);
	g_ind1 = (g_ind0 == 0);
	g_astr0 = (char *) LB_LOAD_ts;
	g_astr0 += ( 1-1);
	g_ind2 = min ( 5  - ( 1-1),  (1));
	/*LOC = wb_pos(REV,OP,STR1,LEN1,STR2,LEN2);*/
	/*OP:<11,==16,>13,>=19,<=18,<>17,- 7*/
	g_ind3 = wb_pos (0, 16, "AS", (long) 2 , g_astr0, (long) g_ind2 );
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	l_BCD (g_ind3, g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind4 = cmp_NO(g_exp2,2);
	g_ind5 = (g_ind1 && g_ind4);
	if (g_ind5)
	  {
		goto lLOAD_NEXT_REC;
	  }
/* 000481 */
	g_ind0 = ( (SCHEMA_is) -  1);
	g_ind1 = (g_ind0 == 0);
	g_astr0 = (char *) LB_LOAD_ts;
	g_astr0 += ( 1-1);
	g_ind2 = min ( 5  - ( 1-1),  (1));
	/*LOC = wb_pos(REV,OP,STR1,LEN1,STR2,LEN2);*/
	/*OP:<11,==16,>13,>=19,<=18,<>17,- 7*/
	g_ind3 = wb_pos (0, 16, "S", (long) 1 , g_astr0, (long) g_ind2 );
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	l_BCD (g_ind3, g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind4 = cmp_NO(g_exp2,2);
	g_ind5 = (g_ind1 && g_ind4);
	if (g_ind5)
	  {
		goto lLOAD_NEXT_REC;
	  }
/* 000483 */
	g_ind0 = ( (SCHEMA_is) -  1);
	g_ind1 = (g_ind0 == 0);
	g_astr0 = (char *) LB_LOAD_ts;
	g_astr0 += ( 1-1);
	g_ind2 = min ( 5  - ( 1-1),  (1));
	/*LOC = wb_pos(REV,OP,STR1,LEN1,STR2,LEN2);*/
	/*OP:<11,==16,>13,>=19,<=18,<>17,- 7*/
	g_ind3 = wb_pos (0, 16, "A", (long) 1 , g_astr0, (long) g_ind2 );
	g_exp0[0] = 0L;
	g_exp0[1] = 0L;
	l_BCD (g_ind3, g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind4 = cmp_NO(g_exp2,2);
	g_ind5 = (g_ind1 && g_ind4);
	g_ind6 = wb_str (LB_DEPT_ts,  3 , "104",  3 );
	g_ind7 = (g_ind6 != 0);
	g_ind8 = (g_ind5 && g_ind7);
	if (g_ind8)
	  {
		goto lLOAD_NEXT_REC;
	  }
/* 000485 */
	goto lL30020;
/* 000487 */
lL30010:	;
/* 000487 */
	g_ind0 = wb_str (LB_LOAD_ts,  5 , SC_LOAD_FR_ts,  5 );
	g_ind1 = (g_ind0 < 0);
	g_ind2 = wb_str (LB_LOAD_ts,  5 , SC_LOAD_TO_ts,  5 );
	g_ind3 = (g_ind2 > 0);
	g_ind4 = (g_ind1 || g_ind3);
	if (g_ind4)
	  {
		goto lLOAD_NEXT_REC;
	  }
/* 000490 */
lL30020:	;
/* 000490 */
	g_ind0 = wb_str (LB_BARCODE_ts,  18 , SC_BARCODE_FR_ts,  18 );
	g_ind1 = (g_ind0 < 0);
	g_ind2 = wb_str (LB_BARCODE_ts,  18 , SC_BARCODE_TO_ts,  18 );
	g_ind3 = (g_ind2 > 0);
	g_ind4 = (g_ind1 || g_ind3);
	g_astr0 = (char *) SC_BARCODE_FR_ts;
	g_astr0 += ( 1-1);
	g_ind5 = min ( 18  - ( 1-1),  (3));
	g_ind6 = wb_str (g_astr0,  g_ind5 , "ALL",  3 );
	g_ind7 = (g_ind6 != 0);
	g_ind8 = (g_ind4 && g_ind7);
	if (g_ind8)
	  {
		goto lLOAD_NEXT_REC;
	  }
/* 000493 */
	goto lL30025;
/* 000495 */
lL30025:	;
/* 000495 */
	g_ind0 = wb_str (LB_SEQ_ts,  5 , SC_SEQ_FR_ts,  5 );
	g_ind1 = (g_ind0 < 0);
	g_ind2 = wb_str (LB_SEQ_ts,  5 , SC_SEQ_TO_ts,  5 );
	g_ind3 = (g_ind2 > 0);
	g_ind4 = (g_ind1 || g_ind3);
	g_astr0 = (char *) SC_SEQ_FR_ts;
	g_astr0 += ( 1-1);
	g_ind5 = min ( 5  - ( 1-1),  (3));
	g_ind6 = wb_str (g_astr0,  g_ind5 , "ALL",  3 );
	g_ind7 = (g_ind6 != 0);
	g_ind8 = (g_ind4 && g_ind7);
	if (g_ind8)
	  {
		goto lLOAD_NEXT_REC;
	  }
/* 000501 */
	G_stack[++G_scnt] = 22;
	goto lCHECK_REPAIR;
ret_022:	;
/* 000502 */
	g_ind0 = ( (LB_FOAM_is) -  1);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lLOAD_NEXT_REC;
	  }
/* 000509 */
	 (ERR_is) =  0;
/* 000524 */
	Call_args = 11;
	EWDPLA71 (&BEEN_HERE_IS, &LB_REC_TV, ufbptr[4-1], ufbptr[1-1], ufbptr[6-1], ufbptr[63-1], ufbptr[58-1], ufbptr[8-1], ufbptr[9-1], ufbptr[16-1], &ERR_IS);
/* 000526 */
	g_ind0 = ( (ERR_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 23;
		goto lPRINT_ERROR;
ret_023:	;
	  }
/* 000528 */
	g_ind0 = ( (LBL_is) +  1);
	 (LBL_is) =  g_ind0;
/* 000530 */
/* 000532 */
	g_ind0 = 25;
	l_BCD (g_ind0, g_exp0);
	g_ind0 =  ((LBL_is));
	l_BCD (g_ind0, g_exp1);
	wb_mod (g_exp1, g_exp0, g_exp2);
	g_exp3[0] = 0L;
	g_exp3[1] = 0L;
	addl ('-', g_exp2, g_exp3, g_exp4);
	g_ind0 = cmp_NO(g_exp4,6);
	if (g_ind0)
	  {
		goto lLOAD_NEXT_REC;
	  }
/* 000533 */
	l_BCD ( (LBL_is), g_exp0);
	g_astr0 = (char *) COUNT_ts;
	g_astr0 += ( 17-1);
	g_ind0 = min ( 22  - ( 17-1),  (5));
	wb_convnum (g_exp0, g_astr0,  g_ind0 , "#####", 8);
/* 000535 */
	Call_args = 1;
	SHOSTAT (&COUNT_TS);
/* 000536 */
	goto lLOAD_NEXT_REC;
/* 000538 */
lLOAD_DONE:	;
/* 000553 */
	INT_ISD0.p = (char *) INT_IS0;
	INT_IS0[0] = 99;
	Call_args = 11;
	EWDPLA71 (&BEEN_HERE_IS, &LB_REC_TV, ufbptr[4-1], ufbptr[1-1], ufbptr[6-1], ufbptr[63-1], ufbptr[58-1], ufbptr[8-1], ufbptr[9-1], ufbptr[16-1], &INT_ISD0);
/* 000555 */
	g_ind0 = ( (ERR_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		G_stack[++G_scnt] = 24;
		goto lPRINT_ERROR;
ret_024:	;
	  }
/* 000556 */
	G_stack[++G_scnt] = 25;
	goto lLOAD_RESULTS;
ret_025:	;
/* 000557 */
	goto lINPUTMODE;
/* 000559 */
lCHECK_REPAIR:	;
/* 000560 */
	 (LB_FOAM_is) =  0;
/* 000561 */
	g_ind0 = wb_str (LB_FOAM_ts,  1 , "Y",  1 );
	g_ind1 = (g_ind0 == 0);
	g_ind2 = wb_str (LB_FOAM_ts,  1 , "N",  1 );
	g_ind3 = (g_ind2 == 0);
	g_ind4 = (g_ind1 || g_ind3);
	if (g_ind4)
	  {
		goto lL30110;
	  }
/* 000563 */
	g_astr0 = (char *) LB_LOAD_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 5  - ( 1-1),  (1));
	/*LOC = wb_pos(REV,OP,STR1,LEN1,STR2,LEN2);*/
	/*OP:<11,==16,>13,>=19,<=18,<>17,- 7*/
	g_ind1 = wb_pos (0, 16, "AS", (long) 2 , g_astr0, (long) g_ind0 );
	 (P_is) =  g_ind1;
/* 000565 */
	g_astr0 = (char *) SC_BARCODE_FR_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 18  - ( 1-1),  (3));
	g_ind1 = wb_str (g_astr0,  g_ind0 , "ALL",  3 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lL30110;
	  }
/* 000567 */
	g_ind0 = ( (P_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL30110;
	  }
/* 000569 */
	 (LB_FOAM_is) =  1;
/* 000570 */
lL30110:	;
/* 000570 */
	goto Go_Sub_Ret;
/* 000573 */
/* 000580 */
/* 000585 */
lL35040:	;
/* 000585 */
/* 000588 */
/* 000594 */
	/* DEFFN Prime */
	goto f_s00101;
    f_000101:
	/* Handle parameters to DEFFN */ 
	g_ind0 = 1;  /* GSP_param index */
	              /*start w/ 1 to skip number of params */
	if (!GSP_param[0]--) /* test number of parameters*/
		wb_err("GOSUB' ERROR:Invalid number of parameters");
	/* Integer parameter */ 
	if ((GSP_param[g_ind0] & 0xF0) == 0x40)
	  {  /* passes an integer */
	    (FIELDNR_is) = GSP_param[++g_ind0];
	   g_ind0++;
	  }
	else if ((GSP_param[g_ind0] & 0xF0) == 0x80)
	  {  /* convert from BCD */
	   BCD_l(&GSP_param[++g_ind0], &g_ind1);
	    (FIELDNR_is) = g_ind1;
	   g_ind0 += 2;
	  }
	else 
		wb_err("GOSUB' ERROR:parameter mismatch");
	if (!GSP_param[0]--) /* test number of parameters*/
		wb_err("GOSUB' ERROR:Invalid number of parameters");
	/* Integer parameter */ 
	if ((GSP_param[g_ind0] & 0xF0) == 0x40)
	  {  /* passes an integer */
	    (EDIT_is) = GSP_param[++g_ind0];
	   g_ind0++;
	  }
	else if ((GSP_param[g_ind0] & 0xF0) == 0x80)
	  {  /* convert from BCD */
	   BCD_l(&GSP_param[++g_ind0], &g_ind2);
	    (EDIT_is) = g_ind2;
	   g_ind0 += 2;
	  }
	else 
		wb_err("GOSUB' ERROR:parameter mismatch");
    f_s00101:
	;
/* 000595 */
	/* GOSUB PRIME CALL */
	GSP_param[1] = INTCONST;
	GSP_param[2] = 1;
	GSP_param[3] = INTADR;
	GSP_param[4] =  (FIELDNR_is);
	GSP_param[0] = 2;   /* number of params */
	G_stack[++G_scnt] = 26;
	goto f_000050;
ret_026:	;
/* 000596 */
	G_stack[++G_scnt] = 27;
	goto lSET_PF1;
ret_027:	;
/* 000597 */
	g_ind0 = ( (FIELDNR_is) -  0);
	g_ind1 = (g_ind0 > 0);
	if (g_ind1)
	  {
		g_astr0 = g_str0;
		g_ind0 = wb_hex ("8c", g_astr0);
		wb_vinit (g_astr0, &LFAC_TV, NULL);
	  }
	else
	  {
		g_astr0 = g_str0;
		g_ind0 = wb_hex ("86", g_astr0);
		wb_vinit (g_astr0, &LFAC_TV, NULL);
	  }
/* 000599 */
	switch ( (FIELDNR_is))
	  {
		case 1: 
	G_stack[++G_scnt] = 28;
	goto lL40170;
ret_028:	;
		  break;
		case 2: 
	G_stack[++G_scnt] = 29;
	goto lL40160;
ret_029:	;
		  break;
		case 3: 
	G_stack[++G_scnt] = 30;
	goto lL40160;
ret_030:	;
		  break;
		case 4: 
	G_stack[++G_scnt] = 31;
	goto lL40160;
ret_031:	;
		  break;
		case 5: 
	G_stack[++G_scnt] = 32;
	goto lL40160;
ret_032:	;
		  break;
		case 6: 
	G_stack[++G_scnt] = 33;
	goto lL40170;
ret_033:	;
		  break;
		default:
		  break;
	  }
/* 000607 */
	goto lL40190;
/* 000609 */
	g_astr0 = (char *) (LFAC_tv) + (( (FIELDNR_is)-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	g_astr1 = g_str0;
	g_ind1 = wb_hex ("80", g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		g_astr0, (long) g_ind0 );
/* 000609 */
	goto Go_Sub_Ret;
/* 000610 */
lL40160:	;
/* 000610 */
	g_astr0 = (char *) (LFAC_tv) + (( (FIELDNR_is)-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	g_astr1 = g_str0;
	g_ind1 = wb_hex ("81", g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		g_astr0, (long) g_ind0 );
/* 000610 */
	goto Go_Sub_Ret;
/* 000611 */
lL40170:	;
/* 000611 */
	g_astr0 = (char *) (LFAC_tv) + (( (FIELDNR_is)-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	g_astr1 = g_str0;
	g_ind1 = wb_hex ("82", g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		g_astr0, (long) g_ind0 );
/* 000611 */
	goto Go_Sub_Ret;
/* 000613 */
lL40190:	;
/* 000613 */
	g_ind0 = 2;
	g_ind1 = 1;
	g_f[0] = (long) AT;
	g_f[1] = 2;
	g_f[2] = (long) g_ind1;
	g_f[3] = (long) g_ind0;
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("8c", g_astr0);
	/* skip FAC operation */
	g_f[4] = (long) FAC;
	g_f[5] = (long) g_astr0;
	g_f[6] = (long)  g_ind0 ;
	g_f[7] = (long) STRADR;
	g_f[8] = (long) PNAME_ts;
	g_f[9] = (long)  21 ;
	g_ind0 = 21;
	g_f[10] = (long) CH;
	g_f[11] = 1;
	g_f[12] = (long) g_ind0;
	g_ind0 = 66;
	g_ind1 = 1;
	g_f[13] = (long) AT;
	g_f[14] = 2;
	g_f[15] = (long) g_ind1;
	g_f[16] = (long) g_ind0;
	g_f[17] = (long) STRCONST;
	g_f[18] = (long) "Today:";
	g_f[19] = (long)  6 ;
	g_ind0 = 73;
	g_ind1 = 1;
	g_f[20] = (long) AT;
	g_f[21] = 2;
	g_f[22] = (long) g_ind1;
	g_f[23] = (long) g_ind0;
	g_astr1 = g_str1;
	g_ind0 = wb_hex ("8c", g_astr1);
	/* skip FAC operation */
	g_f[24] = (long) FAC;
	g_f[25] = (long) g_astr1;
	g_f[26] = (long)  g_ind0 ;
	g_f[27] = (long) STRADR;
	g_f[28] = (long) DATE_ts;
	g_f[29] = (long)  8 ;
	g_ind0 = 8;
	g_f[30] = (long) CH;
	g_f[31] = 1;
	g_f[32] = (long) g_ind0;
	g_ind0 = 24;
	g_ind1 = 1;
	g_f[33] = (long) AT;
	g_f[34] = 2;
	g_f[35] = (long) g_ind1;
	g_f[36] = (long) g_ind0;
	g_astr2 = g_str2;
	g_ind0 = wb_hex ("a4", g_astr2);
	/* skip FAC operation */
	g_f[37] = (long) FAC;
	g_f[38] = (long) g_astr2;
	g_f[39] = (long)  g_ind0 ;
	g_f[40] = (long) STRADR;
	g_f[41] = (long) APC_ts;
	g_f[42] = (long)  40 ;
	g_ind0 = 40;
	g_f[43] = (long) CH;
	g_f[44] = 1;
	g_f[45] = (long) g_ind0;
	g_ind0 = 2;
	g_ind1 = 2;
	g_f[46] = (long) AT;
	g_f[47] = 2;
	g_f[48] = (long) g_ind1;
	g_f[49] = (long) g_ind0;
	g_astr3 = g_str3;
	g_ind0 = wb_hex ("94", g_astr3);
	/* skip FAC operation */
	g_f[50] = (long) FAC;
	g_f[51] = (long) g_astr3;
	g_f[52] = (long)  g_ind0 ;
	g_f[53] = (long) STRADR;
	g_f[54] = (long) ERRORMSG_ts;
	g_f[55] = (long)  79 ;
	g_ind0 = 79;
	g_f[56] = (long) CH;
	g_f[57] = 1;
	g_f[58] = (long) g_ind0;
	g_ind0 = 2;
	g_ind1 = 3;
	g_f[59] = (long) AT;
	g_f[60] = 2;
	g_f[61] = (long) g_ind1;
	g_f[62] = (long) g_ind0;
	g_f[63] = (long) STRCONST;
	g_f[64] = (long) "Prod Beg/End Date:";
	g_f[65] = (long)  18 ;
	g_ind0 = 25;
	g_ind1 = 3;
	g_f[66] = (long) AT;
	g_f[67] = 2;
	g_f[68] = (long) g_ind1;
	g_f[69] = (long) g_ind0;
	g_astr4 = (char *) (LFAC_tv) + (( 1-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	/* skip FAC operation */
	g_f[70] = (long) FAC;
	g_f[71] = (long) g_astr4;
	g_f[72] = (long)  g_ind0 ;
	g_f[73] = (long) STRADR;
	g_f[74] = (long) SC_PRDDATE_ts;
	g_f[75] = (long)  10 ;
	g_ind0 = 10;
	g_f[76] = (long) CH;
	g_f[77] = 1;
	g_f[78] = (long) g_ind0;
	g_ind0 = 40;
	g_ind1 = 3;
	g_f[79] = (long) AT;
	g_f[80] = 2;
	g_f[81] = (long) g_ind1;
	g_f[82] = (long) g_ind0;
	g_astr5 = (char *) (LFAC_tv) + (( 1-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	/* skip FAC operation */
	g_f[83] = (long) FAC;
	g_f[84] = (long) g_astr5;
	g_f[85] = (long)  g_ind0 ;
	g_f[86] = (long) STRADR;
	g_f[87] = (long) SC_END_PRDDATE_ts;
	g_f[88] = (long)  10 ;
	g_ind0 = 10;
	g_f[89] = (long) CH;
	g_f[90] = 1;
	g_f[91] = (long) g_ind0;
	g_ind0 = 2;
	g_ind1 = 4;
	g_f[92] = (long) AT;
	g_f[93] = 2;
	g_f[94] = (long) g_ind1;
	g_f[95] = (long) g_ind0;
	g_f[96] = (long) STRCONST;
	g_f[97] = (long) "Dept. Code       :";
	g_f[98] = (long)  18 ;
	g_ind0 = 25;
	g_ind1 = 4;
	g_f[99] = (long) AT;
	g_f[100] = 2;
	g_f[101] = (long) g_ind1;
	g_f[102] = (long) g_ind0;
	g_astr6 = (char *) (LFAC_tv) + (( 2-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	/* skip FAC operation */
	g_f[103] = (long) FAC;
	g_f[104] = (long) g_astr6;
	g_f[105] = (long)  g_ind0 ;
	g_f[106] = (long) STRADR;
	g_f[107] = (long) SC_DEPT_ts;
	g_f[108] = (long)  3 ;
	g_ind0 = 3;
	g_f[109] = (long) CH;
	g_f[110] = 1;
	g_f[111] = (long) g_ind0;
	g_ind0 = 40;
	g_ind1 = 4;
	g_f[112] = (long) AT;
	g_f[113] = 2;
	g_f[114] = (long) g_ind1;
	g_f[115] = (long) g_ind0;
	g_astr7 = g_str4;
	g_ind0 = wb_hex ("84", g_astr7);
	/* skip FAC operation */
	g_f[116] = (long) FAC;
	g_f[117] = (long) g_astr7;
	g_f[118] = (long)  g_ind0 ;
	g_f[119] = (long) STRADR;
	g_f[120] = (long) SC_DEPT_D_ts;
	g_f[121] = (long)  30 ;
	g_ind0 = 30;
	g_f[122] = (long) CH;
	g_f[123] = 1;
	g_f[124] = (long) g_ind0;
	g_ind0 = 2;
	g_ind1 = 5;
	g_f[125] = (long) AT;
	g_f[126] = 2;
	g_f[127] = (long) g_ind1;
	g_f[128] = (long) g_ind0;
	g_f[129] = (long) STRCONST;
	g_f[130] = (long) "Shift Code       :";
	g_f[131] = (long)  18 ;
	g_ind0 = 25;
	g_ind1 = 5;
	g_f[132] = (long) AT;
	g_f[133] = 2;
	g_f[134] = (long) g_ind1;
	g_f[135] = (long) g_ind0;
	g_astr8 = (char *) (LFAC_tv) + (( 3-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	/* skip FAC operation */
	g_f[136] = (long) FAC;
	g_f[137] = (long) g_astr8;
	g_f[138] = (long)  g_ind0 ;
	g_f[139] = (long) STRADR;
	g_f[140] = (long) SC_SHIFT_ts;
	g_f[141] = (long)  2 ;
	g_ind0 = 2;
	g_f[142] = (long) CH;
	g_f[143] = 1;
	g_f[144] = (long) g_ind0;
	g_ind0 = 2;
	g_ind1 = 6;
	g_f[145] = (long) AT;
	g_f[146] = 2;
	g_f[147] = (long) g_ind1;
	g_f[148] = (long) g_ind0;
	g_f[149] = (long) STRCONST;
	g_f[150] = (long) "Load No. Range   :";
	g_f[151] = (long)  18 ;
	g_ind0 = 25;
	g_ind1 = 6;
	g_f[152] = (long) AT;
	g_f[153] = 2;
	g_f[154] = (long) g_ind1;
	g_f[155] = (long) g_ind0;
	g_astr9 = (char *) (LFAC_tv) + (( 4-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	/* skip FAC operation */
	g_f[156] = (long) FAC;
	g_f[157] = (long) g_astr9;
	g_f[158] = (long)  g_ind0 ;
	g_f[159] = (long) STRADR;
	g_f[160] = (long) SC_LOAD_FR_ts;
	g_f[161] = (long)  5 ;
	g_ind0 = 5;
	g_f[162] = (long) CH;
	g_f[163] = 1;
	g_f[164] = (long) g_ind0;
	g_ind0 = 45;
	g_ind1 = 6;
	g_f[165] = (long) AT;
	g_f[166] = 2;
	g_f[167] = (long) g_ind1;
	g_f[168] = (long) g_ind0;
	g_astr10 = (char *) (LFAC_tv) + (( 4-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	/* skip FAC operation */
	g_f[169] = (long) FAC;
	g_f[170] = (long) g_astr10;
	g_f[171] = (long)  g_ind0 ;
	g_f[172] = (long) STRADR;
	g_f[173] = (long) SC_LOAD_TO_ts;
	g_f[174] = (long)  5 ;
	g_ind0 = 5;
	g_f[175] = (long) CH;
	g_f[176] = 1;
	g_f[177] = (long) g_ind0;
	g_ind0 = 2;
	g_ind1 = 7;
	g_f[178] = (long) AT;
	g_f[179] = 2;
	g_f[180] = (long) g_ind1;
	g_f[181] = (long) g_ind0;
	g_f[182] = (long) STRCONST;
	g_f[183] = (long) "Barcode No. Range:";
	g_f[184] = (long)  18 ;
	g_ind0 = 25;
	g_ind1 = 7;
	g_f[185] = (long) AT;
	g_f[186] = 2;
	g_f[187] = (long) g_ind1;
	g_f[188] = (long) g_ind0;
	g_astr11 = (char *) (LFAC_tv) + (( 5-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	/* skip FAC operation */
	g_f[189] = (long) FAC;
	g_f[190] = (long) g_astr11;
	g_f[191] = (long)  g_ind0 ;
	g_f[192] = (long) STRADR;
	g_f[193] = (long) SC_BARCODE_FR_ts;
	g_f[194] = (long)  18 ;
	g_ind0 = 18;
	g_f[195] = (long) CH;
	g_f[196] = 1;
	g_f[197] = (long) g_ind0;
	g_ind0 = 45;
	g_ind1 = 7;
	g_f[198] = (long) AT;
	g_f[199] = 2;
	g_f[200] = (long) g_ind1;
	g_f[201] = (long) g_ind0;
	g_astr12 = (char *) (LFAC_tv) + (( 5-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	/* skip FAC operation */
	g_f[202] = (long) FAC;
	g_f[203] = (long) g_astr12;
	g_f[204] = (long)  g_ind0 ;
	g_f[205] = (long) STRADR;
	g_f[206] = (long) SC_BARCODE_TO_ts;
	g_f[207] = (long)  18 ;
	g_ind0 = 18;
	g_f[208] = (long) CH;
	g_f[209] = 1;
	g_f[210] = (long) g_ind0;
	g_ind0 = 2;
	g_ind1 = 8;
	g_f[211] = (long) AT;
	g_f[212] = 2;
	g_f[213] = (long) g_ind1;
	g_f[214] = (long) g_ind0;
	g_f[215] = (long) STRCONST;
	g_f[216] = (long) "Sequence Range   :";
	g_f[217] = (long)  18 ;
	g_ind0 = 25;
	g_ind1 = 8;
	g_f[218] = (long) AT;
	g_f[219] = 2;
	g_f[220] = (long) g_ind1;
	g_f[221] = (long) g_ind0;
	g_astr13 = (char *) (LFAC_tv) + (( 6-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	/* skip FAC operation */
	g_f[222] = (long) FAC;
	g_f[223] = (long) g_astr13;
	g_f[224] = (long)  g_ind0 ;
	g_f[225] = (long) STRADR;
	g_f[226] = (long) SC_SEQ_FR_ts;
	g_f[227] = (long)  5 ;
	g_ind0 = 5;
	g_f[228] = (long) CH;
	g_f[229] = 1;
	g_f[230] = (long) g_ind0;
	g_ind0 = 45;
	g_ind1 = 8;
	g_f[231] = (long) AT;
	g_f[232] = 2;
	g_f[233] = (long) g_ind1;
	g_f[234] = (long) g_ind0;
	g_astr14 = (char *) (LFAC_tv) + (( 6-1) * LFAC_TV.len);
	g_ind0 = LFAC_TV.len;
	/* skip FAC operation */
	g_f[235] = (long) FAC;
	g_f[236] = (long) g_astr14;
	g_f[237] = (long)  g_ind0 ;
	g_f[238] = (long) STRADR;
	g_f[239] = (long) SC_SEQ_TO_ts;
	g_f[240] = (long)  5 ;
	g_ind0 = 5;
	g_f[241] = (long) CH;
	g_f[242] = 1;
	g_f[243] = (long) g_ind0;
	g_ind0 = 2;
	g_ind1 = 21;
	g_f[244] = (long) AT;
	g_f[245] = 2;
	g_f[246] = (long) g_ind1;
	g_f[247] = (long) g_ind0;
	g_astr15 = g_str5;
	g_ind0 = wb_hex ("a4", g_astr15);
	/* skip FAC operation */
	g_f[248] = (long) FAC;
	g_f[249] = (long) g_astr15;
	g_f[250] = (long)  g_ind0 ;
	g_f[251] = (long) STRADR;
	g_f[252] = (long) INPMESSAGE_ts;
	g_f[253] = (long)  79 ;
	g_ind0 = 79;
	g_f[254] = (long) CH;
	g_f[255] = 1;
	g_f[256] = (long) g_ind0;
	g_ind0 = 2;
	g_ind1 = 22;
	g_f[257] = (long) AT;
	g_f[258] = 2;
	g_f[259] = (long) g_ind1;
	g_f[260] = (long) g_ind0;
	g_astr16 = g_str6;
	g_ind0 = wb_hex ("8c", g_astr16);
	/* skip FAC operation */
	g_f[261] = (long) FAC;
	g_f[262] = (long) g_astr16;
	g_f[263] = (long)  g_ind0 ;
	g_astr17 = (char *) (PF_tv) + (( 1-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	g_f[264] = (long) STRADR;
	g_f[265] = (long) g_astr17;
	g_f[266] = (long)  g_ind0 ;
	g_ind0 = 79;
	g_f[267] = (long) CH;
	g_f[268] = 1;
	g_f[269] = (long) g_ind0;
	g_ind0 = 2;
	g_ind1 = 23;
	g_f[270] = (long) AT;
	g_f[271] = 2;
	g_f[272] = (long) g_ind1;
	g_f[273] = (long) g_ind0;
	g_astr18 = g_str7;
	g_ind0 = wb_hex ("8c", g_astr18);
	/* skip FAC operation */
	g_f[274] = (long) FAC;
	g_f[275] = (long) g_astr18;
	g_f[276] = (long)  g_ind0 ;
	g_astr19 = (char *) (PF_tv) + (( 2-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	g_f[277] = (long) STRADR;
	g_f[278] = (long) g_astr19;
	g_f[279] = (long)  g_ind0 ;
	g_ind0 = 79;
	g_f[280] = (long) CH;
	g_f[281] = 1;
	g_f[282] = (long) g_ind0;
	g_ind0 = 2;
	g_ind1 = 24;
	g_f[283] = (long) AT;
	g_f[284] = 2;
	g_f[285] = (long) g_ind1;
	g_f[286] = (long) g_ind0;
	g_astr20 = g_str8;
	g_ind0 = wb_hex ("8c", g_astr20);
	/* skip FAC operation */
	g_f[287] = (long) FAC;
	g_f[288] = (long) g_astr20;
	g_f[289] = (long)  g_ind0 ;
	g_astr21 = (char *) (PF_tv) + (( 3-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	g_f[290] = (long) STRADR;
	g_f[291] = (long) g_astr21;
	g_f[292] = (long)  g_ind0 ;
	g_ind0 = 79;
	g_f[293] = (long) CH;
	g_f[294] = 1;
	g_f[295] = (long) g_ind0;
		g_f[296] = (long) STRADR;
	g_f[297] = (long) PFKEYS_ts;
	g_f[298] = (long)  32 ;
		g_f[299] = (long) INTADR;
	g_f[300] = (long) &((KEYHIT_is));
	g_f[301] = 0L;
	g_ret = wb_accept (90, 1, 1, 0);
/* 000651 */
	g_ind0 = ( (KEYHIT_is) -  15);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL40420;
	  }
/* 000652 */
	PRNTSCRN ();
/* 000653 */
	goto lL40190;
/* 000655 */
lL40420:	;
/* 000655 */
	wb_close (1, WS);
/* 000656 */
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
, &(U3_is)
, g_c[g_ctmp+3]
, I_tv
, (CURSOR_iv)
);
/* 000657 */
	goto Go_Sub_Ret;
/* 000659 */
lSET_PF1:	;
/* 000660 */
	g_ind0 = ( (EDIT_is) -  2);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL40610;
	  }
/* 000661 */
	g_astr0 = (char *) (PF_tv) + (( 1-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	str_asgn ("(1)Start Over    (4)Previous Field                                             ", (long) 79 , 
		g_astr0, (long) g_ind0 );
/* 000663 */
	g_astr0 = (char *) (PF_tv) + (( 2-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	str_asgn ("                                                               (15)Print Screen", (long) 79 , 
		g_astr0, (long) g_ind0 );
/* 000665 */
	g_astr0 = (char *) (PF_tv) + (( 3-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	str_asgn ("                                                               (16)Exit Program", (long) 79 , 
		g_astr0, (long) g_ind0 );
/* 000667 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("01ffff04ffffffffffffffffffff0f1000", g_astr0);
	str_asgn (g_astr0, (long) g_ind0 , 
		PFKEYS_ts, (long) 32 );
/* 000668 */
	g_ind0 = ( (FIELDNR_is) -  1);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL40570;
	  }
/* 000669 */
	g_astr0 = (char *) (PF_tv) + (( 3-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	g_astr0 += ( 64-1);
	g_ind1 = ( g_ind0  - ( 64-1));
	str_asgn (" ", (long) 1 , 
		g_astr0, (long) g_ind1 );
/* 000669 */
	g_astr0 = (char *) PFKEYS_ts;
	g_astr0 += ( 16-1);
	g_ind0 = min ( 32  - ( 16-1),  (1));
	g_astr1 = g_str0;
	g_ind1 = wb_hex ("ff", g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		g_astr0, (long) g_ind0 );
/* 000670 */
lL40570:	;
/* 000670 */
	g_ind0 = ( (FIELDNR_is) -  1);
	g_ind1 = (g_ind0 > 0);
	if (g_ind1)
	  {
		goto lL40590;
	  }
/* 000671 */
	g_astr0 = (char *) (PF_tv) + (( 1-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	g_astr0 += ( 18-1);
	g_ind1 = min ( g_ind0  - ( 18-1),  (26));
	str_asgn (" ", (long) 1 , 
		g_astr0, (long) g_ind1 );
/* 000671 */
	g_astr0 = (char *) PFKEYS_ts;
	g_astr0 += ( 4-1);
	g_ind0 = min ( 32  - ( 4-1),  (1));
	g_astr1 = g_str0;
	g_ind1 = wb_hex ("ff", g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		g_astr0, (long) g_ind0 );
/* 000672 */
lL40590:	;
/* 000672 */
	goto Go_Sub_Ret;
/* 000674 */
lL40610:	;
/* 000674 */
	g_ind0 = ( (FIELDNR_is) -  0);
	g_ind1 = (g_ind0 > 0);
	if (g_ind1)
	  {
		goto lL40700;
	  }
/* 000675 */
	g_astr0 = (char *) (PF_tv) + (( 1-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	str_asgn ("(1)Start Over                                                                  ", (long) 79 , 
		g_astr0, (long) g_ind0 );
/* 000677 */
	g_astr0 = (char *) (PF_tv) + (( 2-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	str_asgn ("                                                              (15)Print Screen ", (long) 79 , 
		g_astr0, (long) g_ind0 );
/* 000679 */
	g_astr0 = (char *) (PF_tv) + (( 3-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	str_asgn ("                                                              (16)PRINT LABELS ", (long) 79 , 
		g_astr0, (long) g_ind0 );
/* 000681 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("01ffffffffffffffffffffffffff0f1000", g_astr0);
	str_asgn (g_astr0, (long) g_ind0 , 
		PFKEYS_ts, (long) 32 );
/* 000682 */
	goto Go_Sub_Ret;
/* 000683 */
lL40700:	;
/* 000684 */
	g_astr0 = (char *) (PF_tv) + (( 1-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	str_asgn ("(1)Start Over                                                                  ", (long) 79 , 
		g_astr0, (long) g_ind0 );
/* 000686 */
	g_astr0 = (char *) (PF_tv) + (( 2-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	str_asgn ("                                                                               ", (long) 79 , 
		g_astr0, (long) g_ind0 );
/* 000688 */
	g_astr0 = (char *) (PF_tv) + (( 3-1) * PF_TV.len);
	g_ind0 = PF_TV.len;
	str_asgn ("                                                                               ", (long) 79 , 
		g_astr0, (long) g_ind0 );
/* 000690 */
	g_astr0 = g_str0;
	g_ind0 = wb_hex ("01ffffffffffffffffffffffffffffff00", g_astr0);
	str_asgn (g_astr0, (long) g_ind0 , 
		PFKEYS_ts, (long) 32 );
/* 000691 */
	goto Go_Sub_Ret;
/* 000693 */
/* 000699 */
	/* DEFFN Prime */
	goto f_s00151;
    f_000151:
	/* Handle parameters to DEFFN */ 
	g_ind0 = 1;  /* GSP_param index */
	              /*start w/ 1 to skip number of params */
	if (!GSP_param[0]--) /* test number of parameters*/
		wb_err("GOSUB' ERROR:Invalid number of parameters");
	/* Integer parameter */ 
	if ((GSP_param[g_ind0] & 0xF0) == 0x40)
	  {  /* passes an integer */
	    (FIELDNR_is) = GSP_param[++g_ind0];
	   g_ind0++;
	  }
	else if ((GSP_param[g_ind0] & 0xF0) == 0x80)
	  {  /* convert from BCD */
	   BCD_l(&GSP_param[++g_ind0], &g_ind1);
	    (FIELDNR_is) = g_ind1;
	   g_ind0 += 2;
	  }
	else 
		wb_err("GOSUB' ERROR:parameter mismatch");
    f_s00151:
	;
/* 000700 */
	str_asgn (" ", (long) 1 , 
		ERRORMSG_ts, (long) 79 );
/* 000701 */
	switch ( (FIELDNR_is))
	  {
		case 1: 
	G_stack[++G_scnt] = 34;
	goto lL50010;
ret_034:	;
		  break;
		case 2: 
	G_stack[++G_scnt] = 35;
	goto lL50050;
ret_035:	;
		  break;
		case 3: 
	G_stack[++G_scnt] = 36;
	goto lL50080;
ret_036:	;
		  break;
		case 4: 
	G_stack[++G_scnt] = 37;
	goto lL50090;
ret_037:	;
		  break;
		case 5: 
	G_stack[++G_scnt] = 38;
	goto lL50200;
ret_038:	;
		  break;
		case 6: 
	G_stack[++G_scnt] = 39;
	goto lL50300;
ret_039:	;
		  break;
		default:
		  break;
	  }
/* 000708 */
	goto Go_Sub_Ret;
/* 000710 */
lL50010:	;
/* 000710 */
/* 000711 */
	INT_ISD0.p = (char *) INT_IS0;
	INT_IS0[0] = 0;
	Call_args = 3;
	DATEOKC (&SC_PRDDATE_TS, &INT_ISD0, &ERRORMSG_TS);
/* 000713 */
	g_ind0 = wb_len ((long)0, SC_END_PRDDATE_ts,  10 );
	g_exp0[0] = 0x41500000L;
	g_exp0[1] = 0L;
	l_BCD (g_ind0, g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind1 = cmp_NO(g_exp2,0);
	if (g_ind1)
	  {
		str_asgn (SC_PRDDATE_ts, (long) 10 , 
			SC_END_PRDDATE_ts, (long) 10 );
	  }
/* 000716 */
	INT_ISD0.p = (char *) INT_IS0;
	INT_IS0[0] = 0;
	Call_args = 3;
	DATEOKC (&SC_END_PRDDATE_TS, &INT_ISD0, &ERRORMSG_TS);
/* 000718 */
	g_ind0 = wb_str (SC_PRDDATE_ts,  10 , SC_END_PRDDATE_ts,  10 );
	g_ind1 = (g_ind0 > 0);
	if (g_ind1)
	  {
		goto lL50015;
	  }
/* 000720 */
	g_ind0 = wb_str (SC_END_PRDDATE_ts,  10 , SC_PRDDATE_ts,  10 );
	g_ind1 = (g_ind0 < 0);
	if (g_ind1)
	  {
		goto lL50020;
	  }
/* 000721 */
	goto Go_Sub_Ret;
/* 000722 */
lL50015:	;
/* 000722 */
	str_asgn ("(Error) Invalid Beginning Production date?", (long) 42 , 
		ERRORMSG_ts, (long) 79 );
/* 000723 */
	G_stack[++G_scnt] = 40;
	goto lERROR_PROMPT;
ret_040:	;
/* 000724 */
	wb_vinit (" ", &SC_PRDDATE_TS, &SC_END_PRDDATE_TS, NULL);
/* 000725 */
	goto Go_Sub_Ret;
/* 000726 */
lL50020:	;
/* 000726 */
	str_asgn ("(Error) Invalid Ending Production date?", (long) 39 , 
		ERRORMSG_ts, (long) 79 );
/* 000727 */
	G_stack[++G_scnt] = 41;
	goto lERROR_PROMPT;
ret_041:	;
/* 000728 */
	wb_vinit (" ", &SC_PRDDATE_TS, &SC_END_PRDDATE_TS, NULL);
/* 000729 */
	goto Go_Sub_Ret;
/* 000732 */
lL50050:	;
/* 000732 */
/* 000733 */
	wb_vinit (" ", &SC_DEPT_D_TS, NULL);
/* 000734 */
	g_ind0 = wb_str (SC_DEPT_ts,  3 , " ",  1 );
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL50055;
	  }
/* 000735 */
	str_asgn ("ALL", (long) 3 , 
		SC_DEPT_ts, (long) 3 );
/* 000737 */
lL50055:	;
/* 000737 */
	g_ind0 = wb_str (SC_DEPT_ts,  3 , "ALL",  3 );
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL50060;
	  }
/* 000738 */
	str_asgn ("*** All Departments", (long) 19 , 
		SC_DEPT_D_ts, (long) 30 );
/* 000739 */
	goto Go_Sub_Ret;
/* 000740 */
lL50060:	;
/* 000740 */
	G_stack[++G_scnt] = 42;
	goto lCHECK_DEPT;
ret_042:	;
/* 000741 */
	g_ind0 = ( (DEPT_is) -  0);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL50070;
	  }
/* 000742 */
	str_asgn (DESC_ts, (long) 30 , 
		SC_DEPT_D_ts, (long) 30 );
/* 000743 */
	goto Go_Sub_Ret;
/* 000745 */
lL50070:	;
/* 000745 */
	str_asgn ("(Error) Invalid Department Code", (long) 31 , 
		ERRORMSG_ts, (long) 79 );
/* 000746 */
	G_stack[++G_scnt] = 43;
	goto lERROR_PROMPT;
ret_043:	;
/* 000747 */
	wb_vinit (" ", &SC_DEPT_TS, &SC_DEPT_D_TS, NULL);
/* 000748 */
	goto Go_Sub_Ret;
/* 000750 */
lL50080:	;
/* 000750 */
/* 000751 */
	g_ind0 = wb_str (SC_SHIFT_ts,  2 , " ",  1 );
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		str_asgn ("AA", (long) 2 , 
			SC_SHIFT_ts, (long) 2 );
	  }
/* 000752 */
	goto Go_Sub_Ret;
/* 000754 */
lL50090:	;
/* 000754 */
/* 000755 */
	g_astr0 = (char *) SC_LOAD_FR_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 5  - ( 1-1),  (1));
	g_ind1 = wb_str (g_astr0,  g_ind0 , " ",  1 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lL50095;
	  }
/* 000756 */
	str_asgn ("ALL  ", (long) 5 , 
		SC_LOAD_FR_ts, (long) 5 );
/* 000757 */
	str_asgn (SC_LOAD_FR_ts, (long) 5 , 
		SC_LOAD_TO_ts, (long) 5 );
/* 000759 */
lL50095:	;
/* 000759 */
	g_astr0 = (char *) SC_LOAD_FR_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 5  - ( 1-1),  (3));
	g_ind1 = wb_str (g_astr0,  g_ind0 , "ALL",  3 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lL50098;
	  }
/* 000760 */
	str_asgn ("ALL  ", (long) 5 , 
		SC_LOAD_TO_ts, (long) 5 );
/* 000761 */
	goto Go_Sub_Ret;
/* 000762 */
lL50098:	;
/* 000762 */
	g_ind0 = wb_str (SC_LOAD_TO_ts,  5 , " ",  1 );
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		str_asgn (SC_LOAD_FR_ts, (long) 5 , 
			SC_LOAD_TO_ts, (long) 5 );
	  }
/* 000763 */
	g_ind0 = wb_str (SC_LOAD_FR_ts,  5 , SC_LOAD_TO_ts,  5 );
	g_ind1 = (g_ind0 > 0);
	if (g_ind1)
	  {
		goto lL50100;
	  }
/* 000764 */
	goto Go_Sub_Ret;
/* 000765 */
lL50100:	;
/* 000765 */
	str_asgn ("'TO' Load No. must be > or = 'FROM' Load No.", (long) 44 , 
		ERRORMSG_ts, (long) 79 );
/* 000766 */
	G_stack[++G_scnt] = 44;
	goto lERROR_PROMPT;
ret_044:	;
/* 000767 */
	wb_vinit (" ", &SC_LOAD_FR_TS, &SC_LOAD_TO_TS, NULL);
/* 000768 */
	goto Go_Sub_Ret;
/* 000770 */
lL50200:	;
/* 000770 */
/* 000771 */
	g_astr0 = (char *) SC_BARCODE_FR_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 18  - ( 1-1),  (1));
	g_ind1 = wb_str (g_astr0,  g_ind0 , " ",  1 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lL50205;
	  }
/* 000772 */
	str_asgn ("ALL Barcodes      ", (long) 18 , 
		SC_BARCODE_FR_ts, (long) 18 );
/* 000773 */
	str_asgn (SC_BARCODE_FR_ts, (long) 18 , 
		SC_BARCODE_TO_ts, (long) 18 );
/* 000775 */
lL50205:	;
/* 000775 */
	g_astr0 = (char *) SC_BARCODE_FR_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 18  - ( 1-1),  (3));
	g_ind1 = wb_str (g_astr0,  g_ind0 , "ALL",  3 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lL50210;
	  }
/* 000776 */
	str_asgn (SC_BARCODE_FR_ts, (long) 18 , 
		SC_BARCODE_TO_ts, (long) 18 );
/* 000778 */
	g_ind0 = wb_str (USERID_ts,  3 , "WWW",  3 );
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL50215;
	  }
/* 000779 */
	goto Go_Sub_Ret;
/* 000781 */
lL50210:	;
/* 000781 */
	g_ind0 = wb_len ((long)0, SC_BARCODE_TO_ts,  18 );
	g_exp0[0] = 0x41300000L;
	g_exp0[1] = 0L;
	l_BCD (g_ind0, g_exp1);
	addl ('-', g_exp1, g_exp0, g_exp2);
	g_ind1 = cmp_NO(g_exp2,0);
	if (g_ind1)
	  {
		str_asgn (SC_BARCODE_FR_ts, (long) 18 , 
			SC_BARCODE_TO_ts, (long) 18 );
	  }
/* 000784 */
	str_asgn (SC_BARCODE_FR_ts, (long) 18 , 
		LB_BARCODE_ts, (long) 18 );
/* 000785 */
	G_stack[++G_scnt] = 45;
	goto lCHECK_BARCODE;
ret_045:	;
/* 000786 */
	g_ind0 = ( (BARCODE_is) -  0);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL50230;
	  }
/* 000788 */
	str_asgn (SC_BARCODE_TO_ts, (long) 18 , 
		LB_BARCODE_ts, (long) 18 );
/* 000789 */
	G_stack[++G_scnt] = 46;
	goto lCHECK_BARCODE;
ret_046:	;
/* 000790 */
	g_ind0 = ( (BARCODE_is) -  0);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto lL50230;
	  }
/* 000792 */
	g_ind0 = wb_str (SC_BARCODE_FR_ts,  18 , SC_BARCODE_TO_ts,  18 );
	g_ind1 = (g_ind0 > 0);
	if (g_ind1)
	  {
		goto lL50220;
	  }
/* 000794 */
lL50215:	;
/* 000795 */
	g_ind0 = wb_str (USERID_ts,  3 , "WWW",  3 );
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto Go_Sub_Ret;
	  }
/* 000796 */
	g_astr0 = (char *) SC_BARCODE_FR_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 18  - ( 1-1),  (3));
	g_ind1 = wb_str (g_astr0,  g_ind0 , "ALL",  3 );
	g_ind2 = (g_ind1 == 0);
	if (g_ind2)
	  {
		goto lL50250;
	  }
/* 000797 */
	g_ind0 = wb_str (SC_BARCODE_TO_ts,  18 , SC_BARCODE_FR_ts,  18 );
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL50250;
	  }
/* 000799 */
	goto Go_Sub_Ret;
/* 000800 */
lL50220:	;
/* 000800 */
	str_asgn ("'TO' Barcode No. must be > or = 'FROM' Barcode No.", (long) 50 , 
		ERRORMSG_ts, (long) 79 );
/* 000801 */
	G_stack[++G_scnt] = 47;
	goto lERROR_PROMPT;
ret_047:	;
/* 000802 */
	wb_vinit (" ", &SC_BARCODE_FR_TS, &SC_BARCODE_TO_TS, NULL);
/* 000803 */
	goto Go_Sub_Ret;
/* 000804 */
lL50230:	;
/* 000804 */
	str_asgn ("(Error) Invalid Barcode, or Not on File", (long) 39 , 
		ERRORMSG_ts, (long) 79 );
/* 000805 */
	G_stack[++G_scnt] = 48;
	goto lERROR_PROMPT;
ret_048:	;
/* 000806 */
	wb_vinit (" ", &SC_BARCODE_FR_TS, &SC_BARCODE_TO_TS, NULL);
/* 000807 */
	goto Go_Sub_Ret;
/* 000809 */
lL50250:	;
/* 000809 */
	str_asgn ("(Error) Invalid Barcode, Can Only Print Single Label?", (long) 53 , 
		ERRORMSG_ts, (long) 79 );
/* 000810 */
	G_stack[++G_scnt] = 49;
	goto lERROR_PROMPT;
ret_049:	;
/* 000811 */
	wb_vinit (" ", &SC_BARCODE_FR_TS, &SC_BARCODE_TO_TS, NULL);
/* 000812 */
	goto Go_Sub_Ret;
/* 000814 */
lL50300:	;
/* 000814 */
/* 000815 */
	g_astr0 = (char *) SC_SEQ_FR_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 5  - ( 1-1),  (1));
	g_ind1 = wb_str (g_astr0,  g_ind0 , " ",  1 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lL50305;
	  }
/* 000816 */
	str_asgn ("ALL ", (long) 4 , 
		SC_SEQ_FR_ts, (long) 5 );
/* 000817 */
	str_asgn (SC_SEQ_FR_ts, (long) 5 , 
		SC_SEQ_TO_ts, (long) 5 );
/* 000819 */
lL50305:	;
/* 000819 */
	g_astr0 = (char *) SC_SEQ_FR_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 5  - ( 1-1),  (3));
	g_ind1 = wb_str (g_astr0,  g_ind0 , "ALL",  3 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lL50310;
	  }
/* 000820 */
	str_asgn (SC_SEQ_FR_ts, (long) 5 , 
		SC_SEQ_TO_ts, (long) 5 );
/* 000821 */
	goto Go_Sub_Ret;
/* 000823 */
lL50310:	;
/* 000823 */
	g_ind0 = wb_str (SC_SEQ_FR_ts,  5 , SC_SEQ_TO_ts,  5 );
	g_ind1 = (g_ind0 > 0);
	if (g_ind1)
	  {
		goto lL50320;
	  }
/* 000825 */
	g_ret = wb_convstr (SC_SEQ_TO_ts,  5 , g_exp0);
if (g_ret == DATAX)
	  goto lL50330;
	else 
	{  
		BCD_l (g_exp0, &(SCSEQ_is));
	}  
/* 000826 */
	g_ret = wb_convstr (SC_SEQ_FR_ts,  5 , g_exp0);
if (g_ret == DATAX)
	  goto lL50330;
	else 
	{  
		BCD_l (g_exp0, &(SCSEQ_is));
	}  
/* 000827 */
	goto Go_Sub_Ret;
/* 000829 */
lL50320:	;
/* 000829 */
	str_asgn ("'TO' Sequence No. must be > or = 'FROM' Sequence No.", (long) 52 , 
		ERRORMSG_ts, (long) 79 );
/* 000830 */
	G_stack[++G_scnt] = 50;
	goto lERROR_PROMPT;
ret_050:	;
/* 000831 */
	wb_vinit (" ", &SC_SEQ_FR_TS, &SC_SEQ_TO_TS, NULL);
/* 000832 */
	goto Go_Sub_Ret;
/* 000834 */
lL50330:	;
/* 000834 */
	str_asgn ("Sequence is not numeric", (long) 23 , 
		ERRORMSG_ts, (long) 79 );
/* 000835 */
	G_stack[++G_scnt] = 51;
	goto lERROR_PROMPT;
ret_051:	;
/* 000836 */
	wb_vinit (" ", &SC_SEQ_FR_TS, &SC_SEQ_TO_TS, NULL);
/* 000837 */
	goto Go_Sub_Ret;
/* 000839 */
lCHECK_DEPT:	;
/* 000840 */
	 (DEPT_is) =  0;
/* 000841 */
	wb_vinit (" ", &READKEY_TS, &DESC_TS, NULL);
/* 000842 */
	g_astr0 = (char *) READKEY_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 50  - ( 1-1),  (9));
	str_asgn ("PLAN DEPT", (long) 9 , 
		g_astr0, (long) g_ind0 );
/* 000843 */
	g_astr0 = (char *) READKEY_ts;
	g_astr0 += ( 10-1);
	g_ind0 = min ( 50  - ( 10-1),  (15));
	str_asgn (SC_DEPT_ts, (long) 3 , 
		g_astr0, (long) g_ind0 );
/* 000844 */
	G_chan = 4 - 1;
	g_ind0 = 25;
	g_f[0] = (long) POS;
	g_f[1] = 1L;
	g_f[2] = (long) g_ind0;
	g_ind1 = 30;
	g_f[3] = (long) CH;
	g_f[4] = 1L;
	g_f[5] = (long) g_ind1;
	g_f[6] = (long) STRCONST;
	g_f[7] = (long) READKEY_ts;
	g_f[8] =  50 ;
	g_f[9] = (long) STRADR;
	g_f[10] = (long) (DESC_ts);
	g_f[11] = (long)  30 ;
	g_f[12] = 0L;
	/*GETT,UFBPTR[CH],HOLD,KEYIND,KEYREL,RELF,RECF,PIC,CNT*/
	g_ret = wb_rdgt (READ, ufbptr[G_chan], 0, 0,
	61, 1,  0, 2, 1);
	if (g_ret == EODX)
	  goto lL51010;
	else if (g_ret == DATAX)
	  wb_iserr ("DATA", ufbptr[G_chan]->prname);
	else if (g_ret == TIMEOX)
	  wb_iserr ("TIMEOUT", ufbptr[G_chan]->prname);
	else if (g_ret == IOERRX)
	  wb_iserr ("IOERR", ufbptr[G_chan]->prname);
/* 000845 */
lL51000:	;
/* 000845 */
/* 000846 */
	 (DEPT_is) =  1;
/* 000847 */
lL51010:	;
/* 000847 */
	goto Go_Sub_Ret;
/* 000849 */
lCHECK_BARCODE:	;
/* 000850 */
	 (BARCODE_is) =  0;
/* 000851 */
	wb_vinit (" ", &LB_KEY1_TS, NULL);
/* 000852 */
	g_astr0 = (char *) LB_KEY1_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 23  - ( 1-1),  (18));
	str_asgn (LB_BARCODE_ts, (long) 18 , 
		g_astr0, (long) g_ind0 );
/* 000853 */
	G_chan = 5 - 1;
	g_ind0 = 278;
	g_f[0] = (long) POS;
	g_f[1] = 1L;
	g_f[2] = (long) g_ind0;
	g_ind1 = 23;
	g_f[3] = (long) CH;
	g_f[4] = 1L;
	g_f[5] = (long) g_ind1;
	g_f[6] = (long) STRCONST;
	g_f[7] = (long) LB_KEY1_ts;
	g_f[8] =  23 ;
	g_f[9] = (long) STRADR;
	g_f[10] = (long) (LB_KEY1_ts);
	g_f[11] = (long)  23 ;
	g_f[12] = 0L;
	/*GETT,UFBPTR[CH],HOLD,KEYIND,KEYREL,RELF,RECF,PIC,CNT*/
	g_ret = wb_rdgt (READ, ufbptr[G_chan], 0, 1,
	62, 1,  0, 2, 1);
	if (g_ret == EODX)
	  goto lL52010;
	else if (g_ret == DATAX)
	  wb_iserr ("DATA", ufbptr[G_chan]->prname);
	else if (g_ret == TIMEOX)
	  wb_iserr ("TIMEOUT", ufbptr[G_chan]->prname);
	else if (g_ret == IOERRX)
	  wb_iserr ("IOERR", ufbptr[G_chan]->prname);
/* 000854 */
lL52000:	;
/* 000854 */
/* 000855 */
	g_astr0 = (char *) LB_KEY1_ts;
	g_astr0 += ( 1-1);
	g_ind0 = min ( 23  - ( 1-1),  (18));
	g_ind1 = wb_str (g_astr0,  g_ind0 , LB_BARCODE_ts,  18 );
	g_ind2 = (g_ind1 != 0);
	if (g_ind2)
	  {
		goto lL52010;
	  }
/* 000856 */
	 (BARCODE_is) =  1;
/* 000857 */
lL52010:	;
/* 000857 */
	goto Go_Sub_Ret;
/* 000859 */
/* 000863 */
lERROR_PROMPT:	;
/* 000864 */
	 (COMP_is) =  2;
/* 000865 */
	str_asgn ("******* (Error) (Error) (Error)  *******", (long) 40 , 
		HDR_ts, (long) 40 );
/* 000866 */
	g_astr0 = (char *) (MSG_tv) + (( 1-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	str_asgn (" - - - - - - - - E r r o r - - - - - - - - ", (long) 43 , 
		g_astr0, (long) g_ind0 );
/* 000867 */
	g_astr0 = (char *) (MSG_tv) + (( 2-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	str_asgn (ERRORMSG_ts, (long) 79 , 
		g_astr0, (long) g_ind0 );
/* 000868 */
	g_astr0 = (char *) (MSG_tv) + (( 3-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	str_asgn ("Press Any Key To Continue.", (long) 26 , 
		g_astr0, (long) g_ind0 );
/* 000869 */
	g_astr0 = (char *) (MSG_tv) + (( 1-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	STR_TSD0.p = g_astr0;
	STR_TSD0.len = g_ind0;
	STR_TSD0.tbyte = g_ind0;
	g_astr2 = (char *) (MSG_tv) + (( 2-1) * MSG_TV.len);
	g_ind1 = MSG_TV.len;
	STR_TSD1.p = g_astr2;
	STR_TSD1.len = g_ind1;
	STR_TSD1.tbyte = g_ind1;
	g_astr4 = (char *) (MSG_tv) + (( 3-1) * MSG_TV.len);
	g_ind2 = MSG_TV.len;
	STR_TSD2.p = g_astr4;
	STR_TSD2.len = g_ind2;
	STR_TSD2.tbyte = g_ind2;
	Call_args = 5;
	ASKUSER (&COMP_IS, &HDR_TS, &STR_TSD0, &STR_TSD1, &STR_TSD2);
/* 000870 */
	goto Go_Sub_Ret;
/* 000872 */
lOPEN_ERROR:	;
/* 000873 */
	g_astr0 = "(Open Error) - File = ";
	g_ind0 = 22;
	g_astr1 = g_str0;
	g_ind1 = wb_cat (g_astr0,  g_ind0 , 20,
		FILENAME_ts,  8 , 17, g_astr1);
	str_asgn (g_astr1, (long) g_ind1 , 
		ERRORMSG_ts, (long) 79 );
/* 000874 */
	G_stack[++G_scnt] = 52;
	goto lERROR_PROMPT;
ret_052:	;
/* 000875 */
	goto Go_Sub_Ret;
/* 000877 */
lLOAD_RESULTS:	;
/* 000878 */
	 (K_is) =  2;
/* 000879 */
	str_asgn ("***** Label Generation Results *****", (long) 36 , 
		HDR_ts, (long) 40 );
/* 000880 */
	g_astr0 = (char *) (MSG_tv) + (( 1-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	str_asgn ("This run generated xxxxx label(s).", (long) 34 , 
		g_astr0, (long) g_ind0 );
/* 000881 */
	g_astr0 = (char *) (MSG_tv) + (( 2-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	str_asgn ("---", (long) 3 , 
		g_astr0, (long) g_ind0 );
/* 000882 */
	g_astr0 = (char *) (MSG_tv) + (( 3-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	str_asgn ("Press <ENTER> to Acknowledge & Continue", (long) 39 , 
		g_astr0, (long) g_ind0 );
/* 000883 */
	l_BCD ( (LBL_is), g_exp0);
	g_astr0 = (char *) (MSG_tv) + (( 1-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	g_astr0 += ( 20-1);
	g_ind1 = min ( g_ind0  - ( 20-1),  (5));
	wb_convnum (g_exp0, g_astr0,  g_ind1 , "####0", 8);
/* 000884 */
	g_ind0 = ( (LBL_is) -  0);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL64100;
	  }
/* 000885 */
	g_astr0 = (char *) (MSG_tv) + (( 1-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	str_asgn ("NO LABELS GENERATED!!!", (long) 22 , 
		g_astr0, (long) g_ind0 );
/* 000886 */
	g_astr0 = (char *) (MSG_tv) + (( 3-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	g_ind1 = min ( g_ind0 ,  (13));
	str_asgn (" Press <PF16>", (long) 13 , 
		g_astr0, (long) g_ind1 );
/* 000887 */
lL64100:	;
/* 000887 */
	g_astr0 = (char *) (MSG_tv) + (( 1-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	STR_TSD0.p = g_astr0;
	STR_TSD0.len = g_ind0;
	STR_TSD0.tbyte = g_ind0;
	g_astr2 = (char *) (MSG_tv) + (( 2-1) * MSG_TV.len);
	g_ind1 = MSG_TV.len;
	STR_TSD1.p = g_astr2;
	STR_TSD1.len = g_ind1;
	STR_TSD1.tbyte = g_ind1;
	g_astr4 = (char *) (MSG_tv) + (( 3-1) * MSG_TV.len);
	g_ind2 = MSG_TV.len;
	STR_TSD2.p = g_astr4;
	STR_TSD2.len = g_ind2;
	STR_TSD2.tbyte = g_ind2;
	Call_args = 5;
	ASKUSER (&K_IS, &HDR_TS, &STR_TSD0, &STR_TSD1, &STR_TSD2);
/* 000888 */
	g_ind0 = ( (LBL_is) -  0);
	g_ind1 = (g_ind0 != 0);
	g_ind2 = ( (K_is) -  0);
	g_ind3 = (g_ind2 != 0);
	g_ind4 = (g_ind1 && g_ind3);
	if (g_ind4)
	  {
		goto lLOAD_RESULTS;
	  }
/* 000889 */
	g_ind0 = ( (LBL_is) -  0);
	g_ind1 = (g_ind0 == 0);
	g_ind2 = ( (K_is) -  16);
	g_ind3 = (g_ind2 != 0);
	g_ind4 = (g_ind1 && g_ind3);
	if (g_ind4)
	  {
		goto lLOAD_RESULTS;
	  }
/* 000890 */
	 (LBL_is) =  0;
/* 000891 */
	goto Go_Sub_Ret;
/* 000893 */
lPRINT_ERROR:	;
/* 000894 */
	str_asgn ("***** Label Printing Error *****", (long) 32 , 
		HDR_ts, (long) 40 );
/* 000895 */
	g_astr0 = (char *) (MSG_tv) + (( 1-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	str_asgn ("ERROR OCCURRED WHILE PRINTING LABELS!!!", (long) 39 , 
		g_astr0, (long) g_ind0 );
/* 000896 */
	g_astr0 = (char *) (MSG_tv) + (( 2-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	str_asgn ("Return Code (EWDPLA71) = ", (long) 25 , 
		g_astr0, (long) g_ind0 );
/* 000897 */
	g_astr0 = (char *) (MSG_tv) + (( 3-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	str_asgn ("Press <ENTER> to Continue, <PF16> to Exit", (long) 41 , 
		g_astr0, (long) g_ind0 );
/* 000899 */
	l_BCD ( (ERR_is), g_exp0);
	g_astr0 = (char *) (MSG_tv) + (( 2-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	g_astr0 += ( 26-1);
	g_ind1 = min ( g_ind0  - ( 26-1),  (2));
	wb_convnum (g_exp0, g_astr0,  g_ind1 , "#0", 8);
/* 000900 */
lL64550:	;
/* 000900 */
	 (K_is) =  2;
/* 000901 */
	g_astr0 = (char *) (MSG_tv) + (( 1-1) * MSG_TV.len);
	g_ind0 = MSG_TV.len;
	STR_TSD0.p = g_astr0;
	STR_TSD0.len = g_ind0;
	STR_TSD0.tbyte = g_ind0;
	g_astr2 = (char *) (MSG_tv) + (( 2-1) * MSG_TV.len);
	g_ind1 = MSG_TV.len;
	STR_TSD1.p = g_astr2;
	STR_TSD1.len = g_ind1;
	STR_TSD1.tbyte = g_ind1;
	g_astr4 = (char *) (MSG_tv) + (( 3-1) * MSG_TV.len);
	g_ind2 = MSG_TV.len;
	STR_TSD2.p = g_astr4;
	STR_TSD2.len = g_ind2;
	STR_TSD2.tbyte = g_ind2;
	Call_args = 5;
	ASKUSER (&K_IS, &HDR_TS, &STR_TSD0, &STR_TSD1, &STR_TSD2);
/* 000902 */
	g_ind0 = ( (K_is) -  0);
	g_ind1 = (g_ind0 == 0);
	if (g_ind1)
	  {
		goto Go_Sub_Ret;
	  }
/* 000903 */
	g_ind0 = ( (K_is) -  16);
	g_ind1 = (g_ind0 != 0);
	if (g_ind1)
	  {
		goto lL64550;
	  }
/* 000904 */
	G_scnt = 0;	/* RETURN CLEAR ALL */
/* 000905 */
	goto lINPUTMODE;
/* 000907 */
/* 000911 */
lEXIT_PROGRAM:	;
/* 000913 */
	wb_end (0);
#ifdef END
#undef END
#endif
	END:
wb_end(0);

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
	case 3:
		goto ret_003;
	case 4:
		goto ret_004;
	case 5:
		goto ret_005;
	case 6:
		goto ret_006;
	case 7:
		goto ret_007;
	case 8:
		goto ret_008;
	case 9:
		goto ret_009;
	case 10:
		goto ret_010;
	case 11:
		goto ret_011;
	case 12:
		goto ret_012;
	case 13:
		goto ret_013;
	case 14:
		goto ret_014;
	case 15:
		goto ret_015;
	case 16:
		goto ret_016;
	case 17:
		goto ret_017;
	case 18:
		goto ret_018;
	case 19:
		goto ret_019;
	case 20:
		goto ret_020;
	case 21:
		goto ret_021;
	case 22:
		goto ret_022;
	case 23:
		goto ret_023;
	case 24:
		goto ret_024;
	case 25:
		goto ret_025;
	case 26:
		goto ret_026;
	case 27:
		goto ret_027;
	case 28:
		goto ret_028;
	case 29:
		goto ret_029;
	case 30:
		goto ret_030;
	case 31:
		goto ret_031;
	case 32:
		goto ret_032;
	case 33:
		goto ret_033;
	case 34:
		goto ret_034;
	case 35:
		goto ret_035;
	case 36:
		goto ret_036;
	case 37:
		goto ret_037;
	case 38:
		goto ret_038;
	case 39:
		goto ret_039;
	case 40:
		goto ret_040;
	case 41:
		goto ret_041;
	case 42:
		goto ret_042;
	case 43:
		goto ret_043;
	case 44:
		goto ret_044;
	case 45:
		goto ret_045;
	case 46:
		goto ret_046;
	case 47:
		goto ret_047;
	case 48:
		goto ret_048;
	case 49:
		goto ret_049;
	case 50:
		goto ret_050;
	case 51:
		goto ret_051;
	case 52:
		goto ret_052;
	default:
		wb_err("GOSUB TABLE ERROR");
	}
}
/* END OF ewdpln71 */

