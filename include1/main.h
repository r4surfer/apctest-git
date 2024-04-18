/*****************************************************************************
                 C A E L I S   I N C L U D E   M O D U L E
          	    (c)Copyright 1991 CAELUS, Inc.
                  A L L   R I G H T S   R E S E R V E D
==============================================================================
                   M O D U L E   D E S C R I P T I O N
------------------------------------------------------------------------------
	main.h		Main include file for c_port
==============================================================================
               M O D I F I C A T I O N   H I S T O R Y
------------------------------------------------------------------------------
 	DATE		WHO	DESCRIPTION
 	--------	----	----------------------------------------------
	unknown		??? 	Original
	09/25/91	DWL	Added comments for clarification
	10/10/91	DP	increased STBMAX from 32,000 to 35,000 to
				prevent overflowing Stbuf
	01/09/92	MDH	Added counters for GOSUB' parameters
	06/29/92	MDH	Removed Alone which was used by psyprint
	08/19/92	MDH	Remove all old FOR structures & create new ones
	10/19/93	MDH	Define flags for including basic comments
				Remove Vs_proc_xxx, no_flag, line_no_flag
        05/24/94        MDH     Up MAX_RET gosub return table size

==============================================================================
               M O D U L E   B E G I N S   H E R E
*****************************************************************************/
static char main_SccsId[] = "@(#)include main.h  Version 5.1  09/16/94";

#ifndef EXTERN
#  define EXTERN extern
#endif
#ifndef ISZERO
#  define ISZERO
#endif


#define ONE_KB	1024

#ifndef abs
#define abs(a) ( ((a) >=  0) ? (a) : (-(a)))
#endif

#define MAX_ARGS	800
struct args 
  {
    int typ;
    int indx;
  };
EXTERN struct args arg_list[MAX_ARGS];

#define SYMSIZE (30)
#define SYMMAX (SYMSIZE * ONE_KB)
/* symbol table is defined in a different way comparing to run time */
/* this is only for saving space */
struct cmpsym {			/* symbol table in compiling time */
	char *varnam;
	short typ;
	short len;
	short rank;
        short count;
	long dim1;
	long dim2;
};
EXTERN struct cmpsym *Cmpsym;

/* global compile time structure for generation of FOR loops */
#define MAXFOR 100
EXTERN int	For_max ISZERO;	/* largest entry in For_data used */
struct fornest 		/* struct to keep info on for index variables */
  {
    long var;		/* index into Cmpsym table */
    int  typ;		/* variable type of index */
    char nam[80];	/* variable name */
  };
EXTERN struct fornest For_data[MAXFOR];  /* table of for index info */

#define FMTNO (16L)
#define FMTMAX ((16L)*ONE_KB)
struct Fmt_str {
	long fmt_no;	/* statement NO */
	long *fmt_ptr;	/* ptr to format statement */
};
EXTERN struct Fmt_str *Fmtinf;

EXTERN int	Iff;		/* control IF ...THEN ...ELSE */
EXTERN int	Iff_indflag ISZERO;	/* Flag to control indents for IF statements */
EXTERN int	not_flag;
EXTERN int	Adata ISZERO;		/* all data statements items */

#define KEY_MAX 10000
char	*Tk_i[KEY_MAX];	/* ptr to input buffer */	

#define GDATA	0	/* get const data */
#define GADR	-1	/* get addr of variable */
#define GDIM	-2	/* get dim of variable */

#define DIM0	0	/* scale */
#define DIM1	1	/* array */
#define DIM2	2	/* area 2-dimension */

/* #define STBMAX 220000 */
#define STBMAX 35000
EXTERN unsigned char Stbuf[STBMAX];       /* basic statement buffer */
#define MAXGO 100
EXTERN int Go_typ[MAXGO];
EXTERN int Go_i ISZERO;
#define MAXFN 100
EXTERN int Fun_typ[MAXFN];
EXTERN int Fun_iISZERO;

#define GCOMMAX	65000
EXTERN long	g_comidx ISZERO;	/* Index for COM variables */
EXTERN int	Gfind	ISZERO;		/* Index for g_f table */
EXTERN int	Gmind	ISZERO;		/* Index for min/max table */
EXTERN int	Unc;

EXTERN long Gno ISZERO;			/* current line number in BASIC */
EXTERN long Gno0 ISZERO;		/* current line number in BASIC */
EXTERN long CurNO ISZERO;		/* current number in BASIC */
EXTERN long Gonum ISZERO;		/* goto number */
EXTERN int Curtyp ISZERO;		/* == 4 -> LONG, ==8 -> double */
short Err_NO ISZERO;	     		/* current error NO */
EXTERN int Stbuf_i ISZERO;	   	/* index for Stbuf */
EXTERN unsigned char Pgname[30];	/* effects when LOAD or NEW */
EXTERN long *Beg;			/* begin of statement (after KEYWORD) */
EXTERN long *End;			/* end of statement */

#define SYMNAMNO 20
#define SYMNAMMAX (SYMNAMNO * ONE_KB)
EXTERN char *Symnam;

#define MAX_SYS_LIT_SIZE 16000
EXTERN char Strbuf[MAX_SYS_LIT_SIZE];	/*  Buffer for the system's literals. 
					 *  The strings in this buffer are 
					 *  always NULL terminated! */

EXTERN char *Symptr;

EXTERN int Cmpsym_e;
EXTERN int Cmpsym_p;

EXTERN int Err_cnt;			/* error count in BASIC */

EXTERN long State_i;
EXTERN int Gotono_i;
#define STATESIZE (60L)
#define STATEMAX (STATESIZE * ONE_KB)
#define GOTOSIZE (10)
#define GOTOMAX (GOTOSIZE * ONE_KB)
EXTERN long *State;
EXTERN long *Gotono;

EXTERN int Subf ISZERO;			/* Subroutine flag */
EXTERN int Subadr ISZERO;
/*
 *	Temporary Variables for Code Gen Programs
 */
EXTERN int Exp_max ISZERO;	/* maximum count for g_exp */
EXTERN int Ind_max ISZERO;	/* maximum count for g_ind */
EXTERN int Str_max ISZERO;	/* maximum count for g_str */
EXTERN int Gs_pmax ISZERO;	/* maximum count for gs_pcnt */

EXTERN int Exp_amax ISZERO;	/* maximum count for g_aexp */
EXTERN int Ind_amax ISZERO;	/* maximum count for g_aind */
EXTERN int Str_amax ISZERO;	/* maximum count for g_astr */

EXTERN int Exp_cnt ISZERO;	/* count for g_exp */
EXTERN int Ind_cnt ISZERO;	/* count for g_ind */
EXTERN int Str_cnt ISZERO;	/* count for g_str */
EXTERN int Gs_pcnt ISZERO;	/* count of gosub prime parameters */

EXTERN int Exp_acnt ISZERO;	/* count for g_aexp */
EXTERN int Ind_acnt ISZERO;	/* count for g_aind */
EXTERN int Str_acnt ISZERO;	/* count for g_astr */

EXTERN int Str_dcnt ISZERO;	/* count for STR_TSD */
EXTERN int Int_dcnt ISZERO;	/* count for INT_ISD */
EXTERN int Bcd_dcnt ISZERO;	/* count for BCD_DSD */

EXTERN int Str_dmax ISZERO;	/* maximum count for STR_TSD */
EXTERN int Int_dmax ISZERO;	/* maximum count for INT_ISD */
EXTERN int Bcd_dmax ISZERO;	/* maximum count for BCD_DSD */

EXTERN int Fileno;	/* file channel no */
EXTERN int Gnoon;	/* line # is on, not in countinue */
EXTERN int isfunp;	/* count parenthesis in func */
EXTERN long *Stptr;	/* ptr to Statement for GOTO */

/* Filename Buffers */
char Basic_buf[80];     /* file name for BASIX program */
char Cbuf[80];		/* file name for C program of BASIC */
char Hbuf[80];		/* header file name for C program of BASIC */
char Btmp[80];		/* binary tmp file */
char Xtmp[80];		/* temp file for code gen */
char Dtmp[80];		/* temp file for code gen in DATA */
char Comtmp[80];	/* temp file to hold comments for -c option */
EXTERN int  com_flag;	/* flag for including comments in C code */
EXTERN int  com_pos;
EXTERN char Com_line[80];

EXTERN long *Fmt;	/* content of fmt buffer (for all) */
EXTERN long *Fmt_st;	/* current ptr for fmt_ptr */
EXTERN long Fmt_i;	/* index of total fmt */


/********** new Globals for jmpbuf **************/
#define MAX_RET 128
EXTERN long G_stack[MAX_RET];		/* return stack */
EXTERN int G_rcnt ISZERO;		/* # of elements in return table */
EXTERN int G_scnt ISZERO;		/* index to return stack */

EXTERN int args_indx ISZERO;

#define VARTYP 0
#define FILTYP 1

/* if this is the module that allocates space, perform initialize */
#ifdef TABLE_INIT


/*	Table of function names (not all are really used */
char *xfun[] = {
	"wb_abs",	/* 0 */
	"wb_all",	/* 1 */
	"wb_arccos",	/* 2 */
	"wb_arcsin",	/* 3 */
	"wb_arctan",	/* 4 */
	"wb_asort",	/* 5 */
	"wb_at",	/* 6 */	
	"wb_arctan",	/* 6 */	
	"wb_bell",	/* 50 */
	"wb_bi",	/* 7 */
	"wb_bin",	/* 8 */
	"wb_ch",	/* 9 */
	"wb_col",	/* 10 */
	"wb_con",	/* 53 */
	"wb_cos",	/* 11 */
	"wb_date",	/* 51 */
	"wb_dim",	/* 12 */
	"wb_dsort",	/* 13 */
	"wb_exp",	/* 14 */
	"wb_fac",	/* 15 */
	"wb_fs",	/* 16 */
	"wb_fl",	/* 17 */
	"wb_hex",	/* 18 */
	"wb_hexof",	/* 54 */
	"wb_idn",	/* 19 */
	"wb_int",	/* 20 */
	"wb_inv",	/* 21 */
	"wb_key",	/* 22 */
	"wb_len",	/* 23 */
	"wb_lgt",	/* 24 */
	"wb_log",	/* 25 */
	"wb_mask",	/* 26 */
	"wb_max",	/* 27 */
	"wb_min",	/* 28 */
	"wb_mod",	/* 29 */
	"wb_num",	/* 30 */
	"wb_page",	/* 31 */
	"wb_pd",	/* 32 */
	"wb_pic",	/* 33 */
	"wb_pos",	/* 34 */
	"wb_range",	/* 35 */
	"wb_rnd",	/* 36 */
	"wb_round",	/* 37 */
	"wb_sgn",	/* 38 */
	"wb_sin",	/* 39 */
	"wb_size",	/* 40 */
	"wb_skip",	/* 41 */
	"wb_sqr",	/* 42 */
	"wb_str",	/* 43 */
	"wb_tab",	/* 44 */
	"wb_tan",	/* 45 */
	"wb_time",	/* 52 */
	"wb_trn",	/* 46 */
	"wb_val",	/* 47 */
	"wb_xx",	/* 49 */
	"wb_zer",	/* 48 */
	"wb_nofun"	/* 55 */
};

/*"?","-","(",")","*","+",",","-","/",":",";","<","=",">","^",? (15) */
/* ==,!=,<=,>=,&&,||,^^,!,&,ADD,ADDC, (26) */
/* BOOL0,BOOL1,.....BOOL9,BOOLA,BOOLB,...BOOLF, (36) */
/*ABS,.......NOFUN */

int Icpmap[44] =
		{0,4,6,0,2,1,1 ,1,2,4,0,-2,-4,-2,3,0,
		-2,-2,-2,-2,-3,-3,-3,4,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,5
		};

int Ispmap[44]=
		{-1,4,-5,-1,2,1,-1,1,2,4,-1,-3,-5,-3,3,-6,
		-3,-3,-3,-3,-3,-3,-3,4,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,4
		};


/*
 *	Table of temporary variable names 
 */
char *Tmpnam[] = {
		"g_exp",		/* expression tmp variable */
		"g_ind",		/* integer tmp variable */
		"g_iva",		/* for loop variable (INT) */
		"g_ima",		/* for loop maximum (INT) */
		"g_ist",		/* for loop step (INT) */
		"g_dva",		/* for loop variable (BCD) */
		"g_dma",		/* for loop maximum (BCD) */
		"g_dst",		/* for loop step (BCD )*/
		"g_aexp",		/* expression tmp variable adr  */
		"g_aind",		/* integer tmp variable adr */
		"g_astr"		/* str tmp variable adr */
		};

/* Variable Types & Ranks */
char Gtyps[2][3]=
		{'t','i','d','T','I','D'};
char Grank[2][3]=
		{'s','v','m','S','V','M'};

char *Strptr = Strbuf;		/* Pointer to buffer for system's literals */

#else
extern char *xfun[];
extern int Icpmap[44];
extern int Ispmap[44];
extern int  For_typ[];
extern char *Tmpnam[];
extern char Gtyps[2][3];
extern char Grank[2][3];
extern char *Strptr;		/* Pointer to buffer for system's literals */
#endif

