#define IS_OUTPUT	0x00000001							/* output only file			*/
#define IS_PRINTFILE	0x00000002							/* printerfile				*/
#define IS_SCRATCH	0x00000004							/* scratch file				*/
#define IS_SUBMIT       0x00000008							/* This file will be submitted.		*/
#define IS_IO		0x00000010							/* This file is opened IO.		*/
#define IS_SORT		0x00000020							/* This file is s SORT file.		*/
#define IS_LIB		0x00000040							/* Generate a LIB only.			*/
#define IS_NORESPECIFY	0x00000080							/* already done				*/
#define IS_INDEXED	0x00000100							/* an indexed file			*/
#define IS_CASE_SEN	0x00000200							/* name is case sensitive don't change	*/
#define IS_NOWRITE	0x00000400							/* File is opened allowing no writers.	*/
#define IS_PRNAME	0x00000800							/* Its a call with a prname ref.	*/
#define IS_BACKFILL	0x00001000							/* Backfill file/lib/vol.		*/
#define IS_SEQSEQ	0x00002000							/* It's a Sequential/Sequential file.	*/
#define IS_GETPARM	0x00004000							/* Has the initial hidden GP been done?	*/
#define IS_NODISPLAY	0x00008000							/* Select file "NODISPLAY".		*/
#define IS_SEQDYN	0x00010000							/* File is SEQ/DYN without relative key.*/
#define IS_WORK		0x00020000							/* A WORK file.				*/
#define IS_TEMP		0x00040000							/* A temporary ## file			*/
#define IS_ERROR	0x00080000							/* Set to force a GETPARM.		*/
#define IS_DECLARE	0x00100000							/* There are DECLARITIVES for this file */
#define IS_EXTEND	0x00200000							/* File opened EXTEND.			*/



