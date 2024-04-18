        REM *************************************************************~
            *                                                           *~
            *   DDDD    AAA   TTTTT  U   U  FFFFF  M   M  TTTTT   CCC   *~
            *   D   D  A   A    T    U   U  F      MM MM    T    C   C  *~
            *   D   D  AAAAA    T    U   U  FFFF   M M M    T    C      *~
            *   D   D  A   A    T    U   U  F      M   M    T    C   C  *~
            *   DDDD   A   A    T     UUU   F      M   M    T     CCC   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DATUFMTC - This subroutine receives a formatted date and  *~
            *            returns the date as normalized YYMMDD/YYYYMMDD.*~
            *            It expects the formatted date to follow the    *~
            *            system default set in GENEDIT to determine     *~
            *            the expected format of the date received.      *~
            *            Usually used to convert formatted date fields  *~
            *            prior to writing to a file.                    *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 04/28/80 ! ORIGINAL (RIPPED OFF FROM "DATEFMT")     ! BCW *~
            * 11/20/85 ! Rewrite, now extracts format from system ! LDJ *~
            * 09/22/87 ! Modified to accept Century in Date String! LDJ *~
            * 04/09/96 ! Changes for Century System Conversion    ! LDJ *~
            * 09/25/97 ! Exception for 00/00/00                   ! DXL *~
            *************************************************************

            sub "DATUFMTC" (passing$,   /* In: Formatted Date          */~
                                        /* Out: Unformatted as PD(11,1)*/~
                            passdate%,  /* Optional Arg Out: CCYYMMDD% */~
                            unformatted$)/*Optional Arg Out: CCYYMMDD$ */

        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto cms_start
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
cms_start
            call "NARGS" addr(args%)    /* test to see if extra args   */

            dim date$10,                 /* Date Passed In             */~
                format$1,                /* System Date Format Flag    */~
                passing$10,              /* Date Passed In             */~
	            unformatted$8,           /* Date Out in CCYYMMDD format*/~
                work$10                  /* Date Work Variable         */

            date% = 0%
            date$ = passing$
            if date$ = " " then STUFF_IT
            if str(date$,,1%) = hex(00) then END_ROUTINE
            if len(date$) <= 8% then y% = 2% else y% = 4%
            if format$ = " " then call "EXTDTFMT" addr(format$)
            if format$ <> "A" then EUROPE /* Test for U.S. Format       */
               work$ = str(date$,7%,y%) & str(date$,1%,2%) &             ~
                       str(date$,4%,2%)
EUROPE:     if format$ <> "E" then STANDARD /* Test for European Format */
               work$ = str(date$,7%,y%) & str(date$,4%,2%) &             ~
                       str(date$,1%,2%)
STANDARD:   if format$ <> "S" then CONVERT_2_PD /* Test for Standard Format*/
               work$ = str(date$,1%,y%) & str(date$,y%+2%,2%) &          ~
                       str(date$,y%+5%,2%)
CONVERT_2_PD:
            if y% = 4% then DO_IT
            if work$ = "000000" then DO_IT
                if str(work$,,2%) < "21" then work$ = "20" & work$ ~
                                         else work$ = "19" & work$
DO_IT:      convert work$ to date%, data goto ARG_CHECK
            init (hex(20)) passing$
STUFF_IT:   put passing$ using DTE_FMT, date%
DTE_FMT:    FMT PD(11,1)

ARG_CHECK:  if args% < 2% then END_ROUTINE
            passdate% = date%
            if args% < 3% then END_ROUTINE
            convert date% to work$, pic(00000000)
            unformatted$ = work$

END_ROUTINE: end
