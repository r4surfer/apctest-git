        REM *************************************************************~
            *                                                           *~
            *   DDDD    AAA   TTTTT  FFFFF  M   M  TTTTT   CCC          *~
            *   D   D  A   A    T    F      MM MM    T    C   C         *~
            *   D   D  AAAAA    T    FFFF   M M M    T    C             *~
            *   D   D  A   A    T    F      M   M    T    C   C         *~
            *   DDDD   A   A    T    F      M   M    T     CCC          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DATFMTC - This subroutine takes a string with the date    *~
            *           packed in the first 6 characters as CCYYMMDD and*~
            *           returns it in the format specified by the system*~
            *           GENEDIT defaults (e.g. MM/DD/YY, DD/MM/YYYY...).*~
            *           Very useful for screen displays, reports, etc.  *~
            *           Note; the dimensioned length of the passed date *~
	        *           variable determines whether or not century      *~
	        *           digits are prefixed (or appended) to the        *~
	        *           formatted date.  Example; date1$8 will not      *~
	        *           century digits but date2$10 will.               *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 01/22/80 ! ORIGINAL                                 ! BCW *~
            * 06/07/81 ! NO FORMAT BLANK FIELD                    ! TEM *~
            * 11/20/85 ! Rewrite, extracts date format from System! LDJ *~
            * 09/22/87 ! Modified to accept Century in Date String! LDJ *~
            *          ! Also now extracts date separator         !     *~
            *          ! character thanks to KAB.                 !     *~
            * 09/01/88 ! Tests Dim'd size of passing if cent FMT  ! RJM *~
            * 04/09/96 ! Changes for Century System Conversion    ! LDJ *~
            *************************************************************

            sub "DATFMTC" (passing$,     /* In: Unformatted date       */~
                                         /* Out: Formatted date        */~
                           passdate%,    /* Optional Arg Out: CCYYMMDD%*/~
                           unformatted$) /* Optional Arg Out: CCYYMMDD$*/

        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto cms_start
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
cms_start
            call "NARGS" addr(args%)     /* test to see if second arg given */

            dim date$10,                 /* Date Work Variable         */~
                ds$1,                    /* Date Separator Character   */~
                format$1,                /* System Date Format Flag    */~
                passing$10,              /* Date Passed In             */~
                unformatted$8,           /* Date Out in CCYYMMDD format*/~
                work$10                  /* Date Work Variable         */

            date% = 0% : work$ = " "
            date$ = passing$
            if date$ = " " then ARG_CHECK
            if str(date$,1%,1%) > hex(00) then STR_TO_INT
               date% = 0%
               get date$ using DAT_FMT, date%, data goto ARG_CHECK
               if date% = 0% then STUFF_IT
DAT_FMT:       FMT PD(11,1)

            if date% < 999999% then convert date% to str(date$,,6%), pic(000000)~
                               else convert date% to str(date$,,8%), pic(00000000)
            goto FORMAT_IT

STR_TO_INT: convert date$ to date%, data goto ARG_CHECK

FORMAT_IT:  y% = 1%
            if len(date$) < 8% then SHORT_DATE
            m% = 5% : d% = 7% : yl% = 4%
            if len(str(passing$)) >= 10% then GET_FORMAT
               y% = 3% : yl% = 2%    /* Receiver Too Short For Century */
               goto GET_FORMAT

SHORT_DATE: m% = 3% : d% = 5% : yl% = 2%

GET_FORMAT: if format$ = " " then call "EXTDTFMT" addr(format$)
            if ds$     = " " then call "EXTDTSEP" addr(ds$)

            if format$ <> "A" then EUROPE   /* Test for U.S. Format     */
               work$ = str(date$,m%,2%) & ds$ & str(date$,d%,2%) & ds$ & ~
                       str(date$,y%,yl%)
EUROPE:     if format$ <> "E" then STANDARD /* Test for European Format */
               work$ = str(date$,d%,2%) & ds$ & str(date$,m%,2%) & ds$ & ~
                       str(date$,y%,yl%)
STANDARD:   if format$ <> "S" then STUFF_IT /* Test for Standard Format */
               work$ = str(date$,y%,yl%) & ds$ & str(date$,m%,2%) & ds$  ~
                       & str(date$,d%,2%)
STUFF_IT:   passing$ = work$

ARG_CHECK:  if args% < 2% or date% = 0% then END_ROUTINE
            if date% < 210000% then date% = date% + 20000000%
            if date% < 999999% then date% = date% + 19000000%
            passdate% = date%
	    if args% < 3% then END_ROUTINE
	    convert date% to work$, pic(00000000)
	    unformatted$ = work$

END_ROUTINE: end
