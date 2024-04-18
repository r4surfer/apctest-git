        REM *************************************************************~
            *                                                           *~
            *   DDDD    AAA   TTTTT  U   U  N   N  FFFFF  M   M  TTTTT  *~
            *   D   D  A   A    T    U   U  NN  N  F      MM MM    T    *~
            *   D   D  AAAAA    T    U   U  N N N  FFFF   M M M    T    *~
            *   D   D  A   A    T    U   U  N  NN  F      M   M    T    *~
            *   DDDD   A   A    T     UUU   N   N  F      M   M    T    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DATUNFMT - This subroutine receives a formatted date and  *~
            *            returns the date in normalized YYMMDD format.  *~
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
            * 04/09/96 ! Changes for Century System Conversion    ! LDJ *~
            *************************************************************

           sub "DATUNFMT" (passing$,    /* In: formatted date          */~
                                        /* Out: unformatted as PD(11,1)*/~
                           passdate%,   /* Optional arg - Returned Integer Date (CCYYMMDD) */~
                          unformatted$) /* ""       ""       ""    ""  */

        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto cms_start
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
cms_start
            call "NARGS" addr(args%)    /* test to see if extra args given */

            dim std_date$8,             /* Date in CCYYMMDD format     */~
                unformatted$8           /* Date in CCYYMMDD format     */

            call "DATUFMTC" (passing$,date%,std_date$)
            if args% > 1% then passdate% = date%
            if args% > 2% then unformatted$ = std_date$
            end
