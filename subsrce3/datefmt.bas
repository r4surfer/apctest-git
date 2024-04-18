        REM *************************************************************~
            *                                                           *~
            *   DDDD    AAA   TTTTT  EEEEE  FFFFF  M   M  TTTTT         *~
            *   D   D  A   A    T    E      F      MM MM    T           *~
            *   D   D  AAAAA    T    EEEE   FFFF   M M M    T           *~
            *   D   D  A   A    T    E      F      M   M    T           *~
            *   DDDD   A   A    T    EEEEE  F      M   M    T           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DATEFMT - This subroutine takes a string with the first 6 *~
            *           or 8 characters a date in the format YYMMDD or  *~
            *           in the format YYYYMMDD (century included) and   *~
            *           returns it in the format specified by the system*~
            *           defaults (e.g. MM/DD/YY, DD/MM/YY, ...).        *~
            *           Normally expects input date to be in PD(11,1)   *~
            *           format but will accept character string format. *~
            *           Very useful for screen displays, reports, etc.  *~
	    *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 01/22/80 ! ORIGINAL                                 ! BCW *~
            * 06/07/81 ! NO FORMAT BLANK FIELD                    ! TEM *~
            * 11/20/85 ! Rewrite, extracts date format from System! LDJ *~
            * 09/22/87 ! Now extracts date separator.             ! KAB *~
            * 04/09/96 ! Changes for Century System Conversion    ! LDJ *~
            *************************************************************

           sub "DATEFMT" (passing$,     /* IN: Unformatted date        */~
                                        /* OUT: formatted date         */~
                          passdate%,    /* Optional arg - Returned Date*/~
                          unformatted$) /* ""       ""       ""    ""  */

        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto cms_start
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
cms_start
            call "NARGS" addr(args%)     /* test to see if second arg given */

            dim short_date$8,           /* 8 bytes tells DATFMTC to    */~
                                        /* return w/no century digits  */~
                std_date$8,             /* Date in CCYYMMDD format     */~
                unformatted$8           /* Date in CCYYMMDD format     */

            short_date$ = passing$
            call "DATFMTC" (short_date$,date%,std_date$)
            passing$ = short_date$
            if args% > 1% then passdate% = date%
            if args% > 2% then unformatted$ = std_date$
            end
