        REM *************************************************************~
            *                                                           *~
            *   DDDD    AAA   TTTTT  EEEEE   CCC    OOO   N   N   V   V *~
            *   D   D  A   A    T    E      C   C  O   O  NN  N   V   V *~
            *   D   D  AAAAA    T    EEEE   C      O   O  N N N   V   V *~
            *   D   D  A   A    T    E      C   C  O   O  N  NN    V V  *~
            *   DDDD   A   A    T    EEEEE   CCC    OOO   N   N     V   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DATECONV- This subroutine takes a string with the date    *~
            *           stored in character 6 or 8 format (YYMMDD or    *~
            *           YYYYMMDD) and then converts it into the Caelus  *~
            *           infernal Caelus Packed Decimal Date Format      *~
            *            (YYYYMMDD - PD(11,1)).                         *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 05/30/96 ! Original                                 ! LDJ *~
            *************************************************************

            sub "DATECONV" (passing$,    /* In: UnFormatted char date  */~
                                         /* Out: UnFormatted PD date   */~
                           passdate%)    /* Optional Arg Out: CCYYMMDD%*/
			
        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto cms_start
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
cms_start
            call "NARGS" addr(args%)     /* test to see if second arg given */

            dim date$8,                  /* Date Work Variable         */~
                passing$8                /* Date Passed In             */


            date% = 0%
            date$ = passing$
            if date$ = " " then ARG_CHECK
            if str(date$,1%,1%) > hex(00) then STR_TO_INT
               date% = 0%
               get date$ using DAT_FMT, date%, data goto ARG_CHECK
DAT_FMT:       FMT PD(11,1)
               goto ARG_CHECK      /* already in infernal format */

STR_TO_INT: convert date$ to date%, data goto ARG_CHECK
            if date% < 210000% then date% = date% + 20000000%
            if date% < 999999% then date% = date% + 19000000%

ARG_CHECK:  init (hex(20)) date$
            put date$ using DAT_FMT, date%
            passing$ = date$
            if args% < 2% then END_ROUTINE
            passdate% = date%

END_ROUTINE: end
