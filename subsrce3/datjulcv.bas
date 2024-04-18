        REM *************************************************************~
            *                                                           *~
            *   DDDD    AAA   TTTTT  JJJJJJ  U   U  L       CCC   V   V *~
            *   D   D  A   A    T       J    U   U  L      C   C  V   V *~
            *   D   D  AAAAA    T       J    U   U  L      C      V   V *~
            *   D   D  A   A    T    J  J    U   U  L      C   C   V V  *~
            *   DDDD   A   A    T     JJ      UUU   LLLLL   CCC     V   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DATJULCV- This subroutine takes a string with a Julian    *~
            *           date in character 5 or 7 format (YYDDD  or      *~
            *           CCYYDDD) and then converts it into the Caelus   *~
            *           internal Caelus Packed Decimal Date Format      *~
            *            (CCYYMMDD - PD(9,0)), or vice versa.          *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 06/07/96 ! Original - taken from DATCONV            ! DXL *~
            *************************************************************

            sub "DATJULCV" (passing$,    /* In: Julian char date       */~
                                         /* Out: Julian PD date        */~
					 /* (or the other way round)   */~
                           passdate%)    /* Optional Arg Out: CCYYDDD% */
			
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
            if str(date$,1%,1%) > hex(00) then STR_TO_PD
            if date$ = " " then ARG_CHECK
               get date$ using JUL_FMT, date%, data goto ARG_CHECK
	       convert date% to date$, pic(#######)
               goto ARG_CHECK      /* already in internal format */

JUL_FMT:       FMT PD(9,0)

STR_TO_PD:
            if date$ = " " then ARG_CHECK
            convert date$ to date%, data goto ARG_CHECK
            if date% < 21000% then date% = date% + 2000000%
            if date% < 99999% then date% = date% + 1900000%
            init(hex(20)) date$
            put date$ using JUL_FMT, date%
                                   /* now it is in string format */

ARG_CHECK:
            passing$ = date$
            if args% < 2% then END_ROUTINE
            passdate% = date%

END_ROUTINE: end
