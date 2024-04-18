        REM *************************************************************~
            *                                                           *~
            *              (MILLIE PROJECT ADDITION)                    *~
            *                                                           *~
            *   EEEEE  W   W  DDDD   M   M  DDDD   Y   Y                *~
            *   E      W   W  D   D  MM MY  D   D  Y   Y                *~
            *   EEE    W W W  D   D  M M M  D   D   YYY                 *~
            *   E      W W W  D   D  M   M  D   D    Y                  *~
            *   EEEEE  WWWWW  DDDD   M   M  DDDD     Y                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDMDY  - This subroutine takes a string with the         *~
            *           packed date and a date type and will parse      *~
            *           the date into integer and strings of the month  *~
            *           day and year.  The year is returned in century  *~
            *           format.  This means that the format of the year *~
            *           is CCYY.                                        *~
            *                                                           *~
            *           USES SUBROUTINES - DATEFMTC                     *~
            *-----------------------------------------------------------*~
            *                 M O D I F I C A T I O N S                 *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 12/05/97 ! ORIGINAL                                 ! DJD *~
            *************************************************************

SUB "EWDMDY"  (date$,                   /* Input - Packed Date (reqd) */~
               type%,                   /* Input - Type 1-3    (reqd) */~
               mm%,                     /* Output - Month (int)       */~
               mm$,                     /* Output - Month (CH(02))    */~
               dd%,                     /* Output - Day (Integer)     */~
               dd$,                     /* Output - Day (CH(02))      */~
               yy%,                     /* Output - Year incl cntry   */~
               yy$)                     /* Output - Year (CH4)        */



	dim date$8,                     /* Date to work with          */~
            newdate$10,			/* Output date (with century) */~
            type%,                      /* conversion type	      */~
	    mm%,                        /* Month (integer)            */~
	    mm$2,                       /* Month (2 Char String)      */~
	    dd%,                        /* Day (integer)              */~
	    dd$2,                       /* Day (2 Char String)        */~
	    yy%,                        /* Year (Integer)             */~
	    yy$4                        /* Year (4 Char String)       */


        init(" ") mm$,     dd$,     yy$
                  mm% = 0: dd% = 0: yy% = 0

        /* Types 1 and 2 use the same code so thats why the */
        /* on gosub calls the same subroutine for types     */
        /* 1 and 2.  Type 3 is the year conversion          */
        /* The 1 and 2 are only here for compatibility to   */
        /* CAELUS documentation                             */
	str(newdate$,1%,8%) = str(date$,1%,8%)
        on type% gosub standard_6, ~
                       standard_6, ~
                       year

        goto program_exit

        REM *****************************************************
        REM *                  DATE CONVERSION  
        REM *****************************************************
standard_6

        call "DATFMTC" (newdate$)

        if len(newdate$) < 10 then goto exit_standard     

rem print at(10,1), newdate$
rem print at(11,1), " "
rem stop

	mm$ = str(newdate$,1,2)
	convert mm$ to mm%, data goto exit_standard

	dd$ = str(newdate$,4,2)
	convert dd$ to dd%, data goto exit_standard

	yy$ = str(newdate$,7,4)
	convert yy$ to yy%, data goto exit_standard

exit_standard
        return

        REM *****************************************************
        REM *                   YEAR CONVERSION 
        REM *****************************************************
year
        get str(newdate$,1,2) using year_fmt, yy%, data goto exit_year
year_fmt:   FMT bi(2)

	convert yy% to str(yy$,1%,4%), pic(####) 
exit_year
        return
 
program_exit
        end

