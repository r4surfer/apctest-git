        REM *************************************************************~
            *                                                           *~
            *  DDDD    AAA   TTTTT  EEEEE   OOO   K   K                 *~
            *  D   D  A   A    T    E      O   O  K  K                  *~
            *  D   D  AAAAA    T    EEEE   O   O  KKK                   *~
            *  D   D  A   A    T    E      O   O  K  K                  *~
            *  DDDD   A   A    T    EEEEE   OOO   K   K                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DATEOK - Tests a date to make sure it is a valid date.    *~
            *          Returns date normalized in input string plus     *~
            *          date in Integer plus error message text usable   *~
            *          by calling program to detect & display errors    *~
            *          (if any).                                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+-------------------WHAT-------------------+-WHO-*~
            * 01/24/80 ! ORIGINAL (SWIPED FROM 2200T PROGRAMMING) ! BCW *~
            * 06/01/81 ! UPDATE ERROR MESSAGE STYLE AND ADD MONTH ! BCW *~
            *          ! NUMBER OF DAYS RIGHT TEST.               !     *~
            * 05/02/83 ! FREE FORMAT ENTRY (E.G. 050283 OR 030502)! KAB *~
            * 11/20/85 ! Totally Rewrote This S.O.B.              ! LDJ *~
            * 04/10/96 ! Changes for Century System Conversion    ! LDJ *~
            *************************************************************

            sub "DATEOK" (pass$,         /* passed in date             */~
                          date%,         /* returned date value        */~
                          error$)        /* return error msg or blank  */

            dim short_date$8             /* date to pass to DATEOKC    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

	    short_date$ = pass$
            call "DATEOKC" (short_date$, date%, error$)
            if error$ = " " then pass$ = short_date$
	    end
