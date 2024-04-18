        REM *************************************************************~
            *                                                           *~
            *  DDDD    AAA   TTTTT  RRRR   EEEEE  V   V  RRRR    SSS    *~
            *  D   D  A   A    T    R   R  E      V   V  R   R  S       *~
            *  D   D  AAAAA    T    RRRR   EEEE   V   V  RRRR    SSS    *~
            *  D   D  A   A    T    R  R   E       V V   R  R       S   *~
            *  DDDD   A   A    T    R   R  EEEEE    V    R   R   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DATREVRS - Accepts a date, (formatted or unformated) &    *~
            *          returns the reversed date in packed decimal.     *~
            *          Also returns an error message text string usable *~
            *          by calling program to detect & display errors    *~
            *          (if any).                                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+-------------------WHAT-------------------+-WHO-*~
            * 05/28/96 ! ORIGINAL                                 ! DXL*~
            *************************************************************

            sub "DATREVRS" (pass$,        /* passed in date            */~
                            rev_date$,    /* returned (reversed) date  */~
                            error$)       /* return error msg or blank */
            Dim ~
            error$79,                     /* error message             */~
            pass$8,                       /* holds the date passed in  */~
            rev_date$6,                   /* packed reversed date      */~
                                          /* returned to the caller    */~
            temp_date$10                  /* working variable          */




        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            temp_date$ = pass$            /* so date doesn't change    */
            call "DATEOKC" (temp_date$, date%, error$)
            if error$ <> " " then end_of_sub
            rev_date% = 99999999% - date%
            rev_date$ = " "
            put rev_date$, using rev_fmt, rev_date%

rev_fmt:    fmt PD(11,1)

end_of_sub
            end
