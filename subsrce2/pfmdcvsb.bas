        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  PPPP   FFFFF  M   M  DDDD    CCC   V   V   SSS   BBBB    *~
            *  P   P  F      MM MM  D   D  C   C  V   V  S      B   B   *~
            *  PPPP   FFFF   M M M  D   D  C      V   V   SSS   BBBB    *~
            *  P      F      M   M  D   D  C   C   V V       S  B   B   *~
            *  P      F      M   M  DDDD    CCC     V     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PFMDCVSB - On passed code of 'IS', returns a date in a    *~
            *            string converted from integar input.  On a code*~
            *            of 'SI', returns a the value of a date in a    *~
            *            string in 3 integar values.  Temporary Bridge  *~
            *            between YY and CCYY date formats.              *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/28/92 ! ORIGINAL                                 ! JBK *~
            * 07/18/96 | Changes for the year 2000.               ! DXL *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "PFMDCVSB"  (op_code$,       /* 'SI'= String to Integers   */~
                                         /* 'IS'= Integers to String   */~
                         year%,          /* Integer Value of Year      */~
                                         /* If Value is > 99 then will */~
                                         /*    placed in string with   */~
                                         /*    a length of 4 else 2    */~
                         month%,         /* Integar Value of Month     */~
                         day%,           /* Integar Value of Day       */~
                         stringdate$,    /* Passed in or passed back   */~
                                         /*    string date in format   */~
                                         /*    YYMMDD or CCYYMMDD. If  */~
                                         /*    'SI' len will be check, */~
                                         /*    if 8 bytes then assume  */~
                                         /*    CCYYMMDD format, if 6   */~
                                         /*    bytes assume YYMMDD     */~
                                         /*    format                  */~
                         ret%)           /* No meaning on input        */
                                         /* Returned 0%- No error      */
                                         /* Returned 1%- Error detected*/

        dim                                                              ~
            op_code$2,                   /* Operation to perform       */~
            stringdate$10,               /* Input or output date string*/~
            tdate$10,                    /* Temporary date work area   */~
            tempdate$10                  /* Temporary date work area   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *    S U B R O U T I N E   I N I T I A L I Z A T I O N S    *~
            *************************************************************


        REM *************************************************************~
            *        HEIGH HO, HEIGH HO, IT'S OFF TO WORK WE GO!        *~
            *************************************************************

            ret% = 1%                  /* Indicate 'there is an error' */
            on pos("SI" = op_code$) goto string_to_integer,              ~
                integer_to_string     /* 1st char determines operation */
            goto error_exit             /* Unknown operation requested */

        string_to_integer
            tdate$ = stringdate$
            call "DATEFMT" ( tdate$, 0%, tempdate$)
            year_len% = len(tempdate$) - 4%
            if year_len% <> 2% and year_len% <> 4% then error_exit

            convert str(tempdate$, 1, year_len%) to year%,        data   ~
                                      goto error_exit
            convert str(tempdate$, year_len% + 1%, 2%) to month%, data   ~
                                      goto error_exit
            convert str(tempdate$, year_len% + 3%, 2%) to day%,   data   ~
                                      goto error_exit
            call "DATECONV" (tempdate$)  /* to internal fmt */
            goto exit_ok

        integer_to_string
            year_len% = 2%  :  tempdate$ = " "
            if year% > 99% then year_len% = 4%

            if year_len% = 2% then                                       ~
                convert year% to str(tempdate$, 1, 2), pic(00)           ~
            else                                                         ~
                convert year% to str(tempdate$, 1, 4), pic(0000)
            convert month% to str(tempdate$, year_len% + 1%, 2%), pic(00)
            convert day%   to str(tempdate$, year_len% + 3%, 2%), pic(00)

            call "DATECONV" (tempdate$)  /* to internal fmt */

            stringdate$ = tempdate$
            goto exit_ok

        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        exit_ok
            ret% = 0%             /* Indicate 'no error in processing' */

        error_exit
            end
