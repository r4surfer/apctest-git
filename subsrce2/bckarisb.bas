        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    CCC   K   K   AAA   RRRR   IIIII   SSS   BBBB    *~
            *  B   B  C   C  K  K   A   A  R   R    I    S      B   B   *~
            *  BBBB   C      KKK    AAAAA  RRRR     I     SSS   BBBB    *~
            *  B   B  C   C  K  K   A   A  R   R    I        S  B   B   *~
            *  BBBB    CCC   K   K  A   A  R   R  IIIII   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKARISB - This program determines the Sales Orders Status*~
            *            in A/R sessions.  If it is then have different *~
            *            levels of warning/restrictions.                *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1995  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/18/95 ! Original                                 ! RJ1 *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "BCKARISB" (so_in$,        /* Sales Order Number-Input     */~
                        err$           /* Error Message               */)

        dim                                                              ~
            cuscode$9,                   /* Customer Code              */~
            err$79,                      /* Error Message              */~
            errormsg$79,                 /* Error message              */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            so$16,                       /* Sales Order Number         */~
            so_in$16,                    /* Sales Order Number         */~
            status$1,                    /* Return Status Code         */~
            temp1$1                      /* Temp Variable              */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #02 ! UPDSESSN ! Update sessions                          *~
            * #03 ! ARIBUFFR ! Invoice Master File                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            if been_here% <> 0% then L19060

            select #02, "UPDSESSN",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    4, keylen =  17,                     ~
                        alt key  1, keypos = 1, keylen =  20

            select #03, "ARIBUFFR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos = 2001, keylen =  24,         ~
                            key  2, keypos =   34, keylen =  16, dup

            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))

            been_here% = 1%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

        REM *************************************************************~
            *           S E A R C H   D A T A   O N   F I L E           *~
            *-----------------------------------------------------------*~
            * Searchs for A/R - SO Conflict                             *~
            *************************************************************

L19060:     gosub initialize_variables
            gosub data_search
            gosub do_askuser
            err$ = errormsg$

            goto exit_program

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, status$, cuscode$, err$, so$
            so$ = so_in$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1995  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        data_search
            pos% = 0%
            plowkey$ = so$
            call "REDALT0" (#3, plowkey$, 2%, f1%(3%))
            if f1%(3%) = 0% then return else L30100

L30085:     call "READNEXT" (#3, f1%(3%))
            if f1%(3%) = 0% then  return
            if key(#3, 2%) <> so$ then return
L30100:     get #3 using L30110, ari_ssn$
L30110:         FMT  POS(2001), CH(6)
            readkey$ = "ARIUPDTE" & ari_ssn$
            call "READ100" (#02, readkey$, f1%(2%))
            if f1%(2%) <> 0% then L30150
                goto L30085    /* Try again */

L30150:     get #02 using L30155, temp1$
L30155:         FMT POS(47), CH(1)

           /* Lets set the most critical status */
            tmp_pos% = pos( "DCVU" = temp1$)  /* D = Data Entry    */
                                              /* C = Closed        */
                                              /* V = Verification  */
                                              /* U = Updating      */

            if tmp_pos% = 0% then L30085     /* Try Again */
            if pos% > tmp_pos% then L30085
            status$ = temp1$
            pos% = tmp_pos%
            goto L30085

            return

        do_askuser
            if pos% = 0% then return
            on pos% goto              L30400,      /* D = Data Entry    */~
                                      L30400,      /* C = Closed        */~
                                      L30500,      /* V = Verification  */~
                                      L30500       /* U = Updating      */~

L30400:     ask% = 2%
            call "ASKUSER" (ask%, " *** A/R IN USE WARNING *** " ,       ~
                         "This Sales Order is referenced in A/R Session",~
                          ari_ssn$ & ".  Proceed with Caution.",         ~
                         " Press Any Function Key to Acknowledge ")
            errormsg$ = " "
            return

L30500:     ask% = 2%
            call "ASKUSER" (ask%, " *** A/R IN USE WARNING *** " ,       ~
                         "This Sales Order is referenced in A/R Session",~
                          ari_ssn$ & ".  Use BCKDSPLY or Try Later.",    ~
                         " Press Any Function Key to Acknowledge ")
            errormsg$ = "Sales Order is being used in A/R Session."
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1995  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
*          CALL "SHOSTAT" ("One Moment Please")

            end
