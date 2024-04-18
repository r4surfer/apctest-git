        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   TTTTT  U   U   SSS   EEEEE  DDDD    SSS   BBBB    *~
            *  P   P    T    U   U  S      E      D   D  S      B   B   *~
            *  PPPP     T    U   U    S    EEEE   D   D    S    BBBB    *~
            *  P        T    U   U     S   E      D   D     S   B   B   *~
            *  P        T     UUU    SSS   EEEEE  DDDD    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PTUSEDSB - This program writes or retrieves Customer Xref *~
            *              Part numbers to the BCKLIINES shadow file.   *~
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
            * 03/02/95 ! Original                                 ! RJ2 *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "PTUSEDSB" ( mode$,          /* Process Mode'W'rite,'R'ead,*/~
                                         /*   'D'elete.                */~
                         source_code_in$,/* Source(BCK, BCKH, MLQ, ARI)*/~
                         source_key_in$, /* Source File Key Note       */~
                         source_sqn_in$, /* Source File Seq number-ALL'*/~
                                         /*   at end then delete all   */~
                         ref_part$,      /* Customer Xref Part Number  */~
                         ref_descr$,     /* Customer Xref Part Descrptn*/~
                         ref_type$,      /* Reference Type Code        */~
                         ret%)           /* Return Value 0 = Error,1=OK*/

        dim                                                              ~
            mode$1,                      /* Process Mode'W'rite,'R'ead */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            ref_descr$30,                /* Reference Part Description */~
            ref_part$25,                 /* Reference Part Number      */~
            ref_type$1,                  /* Reference Type Code        */~
            source_code$4,               /* Source Code                */~
            source_key$17,               /* Source File Key            */~
            source_sqn$3                 /* Source File Seq Number     */

        dim f2%(64),                     /* = 0 if the file is open    */~
         /* F1%(64),                        = 1 if READ was successful */~
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

            source_code$ = source_code_in$
            source_key$  = source_key_in$
            source_sqn$  = source_sqn_in$

            if done_this% <> 0% then data_process

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
            * #01 ! CUSPTUSD ! SO - Customer Part Number Cross Referenc *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CUSPTUSD",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos = 1,    keylen =  24


            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 100%, rslt$(01%))

            done_this% = 1%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
        REM *************************************************************~
            *       P R O C E S S   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            *************************************************************

        data_process
            ret% = 0%
            if mode$ = "R" then L19120     /* 'R'ead   */
            if mode$ = "W" then L19150     /* 'W'rite  */
            if mode$ = "D" then L19180     /* 'D'elete */
                goto exit_program

L19120:     gosub dataload
            goto exit_program

L19150:     gosub dataput
            goto exit_program

L19180:     gosub datadelete
            goto exit_program

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

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
        dataload

            readkey$ = str(source_code$) & str(source_key$) & source_sqn$
            call "READ100" (#01, readkey$, ret%)
                if ret% = 0% then return
            get #01 using L30120, ref_part$, ref_descr$, ref_type$
L30120:          FMT POS(25), CH(25), CH(30), CH(1)


            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            ret% = 0%
            if ref_part$ = " " then return  /* Only write valid xrefs */

            readkey$ = str(source_code$) & str(source_key$) & source_sqn$
            call "READ101" (#01, readkey$, ret%)

            put #01 using L31110, source_code$, source_key$, source_sqn$, ~
                                ref_part$, ref_descr$, ref_type$, " "
L31110:          FMT CH(4), CH(17), CH(3), CH(25), CH(30), CH(1), CH(70)

            if ret% = 0% then write #01 else rewrite #01
            ret% = 1%

            return

        REM *************************************************************~
            *        D E L E T E   D A T A   F R O M   F I L E          *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************
        datadelete
            ret% = 0%
            if source_sqn$ <> "ALL" then  L32150
                /* Delete All Lines for Header */
                readkey$ = str(source_code$) & str(source_key$) & " "
                call "DELETE" (#01, readkey$, 21%)
                goto L32970

L32150:     /* Delete individual line item */
            readkey$ = str(source_code$) & str(source_key$) & source_sqn$
            call "DELETE" (#01, readkey$, 24%)

L32970:     ret% = 1%
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

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
