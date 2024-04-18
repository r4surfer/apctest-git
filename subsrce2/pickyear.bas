        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  PPPP   IIIII   CCC   K   K  Y   Y  EEEEE   AAA   RRRR    *~
            *  P   P    I    C   C  K  K   Y   Y  E      A   A  R   R   *~
            *  PPPP     I    C      KKK     YYY   EEEE   AAAAA  RRRR    *~
            *  P        I    C   C  K  K     Y    E      A   A  R   R   *~
            *  P      IIIII   CCC   K   K    Y    EEEEE  A   A  R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PICKYEAR - This Program will allow users to select an     *~
            *            archive file to view.  Users select an existing*~
            *            archive files from current active database to  *~
            *            display or restore from backups.               *~
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
            * 05/28/91 ! Clone from Formax Inc. By ED1            ! SID *~
            * 07/14/93 ! PRR 12877. Screen cleanup. GLDETAL2 Out! ! JDH *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            sub "PICKYEAR" (fileid$,     /* File's 1st four characters */~
                            choice$)     /* User's choice of year      */~

        dim choice$4,                    /* Year selected (or 'CURR')  */~
            choices$(50)23,              /* Year choices to select     */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for display           */~
            displayline$79,              /* Line of header             */~
            file$8,                      /* File ID for 'FIND' search  */~
            fileid$4,                                                    ~
            found$(50)22,                /* File ID's found            */~
            lastline$79,                 /* Last line of select screen */~
            lib$8,                       /* Library for 'FIND'         */~
            options$23,                  /*                            */~
            pf2$8,                       /* (2)First                   */~
            pf4$11,                      /* (4)Previous                */~
            pf5$7,                       /* (5)Next                    */~
            pf16$,                       /* (16)Exit                   */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            vol$6                        /* Volumes to search          */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            i$(24)80                     /* Screen Image               */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

            u3% = 0%
            file$ = str(fileid$,,4%) & "?"
            call "EXTRACT" addr("IL", lib$)
            vol$ = "?"
            start% = 1% : count%, files% = 50%
            options$ = "On Line Archive File(s)"
            init(" ") choices$(), pf2$, pf4$, pf5$

            call "FIND" addr(file$, lib$, vol$, start%, count%,          ~
               found$(), files%, "A")

*        Look for a Dual Books File (GLDETAL2) and eliminate it
            if fileid$ <> "GLDE" then L10130
            if count% < 2% then L10130
                for i% = 1% to files%
                     if str(found$(i%),15%,8%) = "GLDETAL2" then L10118
                next i% : goto L10130
L10118:              count% = count% - 1% : files% = files% - 1%
                     found$(i%) = " "
                     for e% = i% + 1% to files% + 1%
                          found$(e% - 1%) = found$(e%)
                     next i%

L10130:     if files% > 10% then pf5$ = "(5)Next" else pf5$ = " "
            if count% > 1% then L10162

            ask% = 0%
            call "ASKUSER" (ask%, "***** NO ARCHIVE FILES *****",        ~
                 "The required file was NOT FOUND!",                     ~
                 "Either it doesn't exist or it could not be opened ",   ~
                 "Press Any Key to return")
            goto L65000

L10162:     pickptr% = 2%
            str(choices$(1%),3) = "CURRENT Database File"
            for i% = 1% to files%
               convert str(found$(i%),19,4) to found%, data goto L10183
               if found% = 0% then L10183
               str(choices$(pickptr%),3) = str(found$(i%),19,4) &        ~
                                           " Archive File"
               pickptr% = pickptr% + 1%
L10183:     next i%

            for i% = 1% to files%
                str(choices$(i%),,2) = hex(0b8c)
            next i%

            date$ = date
            call "DATEFMT"(date$)
            str(displayline$,62) = "PICKYEAR: " & str(cms2v$,,8%)
            lastline$="Position Cursor to Desired Archive Year & Press " ~
                      & " (RETURN)."

            k% = 0%
            if choice$ = "CURR" or choice$ = " "                         ~
             then str(displayline$,,61%) = "Archive File Year: Current"  ~
             else str(displayline$,,61%) = "Archive File Year: " & choice$
L10230:     gosub L40000
               if keyhit% <> 2% then L10240  /* First */
                  k% = 0% : pf2$,pf4$ =" " : pf5$ = "(5)Next" : goto L10230
L10240:        if keyhit% <> 0% then L10250
                  if cursor%(2%) <> 29% then L10230
                  fieldnr% = cursor%(1%) - 7%
                  if fieldnr% < 1% or fieldnr% > 10% then L10230
                  if str(choices$(k% + fieldnr%),3,4) = " " then L10230
                  goto L10290
L10250:        if keyhit% <> 4% then L10255  /* Previous */
                  k% = k% - 10%
                  if k% <> 0% then goto L10230
                    pf2$, pf4$ = " " : pf5$ = "(5)Next" : goto L10230
L10255:        if keyhit% <> 5% then L10263  /* Next */
                  if (k% + 10%) > files% then L10230 else k% = k% + 10%
                  if choices$(k% + 1%) = " " then L10259
                     pf2$ = "(2)First" : pf4$ = "(4)Previous"
L10259:           if choices$(k% + 10%) = " " then pf5$ = " "
                  goto L10230
L10263:        if keyhit% = 16% then L65000  /* Don't feel like picking */
            goto L10230

L10290:     choice$ = str(choices$(k% + fieldnr%), 3, 4)
            goto L65000

        REM *************************************************************~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *************************************************************

L40000: REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * File Selection Screen                                     *~
            *************************************************************

            pf16$ = "(16)Exit"
            gosub set_pfkeys
            str(choices$(k% + 1%),,2) = hex(0b8c)

L40090:     accept                                                       ~
               at (01,02),"Select an Archive Year "              ,       ~
               at (01,66),"Today:", fac(hex(8c)), date$,                 ~
               at (02,02),fac(hex(ac)), displayline$,                    ~
               at (07,29),fac(hex(a4)) ,  options$               ,ch(23),~
                                                                         ~
               at (08,29),fac(hex(80)) , choices$(k% +  1%)      ,       ~
               at (08,29),fac(hex(8e)) , choices$(k% +  1%)      ,       ~
               at (09,29),fac(hex(8e)) , choices$(k% +  2%)      ,       ~
               at (10,29),fac(hex(8e)) , choices$(k% +  3%)      ,       ~
               at (11,29),fac(hex(8e)) , choices$(k% +  4%)      ,       ~
               at (12,29),fac(hex(8e)) , choices$(k% +  5%)      ,       ~
               at (13,29),fac(hex(8e)) , choices$(k% +  6%)      ,       ~
               at (14,29),fac(hex(8e)) , choices$(k% +  7%)      ,       ~
               at (15,29),fac(hex(8e)) , choices$(k% +  8%)      ,       ~
               at (16,29),fac(hex(8e)) , choices$(k% +  9%)      ,       ~
               at (17,29),fac(hex(8e)) , choices$(k% + 10%)      ,       ~
                                                                         ~
               at (21,02), fac(hex(a4)), lastline$               ,       ~
               at (22,02), fac(hex(8c)), pf2$                    ,       ~
               at (22,25), fac(hex(8c)), pf4$                    ,       ~
               at (22,65), "(13)Instructions"                    ,       ~
               at (23,25), fac(hex(8c)), pf5$                    ,       ~
               at (23,65), "(15)Print Screen"                    ,       ~
               at (24,65),fac(hex(84)), pf16$                    ,ch(08),~
               keys(pfkeys$)                                     ,       ~
               key(keyhit%)

               if keyhit% <> 13 then L40300
                  call "MANUAL" ("PICKYEAR")
                  goto L40090

L40300:        if keyhit% <> 15 then L40340
                  call "PRNTSCRN"
                  goto L40090

L40340:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pfkeys
            pfkeys$ = hex(000204050d0f10)
            if files% > 10% then L45050
               pf2$, pf4$, pf5$ = " "  :  str(pfkeys$, 2, 3) = hex(ff)
               return
L45050:     if k% <> 0% then L45070
               pf2$, pf4$ = " " : str(pfkeys$, 2, 2) = hex(ff)
L45070:     if k% <= files% then L45090
               pf5$ = " " : str(pfkeys$, 4, 1) = hex(ff)
            if (k% + 10%) > files% then pf5$ = " "
L45090:     return

L65000: REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            end
