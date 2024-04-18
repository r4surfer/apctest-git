        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *-----------------------------------------------------------*~
            * EWDPLN63 - Driver program for display/printing of tables  *~
            *            in GENCODES.  Calls subprogram EWDPLA63 to do  *~
            *            the dirty work.  Table to use determined here. *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/28/98 ! ORIGINAL - Copied & Mod GENCDSIN.        ! BWS *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            file$9,                      /* LOGICAL FILE CODE          */~
            filedescr$30,                /* LOGICAL FILE DESCRIPTION   */~
            gcode$50,                    /* Caller to 'CTLGETCD'       */~
            hx00$9,                      /* 9 HEX 00's for File Dir    */~
            lastfile$9,                  /* Last File Managed          */~
            title$79                     /* For Screen Titles          */

        dim f1%(5)                       /* = 1 IF READ WAS SUCCESSFUL */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! Control System Codes File                *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select # 1, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos =    1,  keylen = 24

            call "SHOSTAT" ("Opening Files, One Moment Please.")
            call "OPENCHCK" (#1, open%, 0%, 150%, " ")
                if open% <> 1 then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$  = date  :  call "DATEFMT" (date$)
            hx00$  = all(hex(00))

        REM *************************************************************~
            *        G E T   C O D E   F I L E   I D                    *~
            *-----------------------------------------------------------*~
            * Get Id of Logical File to be maintained.                  *~
            *************************************************************

        get_fileid  /*A little non-std. All that's needed is right here*/
            init (" ") errormsg$, edtmessage$, file$
                       
            title$ = "Last Code Table: "& lastfile$ &" ("& filedescr$ &")"
            str(title$,62%) = "EWDPLN63: " & cms2v$
            edtmessage$ = "Enter table to see/print. Enter blank or" &   ~
                          " partial name to select from display."
L10140: accept                                                           ~
               at (01,02), "Display/Print General System Codes",         ~
               at (01,67), "Date:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "TABLE TO DISPLAY/PRINT:",                    ~
               at (06,30), fac(hex(81)), file$                  , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)), edtmessage$            , ch(79),~
               at (24,02), "(3)Print Codes",                             ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)EXIT Program",                           ~
                                                                         ~
               keys(hex(00030d0f10)),                                    ~
               key (keyhit%)

            if keyhit% = 16 then L65000

            if keyhit% <> 3 then L10390
                gosub validate_table
                call "EWDPLA63" (0%, #1, file$, 1%, " ")
                lastfile$ = file$
                goto get_fileid

L10390:     if keyhit% <> 13 then L10430
*               call "MANUAL" ("GENCDSIN")
*               goto get_fileid

L10430:     if keyhit% <> 15 then L10470
                call "PRNTSCRN"
                goto get_fileid

L10470:     if keyhit% <> 0% then get_fileid
                gosub validate_table
                call "EWDPLA63" (0%, #1, file$, 0%, " ")
                lastfile$ = file$
                goto get_fileid

            validate_table
                if file$ < ".ZZZZZZZZ" then file$ = ".ZZZZZZZZ"
                gcode$ = str(hx00$) &  file$
                call "PLOWCODE" (#1, gcode$, filedescr$, 9%, 0.30, f1%(1))
                file$ = str(gcode$,10)
                if file$ > ".ZZZZZZZZ" then L10550
                    f1%(1) = 0%
                    file$ = " "
L10550:         if f1%(1) = 1 then return
                return clear all
                goto L10140


L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end


