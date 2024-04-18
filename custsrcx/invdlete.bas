        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CLONE of HNYDELETE                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * INVDETAL - Part Deletion program (with lots of checks).   *~
            *          - Attempts to verify if part to be deleted is in *~
            *            use. If not deletes part from system. If in use*~
            *            displays all the areas of use.                 *~
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
            *---WHEN----+---------------WHAT----------------------+-WHO-*~
            * 08/09/2018!ORIGINAL                                 ! RDB *~
            *************************************************************

        dim                                                              ~
            a_moddate$6,                 /* Usage Audit - Mod Date     */~
            a_modid$3,                   /* Usage Audit - Mod ID       */~
            a_text$(2)45,                /* Usage Audit - Mod Text     */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            line2$79,                    /* Second line of Screen      */~
            lfac$(1)1,                   /* Field Fac Variable         */~
            mode$5,                      /* Mode for opening files     */~
            part$25,                     /* PART CODE                  */~
            subpart$20,                  /* SUBPART                    */~
            readkey2$45,                 /* READ KEY with SUBPART      */~
            text$4,                      /* TEXT ID XREFs              */~
            use$(16)75                   /* DISPLAY ARRAY FOR INUSE MSG*/

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/
 
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *  DISPLAY WARNING SCREEN AND ALLOW EXIT BEFORE THIS GETS   *~
            *  TOO SERIOUS.                                             *~
            *************************************************************

            gosub warnscreen
                mode$ = "SHARE"
                if keyhit% = 16% then end

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! INVMASTR ! Inventory Master File                    *~
            * #03 ! INVDETAL ! Inventory detail file                    *~
            * #07 ! INVPOOL  ! Inventory LIFO/FIFO pool records         *~
            * #08 ! INVQUAN  ! Inventory Store Quantity File            *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "INVMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 1024,                                 ~
                         keypos = 1, keylen = 45,                        ~
                         alternate key 1, keypos = 122, keylen = 9, dup, ~
                                   key 2, keypos = 110, keylen = 4, dup, ~
                                   key 3, keypos = 46, keylen = 32, dup

            select #03, "INVDETAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  62,                     ~
                        alt key  1, keypos =   63, keylen =   6, dup,    ~
                            key  2, keypos =   69, keylen =   2, dup

            select #07, "INVPOOL",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  58

            select #08, "INVQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   768,                                 ~
                        keypos =   17, keylen =  64,                     ~
                        alt key  1, keypos =    1, keylen =  64          

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#01, mode$, f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#03, mode$, f2%(03), rslt$(03), axd$(03))
            call "OPENFILE" (#07, mode$, f2%(07), rslt$(07), axd$(07))
            call "OPENFILE" (#08, mode$, f2%(08), rslt$(08), axd$(08))
            call "OPENFILE" (#27, "SHARE", f2%(27), rslt$(27), axd$(27))
  
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            call "EXTRACT" addr("ID", a_modid$)
            a_moddate$, date$ = date
            call "DATEFMT" (date$)

            a_text$(1) = "Part Number Deleted Through INVDETAL"
            edtmessage$ = "Enter Part Code to be Deleted then Press (RETU~
        ~RN)."


            goto L10000

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * GET PART NUMBER TO BE DELETED.                            *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, part$, subpart$

            call "ALLFREE"
L10120:     gosub'101(1%)
                  if keyhit%  = 16 then L65000
                  if keyhit% <>  0 then L10120
            gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
            gosub'101(0%)
                  if keyhit%  = 16 then L65000
                  if keyhit% <>  0 then L10120

            goto confirmdelete

L10340:     goto inputmode

        REM ***** CONFIRM IF REALLY TO DELETE PART OR NOT ******
        confirmdelete

            gosub confirmscreen
            if keyhit% <> 12% then L10340

            REM ******  GET RID OF PART  ********

                gosub undatasave
                goto inputmode

        EJECT
        REM *************************************************************~
            *       U N - S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * DELETE PART CODE WHERE-EVER WE DECIDED TO DELETE IT FROM. *~
            *************************************************************

        undatasave
        call "SHOSTAT" ("** Deleting Part Code from active data base **")

            str(readkey2$, 1%,25%) = part$
            str(readkey2$,26%,20%) = subpart$                
            gosub    INVMASTR
            gosub    INVDETAL
            gosub    INVPOOL
            gosub    INVQUAN
            line2$ = "Part: " & part$ & "  " & subpart$ & " DELETED "

        return

        EJECT
        REM *************************************************************~
            *    F I L E   A C C E S S I N G   R O U T I N E S          *~
            *************************************************************

        INVMASTR
        REM CALL "SHOSTAT" ("Removing from Parts Master               ")
            delete #1
            call "CDANPOST" (#1, "D")
            return

        INVDETAL
        REM CALL "SHOSTAT" ("Removing from Inventory Detail file")
            call "DELETE" (#3, readkey2$, 45%)
            return

        INVPOOL
        REM CALL "SHOSTAT" ("Removing from Inventory Pool file")
            call "DELETE" (#7, readkey2$, 45%)
            return

        INVQUAN
        REM CALL "SHOSTAT" ("Removing Inventory Quantity records")
            call "DELETE" (#8, readkey2$, 45%)
            return

        EJECT
        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * GET PART CODE TO BE EXECUTED.                             *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "INVDETAL: " & cms2v$
                if fieldnr% = 1% then lfac$(1) = hex(81)                 ~
                else                  lfac$(1) = hex(84)
                if fieldnr% = 1% then inpmessage$ = "Enter the Part Code ~
        ~to DELETE"                                                       ~
                else inpmessage$ = "Press (RETURN) to See If Eligible for~
        ~ Deletion Or PF(1) to Re-enter"

L40220:     accept                                                        ~
               at (01,02),                                               ~
                  "Delete Part Code From System",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Part Code",                                           ~
               at (06,30), fac(lfac$(1)),part$                  , ch(25),~
               at (07,02),                                               ~
                  "Sub Part ",                                    ~
               at (07,30), fac(lfac$(2)), subpart$              , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13% then L40570
                  call "MANUAL" ("HNYDLETE")
                  goto L40220

L40570:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L40220

        REM *************************************************************~
            *   D I S P L A Y   I N   U S E   S C R E E N               *~
            *                                                           *~
            * SHOW WHY PART CAN'T BE DELETED.                           *~
            *************************************************************
        inusescreen
                  line2$ = "Part: " & part$ & "  " & subpart$
                  str(line2$,62%) = "INVDETAL: " & cms2v$
                  inpmessage$ = "Press (RETURN) to Acknowledge & Continue"

        accept                                                           ~
               at (01,02), "Delete Part Code From System",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,03),                                               ~
        "This Part May Not be Deleted Because of the Following:",        ~
               at (05,03), fac(hex(84)), use$(1)       , ch(75),         ~
               at (06,03), fac(hex(84)), use$(2)       , ch(75),         ~
               at (07,03), fac(hex(84)), use$(3)       , ch(75),         ~
               at (08,03), fac(hex(84)), use$(4)       , ch(75),         ~
               at (09,03), fac(hex(84)), use$(5)       , ch(75),         ~
               at (10,03), fac(hex(84)), use$(6)       , ch(75),         ~
               at (11,03), fac(hex(84)), use$(7)       , ch(75),         ~
               at (12,03), fac(hex(84)), use$(8)       , ch(75),         ~
               at (13,03), fac(hex(84)), use$(9)       , ch(75),         ~
               at (14,03), fac(hex(84)), use$(10)      , ch(75),         ~
               at (15,03), fac(hex(84)), use$(11)      , ch(75),         ~
               at (16,03), fac(hex(84)), use$(12)      , ch(75),         ~
               at (17,03), fac(hex(84)), use$(13)      , ch(75),         ~
               at (18,03), fac(hex(84)), use$(14)      , ch(75),         ~
               at (19,03), fac(hex(84)), use$(15)      , ch(75),         ~
               at (20,03), fac(hex(84)), use$(16)      , ch(75),         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(000d0f10)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L41570
                  call "MANUAL" ("HNYDLETE")
                  goto inusescreen

L41570:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto inusescreen


        REM *************************************************************~
            * CONFIRM DELETE SCREEN                                     *~
            *                                                           *~
            * LAST CHANCE SCREEN BEFORE THIS GUY GOES BYE-BYE           *~
            *************************************************************
        confirmscreen
            inpmessage$ = "****  PLEASE CONFIRM DELETION ****"
                  line2$ = "Part: " & part$ & "  " & subpart$
                  str(line2$,62%) = "INVDETAL: " & cms2v$

L42070:     accept                                                       ~
               at (01,02), "Delete Part Code From System",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Part Code",                                           ~
               at (06,30), fac(hex(84)), part$                  , ch(25),~
               at (07,02),                                               ~
                  "Sub Part ",                                    ~
               at (07,30), fac(hex(84)), subpart$               , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02),                                               ~
                  "(RETURN) Do Not Delete Part ",                        ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65),                                               ~
                  "(12)DELETE PART!",                                    ~
                                                                         ~
               keys(hex(000c0f)),                                        ~
               key (keyhit%)

               if keyhit% <> 13 then L42400
                  call "MANUAL" ("HNYDLETE")
                  goto L42070

L42400:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto confirmscreen

        REM *************************************************************~
            *  FRONT-END WARNING SCREEN                                 *~
            *                                                           *~
            *  DISPLAY WHO, WHAT, WHERE, WHEN & WHY BEFORE ANY HARM IS  *~
            *  PERPETRATED.                                             *~
            *************************************************************
        warnscreen

        accept                                                           ~
               at (01,03),                                               ~
        "*****************  INVDETAL: DELETE PART CODE FROM SYSTEM  *****~
        ~************",                                                   ~
               at (02,03),                                               ~
        "* This Program MUST be run with NO ONE ELSE on the System! Any a~
        ~ttempt to  *",                                                   ~
               at (03,03),                                               ~
        "* access any Part Code will be Denied as long as this program is~
        ~ running.  *",                                                   ~
               at (04,03),                                               ~
        "****************************************************************~
        ~************",                                                   ~
               at (05,03),                                               ~
        "Program execution is as follows:",                              ~
               at (06,03),                                               ~
        "   (1) Enter the Part Code to be deleted.",                     ~
               at (07,03),                                               ~
        "   (2) The System will then attempt to determine if that part is~
        ~, in fact,",                                                     ~
               at (08,03),                                               ~
        "       Inactive.",                                              ~
               at (09,03),                                               ~
        "   (3) If Usage of the Part is detected, that Usage(s) is displa~
        ~yed.",                                                           ~
               at (10,03),                                               ~
        "   (4) If No Usage for the Part is found, You are given one last~
        ~ chance to",                                                     ~
               at (11,03),                                               ~
        "       abort. If You continue, the Part will be DELETED.",      ~
               at (12,03),                                               ~
        "****************************************************************~
        ~************",                                                   ~
               at (13,03),                                               ~
        "*        DELETION CRITERIA            !        WHAT IS DELETED  ~
        ~           *",                                                   ~
               at (14,03),                                               ~
        "*-------------------------------------!-------------------------~
        ~-----------*",                                                   ~
               at (15,03),                                               ~
        "* - No BOMS or Routes. No AutoReplcmnt! - Part Master File Data ~
        ~           *",                                                   ~
               at (16,03),                                               ~
        "* - No Inventory, On-Order, In-Process! - Vendor Price Informati~
        ~on         *",                                                   ~
               at (17,03),                                               ~
        "* - No Demand For Part. No SA History ! - Customer Price Informa~
        ~tion       *",                                                   ~
               at (18,03),                                               ~
        "****************************************************************~
        ~************",                                                   ~
               at (19,03),                                               ~
        "****    If in doubt, DON'T DELETE THE PART.  Subsequent re-use o~
        ~f the   ****",                                                   ~
               at (20,03),                                               ~
        "****    Part Code may cause history for the 'old' Part to appear~
        ~ for    ****",                                                   ~
               at (21,03),                                               ~
        "****    the 'new' Part.                                         ~
        ~        ****",                                                   ~
               at (22,03),                                               ~
        "****************************************************************~
        ~************",                                                   ~
               at (23,03),                                               ~
        "PF KEYS:",                                                      ~
               at (24,03),                                               ~
        "(RETURN)Continue    (13)Instructions    (15)Print Screen    (16)~
        ~Exit Program",                                                   ~
                                                                         ~
               keys(hex(000d0f1011)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L43840
                  call "MANUAL" ("HNYDLETE")
                  goto warnscreen

L43840:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto warnscreen

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * CHECK THAT PART ON FILE AND GET DESCRIPTION.              *~
            *************************************************************

            deffn'151(fieldnr%)
                errormsg$ = " "
                desc$ = hex(06) & "Select Part Code to Delete "
                str(readkey2$, 1%,25%) = part$
                str(readkey2$,26%,20%) = subpart$                
                call "GETCODE" (#1, readkey2$, desc$, 0%, 0, f1%(1))
                line2$ = " "
                if f1%(1) = 1% then L50081
                     errormsg$ = "Part Code not on file"      : return
L50081:         call "READ101" (#1, readkey2$, f1%(1))
                get #1 using L50091, text$
L50091:              FMT POS(98), CH(4)
                part$ = str(readkey2$,1%,25%)
                subpart$ = str(readkey2$,26%,20%)

                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One moment please")
            end
