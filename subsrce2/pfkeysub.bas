        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   FFFFF  K   K  EEEEE  Y   Y   SSS   U   U  BBBB    *~
            *  P   P  F      K  K   E      Y   Y  S      U   U  B   B   *~
            *  PPPP   FFFF   KKK    EEEE    YYY    SSS   U   U  BBBB    *~
            *  P      F      K  K   E        Y        S  U   U  B   B   *~
            *  P      F      K   K  EEEEE    Y     SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PFKEYSUB - This subroutine displays for selection the PF  *~
            *            keys available for a program screen.  Its use  *~
            *            is for longer descriptives when the normal PF  *~
            *            keys are many and text has been severely       *~
            *            abbreviated.                                   *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/29/93 ! Original                                 ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

            sub "PFKEYSUB" (header$(), pf$(), descr$(), return%)

        REM Header and PF key section mimic the calling program by being ~
            passed in as HEADER$ and PF$.  DESCR$ is the expanded descr- ~
            iptions to be shown on this screen.  Each description can be ~
            up to 32 characters long.  A PF Key selection can be made by ~
            pressing a PF key or RETURN will return to the caller        ~
            without making a selection.   If no selection is made,       ~
            RETURN% will be 99%; otherwise, RETURN% will indicate the PF ~
            Key pressed.                                                 ~
            NOTE - This is to be called by PF29.  Cursor in caller       ~
                   should be at (1,1).   This disallows a conflict with  ~
                   Soft Enables.  Calling program should have PF13 prompt~
                   highlit as a flag that more info is available.

        dim                                                              ~
            descr$(32)32,                /* PF Descriptions from Caller*/~
            edtmessage$79,               /* Edit screen message        */~
            header$(2)79,                /* Screen Header from Caller  */~
            pf$(3)79,                    /* PF Screen Literals - Caller*/~
            pf_no$(32)4,                 /* PF Numbers 1-32            */~
            pfkeys$33                    /* PF Key Hex Values          */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "Press a PF Key selection or press RETURN to"&~
                           " return to the previous screen."

            pf_no$(01%) = "PF 1"       : pf_no$(17%) = "PF17"
            pf_no$(02%) = "PF 2"       : pf_no$(18%) = "PF18"
            pf_no$(03%) = "PF 3"       : pf_no$(19%) = "PF19"
            pf_no$(04%) = "PF 4"       : pf_no$(20%) = "PF20"
            pf_no$(05%) = "PF 5"       : pf_no$(21%) = "PF21"
            pf_no$(06%) = "PF 6"       : pf_no$(22%) = "PF22"
            pf_no$(07%) = "PF 7"       : pf_no$(23%) = "PF23"
            pf_no$(08%) = "PF 8"       : pf_no$(24%) = "PF24"
            pf_no$(09%) = "PF 9"       : pf_no$(25%) = "PF25"
            pf_no$(10%) = "PF10"       : pf_no$(26%) = "PF26"
            pf_no$(11%) = "PF11"       : pf_no$(27%) = "PF27"
            pf_no$(12%) = "PF12"       : pf_no$(28%) = "PF28"
            pf_no$(13%) = "PF13"       : pf_no$(29%) = "PF29"
            pf_no$(14%) = "PF14"       : pf_no$(30%) = "PF30"
            pf_no$(15%) = "PF15"       : pf_no$(31%) = "PF31"
            pf_no$(16%) = "PF16"       : pf_no$(32%) = "PF32"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

            gosub initialize_variables

            gosub'101

            return% = keyhit%  /* PF Key Hit, so Set and End */
            if return% = 0% then return% = 99%
            goto L65000

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            return% = 98%
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101
              gosub set_pf1

L40100:     accept                                                       ~
               at (01,02), fac(hex(8c)), header$(1%)            , ch(79),~
               at (02,02), fac(hex(ac)), header$(2%)            , ch(79),~
                                                                         ~
               at (04,04), fac(hex(8c)),   pf_no$(01%)          , ch(04),~
               at (04,09), fac(hex(84)),   descr$(01%)          , ch(32),~
               at (05,04), fac(hex(8c)),   pf_no$(02%)          , ch(04),~
               at (05,09), fac(hex(84)),   descr$(02%)          , ch(32),~
               at (06,04), fac(hex(8c)),   pf_no$(03%)          , ch(04),~
               at (06,09), fac(hex(84)),   descr$(03%)          , ch(32),~
               at (07,04), fac(hex(8c)),   pf_no$(04%)          , ch(04),~
               at (07,09), fac(hex(84)),   descr$(04%)          , ch(32),~
               at (08,04), fac(hex(8c)),   pf_no$(05%)          , ch(04),~
               at (08,09), fac(hex(84)),   descr$(05%)          , ch(32),~
               at (09,04), fac(hex(8c)),   pf_no$(06%)          , ch(04),~
               at (09,09), fac(hex(84)),   descr$(06%)          , ch(32),~
               at (10,04), fac(hex(8c)),   pf_no$(07%)          , ch(04),~
               at (10,09), fac(hex(84)),   descr$(07%)          , ch(32),~
               at (11,04), fac(hex(8c)),   pf_no$(08%)          , ch(04),~
               at (11,09), fac(hex(84)),   descr$(08%)          , ch(32),~
               at (12,04), fac(hex(8c)),   pf_no$(09%)          , ch(04),~
               at (12,09), fac(hex(84)),   descr$(09%)          , ch(32),~
               at (13,04), fac(hex(8c)),   pf_no$(10%)          , ch(04),~
               at (13,09), fac(hex(84)),   descr$(10%)          , ch(32),~
               at (14,04), fac(hex(8c)),   pf_no$(11%)          , ch(04),~
               at (14,09), fac(hex(84)),   descr$(11%)          , ch(32),~
               at (15,04), fac(hex(8c)),   pf_no$(12%)          , ch(04),~
               at (15,09), fac(hex(84)),   descr$(12%)          , ch(32),~
               at (16,04), fac(hex(8c)),   pf_no$(13%)          , ch(04),~
               at (16,09), fac(hex(84)),   descr$(13%)          , ch(32),~
               at (17,04), fac(hex(8c)),   pf_no$(14%)          , ch(04),~
               at (17,09), fac(hex(84)),   descr$(14%)          , ch(32),~
               at (18,04), fac(hex(8c)),   pf_no$(15%)          , ch(04),~
               at (18,09), fac(hex(84)),   descr$(15%)          , ch(32),~
               at (19,04), fac(hex(8c)),   pf_no$(16%)          , ch(04),~
               at (19,09), fac(hex(84)),   descr$(16%)          , ch(32),~
               at (04,43), fac(hex(8c)),   pf_no$(17%)          , ch(04),~
               at (04,48), fac(hex(84)),   descr$(17%)          , ch(32),~
               at (05,43), fac(hex(8c)),   pf_no$(18%)          , ch(04),~
               at (05,48), fac(hex(84)),   descr$(18%)          , ch(32),~
               at (06,43), fac(hex(8c)),   pf_no$(19%)          , ch(04),~
               at (06,48), fac(hex(84)),   descr$(19%)          , ch(32),~
               at (07,43), fac(hex(8c)),   pf_no$(20%)          , ch(04),~
               at (07,48), fac(hex(84)),   descr$(20%)          , ch(32),~
               at (08,43), fac(hex(8c)),   pf_no$(21%)          , ch(04),~
               at (08,48), fac(hex(84)),   descr$(21%)          , ch(32),~
               at (09,43), fac(hex(8c)),   pf_no$(22%)          , ch(04),~
               at (09,48), fac(hex(84)),   descr$(22%)          , ch(32),~
               at (10,43), fac(hex(8c)),   pf_no$(23%)          , ch(04),~
               at (10,48), fac(hex(84)),   descr$(23%)          , ch(32),~
               at (11,43), fac(hex(8c)),   pf_no$(24%)          , ch(04),~
               at (11,48), fac(hex(84)),   descr$(24%)          , ch(32),~
               at (12,43), fac(hex(8c)),   pf_no$(25%)          , ch(04),~
               at (12,48), fac(hex(84)),   descr$(25%)          , ch(32),~
               at (13,43), fac(hex(8c)),   pf_no$(26%)          , ch(04),~
               at (13,48), fac(hex(84)),   descr$(26%)          , ch(32),~
               at (14,43), fac(hex(8c)),   pf_no$(27%)          , ch(04),~
               at (14,48), fac(hex(84)),   descr$(27%)          , ch(32),~
               at (15,43), fac(hex(8c)),   pf_no$(28%)          , ch(04),~
               at (15,48), fac(hex(84)),   descr$(28%)          , ch(32),~
               at (16,43), fac(hex(8c)),   pf_no$(29%)          , ch(04),~
               at (16,48), fac(hex(84)),   descr$(29%)          , ch(32),~
               at (17,43), fac(hex(8c)),   pf_no$(30%)          , ch(04),~
               at (17,48), fac(hex(84)),   descr$(30%)          , ch(32),~
               at (18,43), fac(hex(8c)),   pf_no$(31%)          , ch(04),~
               at (18,48), fac(hex(84)),   descr$(31%)          , ch(32),~
               at (19,43), fac(hex(8c)),   pf_no$(32%)          , ch(04),~
               at (19,48), fac(hex(84)),   descr$(32%)          , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then L40920
                  call "PRNTSCRN" : goto L40100

L40920:        return

        set_pf1
            pfkeys$ = hex(0102030405060708090a0b0c0d0e0f10) &            ~
                      hex(1112131415161718191a1b1c1d1e1f2000)

            for i% = 1% to 32%
                if descr$(i%) <> " " then L41040
                     pf_no$(i%) = " "
                     str(pfkeys$,i%,1%) = hex(ff)
L41040:         next i%
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

            end
