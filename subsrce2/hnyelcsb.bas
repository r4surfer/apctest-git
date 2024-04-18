        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y  EEEEE  L       CCC    SSS   BBBB    *~
            *  H   H  NN  N  Y   Y  E      L      C   C  S      B   B   *~
            *  HHHHH  N N N   YYY   EEEE   L      C       SSS   BBBB    *~
            *  H   H  N  NN    Y    E      L      C   C      S  B   B   *~
            *  H   H  N   N    Y    EEEEE  LLLLL   CCC    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYELCSB - Searchs for and displays empty store locations *~
            *            when valid locations flag is set to "Y".       *~
            *            Allows either display or display and selection *~
            *            of an empty location depending on the users    *~
            *            Action.                                        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/14/91 ! Original                                 ! JBK *~
            * 01/31/94 ! Modified to correctly provide selection  ! MLJ *~
            *          !   capability for ACTION% 2 & 7 only.     !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "HNYELCSB" (store$,     /* Store to search                 */~
                        location$,  /* Location for start or retur     */~
                        action%,    /* Caller's Current Action         */~
                                    /*   1%  -  Display Only           */~
                                    /*   2%  -  Selection for transfer */~
                                    /*   3%  -  Display Only           */~
                                    /*   4%  -  Display Only           */~
                                    /*   5%  -  Display Only           */~
                                    /*   6%  -  Display Only           */~
                                    /*   7%  -  Selection for Add      */~
                        #1,         /* LOCATION  Location master file  */~
                        #2,         /* HNYLOCNS  Location quantity file*/~
                        #3,         /* STORNAME  Warehouse information */~
                        ret%)       /* Return code                     */~
                                    /*  Ret% = 0   'No Selection'      */~
                                    /*  Ret% = 1   'Selection made'    */

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            e_location$(300)75,          /* Empty Locations            */~
            errormsg$79,                 /* Error message              */~
            heading$79,                  /* Screen heading             */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$1,                      /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            location$8,                  /* Store Location             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            schar$(300)1,                /* Pseudo blank for selection */~
            sfac$(300)1,                 /* Fac for selection column   */~
            store$3,                     /* Store                      */~
            storedescr$32,               /* Description of Store       */~
            userid$3                     /* Current User Id            */~

        dim                                                              ~
            f1%(2)                       /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! LOCATION ! Location master file                     *~
            * #02 ! HNYLOCNS ! Inventory quantity by location file      *~
            *************************************************************~


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            inpmessage$  = "Press (RETURN) or PF16 to Exit the Display"


            call "GETCODE" (#3, store$, storedescr$, 1%, 0, f1%(3))
            str(line2$,62) = "HNYELCSB: " & str(cms2v$,,8)
            str(line2$,1,61) = "Warehouse (Store): " & store$ &          ~
                               " " & storedescr$

            heading$ = "  Location  Description                     " &  ~
                       "Comment"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

            gosub initialize_variables

            gosub dataload

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        REM  DISPLAY_ONLY
            if action% = 2% or action% = 7% then display_and_select
            gosub'101                  /* Display Screen - No Entry   */
            goto exit_program

        display_and_select
L12010:     init (hex(0b))  str(schar$(),,maxlines%)
            init (hex(86))  str(sfac$(),,maxlines%)
            if loaded% <> 0% then L12060
                schar$(1%) = " "
                sfac$(1%)  = hex(86)
L12060:     inpmessage$ = "Position Cursor (TAB) to Line and Press " &   ~
                          "(RETURN) to Return with that Code."
L12080:     gosub'101                  /* Display Screen - Selection  */
                  if keyhit%  = 16% then       exit_program
                  if keyhit% <>  0% then       L12080
            errormsg$ = " "
            fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  12% then L12200
            if top% + fieldnr% > 299% then L12200
            location$ = str(e_location$(top% + fieldnr% - 1%), 1,8)
            if location$ = " " then L12200
            ret% = 1%
            goto exit_program

L12200:     errormsg$ = "Invalid Selection.  Please try again."
            goto L12010

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ")  schar$()  :  init(hex(8c))  sfac$()
            maxlines%, loaded% = 0%
            top% = 1%
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            maxlines% = 0
            init (" ") e_location$()

            plowkey$ = str(store$) & location$

            call "SHOSTAT" ("Searching for EMPTY Locations...")

L30130:     call "PLOWNEXT" (#1, plowkey$, 3%, f1%(1))
            if f1%(1) = 0 then L30320

            readkey$ = str(plowkey$,1,11)
            call "PLOWNEXT" (#2, readkey$, 11%, f1%(2))
            if f1%(2) = 1% then L30130

            maxlines% = maxlines% + 1%
            if maxlines% < 300% then L30260
                e_location$(300) = "There are more records - " &         ~
                                    "see your SSA"
                return

L30260:         get #1, using L30290, str(e_location$(maxlines%), 1, 8),  ~
                                     str(e_location$(maxlines%),11,30),  ~
                                     str(e_location$(maxlines%),43,30)
L30290:              FMT POS(4), CH(8), POS(15), 2*CH(30)
                loaded% = loaded% + 1%
                goto L30130
L30320:     if maxlines% <> 0% then return
                str(e_location$(1),27,25) = "** No Empty Locations **"
                maxlines% = maxlines% + 1%
                return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101
L40070:       gosub set_pf1
              if action% = 2% or action% = 7% then lfac$ = hex(80)       ~
                                              else lfac$ = hex(8c)

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Search for Valid Empty Locations",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(ac)), heading$               , ch(79),~
               at (07,02), fac(lfac$),         schar$(top% + 0%), ch(01),~
                                                                         ~
               at (07,02), fac(sfac$(top% +  0%)),                       ~
                                               schar$(top% + 0%), ch(01),~
               at (08,02), fac(sfac$(top% +  1%)),                       ~
                                              schar$(top% +  1%), ch(01),~
               at (09,02), fac(sfac$(top% +  2%)),                       ~
                                              schar$(top% +  1%), ch(01),~
               at (10,02), fac(sfac$(top% +  3%)),                       ~
                                              schar$(top% +  3%), ch(01),~
               at (11,02), fac(sfac$(top% +  4%)),                       ~
                                              schar$(top% +  4%), ch(01),~
               at (12,02), fac(sfac$(top% +  5%)),                       ~
                                              schar$(top% +  5%), ch(01),~
               at (13,02), fac(sfac$(top% +  6%)),                       ~
                                              schar$(top% +  6%), ch(01),~
               at (14,02), fac(sfac$(top% +  7%)),                       ~
                                              schar$(top% +  7%), ch(01),~
               at (15,02), fac(sfac$(top% +  8%)),                       ~
                                              schar$(top% +  8%), ch(01),~
               at (16,02), fac(sfac$(top% +  9%)),                       ~
                                              schar$(top% +  9%), ch(01),~
               at (17,02), fac(sfac$(top% + 10%)),                       ~
                                              schar$(top% + 10%), ch(01),~
               at (18,02), fac(sfac$(top% + 11%)),                       ~
                                              schar$(top% + 11%), ch(01),~
               at (19,02), fac(sfac$(top% + 12%)),                       ~
                                              schar$(top% + 12%), ch( 1),~
                                                                         ~
               at (07,04), fac(hex(8c)), e_location$(top% + 0%) ,        ~
               at (08,04), fac(hex(8c)), e_location$(top% + 1%) ,        ~
               at (09,04), fac(hex(8c)), e_location$(top% + 2%) ,        ~
               at (10,04), fac(hex(8c)), e_location$(top% + 3%) ,        ~
               at (11,04), fac(hex(8c)), e_location$(top% + 4%) ,        ~
               at (12,04), fac(hex(8c)), e_location$(top% + 5%) ,        ~
               at (13,04), fac(hex(8c)), e_location$(top% + 6%) ,        ~
               at (14,04), fac(hex(8c)), e_location$(top% + 7%) ,        ~
               at (15,04), fac(hex(8c)), e_location$(top% + 8%) ,        ~
               at (16,04), fac(hex(8c)), e_location$(top% + 9%) ,        ~
               at (17,04), fac(hex(8c)), e_location$(top% + 10%),        ~
               at (18,04), fac(hex(8c)), e_location$(top% + 11%),        ~
               at (19,04), fac(hex(8c)), e_location$(top% + 12%),        ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
               keys(pfkeys$), key(keyhit%)

            if keyhit% <> 2% then L40730
                top% = 1%
                goto L40070

L40730:     if keyhit% <> 3% then L40770
                top% = max(maxlines% - 12%, 1%)
                goto L40070

L40770:     if keyhit% <> 4% then L40810
                top% = max(top% - 13%, 1%)
                goto L40070

L40810:     if keyhit% <> 5% then L40850
                top% = min(top% + 13%, maxlines% - 12%)
                goto L40070

L40850:     if keyhit% <> 6% then L40890
                top% = max(top% - 1%, 1%)
                goto L40070

L40890:     if keyhit% <> 7% then L40930
                top% = min(top% + 1%, maxlines% - 12%)
                goto L40070

L40930:        if keyhit% <> 13 then L40960
                  call "MANUAL" ("HNYELCSB") : goto L40110

L40960:        if keyhit% <> 15 then L40990
                  call "PRNTSCRN" : goto L40110

L40990:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               u3% = u3%
               return

        set_pf1
            pf$(1) = "                                        " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)First (4)Previous (6)Prev one        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Last  (5)Next     (7)Next one        " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(ff020304050607ffffffffff0dff0f1000)
            if action% <> 2% or action% <> 7% then L41140
                str(pf$(3),61) = "(16)Cancel & Return"
L41140:     if top% > 1% then L41200
                str(pf$(2), 1,32) = " "        /* Surpress first/prev  */
                str(pfkey$, 2,1)  = hex(ff)    /* Deactivate PF 2      */
                str(pfkey$, 4,1)  = hex(ff)    /* Deactivate PF 4      */
                str(pfkey$, 6,1)  = hex(ff)    /* Deactivate PF 6      */

L41200:     if top% + 12% < maxlines% then return
                str(pf$(3), 1,32) = " "        /* Surpress last/next   */
                str(pfkey$, 3,1)  = hex(ff)    /* Deactivate PF 3      */
                str(pfkey$, 5,1)  = hex(ff)    /* Deactivate PF 5      */
                str(pfkey$, 7,1)  = hex(ff)    /* Deactivate PF 7      */
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
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program

            end
