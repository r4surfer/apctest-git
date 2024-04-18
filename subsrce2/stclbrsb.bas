        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   L      BBBB   RRRR    SSS   BBBB    *~
            *  S        T    C   C  L      B   B  R   R  S      B   B   *~
            *   SSS     T    C      L      BBBB   RRRR    SSS   BBBB    *~
            *      S    T    C   C  L      B   B  R   R      S  B   B   *~
            *   SSS     T     CCC   LLLLL  BBBB   R   R   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCLBRSB - Allows management of Labor Class Std Rates.    *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/31/87 ! Original                                 ! ERN *~
            * 06/07/91 ! PRRs 11443,11766.  Display description.  ! JDH *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "STCLBRSB"   (set$,            /* Cost Set                 */~
                          set_descr$,      /* Set Description          */~
                          mod$,            /* Global/Mod Flag (Y/N/F)  */~
                          bucket_ids$(),   /* Cost Bucket IDs          */~
                          bucket_descrs$(),/* Cost Bucket Descriptions */~
                          #2, #3, #13)     /* SYSFILE2, GENCODES,      */
                                           /* STCLABOR UFBs            */

        dim                                                              ~
            bucket_ids$(12)10,           /* Cost Bucket IDs            */~
            bucket_descrs$(12)20,        /* Cost Bucket Descriptions   */~
            cat$(8)22,                   /* Rate Categories for Report */~
            company$60,                  /* Company Name for Report    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hdr$(3)40,                   /* Screen Headings            */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lbr_class$4,                 /* Labor Class Code           */~
            lbr_class_descr$30,          /* Labor Class Code Descr     */~
            lfac$(20,2)1,                /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            mod$1, mod_flag$30,          /* Global / Modification Flag */~
            new_data$(2)170,             /* Data after Modifications   */~
            old_data$(2)170,             /* Data before Load           */~
            ovhd_pct$10,                 /*            Overhead %      */~
            p%(1),                       /* Reciever for Search fn     */~
            pails%(8),                   /* Bucket Distributions       */~
            pail_descrs$(8)20,           /*                            */~
            pail_ids$(8)10,              /*                            */~
            pail_nrs$(8)2,               /*                            */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* A Plow Key                 */~
            prt_class$4, prt_descr$30,   /* Cose and Descr for Report  */~
            rate$10,                     /* Numeric Print for Report   */~
            rates(6), rates$(6)10,       /* Standards Rates            */~
            readkey$99,                  /* Read Key                   */~
            runtime$8,                   /* Report Run Time            */~
            set$8, set_descr$30,         /* Cost Set and Description   */~
            trans$24,                    /* Field Location Map         */~
            userid$3                     /* Who it is                  */

        dim f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.04 05/19/92 UNIX Compatibility Changes      "
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 2 ! SYSFILE2 ! System Reference File                    *~
            * # 3 ! GENCODES ! System General Codes file                *~
            * #13 ! STCLABOR ! Standard Costing Labor Standards         *~
            * #15 ! STCCHNGS ! Standard Costing Chenges File.           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date  :  call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            line2$ = "Cost Set: " & set$ & "  (" & set_descr$ & ")"
            str(line2$,62) = "STCLBRSB: " & str(cms2v$,,8)

            hdr$(1) = "     Value"
            hdr$(2) = "Bucket ID "
            hdr$(3) = "Bucket Description & Nr."

            mod_flag$ = " "
            if mod$ = "F" then mod_flag$ = "Set Frozen- Edit not Allowed"

            trans$ = hex(00000000000020000304050607080000090a000000000000)
                     /*   1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2  */

            cat$(1), cat$(2) = "Standard Fixed Dollars"
            cat$(3), cat$(4) = "Standard Dollars/Part "
            cat$(5), cat$(6) = "Standard Dollars/Hour "
            cat$(7)          = "Actual's Distribution "
            cat$(8)          = "Overhead Percent/Distr"

*        Set up GENCODES file header if so required
            readkey$ = all(hex(00))
            str(readkey$,10) = "LBR CLASS"
            write #3 using L09350, str(readkey$,,24), "Labor Class Codes", ~
                                 " 4", " ", eod goto L10000
L09350:         FMT CH(24), CH(30), CH(2), CH(72)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub L29000

            for fieldnr% = 1% to 10%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0 then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10190
                         fieldnr% = max(2%, fieldnr% - 1%)
                         if fieldnr% = 2% and lbr% = 1% then fieldnr% = 3%
                         gosub'051(fieldnr%)
                         goto L10100
L10190:               if keyhit% = 14 then print_listing
                      if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editmode
          if mod_flag$ > " " then display_only
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 12 then       delete_code
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editmode
L11140:     fieldnr% = val(str(trans$,cursor%(1),1))
            if fieldnr% < 2 or fieldnr% > 10 then editmode
            if fieldnr% = lastfieldnr% then editmode
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editmode
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
                  goto L11140

        display_only /* Set frozen- just allow display of Mapping      */
            gosub'101(0%, 3%)
            goto inputmode

        delete_code
            u3% = 2%
            call "ASKUSER" (u3%, "*** CONFIRM DELETE ***",               ~
                            "Press RETURN to Delete -or- PF-1 to Exit.", ~
                            "Note: If the code is used all standards",   ~
                            "will be interpreted as zeroes.")
            if u3%  = 1% then editmode
            if u3% <> 0% then delete_code
                call "READ101" (#3, "LBR CLASS" & lbr_class$, onfile%)
                if onfile% <> 0% then delete #3  /* Drop from GENCODES */
                call "READ101" (#13, lbr_class$, onfile%)
                if onfile% = 0% then inputmode
                     delete #13
                     str(new_data$()) = " "
                     gosub flag_for_global
                     goto  inputmode


        REM *************************************************************~
            *               P R I N T    L I S T I N G                  *~
            * --------------------------------------------------------- *~
            * Print Listing of Labor Rates File.                        *~
            *************************************************************
        print_listing
            call "SHOSTAT" ("Printing Labor Rates Listing")
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            page% = 0% : line% = 857%
            select printer(134)
            call "SETPRNT" ("STC003", " ", 0%, 0%)
            plowkey$ = hex(00)
            count%   = 0%

        report_loop
            call "PLOWNEXT" (#13, plowkey$, 0%, f1%(13))
            if f1%(13) = 0% then end_report

            get #13 using L12200, lbr_class$, rates(), pails%(), ovhd_pct
L12200:         FMT CH(4), 6*PD(14,4), 8*BI(1), PD(14,4)
            readkey$ = "LBR CLASS" & lbr_class$
            call "GETCODE" (#3, readkey$, lbr_class_descr$, 0%, 99,f1%(3))
            prt_class$ = lbr_class$ : prt_descr$ = lbr_class_descr$

            for r% = 1% to 6%
                call "CONVERT" (rates(r%), 4.4, rates$(r%))
                if rates(r%) = 0% then rates$(r%) = " "
            next r%
            for r% = 1% to 8%
                gosub describe_pail
            next r%
            call "CONVERT" (ovhd_pct, 2.2, ovhd_pct$)
            if ovhd_pct = 0 then ovhd_pct$ = " "

            printed% = 0%
            for r% = 1% to 8%
                if r% <= 6% then rate$ = rates$(r%)
                if r%  = 7% then rate$ = " "
                if r%  = 8% then rate$ = ovhd_pct$
                if rate$ = " " and pails%(r%) = 0% then L12490

                printed% = 1%
                if line% > 50% then gosub page_heading
                print using L12960, prt_class$, prt_descr$, cat$(r%),     ~
                                  rate$, pail_ids$(r%), pail_descrs$(r%),~
                                  pail_nrs$(r%)
                line% = line% + 1%
                prt_class$, prt_descr$ = " "
L12490:     next r%
            if printed% = 0% then report_loop
                count% = count% + 1%
                print
                line% = line% + 1%
                goto report_loop

        end_report
            if count% > 0% then L12620
                call "ASKUSER" (0%, "NOTHING TO PRINT",                  ~
                                "There are no codes to print.",  " ",    ~
                                "Press RETURN to Continue...")
                goto L12650
L12620:     print using L12980, count%
            print
            print "** END OF REPORT **"
L12650:     call "SETPRNT" ("STC003", " ", 0%, 1%)
            close printer
            goto inputmode

        page_heading
            page% = page% + 1%  :  line% = 8%
            print page
            print using L12840, date$, runtime$, company$
            print using L12860, page%
            print
            print using L12880, set$, set_descr$
            print
            print using L12900
            print using L12920
            print using L12940
            prt_class$ = lbr_class$ : prt_descr$ = lbr_class_descr$
            return


L12840: %RUN DATE: ######## ########             ########################~
        ~####################################              STCLBRSB:STC003
L12860: %                                                 LABOR CLASS RAT~
        ~ES AND DISTRIBUTION LISTING                           PAGE: ##
L12880: %COST SET: ########  ##############################

L12900: %         LABOR                                                  ~
        ~                      -STANDARD COST BUCKET DISTRIBUTION-
L12920: %         CLASS  LABOR CLASS CODE DESCRIPTION     RATE CATEGORY  ~
        ~               RATE   BUCKET ID  BUCKET DESCRIPTION    NO
L12940: %         -----  ------------------------------   ---------------~
        ~-------  ----------   ----------  --------------------  --
L12960: %          ####  ##############################   ###############~
        ~#######  ##########   ##########  ####################  ##
L12980: %#### CODES LISTED

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20200,         /* Labor Class Code   */    ~
                              L20230,         /* Labor Class Descr  */    ~
                              L20270,         /* Fixed $s (1)       */    ~
                              L20300,         /* Fixed $s (2)       */    ~
                              L20330,         /* $/Part (1)         */    ~
                              L20360,         /* $/Part (2)         */    ~
                              L20390,         /* $/Hour (1)         */    ~
                              L20420,         /* $/Hour (2)         */    ~
                              L20450,         /* Direct Labor Dstr  */    ~
                              L20480          /* Overhead %         */
            return

L20200
*        Def/Enable LABOR CLASS CODE            LBR_CLASS$
            return

L20230
*        Def/Enable LABOR CLASS CODE DESCR      LBR_CLASS_DESCR$
            if lbr% = 1% then enabled% = 0%
            return

L20270
*        Def/Enable FIXED $S #1
            return

L20300
*        Def/Enable FIXED $S #2
            return

L20330
*        Def/Enable $/PART #1
            return

L20360
*        Def/Enable $/PART #2
            return

L20390
*        Def/Enable $/HOUR #1
            return

L20420
*        Def/Enable $/HOUR #2
            return

L20450
*        Def/Enable DIRECT LABOR DISTR
            return

L20480
*        Def/Enable OVERHEAD %
            return


        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Labor Class Code.  Enter a '?' to list Rates on file.  ",~
         "Enter Labor Class Code's Description.                        ",~
         "Enter Standard Fixed Dollars and Standard Cost Bucket ID.    ",~
         "Enter Standard Fixed Dollars and Standard Cost Bucket ID.    ",~
         "Enter Standard Dollars per Part and Standard Cost Bucket ID. ",~
         "Enter Standard Dollars per Part and Standard Cost Bucket ID. ",~
         "Enter Standard Dollars per Hour and Standard Cost Bucket ID. ",~
         "Enter Standard Dollars per Hour and Standard Cost Bucket ID. ",~
         "Enter Cost Bucket ID for Actual Direct Labor Distribution.   ",~
         "Enter Overhead Percentage for Actual Labor Distribution.     "

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, lbr_class$,                ~
                      lbr_class_descr$, pail_ids$(), pail_descrs$(),     ~
                      pail_nrs$(), rates$(), ovhd_pct$
            ovhd_pct = 0
            mat pails% = zer
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                return clear all
                goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        load_data
            init (hex(00)) old_data$()
            call "READ100" (#13, lbr_class$, onfile%)
            if onfile% = 0% and mod$ = "F" then                          ~
                     errormsg$ = "No rates on file for Labor Class Code"
            if onfile% = 0% then return
                get #13 using L30120, rates(), pails%(), ovhd_pct
L30120:              FMT XX(4), 6*PD(14,4), 8*BI(1), PD(14,4)
                get #13, str(old_data$(),,323)
                for r% = 1% to 6%
                     call "CONVERT" (rates(r%), 4.4, rates$(r%))
                     if rates(r%) = 0% then rates$(r%) = " "
                next r%
                for r% = 1% to 8%
                     gosub describe_pail
                next r%
                call "CONVERT" (ovhd_pct, 2.2, ovhd_pct$)
                if ovhd_pct = 0 then ovhd_pct$ = " "
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "READ101" (#13, lbr_class$, onfile%)
            call "PACKZERO" (rates(), str(new_data$(),,48))
            put #13 using L31100, lbr_class$, str(new_data$(),,48),       ~
                                 pails%(), ovhd_pct, " "
L31100:         FMT CH(4), CH(48), 8*BI(1), PD(14,4), CH(255)
            get #13, str(new_data$(),,323)
            if onfile% = 0% then write #13 else rewrite #13

            if lbr% = 1% then flag_for_global
                write #3 using L31180, "LBR CLASS", lbr_class$,           ~
                                      lbr_class_descr$, " ",             ~
                                      eod goto flag_for_global
L31180:              FMT CH(9), CH(15), CH(30), CH(74)

        flag_for_global
            if mod$ = "Y" then return    /* Global rollup pending      */
                str(old_data$(),59,10), str(new_data$(),59,10) = " "
                if str(old_data$()) = str(new_data$()) then return
                     readkey$ = "STC.HDR." & set$
                     call "READ101" (#2, readkey$, f1%(2))
                     put #2 using L31270, "Y"
L31270:                   FMT POS(420), CH(1)
                     rewrite #2
                     return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1
            if errormsg$ <> " " then L40280
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub   L40230,         /* Labor Class Code  */   ~
                                L40260,         /* Class Code Descr  */   ~
                                L40250,         /* Fixed $s (1)      */   ~
                                L40250,         /* Fixed $s (2)      */   ~
                                L40250,         /* $/Part (1)        */   ~
                                L40250,         /* $/Part (2)        */   ~
                                L40250,         /* $/Hour (1)        */   ~
                                L40250,         /* $/Hour (2)        */   ~
                                L40250,         /* Direct Labor Dstr */   ~
                                L40250          /* Overhead %        */
            goto L40280
L40230:       lfac$(fieldnr%,1) = hex(81)  :  return  /* Upper Only */

L40250:       lfac$(fieldnr%,1) = hex(82)             /* Numeric    */
L40260:       lfac$(fieldnr%,2) = hex(80)  :  return  /* Upper/Lower*/

L40280:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Standard Labor Rates",                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), mod_flag$              , ch(30),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Labor Class Code",                           ~
               at (06,22), fac(lfac$(1,1)), lbr_class$          , ch(04),~
               at (06,34), fac(lfac$(2,2)), lbr_class_descr$    , ch(30),~
                                                                         ~
               at (08,02), "STANDARDS",                                  ~
               at (08,22), fac(hex(ac)),   hdr$(1)              , ch(10),~
               at (08,34), fac(hex(ac)),   hdr$(2)              , ch(10),~
               at (08,46), fac(hex(ac)),   hdr$(3)              , ch(24),~
               at (09,02), "  Fixed    #1",                              ~
               at (09,22), fac(lfac$(3,1)), rates$(1)           , ch(10),~
               at (09,34), fac(lfac$(3,2)), pail_ids$(1)        , ch(10),~
               at (09,46), fac(hex(8c)),    pail_descrs$(1)     , ch(20),~
               at (09,68), fac(hex(8c)),    pail_nrs$(1)        , ch(02),~
                                                                         ~
               at (10,02), "           #2",                              ~
               at (10,22), fac(lfac$(4,1)), rates$(2)           , ch(10),~
               at (10,34), fac(lfac$(4,2)), pail_ids$(2)        , ch(10),~
               at (10,46), fac(hex(8c)),    pail_descrs$(2)     , ch(20),~
               at (10,68), fac(hex(8c)),    pail_nrs$(2)        , ch(02),~
                                                                         ~
               at (11,02), "  Per Part #1",                              ~
               at (11,22), fac(lfac$(5,1)), rates$(3)           , ch(10),~
               at (11,34), fac(lfac$(5,2)), pail_ids$(3)        , ch(10),~
               at (11,46), fac(hex(8c)),    pail_descrs$(3)     , ch(20),~
               at (11,68), fac(hex(8c)),    pail_nrs$(3)        , ch(02),~
                                                                         ~
               at (12,02), "           #2",                              ~
               at (12,22), fac(lfac$(6,1)), rates$(4)           , ch(10),~
               at (12,34), fac(lfac$(6,2)), pail_ids$(4)        , ch(10),~
               at (12,46), fac(hex(8c)),    pail_descrs$(4)     , ch(20),~
               at (12,68), fac(hex(8c)),    pail_nrs$(4)        , ch(02),~
                                                                         ~
               at (13,02), "  Per Hour #1",                              ~
               at (13,22), fac(lfac$(7,1)), rates$(5)           , ch(10),~
               at (13,34), fac(lfac$(7,2)), pail_ids$(5)        , ch(10),~
               at (13,46), fac(hex(8c)),    pail_descrs$(5)     , ch(20),~
               at (13,68), fac(hex(8c)),    pail_nrs$(5)        , ch(02),~
                                                                         ~
               at (14,02), "           #2",                              ~
               at (14,22), fac(lfac$(8,1)), rates$(6)           , ch(10),~
               at (14,34), fac(lfac$(8,2)), pail_ids$(6)        , ch(10),~
               at (14,46), fac(hex(8c)),    pail_descrs$(6)     , ch(20),~
               at (14,68), fac(hex(8c)),    pail_nrs$(6)        , ch(02),~
                                                                         ~
               at (16,02), "ACTUALS DISTRIBUTION",                       ~
               at (17,02), "  Direct Labor",                             ~
               at (17,22), "     --.--",                                 ~
               at (17,34), fac(lfac$(9,2)), pail_ids$(7)        , ch(10),~
               at (17,46), fac(hex(8c)),    pail_descrs$(7)     , ch(20),~
               at (17,68), fac(hex(8c)),    pail_nrs$(7)        , ch(02),~
                                                                         ~
               at (18,02), "  Overhead (%)",                             ~
               at (18,22), fac(lfac$(10,1)), ovhd_pct$          , ch(10),~
               at (18,34), fac(lfac$(10,2)), pail_ids$(8)       , ch(10),~
               at (18,46), fac(hex(8c)),    pail_descrs$(8)     , ch(20),~
               at (18,68), fac(hex(8c)),    pail_nrs$(8)        , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41010
                  call "MANUAL" ("STCLBRSB") : goto L40280

L41010:        if keyhit% <> 15 then L41040
                  call "PRNTSCRN" : goto L40280

L41040:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 3% then L41440
        if edit% = 2% then L41250     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "   (14)Print Listing   (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Routine"
            pfkeys$ = hex(01ffff04ffffffffffffffff0d0e0f1000)
            if fieldnr% = 1% then L41210
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
                str(pf$(2),44,18) = " "  :  str(pfkeys$,14,1) = hex(ff)
L41210:     if fieldnr% > 2% then L41230
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41230:     return

L41250: if fieldnr% > 0% then L41340  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "  (12)Delete           (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0c0dff0f1000)
            return
L41340:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

L41440:     pf$() = " "              /*  Display Only           */
            pfkeys$ = hex(0102030405060708090a0b0c0d0e0f1000)
            inpmessage$ = "Press RETURN to Continue..."
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            r% = fieldnr% - 2%
            errormsg$ = " "
            on fieldnr% gosub L50210,         /* Labor Class Code       */~
                              L50460,         /* Labor Class Descr      */~
                              L50490,         /* Std Rates and Pails    */~
                              L50490,         /* Std Rates and Pails    */~
                              L50490,         /* Std Rates and Pails    */~
                              L50490,         /* Std Rates and Pails    */~
                              L50490,         /* Std Rates and Pails    */~
                              L50490,         /* Std Rates and Pails    */~
                              L50540,         /* Direct Labor Dstr Pail */~
                              L50580          /* Overhead % and Pail    */
            return

L50210
*        Test for LABOR CLASS CODE             LBR_CLASS$
*          If a '?' is entered then display rates file else get code
*          from GENCODES.
            if lbr_class$ <> "?" then L50330
                lbr_class$ = " "
                call "PLOWCODE" (#13, lbr_class$, " ", 0%, 0, f1%(13))
                if f1%(13) = 0% then L50430
                     readkey$ = "LBR CLASS" & lbr_class$
                     call "GETCODE" (#3, readkey$, lbr_class_descr$,     ~
                                                          0%, 99, lbr%)
                     if lbr% = 0% then lbr_class_descr$ = " "
                     goto L50380
L50330:     readkey$ = "LBR CLASS" & lbr_class$
            call "PLOWCODE" (#3, readkey$, lbr_class_descr$,             ~
                                                         9%, 0.3, lbr%)
            if lbr% = 1% then lbr_class$ = str(readkey$,10)
            if lbr_class$ = " " then L50430

L50380:     gosub load_data
            if onfile% = 0% then return
                return clear all
                goto editmode

L50430:     errormsg$ = hex(00)
            return

L50460
*        Test for Description                    LBR_CLASS_DESCR$
            return

L50490
*        Test for Standards                      RATES$() / PAIL_ID$()
            gosub test_rates
            gosub test_pails
            return

L50540
*        Test for DIRECT LABOR PAIL              PAIL_ID$(7)
            gosub test_pails
            return

L50580
*        Test for OVERHEAD% & PAIL               OVHD_PCT$ / PAIL_ID$(8)
            if ovhd_pct$ = " " then ovhd_pct$ = "0"
            convert ovhd_pct$ to ovhd_pct, data goto L50610 : goto L50620
L50610:         errormsg$ = "Invalid entry for Overhead %" : return
L50620:     call "CONVERT" (ovhd_pct, 2.2, ovhd_pct$)
            if ovhd_pct = 0 then ovhd_pct$ = " "
            gosub test_pails
            return

        test_rates
            if rates$(r%) = " " then rates$(r%) = "0"
            convert rates$(r%) to rates(r%), data goto L50700 : goto L50730
L50700:         errormsg$ = "Invalid Entry for Standard Rate"
                return clear
                return
L50730:     call "CONVERT" (rates(r%), 4.4, rates$(r%))
            if rates(r%) = 0 then rates$(r%) = " "
            lfac$(fieldnr%, 1) = hex(8c)
            return

        test_pails
            if r% <= 6% then rate = rates(r%)
            if r%  = 7% then rate = 1
            if r%  = 8% then rate = ovhd_pct
            if rate <> 0 or pail_ids$(r%) > " " then L50890
L50830:         pails%(r%) = 0%
                goto L51010
L50890:     if pail_ids$(r%) = " " then L50930
            search str(bucket_ids$()) = pail_ids$(r%) to p%() step 10%
            if p%(1) = 0% then L50930
                pails%(r%) = (p%(1)+9%) / 10%
                goto L51010
L50930:     readkey$ = "STC.BD." & str(userid$) & "."
            call "PLOWCODE" (#2, readkey$, " ", 11%, 0.34, f1%(2))
            if f1%(2) = 1% then L51000
                if rate   = 0 then L50830
                errormsg$ = "Invalid Distribution Cost Bucket"
                return clear
                return
L51000:     convert str(readkey$,12,2) to pails%(r%)
L51010:     gosub describe_pail
            return

        describe_pail     /* Describe Pail for PAILS%(R%)    */
            pail_ids$(r%), pail_descrs$(r%), pail_nrs$(r%) = " "
            if pails%(r%) = 0% then return
                readkey$ = "STC.BD." & str(userid$) & "."
                convert pails%(r%) to str(readkey$,12,2), pic(#0)
                call "READ100" (#2, readkey$, f1%(2))
                if f1%(2) = 0% then return
                     get #2 using L51130, pail_nrs$(r%),                  ~
                                         pail_ids$(r%), pail_descrs$(r%)
L51130:                   FMT POS(12), CH(2), POS(21), CH(10), XX(4),    ~
                              CH(20)
                     return
        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            end
