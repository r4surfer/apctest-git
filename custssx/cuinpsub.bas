        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   U   U  IIIII  N   N  PPPP    SSS   U   U  BBBB    *~
            *  C   C  U   U    I    NN  N  P   P  S      U   U  B   B   *~
            *  C      U   U    I    N N N  PPPP    SSS   U   U  BBBB    *~
            *  C   C  U   U    I    N  NN  P          S  U   U  B   B   *~
            *   CCC    UUU   IIIII  N   N  P       SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CUINPSUB - Handles Variable Fields screens for files      *~
            *            defined in SYSFILE2 'VF'.                      *~
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
            *04/10/2019! ORIGINAL, copied from vfinpsub           ! DES *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


            close ws

        sub "CUINPSUB"   (file$,         /* File containing VFs        */~
                          mode$,         /* Input, Edit, Display       */~
                          line1$,        /* Screen Title Line          */~
                          line2$,        /* Screen Line 2              */~
                          pf45$,         /* Display PF-4 &/or PF-5?    */~
                          vfields$,      /* Variable fields string     */~
                          keyhit%  )     /*  1- Start Over             */
                                         /*  4- Previous Screen        */
                                         /*  5- Next Screen            */
                                         /* 16- Exit / Save Data       */
                                         /* 99- Error                  */

*        FILE$ is the name of the file where the variable fields reside.
*
*        MODE$ is either "I", "E", or "D"
*
*        LINE1$ is 64 characters and is the Screen title line less date.
*        LINE2$ is 60 characters and is the second screen line less the
*               program name and revision.
*
*        PF45$ is 2 characters, for PF-4 and 5 respectively. 'Y' if
*              prompt should appear.
*
*        VFIELDS$ contains the variable fields string in increments
*                 of 20 AND MATCHING THE NUMBER OF FIELDS DEFINED BY
*                 VFDEFINE.  No responsibility is taken if the length
*                 of this sucker is passed incorrectly.
*

        dim                                                              ~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descrs$(10)30,               /* GENCODES Description       */~
            descrs2$(10)30,               /* GENCODES Description       */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            file$8,                      /* File Name w/ VFs           */~
            gencodes$(10)9,              /* GENCODES Edit files        */~
            gencodes2$(10)9,              /* GENCODES Edit files        */~
            hdr$(2)79,                   /* Screen Headers             */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line1$64, line2$60,          /* Screen Hdr arguments       */~
            key$20, key2$50, key3$50,    /* SYSFILE2 key, misc key     */~
            mode$1, vx$2,                /* Input, Edit or Display     */~
            msg$(10)40,                  /* Info Messages              */~
            msg2$(10)40,                 /* Info Messages              */~
            pf4$18, pf5$18, pf16$16,     /* PF Key prompts             */~
            pf45$2,                      /* PF-4&5 prompt flags        */~
            prompt$(10)20,               /* Field Prompts              */~
            prompt2$(10)20,              /* Field Prompts              */~
            reqd$(10)1,                  /* Field Required? (R/O)      */~
            reqd2$(10)1,                 /* Field Required? (R/O)      */~
            size%(10),                   /* Field sizes                */~
            size2%(10),                  /* Field sizes                */~
            type$(10)2,                  /* Field Types                */~
            type2$(10)2,                 /* Field Types                */~
            vf$(10)20,                   /* Variable Fields            */~
	    /* VF$(5 & 6) have the cut-off/delivery data for C, D & E */ ~
            vfields$200                  /* Variable Fields argument   */

        dim f2%(02),                     /* = 0 if OPEN was successful */~
            f1%(02)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *  1  ! SYSFILE2 ! System File                              *~
            *  2  ! GENCODES ! General Codes File                       *~
            *************************************************************~

            select #1, "SYSFILE2"                                        ~
                       varc, indexed, recsize = 500,                     ~
                       keypos = 1, keylen = 20

            select #2, "GENCODES"                                        ~
                       varc, indexed, recsize = 128,                     ~
                       keypos = 1, keylen = 24

            call "OPENCHCK" (#1, 0%, f2%(1%), 0%, " ")
            call "OPENCHCK" (#2, 0%, f2%(2%), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            close ws

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value and Press RETURN."

*
*        Set up screen variables
            hdr$(1%) = line1$ : str(hdr$(1%),65%) = "Today: " & date$
            hdr$(2%) = line2$ : str(hdr$(2%),62%) = "CUINPSUB: " &       ~
                                                          str(cms2v$,,8%)

*
*        Get Variable Field Parameters from SYSFILE2
            key$ = "VF1:" & file$
            keyhit% = 99%
            call "READ100" (#1, key$, f1%(1%))
            if f1%(1%) = 0% then exit_abort
            get #1 using L09240, fields%, type$(), size%(), prompt$(),     ~
                               reqd$(), gencodes$()
L09240:         FMT XX(50), BI(1), 10*CH(2), 10*BI(1), 10*CH(20),        ~
                    10*CH(1), 10*CH(9)
            init (" ") msg$()
            str(key$,3%,1%) = "2"
            call "READ100" (#1, key$, f1%(1%))
            if f1%(1%) = 0% then L09350
            get #1 using L09310, msg$()
L09310:         FMT XX(20), 10*CH(40)


            str(key$,3%,1%) = "3"
            call "READ100" (#1, key$, f1%(1%))
            if f1%(1%) = 0% then exit_abort
            get #1 using L09240, fields2%, type2$(), size2%(), prompt2$(),     ~
                               reqd2$(), gencodes2$()

            str(key$,3%,1%) = "4"
            call "READ100" (#1, key$, f1%(1%))
            if f1%(1%) = 0% then L09350
            get #1 using L09310, msg2$()


            for f% = fields2%+1% to 10% : prompti2$(f%) = " " : next f%

            if fields% = 10% then L09350
                for f% = fields%+1% to 10% : prompt$(f%) = " " : next f%

L09350
            fieldsx% = fields% + fields2%

*        Move the variable fields passed into the array; format dates
            str(vf$()) = vfields$
            valid_fields% = 0%
            for f% = 1% to fields%
                if type$(f%) = "D" then call "DATEFMT" (vf$(f%))
                if type$(f%) <> "NU" then valid_fields% = 1%
            next f%
	    /* @@@ VF1/VF2 record must have data to use VF3 & VF4 */
            on valid_fields% + 1% goto exit_no_fields



*        Describe any GENCODES validated fields.
            descrs$() = " "
            descrs2$() = " "
            for f% = 1% to fields%
                if gencodes$(f%) = " " then L09490
		     gosub get_key2
                     call "READ100" (#2, key2$, f1%(2%))
                     if f1%(2%) = 1% then get #2 using L09480, descrs$(f%)
L09480:                   FMT POS(25), CH(30)
L09490:     next f%
            for f% = 1% to fields2%
                if gencodes2$(f%) = " " then L09590
		     gosub get_key3
                     call "READ100" (#2, key3$, f1%(2%))
                     if f1%(2%) = 1% then get #2 using L09480, descrs2$(f%)
L09590:     next f%

*        Now branch to the appropriate routine per Mode Requested
            keyhit%  = 99%
            if mode$ = "I" then inputmode
            if mode$ = "E" then edit_display_mode
            if mode$ = "D" then edit_display_mode
            goto exit_ok  /* Not really ok, but enough ok */

get_key2:  /* for VF1 & VF2 records */
         key2$ = str(gencodes$(f%),,9%) & vf$(f%)
         if f% < 5% or f% > 6% then goto not_mult_key 
         key2$ = str(gencodes$(f%),,9%) & str(vf$(f%),1%,2%)

not_mult_key:
         return

get_key3:  /* for VF3 & VF4 records */
         o1% = f% / 2%  /* check for odd numbers */
         o2% = o1% * 2%
         odd% = 1%
	 sub% = 5%  /* for odd #'s (1, 3, 5) */
         if o2% = f% then sub% = 6%  /* even #'s 2, 4, 6 */

         pos% = 3% + (((f% - 1%) / 2%) * 2%) 
         key3$ = str(gencodes2$(f%),,9%) & str(vf$(sub%),pos%,2%)
	 /* data is embedded in vf$(5) & vf$(6) */

         return

put_vx1: /* for VF1/2 where f% in (5, 6)          */ 
         str(vf$(f%),1%,2%) = vx$
         return

put_vx2: /* make sure get_key3 has been run first */ 
         str(vf$(sub%),pos%,2%) = vx$
         return

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Handles Input Mode for variable fields.                   *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, pf4$, pf5$, pf16$
            pf4$ = "(4)Previous Field"
            fieldsx% = fields% + fields2%
            for fieldnr% = 1% to fieldsx%
                gosub'051(fieldnr%)                /* Set INPMESSAGE$  */
                      if enabled% =  0% then L10180
L10130:         gosub'101(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  4% then                             ~
                                        fieldnr% = max(1%, fieldnr% - 1%)
                      if keyhit% <>  0% then       L10130
L10180:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
            next fieldnr%

            keyhit% = 0%
            goto exit_ok

        REM *************************************************************~
            *        E D I T   &  D I S P L A Y   M O D E               *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT and DISPLAY Modes.              *~
            *************************************************************
        edit_display_mode
            init(" ") pf4$, pf5$
            if str(pf45$,1%,1%) = "Y" then pf4$  = "(4)Previous Screen"
            if str(pf45$,2%,1%) = "Y" then pf5$  = "(5)Next Screen    "
            if mode$ = "D" then pf16$ = "(16)Return"  else               ~
                                pf16$ = "(16)Save Data"

L11120:     inpmessage$ = edtmessage$
            gosub'111(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% and str(pf45$,1,1) = "Y" then exit_ok
                  if keyhit%  =  5% and str(pf45$,2,1) = "Y" then exit_ok
                  if keyhit%  = 16% then exit_ok
                  if keyhit% <>  0% or mode$ = "D" then L11120
L11190:     fieldnr% = cursor%(1%) - 3% 
            if fieldnr% < 1% or fieldnr% > fieldsx% then L11120

            gosub'051(fieldnr%)
                  if enabled% = 0% then L11120
L11240:     gosub'111(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11240
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11240
                     if fieldnr% <> cursor%(1%) - 3% then L11190
            goto L11120

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for the Page 1 of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = msg$(fieldnr%)
            enabled% = 1%
	    if fieldnr% > 6% and fieldnr% < 12% then goto check_34 
	    fieldnx% = fieldnr%
	    if fieldnx% > 6% then fieldnx% = fieldnx% - 6%
            if mode$ = "I" and reqd$(fieldnx%) = "O" then enabled% = 0%
            if type$(fieldnx%) = "NU"                then enabled% = 0%
            return

check_34:
	    fieldnx% = fieldnr% - 6%
            if mode$ = "I" and reqd2$(fieldnx%) = "O" then enabled% = 0%
            if type2$(fieldnx%) = "NU"                then enabled% = 0%
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

L29920:     keyhit1% = 2%  /* PUT MSG AREA AT BOTTOM OF SCREEN  */
            close ws
            call "ZSTARTOV" (keyhit1%)
            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29920

                keyhit% = 1%
                return clear all
                goto exit_ok


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            * --------------------------------------------------------- *~
            * Inputs Document for the first time.                       *~
            *************************************************************

            deffn'101(fieldnr%)          /* Input Mode                 */
                init(hex(8c)) lfac$()
                init(hex(9c)) str(lfac$(), fieldsx%+1%)
                goto L40180


            deffn'111(fieldnr%)          /* Edit and Display Mode      */
                init(hex(8c)) lfac$()
                if fieldnr% = 0% then init(hex(86)) lfac$()
                init(hex(9c)) str(lfac$(), fieldsx%+1%)
                if mode$ = "D" then L40250

L40180  /* VF1_2 records */
*        Set Modifiable FACs and drop into ACCEPT statement.
            if fieldnr% = 0% then L40250
            if fieldnr% > 6% and fieldnr% < 13% then goto VF3_4
            fieldy% = fieldnr%
            if fieldnr% > 12% then fieldy% = fieldnr% - 6%
            if type$(fieldy%) = "AU" then lfac$(fieldnr%) = hex(81)
            if type$(fieldy%) = "AL" then lfac$(fieldnr%) = hex(80)
            if str(type$(fieldy%),2%,1%) = "+"                      ~
                                      then lfac$(fieldnr%) = hex(82)
            if str(type$(fieldy%),2%,1%) = "-"                      ~
                                      then lfac$(fieldnr%) = hex(82)
            if type$(fieldy%) = "D " then lfac$(fieldnr%) = hex(80)
            goto L40250

VF3_4:
            fieldy% = fieldnr% - 6%
            if type2$(fieldy%) = "AU" then lfac$(fieldnr%) = hex(81)
            if type2$(fieldy%) = "AL" then lfac$(fieldnr%) = hex(80)
            if str(type2$(fieldy%),2%,1%) = "+"                      ~
                                      then lfac$(fieldnr%) = hex(82)
            if str(type2$(fieldy%),2%,1%) = "-"                      ~
                                      then lfac$(fieldnr%) = hex(82)



L40250:   accept                                                         ~
            at (01,02), fac(hex(8c)), hdr$(1%)                  , ch(79),~
            at (02,02), fac(hex(ac)), hdr$(2%)                  , ch(79),~
            at (03,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (04,02), fac(hex(8c)), prompt$( 1%)              , ch(20),~
            at (05,02), fac(hex(8c)), prompt$( 2%)              , ch(20),~
            at (06,02), fac(hex(8c)), prompt$( 3%)              , ch(20),~
            at (07,02), fac(hex(8c)), prompt$( 4%)              , ch(20),~
            at (08,02), fac(hex(8c)), prompt$( 5%)              , ch(20),~
            at (09,02), fac(hex(8c)), prompt$( 6%)              , ch(20),~
            at (10,02), fac(hex(8c)), prompt2$(1%)              , ch(20),~
            at (11,02), fac(hex(8c)), prompt2$(2%)              , ch(20),~
            at (12,02), fac(hex(8c)), prompt2$(3%)              , ch(20),~
            at (13,02), fac(hex(8c)), prompt2$(4%)              , ch(20),~
            at (14,02), fac(hex(8c)), prompt2$(5%)              , ch(20),~
            at (15,02), fac(hex(8c)), prompt2$(6%)              , ch(20),~
            at (16,02), fac(hex(8c)), prompt$( 7%)              , ch(20),~
            at (17,02), fac(hex(8c)), prompt$( 8%)              , ch(20),~
            at (18,02), fac(hex(8c)), prompt$( 9%)              , ch(20),~
            at (19,02), fac(hex(8c)), prompt$(10%)              , ch(20),~
                                                                         ~
            at (04,25), fac(lfac$( 1%)), str(vf$( 1%),,size%( 1%))      ,~
            at (05,25), fac(lfac$( 2%)), str(vf$( 2%),,size%( 2%))      ,~
            at (06,25), fac(lfac$( 3%)), str(vf$( 3%),,size%( 3%))      ,~
            at (07,25), fac(lfac$( 4%)), str(vf$( 4%),,size%( 4%))      ,~
            at (08,25), fac(lfac$( 5%)), str(vf$( 5%),1%,2%)      ,~
            at (09,25), fac(lfac$( 6%)), str(vf$( 6%),1%,2%)      ,~
            at (10,25), fac(lfac$( 7%)), str(vf$( 5%),3%,2%)      ,~
            at (11,25), fac(lfac$( 8%)), str(vf$( 6%),3%,2%)      ,~
            at (12,25), fac(lfac$( 9%)), str(vf$( 5%),5%,2%)      ,~
            at (13,25), fac(lfac$(10%)), str(vf$( 6%),5%,2%)      ,~
            at (14,25), fac(lfac$(11%)), str(vf$( 5%),7%,2%)      ,~
            at (15,25), fac(lfac$(12%)), str(vf$( 6%),7%,2%)      ,~
            at (16,25), fac(lfac$(13%)), str(vf$( 7%),,size%( 7%))      ,~
            at (17,25), fac(lfac$(14%)), str(vf$( 8%),,size%( 8%))      ,~
            at (18,25), fac(lfac$(15%)), str(vf$( 9%),,size%( 9%))      ,~
            at (19,25), fac(lfac$(16%)), str(vf$(10%),,size%(10%))      ,~
                                                                         ~
            at (04,50), fac(hex(8c)), descrs$( 1%)              , ch(30),~
            at (05,50), fac(hex(8c)), descrs$( 2%)              , ch(30),~
            at (06,50), fac(hex(8c)), descrs$( 3%)              , ch(30),~
            at (07,50), fac(hex(8c)), descrs$( 4%)              , ch(30),~
            at (08,50), fac(hex(8c)), descrs$( 5%)              , ch(30),~
            at (09,50), fac(hex(8c)), descrs$( 6%)              , ch(30),~
            at (10,50), fac(hex(8c)), descrs2$(1%)              , ch(30),~
            at (11,50), fac(hex(8c)), descrs2$(2%)              , ch(30),~
            at (12,50), fac(hex(8c)), descrs2$(3%)              , ch(30),~
            at (13,50), fac(hex(8c)), descrs2$(4%)              , ch(30),~
            at (14,50), fac(hex(8c)), descrs2$(5%)              , ch(30),~
            at (15,50), fac(hex(8c)), descrs2$(6%)              , ch(30),~
            at (16,50), fac(hex(8c)), descrs$( 7%)              , ch(30),~
            at (17,50), fac(hex(8c)), descrs$( 8%)              , ch(30),~
            at (18,50), fac(hex(8c)), descrs$( 9%)              , ch(30),~
            at (19,50), fac(hex(8c)), descrs$(10%)              , ch(30),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), "(1)Start Over",                                 ~
            at (23,20), fac(hex(8c)), pf4$,                              ~
            at (24,20), fac(hex(8c)), pf5$,                              ~
            at (22,65), "(13)Instructions",                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), fac(hex(8c)), pf16$,                             ~
                                                                         ~
                keys(hex(000104050d0f10)),                               ~
                key (keyhit%)

                if keyhit% <> 13 then L40780
                     close ws
                     call "ZMANUAL" ("CUINPSUB")
                     goto L40250

L40780:         if keyhit% <> 15 then L40820
                     close ws
                     call "ZPRNTSCR"
                     goto L40250

L40820:         close ws
                call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1.                        *~
            *************************************************************

        deffn'151(fieldnr%)
            f2%       = fieldnr%
            f%        = fieldnr%
	    if f% > 6% and f% < 13% then goto VF3_4_fields
	    if f% > 4% and f% < 7% then goto VF1_2_fields
	    if f% > 12% then f% = f% - 6%
            errormsg$ = " "

*        First test for Required or Optional field
            if reqd$(f%) <> "R" or vf$(f%) <> " " then L50200
                errormsg$ = "This field must be entered."
                return

L50200
*        Now branch to the appropriate test per field data type
            if str(type$(f%),,2%) = "NU" then L50260    /* Not used      */
            if str(type$(f%),,1%) = "A" then gencodes  /* Alpha         */
            if str(type$(f%),,1%) = "N" then L50300     /* Numeric       */
            if str(type$(f%),,1%) >= "0" and str(type$(f%),,1%) <= "9"   ~
                                        then L50300     /* Numeric       */
            if str(type$(f%),,1%) = "D" then L50500     /* Date          */
            return

L50260
*        Test for Blank Field   (ie 'NU' not used)
            vf$(f%) = " " : return

L50300
*        Test NUMERIC data field
            if vf$(f%) = " " then vf$(f%) = "0"
            convert vf$(f%) to n, data goto L50325 : goto L50330
L50325:         errormsg$ = "Invalid numeric entry."  : return
L50330:     if n >= 0 or str(type$(f%),2%,1%) = "-" then L50360
                errormsg$ = "Can only enter positive numbers."
                return
L50360:     format = 2.2
            if str(type$(f%),1%,1%) = "N" then L50368
                convert str(type$(f%),1%,1%) to decimals, data goto L50368
                format = decimals + (decimals/10)
L50368:     call "CONVERT" (n, format, str(vf$(f%),,12%))
            goto gencodes

L50500
*        Test DATE data field
            if vf$(f%) = " " then gencodes
            call "DATEOK" (vf$(f%), u3%, errormsg$)
            if errormsg$ > " " then return

        gencodes /* Test against GENCODES file for validity            */
            if gencodes$(f%) = " " then return
            if reqd$(f%) <> "R" and vf$(f%) = " " then return
                gosub get_key2                                  
                close ws
                call "ZPLOWCOD" (#2, key2$, descrs$(f%), 9%, .30, f1%(2%))
                if f1%(2%) = 1% then vf$(f%) = str(key2$,10%)  else      ~
                     errormsg$ = "Code not on file (GENCODES = " &       ~
                                                            gencodes$(f%)
                return

VF1_2_fields: /* for sub 5 & 6, like VF3_4 */
            errormsg$ = " "
            vx$ = str(vf$(f%),1%,2%)
*        First test for Required or Optional field
            if reqd$(f%) <> "R" or vx$ <> " " then L51200
                errormsg$ = "This field must be entered."
                return

L51200 
*        Now branch to the appropriate test per field data type
            if str(type$(f%),,2%) = "NU" then L51260    /* Not used      */
            if str(type$(f%),,1%) = "A" then gencodes1 /* Alpha         */
            return

L51260
*        Test for Blank Field   (ie 'NU' not used)
            vx$ = " " 
	    gosub put_vx1
	    return

gencodes1 /* Test against GENCODES file for validity            */
            if gencodes$(f%) = " " then return
            if reqd$(f%) <> "R" and vx$ = " " then return
	        gosub get_key2
                close ws
                call "ZPLOWCOD" (#2, key2$, descrs$(f%), 9%, .30, f1%(2%))
                if f1%(2%) = 1% then vx$ = str(key2$,10%,2%)  else      ~
                     errormsg$ = "Code not on file (GENCODES = " &       ~
                                                      gencodes$(f%)

	    gosub put_vx1
            return

VF3_4_fields:
            f%        = fieldnr% - 6%
	    if f% > 6% then return
            errormsg$ = " "
	    vx$ = "  "
            gosub get_key3
            vx$ = str(vf$(sub%),pos%,2%)
*        First test for Required or Optional field
            if reqd2$(f%) <> "R" or vx$ <> " " then L52200
                errormsg$ = "This field must be entered."
                return

L52200 /* VF3/4 only alpha or not used */
*        Now branch to the appropriate test per field data type
            if str(type2$(f%),,2%) = "NU" then L52260    /* Not used      */
            if str(type2$(f%),,1%) = "A" then gencodes2 /* Alpha         */
            return

L52260
*        Test for Blank Field   (ie 'NU' not used)
            vx$ = " " 
	    gosub put_vx1
	    return

gencodes2 /* Test against GENCODES file for validity            */
            if gencodes2$(f%) = " " then return
            if reqd2$(f%) <> "R" and vx$ = " " then return
                close ws
                call "ZPLOWCOD" (#2, key3$, descrs2$(f%), 9%, .30, f1%(2%))
                if f1%(2%) = 1% then vx$ = str(key3$,10%,2%)  else      ~
                     errormsg$ = "Code not on file (GENCODES = " &       ~
                                                      gencodes2$(f%)

	    gosub put_vx2
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN
        exit_ok
*        Unformat dates and replace callers variable fields argument
            for f% = 1% to fields%
		if f% = 5% or f% = 6% then goto skip_date
                if type$(f%) = "D" then call "DATUNFMT" (vf$(f%))
		skip_date:
            next f%
            vfields$ = str(vf$(),,20%*fields%)
        exit_abort
            close ws
            end
        exit_no_fields
            keyhit% = 0%
            end

