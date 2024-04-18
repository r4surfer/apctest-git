        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    OOO   M   M   AAA   PPPP   PPPP   RRRR   V   V   *~
            *  B   B  O   O  MM MM  A   A  P   P  P   P  R   R  V   V   *~
            *  BBBB   O   O  M M M  AAAAA  PPPP   PPPP   RRRR   V   V   *~
            *  B   B  O   O  M   M  A   A  P      P      R   R   V V    *~
            *  BBBB    OOO   M   M  A   A  P      P      R   R    V     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMAPPRV - Allows approval of existing BOMs.              *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/06/88 ! Original                                 ! TLJ *~
            * 12/27/89 ! Merged in Option Auto-Replace code       ! MJB *~
            * 07/31/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            apprby$15,                   /* Approved By                */~
            appron$8,                    /* Approved On                */~
            approval$6,                  /* Approval flag              */~
            apprmethod$1,                /* Approval Method            */~
            assymsg$79,                  /* Assembly Mess. for Display */~
            assypart$25,                 /* Assembly Part No.          */~
            assypartdescr$32,            /* Assembly part description  */~
            assydescr$30,                /* Assembly part description  */~
            assybom$3,                   /* Assembly BOM               */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bomid$3,                     /* BOM ID.                    */~
            comppart$25,                 /* Component Part             */~
            compbom$3,                   /* Component BOM ID           */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dsplyon$20,                  /* Display for entered on     */~
            dsplyby$20,                  /* Display for entered by     */~
            edtmessage$79,               /* Edit screen message        */~
            effective$(490)3,            /* BOM EFFECTIVE DATES        */~
            enteron$8,                   /* System date of approval    */~
            enterby$3,                   /* User that modified approval*/~
            errormsg$79,                 /* Error message              */~
            found%(1),                   /* Found flag for search      */~
            hdrmsg$79,                   /* Header message for ASKUSER */~
            hdr$(3)79,                   /* Header for PLOWCODE        */~
            i$(24)80,                    /* Screen Image               */~
            incl(1),                     /* For PLOWCODE               */~
            incl$(1)1,                   /* Variable for PLOWCODE      */~
            initon$6,                    /* Init. approved on date     */~
            initby$15,                   /* Init. approved by value    */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            opappron$6,                  /* Option Approval Flag       */~
            opflag$1,                    /* Option flag                */~
            oppart$25,                   /* Option Part                */~
            optype$3,                    /* Option Part Type           */~
            opbom$3,                     /* Option Parts BOM           */~
            p$(5000)31,                  /* Stack for prime 7          */~
            parappron$6,                 /* Parents approval flag      */~
            parent$25,                   /* Parent of BOM              */~
            parbomid$3,                  /* Bom ID of PARENT           */~
            part$(20)25,                 /* Parts Approved             */~
            partbom$(20)3,               /* Parts Approved BOM         */~
            partdescr$(20)30,            /* Parts Approved Description */~
            pnum$(20)4,                  /* Parts number for display   */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey2$99,                 /* Miscellaneous Read/Plow Key*/~
            readkey3$99,                 /* Miscellaneous Read/Plow Key*/~
            readparent$99,               /* Miscellaneous Read/Plow Key*/~
            repmsg$20,                   /* Replacement Message        */~
            userid$3                     /* Current User Id            */

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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * # 1 ! BOMMASTR ! BOM relationship file                    *~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 3 ! HNYMASTR ! Inventory Master File                    *~
            * # 4 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            * # 5 ! ENGMASTR ! Engineering Master Filer                 *~
            * # 6 ! STCBOMXF ! Standard Cost Set / BOM-RTE X-Ref        *~
            * # 7 ! HNYOPTNS ! Option List                              *~
            * # 9 ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56          ~

            select # 2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 3, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            select # 4, "JBCROSS2",                                      ~
                        varc,     indexed,  recsize =   94,              ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47          ~

            select # 5, "ENGMASTR",                                      ~
                        varc,     indexed,  recsize = 2015,              ~
                        keypos =    1, keylen =  29                      ~

            select # 6, "STCBOMXF",                                      ~
                        varc, indexed, recsize = 72,                     ~
                        keypos = 29, keylen = 33,                        ~
                        alt key 1, keypos =  1, keylen = 36,             ~
                            key 2, keypos = 37, keylen = 36

            select # 7, "HNYOPTNS",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 1, keylen = 54

            select # 9, "WORKFILE", consec,  recsize = 75

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, 0%, f2%( 3),   0%, " ")
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "READ100" (#2, "SWITCHS.BOM", f1%(2))
            if f1%(2) = 0% then apprflag$ = "N"                          ~
            else get #2 using L09098 , apprflag$, apprmethod$
L09098:          FMT XX(20), CH(1), CH(1)
            gosub get_method_descr
            if apprflag$ = "Y" then L09230
L09130:     call "ASKUSER" (0%, "APPROVAL FLAG NOT SET",                 ~
                             "To approve BOMS the APPROVAL FLAG must "  &~
                             "be set to yes.",                           ~
                             "To set the APPROVAL FLAG run BOMFLAGS.",   ~
                             "Press any key to EXIT.")
            goto L65000

L09230:     edtmessage$  = "Place cursor at field to modify and Press " &~
                           "(RETURN); Press PF32 to EXIT program."

            str(line2$,62) = "BOMAPPRV: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            gosub get_method
            gosub get_method_descr

            for fieldnr% = 1% to  4%
L10110:         gosub'051(fieldnr%, 1%)    /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%, 1%)
                         gosub'151(fieldnr%, 2%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%, 1%)  /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            gosub get_method
            gosub get_method_descr
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then       unapprove
                  if keyhit%  = 16% then       apprv_boms
                  if keyhit%  = 32% then       exit_program
                  if keyhit% <>  0% then       editpg1
L11160:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% then fieldnr% = 1%
            if fieldnr% >  4% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%, 2%)     /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11220:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11220
            gosub'151(fieldnr%, 2%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11220
                  lastfieldnr% = fieldnr%
            goto L11160

        REM *************************************************************~
            *                   A P P R O V E   B O M S                 *~
            *-----------------------------------------------------------*~
            * Driver for approving the BOMs top down.                   *~
            *************************************************************
        apprv_boms:
            call "DATUNFMT"(appron$)
            call "DATUNFMT"(enteron$)
            /* has there been a change to the input screen? */
            if initon$ = appron$ and initby$ = apprby$ then L12320
                call "SHOSTAT" ("APPROVING " & assypart$ & ", " & bomid$)
                if initon$ <> " " and initon$ <> blankdate$ then L12150  /****/
                     enteron$ = date
                     enterby$ = userid$
                     goto L12210
L12150:         /* write to the header the modified data */
                readkey$ = str(assypart$,,25) & str(bomid$,,3) & "  0"
                call "READ101" (#1, readkey$, f1%(1))
                put #1 using L12190, enteron$, enterby$
L12190:              FMT POS(136), CH(6), CH(3)
                rewrite #1
L12210:     readkey$ = str(assypart$,,25) & str(bomid$,,3)
            to_apprv% = 0%
            l% = 0%
            call "WORKOPEN" (#9, "OUTPUT", 500%, f2%(8))
            gosub prime7

        apprvls_done:
            gosub L12290          /* needed for RETURN CLEAR ALL */
L12290:     return clear all
            if errormsg$ = " " then gosub use_approval_method
            call "FILEBGON" (#9)
L12320:     goto inputmode

        REM *************************************************************~
            *            U S E   A P P R O V A L   M E T H O D          *~
            *-----------------------------------------------------------*~
            * Use the selected approval method. (selected in BOMFLAGS)  *~
            *************************************************************
        use_approval_method:
            if apprmethod$ = "A" or to_apprv% > 1% then L12430
                readkey$ = str(assypart$,,25) & str(bomid$,,3) & "  0"
                gosub headerput
L12430:     if to_apprv% <= 1% then return
                startrec% = 1%
                keyhit% = 0%
        cont_paging:
            if keyhit%  =  1% then gosub startover
            if keyhit%  =  2% then startrec% = 1%
            if keyhit% <>  3% then L12510
                if mod(to_apprv%, 13%) <> 0% then L12496
                     startrec% = to_apprv% - 12%
                     goto L12510
L12496:         if mod(to_apprv%,13%) = 1% then startrec% = to_apprv%    ~
                     else startrec% = to_apprv% / 13% * 13% + 1%

L12510:     if keyhit%  <>  4% then L12550
                if to_apprv% <= 13% then startrec% = 1%                  ~
                     else startrec% = startrec% - 13%
                if startrec% < 1% then startrec% = 1%
L12550:     if keyhit%  <>  5% then L12590
                if startrec% + 13% > to_apprv%                           ~
                    then startrec% = to_apprv% / 13% * 13% + 1%          ~
                    else startrec% = startrec% + 13%
L12590:     if keyhit%  <> 10% then L12660
                apprmethod$ = "A"
                call "FILEBGON" (#9)
                call "DATEFMT"(appron$)
                call "DATEFMT"(enteron$)
                gosub apprv_boms
                return
L12660:     if keyhit%  = 16% then return
                gosub get_page
                gosub'201         /*Display Screen - No Entry*/
                goto cont_paging

        REM *************************************************************~
            *                     G E T   P A G E                       *~
            *-----------------------------------------------------------*~
            * Load a page full of BOMs (to be approved) for display.    *~
            *************************************************************
        get_page:
            part$(), partbom$(), partdescr$(), pnum$() = " "
            close #9
            call "WORKOPN2" (#9, "INPUT", 500%, f2%(9))
            read #9, record = startrec%, eod goto L12835
            for i% = 1% to 13%
                read #9, using L12805, part$(i%), partbom$(i%),           ~
                                      partdescr$(i%), eod goto L12835
L12805:             FMT CH(31), XX(9), CH(3), CH(30)
                convert i% + startrec% - 1% to pnum$(i%), pic(####)
                str(pnum$(i%),1,3) = str(pnum$(i%),2,3)
                str(pnum$(i%),4,1) = ")"
            next i%

L12835:     if to_apprv% > 13% then L12855
                part$(to_apprv%) = "** END **"
                return

L12855:     if startrec% + 12% > to_apprv% then                          ~
                part$(mod(to_apprv%,13%)) = "** END **"
            if startrec% + 12% = to_apprv% then part$(13%) = "** END **"
            if startrec% = to_apprv% then part$(1%) = "** END **"
            return

        REM *************************************************************~
            *                     G E T   M E T H O D                   *~
            *-----------------------------------------------------------*~
            * Obtain the approval method.                               *~
            *************************************************************
        get_method:
            call "READ100" (#2, "SWITCHS.BOM", f1%(2))
            if f1%(2) = 0% then L09130 /* error reading SYSFILE2 */
             get #2 using L12990, apprmethod$
L12990:             FMT POS(22), CH(1)
         return

        REM *************************************************************~
            *          G E T   M E T H O D   D E S C R I P T I O N      *~
            *-----------------------------------------------------------*~
            * Obtain the approval method description.                   *~
            *************************************************************
         get_method_descr:
            if apprmethod$ = "A" then line2$ =                           ~
        "METHOD: Automatically approve components."
            if apprmethod$ = "P" then line2$ =                           ~
        "METHOD: Prompt for automatic approval of components."
            if apprmethod$ = "D" then line2$ =                           ~
        "METHOD: Do not approve unless all components are approved."
            return

        REM *************************************************************~
            *                       P R I M E  7                        *~
            *-----------------------------------------------------------*~
            * Approves BOMS from the top down.                          *~
            *************************************************************
        prime7:
            l% = l% + 1%
            p$(l%) = readkey$
L13200:     call "PLOWNEXT" (#1, p$(l%), 28%, f1%(1))
            if f1%(1) = 0% then done
            if str(p$(l%), 29, 3) <> "  0" then L13340
            get #1 using L13240, assybom$, assydescr$
L13240:         FMT    POS(51), CH(3), POS(57), CH(30)
*        Proceess header
                  search str(p$(),,(l%-1)*31) = str(p$(l%),,28) to       ~
                        found%() step 31
                  if found%(1) <> 0 then forever_loop
                  gosub load_approval
                  if approval$ <> " " then L13330
                     gosub save_prev
                     if apprmethod$ = "A" then gosub save_apprv
L13330:           goto L13200
L13340
*        Process component item
                  gosub load_component
                  if opflag$ = "Y" then gosub test_options
                  gosub write_type
                  if typeflag% = 0% then bom_not_pegged
                  if typeflag% = 1% then L13200
                  if typeflag% = 3% then generic_part
                  readkey$ = str(comppart$,,25) & str(compbom$,,3)
                  gosub prime7
                  goto L13200
            done:
                  l% = l% - 1%
                  return

        REM *************************************************************~
            *                S A V E   P R E V I O U S                  *~
            *-----------------------------------------------------------*~
            * Saves previous approval information for unapproving if    *~
            * when approving top down a part is not a purchased item    *~
            * or hard pegged.                                           *~
            *************************************************************
        save_prev
            write #9 using L13570, p$(l%), enteron$, enterby$, assybom$,  ~
                                                               assydescr$
L13570:           FMT             CH(31), CH(6), CH(3), CH(3), CH(30)
            to_apprv% = to_apprv% + 1%
            return

        REM *************************************************************~
            *                S A V E   A P P R O V A L                  *~
            *-----------------------------------------------------------*~
            * Saves approval data on BOMMASTR.                          *~
            *************************************************************
        save_apprv
            readkey$ = p$(l%)
            gosub headerput
            return

        REM *************************************************************~
            *                 L O A D   A P P R O V A L                 *~
            *-----------------------------------------------------------*~
            * Load approval date to be used as approval flag.           *~
            *************************************************************
        load_approval:
            get #1% using L19170, approval$
L19170:         FMT    POS(115), CH(6)
            return

        REM *************************************************************~
            *                L O A D   C O M P O N E N T                *~
            *-----------------------------------------------------------*~
            * Load component information for approvals.                 *~
            *************************************************************
        load_component:
            get #1% using L19270, comppart$, opflag$, compbom$
L19270:         FMT              CH(25), POS(91), CH(1), CH(3)
            return

        REM *************************************************************~
            *                  T E S T   O P T I O N S                  *~
            *-----------------------------------------------------------*~
            * Test option parts.  The replacement parts mus be approved.*~
            *************************************************************
        test_options:
            readkey2$=str(p$(l%),,25) & str(p$(l%),26,3)                 ~
                                             & str(comppart$,,) & hex(00)
L19330:     call "PLOWNEXT" (#7, readkey2$, 53%, f1%(7))
            if f1%(7) = 0% then return/*Replacement Components Approved*/
               get #7 using L19360, oppart$, optype$, opbom$
L19360:               FMT POS(55), CH(25), POS(81), CH(3), CH(3)
            if optype$ >= "000" and optype$ <= "999" then L19450
               assypart$ = str(p$(l%),,25) : bomid$ = str(p$(l%),26,3)
               hdrmsg$ = "ERROR-- APPROVAL cannot be completed. "
               pf$(1) = "The component " & comppart$ & " of the BOM "   &~
                      assypart$ & ", " & bomid$
               pf$(2) = "has replacement parts, without the PART TYPE " &~
                      "or SPECIFIC BOM on file."
               pf$(3) = "Press RETURN to START OVER."
               goto L19590
L19450:     if optype$ > "000" and optype$ < "500" then L19330
            readkey3$ = str(oppart$,,25) & str(opbom$,,3) & "  0"
            call "READ101" (#1, readkey3$, f1%(1))
                get #1% using L19510, opappron$
L19510:             FMT    POS(115), CH(6)
            if opappron$ = "Y" then return
            assypart$ = str(p$(l%),,25) : bomid$ = str(p$(l%),26,3)
            hdrmsg$ = "ERROR-- APPROVAL cannot be completed. "
            pf$(1) = "The component " & comppart$ & " of the BOM "      &~
                      assypart$ & ", " & bomid$
            pf$(2) = "has replacement parts NOT approved."
            pf$(3) = "Press RETURN to START OVER."
L19590:     if apprmethod$ = "A" then gosub undo_apprvls
            call "ASKUSER" (2%, hdrmsg$, pf$(1), pf$(2), pf$(3))
            errormsg$ = "ERROR"
            goto apprvls_done

        REM *************************************************************~
            *                   W R I T E   T Y P E                     *~
            *-----------------------------------------------------------*~
            * Writes the part type out to the component record.         *~
            * And sets the the TYPEFLAG% to                             *~
            * 0- a manufactured item without a BOM ID                   *~
            * 1- a purchased item                                       *~
            * 2- a manufacutured item with a BOM ID (hard pegged)       *~
            *************************************************************
        write_type:
            call "READ100" (#3, comppart$, f1%(3))
            get #3 using L19750, parttype$
L19750:         FMT POS(180), CH(3)
            readkey$ = str(comppart$,,25) & p$(l%)
            call "REDALT1" (#1, readkey$, 1%, f1%(1))
            put #1 using L19790, parttype$
L19790:          FMT POS(103), CH(3)
            rewrite #1
            convert parttype$ to parttype%
            if parttype% <> 0% then L19820
                typeflag% = 3%
                return                                 /* Generic      */
L19820:     if parttype% < 500% and parttype% > 0% then L19860
                if compbom$ = " " then typeflag% = 0%  /* Manufactured */~
                else typeflag% = 2%
                return
L19860:     typeflag% = 1%                             /* Purchased    */
            return

        REM *************************************************************~
            *                U N D O   A P P R O V A L S                *~
            *-----------------------------------------------------------*~
            * Writes the previous values of enteron and enterby to the  *~
            * header records of BOMMASTR.  This is called when a part in*~
            * the bom is neither a purchased item or hard pegged.  Also *~
            * if PRIME7 will end up in an infinite loop.                *~
            *************************************************************
        undo_apprvls:
            init (" ") appron$, apprby$
            close #9
            call "WORKOPN2" (#9, "INPUT", 500%, f2%(8))
            continue_unapprving
                call "READNEXT" (#9, f1%(9))
                if f1%(9) = 0% then L20090
                    get #9 using L20050, readkey$, enteron$, enterby$
L20050:                 FMT CH(31), CH(6), CH(3)
                    gosub headerput
                    gosub clear_types
                    goto continue_unapprving
L20090:     return

        clear_types:
            call "PLOWNXT1" (#1, readkey$, 28%, f1%(1))
            if f1%(1) = 0% then return
              put #1 using L20150, " "
L20150:           FMT   POS(103), CH(3)
            rewrite #1
            goto clear_types

        REM *************************************************************~
            *              B O M   N O T   P E G G E D                  *~
            *-----------------------------------------------------------*~
            * If trying to approve a BOM not hard pegged display error  *~
            * message and undo any upper level approvals due to the     *~
            * present approval request.                                 *~
            *************************************************************
        bom_not_pegged:
            assypart$ = str(p$(l%),,25) : bomid$ = str(p$(l%),26,3)
            hdrmsg$ = "ERROR-- APPROVAL cannot be completed. "
            pf$(1) = "The component " & comppart$ & " of the Part "  &   ~
                      assypart$ & ", BOM ID " & bomid$
            pf$(2) = "is not hard pegged."
            pf$(3) = "Press RETURN to START OVER."
            if apprmethod$ = "A" then gosub undo_apprvls
            call "ASKUSER" (2%, hdrmsg$, pf$(1), pf$(2), pf$(3))
            errormsg$ = "ERROR"
            goto  apprvls_done

        REM *************************************************************~
            *              G E N E R I C   P A R T                      *~
            *-----------------------------------------------------------*~
            * If trying to approve a Generic Part display error message *~
            * and undo upper level approvals.                           *~
            *************************************************************
        generic_part:
            assypart$ = str(p$(l%),,25) : bomid$ = str(p$(l%),26,3)
            hdrmsg$ = "ERROR-- APPROVAL cannot be completed. "
            pf$(1) = "The component " & comppart$ & " of the Part "     &~
                      assypart$ & ", BOM ID " & bomid$
            pf$(2) = "is a Generic Part."
            pf$(3) = "Press RETURN to START OVER."
            if apprmethod$ = "A" then gosub undo_apprvls
            call "ASKUSER" (2%, hdrmsg$, pf$(1), pf$(2), pf$(3))
            errormsg$ = "ERROR"
            goto  apprvls_done

        REM *************************************************************~
            *                F O R E V E R   L O O P                    *~
            *-----------------------------------------------------------*~
            * If trying to approve a BOM existing at a level below      *~
            * itself display an error message and undo any upper level  *~
            * approvals due to the present approval request.            *~
            *************************************************************
        forever_loop:
            assypart$ = str(p$(l%),,25) : bomid$ = str(p$(l%),26,3)
            hdrmsg$ = " -- ERROR -- "
            pf$(1) = "Approving this BOM will cause an infinite loop."
            pf$(2) = "Part " & assypart$ & ", BOM ID " & bomid$         &~
                                     " exists at a level below itself."
            pf$(3) = "Press RETURN to START OVER."
            if apprmethod$ = "A" then gosub undo_apprvls
            call "ASKUSER" (2%, hdrmsg$, pf$(1), pf$(2), pf$(3))
            errormsg$ = "ERROR"
            goto apprvls_done

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%, edit%)
            enabled% = 1%
            on fieldnr% gosub L25140,         /* Assembly Part No.      */~
                              L25180,         /* BOM ID.                */~
                              L25220,         /* Approved On            */~
                              L25260          /* Approved By            */
            return

L25140: REM Def/Enable Assembly Part No.          ASSYPART$
            if edit% = 2% then enabled% = 0%
            return

L25180: REM Def/Enable BOM ID.                    BOMID$
            if edit% = 2% then enabled% = 0%
            return

L25220: REM Def/Enable Approved On                APPRON$
            if appron$ = " " or appron$ = blankdate$ then appron$ = date$
            return

L25260: REM Def/Enable Approved By                APPRBY$
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L27110
                inpmessage$ = edtmessage$
                return

L27110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter the ASSEMBLY PART number to be approved.               ",~
         "Enter the BOM ID of the Assembly Part to be approved.        ",~
         "Enter the DATE approved on.                                  ",~
         "Enter the NAME the approval was made by.                     "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, enterby$, enteron$,        ~
                      apprby$, appron$, assypart$, bomid$,               ~
                      assypartdescr$, dsplyon$, dsplyby$
            l% = 0%
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            call "FILEBGON" (#9)
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload:
            readkey$ = str(assypart$,,25) & str(bomid$,,3) & "  0"
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then L30150
                get #1% using L30100, appron$, apprby$, enteron$, enterby$
L30100:             FMT POS(115), CH(6), CH(15), CH(6), CH(3)
            initon$ = appron$
            initby$ = apprby$
            call "DATEFMT" (appron$ )
            call "DATEFMT" (enteron$)
L30150:     return


        REM *************************************************************~
            *               U N A P P R O V E   B O M                   *~
            *-----------------------------------------------------------*~
            *            Unapproves a previously approved BOM.          *~
            *************************************************************
        unapprove:
            gosub check_usage
            if errormsg$ = " " then L30290
                  errormsg$ = "Unable to UNAPPROVE BOM, " & errormsg$
                  goto editpg1

L30290:     u3% = 2%
            call "ASKUSER" (u3%, "UNAPPROVING BOM",                      ~
                             "To UNAPPROVE this BOM press RETURN.",      ~
                             "- OR -",                                   ~
                             "Press PF1 to Continue with EDITTING.")
            if u3% = 1% then editpg1
            if u3% <> 0% then L30290
            appron$ = all(" ")
            apprby$ = all(" ")
            if (initon$ = " " or initon$ <> blankdate$) and ~
                                 initby$ = " " then L30430
                enteron$ = date
                enterby$ = userid$
            readkey$ = str(assypart$,,25) & str(bomid$,,3) & "  0"
            gosub headerput
            gosub clear_types
L30430:     goto inputmode

        REM *************************************************************~
            *                   C H E C K   U S A G E                   *~
            *-----------------------------------------------------------*~
            *               IS IT OK TO CHANGE THIS BILL?               *~
            *************************************************************

        check_usage:
            errormsg$ = " "

*        Check if Parent is approved
            call "SHOSTAT" ("Checking if Parent BOM is Approved")
            readkey$ = str(assypart$,,25) & hex(00)
            next_parent
                call "PLOWALTS" (#1%, readkey$, 1%, 25%, f1%(1))
                if f1%(1) = 0 then L30730
                    get #1 using L30610, parent$, parbomid$,      chkbomid$
L30610:                 FMT    POS(26), CH(25),  CH(3), POS(92), CH(3)
                    if chkbomid$ <> bomid$ then next_parent
                    readparent$ = str(parent$,,25) & str(parbomid$,,3) & ~
                                                     "  0"
                    call "READ100" (#1%, readparent$, f1%(1))
                    get #1% using L30670, parappron$
L30670:                 FMT    POS(115), CH(6)
                    if parappron$ = " " then next_parent
                    errormsg$ = "Parent " & parent$ & ", BOM "& parbomid$~
                                          & " is APPROVED"
                    return

L30730
*        First See if the Bill is used in a Job (via JBCROSS2).
            call "SHOSTAT" ("Checking for use in Planning")
            readkey$ = str(assypart$,,25) & str(bomid$,,3)
            call "PLOWALTS" (#4, readkey$, 2%, 28%, used%)
            if used% = 0% then L30820
                errormsg$ = "BOM is used for Planning"  : return
                                     /*****/
*        Next See if Bill has been set effective (via ENGMASTR)
            call "SHOSTAT" ("Checking if BOM has been set effective.")
L30820:     readkey$ = str(assypart$,,25) & "1001"
            call "READ100" (#5, str(readkey$,,29), used%)
            if used% = 0% then L30920
                errormsg$ = "BOM has an Effectivity Date"
                get #5, using L30870, effective$()
L30870:              FMT XX(29), 490*CH(3)
                search effective$() = str(bomid$,,3) to cursor%() step 3
                if cursor%(1) > 0% then return
                     errormsg$ = " " : used% = 0%

L30920
*        And last see if Bill referenced by Standard Costing (STCBOMXF)
            call "SHOSTAT"("Checking if referenced by Standard Costing.")
            readkey$ = str(assypart$,,25) & str(bomid$,,3) & hex(00)
            call "PLOWALTS" (#6, readkey$, 1%, 28%, used%)
            if used% = 0% then L31000
                errormsg$ = "Used by Standard Cost Set " &               ~
                                                       str(readkey$,29,8)
                return
L31000
*        One more test, if a replacement part is it for an approved BOM
                call "SHOSTAT" ("Checking if BOM is a replacement part " ~
                                  & "for an APPROVED BOM.")
                repmsg$ = "a REPLACEMENT"
                readkey$ = hex(00)
                next_option:
                   call "PLOWNEXT" (#7, readkey$, 0%, f1%(7))
                   if f1%(7) = 0% then return
                   get #7 using L31070, oppart$, opbom$
L31070:                   FMT POS(55), CH(25), POS(81), CH(3)
                   if oppart$ <> comppart$ then next_option
                   gosub test_parent
                   if errormsg$ <> " " then return
                   goto next_option

        test_parent:
          /*Test if Assembly with replacement list is approved*/
            readparent$=str(oppart$,,25) & str(opbom$,,3) & "  0"
            call "READ100" (#1%, readparent$, f1%(1))
            get #1% using L31300, parappron$
L31300:         FMT POS(115), CH(6)
            if parappron$ = " " then return
                errormsg$ = "Part is " & repmsg$ & " for approved "      ~
                           & "BOM: " & oppart$ &  ", BOM " & opbom$ & "."
                return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        headerput:
            call "READ101" (#1, readkey$, f1%(1))
            put #1 using L35090, appron$, apprby$, enteron$, enterby$
            rewrite #1
L35090:         FMT   POS(115), CH(6),   CH(15),  CH(6),    CH(3)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()

              if edit% = 2% then let lfac$(1), lfac$(2) = hex(8c)


              on fieldnr% gosub L40220,         /* Assembly Part No. */   ~
                                L40220,         /* BOM ID.           */   ~
                                L40210,         /* Approved On       */   ~
                                L40210          /* Approved By       */
              goto L40250

L40210:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40220:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40250:     accept                                                       ~
               at (01,02),                                               ~
                  "MANAGE BILL OF MATERIAL STRUCTURE APPROVALS",         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Assembly Part No.",                          ~
               at (06,22), fac(lfac$( 1)), assypart$            , ch(25),~
               at (06,48), fac(hex(8c)),   assypartdescr$       , ch(32),~
                                                                         ~
               at (07,02), "BOM ID",                                     ~
               at (07,22), fac(lfac$( 2)), bomid$               , ch(03),~
                                                                         ~
               at (08,02), "Approved On",                                ~
               at (08,22), fac(lfac$( 3)), appron$              , ch(08),~
               at (08,48), fac(hex(8c)),   dsplyon$             , ch(20),~
               at (08,69), fac(hex(8c)),   enteron$             , ch(08),~
                                                                         ~
               at (09,02), "Approved By",                                ~
               at (09,22), fac(lfac$( 4)), apprby$              , ch(15),~
               at (09,48), fac(hex(8c)),   dsplyby$             , ch(20),~
               at (09,69), fac(hex(8c)),   enterby$             , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40600
                  call "MANUAL" ("BOMAPPRV") : goto L40250

L40600:        if keyhit% <> 15 then L40630
                  call "PRNTSCRN" : goto L40250

L40630:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
           str(line2$,62) = "BOMAPPRV: " & cms2v$
        if edit% = 2% then L40830     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40790
                str(pf$(3),61)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40790:     if fieldnr% > 1% then L40810
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40810:     return

L40830: if fieldnr% > 0% then L40950  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over          (8)Unapprove BOM " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                    (16/32)Approve/Exit"
            pfkeys$ = hex(01ffffffffffff08ffffffff0dff0f102000)
            call "DATUNFMT" (appron$)
            if initon$ <> " " and ~
               initon$ <> blankdate$ and ~
               initon$ = appron$ and ~
               initby$ = apprby$ then str(pf$(3),61,) = "(16)Exit"
            call "DATEFMT" (appron$)
            if initon$ <> " " and initon$ <> blankdate$ then L40940
              str(pf$(1),23,17) = " " : str(pfkeys$,08,1) = hex(ff)
L40940:     return
L40950:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        deffn'201
            inpmessage$ = " "
            gosub set_pf2
L41080:     accept                                                       ~
               at (01,02),                                               ~
                  "MANAGE BILL OF MATERIAL STRUCTURE APPROVALS",         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(8c)), assymsg$               , ch(79),~
               at (06,08), fac(hex(ac)), str(hdrmsg$,1,8)       , ch(25),~
               at (06,37), fac(hex(ac)), str(hdrmsg$,26,3)      , ch(03),~
               at (06,44), fac(hex(ac)), str(hdrmsg$,30,25)     , ch(30),~
                                                                         ~
               at (07,02), fac(hex(8c)),   pnum$(1)             , ch(04),~
               at (07,08), fac(hex(8c)),   part$(1)             , ch(25),~
               at (07,37), fac(hex(8c)),   partbom$(1)          , ch(03),~
               at (07,44), fac(hex(8c)),   partdescr$(1)        , ch(30),~
                                                                         ~
               at (08,02), fac(hex(8c)),   pnum$(2)             , ch(04),~
               at (08,08), fac(hex(8c)),   part$(2)             , ch(25),~
               at (08,37), fac(hex(8c)),   partbom$(2)          , ch(03),~
               at (08,44), fac(hex(8c)),   partdescr$(2)        , ch(30),~
                                                                         ~
               at (09,02), fac(hex(8c)),   pnum$(3)             , ch(04),~
               at (09,08), fac(hex(8c)),   part$(3)             , ch(25),~
               at (09,37), fac(hex(8c)),   partbom$(3)          , ch(03),~
               at (09,44), fac(hex(8c)),   partdescr$(3)        , ch(30),~
                                                                         ~
               at (10,02), fac(hex(8c)),   pnum$(4)             , ch(04),~
               at (10,08), fac(hex(8c)),   part$(4)             , ch(25),~
               at (10,37), fac(hex(8c)),   partbom$(4)          , ch(03),~
               at (10,44), fac(hex(8c)),   partdescr$(4)        , ch(30),~
                                                                         ~
               at (11,02), fac(hex(8c)),   pnum$(5)             , ch(04),~
               at (11,08), fac(hex(8c)),   part$(5)             , ch(25),~
               at (11,37), fac(hex(8c)),   partbom$(5)          , ch(03),~
               at (11,44), fac(hex(8c)),   partdescr$(5)        , ch(30),~
                                                                         ~
               at (12,02), fac(hex(8c)),   pnum$(6)             , ch(04),~
               at (12,08), fac(hex(8c)),   part$(6)             , ch(25),~
               at (12,37), fac(hex(8c)),   partbom$(6)          , ch(03),~
               at (12,44), fac(hex(8c)),   partdescr$(6)        , ch(30),~
                                                                         ~
               at (13,02), fac(hex(8c)),   pnum$(7)             , ch(04),~
               at (13,08), fac(hex(8c)),   part$(7)             , ch(25),~
               at (13,37), fac(hex(8c)),   partbom$(7)          , ch(03),~
               at (13,44), fac(hex(8c)),   partdescr$(7)        , ch(30),~
                                                                         ~
               at (14,02), fac(hex(8c)),   pnum$(8)             , ch(04),~
               at (14,08), fac(hex(8c)),   part$(8)             , ch(25),~
               at (14,37), fac(hex(8c)),   partbom$(8)          , ch(03),~
               at (14,44), fac(hex(8c)),   partdescr$(8)        , ch(30),~
                                                                         ~
               at (15,02), fac(hex(8c)),   pnum$(9)             , ch(04),~
               at (15,08), fac(hex(8c)),   part$(9)             , ch(25),~
               at (15,37), fac(hex(8c)),   partbom$(9)          , ch(03),~
               at (15,44), fac(hex(8c)),   partdescr$(9)        , ch(30),~
                                                                         ~
               at (16,02), fac(hex(8c)),   pnum$(10)            , ch(04),~
               at (16,08), fac(hex(8c)),   part$(10)            , ch(25),~
               at (16,37), fac(hex(8c)),   partbom$(10)         , ch(03),~
               at (16,44), fac(hex(8c)),   partdescr$(10)       , ch(30),~
                                                                         ~
               at (17,02), fac(hex(8c)),   pnum$(11)            , ch(04),~
               at (17,08), fac(hex(8c)),   part$(11)            , ch(25),~
               at (17,37), fac(hex(8c)),   partbom$(11)         , ch(03),~
               at (17,44), fac(hex(8c)),   partdescr$(11)       , ch(30),~
                                                                         ~
               at (18,02), fac(hex(8c)),   pnum$(12)            , ch(04),~
               at (18,08), fac(hex(8c)),   part$(12)            , ch(25),~
               at (18,37), fac(hex(8c)),   partbom$(12)         , ch(03),~
               at (18,44), fac(hex(8c)),   partdescr$(12)       , ch(30),~
                                                                         ~
               at (19,02), fac(hex(8c)),   pnum$(13)            , ch(04),~
               at (19,08), fac(hex(8c)),   part$(13)            , ch(25),~
               at (19,37), fac(hex(8c)),   partbom$(13)         , ch(03),~
               at (19,44), fac(hex(8c)),   partdescr$(13)       , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41810
                  call "MANUAL" ("BOMAPPRV") : goto L41080

L41810:        if keyhit% <> 15 then L41840
                  call "PRNTSCRN" : goto L41080

L41840:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return
                                                                         ~
        set_pf2:
            str(line2$,62) = "BOMAPPRV: " & cms2v$
            pf$(1) = "(1)Start Over                 (10)Approv" &        ~
                     "e BOMs                 (13)Instructions"
            pf$(2) = "(2)First       (4)Previous              " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Last        (5)Next                  " &        ~
                     "                       (16)Exit        "

            pfkeys$ = hex(0102030405ffffffff0affff0dff0f1000)

            if startrec% > 1% then L42030
                str(pf$(2),01,30) = " "
                str(pfkeys$,02,1)=hex(ff)
                str(pfkeys$,04,1)=hex(ff)
L42030:     if startrec% + 13% < to_apprv% then L42080
                str(pf$(3),01,30) = " "
                str(pfkeys$,03,1)=hex(ff)
                str(pfkeys$,05,1)=hex(ff)

L42080:     str(hdrmsg$,1,25)= "ASSEMBLY" : str(hdrmsg$,26,3)="BOM"
            str(hdrmsg$,30,30)= "DESCRIPTION"

            if apprmethod$ <> "A" then L42180
              str(pf$(1),30,17) = " "  :  str(pfkeys$,10,1) = hex(ff)
              inpmessage$ = "The Listed BOMs have been Automatically "  &~
                            "APPROVED.  Press PF(16) to Exit."
              assymsg$="Components of part "& assypart$& ", BOM " &bomid$
              goto L42280

L42180:       assymsg$="Unapproved components of part " & assypart$ &    ~
                                                         ", BOM " &bomid$

              if apprmethod$ = "P" then L42260
              inpmessage$="Unable to approve BOM until all components " &~
                           "are approved.  Press PF(16) to Exit."
                   str(pf$(1),30,17) = " " : str(pfkeys$,10,1)=hex(ff)
                   goto L42280
L42260:      inpmessage$ = "Press PF(10) to APPROVE the listed Bill of "&~
                           "Materials.  Press PF(16) to Exit."
L42280:  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%, edit%)
            errormsg$ = " "
            on fieldnr% gosub L50130,         /* Assembly Part No.      */~
                              L50350,         /* BOM ID.                */~
                              L50600,         /* Approved On            */~
                              L50660          /* Approved By            */
            return
L50130: REM Test for Assembly Part No.           ASSYPART$
            readkey$ = assypart$
            hdr$(2)="  Part Assemblies             Part Descriptions"
            hdr$(3) = hex(ac) & "Position cursor (TAB) to a line "       ~
                      &  "and press (RETURN) to choose that PART."
            assypartdescr$ = hex(06) & "   Select an Assembly Part."
            incl(1) = 0
            call "PLOWCODE" (#1, readkey$, assypartdescr$, -8025%,       ~
                             -.32, f1%(1), hdr$(), 3, 75,                ~
                               incl(), incl$(), "Y", "Y", #3%)
            if f1%(1) = 1% then L50260
                errormsg$ = "ERROR - Unknown Assembly Part."
                return
L50260:     assypart$ = readkey$
            assypartdescr$ = "(NOT ON FILE)"
            call "DESCRIBE" (#3%, assypart$, assypartdescr$, 1%, f1%(3))
            return

L50350: REM Test for BOM ID.                     BOMID$
            readkey$ = str(assypart$,,25) & str(bomid$,,3)
            hdr$()= " Listed Below Are The Existing BOMs "               ~
                                              & "For Part: " & assypart$
            errormsg$ = hex(06) & "Select a Bill Of Materials."
            call "PLOWCODE" (#1, readkey$, errormsg$, 2025%, .30, f1%(1),~
                                                               hdr$(), 3)
            errormsg$ = " "
                if f1%(1) <> 0% then L50450
                errormsg$ = "ERROR - BOM not on file" : return
L50450:     bomid$ = str(readkey$,26%,3%)
            if edit% = 2% then L50580

*          Load aprroval information from BOMMASTR
            gosub dataload
            if appron$ <> " " and appron$ <> blankdate$ then L50560
                  if enteron$ <> " " and enteron$ <> blankdate$ ~
                                         then L50530 /* never approved */
                     init(" ") dsplyon$, dsplyby$
                     goto L50552
L50530:           dsplyon$ = "Unapproved on:"        /* unapproved     */
                  dsplyby$ = "Unapproved by:"
L50552:           if f1%(3) = 0% then return
                    get #3, using L50554, type$
L50554:             FMT POS(180), CH(3)
                    if type$="000" then errormsg$="An Assembly with "&   ~
                             "Option Parts is not APPROVABLE.  "     &   ~
                             "Press PF(1) to STARTOVER."
                    return
L50560:     dsplyon$ = "Approval entered on:"        /* approved       */
            dsplyby$ = "Approval entered by:"
            goto editpg1
L50580:     return

L50600: REM Test for Approved On                 APPRON$
            if appron$ = " " or appron$ = blankdate$ then ~
               errormsg$ = "APPROVED ON date may not be BLANK"           ~
            else call "DATEOK" (appron$, u3%, errormsg$)
            return

L50660: REM Test for Approved By                 APPRBY$
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
