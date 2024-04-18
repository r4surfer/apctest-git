        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    OOO   M   M   AAA   U   U  TTTTT  RRRR   PPPP    *~
            *  B   B  O   O  MM MM  A   A  U   U    T    R   R  P   P   *~
            *  BBBB   O   O  M M M  AAAAA  U   U    T    RRRR   PPPP    *~
            *  B   B  O   O  M   M  A   A  U   U    T    R   R  P       *~
            *  BBBB    OOO   M   M  A   A   UUU     T    R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMAUTRP - A detail report for automatic replacements.    *~
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
            * 06/28/88 ! Original                                 ! TLJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            ass$25,                      /* Specific Assembly          */~
            ass1$25,                     /* Specific Assembly          */~
            assbom$4,                    /* Specific BOM ID            */~
            assbom1$4,                   /* Specific BOM ID            */~
            comp$25,                     /* Component/Option Part      */~
            comma$1,                     /* Comma for Report           */~
            company$60,                  /* Company Name for Report    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            opt$25,                      /* Option Part                */~
            objpar$25,                   /* Object Parent              */~
            objbom$3,                    /* Object BOM                 */~
            objcomp$25,                  /* Object Component/Replaced  */~
            objseq$4,                    /* Object Sequence            */~
            op$25,                       /* Option Part                */~
            op1$25,                      /* Option Part                */~
            par$25,                      /* Parent Part                */~
            parbom$3,                    /* Parent BOM                 */~
            ppar$25,                     /* Previos Parent             */~
            pparbom$3,                   /* Previous Parent BOM        */~
            pcomp$25,                    /* Previous Component         */~
            pseq$3,                      /* Previous Sequence of Comp. */~
            popt$25,                     /* Previous Option            */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prntrepl$29,                 /* Concatenate for Print      */~
            prntcomp$29,                 /* Concatenate for Print      */~
            prntpar$29,                  /* Concatenate for Print      */~
            replacement$25,              /* Replacement Part           */~
            replbom$3,                   /* Replacement BOM            */~
            qty$8,                       /* Quantity                   */~
            repl$25,                     /* Replacement Part           */~
            repl1$25,                    /* Replacement Part           */~
            replaced$25,                 /* Part Replaced              */~
            replaced1$25,                /* Part Replaced              */~
            size$8,                      /* Size                       */~
            sel$25,                      /* Selected Part              */~
            sel1$25,                     /* Selected Part              */~
            seq$3,                       /* Seq of Replacement Part    */~
            sort$1,                      /* Sort By                    */~
            sortdescr$20,                /* Sort By Description        */~
            to$(6)2,                     /* Used for Display           */~
            tass$25,                     /* Specific Assembly          */~
            tass1$25,                    /* Specific Assembly          */~
            tassbom$4,                   /* Specific Assembly BOM      */~
            tassbom1$4,                  /* Specific Assembly BOM      */~
            top$25,                      /* Option Part                */~
            top1$25,                     /* Option Part                */~
            trepl$25,                    /* Replacement Part           */~
            trepl1$25,                   /* Replacement Part           */~
            tsel$25,                     /* Selected Part              */~
            tsel1$25,                    /* Selected Part              */~
            userid$3                     /* Current User Id            */~

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
            cms2v$ = "R6.00.01 04/13/90 Patch Release w/ Auto Replace   "
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
            * #01 ! HNYOPTN2 ! Automatic Replacement File               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYOPTN2",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =   26, keylen =  58,                     ~
                        alt key  1, keypos =  112, keylen =  56, dup,    ~
                            key  2, keypos =    1, keylen =  83,         ~
                            key  3, keypos =  140, keylen =  25, dup,    ~
                            key  4, keypos =   84, keylen =  28, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%( 1), f2%( 1), 0%, rslt$( 1))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "BOMAUTRP: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  7%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10290
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 7% then L10280
                         for i% = fieldnr% to 7%
                            gosub'051(i%)
                            gosub'151(i%)
                            if errormsg$<>" " then L10120
                            next i%
                         goto editpg1
L10280:               if keyhit% <> 0% then       L10120
L10290:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       generate_report
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% then fieldnr% = 1%
            if fieldnr% > 7% then fieldnr% = 7%
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11120


        REM *************************************************************~
            *                 P R I N T   R E P O R T                   *~
            *-----------------------------------------------------------*~
            * Prints the Report.                                        *~
            *************************************************************

        generate_report:
            call "SHOSTAT" ("Generating Auto Replacement Report")
            runtime$ = " "
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            select printer(134)
            call "SETPRNT" ("BOM007", " ", 0%, 0%)
            something_printed% = 0%
            line% = 99%
            page% = 0%

            plowkey$ = hex(00)

            if sort$ = "1" then sort% = 0% else convert sort$ to sort%
            plowkey$ = all(hex(00))
            call "PLOWALTS" (#1, plowkey$, sort%, 0%, f1%(1))
            next_line:
              if f1%(1) = 0% then exit_report
              gosub datastore /* Save Previous */
              gosub dataload
              failed% = 0%
              gosub testdata
              if failed% = 0 then gosub print_line
              call "READNEXT" (#1, f1%(1))
              goto next_line


        print_line:
            if line% > 56% then gosub print_headings
            if ppar$ = par$ and pparbom$ = parbom$ and pcomp$ = comp$    ~
                                              and pseq$ = seq$ then L12430
              print
              print
              print using L19130, par$, parbom$
              print using L19140, comp$, seq$
              print
              print using L19160
              print using L19200
              line% = line% + 7%
L12430:     if replbom$ = " " then comma$ = " " else comma$ = ","
            prntrepl$ = replacement$ & comma$ & replbom$
            prntcomp$ = objcomp$ & "," & objseq$
            prntpar$ = objpar$ & "," & objbom$
            print using L19240,  opt$, prntrepl$, prntcomp$, prntpar$,    ~
                                qty$, size$
            line% = line% + 1%
            return

        print_headings:
            if page% = 0% then gosub print_page_zero
            something_printed% = 1%
            line% = 4%
            page% = page% + 1%
            if page% = 100% then page% = 1%
            convert page% to page$, pic(##)
            call "STRING" addr("RJ", page$, 2%)
            print page
            print using L19050, date$, company$
            print using L19090, runtime$, page$
            return

        print_page_zero:
            convert page% to page$, pic(##)
            call "STRING" addr("RJ", page$, 2%)
            print page
            print using L19050, date$, company$
            print using L19090, runtime$, page$
            print
            print using L19280
            print
            print using L19310, ass$, to$(1), ass1$
            print using L19330, assbom$, to$(2), assbom1$
            print using L19350, op$, to$(3), op1$
            print using L19370, sel$, to$(4), sel1$
            print using L19390, repl$, to$(5), repl1$
            print using L19410, replaced$, to$(6), replaced1$
            print using L19430, sort$, sortdescr$
            return

        testdata:
            if comp$ < top$ or comp$ > top1$ then failed
            if par$ < tass$ or par$ > tass1$ then failed
            if parbom$ < tassbom$ or parbom$ > tassbom1$ then failed
            if opt$ < tsel$ or opt$ > tsel1$ then failed
            if replacement$< trepl$ or replacement$ > trepl1$ then failed
            if objcomp$< treplaced$ or objcomp$ > treplaced1$ then failed
            return

        failed:
            failed% = 1%
            return

        datastore:
            pcomp$ = comp$
            ppar$ = par$
            pparbom$ = parbom$
            pseq$ = seq$
            popt$ = opt$
            return

        exit_report:
            if something_printed% = 1% then L13150
               u3% = 2%
               call "ASKUSER" (u3%, " ", "No report printed.",           ~
                                  "No RECORDS EXIST Meeting the "       &~
                                         "Specified Criteria.",          ~
                                  "Press PF(1) to STARTOVER or RETURN " &~
                                         "to EDIT criteria." )
                goto L13180
L13150:     print skip(58% - line%)
            print "END OF REPORT"
            u3% = 1%
L13180:     close printer
            call "SETPRNT" ("BOM007", " ", 0%, 1%)
            errormsg$ = " "
            if u3% = 1% then goto inputmode else goto editpg1

        REM *************************************************************~
            *                P R I N T   F O R M A T S                  *~
            *-----------------------------------------------------------*~
            * Formats and image statements for report output.           *~
            *************************************************************
L19050: %Date: ########                      ############################~
        ~################################                     BOMAUTRP-BOM~
        ~007

L19090: %Time: ########                                    BOM AUTO REPLA~
        ~CEMENT REPORT                                            PAGE:   ~
        ~ ##

L19130: %Specific Assembly: #########################   BOM: ###
L19140: %Option Part:       #########################   SEQ: ###

L19160: %Selection Of              Replacement Part (Part,Bom)   Componen~
        ~t Replaced (Part,Seq) In Assembly (Part,Bom)        Quantity    S~
        ~ize

L19200: %------------------------- ----------------------------- --------~
        ~--------------------- ----------------------------- -------- ----~
        ~---

L19240: %######################### ############################# ########~
        ~##################### ############################# ######## ####~
        ~###

L19280: %                                                        REPORT C~
        ~RITERIA

L19310: %Specific Assembly     ######################### ## #############~
        ~############
L19330: %Specific BOM          ######################### ## #############~
        ~############
L19350: %Option Part           ######################### ## #############~
        ~############
L19370: %Selected Part         ######################### ## #############~
        ~############
L19390: %Replacement Part      ######################### ## #############~
        ~############
L19410: %Component Replaced    ######################### ## #############~
        ~############
L19430: %Sort by               #      ####################

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20160,         /* Specific Assembly      */~
                              L20210,         /* Specific BOM ID        */~
                              L20300,         /* Option Part            */~
                              L20350,         /* Selected Part          */~
                              L20400,         /* Replacement Part       */~
                              L20450,         /* Part Replaced          */~
                              L20500          /* Sort By                */
            return
L20160
*        Def/Enable Specific Assembly           ASS$
            if ass$ = " " and ass1$ = " " then ass$ = "ALL"
            to$(1) = "TO"
            return

L20210
*        Def/Enable Specific BOM ID             ASSBOM$
            if assbom$ = " " and assbom1$ = " " then assbom$ = "ALL"
            to$(2) = "TO"
            return

L20300
*        Def/Enable Option Part                 OP$
            if op$ = " " and op1$ = " " then op$ = "ALL"
            to$(3) = "TO"
            return

L20350
*        Def/Enable Selected Part               SEL$
            if sel$ = " " and sel1$ = " " then sel$ = "ALL"
            to$(4) = "TO"
            return

L20400
*        Def/Enable Replacement Part            REPL$
            if repl$ = " " and repl1$ = " " then repl$ = "ALL"
            to$(5) = "TO"
            return

L20450
*        Def/Enable Part Replaced               REPLACED$
            if replaced$ = " " and replaced1$ = " " then replaced$ = "ALL"
            to$(6) = "TO"
            return

L20500
*        Def/Enable Sort By                     SORT$
            if sort$ = " " then sort$ = "1"
            gosub get_sort_descr
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
            if fieldnr% <> 2% then L28130
              if ass1$ <> " " or ass$ = "ALL" or ass$ = "FIRST" or       ~
                                                 ass$ = "LAST" then L28130
              inpmessage$ = "Enter the Specific BOM ID.  Use '?' to view"~
                                                               & " list."
              goto L28140
L28130:     read inpmessage$      /* Read Input Message */
L28140:     return

        scrn1_msg  :  data                                               ~
         "Enter the Specific Assembly Range.  Use '?' to view list.    ",~
         "Enter the Specific BOM ID.                                   ",~
         "Enter the Option Part.  Use '?' to view list.                ",~
         "Enter the Part to be Selected from the Option List.          ",~
         "Enter the Replacement Part.  Use '?' to view list.           ",~
         "Enter the Part to be Automatically Replaced.  Use '?' to view l~
        ~ist. ",                                                          ~
         "(1)Assembly, (2)Option Part, (3)Replacement, (4)Replaced Part"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, to$(),                     ~
                      ass$, assbom$, op$, repl$, replaced$, sel$, sort$, ~
                      ass1$, assbom1$, op1$, repl1$, replaced1$, sel1$,  ~
                      pcomp$, ppar$, pparbom$, pseq$, popt$, comp$, par$,~
                      parbom$, seq$, opt$, sortdescr$
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
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #1 using L30100, comp$, par$, parbom$, seq$, opt$,        ~
                                replacement$, replbom$, objpar$, objbom$,~
                                objcomp$, objseq$, qty, sze

L30100:        FMT              CH(25), CH(25), CH(3), CH(3), CH(25),    ~
                                POS(84), CH(25), CH(3), CH(25), CH(3),   ~
                                CH(25), CH(3), POS(168), PD(14,4),       ~
                                PD(14,4)

               convert qty to qty$, pic(########)
               convert sze to size$, pic(#######)
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
              on fieldnr% gosub L40210,         /* Specific Assembly */   ~
                                L40210,         /* Specific BOM ID   */   ~
                                L40210,         /* Option Part       */   ~
                                L40210,         /* Selected Part     */   ~
                                L40210,         /* Replacement Part  */   ~
                                L40210,         /* Part Replaced     */   ~
                                L40220          /* Sort By           */
              goto L40240

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40210:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40220:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40240:     accept                                                       ~
               at (01,02),                                               ~
                  "BOM Automatic Replacement Report",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Specific Assembly",                          ~
               at (06,27), fac(lfac$( 1)), ass$                 , ch(25),~
               at (06,53), fac(hex(8c)), to$(1)                 , ch(02),~
               at (06,56), fac(lfac$( 1)), ass1$                , ch(25),~
                                                                         ~
               at (07,02), "Specific BOM ID",                            ~
               at (07,27), fac(lfac$( 2)), assbom$              , ch(04),~
               at (07,32), fac(hex(8c)), to$(2)                 , ch(02),~
               at (07,35), fac(lfac$( 2)), assbom1$             , ch(04),~
                                                                         ~
               at (08,02), "Option Part",                                ~
               at (08,27), fac(lfac$( 3)), op$                  , ch(25),~
               at (08,53), fac(hex(8c)), to$(3)                 , ch(02),~
               at (08,56), fac(lfac$( 3)), op1$                 , ch(25),~
                                                                         ~
               at (09,02), "Selected Part",                              ~
               at (09,27), fac(lfac$( 4)), sel$                 , ch(25),~
               at (09,53), fac(hex(8c)), to$(4)                 , ch(02),~
               at (09,56), fac(lfac$( 4)), sel1$                , ch(25),~
                                                                         ~
               at (10,02), "Replacement Part",                           ~
               at (10,27), fac(lfac$( 5)), repl$                , ch(25),~
               at (10,53), fac(hex(8c)), to$(5)                 , ch(02),~
               at (10,56), fac(lfac$( 5)), repl1$               , ch(25),~
                                                                         ~
               at (11,02), "Part Replaced",                              ~
               at (11,27), fac(lfac$( 6)), replaced$            , ch(25),~
               at (11,53), fac(hex(8c)), to$(6)                 , ch(02),~
               at (11,56), fac(lfac$( 6)), replaced1$           , ch(25),~
                                                                         ~
               at (12,02), "Sort By",                                    ~
               at (12,27), fac(lfac$( 7)), sort$                , ch(01),~
               at (12,34), fac(hex(8c)), sortdescr$             , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40760
                  call "MANUAL" ("BOMAUTRP") : goto L40240

L40760:        if keyhit% <> 15 then L40790
                  call "PRNTSCRN" : goto L40240

L40790:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40980     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                      (13)Instructions"
            pf$(2) = "                 (4)Previous Field     (" &        ~
                     "7)Proceed to Edit     (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                      (16)Exit Program"
            pfkeys$ = hex(01ffff04ffff07ffffffffff0dff0f1000)
            if fieldnr% = 1% then L40940
                str(pf$(3),63)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40940:     if fieldnr% > 1% then L40960
                str(pf$(2),18,18) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40960:     return

L40980: if fieldnr% > 0% then L41070  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                      (13)Instructions "
            pf$(2) = "                                        " &        ~
                     "                      (15)Print Screen "
            pf$(3) = "                                        " &        ~
                     "                      (16)Create Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L41070:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                      (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                      (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50170,         /* Specific Assembly      */~
                              L50460,         /* Specific BOM ID        */~
                              L50820,         /* Option Part            */~
                              L51110,         /* Selected Part          */~
                              L51170,         /* Replacement Part       */~
                              L51460,         /* Part Replaced          */~
                              L51790          /* Sort By                */
            return

L50170
*        Test for Specific Assembly            ASS$
            plowkey$ = " "
            if str(ass$,1,1) <> "?" then L50230
               call "PLOWCODE" (#1, plowkey$,descr$, -25%, -.001, f1%(1))
               if f1%(1) = 1% then ass$ = str(plowkey$,,25)
               plowkey$ = " "
L50230:     if str(ass1$,1,1) <> "?" then L50260
               call "PLOWCODE" (#1, plowkey$,descr$, -25%, -.001, f1%(1))
               if f1%(1) = 1% then ass1$ = str(plowkey$,,25)
L50260:     if str(ass$,,1) <> "?" and str(ass1$,,1) <> "?" then L50290
               errormsg$ = "Enter ASSEMBLY RANGE"
               return
L50290:     if ass$ = " " and ass1$ <> " " then ass$ = "FIRST"
            call "TESTRNGE" (ass$, ass1$, tass$, tass1$, errormsg$)
            if errormsg$ <> " " then L50440
            if ass$ <> ass1$ then L50430
               ass1$ = " "
               plowkey$ = str(ass$,,25)
               call "PLOWCODE" (#1, plowkey$,descr$, -25%, -.001, f1%(1))
               if f1%(1) = 1% then L50400
                  errormsg$ = "An Automatic Replacement does not exist "&~
                                                     "for this Assembly."
                  goto L50440
L50400:        ass$ = str(plowkey$,1,25)
               call "TESTRNGE" (ass$, ass1$, tass$, tass1$, errormsg$)
               ass1$ = " "
L50430:     if ass1$ = " " then to$(1) = " "
L50440:     return

L50460
*        Test for Specific BOM ID              ASSBOM$
            if ass1$ <> " " or ass$ = "ALL" or ass$ = "FIRST"            ~
                                             or ass$ = "LAST$" then L50760
            errormsg$ = hex(06) & "Select a Bill Of Materials."
            plowkey$ = str(ass$,,25) & hex(00)
            if str(assbom$,1,1) <> "?" then L50560
               call "PLOWCODE" (#1, plowkey$, errormsg$, 2025%, 0.0,     ~
                                                       f1%(1), hdr$(), 3)
               if f1%(1) = 1% then assbom$ = str(plowkey$,26,3)
               plowkey$ = str(ass$,,25) & hex(00)
L50560:     if str(ass1$,1,1) <> "?" then L50600
               call "PLOWCODE" (#1, plowkey$, errormsg$, 2025%, 0.0,     ~
                                                       f1%(1), hdr$(), 3)
               if f1%(1) = 1% then assbom1$ = str(plowkey$,26,3)
L50600:     if str(assbom$,,1)<>"?" and str(assbom1$,,1)<>"?" then L50630
               errormsg$ = "Enter BOM ID RANGE"
               return
L50630:     if assbom$ = " " and assbom1$ <> " " then assbom$ = "FIRST"
            call "TESTRNGE"(assbom$,assbom1$,tassbom$,tassbom1$,errormsg$)
            if errormsg$ <> " " then L50800
            if assbom$ <> assbom1$ then L50790
               assbom1$ = " "
               plowkey$ = str(ass$,,25) & str(assbom$,,3)
               call "PLOWCODE" (#1, plowkey$, errormsg$, 2025%, 0.0,     ~
                                                       f1%(1), hdr$(), 3)
               if f1%(1) = 1% then L50750
                  errormsg$ = "An Automatic Replacement does not exist "&~
                                                     "for this Assembly."
                  goto L50800
L50750:        assbom$ = str(plowkey$,26,3)
L50760:        call "TESTRNGE" (assbom$, assbom1$, tassbom$, tassbom1$,  ~
                                                               errormsg$)
               assbom1$ = " "
L50790:     if assbom1$ = " " then to$(2) = " "
L50800:     return

L50820
*        Test for Option Part                  OP$
            plowkey$ = " "
            if str(op$,1,1) <> "?" then L50880
               call "PLOWCODE" (#1, plowkey$, descr$, -25%, -2.0, f1%(1))
               if f1%(1) = 1% then op$ = str(plowkey$,,25)
               plowkey$ = " "
L50880:     if str(op1$,1,1) <> "?" then L50910
               call "PLOWCODE" (#1, plowkey$, descr$, -25%, -2.0, f1%(1))
               if f1%(1) = 1% then op1$ = str(plowkey$,,25)
L50910:     if str(op$,,1) <> "?" and str(op1$,,1) <> "?" then L50940
               errormsg$ = "Enter Option Part RANGE"
               return
L50940:     if op$ = " " and op1$ <> " " then op$ = "FIRST"
            call "TESTRNGE" (op$, op1$, top$, top1$, errormsg$)
            if errormsg$ <> " " then L51090
            if op$ <> op1$ then L51080
               op1$ = " "
               plowkey$ = str(op$,,25)
               call "PLOWCODE" (#1, plowkey$, descr$, -25%, -2.0, f1%(1))
               if f1%(1) = 1% then L51050
                  errormsg$ = "The Part specified is not used as an "   &~
                                 "Option Part for Automatic Replacement."
                  goto L51090
L51050:        op$ = str(plowkey$,1,25)
               call "TESTRNGE" (op$, op1$, top$, top1$, errormsg$)
               op1$ = " "
L51080:     if op1$ = " " then to$(3) = " "
L51090:     return

L51110
*        Test for Selected Part                SEL$
            call "TESTRNGE" (sel$, sel1$, tsel$, tsel1$, errormsg$)
            if sel$ = sel1$ then sel1$ = " "
            if errormsg$ = " " and sel1$ = " " then to$(4) = " "
            return

L51170
*        Test for Replacement Part             REPL$
            plowkey$ = " "
            if str(repl$,1,1) <> "?" then L51230
               call "PLOWCODE" (#1, plowkey$, descr$, -28%, -4.0, f1%(1))
               if f1%(1) = 1% then repl$ = str(plowkey$,,25)
               plowkey$ = " "
L51230:     if str(repl1$,1,1) <> "?" then L51260
               call "PLOWCODE" (#1, plowkey$, descr$, -28%, -4.0, f1%(1))
               if f1%(1) = 1% then repl1$ = str(plowkey$,,25)
L51260:     if str(repl$,,1) <> "?" and str(repl1$,,1) <> "?" then L51290
               errormsg$ = "Enter the Replacement Part Range."
               return
L51290:     if repl$ = " " and repl1$ <> " " then repl$ = "FIRST"
            call "TESTRNGE" (repl$, repl1$, trepl$, trepl1$, errormsg$)
            if errormsg$ <> " " then L51440
            if repl$ <> repl1$ then L51430
               repl1$ = " "
               plowkey$ = str(repl$,,25)
               call "PLOWCODE" (#1, plowkey$,descr$, -28%, -4.0, f1%(1))
               if f1%(1) = 1% then L51400
                  errormsg$ = "The part specified is not used for "     &~
                                                 "Automatic Replacement."
                  goto L51440
L51400:        repl$ = str(plowkey$,1,25)
               call "TESTRNGE" (repl$, repl1$, trepl$, trepl1$, errormsg$)
               repl1$ = " "
L51430:     if repl1$ = " " then to$(5) = " "
L51440:     return

L51460
*        Test for Part Replaced                REPLACED$
            plowkey$ = " "
            if str(replaced$,1,1) <> "?" then L51520
               call "PLOWCODE" (#1, plowkey$,descr$, -25%, -3.0, f1%(1))
               if f1%(1) = 1% then replaced$ = str(plowkey$,,25)
               plowkey$ = " "
L51520:     if str(replaced1$,1,1) <> "?" then L51550
               call "PLOWCODE" (#1, plowkey$, descr$, -25%, -3.0, f1%(1))
               if f1%(1) = 1% then replaced1$ = str(plowkey$,,25)
L51550:     if str(replaced$,,1)<>"?" and                                ~
                                       str(replaced1$,,1)<>"?" then L51590
               errormsg$ = "Enter RANGE for Replaced Parts."
               return
L51590:     if replaced$ = " " and replaced1$ <> " "                     ~
                                                then replaced$ = "FIRST"
            call "TESTRNGE" (replaced$, replaced1$, treplaced$,          ~
                                                  treplaced1$, errormsg$)
            if errormsg$ <> " " then L51770
            if replaced$ <> replaced1$ then L51760
               replaced1$ = " "
               plowkey$ = str(replaced$,,25)
               call "PLOWCODE" (#1, plowkey$,descr$, -25%, -3.0, f1%(1))
               if f1%(1) = 1% then L51720
                  errormsg$ = "An Automatic Replacement does not exist "&~
                                                     "for this Assembly."
                  goto L51770
L51720:        replaced$ = str(plowkey$,1,25)
               call "TESTRNGE" (replaced$, replaced1$, treplaced$,       ~
                                                  treplaced1$, errormsg$)
               replaced1$ = " "
L51760:     if replaced1$ = " " then to$(6) = " "
L51770:     return

L51790
*        Test for Sort By                      SORT$
            if sort$ < "1" or sort$ > "4" then errormsg$ =               ~
                                       "Enter Sort Method : 1, 2, 3 or 4"
            gosub get_sort_descr
            return

        get_sort_descr:
            if sort$ = "1" then sortdescr$ = "(Specific Assembly)"
            if sort$ = "2" then sortdescr$ = "(Option Part)"
            if sort$ = "3" then sortdescr$ = "(Replacement Part)"
            if sort$ = "4" then sortdescr$ = "(Part Replaced)"
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
            * COPYRIGHT (C) 1988  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
