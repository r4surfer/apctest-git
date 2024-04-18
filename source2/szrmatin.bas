        REM THISPROGRAMWASGENERATEDUSINGTHEGENFSPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   SSS   ZZZZZ  RRRR   M     M   AAA  TTTTT IIIII  N    N  *~
            *  S         Z   R   R  M M M M  A   A   T     I    N N  N  *~
            *   SSS     Z    RRRR   M  M  M  AAAAA   T     I    N  N N  *~
            *      S   Z     R  R   M     M  A   A   T     I    N   NN  *~
            *   SSS   ZZZZZ  R   R  M     M  A   A   T   IIIII  N    N  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SZRMATIN - Define column and row labels and part number   *~
            *            matrix for Size Runs.                          *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/01/92 ! Original                                 ! WPH *~
            * 07/06/92 ! Added copy and print functions           ! WPH *~
            * 10/29/92 ! Corrected branch in report generation.   ! JDH *~
            * 01/13/93 ! Page 0 Facs fix                          ! RJH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            code$14,                     /* Size Run Code              */~
            col$10,                      /* Column                     */~
            cursor%(2),                  /* Cursor position            */~
            column$(6)10,                /* Column                     */~
            descr$30,                    /* Description                */~
            descr$(72)32,                /* Descriptions for report    */~
            company$60,                  /* Company name for report    */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            fromcode$14,                 /* Beginning of range         */~
            inpmessage$79,               /* Informational Message      */~
            i$(24)80,                    /* Screen Image               */~
            lfac$(28)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            message$79,                  /* Text passed to GETCODE     */~
            part$(72)25,                 /* Part Number array (col/row)*/~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            row$(12)12,                  /* Row                        */~
            row$12,                      /* Row                        */~
            tocode$14,                   /* End of range               */~
            readkey$99,                  /*                            */~
            rpttitle$35,                 /* Report Title               */~
            savecode$14,                 /* Temporary variable         */~
            runtime$8,                   /*                            */~
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
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
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
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #02 ! SIZERUNS ! Size Run File                            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            select #02, "SIZERUNS",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos = 1,    keylen = 14

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 100%, rslt$(02))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62%) = "SZRMATIN: " & str(cms2v$,,8%)


        REM *************************************************************~
            *       I N P U T   M O D E   K E Y   F I E L D             *~
            *-----------------------------------------------------------*~
            * Input & Validate the Key Field Only                       *~
            *************************************************************

        inputmode
            gosub initialize_variables
            mode% = 1%
        input_key
            gosub screen1                  /* Key Field Input */
                if keyhit% = 14% then select_report
                if keyhit% = 16% then exit_program
                if keyhit% <> 0% then input_key
            gosub test_key            /* Key Field Validation */
                if errormsg$ <> " " then input_key

        REM *************************************************************~
            *       I N P U T   M O D E   F U L L   S C R E E N         *~
            *-----------------------------------------------------------*~
            * Full Screen Input                                         *~
            *************************************************************

*       INPUT_SCREEN1
            mode% = 2%
        input_screen1a
            gosub screen1                /* Full Screen Input */
                if keyhit% = 3% then gosub copy_control
                if keyhit%  = 10% then mode% = 2%
                if keyhit% <>  0% then input_screen1a
            gosub test_screen1            /* Validate Fields */
                if errormsg$ = " " then input_screen2
            mode% = 4%
            goto input_screen1a


        input_screen2
            mode% = 2%
        input_screen2a
            gosub screen2                /* Full Screen Input */
                if keyhit%  =  4% then input_screen1a
                if keyhit%  = 10% then mode% = 2%
                if keyhit% <>  0% then input_screen2a
            gosub test_screen2            /* Validate Fields */
                if errormsg$ = " " then input_screen3
            mode% = 4%
            goto input_screen2a


        input_screen3
            mode% = 2%
        input_screen3a
            gosub screen3                /* Full Screen Input */
                if keyhit%  =  4% then input_screen2a
                if keyhit%  = 10% then mode% = 2%
                if keyhit% <>  0% then input_screen3a
            gosub test_screen3            /* Validate Fields */
                if errormsg$ = " " then edit_display1
            mode% = 4%
            goto input_screen3a


        REM *************************************************************~
            *        E D I T   M O D E   F U L L   S C R E E N          *~
            *-----------------------------------------------------------*~
            * Handles Full Screen Edit Mode                             *~
            *************************************************************

        edit_display1
            mode% = 3%
            gosub screen1        /* Display Screen - No Entry */
                if keyhit%  =  3% then edit_display3
                if keyhit%  =  5% then edit_display2
                if keyhit% <> 12% then L11110
                      gosub delete_record
                      goto edit_display1  /* get here if delete aborted*/
L11110:         if keyhit%  = 16% then process_data
                if keyhit% <> 10% then edit_display1
            mode% = 5%
        edit_screen1
            gosub screen1            /* Full Screen Edit Mode */
                if keyhit%  = 10% then mode% = 5%
                if keyhit% <>  0% then edit_screen1
            gosub test_screen1         /* Validate Field Data */
                if errormsg$ = " " then edit_display1
                mode% = 4%
                goto edit_screen1


        edit_display2
            mode% = 3%
            gosub screen2        /* Display Screen - No Entry */
                if keyhit%  =  2% then edit_display1
                if keyhit%  =  4% then edit_display1
                if keyhit%  =  3% then edit_display3
                if keyhit%  =  5% then edit_display3
                if keyhit%  = 16% then process_data
                if keyhit% <> 10% then edit_display2
            mode% = 5%
        edit_screen2
            gosub screen2            /* Full Screen Edit Mode */
                if keyhit%  = 10% then mode% = 5%
                if keyhit% <>  0% then edit_screen2
            gosub test_screen2         /* Validate Field Data */
                if errormsg$ = " " then edit_display2
                mode% = 4%
                goto edit_screen2


        edit_display3
            mode% = 3%
            gosub screen3        /* Display Screen - No Entry */
                if keyhit%  =  2% then edit_display1
                if keyhit%  =  4% then edit_display2
                if keyhit%  = 16% then process_data
                if keyhit% <> 10% then edit_display3
            mode% = 5%
        edit_screen3
            gosub screen3            /* Full Screen Edit Mode */
                if keyhit%  = 10% then mode% = 5%
                if keyhit% <>  0% then edit_screen3
            gosub test_screen3         /* Validate Field Data */
                if errormsg$ = " " then edit_display3
                mode% = 4%
                goto edit_screen3


        REM *************************************************************~
            *          C O P Y  A  M A T R I X                          *~
            *-----------------------------------------------------------*~
            * Copies a matrix to create another matrix                  *~
            *************************************************************

        copy_control
            savecode$ = code$
            code$ = " "
            descr$ = hex(06) & "Select Size Run Code to Copy."
            call "GETCODE" (#2, code$, descr$, 0% , 0, f1%(2%))
                if f1%(2%) = 1% then L11770
                descr$ = " "
                if code$ <> " " then return
                errormsg$ = "Field Cannot be blank - enter a code or PF16"
                return
L11770:     return clear all

            readkey$ = code$
            call "READ100" (#2, readkey$, f1%(2%))

            get #2, using L11840,  descr$, column$(), row$(), part$()

L11840:     FMT  POS(15), CH(30), 6*CH(10), 12*CH(10), 72*CH(25)
            code$ = savecode$
            preventcopy% = 1%

            goto edit_display1

        REM *************************************************************~
            *                P R I N T   R E P O R T                    *~
            * --------------------------------------------------------- *~
            * Prompt for range of codes, then print matrices in range.  *~
            *************************************************************

        select_report
            fromcode$ = "ALL"
            tocode$ = " "
            mode% = 2%
        input_screen4a
            gosub screen4            /* Present range selection screen*/
                if keyhit% = 16% then inputmode
                if keyhit% <> 0% then input_screen4a
            gosub test_range          /* Range Validation */
                if errormsg$ <> " " then input_screen4a
        edit_display4
            mode% = 3%
            gosub screen4            /* Display screen, no entry      */
                if keyhit% = 16% then generate_report
                if keyhit% <> 10% then edit_display4
                mode% = 5%
        edit_screen4
            gosub screen4            /* Display screen, Edit Mode     */
                if keyhit% =  10% then mode% = 5%
                if keyhit% <> 0% then edit_display4
            gosub test_range          /* Range Validation */
                if errormsg$ = " " then edit_display4
                mode% = 4%
                goto edit_screen4

        generate_report

            page% =  -1%
            line% = 857%
            rpttitle$ = "Size Run Matrix Report"
            init (hex(00)) readkey$

            if fromcode$ <> "ALL" then L14277
                fromcode$ = all(hex(00))
                tocode$ = all(hex(ff))
                goto L14278
L14277:     fromcode$ =   addc all(hex(ff))
L14278:     str(readkey$,1,14) = str(fromcode$,,)

            call "SETPRNT" ("BCK013", " ", 0%, 0%)
            call "SHOSTAT" ("Printing Size Run Matrix Report")
            select printer
            runtime$ = " "
            call "TIME" (runtime$)
            call "COMPNAME"(12%, company$, k%)

        report_loop
            init(" ") descr$()
            call "PLOWNEXT" (#2, readkey$, 0%, f1%(2))

            if f1%(2) = 0% then end_of_report
            if readkey$ > tocode$ then end_of_report

            get #2, using L14425, code$,descr$, column$(), row$(), part$()

L14425:     FMT  CH(14), CH(30), 6*CH(10), 12*CH(10), 72*CH(25)
            for i% = 1% to 72%
               if part$(i%) = " " then L14450
               call "DESCRIBE"(#1, part$(i%), descr$(i%), 1%, f1%(1%))
L14450:     next i%

            gosub page_heading
            print using L14750, code$, descr$
            print
            print using L14753, column$(1%), column$(2%), column$(3%)
            print
            for i% = 1% to 12%

               print using L14760, i%, row$(i%), part$(i%), part$(12%+i%),~
                                            part$(24%+i%)
               print using L14780, descr$(i%), descr$(12%+i%),            ~
                                            descr$(24%+i%)

            next i%
            print
            print using L14756, column$(4%), column$(5%), column$(6%)
            print
            for i% = 1% to 12%

               print using L14760, i%, row$(i%), part$(36%+i%),           ~
                                        part$(48%+i%), part$(60%+i%)
               print using L14780, descr$(36%+i%), descr$(48%+i%),        ~
                                            descr$(60%+i%)

            next i%

            goto report_loop

        end_of_report
            runtime$ = " " : call "TIME" (runtime$)
            print
            print using L14800 , runtime$
            close printer
            call "SETPRNT" ("BCK013", " ", 0%, 1%)
            goto inputmode

        page_heading
            page% = page% + 1% : line% = 7%
            print page
            print using L14670, date$, runtime$, company$
            print using L14690, rpttitle$, page%
            print
            if page% <> 0% then return
              gosub print_params
            goto page_heading

        print_params           /* Print Page Zero */
L14635:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L14639
                str(i$(), i%, 1%) = hex(20)
                goto L14635
L14639:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            return



L14670: %RUN DATE ######## TIME ########    #############################~
        ~################################                SZRMATIN:BCK013
L14690: %                                                      ##########~
        ~#########################                             PAGE: ###

L14750: %                                  Size Run Code: ############## ~
        ~Description: ##############################

L14753: %                        (A) ##########                    (B) ##~
        ~########                    (C) ##########

L14756: %                        (D) ##########                    (E) ##~
        ~########                    (F) ##########

L14760: % (##) ##########        #########################         ######~
        ~###################         #########################

L14780: %                         ##################################  ###~
        ~#############################  ################################

L14800: %                                               * * * * * End of ~
        ~Report (Time: ########) * * * * *

        REM *************************************************************~
            *          P R O C E S S I N G   S E C T I O N              *~
            *-----------------------------------------------------------*~
            * Process Data Prior to File Output.                        *~
            *************************************************************

        process_data
            gosub dataput
            goto inputmode

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, descr$,                    ~
                      code$, column$(), part$(), row$()
            preventcopy% = 0%

            call "ALLFREE"
            return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload

            call "READ100" (#2, code$, f1%(2%))

            get #2, using L30150, code$, descr$, column$(), row$(), part$()

L30150:     FMT  CH(14), CH(30), 6*CH(10), 12*CH(10), 72*CH(25)
            preventcopy% = 1%
            goto edit_display1

        REM *************************************************************~
            *              W R I T E   D A T A   T O   F I L E S        *~
            *-----------------------------------------------------------*~
            * Writes data from Program Variables into Files.            *~
            *************************************************************

        dataput
            call "READ101" (#2, code$, f1%(2%))

            put #2 using  L30150, code$, descr$, column$(), row$(), part$()

            if f1%(2%) = 1% then rewrite #2 else write #2


            return

        REM *************************************************************~
            *              D E L E T E   R E C O R D                    *~
            *-----------------------------------------------------------*~
            * Blows away the currently displayed record                 *~
            *************************************************************

        delete_record
            k% = 0%
            call "ASKUSER" (k%, "**** ARE YOU SURE? ****",               ~
               "To Delete the currently displayed record",               ~
               "Press PF 32,",                                           ~
               "- or - Press any other key to abort.")

            if k% <> 32% then return

            call "DELETE" (#2, code$, 14%)

            return clear all
            goto inputmode

        REM *************************************************************~
            *               S C R E E N   N U M B E R   1               *~
            *-----------------------------------------------------------*~
            * Input and Edit Screen Number 1                            *~
            *************************************************************

        screen1
            gosub setpf1
            on mode% gosub set_key,     /* Input Key Field Only */       ~
                           set_full1,   /* Full Screen Input    */       ~
                           set_edit,    /* Edit Mode - Display  */       ~
                           set_errs,    /* Error Screen         */       ~
                           set_full1    /* Full Screen Edit     */
            goto display_screen1

        set_full1       /* Full Screen Input and Edit */
            inpmessage$ = "Enter or Modify Column & Row Labels an"    &  ~
                            "d Part Numbers, Press RETURN to VALIDATE."
            init(hex(8c)) lfac$()
            str(lfac$(), 2,  3) = all(hex(80))
            str(lfac$(), 5, 24) = all(hex(81))
            lfac$(1) = hex(8c)
            return

        display_screen1

            accept                                                       ~
                at (01,02),                                              ~
                    "Set-up Size Run Matrix",                            ~
                at (01,64), "1 of 3 :",                                  ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (04,02), "Size Run ID Code   ",                       ~
                at (04,25), fac(lfac$( 1)), code$               , ch(14),~
                at (04,40), fac(lfac$( 2)), descr$              , ch(30),~
                                                                         ~
                at (07,20), "(A)",                                       ~
                at (07,24), fac(lfac$( 3)), column$(1%)         , ch(10),~
                at (07,46), "(B)",                                       ~
                at (07,50), fac(lfac$( 3)), column$(2%)         , ch(10),~
                                                                         ~
                at (08,02), "( 1)",                                      ~
                at (08,07), fac(lfac$( 4%)), row$(1%)           , ch(10),~
                at (08,20), fac(lfac$( 5%)), part$(1%)          , ch(25),~
                at (08,46), fac(lfac$(17%)), part$(13%)         , ch(25),~
                at (09,02), "( 2)",                                      ~
                at (09,07), fac(lfac$( 4%)), row$(2%)           , ch(10),~
                at (09,20), fac(lfac$( 6%)), part$(2%)          , ch(25),~
                at (09,46), fac(lfac$(18%)), part$(14%)         , ch(25),~
                at (10,02), "( 3)",                                      ~
                at (10,07), fac(lfac$( 4%)), row$(3%)           , ch(10),~
                at (10,20), fac(lfac$( 7%)), part$(3%)          , ch(25),~
                at (10,46), fac(lfac$(19%)), part$(15%)         , ch(25),~
                at (11,02), "( 4)",                                      ~
                at (11,07), fac(lfac$( 4%)), row$(4%)           , ch(10),~
                at (11,20), fac(lfac$( 8%)), part$(4%)          , ch(25),~
                at (11,46), fac(lfac$(20%)), part$(16%)         , ch(25),~
                at (12,02), "( 5)",                                      ~
                at (12,07), fac(lfac$( 4%)), row$(5%)           , ch(10),~
                at (12,20), fac(lfac$( 9%)), part$(5%)          , ch(25),~
                at (12,46), fac(lfac$(21%)), part$(17%)         , ch(25),~
                at (13,02), "( 6)",                                      ~
                at (13,07), fac(lfac$( 4%)), row$(6%)           , ch(10),~
                at (13,20), fac(lfac$(10%)), part$(6%)          , ch(25),~
                at (13,46), fac(lfac$(22%)), part$(18%)         , ch(25),~
                at (14,02), "( 7)",                                      ~
                at (14,07), fac(lfac$( 4%)), row$(7%)           , ch(10),~
                at (14,20), fac(lfac$(11%)), part$(7%)          , ch(25),~
                at (14,46), fac(lfac$(23%)), part$(19%)         , ch(25),~
                at (15,02), "( 8)",                                      ~
                at (15,07), fac(lfac$( 4%)), row$(8%)           , ch(10),~
                at (15,20), fac(lfac$(12%)), part$(8%)          , ch(25),~
                at (15,46), fac(lfac$(24%)), part$(20%)         , ch(25),~
                at (16,02), "( 9)",                                      ~
                at (16,07), fac(lfac$( 4%)), row$(9%)           , ch(10),~
                at (16,20), fac(lfac$(13%)), part$(9%)          , ch(25),~
                at (16,46), fac(lfac$(25%)), part$(21%)         , ch(25),~
                at (17,02), "(10)",                                      ~
                at (17,07), fac(lfac$( 4%)), row$(10%)          , ch(10),~
                at (17,20), fac(lfac$(14%)), part$(10%)         , ch(25),~
                at (17,46), fac(lfac$(26%)), part$(22%)         , ch(25),~
                at (18,02), "(11)",                                      ~
                at (18,07), fac(lfac$( 4%)), row$(11%)          , ch(10),~
                at (18,20), fac(lfac$(15%)), part$(11%)         , ch(25),~
                at (18,46), fac(lfac$(27%)), part$(23%)         , ch(25),~
                at (19,02), "(12)",                                      ~
                at (19,07), fac(lfac$( 4%)), row$(12%)          , ch(10),~
                at (19,20), fac(lfac$(16%)), part$(12%)         , ch(25),~
                at (19,46), fac(lfac$(28%)), part$(24%)         , ch(25),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                gosub pfkey_hit
                if keyhit% = 15% then screen1
                return

        setpf1
        if mode% > 1% then setpf_full       /* Key Field Only */
            pf$(1%) = "(1)Start Over          (14)Print Report " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffff0d0e0f1000)
            return

        setpf_full      /* Full Screen input, edit or error */
        if mode% = 3% then setpf1_display
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Copy Existing Matrix                 " &       ~
                      "                                       "
            pfkeys$ = hex(01ff03ffffffffffffffffff0dff0fff00)
            if preventcopy% = 1% then  str(pf$(3),1,23) = "  "
            if preventcopy% = 1% then str(pfkeys$,3, 1) = hex(ff)
            return

        setpf1_display               /* Edit Mode - Display */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "(10)Full Screen Edit   (15)Print Screen"
            pf$(3%) = "(3)Last Screen   (5)Next Screen         " &       ~
                      "(12)Delete Record      (16)SAVE DATA   "
            pfkeys$ = hex(01ff03ff05ffffffff0aff0c0dff0f10ff)

            return



        REM *************************************************************~
            *               S C R E E N   N U M B E R   2               *~
            *-----------------------------------------------------------*~
            * Input and Edit Screen Number 2                            *~
            *************************************************************

        screen2
            gosub setpf2
            on mode% gosub set_key,     /* Input Key Field Only */       ~
                           set_full2,   /* Full Screen Input    */       ~
                           set_edit,    /* Edit Mode - Display  */       ~
                           set_errs,    /* Error Screen         */       ~
                           set_full2    /* Full Screen Edit     */
            goto display_screen2

        set_full2       /* Full Screen Input and Edit */
            inpmessage$ = "Enter or Modify Labels and Part Numbers "  &  ~
                            "as required; Press RETURN to VALIDATE"
            init(hex(8c)) lfac$()
            str(lfac$(), 3,  1) = all(hex(80))
            str(lfac$(), 5, 24) = all(hex(81))

            return

        display_screen2

            accept                                                       ~
                at (01,02),                                              ~
                    "Set-up Size Run Matrix",                            ~
                at (01,64), "2 of 3 :",                                  ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (04,02), "Size Run ID Code   ",                       ~
                at (04,25), fac(hex(8c)), code$                 , ch(14),~
                at (04,40), fac(hex(8c)), descr$                , ch(30),~
                at (07,20), "(C)",                                       ~
                at (07,24), fac(lfac$( 3%)), column$(3%)        , ch(10),~
                at (07,46), "(D)",                                       ~
                at (07,50), fac(lfac$( 3%)), column$(4%)        , ch(10),~
                                                                         ~
                at (08,02), "( 1)",                                      ~
                at (08,07), fac(hex(8c))  , row$(1%)            , ch(10),~
                at (08,20), fac(lfac$( 5%)), part$(25%)         , ch(25),~
                at (08,46), fac(lfac$(17%)), part$(37%)         , ch(25),~
                at (09,02), "( 2)",                                      ~
                at (09,07), fac(hex(8c))  , row$(2%)            , ch(10),~
                at (09,20), fac(lfac$( 6%)), part$(26%)         , ch(25),~
                at (09,46), fac(lfac$(18%)), part$(38%)         , ch(25),~
                at (10,02), "( 3)",                                      ~
                at (10,07), fac(hex(8c))  , row$(3%)            , ch(10),~
                at (10,20), fac(lfac$( 7%)), part$(27%)         , ch(25),~
                at (10,46), fac(lfac$(19%)), part$(39%)         , ch(25),~
                at (11,02), "( 4)",                                      ~
                at (11,07), fac(hex(8c))  , row$(4%)            , ch(10),~
                at (11,20), fac(lfac$( 8%)), part$(28%)         , ch(25),~
                at (11,46), fac(lfac$(20%)), part$(40%)         , ch(25),~
                at (12,02), "( 5)",                                      ~
                at (12,07), fac(hex(8c))  , row$(5%)            , ch(10),~
                at (12,20), fac(lfac$( 9%)), part$(29%)         , ch(25),~
                at (12,46), fac(lfac$(21%)), part$(41%)         , ch(25),~
                at (13,02), "( 6)",                                      ~
                at (13,07), fac(hex(8c))  , row$(6%)            , ch(10),~
                at (13,20), fac(lfac$(10%)), part$(30%)         , ch(25),~
                at (13,46), fac(lfac$(22%)), part$(42%)         , ch(25),~
                at (14,02), "( 7)",                                      ~
                at (14,07), fac(hex(8c))  , row$(7%)            , ch(10),~
                at (14,20), fac(lfac$(11%)), part$(31%)         , ch(25),~
                at (14,46), fac(lfac$(23%)), part$(43%)         , ch(25),~
                at (15,02), "( 8)",                                      ~
                at (15,07), fac(hex(8c))  , row$(8%)            , ch(10),~
                at (15,20), fac(lfac$(12%)), part$(32%)         , ch(25),~
                at (15,46), fac(lfac$(24%)), part$(44%)         , ch(25),~
                at (16,02), "( 9)",                                      ~
                at (16,07), fac(hex(8c))  , row$(9%)            , ch(10),~
                at (16,20), fac(lfac$(13%)), part$(33%)         , ch(25),~
                at (16,46), fac(lfac$(25%)), part$(45%)         , ch(25),~
                at (17,02), "(10)",                                      ~
                at (17,07), fac(hex(8c))  , row$(10%)           , ch(10),~
                at (17,20), fac(lfac$(14%)), part$(34%)         , ch(25),~
                at (17,46), fac(lfac$(26%)), part$(46%)         , ch(25),~
                at (18,02), "(11)",                                      ~
                at (18,07), fac(hex(8c))  , row$(11%)           , ch(10),~
                at (18,20), fac(lfac$(15%)), part$(35%)         , ch(25),~
                at (18,46), fac(lfac$(27%)), part$(47%)         , ch(25),~
                at (19,02), "(12)",                                      ~
                at (19,07), fac(hex(8c))  , row$(12%)           , ch(10),~
                at (19,20), fac(lfac$(16%)), part$(36%)         , ch(25),~
                at (19,46), fac(lfac$(28%)), part$(48%)         , ch(25),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                gosub pfkey_hit
                if keyhit% = 15% then screen2
                return

        setpf2
        if mode% = 3% then setpf2_display
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            if mode% = 2% then str(pf$(2),18,18) = "(4)Previous Screen"
            if mode% = 2% then str(pfkeys$,4, 1) = hex(04)
            return

        setpf2_display               /* Edit Mode - Display */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Screen     " &       ~
                      "(10)Full Screen Edit   (15)Print Screen"
            pf$(3%) = "                 (5)Next Screen         " &       ~
                      "                       (16)SAVE DATA   "
            pfkeys$ = hex(0102030405ffffffff0affff0dff0f10ff)

            return

        REM *************************************************************~
            *               S C R E E N   N U M B E R   3               *~
            *-----------------------------------------------------------*~
            * Input and Edit Screen Number 3                            *~
            *************************************************************

        screen3
            gosub setpf3
            on mode% gosub set_key,     /* Input Key Field Only */       ~
                           set_full3,   /* Full Screen Input    */       ~
                           set_edit,    /* Edit Mode - Display  */       ~
                           set_errs,    /* Error Screen         */       ~
                           set_full3    /* Full Screen Edit     */
            goto display_screen3

        set_full3       /* Full Screen Input and Edit */
            inpmessage$ = "Enter or Modify Labels and Part Numbers "  &  ~
                            "as required; Press RETURN to VALIDATE"
            init(hex(8c)) lfac$()
            str(lfac$(), 3,  1) = all(hex(80))
            str(lfac$(), 5, 24) = all(hex(81))
            return

        display_screen3

            accept                                                       ~
                at (01,02),                                              ~
                    "Set-up Size Run Matrix",                            ~
                at (01,64), "3 of 3 :",                                  ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (04,02), "Size Run ID Code   ",                       ~
                at (04,25), fac(hex(8c)), code$                 , ch(14),~
                at (04,40), fac(hex(8c)), descr$                , ch(30),~
                at (07,20), "(E)",                                       ~
                at (07,24), fac(lfac$( 3%)), column$(5%)        , ch(10),~
                at (07,46), "(F)",                                       ~
                at (07,50), fac(lfac$( 3%)), column$(6%)        , ch(10),~
                                                                         ~
                at (08,02), "( 1)",                                      ~
                at (08,07), fac(hex(8c)),   row$(1%)            , ch(10),~
                at (08,20), fac(lfac$( 5%)), part$(49%)         , ch(25),~
                at (08,46), fac(lfac$(17%)), part$(61%)         , ch(25),~
                at (09,02), "( 2)",                                      ~
                at (09,07), fac(hex(8c)),   row$(2%)            , ch(10),~
                at (09,20), fac(lfac$( 6%)), part$(50%)         , ch(25),~
                at (09,46), fac(lfac$(18%)), part$(62%)         , ch(25),~
                at (10,02), "( 3)",                                      ~
                at (10,07), fac(hex(8c)),   row$(3%)            , ch(10),~
                at (10,20), fac(lfac$( 7%)), part$(51%)         , ch(25),~
                at (10,46), fac(lfac$(19%)), part$(63%)         , ch(25),~
                at (11,02), "( 4)",                                      ~
                at (11,07), fac(hex(8c)),   row$(4%)            , ch(10),~
                at (11,20), fac(lfac$( 8%)), part$(52%)         , ch(25),~
                at (11,46), fac(lfac$(20%)), part$(64%)         , ch(25),~
                at (12,02), "( 5)",                                      ~
                at (12,07), fac(hex(8c)),   row$(5%)            , ch(10),~
                at (12,20), fac(lfac$( 9%)), part$(53%)         , ch(25),~
                at (12,46), fac(lfac$(21%)), part$(65%)         , ch(25),~
                at (13,02), "( 6)",                                      ~
                at (13,07), fac(hex(8c)),   row$(6%)            , ch(10),~
                at (13,20), fac(lfac$(10%)), part$(54%)         , ch(25),~
                at (13,46), fac(lfac$(22%)), part$(66%)         , ch(25),~
                at (14,02), "( 7)",                                      ~
                at (14,07), fac(hex(8c)),   row$(7%)            , ch(10),~
                at (14,20), fac(lfac$(11%)), part$(55%)         , ch(25),~
                at (14,46), fac(lfac$(23%)), part$(67%)         , ch(25),~
                at (15,02), "( 8)",                                      ~
                at (15,07), fac(hex(8c)),   row$(8%)            , ch(10),~
                at (15,20), fac(lfac$(12%)), part$(56%)         , ch(25),~
                at (15,46), fac(lfac$(24%)), part$(68%)         , ch(25),~
                at (16,02), "( 9)",                                      ~
                at (16,07), fac(hex(8c)),   row$(9%)            , ch(10),~
                at (16,20), fac(lfac$(13%)), part$(57%)         , ch(25),~
                at (16,46), fac(lfac$(25%)), part$(69%)         , ch(25),~
                at (17,02), "(10)",                                      ~
                at (17,07), fac(hex(8c)),   row$(10%)           , ch(10),~
                at (17,20), fac(lfac$(14%)), part$(58%)         , ch(25),~
                at (17,46), fac(lfac$(26%)), part$(70%)         , ch(25),~
                at (18,02), "(11)",                                      ~
                at (18,07), fac(hex(8c)),   row$(11%)           , ch(10),~
                at (18,20), fac(lfac$(15%)), part$(59%)         , ch(25),~
                at (18,46), fac(lfac$(27%)), part$(71%)         , ch(25),~
                at (19,02), "(12)",                                      ~
                at (19,07), fac(hex(8c)),   row$(12%)           , ch(10),~
                at (19,20), fac(lfac$(16%)), part$(60%)         , ch(25),~
                at (19,46), fac(lfac$(28%)), part$(72%)         , ch(25),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                gosub pfkey_hit
                if keyhit% = 15% then screen3
                return

        setpf3
        if mode% = 3% then setpf3_display
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            if mode% = 2% then str(pf$(2),18,18) = "(4)Previous Screen"
            if mode% = 2% then str(pfkeys$,4, 1) = hex(04)
            return

        setpf3_display               /* Edit Mode - Display */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "(2)First Screen  (4)Previous Screen     " &       ~
                      "(10)Full Screen Edit   (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)SAVE DATA   "
            pfkeys$ = hex(0102ff04ffffffffff0affff0dff0f10ff)

            return

        REM *************************************************************~
            *               S C R E E N   N U M B E R   4               *~
            *-----------------------------------------------------------*~
            * Range selection screen for printed report.                *~
            *************************************************************

        screen4
            gosub setpf4
            on mode% gosub set_key,     /* Input Key Field Only */       ~
                           set_full4,   /* Full Screen Input    */       ~
                           set_edit_prt,/* Edit Mode - Display  */       ~
                           set_errs,    /* Error Screen         */       ~
                           set_full4    /* Full Screen Edit     */
            goto display_screen4

        set_full4       /* Full Screen Input and Edit */
            init(hex(8c)) lfac$()
            if mode% =  2% or mode% = 5% then lfac$() = all(hex(81))

            inpmessage$ = "Enter Range of Codes to Print or 'ALL' or " & ~
                          "'?' to select.  RETURN to Continue."
            return

        display_screen4
            accept                                                       ~
                at (01,02),                                              ~
                    "Print Size Run Matices",                            ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (01,66), "Today:"                            ,        ~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (06,02), "First Size Run Code:"              ,        ~
                at (06,23), fac(lfac$(1%)), fromcode$           , ch(14),~
                at (07,02), "Last Size Run Code:"               ,        ~
                at (07,23), fac(lfac$(1%)), tocode$             , ch(14),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                gosub pfkey_hit
                if keyhit% = 15% then screen4

            close ws
            call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
            return

        setpf4
            if mode% =  3% then setpf4_display
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit/Return "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            if mode% <> 5% then return
                str(pf$(3%),64,15) = " "
                str(pfkeys$,16,1) = hex(ff)
                return

        setpf4_display               /* Edit Mode - Display */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "(10)Full Screen Edit   (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Print Report"
            pfkeys$ = hex(0102ffffffffffffff0affff0dff0f10ff)

            return

        REM *************************************************************~
            *      C O M M O N   S C R E E N   S U B R O U T I N E S    *~
            *-----------------------------------------------------------*~
            * Subroutines Common to all Screens.                        *~
            *************************************************************

        set_key         /* Input For Code Field Only */
            inpmessage$ = "Enter a Size Run Code or blank to see list"
            init(hex(8c)) lfac$()
            lfac$(1%) = hex(81)
            return

        set_edit        /* Edit Mode Display Only */
            inpmessage$ = "Press PF-10 for Full Screen Edit - OR - "  &  ~
                            "PF-16 to SAVE DATA"
            init(hex(8c)) lfac$()
            return

        set_edit_prt    /* Edit Mode Display Only */
            inpmessage$ = "Press PF-10 for Full Screen Edit - OR - "  &  ~
                            "PF-16 to PRINT REPORT"
            init(hex(8c)) lfac$()
            return

        set_errs        /* Error Correction Screen */
            inpmessage$ = "Correct ALL Bright Fields and Press RETURN" & ~
                            " to Validate Entries"
            return


        pfkey_hit
            if keyhit% <> 1% then pf13
                gosub startover
                keyhit% = 15%
                return

        pf13
            if keyhit% <> 13% then pf15
                call "MANUAL" ("SZRMATIN")
                keyhit% = 15%
                return

        pf15
            if keyhit% <> 15% then return
            call "PRNTSCRN"
            return

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *                 T E S T   K E Y   D A T A                 *~
            *-----------------------------------------------------------*~
            * Test data for Key Value Entered                           *~
            *************************************************************

        test_key
            errormsg$ = " "
            init(hex(8c)) lfac$()

        REM Test for Key Value Entered

            descr$ = hex(06) & "Size Run Codes on file"
            call "GETCODE" (#2, code$, descr$, 0% , 0, f1%(2%))
                if f1%(2%) = 1% then L50900
                descr$ = " "
                if code$ <> " " then return
                errormsg$ = "Field Cannot be blank - enter a code"
                return
L50900:     return clear all
            goto dataload

        REM *************************************************************~
            *                 T E S T   D A T A                         *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screens 1 through 3.           *~
            *************************************************************

        test_screen1
            screen% = 1%
            gosub test_loop
            return

        test_screen2
            screen% = 2%
            gosub test_loop
            return

        test_screen3
            screen% = 3%
            gosub test_loop
            return

        test_loop
            errormsg$ = " "
            init(hex(8c)) lfac$()
            warn% = 0%
            for i% = 1% to 24%
                if part$(i%+(24%*(screen%-1%))) =  " " then L51260
                if i% > 12% then L51210
                    row$ = row$(i%)
                    col$ = column$(1%+((screen%-1%)*2%))
                    goto L51212
L51210:         row$ = row$(i%-12%)
                col$ = column$(2%+(2%*(screen%-1%)))
L51212:         if row$ = " " or col$ = " " then warn% = 1%
                message$ = hex(06) & "Select Part Number for " & hex(84) ~
                                   &  row$ & " / " & col$

                call "GETCODE" (#1, part$(i%+(24%*(screen%-1%))),        ~
                               message$, 0%, 0, f1%(1%))
                   if f1%(1%) = 1% then L51260
                if errormsg$ = " " then errormsg$ =                      ~
                    "Enter or Select a Valid Part Number or leave blank"
                lfac$(i%+4%) = hex(81)

L51260:     next i%
            if warn% = 0% then return
            key% = 2%
            call "ASKUSER" (key%, "* * * * *  WARNING  * * * * *",       ~
                 "You have entered a part number into at least one cell",~
                 "that does not have a column and/or row label.",        ~
                          "Press any key to acknowledge.")


            return

        REM *************************************************************~
            *                 T E S T   R A N G E                       *~
            *-----------------------------------------------------------*~
            * Test data for the range selection for printing            *~
            *************************************************************

         test_range
            errormsg$ = " "

            if fromcode$ <> "ALL" then L53060
                tocode$ = "  "
                return

L53060:     message$ = hex(06) & hex(84) & "Select Code Number for" &    ~
                                  " Beginning of Range to Print."

            call "GETCODE" (#2, fromcode$, message$,0%,0, f1%(1%))
                if f1%(1%) = 1% then L53130

            errormsg$ = "Enter or Select a Valid Code or set to 'ALL'"


L53130:     if tocode$ = " " then tocode$ = fromcode$


            message$ = hex(06) & hex(84) &  "Select Code Number for " &  ~
                                   "Ending Value of Range to Print."
            if tocode$ = fromcode$ then L53245
            call "GETCODE" (#2, tocode$, message$,0%,0, f1%(1%))
                if f1%(1%) = 1% then L53245

                if errormsg$ = " " then errormsg$ =                      ~
                    "Select a Valid Ending Code of Range or Leave Blank"

L53245:     if tocode$ >= fromcode$ then return
                if errormsg$ = " " then errormsg$ =                      ~
                    "Ending Value Must Be Greater Than Beginning Value."

            return
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
