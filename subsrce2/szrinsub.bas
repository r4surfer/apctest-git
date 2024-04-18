        REM THISPROGRAMWASGENERATEDUSINGTHEGENFSPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   SSS   ZZZZZ  RRRR   IIIII  N   N   SSS   U   U  BBBB    *~
            *  S         Z   R   R    I    NN  N  S      U   U  B   B   *~
            *   SSS     Z    RRRR     I    N N N   SSS   U   U  BBBB    *~
            *      S   Z     R  R     I    N  NN      S  U   U  B   B   *~
            *   SSS   ZZZZZ  R   R  IIIII  N   N   SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SZRINSUB - Prompts user to select a Size Run, then allows *~
            *            entry of quantity matrix for that run.         *~
            *            Passes the resulting part and corresponding    *~
            *            quantity arrays back to the caller.  Most of   *~
            *            the channels & stuff passed in are to allow    *~
            *            calls to PIPATCSB and HNYQDISP  for display of *~
            *            ATC/Qty information.                           *~
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
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "SZRINSUB" (caller%,      /*  1 = Sales order, 2 = PO      */~
                        lines%,       /*  current lines of SO or PO    */~
                        passpart$(),  /*  part array passed back       */~
                        passqty$(),   /*  quantity array passed back   */~
                        default$,     /*  line defaults passed back    */~
                        #2,           /*  SYSFILE2                     */~
                        #4,           /*  HNYMASTR                     */~
                        #13,          /*  HNYQUAN                      */~
                        #19,          /*  CALMASTR                     */~
                        #23,          /*  DEMMASTR                     */~
                        #24,          /*  PIPMASTR                     */~
                        #25,          /*  PIPIN                        */~
                        #26,          /*  PIPOUT                       */~
                        #27,          /*  PIPCROSS                     */~
                        #28,          /*  SFCUM2                       */~
                        #29,          /*  HNYDETAL                     */~
                        limit%)       /*  Max. allowable lines on order*/~

        dim                                                              ~
            allowed$3,                   /* Number of lines allowed    */~
            code$14,                     /* Size Run ID Code           */~
            column$(6)10,                /* Column                     */~
            cursor%(2),                  /* Cursor position            */~
            default$30,                  /* Line item default values   */~
            descr$32,                    /* Description                */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            error$79,                    /* Error message              */~
            fieldprompt$(8)8,            /* prompts for default fields */~
            field$1,                     /* dummy variable             */~
            inpmessage$79,               /* Informational Message      */~
            i$(24)80,                    /* Screen Image               */~
            lfac$(80)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            passpart$(72)25,             /* Part array passed to caller*/~
            passqty$(72)10,              /* Qty  array passed to caller*/~
            part$(72)25,                 /* Part Number array (col/row)*/~
            podfac$(4)1,                 /* PO Default prompt facs     */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            qty$(72)10,                  /* Quantities entered         */~
            qty(72),                     /* Quantities entered         */~
            row$(12)12,                  /* Row                        */~
            sodfac$(4)1,                 /* SO Default prompt facs     */~
            testerror$79,                /* error message              */~
            userid$3                     /* Current User Id            */

        dim alloc$1,    /*S.O. Defaults     Allocation Flag            */~
            tax$1,                       /* Taxable flag default       */~
            priority$1,                  /* Demand Priority default    */~
            demtype$1                    /* Demand Type Default        */



        /* DIM for PO defaults would go here */


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
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
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
            * #02 ! SIZERUNS ! Size Runs file                           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #01, "SIZERUNS",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos = 1,    keylen = 14


            if fs%(01) = 1% then L09000
            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
                  if f2%(1) = 0% then L09000
                  call "ASKUSER" (k%, " * * * * FILE NOT FOUND * * * *", ~
                      "The Size Run File could not be found.",           ~
                      "Run the program SZRMATIN or check your database.",~
                      "Press any PF key to acknowledge.")
                  goto exit_program

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            if caller% < 1% or caller% > 2% then exit_program

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62) = "SZRINSUB: " & str(cms2v$,,8)

            init(hex(9c)) sodfac$(), podfac$()
            if caller% <> 1% then  L09180

*       Set up line default fields for Sales Order
            fieldprompt$(1) = "Allocate"
            fieldprompt$(2) = "Taxable?"
            fieldprompt$(3) = "Dem Type"
            fieldprompt$(4) = "Priority"

            init(hex(8c)) sodfac$()                 /* dim protected */
            goto  L10000

L09180
*       Or set up line default fields for Purchase Order
            fieldprompt$(5) = " test   "
            fieldprompt$(6) = " test   "
            fieldprompt$(7) = " test   "
            fieldprompt$(8) = " test   "

            init(hex(9c)) podfac$()     /* to activate, set to dim    */

L10000: REM *************************************************************~
            *       I N P U T   M O D E   K E Y   F I E L D             *~
            *-----------------------------------------------------------*~
            * Input & Validate the Key Field Only                       *~
            *************************************************************

        select_code
            gosub initialize_variables

            descr$ = hex(06) & "Size Run Codes on File"
            call "GETCODE" (#1, code$, descr$, 0% , 0, f1%(1%))
                if f1%(1%) = 0% then exit_program

            gosub  dataload





        REM *************************************************************~
            *       I N P U T   M O D E   F U L L   S C R E E N         *~
            *-----------------------------------------------------------*~
            * Full Screen Input for the one and only screen             *~
            *************************************************************

            mode% = 2%

        input_screen1a
            gosub screen1                /* Full Screen Input */
                if keyhit%  = 32% then exit_program
                if keyhit% <>  0% then input_screen1a
            gosub test_screen1            /* Validate Fields */
                if errormsg$ = " " then edit_display1
            mode% = 4%
            goto input_screen1a

        REM *************************************************************~
            *        E D I T   M O D E   F U L L   S C R E E N          *~
            *-----------------------------------------------------------*~
            * Handles Full Screen Edit Mode                             *~
            *************************************************************

        edit_display1
            mode% = 3%
            gosub screen1        /* Display Screen - No Entry */
                if keyhit% <> 12% then L11110
L11110:         if keyhit%  = 16% then prepare_to_pass
                if keyhit% <> 10% then edit_display1
            mode% = 5%
        edit_screen1
            gosub screen1            /* Full Screen Edit Mode */
                if keyhit%  = 32% then exit_program
                if keyhit%  = 10% then mode% = 5%
                if keyhit% <>  0% then edit_screen1
            gosub test_screen1         /* Validate Field Data */
                if errormsg$ = " " then edit_display1
                mode% = 4%
                goto edit_screen1


        REM *************************************************************~
            *     P R E P A R E   T O   P A S S   D A T A   B A C K     *~
            *-----------------------------------------------------------*~
            * Consolidate selected quantity/parts in array for caller.  *~
            *************************************************************

         prepare_to_pass

*       Consolidate selected parts/quantities into front of pass arrays
            c% = 0%
            for i% = 1% to 72%
               if qty$(i%) = " " then  L20140
               passpart$(c% + 1%) = part$(i%)
               passqty$(c% + 1%) = qty$(i%)
               c% = c% + 1%
L20140:     next i%

*       Build the DEFAULT$ to pass default values back

            if caller% <> 1% then L20995  /* or whereever we handle PO */
            str(default$,1,1) = alloc$
            str(default$,2,1) = tax$
            str(default$,3,1) = demtype$
            str(default$,4,1) = priority$



L20995:     goto exit_program


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, descr$, passpart$(), code$,~
                      qty$(), column$(), part$(), row$(), passqty$(),    ~
                      default$, alloc$, tax$, priority$, demtype$

            call "ALLFREE"
            return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload

            call "READ100" (#1, code$, f1%(1%))
                if f1%(1%) = 0% then exit_program
            get #1, using L30150, code$, descr$, column$(), row$(), part$()

L30150:     FMT  CH(14), CH(30), 6*CH(10), 12*CH(10), 72*CH(25)
            call "PUTPAREN" (descr$)
            str(line2$, 1, 5 ) = " for:"
            str(line2$, 7, 14) = code$
            str(line2$, 22, 32) = descr$

            return


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
            inpmessage$ = "Enter or Modify Quantities in Stocking UOM" & ~
                            " & Press RETURN to VALIDATE"

            init(hex(8c)) lfac$()

            gosub enable_active_fields

*       Set Facs for Sales Order Default fields
            if caller% <> 1% then L40220
            lfac$(73) = hex(81)   /* allocation flag */
            lfac$(74) = hex(81)   /* taxable flag    */
            lfac$(75) = hex(82)   /* demand type     */
            lfac$(76) = hex(81)   /* priority        */
            str(lfac$(), 77, 4) = all(hex(9c))

            return

L40220
*       Set Facs for Purchase Order Default fields
            lfac$(77) = hex(9c)   /*                 */
            lfac$(78) = hex(9c)   /*                 */
            lfac$(79) = hex(9c)   /*                 */
            lfac$(80) = hex(9c)   /*                 */
            str(lfac$(), 73, 4) = all(hex(9c))
            return

        display_screen1

            accept                                                       ~
                at (01,02),                                              ~
                    "Enter Size Run Quantities",                         ~
                at (01,67), "Date:",                                     ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (03,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (05,02), fac(hex(8c)), row$( 1)              , ch(12),~
                at (06,02), fac(hex(8c)), row$( 2)              , ch(12),~
                at (07,02), fac(hex(8c)), row$( 3)              , ch(12),~
                at (08,02), fac(hex(8c)), row$( 4)              , ch(12),~
                at (09,02), fac(hex(8c)), row$( 5)              , ch(12),~
                at (10,02), fac(hex(8c)), row$( 6)              , ch(12),~
                at (11,02), fac(hex(8c)), row$( 7)              , ch(12),~
                at (12,02), fac(hex(8c)), row$( 8)              , ch(12),~
                at (13,02), fac(hex(8c)), row$( 9)              , ch(12),~
                at (14,02), fac(hex(8c)), row$(10)              , ch(12),~
                at (15,02), fac(hex(8c)), row$(11)              , ch(12),~
                at (16,02), fac(hex(8c)), row$(12)              , ch(12),~
                                                                         ~
                at (04,14), fac(hex(8c)),   column$(1%)         , ch(10),~
                at (05,14), fac(lfac$( 1)),  qty$( 1%)          , ch(10),~
                at (06,14), fac(lfac$( 2)),  qty$( 2%)          , ch(10),~
                at (07,14), fac(lfac$( 3)),  qty$( 3%)          , ch(10),~
                at (08,14), fac(lfac$( 4)),  qty$( 4%)          , ch(10),~
                at (09,14), fac(lfac$( 5)),  qty$( 5%)          , ch(10),~
                at (10,14), fac(lfac$( 6)),  qty$( 6%)          , ch(10),~
                at (11,14), fac(lfac$( 7)),  qty$( 7%)          , ch(10),~
                at (12,14), fac(lfac$( 8)),  qty$( 8%)          , ch(10),~
                at (13,14), fac(lfac$( 9)),  qty$( 9%)          , ch(10),~
                at (14,14), fac(lfac$(10)),  qty$(10%)          , ch(10),~
                at (15,14), fac(lfac$(11)),  qty$(11%)          , ch(10),~
                at (16,14), fac(lfac$(12)),  qty$(12%)          , ch(10),~
                                                                         ~
                at (04,25), fac(hex(8c)),   column$(2%)         , ch(10),~
                at (05,25), fac(lfac$(13)),  qty$(13%)          , ch(10),~
                at (06,25), fac(lfac$(14)),  qty$(14%)          , ch(10),~
                at (07,25), fac(lfac$(15)),  qty$(15%)          , ch(10),~
                at (08,25), fac(lfac$(16)),  qty$(16%)          , ch(10),~
                at (09,25), fac(lfac$(17)),  qty$(17%)          , ch(10),~
                at (10,25), fac(lfac$(18)),  qty$(18%)          , ch(10),~
                at (11,25), fac(lfac$(19)),  qty$(19%)          , ch(10),~
                at (12,25), fac(lfac$(20)),  qty$(20%)          , ch(10),~
                at (13,25), fac(lfac$(21)),  qty$(21%)          , ch(10),~
                at (14,25), fac(lfac$(22)),  qty$(22%)          , ch(10),~
                at (15,25), fac(lfac$(23)),  qty$(23%)          , ch(10),~
                at (16,25), fac(lfac$(24)),  qty$(24%)          , ch(10),~
                                                                         ~
                at (04,36), fac(hex(8c)),   column$(3%)         , ch(10),~
                at (05,36), fac(lfac$(25)),  qty$(25%)          , ch(10),~
                at (06,36), fac(lfac$(26)),  qty$(26%)          , ch(10),~
                at (07,36), fac(lfac$(27)),  qty$(27%)          , ch(10),~
                at (08,36), fac(lfac$(28)),  qty$(28%)          , ch(10),~
                at (09,36), fac(lfac$(29)),  qty$(29%)          , ch(10),~
                at (10,36), fac(lfac$(30)),  qty$(30%)          , ch(10),~
                at (11,36), fac(lfac$(31)),  qty$(31%)          , ch(10),~
                at (12,36), fac(lfac$(32)),  qty$(32%)          , ch(10),~
                at (13,36), fac(lfac$(33)),  qty$(33%)          , ch(10),~
                at (14,36), fac(lfac$(34)),  qty$(34%)          , ch(10),~
                at (15,36), fac(lfac$(35)),  qty$(35%)          , ch(10),~
                at (16,36), fac(lfac$(36)),  qty$(36%)          , ch(10),~
                                                                         ~
                at (04,47), fac(hex(8c)),   column$(4%)         , ch(10),~
                at (05,47), fac(lfac$(37)),  qty$(37%)          , ch(10),~
                at (06,47), fac(lfac$(38)),  qty$(38%)          , ch(10),~
                at (07,47), fac(lfac$(39)),  qty$(39%)          , ch(10),~
                at (08,47), fac(lfac$(40)),  qty$(40%)          , ch(10),~
                at (09,47), fac(lfac$(41)),  qty$(41%)          , ch(10),~
                at (10,47), fac(lfac$(42)),  qty$(42%)          , ch(10),~
                at (11,47), fac(lfac$(43)),  qty$(43%)          , ch(10),~
                at (12,47), fac(lfac$(44)),  qty$(44%)          , ch(10),~
                at (13,47), fac(lfac$(45)),  qty$(45%)          , ch(10),~
                at (14,47), fac(lfac$(46)),  qty$(46%)          , ch(10),~
                at (15,47), fac(lfac$(47)),  qty$(47%)          , ch(10),~
                at (16,47), fac(lfac$(48)),  qty$(48%)          , ch(10),~
                                                                         ~
                at (04,58), fac(hex(8c)),   column$(5%)         , ch(10),~
                at (05,58), fac(lfac$(49)),  qty$(49%)          , ch(10),~
                at (06,58), fac(lfac$(50)),  qty$(50%)          , ch(10),~
                at (07,58), fac(lfac$(51)),  qty$(51%)          , ch(10),~
                at (08,58), fac(lfac$(52)),  qty$(52%)          , ch(10),~
                at (09,58), fac(lfac$(53)),  qty$(53%)          , ch(10),~
                at (10,58), fac(lfac$(54)),  qty$(54%)          , ch(10),~
                at (11,58), fac(lfac$(55)),  qty$(55%)          , ch(10),~
                at (12,58), fac(lfac$(56)),  qty$(56%)          , ch(10),~
                at (13,58), fac(lfac$(57)),  qty$(57%)          , ch(10),~
                at (14,58), fac(lfac$(58)),  qty$(58%)          , ch(10),~
                at (15,58), fac(lfac$(59)),  qty$(59%)          , ch(10),~
                at (16,58), fac(lfac$(60)),  qty$(60%)          , ch(10),~
                                                                         ~
                at (04,69), fac(hex(8c)),   column$(6%)         , ch(10),~
                at (05,69), fac(lfac$(61)),  qty$(61%)          , ch(10),~
                at (06,69), fac(lfac$(62)),  qty$(62%)          , ch(10),~
                at (07,69), fac(lfac$(63)),  qty$(63%)          , ch(10),~
                at (08,69), fac(lfac$(64)),  qty$(64%)          , ch(10),~
                at (09,69), fac(lfac$(65)),  qty$(65%)          , ch(10),~
                at (10,69), fac(lfac$(66)),  qty$(66%)          , ch(10),~
                at (11,69), fac(lfac$(67)),  qty$(67%)          , ch(10),~
                at (12,69), fac(lfac$(68)),  qty$(68%)          , ch(10),~
                at (13,69), fac(lfac$(69)),  qty$(69%)          , ch(10),~
                at (14,69), fac(lfac$(70)),  qty$(70%)          , ch(10),~
                at (15,69), fac(lfac$(71)),  qty$(71%)          , ch(10),~
                at (16,69), fac(lfac$(72)),  qty$(72%)          , ch(10),~
                                                                         ~
                at (18,07), fac(sodfac$(1%)),   fieldprompt$(1%), ch(08),~
                at (18,16), fac(lfac$(73)),  alloc$             , ch(01),~
                                                                         ~
                at (18,20), fac(sodfac$(2%)),   fieldprompt$(2%), ch(08),~
                at (18,29), fac(lfac$(74)),  tax$               , ch(01),~
                                                                         ~
                at (18,33), fac(sodfac$(3%)),   fieldprompt$(3%), ch(08),~
                at (18,42), fac(lfac$(75)),  demtype$           , ch(01),~
                                                                         ~
                at (18,46), fac(sodfac$(4%)),   fieldprompt$(4%), ch(08),~
                at (18,55), fac(lfac$(76)),  priority$          , ch(01),~
                                                                         ~
                at (19,07), fac(podfac$(1%)),   fieldprompt$(5%), ch(08),~
                at (19,16), fac(lfac$(77)),  field$             , ch(01),~
                                                                         ~
                at (19,20), fac(podfac$(2%)),   fieldprompt$(6%), ch(08),~
                at (19,29), fac(lfac$(78)),  field$             , ch(01),~
                                                                         ~
                at (19,33), fac(podfac$(3%)),   fieldprompt$(7%), ch(08),~
                at (19,42), fac(lfac$(79)),  field$             , ch(01),~
                                                                         ~
                at (19,46), fac(podfac$(4%)),   fieldprompt$(8%), ch(08),~
                at (19,55), fac(lfac$(80)),  field$             , ch(01),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1)              , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2)              , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3)              , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                gosub pfkey_hit
                if keyhit% = 15% then screen1
                return


        setpf1          /* Full Screen input, edit or error */
        if mode% = 3% then setpf1_display
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "Position Cursor & (8) for ATC or (24) fo" &        ~
                     "r On-Hand              (32)Abort/Exit  "
            pfkeys$ = hex(01ffffffffffff08ffffffff0d200f1800)
            return

        setpf1_display               /* Edit Mode - Display */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "(10)Full Screen Edit   (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Create Lines "
            pfkeys$ = hex(01ffffffffffffffff0affff0dff0f10ff)

            return


        REM *************************************************************~
            *      W H I C H  O N E                                     *~
            *-----------------------------------------------------------*~
            * Finds part number based on cursor position.               *~
            *************************************************************

        which_one
            c% = 0%
            close ws
            call "SCREEN" addr("C", u3%, "I", i$(), cursor%())

            if cursor%(1) < 5% then return
            if cursor%(1) > 17% then return

            if cursor%(2) > 24 then L48150
               c% = cursor%(1) - 4%
               return

L48150:     if cursor%(2) > 35 then L48190
               c% = cursor%(1) - 4% + 12%
               return

L48190:     if cursor%(2) > 46 then L48230
               c% = cursor%(1) - 4% + 24%
               return

L48230:     if cursor%(2) > 57 then L48270
               c% = cursor%(1) - 4% + 36%
               return

L48270:     if cursor%(2) > 68 then L48310
               c% = cursor%(1) - 4% + 48%
               return

L48310:     c% = cursor%(1) - 4% + 60%
               return


        REM *************************************************************~
            *      C O M M O N   S C R E E N   S U B R O U T I N E S    *~
            *-----------------------------------------------------------*~
            * Subroutines Common to all Screens.                        *~
            *************************************************************

        set_key         /* Input For Code Field Only */
            inpmessage$ = "Input Key Field"
            init(hex(8c)) lfac$()
            lfac$(1) = hex(81)
            return

        set_edit        /* Edit Mode Display Only */
            inpmessage$ = "Press PF-10 for Full Screen Edit - OR - "  &  ~
                            "PF-16 to Generate Order Lines"
            init(hex(8c)) lfac$()
            return

        set_errs        /* Error Correction Screen */
            inpmessage$ = "Correct ALL Bright Fields and Press RETURN" & ~
                            " to Validate Entries"
            return

        enable_active_fields
            for i% = 1% to 72%
               if part$(i%) <> " " then lfac$(i%) = hex(82)
            next i%
            return

        pfkey_hit
            if keyhit% <> 1% then pf8
                gosub startover
                keyhit% = 15%
                return

        pf8
            if keyhit% <> 8% then pf24
                gosub which_one
                if part$(c%) = " " then L49705
                if c% = 0% then L49705
                call "PIPATCSB"(part$(c%), #24, #4, #28, #19,            ~
                                   #25, #26, #29, #23, #27)
L49705:         keyhit% = 15%
                return

        pf24
            if keyhit% <> 24% then pf13
                gosub which_one
                if part$(c%) = " " then L49745
                if c% = 0% then L49745
                call "HNYQDISP"(part$(c%), #4, #13, #13, #2)
L49745:         keyhit% = 15%
                return

        pf13
            if keyhit% <> 13% then pf15
                call "MANUAL" ("SZRINSUB")
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
            goto select_code

        REM *************************************************************~
            *                 T E S T   D A T A                         *~
            *-----------------------------------------------------------*~
            * Test data for Quantity and Line Item field Defaults       *~
            *************************************************************

        test_screen1
            testerror$, errormsg$ = " "
            count% = 0%
            init(hex(8c)) lfac$()

*       Test the entered Quantities
            for i% = 1% to 72%
                if qty$(i%) =  " " then L51240
                count% = count% + 1%
                call "NUMTEST" (qty$(i%), 0, 9e7, error$, 2.2, qty(i%))
                if error$ = " " then  L51240
                   if testerror$ = " " then  errormsg$ = error$
                   testerror$ = errormsg$
                   lfac$(i%) = hex(82)
L51240:     next i%

            if errormsg$ <> " " then return
            if count% > 0% then  L51274
               errormsg$ = "Enter quantity in at least one field or " &  ~
                           "press PF 32 to abort"

               gosub enable_active_fields
               return

L51274:        allowed% = limit% - lines%
               if count% <= allowed% then L51332
               k% = 0%

               convert allowed% to allowed$, pic(###)

               call "ASKUSER" (k%, "**** TOO MANY LINES ****",           ~
               "You have selected too many cells in the matrix.",        ~
               "The order has room for only "& allowed$ &" more lines.", ~
               "Press RETURN to edit the matrix, or PF 16 to abort.")

               if k% = 16% then exit_program
               str(lfac$(), 1 ,72) = hex(82)
               errormsg$ = "Too many cells; reduce to " & allowed$
               return

L51332:     if caller% <> 1% then L53000

*       Test Allocation Flag
            if alloc$ = " " then L51410
            if pos("NACZ" = alloc$) > 0% then L51410
                errormsg$ = "Enter 'N', 'A', 'C', or 'Z' for Allocation"&~
                     " Flag."
                testerror$ = errormsg$
                lfac$(73) = hex(81)


L51410
*       Test Taxable Flag
            if tax$ = " " then L52400
            if pos("YN" = tax$) <> 0 then L52400
                if testerror$ = " " then                                 ~
                   errormsg$ = "Enter 'Y' or 'N' for Taxable"
                testerror$ = errormsg$
                lfac$(74) = hex(81)


L52400
*       Test Demand Type
            if demtype$ = " " then L52600
            if demtype$ = "1" or demtype$ = "2" then L52600
                if testerror$ = " " then                                 ~
                errormsg$ = "Planning Demand type must be '1' or '2'"
                lfac$(75) = hex(82)
                testerror$ = errormsg$


L52600
*       Test Priority
            if priority$ = " " then L53000
            if priority$ >= "A" and priority$ <= "Z" then return
                if testerror$ = " " then                                 ~
                errormsg$ = "Enter 'A' - 'Z' for Priority Code."
                lfac$(76) = hex(81)
                testerror$ = errormsg$
                return

L53000
*       Testing for Purchase Order Line Defaults


            return


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program

            end
