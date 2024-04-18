        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   SSS   TTTTT   CCC   M   M   GGG   TTTTT  IIIII  N   N   *~
            *  S        T    C   C  MM MM  G        T      I    NN  N   *~
            *   SSS     T    C      M M M  G GGG    T      I    N N N   *~
            *      S    T    C   C  M   M  G   G    T      I    N  NN   *~
            *   SSS     T     CCC   M   M   GGG     T    IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCMGTIN - This program allows for the maintenance of     *~
            *            Billing & Transfer factors by cost set/part    *~
            *            category.  Also defined is the Inter-company   *~
            *            Cost bucket for the cost set.                  *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11-05-90 ! Original                                 ! JDH *~
            * 08-07-91 ! Added call to ALLFREE.                   ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            billing$(15)8,               /* Billing Factor             */~
            bucket_id$10,                /* Intercompany Cost Bucket ID*/~
            buckets$2, buckets_msg$20,   /* Number of Cost Buckets     */~
            bucket_descrs$(12)20,        /* Cost Bucket Descriptions   */~
            bucket_ids$(12)10,           /* Cost Bucket IDs            */~
            bucket_nrs$(12)2,            /* Bucket Numbers             */~
            cat$(15)4,                   /* Part Category              */~
            catdescr$(15)30,             /* Part Category              */~
            cfac$(14)1,                  /* Part Category FAC          */~
            coname$50,                   /* Company name               */~
            copy_set$8,                  /* Cost Set to copy from      */~
            currset$8, currsetid$4,      /* Current Cost Set           */~
            hdr1$(3)20,                  /* Header for PLOWCODE        */~
            hdr2$(3)20,                  /* Header for Buckets         */~
            hdr3$(6)30,                  /* Header for Factors         */~
            set$8,                       /* Cost Set ID                */~
            set_descr$30,                /* Cost Set ID                */~
            setid$4, setid_msg$20,       /* Internal Set ID            */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$72,                    /* Plowcode variable          */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            ffac$(14)1,                  /* Factors FAC                */~
            frozen$8,                    /* Date Cost Set Frozen       */~
            i$(24)80,                    /* Screen Image               */~
            incl_excl(1),                /* PLOWCODE variable          */~
            incl_excl$(1)1,              /* PLOWCODE variable          */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            mod_date$(15)8,              /* Modification date          */~
            mod_by$(15)3,                /* Modified by                */~
            pf$(3,2)79,                  /* PF Screen Literals         */~
            pfkeys$(2)32,                /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report ID                  */~
            time$8,                      /* Time of day                */~
            transfer$(15)8,              /* Tansfer Factor             */~
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
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
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
            * #01 ! MGTFCTR1 ! Management Factors - Billing & Transfer  *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! CATEGORY ! INVENTORY CATEGORY CODES FILE            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "MGTFCTR1",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen = 19,                      ~
                        alt key 1, keypos = 9,    keylen = 11, dup

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #03, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 100%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            rptid$ = "STC007"
            call "COMPNAME" (02%, coname$, u3%)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "STCMGTIN: " & str(cms2v$,,8)

            call "STCSETID" (1%, #02, currset$, currsetid$)

            hdr2$(1) = "##"
            hdr2$(2) = "Bucket ID"
            hdr2$(3) = "Bucket Description"

            hdr3$(1) = "Cat."
            hdr3$(2) = "Description"
            hdr3$(3) = "  Factor"
            hdr3$(4) = "  Factor"
            hdr3$(5) = " Changed"
            hdr3$(6) = " By"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  3% then gosub copy_set
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

            goto editpg1

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then gosub delete_set
                  if keyhit%  = 16% then       manage_factors
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% = 2% then fieldnr% = 0%
            if fieldnr% = 4% then fieldnr% = 2%
            if fieldnr% < 2% or fieldnr% >  2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11200:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *        M A I N   P R O G R A M   L O G I C                *~
            *-----------------------------------------------------------*~
            *  Screen controls and branching.                           *~
            *************************************************************
        manage_factors
            gosub datasave_bucket    /* First write 'B'ucket record */
            gosub load_first

        screen_loop
            errormsg$ = " "
L11410:     gosub'102(0%)      /* Get what is to be done               */
                if keyhit%  =  2% then gosub load_first
                if keyhit%  =  5% then gosub load_next
                if keyhit%  = 10% then gosub load_stated
                if keyhit%  = 14% then gosub report_printing
                if keyhit%  = 14% then gosub load_first
                if keyhit%  = 16% then       inputmode
                if keyhit% <>  0% then L11410
            errormsg$ = " "
            l% = cursor%(1) - 5%
            if l% <> 1% and frozen$ <> " " then L11410
            if l% <> 1% then L11570
            /* Add -or- Change a specifically mentioned code.          */
                if cat$(1) <> " " then L11560
*                  ERRORMSG$ = "Code may not be blank"
                    goto L11410
L11560:         gosub load_specific
                if errormsg$ <> " " then L11410
L11570:     if l% < 1% or l% > last% then L11410

            gosub'053(l%)      /* Set FACs for line entry              */
L11600:     gosub'102(l%)      /* Get field entries.                   */
                if keyhit%  =  1% then gosub startover2
                if keyhit%  =  8% then gosub delete_code
                if keyhit% <>  0% then L11600
            gosub'152(2%)      /* Edit fields                          */
                if errormsg$ <> " "  then L11600
                gosub save_data
                goto screen_loop

        REM *************************************************************~
            *     M I S C E L L A N E O U S   R O U T I N E S           *~
            *************************************************************

        copy_set
            errormsg$ = " "
            plowkey$ = hex(00)
            hdr1$(1) = " Cost Set ID"
            descr$ = hex(06) & "Choose Cost Set to COPY..."
            incl_excl(1) = 9.01 : incl_excl$(1) = "B"
            call "PLOWCODE" (#01, plowkey$, descr$, 6000%, 0.08, f1%(1), ~
                            hdr1$(), 0.08, 1.0, incl_excl(),             ~
                            incl_excl$(), "D")
            if f1%(1) = 0% then return

            /* First we delete anything that might be out there. */
            call "DELETE" (#01, set$, 8%)

            /* Then we copy the 'B'ucket record. Should only be one. */
            copy_set$ = str(plowkey$, 1, 8)
            plowkey$ = str(copy_set$) & "B" & hex(00)
            call "PLOWNEXT" (#01, plowkey$, 9%, f1%(1))
            if f1%(1) = 0% then L12260
                get #01 using L12200, bucket_id$, id%
L12200:             FMT POS(10), CH(10), BI(1)
                put #01 using L31110, set$, "B", bucket_id$, id%, " ",    ~
                                     userid$, date, " "
                write #01

            /* Last we copy the 'C'ategory records. Need to PLOW these. */
L12260:     plowkey$ = str(copy_set$) & "C" & hex(00)
L12270:     call "PLOWNEXT" (#01, plowkey$, 9%, f1%(1))
            if f1%(1) = 0% then L12360
                get #01 using L12300, cat$, billing, transfer
L12300:              FMT POS(10), CH(4), POS(20), 2*PD(14,4)
                put #01 using L31370, set$, "C", cat$, " ", billing,      ~
                                     transfer, userid$, date, " "
                write #01
            goto L12270

L12360:     frozen$ = " "    /* Act like it's not Frozen if copying. */
            return

        delete_set
L12510:     u3% = 2%
            call "ASKUSER" (u3%, " *** DELETE *** ", "Confirm deletion"& ~
                            " of Cost Set '" & set$ & "' records.", " ", ~
                            "Press PF1 to ABORT deletion; " &            ~
                            "press PF24 to DELETE.")
            if u3% = 1% then return
            if u3% <> 24% then L12510

            plowkey$ = str(set$)
            call "DELETE" (#01, plowkey$, 8%)
            return clear all
            goto inputmode

        clear_first       /* Clear array bucket 1                      */
            init(" ") cat$(1), catdescr$(1), billing$(1), transfer$(1),  ~
                      mod_date$(1), mod_by$(1)
        return

        delete_code
L12810:     errormsg$ = "Category: " & cat$(l%) & " (" & catdescr$(l%) & ~
                        ")"
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "**** DELETE FACTORS ****",        ~
                            errormsg$, " ",                              ~
                            "Press PF1 to ABORT Deletion;" &             ~
                            " press PF24 to DELETE")
            errormsg$ = " "
            if keyhit1%  =  1% then return
            if keyhit1% <> 24% then L12810
                call "DELETE" (#01, str(set$) & "C" & cat$(l%), 13%)
                cat$(1) = cat$(l%)
                gosub screen_align
                return clear
                goto screen_loop

        REM *************************************************************~
            *        S E T   F A C S   F O R   E N T R Y                *~
            *-----------------------------------------------------------*~
            *  Set FACs on for line modification.                       *~
            *************************************************************

        deffn'053(l%)
            ffac$(l%) = hex(80)
          return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Cost Set ID            */~
                              L20200          /* Interco Cost Bkt       */
            return
L20100: REM Def/Enable Cost Set ID                 SET$
            return

L20200: REM Def/Enable Intercompany Cost Bucket ID BUCKET_ID$
            if frozen$ <> " " and bucket_id$ <> " " then enabled% = 0%
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Cost Set ID                                            ",~
         "Enter Intercompany Cost Bucket ID                            "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, setid$, setid_msg$,        ~
                      billing$(), bucket_id$, cat$(), catdescr$(), set$, ~
                      set_descr$, transfer$(), buckets$, buckets_msg$,   ~
                      bucket_ids$(), bucket_descrs$(), bucket_nrs$(),    ~
                      frozen$
            call "ALLFREE"
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
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

        startover2
L29710:     keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "* ABORT CURRENT ENTRY *",         ~
                            "PRESS (1) TO RETURN TO DISPLAY", "- OR -",  ~
                          "PRESS (ENTER) TO ABORT CHANGES MADE TO LINE")

            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29710

            return clear        /* Enter- Abort Changes       */
            if l% <> 1% then L29830
                gosub clear_first
                goto screen_loop
L29830:     cat$(1) = cat$(2)
            gosub load_stated
            goto screen_loop

        REM *************************************************************~
            *           L O A D   D A T A   R O U T I N E S             *~
            *-----------------------------------------------------------*~
            *   Move a screen's worth of data into display arrays.      *~
            *************************************************************

        load_first        /* Load from top of file                     */
            plowkey$ = str(set$) & "C" & hex(00)
            goto load_data

        load_stated       /* Load from operator specified code         */
            plowkey$ = str(set$) & "C" & cat$(1) & hex(00)
            gosub clear_first
            goto load_data

        load_next         /* Load from last code listed                */
            if eof$ <> "YES" then L30190
                errormsg$ = "ALREADY AT END OF FILE."
                return
L30190:     if last% = 1 then load_first
            plowkey$ = str(set$) & "C" & cat$(last%) & hex(00)

        load_data
            errormsg$ = " "
            last% = 1
            eof$  = "NO"
            init (" ") cat$(), catdescr$(), billing$(), transfer$(),     ~
                       mod_date$(), mod_by$()

          plowloop
            call "PLOWNEXT" (#01, plowkey$, 9%, f1%(1))
            if f1%(1) = 1 then L30340
                eof$ = "YES"
                return
L30340:     last% = last% + 1%  :  l% = last%
            gosub get_record   /* Load record and format for display   */
          if l% < 14% then plowloop else return


        load_specific     /* Get specific code from file               */
                          /* If new, supply defaults.                  */
            call "READ100" (#01, str(set$) & "C" & str(cat$(l%)), f1%(1))
            if f1%(1) <> 1 then L30490
                if frozen$ = " " then L30470
                     errormsg$ = "Not allowed to modified frozen data."
                     l% = 0%
                     return
L30470:         gosub get_record
                return
L30490:   /* Not on file-- setup with defaults               */
            catdescr$(l%), billing$(l%), transfer$(l%), mod_date$(l%),   ~
            mod_by$(l%) = " "
          /* and test category code                          */
            gosub'152(1%)
         return


        get_record   /* Get record from buffer and format for display  */
            get #01 using L30690, cat$(l%), billing, transfer,            ~
                                 mod_by$(l%), mod_date$(l%)
            call "CONVERT" (billing, 2.2, billing$(l%))
            call "CONVERT" (transfer, 2.2, transfer$(l%))
            call "DATEFMT" (mod_date$(l%))

               /* Get category description from CATEGORY file. */
            catdescr$(l%) = " * Category no longer on file."
            call "DESCRIBE" (#03, cat$(l%), catdescr$(l%), f1%(3))
        return

L30690:         FMT  XX(09),             /* Logical File ID            */~
                     CH(04),             /* Category                   */~
                     XX(06),             /* Filler                     */~
                     2*PD(14,4),         /* Billing & Transfer Factors */~
                     CH(03),             /* Last changed by            */~
                     CH(06)              /* Last changed date          */


            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        datasave_bucket
            if bucket_id$ = save_id$ then return /*Skip if already done*/
            readkey$ = str(set$) & "B"
            call "DELETE" (#01, readkey$, 9%)
            put #01 using L31110, set$, "B", bucket_id$, id%, " ",        ~
                                 userid$, date, " "
L31110:         FMT CH(8), CH(1), CH(10), BI(1), CH(15), CH(3), CH(6),   ~
                    CH(56)
            write #01
            return

        save_data
           /*  Update file with code maintained (L%). After save, if    */
           /*  Code was on line 1 we redo screen with that code on top. */

            call "READ101" (#01, str(set$) & "C" & str(cat$(l%)), f1%(1))
            put #01 using L31370, set$, "C", cat$(l%), " ", billing,      ~
                                 transfer, userid$, date, " "
            if f1%(1) = 0 then write #01 else rewrite #01

         /* Now make sure that code added/changed appears on screen.   */
         /* Also accessed by DELETE_CODE routine.                      */
         screen_align
            if l%   <> 1 then L31310
            if cat$(l%) > cat$(last%) and last% < 14% then L31310
            if cat$(l%) < cat$(2) or cat$(l%) > cat$(last%) then L31330

L31310:     cat$(1) = cat$(2)      /* Keeps the screen the same  */
                goto L31340
L31330:     cat$(1) = cat$(l%)     /* Put modified code on top   */
L31340:     gosub load_stated      /* Start display at CAT$(1)   */
          return

L31370:         FMT  CH(08),             /* Cost Set ID                */~
                     CH(01),             /* Logical 'C'ategory flag    */~
                     CH(04),             /* Category Code              */~
                     CH(06),             /* Filler                     */~
                     2*PD(14,4),         /* Billing & Transfer Factors */~
                     CH(03),             /* Last changed by            */~
                     CH(06),             /* Last changed date          */~
                     CH(56)              /* Filler                     */

            return

        REM *************************************************************~
            *          R E P O R T   P R I N T I N G                    *~
            *-----------------------------------------------------------*~
            * A report of management factors for a specific cost set.   *~
            *************************************************************

        report_printing
            /* Set up for printing */
            time$ = " " : call "TIME" (time$)
            page% = 0% : line% = 999%
            call "SETPRNT" ("MGTG01", " ", 0%, 0%)
            call "SHOSTAT" ("Printing Report for Cost Set " & set$)
            select printer(134)
            save_l% = l% : l% = 15%     /* 15% used for report only */
            plowkey$ = str(set$) & "C" & hex(00)

            /* Plow thru the file for categories & factors */
        report_loop
            if line% > 54% then gosub print_header
            call "PLOWNEXT" (#01, plowkey$, 9%, f1%(1))
            if f1%(1) = 0% then end_report

            gosub get_record
            print using L32755, cat$(l%), catdescr$(l%), billing$(l%),    ~
                               transfer$(l%), mod_by$(l%), mod_date$(l%)
            line% = line% + 1%
            goto report_loop

        end_report
            l% = save_l%
            call "SETPRNT" ("MGTG01", " ", 0%, 1%)
            close printer
            return

        print_header
            page% = page% + 1%
            print using L32800, date$, coname$, rptid$
            print using L32840, time$, page%
            print using L32870, set$, bucket_id$
            print
            print using L32700
            print using L32730
            line% = 7%
            return

L32700: %  Ctgy   Category Description              Billing   Transfer   ~
        ~Who   Mod Date

L32730: %  ----   ------------------------------   --------   --------   ~
        ~---   --------

L32755: %  ####   ##############################   ########   ########   ~
        ~###   ########

L32800: %DATE: ######## #################################################~
        ~ STCMGTIN:######

L32840: %TIME: ########          MANAGEMENT BILLING AND TRANSFER FACTORS ~
        ~      PAGE: ####

L32870: %              Cost Set: ########  Intercompany Cost Bucket: ####~
        ~######

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              str(line2$,,61) = "Current Cost Set = " & currset$
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40085,         /* Cost Set ID       */   ~
                                L40085          /* Interco Cost Bkt  */
              goto L40100

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40085:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40100:     accept                                                       ~
               at (01,02),                                               ~
                  "Management Factors - Cost Set Information",           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Cost Set ID",                                ~
               at (06,26), fac(lfac$( 1)), set$                 , ch(08),~
               at (06,59), fac(hex(8c))  , setid_msg$           , ch(20),~
                                                                         ~
               at (07,02), "Cost Set Description",                       ~
               at (07,26), fac(hex(8c))  , set_descr$           , ch(30),~
               at (07,59), fac(hex(8c))  , buckets_msg$         , ch(20),~
                                                                         ~
               at (09,02), "Interco. Cost Bucket ID",                    ~
               at (09,26), fac(lfac$( 2)), bucket_id$           , ch(10),~
                                                                         ~
               at (11,05), fac(hex(ac)), hdr2$(1)               , ch(02),~
               at (11,08), fac(hex(ac)), hdr2$(2)               , ch(10),~
               at (11,19), fac(hex(ac)), hdr2$(3)               , ch(20),~
               at (11,43), fac(hex(ac)), hdr2$(1)               , ch(02),~
               at (11,46), fac(hex(ac)), hdr2$(2)               , ch(10),~
               at (11,57), fac(hex(ac)), hdr2$(3)               , ch(20),~
                                                                         ~
               at (12,05), fac(hex(8c)), bucket_nrs$( 1)        , ch(02),~
               at (13,05), fac(hex(8c)), bucket_nrs$( 2)        , ch(02),~
               at (14,05), fac(hex(8c)), bucket_nrs$( 3)        , ch(02),~
               at (15,05), fac(hex(8c)), bucket_nrs$( 4)        , ch(02),~
               at (16,05), fac(hex(8c)), bucket_nrs$( 5)        , ch(02),~
               at (17,05), fac(hex(8c)), bucket_nrs$( 6)        , ch(02),~
               at (12,43), fac(hex(8c)), bucket_nrs$( 7)        , ch(02),~
               at (13,43), fac(hex(8c)), bucket_nrs$( 8)        , ch(02),~
               at (14,43), fac(hex(8c)), bucket_nrs$( 9)        , ch(02),~
               at (15,43), fac(hex(8c)), bucket_nrs$(10)        , ch(02),~
               at (16,43), fac(hex(8c)), bucket_nrs$(11)        , ch(02),~
               at (17,43), fac(hex(8c)), bucket_nrs$(12)        , ch(02),~
                                                                         ~
               at (12,08), fac(hex(8c)),   bucket_ids$( 1)      , ch(10),~
               at (13,08), fac(hex(8c)),   bucket_ids$( 2)      , ch(10),~
               at (14,08), fac(hex(8c)),   bucket_ids$( 3)      , ch(10),~
               at (15,08), fac(hex(8c)),   bucket_ids$( 4)      , ch(10),~
               at (16,08), fac(hex(8c)),   bucket_ids$( 5)      , ch(10),~
               at (17,08), fac(hex(8c)),   bucket_ids$( 6)      , ch(10),~
               at (12,46), fac(hex(8c)),   bucket_ids$( 7)      , ch(10),~
               at (13,46), fac(hex(8c)),   bucket_ids$( 8)      , ch(10),~
               at (14,46), fac(hex(8c)),   bucket_ids$( 9)      , ch(10),~
               at (15,46), fac(hex(8c)),   bucket_ids$(10)      , ch(10),~
               at (16,46), fac(hex(8c)),   bucket_ids$(11)      , ch(10),~
               at (17,46), fac(hex(8c)),   bucket_ids$(12)      , ch(10),~
                                                                         ~
               at (12,19), fac(hex(8c)),   bucket_descrs$( 1)   , ch(20),~
               at (13,19), fac(hex(8c)),   bucket_descrs$( 2)   , ch(20),~
               at (14,19), fac(hex(8c)),   bucket_descrs$( 3)   , ch(20),~
               at (15,19), fac(hex(8c)),   bucket_descrs$( 4)   , ch(20),~
               at (16,19), fac(hex(8c)),   bucket_descrs$( 5)   , ch(20),~
               at (17,19), fac(hex(8c)),   bucket_descrs$( 6)   , ch(20),~
               at (12,57), fac(hex(8c)),   bucket_descrs$( 7)   , ch(20),~
               at (13,57), fac(hex(8c)),   bucket_descrs$( 8)   , ch(20),~
               at (14,57), fac(hex(8c)),   bucket_descrs$( 9)   , ch(20),~
               at (15,57), fac(hex(8c)),   bucket_descrs$(10)   , ch(20),~
               at (16,57), fac(hex(8c)),   bucket_descrs$(11)   , ch(20),~
               at (17,57), fac(hex(8c)),   bucket_descrs$(12)   , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1,1)             , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2,1)             , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3,1)             , ch(79),~
                                                                         ~
               keys(pfkeys$(1)), key(keyhit%)

               if keyhit% <> 13% then L40475
                  call "MANUAL" ("STCMGTIN") : goto L40100

L40475:        if keyhit% <> 15% then L40490
                  call "PRNTSCRN" : goto L40100

L40490:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40590     /*  Input Mode             */
            pf$(1,1) = "(1)Start Over    (3)Copy from Another Se" &      ~
                       "t                      (13)Instructions"
            pf$(2,1) = "                 (4)Previous Field      " &      ~
                       "                       (15)Print Screen"
            pf$(3,1) = "                                        " &      ~
                       "                       (16)Exit Program"
            pfkeys$(1) = hex(01ff0304ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40565
                str(pf$(3,1),64)    = " " : str(pfkeys$(1),16,1) = hex(ff)
L40565:     if fieldnr% > 1% then L40580
                str(pf$(2,1),18,26) = " " : str(pfkeys$(1), 4,1) = hex(ff)
                str(pf$(1,1),18,24) = " " : str(pfkeys$(1), 3,1) = hex(ff)
L40580:     return

L40590: if fieldnr% > 0% then L40635  /*  Edit Mode - Select Fld */
            pf$(1,1) = "(1)Start Over                           " &      ~
                       "     (8)Delete Set     (13)Instructions"
            pf$(2,1) = "                                        " &      ~
                       "                       (15)Print Screen"
            pf$(3,1) = "                                        " &      ~
                       "                       (16)Edit Factors"
            pfkeys$(1) = hex(01ffffffffffff08ffffffff0dff0f1000)
            return
L40635:                              /*  Edit Mode - Enabled    */
            pf$(1,1) = "(1)Start Over                           " &      ~
                       "                       (13)Instructions"
            pf$(2,1) = "                                        " &      ~
                       "                       (15)Print Screen"
            pf$(3,1) = "                                        " &      ~
                       "                                       "
            pfkeys$(1) = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *        D A T A   E N T R Y   S C R E E N                  *~
            *-----------------------------------------------------------*~
            *  Combination Display and Manage Screen.                   *~
            *                                                           *~
            *  There are essentially 2 entry points --                  *~
            *     line =  0 - Get from operator what to do. The code    *~
            *                 field on the 'wild line' is the only      *~
            *                 modifiable field.                         *~
            *     line <> 0 - Get entries for the line.                 *~
            *************************************************************
        deffn'102(l%)
            if l% <> 0% then L41240

          /* Case 1. Get what is to be done.                           */
            init (hex(84)) cfac$()  :  cfac$(1) = hex(81) /* Code FAC  */
            init (hex(8c)) ffac$()                        /* Line FAC  */
            mode% = 1%
            inpmessage$ = "Enter Code to Add/Changed; Blank to Find" &   ~
                          " Existing Code; '?' for Category List"
            if frozen$ = " " then L41290
                inpmessage$ = "Enter Code to Add; or '?' to Select " &   ~
                          "from Category List"
                goto L41290
L41240:   /* Case 2. Modify line L%.                                   */
            init (hex(8c)) cfac$() : cfac$(l%) = hex(a4)
            inpmessage$ = "Modify field(s) then press (ENTER) to save."
            mode% = 2%

L41290:     str(line2$,,61) = "Standard Cost Set: " & set$ & "  (" &     ~
                              set_descr$ & ")"
            gosub set_pf2

L41330:     accept                                                       ~
               at (01,02),                                               ~
                  "Management Factors - Billing & Transfer Factors",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Part",                                       ~
               at (04,08), "Category",                                   ~
               at (04,41), " Billing",                                   ~
               at (04,51), "Transfer",                                   ~
               at (04,62), "    Last",                                   ~
               at (04,72), " ",                                          ~
                                                                         ~
               at (05,02), fac(hex(ac)),   hdr3$(1)             , ch(04),~
               at (05,08), fac(hex(ac)),   hdr3$(2)             , ch(30),~
               at (05,41), fac(hex(ac)),   hdr3$(3)             , ch(08),~
               at (05,51), fac(hex(ac)),   hdr3$(4)             , ch(08),~
               at (05,62), fac(hex(ac)),   hdr3$(5)             , ch(08),~
               at (05,72), fac(hex(ac)),   hdr3$(6)             , ch(03),~
                                                                         ~
               at (06,02), fac(cfac$( 1)), cat$( 1)             , ch(04),~
               at (07,02), fac(cfac$( 2)), cat$( 2)             , ch(04),~
               at (08,02), fac(cfac$( 3)), cat$( 3)             , ch(04),~
               at (09,02), fac(cfac$( 4)), cat$( 4)             , ch(04),~
               at (10,02), fac(cfac$( 5)), cat$( 5)             , ch(04),~
               at (11,02), fac(cfac$( 6)), cat$( 6)             , ch(04),~
               at (12,02), fac(cfac$( 7)), cat$( 7)             , ch(04),~
               at (13,02), fac(cfac$( 8)), cat$( 8)             , ch(04),~
               at (14,02), fac(cfac$( 9)), cat$( 9)             , ch(04),~
               at (15,02), fac(cfac$(10)), cat$(10)             , ch(04),~
               at (16,02), fac(cfac$(11)), cat$(11)             , ch(04),~
               at (17,02), fac(cfac$(12)), cat$(12)             , ch(04),~
               at (18,02), fac(cfac$(13)), cat$(13)             , ch(04),~
               at (19,02), fac(cfac$(14)), cat$(14)             , ch(04),~
                                                                         ~
               at (06,08), fac(hex(8c)),   catdescr$( 1)        , ch(30),~
               at (07,08), fac(hex(8c)),   catdescr$( 2)        , ch(30),~
               at (08,08), fac(hex(8c)),   catdescr$( 3)        , ch(30),~
               at (09,08), fac(hex(8c)),   catdescr$( 4)        , ch(30),~
               at (10,08), fac(hex(8c)),   catdescr$( 5)        , ch(30),~
               at (11,08), fac(hex(8c)),   catdescr$( 6)        , ch(30),~
               at (12,08), fac(hex(8c)),   catdescr$( 7)        , ch(30),~
               at (13,08), fac(hex(8c)),   catdescr$( 8)        , ch(30),~
               at (14,08), fac(hex(8c)),   catdescr$( 9)        , ch(30),~
               at (15,08), fac(hex(8c)),   catdescr$(10)        , ch(30),~
               at (16,08), fac(hex(8c)),   catdescr$(11)        , ch(30),~
               at (17,08), fac(hex(8c)),   catdescr$(12)        , ch(30),~
               at (18,08), fac(hex(8c)),   catdescr$(13)        , ch(30),~
               at (19,08), fac(hex(8c)),   catdescr$(14)        , ch(30),~
                                                                         ~
               at (06,41), fac(ffac$( 1)), billing$( 1)         , ch(08),~
               at (07,41), fac(ffac$( 2)), billing$( 2)         , ch(08),~
               at (08,41), fac(ffac$( 3)), billing$( 3)         , ch(08),~
               at (09,41), fac(ffac$( 4)), billing$( 4)         , ch(08),~
               at (10,41), fac(ffac$( 5)), billing$( 5)         , ch(08),~
               at (11,41), fac(ffac$( 6)), billing$( 6)         , ch(08),~
               at (12,41), fac(ffac$( 7)), billing$( 7)         , ch(08),~
               at (13,41), fac(ffac$( 8)), billing$( 8)         , ch(08),~
               at (14,41), fac(ffac$( 9)), billing$( 9)         , ch(08),~
               at (15,41), fac(ffac$(10)), billing$(10)         , ch(08),~
               at (16,41), fac(ffac$(11)), billing$(11)         , ch(08),~
               at (17,41), fac(ffac$(12)), billing$(12)         , ch(08),~
               at (18,41), fac(ffac$(13)), billing$(13)         , ch(08),~
               at (19,41), fac(ffac$(14)), billing$(14)         , ch(08),~
                                                                         ~
               at (06,51), fac(ffac$( 1)), transfer$( 1)        , ch(08),~
               at (07,51), fac(ffac$( 2)), transfer$( 2)        , ch(08),~
               at (08,51), fac(ffac$( 3)), transfer$( 3)        , ch(08),~
               at (09,51), fac(ffac$( 4)), transfer$( 4)        , ch(08),~
               at (10,51), fac(ffac$( 5)), transfer$( 5)        , ch(08),~
               at (11,51), fac(ffac$( 6)), transfer$( 6)        , ch(08),~
               at (12,51), fac(ffac$( 7)), transfer$( 7)        , ch(08),~
               at (13,51), fac(ffac$( 8)), transfer$( 8)        , ch(08),~
               at (14,51), fac(ffac$( 9)), transfer$( 9)        , ch(08),~
               at (15,51), fac(ffac$(10)), transfer$(10)        , ch(08),~
               at (16,51), fac(ffac$(11)), transfer$(11)        , ch(08),~
               at (17,51), fac(ffac$(12)), transfer$(12)        , ch(08),~
               at (18,51), fac(ffac$(13)), transfer$(13)        , ch(08),~
               at (19,51), fac(ffac$(14)), transfer$(14)        , ch(08),~
                                                                         ~
               at (06,62), fac(hex(8c)),   mod_date$( 1)        , ch(08),~
               at (07,62), fac(hex(8c)),   mod_date$( 2)        , ch(08),~
               at (08,62), fac(hex(8c)),   mod_date$( 3)        , ch(08),~
               at (09,62), fac(hex(8c)),   mod_date$( 4)        , ch(08),~
               at (10,62), fac(hex(8c)),   mod_date$( 5)        , ch(08),~
               at (11,62), fac(hex(8c)),   mod_date$( 6)        , ch(08),~
               at (12,62), fac(hex(8c)),   mod_date$( 7)        , ch(08),~
               at (13,62), fac(hex(8c)),   mod_date$( 8)        , ch(08),~
               at (14,62), fac(hex(8c)),   mod_date$( 9)        , ch(08),~
               at (15,62), fac(hex(8c)),   mod_date$(10)        , ch(08),~
               at (16,62), fac(hex(8c)),   mod_date$(11)        , ch(08),~
               at (17,62), fac(hex(8c)),   mod_date$(12)        , ch(08),~
               at (18,62), fac(hex(8c)),   mod_date$(13)        , ch(08),~
               at (19,62), fac(hex(8c)),   mod_date$(14)        , ch(08),~
                                                                         ~
               at (06,72), fac(hex(8c)),   mod_by$( 1)          , ch(03),~
               at (07,72), fac(hex(8c)),   mod_by$( 2)          , ch(03),~
               at (08,72), fac(hex(8c)),   mod_by$( 3)          , ch(03),~
               at (09,72), fac(hex(8c)),   mod_by$( 4)          , ch(03),~
               at (10,72), fac(hex(8c)),   mod_by$( 5)          , ch(03),~
               at (11,72), fac(hex(8c)),   mod_by$( 6)          , ch(03),~
               at (12,72), fac(hex(8c)),   mod_by$( 7)          , ch(03),~
               at (13,72), fac(hex(8c)),   mod_by$( 8)          , ch(03),~
               at (14,72), fac(hex(8c)),   mod_by$( 9)          , ch(03),~
               at (15,72), fac(hex(8c)),   mod_by$(10)          , ch(03),~
               at (16,72), fac(hex(8c)),   mod_by$(11)          , ch(03),~
               at (17,72), fac(hex(8c)),   mod_by$(12)          , ch(03),~
               at (18,72), fac(hex(8c)),   mod_by$(13)          , ch(03),~
               at (19,72), fac(hex(8c)),   mod_by$(14)          , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1, mode%)        , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2, mode%)        , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3, mode%)        , ch(79),~
                                                                         ~
               keys(pfkeys$(mode%)), key(keyhit%)

               if keyhit% <> 13% then L42550
                  call "MANUAL" ("STCMGTIN") : goto L41330

L42550:        if keyhit% <> 15% then L42580
                  call "PRNTSCRN" : goto L41330

L42580:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

               if keyhit% <> 0% then return
                if mode% <> 1% then return
                if cat$(1) <> " " then return
                if cursor%(1) - 5% <> 1% then return
                if frozen$ <> " " then return
                gcode$ = str(set$) & "C" & cat$(1)
                call "PLOWCODE" (#01, gcode$, " ", 9%, 0.0, f1%(1))
                if f1%(1) = 0% then L41330
                cat$(1) = str(gcode$,10)
                return

        set_pf2

            pf$(1,1) = "(ENTER)Manage code typed or at cursor   " &      ~
                       "                       (13)Instructions"
            pf$(2,1) = "(2)FIRST Screen        (10)MOVE Screen T" &      ~
                       "o Entry Typed          (15)Print Screen"
            pf$(3,1) = "(5)NEXT Screen         (14)Print Codes  " &      ~
                       "                       (16)EXIT        "

            pf$(1,2) = "(ENTER)SAVE DATA                        " &      ~
                       "                       (13)Instructions"
            pf$(2,2) = "(1)Start Line Over                (8)DEL" &      ~
                       "ETE                    (15)Print Screen"
            pf$(3,2) = "                                        " &      ~
                       "                                       "

            pfkeys$(1) = hex(ff02ffff05ffffffff0affff0d0e0f1000)
            pfkeys$(2) = hex(01ffffffffffff08ffffffff0dff0fff00)

            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Cost Set ID            */~
                              L50700          /* Interco Cost Bkt       */
            return
L50100: REM Test for Cost Set ID                  SET$
            plowkey$   = "STC.HDR." & set$
            set_descr$ = hex(06) & "Select Cost Set"
            call "PLOWCODE" (#02, plowkey$, set_descr$, 8%, 0.30, onfile%)
            set$ = str(plowkey$, 9, 8)
            if onfile% = 1% then L50200
                errormsg$ = "Cost Set ID is NOT valid."
                if set$ = " " then errormsg$ = hex(00)
                set_descr$ = " "
                return
L50200:     if set$ = " " then set$ = str(plowkey$,9)
            readkey$ = "STC.HDR." & set$
            call "READ100" (#02, readkey$, onfile%)
            get #02 using L50250, set$, set_descr$, setid$, buckets%,     ~
                                 bucket_ids$(),bucket_descrs$(), frozen$
L50250:         FMT POS(9), CH(8), POS(21), CH(30), CH(4), XX(4), BI(1), ~
                    12*CH(10), 12*CH(20), XX(13), CH(6)
            call "DATEFMT" (frozen$)
            setid_msg$ = "Internal ID = " & setid$
            convert buckets% to str(buckets_msg$,,2), pic(#0)
            buckets$ = buckets_msg$
            buckets_msg$ = buckets_msg$ & " Buckets Defined"
            for b% = 1% to buckets%
                convert b% to str(bucket_nrs$(b%)), pic(#0)
            next b%
            if frozen$ = " " then L50410
                u3% = 2%
                call "ASKUSER" (u3%, " *** FROZEN COST SET *** ", set$ & ~
                                " was frozen on " & frozen$ & ".",       ~
                                "Data may be added, but NOT modified.",  ~
                                "Press RETURN to continue")
L50410:     plowkey$ = str(set$) & "B" & hex(00)
            save_id$ = " "
            call "PLOWNEXT" (#01, plowkey$, 9%, f1%(1))
            if f1%(1) = 0% then return
                get #01 using L50460, bucket_id$, id%
L50460:              FMT POS(10), CH(10), BI(1)
                save_id$ = bucket_id$
                return clear all
                goto editpg1

L50700: REM Test for Intercompany Cost Bucket ID  BUCKET_ID$
            bucket_there% = 0%
            for b% = 1% to buckets%
                if bucket_id$ <> bucket_ids$(b%) then L50750
                     bucket_there% = 1%
                     id% = b%
L50750:     next b%
            if bucket_there% = 1% then return
                errormsg$ = "Pick a valid Bucket ID (see below)."
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51100,         /* Part Category          */~
                              L51200          /* Billing & Transfer Fctr*/
            return

L51100: REM Test for Part Category                CAT$
            call "GETCODE" (#03, cat$(l%), catdescr$(l%), 0%, 0, f1%(3))
            if f1%(3) <> 0% then L51140
                errormsg$ = "Not a valid Category Code."
            return
L51140:     if frozen$ = " " then return
            /* Frozen cost set so we must test if category is new */
                readkey$ = str(set$) & "C" & str(cat$(l%))
                call "READ100" (#01, readkey$, f1%(1))
                if f1%(1) = 0% then return
                errormsg$ = "Can't modify an existing category for a " & ~
                            " frozen cost set."
            return

L51200: REM Test for Billing & Transfer Factors   BILLING$ & TRANSFER
            call "NUMTEST" (billing$(l%), .01, 9.99, errormsg$, -2.2,    ~
                            billing)
            if errormsg$ <> " " then return
            call "NUMTEST" (transfer$(l%), 0, 9.99, errormsg$, -2.2,     ~
                            transfer)
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
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
