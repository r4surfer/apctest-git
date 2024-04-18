        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP    OOO   V   V  RRRR   EEEEE  L       SSS   BBBB    *~
            *  P   P  O   O  V   V  R   R  E      L      S      B   B   *~
            *  PPPP   O   O  V   V  RRRR   EEEE   L       SSS   BBBB    *~
            *  P      O   O   V V   R   R  E      L          S  B   B   *~
            *  P       OOO     V    R   R  EEEEE  LLLLL   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * POVRELSB - Provides user with ability to create Vendor    *~
            *            Purchase Directives (records in the PORLSE     *~
            *            file) from either Vendor Service Advices (from *~
            *            the VBKVSA file) or from User manual entry of  *~
            *            a Vendor Service Purchase Directive.           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/21/94 ! Original                                 ! LDJ *~
            * 11/23/94 ! Now Looks for Default Contract ID value. ! LDJ *~
            * 12/01/94 ! Corrected std costs and job fields when  ! LDJ *~
            *          ! writing to PORLSE.                       !     *~
            * 07/19/96 ! Changes for the year 2000.               ! DXL *~
            * 07/18/97 ! Tag No back to YYMMDD format             ! DER *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "POVRELSB" (                                                 ~
            #01, /* SYSFILE2  Caelus Management System Information     */~
            #02, /* PORLSE    Purchase Order Requisitions file         */~
            #03, /* VENDOR    Vendor Master Record                     */~
            #06, /* CURMASTR  Currency Master file                     */~
            #07, /* GENCODES  System General Codes file.               */~
            #10, /* VPCMASTR  Vendor Purchases Contract Master File    */~
            #11, /* VENPRICE  Vendor current prices - all vendors, all */~
            #12, /* HNYMASTR  Inventory Master File                    */~
            #13) /* HNYPROC   Inventory Procurement History File       */
        dim                                                              ~
            activity$4,                  /* Vendor Service Activity    */~
            activitydescr$32,            /* Vendor Service Activity    */~
            adj_flag$1,                  /* Automatic Adjustment Flag  */~
            advice_nbr$8,                /* Vendor Service Advice Nbr  */~
            alt$(100)1,                  /* Alternate buyer flags      */~
            basedate$8,                  /* Planning Base date         */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bucket_ids$(12)10,           /* Bucket IDs                 */~
            buyer$3,                     /* Buyer Code                 */~
            codes$(100)3,                /* Part/Buyer release codes   */~
            comments$40,                 /* Note / Comments            */~
            contract$20,                 /* Purchase Contract Number   */~
            contractdescr$32,            /* Purchase Contract Number   */~
            cost_bucket$2,               /* Posts to Cost Bucket #     */~
            cost_bucketdescr$30,         /* Posts to Cost Bucket #     */~
            currency$4,                  /* Currency Code              */~
            currencydescr$32,            /* Currency Code              */~
            curr_msg$13,                 /* Screen message for currency*/~
            curr_msg2$21,                /* Screen message for currency*/~
            curr_on_flag$1,              /* Multi-currency on? Y or N  */~
            curr_table$1,                /* Exchange rate table        */~
            curr_vend$4,                 /* Vendor Currency Code       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            date_due$10,                 /* Due Date                   */~
            date_order$10,               /* Order Date                 */~
            descr_map(40),               /* Plowcode Arg               */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            filler$150,                  /* Record Filler Area         */~
            hdr$(3)132,                  /* Plowcode Arg               */~
            i$(24)80,                    /* Screen Image               */~
            incl_excl(101),              /* Plowcode Arg               */~
            incl_excl$(101)3,            /* Plowcode Arg               */~
            inpmessage$79,               /* Informational Message      */~
            jobnr$8,                     /* For Job                    */~
            jobnrdescr$32,               /* For Job                    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            msg$(12)79,                  /* Menu Pick Messages         */~
            part$25,                     /* Dummy Part Code            */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            piptag$19,                   /* PIP Tag field in PORLSE    */~
            pk$2,                        /* Menu Pick                  */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            po$16,                       /* Purchase Order Number      */~
            po_line$3,                   /* Purchase Order Number Line */~
            price$15,                    /* Vendor Unit Price          */~
            qty$15,                      /* Number of Units of Work    */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            seq$3,                       /* Sequence Number            */~
            set$8, setid$4,              /* STC Data                   */~
            stat$4,                      /* Statutory Currency Code    */~
            status$1,                    /* Status Indicator           */~
            statusdescr$32,              /* Status Indicator           */~
            std_costs$96,                /* Std Cost Buckets           */~
            step$4,                      /* Route Step                 */~
            tagdate$6,                   /* Tag No date, YYMMDD used   */~
            tagdtfull$8,                 /* Tag No date, CCYYMMDD      */~
            tagdttmp$8,                  /* temp/scratch date          */~
            temp_date$8,                 /* Working Storage Date       */~
            uom$4,                       /* Unit of Work Measure Code  */~
            uomdescr$32,                 /* Unit of Work Measure Code  */~
            userid$3,                    /* Current User Id            */~
            vendor$9,                    /* Vendor                     */~
            vendordescr$32,              /* Vendor                     */~
            vend_part$25,                /* Vendor Part Number         */~
            wc$4                         /* Work Center ID             */

        dim f2%(16),                     /* = 0 if the file is open    */~
            f1%(16),                     /* = 1 if READ was successful */~
            fs%(16),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(16)20                  /* Text from file opening     */

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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! PORLSE   ! Purchase Order Requisitions file         *~
            * #03 ! VENDOR   ! Vendor Master Record                     *~
            * #04 ! VBKVSA   ! Vendor Service Advices                   *~
            * #05 ! HNYACTXF ! HNY, WC Activity Cross Reference         *~
            * #06 ! CURMASTR ! Currency Master file                     *~
            * #07 ! GENCODES ! System General Codes file.               *~
            * #08 ! PORELUSR ! User Buy order Release Cross Reference   *~
            * #09 ! JBMASTR2 ! Production job master file               *~
            * #10 ! VPCMASTR ! Vendor Purchases Contract Master File    *~
            * #11 ! VENPRICE ! Vendor current prices - all vendors, all *~
            * #12 ! HNYMASTR ! Inventory Master File                    *~
            * #13 ! HNYPROC  ! Inventory Procurement History File       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #04, "VBKVSA",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    5, keylen =   8,                     ~
                        alt key  1, keypos =    1, keylen =  12,         ~
                            key  2, keypos =    2, keylen =  11,         ~
                            key  3, keypos =   13, keylen =  12, dup,    ~
                            key  4, keypos =   29, keylen =   6, dup,    ~
                            key  5, keypos =   41, keylen =  13, dup,    ~
                            key  6, keypos =   50, keylen =   4, dup

            select #05, "HNYACTXF",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    1, keylen =  29,                     ~
                        alt key  1, keypos =   26, keylen =   4, dup

            select #08, "PORELUSR",                                      ~
                          varc, indexed, recsize =   70,                 ~
                          keypos =    1, keylen =   6,                   ~
                          alt key 1, keypos =  4, keylen = 6

            select #09, "JBMASTR2",                                      ~
                        varc, indexed, recsize = 1300,                   ~
                        keypos = 1, keylen =  8

            call "OPENCHCK" (#04, fs%(4%), f2%(4%), 1%, rslt$(4%))
            call "OPENCHCK" (#05, fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (#08, fs%(8%), f2%(8%), 0%, rslt$(8%))
            call "OPENCHCK" (#09, fs%(9%), f2%(9%), 0%, rslt$(9%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            tagdttmp$ = date
            call "DATEFMT" ( tagdttmp$, 0%, tagdtfull$ )
            tagdate$ = str( tagdtfull$, 3%, 6% )


            call "VBKSWTCH" ("VSA ACTV", temp$, temp, u3%)
            if temp$ <> "Y" then exit_program

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            line2$ ="Review Vendor Service Advices / Create Purchase " & ~
                    "Directives"
            str(line2$,62%) = "POVRELSB: " & str(cms2v$,,8%)

            REM See if operator is an administrator or not
            call "CMSMACHK" ("VBK", lfac$(1%), lfac$(2%))
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then admok% = 1%
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then L09420

            REM Load Up His Release Codes For Cross Checking...
            readkey$ = all(hex(00))
            call "PLOWNEXT" (#08, readkey$, 0%, f1%(8%))
                if f1%(8%) = 0% then L09420  /* not using feature */

            c%, admin% = 0% : codes$() = " "
            readkey$ = str(userid$) & hex(00000000)
L09260:     call "PLOWNEXT" (#08, readkey$, 3%, f1%(8%))
                if f1%(8%) = 0% then L09340
            c% = c% + 1%
            if c% >= 100% then L09340
            get #08, using L09310, codes$(c%), alt$(c%)
L09310:     FMT XX(3), CH(3), POS(40), CH(1)
            goto L09260

L09340:     REM What did we get?
            if codes$() <> " " then L09400
                call "ASKUSER" (u3%,"Sorry", "You are not listed as a" & ~
                " valid Buyer", "therefore you may not create" &         ~
                " Purchasing Directives", "Press (ENTER/RETURN) to exit.")
                goto exit_program
L09400:     if codes$() <> "ALL" then L09450

L09420:     REM This guy can do as he pleases...
            admin% = 1

L09450:     REM Check if UOMs defined
            plowkey$ = all(hex(00))              /* If UOM file set up */
            str(plowkey$,10%) = "UOM"            /* then edit entries. */
            call "READ100" (#07, plowkey$, uom%)

*        Check for Multi-Currency
            curr_on_flag$ = "N" : stat$, curr_msg$, curr_msg2$ = " "
            readkey$ = "SWITCHS.CUR"
            call "READ100" (#01, readkey$, f1%(1%))
            if f1%(1%) <> 0% then get #01 using L09550, curr_on_flag$,     ~
                                                      stat$, curr_table$
L09550:         FMT POS(21), CH(1), CH(4), POS(28), CH(1)
            if curr_on_flag$ <> "Y" then L09620
                call "OPENCHCK" (#06, fs%(6%), f2%(6%), 0%, rslt$(6%))
                curr_msg$ = "Currency Code"
                curr_msg2$ = "(Always in Statutory)"

L09620
*        Set Menu Screen Prompts
            msg$(1%)="ENTER  - Manually create a new Vendor Service Advic~
        ~e and Purchase Directive  "
            msg$(3%)="PF (3) - See Open Vendor Service Advices by Order B~
        ~y Date                    "
            msg$(4%)="PF (4) - See Open Vendor Service Advices by Activit~
        ~y Code                    "
            msg$(5%)="PF (5) - See Open Vendor Service Advices by Default~
        ~ Vendor Code              "
            msg$(6%)="PF (6) - See Open Vendor Service Advices by Advice ~
        ~Number                    "
            msg$(7%)="PF (7) - See Open Vendor Service Advices by Job    ~
        ~                          "
            msg$(8%)="PF (8) - See Vendor Service Advices Released to a P~
        ~urchase Directive         "
            msg$(9%)="PF (9) - See Vendor Service Advices Assigned to a P~
        ~urchase Order             "
            msg$(10%)="PF(10) - See ALL Vendor Service Advices (Open, Can~
        ~celled, Released)         "
            msg$(11%)="PF(14) - Review Procurement History file"


            altb%=1%      /* Associated buyer flag.  If set to 0%, only */
                          /* MAIN buyer codes' stuff appears.  If set   */
                          /* to 1%, then alternate buyers stuff appears */
                          /* IN ADDITION to main buyers'. (see MSG$(2)) */
            gosub view_buyer_switch

*          Get Planning Calendar Base Date for Date Validation later
            call "READ100" (#01, "MONTHS OPEN", f1%(1%))
                if f1%(1%) = 1% then L10030
                call "ASKUSER" (0%, "Sorry",                             ~
                            "Can't Find Months Open record in SYSFILE2.",~
                            "Press (ENTER/RETURN) To Exit.", " ")
                goto exit_program
L10030:     get #01, using L10040, basedate$
L10040:         FMT POS(33), CH(6)

            REM Get today in terms of planning calendar...
                call "DATE" addr("G-", basedate$, date, today%, err%)
                today% = today% + 1

*        Get data required to edit and Describe Cost Bucket input.
            buckets% = 2%      /* function requested - return # buckets */
            call "STCSETID" (buckets%, #1, set$, setid$, bucket_ids$())
            if buckets% = 0% then                                        ~
                bucket_ids$(1%) = "No StdCost"

*        Init Std Costs Buckets to 0
            init (hex(00)) std_costs$

*        If no Contracts on file Turn File Open flag off
            plowkey$ = hex(00)
            call "PLOWNEXT" (#10, plowkey$, 0%, fs%(10%))

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            gosub initialize_variables
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  0% then gosub enter_new_vsa
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub view_buyer_switch
                  if keyhit%  =  3% then gosub view_by_date
                  if keyhit%  =  4% then gosub view_by_activity
                  if keyhit%  =  5% then gosub view_by_vendor
                  if keyhit%  =  6% then gosub view_by_advice
                  if keyhit%  =  7% then gosub view_by_job
                  if keyhit%  =  8% then gosub view_released
                  if keyhit%  =  9% then gosub view_assigned
                  if keyhit%  = 10% then gosub view_all
                  if keyhit%  = 14% then gosub view_procurement_history
                  if keyhit%  = 16% then       exit_program
            goto editpg1

        edit_vsa
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       release_advice
                  if keyhit%  =  3% then       cancel_advice
                  if keyhit%  =  4% then       editpg1
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       edit_vsa
L11430:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 15% then edit_vsa
            if fieldnr% = lastfieldnr% then    edit_vsa
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       edit_vsa
L11480:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11480
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11480
                  lastfieldnr% = fieldnr%
            goto L11430

        REM *************************************************************~
            *       I N P U T   M O D E   N E W   E N T R Y             *~
            *-----------------------------------------------------------*~
            * Handles input of of new Purchase Directive.               *~
            *************************************************************
        enter_new_vsa
            gosub initialize_variables
            status$ = "O" : statusdescr$ = "(New Vendor Service Advice)"
            f1%(4%) = 0%  /* lets routines know not editing existing rec*/
            gosub enter_vsa_pd_info
            return

        enter_vsa_pd_info
            edit% = 1%
            for fieldnr% = 1% to 15%
L11650:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L11770
L11670:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L11750
L11700:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L11670
                         if fieldnr% = 1% then L11650
                         goto L11700
L11750:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L11670
L11770:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L11670
            next fieldnr%
            goto edit_vsa

        view_buyer_switch
            altb% =      /* Associated buyer flag.  If set to 0%, only */~
             abs(altb% - /* MAIN buyer codes' stuff appears.  If set   */~
                 1%)     /* to 1%, then alternate buyers stuff appears */
                         /* IN ADDITION to main buyers'. (see MSG$(2)) */

            if altb% = 0% then                                           ~
            msg$(2%)="PF (2) - View Vendor Service Advices under MAIN &  ~
        ~& ALTERNATE Buyer Codes   "                                      ~
            else                                                         ~
            msg$(2%)="PF (2) - View Vendor Service Advices under MAIN Buy~
        ~er Codes Only"
*          Set the Buyer Include/Exclude Codes if Appropriate           *
            mat incl_excl = zer
            if admok% = 1% or admin% = 1% then return
            x% = 2%
            for c% = 1% to dim(codes$(),1)
                if codes$(c%) <> " " then L12020
                     c% = dim(codes$(),1)
                     goto L12050
L12020:         if altb% = 0% and alt$(c%) = "Y" then L12050
                incl_excl (x%) = 2.03 : incl_excl$(x%) = codes$(c%)
                x% = x% + 1%
L12050:     next c%
            incl_excl(x%) = 2.3 : incl_excl$(x%) = userid$
            return

        view_by_date
            key_desclen = 4.132
            plowkey$ = " "
            gosub set_open_edit_stuff
            str(hdr$(3%),40%) = "(by Order Date)"
            gosub view_select_vsas
            if f1%(4%) = 1% then gosub enter_vsa_pd_info
            return

        view_by_activity
            key_desclen = 6.132
            plowkey$ = activity$
            gosub set_open_edit_stuff
            str(hdr$(3%),40%) = "(by Activity Code)"
            gosub view_select_vsas
            if f1%(4%) = 1% then gosub enter_vsa_pd_info
            return

        view_by_vendor
            key_desclen = 5.132
            plowkey$ = hex(00)
            gosub set_open_edit_stuff
            str(hdr$(3%),40%) = "(by Vendor Code)"
            gosub view_select_vsas
            if f1%(4%) = 1% then gosub enter_vsa_pd_info
            return

        view_by_advice
            key_desclen = 0.132
            plowkey$ = " "
            gosub set_open_edit_stuff
            str(hdr$(3%),40%) = "(by Advice Number)"
            gosub view_select_vsas
            if f1%(4%) = 1% then gosub enter_vsa_pd_info
            return

        view_by_job
            key_desclen = 5.132
            plowkey$ = " "
            gosub set_open_edit_stuff
            str(hdr$(3%),40%) = "(by Job)"
            gosub view_select_vsas
            if f1%(4%) = 1% then gosub enter_vsa_pd_info
            return

        view_released
            key_desclen = 1.132
            plowkey$ = "R"
            break% = 9001%
            hdr$(3%) = hex(ac) & "Review Of Vendor Service Advices "     ~
                               & "Released to a Purchase Directive"
            gosub set_view_only_stuff
            if f1%(4%) = 1% then gosub edit_vsa
            return

        view_assigned
            key_desclen = 1.132
            plowkey$ = "P"
            break% = 9001%
            hdr$(3%) = hex(ac) & "Review Of Vendor Service Advices "     ~
                               & "Assigned to a Purchase Order"
            gosub set_view_only_stuff
            if f1%(4%) = 1% then gosub edit_vsa
            return

        view_all
            key_desclen = 0.132
            plowkey$ = " "
            break% = 9000%
            hdr$(3%) = hex(ac) & "Review Of ALL Vendor Service Advices " ~
                               & "Regardless of Status"
            gosub set_view_only_stuff
            if f1%(4%) = 1% then gosub edit_vsa
            return

        set_open_edit_stuff
            inpmessage$ = hex(06) & "Position Cursor and Press ENTER to "~
                        & "Process Selected Vendor Service Advice"
            hdr$(3%) = hex(ac) & "Review Of Open Vendor Service Advices "
            break% = 9000%
            incl_excl(1%) = 1.01 : incl_excl$(1%) = "O"
            return

        set_view_only_stuff
            inpmessage$ = hex(06) & "Position cursor & ENTER to see Deta"~
                        & "il, PF31 to PRINT, or PF16 to EXIT"
            save_admin% = admin%
            admin% = 1%
            gosub view_buyer_switch
            gosub view_select_vsas
            admin% = save_admin%
            gosub view_buyer_switch
            return

        view_select_vsas
            hdr$(1%) = "  Actvty Job-Nbr  RteStep Order-By Due-In   Vendo~
        ~r    Advice-# Quantity UOW Status   Price Buyer Purchase-Order   ~
        ~Contract-ID"
            descr_map(01%) = 050.04 : descr_map(02%) = 001  /* Activity */
            descr_map(03%) = 013.08 : descr_map(04%) = 008  /* Job Nbr  */
            descr_map(05%) = 021.04 : descr_map(06%) = 018  /* Rte Step */
            descr_map(07%) = 029.061: descr_map(08%) = 025  /* StartDate*/
            descr_map(09%) = 035.061: descr_map(10%) = 034  /* End Date */
            descr_map(11%) = 041.09 : descr_map(12%) = 043  /* Vendor   */
            descr_map(13%) = 005.08 : descr_map(14%) = 053  /* Advice # */
            descr_map(15%) = 134.08 : descr_map(16%)=62.084 /* Quantity */
            descr_map(17%) = 142.04 : descr_map(18%) = 071  /* UOW      */
            descr_map(19%) = 001.01 : descr_map(20%) = 077  /* Status   */
            descr_map(21%) =-058.25 : descr_map(22%) =1008  /* Job Part */
            descr_map(23%) =-009.30 : descr_map(24%) =1034  /* Job Descr*/
            descr_map(25%) = 094.40 : descr_map(26%) =2018  /* Comments */
            descr_map(27%) = 146.08 : descr_map(28%)=81.087 /* Price    */
            descr_map(29%) = 002.03 : descr_map(30%) =  91  /* Buyer    */
            descr_map(31%) = 074.16 : descr_map(32%) = 096  /* P.O. #   */
            descr_map(33%) = 054.20 : descr_map(34%) = 113  /* Contract */

            str(hdr$(3%),63%) = str(line2$,62%)

            call "PLOWCODE" (#4, plowkey$, inpmessage$, break%,          ~
                key_desclen, f1%(4%), hdr$(), 0, -13, incl_excl(),       ~
                incl_excl$(), "D", " ", #9, descr_map())
            if f1%(4%) = 1% then gosub dataload
            keyhit% = -1%
            return

        view_procurement_history
            part$ = "ACTIVITY: " & activity$
            call "HNYPRCSB" (vendor$, part$, 1%, #13, #12, #03, #11)
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************
        cancel_advice
            call "ASKUSER" (u3%, "*** CANCEL SERVICE ADVICE? ***",       ~
                "Press F8 to go ahead and CANCEL this Advice",           ~
                               " - or -",                                ~
                "Press (ENTER/RETURN) to UNDO this Operation.")
            if u3% = 0% then edit_vsa
            if u3% <> 8% then cancel_advice
            status$ = "C"
            goto L19150

        release_advice
            status$ = "R"
            goto L19150

        datasave
            if status$ <> "O" then L19160
L19150:     gosub dataput
L19160:     keyhit% = -1%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L21100,         /* Activity Code          */~
                              L21150,         /* Advice Number          */~
                              L21200,         /* For Job                */~
                              L21250,         /* Route Step             */~
                              L21300,         /* Order Date             */~
                              L21350,         /* Due Date               */~
                              L21400,         /* Vendor                 */~
                              L21450,         /* Purchase Contract      */~
                              L21500,         /* Vendor Part Nbr        */~
                              L21550,         /* Unit of Work           */~
                              L21600,         /* Quantity               */~
                              L21650,         /* Vendor Unit Price      */~
                              L21700,         /* Cost Bucket            */~
                              L21750,         /* Currency Code          */~
                              L21800          /* Note / Comments        */
            return
L21100: REM Def/Enable Vendor Service Activity     ACTIVITY$
            if f1%(4%) = 1% and activity$ <> " " then enabled% = 0%
            return

L21150: REM Def/Enable Vendor Service Advice Nbr   ADVICE_NBR$
            enabled% = 0%
            return

L21200: REM Def/Enable For Job                     JOBNR$
            if f1%(4%) = 1% and jobnr$ <> " " then enabled% = 0%
            return

L21250: REM Def/Enable     Route Step              STEP$
            if f1%(4%) = 1% and step$ <> " " then enabled% = 0%
            return

L21300: REM Def/Enable Order Date                  DATE_ORDER$
            if edit% = 1% and date_order$<> " " and date_order$ <> blankdate$ ~
                and keyhit% <> 4% then enabled% = 0%
            return

L21350: REM Def/Enable Due Date                    DATE_DUE$
            if edit% = 1% and date_due$<> " " and date_due$ <> blankdate$ ~
                and keyhit% <> 4% then enabled% = 0%
            return

L21400: REM Def/Enable Vendor                      VENDOR$
            if edit% = 1% and vendor$<> " " and keyhit% <> 4%            ~
                then enabled% = 0%
            return

L21450: REM Def/Enable Purchase Contract Number    CONTRACT$
            if fs%(10%) = 0% then enabled% = 0%
            if contract$ <> " " then return
            call "VPCDEFLT" (vendor$, str(contract$,,16%),               ~
                             str(contract$,17%,4%), "A", activity$,      ~
                             date_order$, #10, f1%(10%))
            return

L21500: REM Def/Enable Vendor Part Number          VEND_PART$
            if edit% = 1% and vend_part$<> " " and keyhit% <> 4%         ~
                then enabled% = 0%
            return

L21550: REM Def/Enable Unit of Work Measure Code   UOM$
            if edit% = 1% and uom$<> " " and keyhit% <> 4%               ~
                then enabled% = 0%
            return

L21600: REM Def/Enable Number of Units of Work     QTY$
            if edit% = 1% and qty$<> " " and keyhit% <> 4%               ~
                then enabled% = 0%
            call "CONVERT" (qty, -0.2, qty$)
            return

L21650: REM Def/Enable Vendor Unit Price           PRICE$
            if edit% = 1% and price$<> " " and keyhit% <> 4%             ~
                then enabled% = 0%
            call "CONVERT" (price, -0.7, price$)
            return

L21700: REM Def/Enable Posts to Cost Bucket #      COST_BUCKET$
            if edit% = 1% and cost_bucket$<> " " and keyhit% <> 4%       ~
                then enabled% = 0%
            if cost_bucket$ = " " or buckets% = 0% then cost_bucket$ = "1"
            if buckets% = 0% then enabled% = 0%
            return

L21750: REM Def/Enable Currency Code               CURRENCY$
            if curr_on_flag$ <> "Y" then enabled% = 0%
            if curr_on_flag$ <> "Y" then return
            if currency$ = " " then currency$ = curr_vend$
            if currency$ = " " then currency$ = stat$
            if edit% = 1% and currency$<> " " and keyhit% <> 4%          ~
                then enabled% = 0%
            return

L21800: REM Def/Enable Note / Comments             COMMENTS$
            if edit% = 1% and comments$<> " " and keyhit% <> 4%          ~
                then enabled% = 0%
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
            if scrnr% = 2% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Vendor Service Activity                                ",~
         "Enter Menu Pick                                              "

        scrn2_msg  :  data                                               ~
         "Enter Vendor Service Activity                                ",~
         "Enter Vendor Service Advice Nbr                              ",~
         "Enter For Job                                                ",~
         "Enter     Route Step                                         ",~
         "Enter Order Date                                             ",~
         "Enter Due Date                                               ",~
         "Enter Vendor                                                 ",~
         "Enter Purchase Contract Number                               ",~
         "Enter Vendor Part Number                                     ",~
         "Enter Unit of Work Measure Code                              ",~
         "Enter Number of Units of Work                                ",~
         "Enter Vendor Unit Price                                      ",~
         "Enter Posts to Cost Bucket #                                 ",~
         "Enter Currency Code                                          ",~
         "Enter Note / Comments                                        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, cost_bucketdescr$,         ~
                      advice_nbr$, comments$, status$, statusdescr$,     ~
                      contract$, contractdescr$, cost_bucket$,           ~
                      currency$, currencydescr$, date_due$,              ~
                      date_order$, jobnr$, jobnrdescr$, pk$, price$,     ~
                      qty$, step$, curr_vend$, uom$, uomdescr$,          ~
                      vendor$, vendordescr$, vend_part$
            init (hex(00)) std_costs$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
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
            goto editpg1

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
        get #4 using L35260, /* FILE: VBKVSA                            */~
            status$,        /* Status Indicator                        */~
            buyer$,         /* Buyer/planner code                      */~
            advice_nbr$,    /* Advice Number                           */~
            jobnr$,         /* Job Number                              */~
            step$,          /* RTE Step To Identify A Route Line       */~
            wc$,            /* Work Center ID                          */~
            date_order$,    /* WC Start Date                           */~
            date_due$,      /* WC End Date                             */~
            vendor$,        /* Vendor                                  */~
            activity$,      /* Activity to be performed                */~
            contract$,      /* Purchasing Contract ID                  */~
            po$,            /* Purchase Order Number                   */~
            po_line$,       /* Purchase Order Line Number              */~
            adj_flag$,      /* Automatic Adjustment Flag               */~
            comments$,      /* Comment                                 */~
            qty,            /* Quantity to Buy                         */~
            uom$,           /* Unit of Measure                         */~
            price,          /* Unit Price                              */~
            cost_bucket ,   /* Standard Cost Bucket                    */~
            vend_part$,     /* Vendor Part Number                      */~
            currency$,      /* Currency Code                           */~
            filler$         /* Unused filler area in record (reserved b*/~

            call "DESCRIBE" (#09, jobnr$, jobnrdescr$, 1%, f1%(9%))
            call "DESCRIBE" (#03, vendor$, vendordescr$, 1%, f1%(3%))
            if f1%(3%) = 1% then get #3 using L30296, curr_vend$          ~
                            else curr_vend$ = " "
L30296:       FMT POS(528), CH(4)
            readkey$ = "WC ACTVTY" & activity$
            call "DESCRIBE" (#07, readkey$, activitydescr$, 1%, f1%(7%))
            call "DESCRIBE" (#10, contract$, contractdescr$, 1%, f1%(10%))
            readkey$ = "UOM      " & uom$
            call "DESCRIBE" (#07, readkey$, uomdescr$, 1%, f1%(7%))
            call "CONVERT" (qty, 0.2, qty$)
            call "CONVERT" (price, 0.7, price$)
            call "CONVERT" (cost_bucket, 0.0, cost_bucket$)
            if buckets% = 0% then                                        ~
                cost_bucketdescr$ = "No Std Cost Set In Force"           ~
            else  cost_bucketdescr$ = bucket_ids$(cost_bucket)
            call "PUTPAREN" (cost_bucketdescr$)
            if status$ = "O" then statusdescr$ = "(Open-not Released or A~
        ~ssigned)"
            if status$ = "R" then statusdescr$ = "(Released to Purchase D~
        ~irective)"
            if status$ = "P" then statusdescr$ = "(Assigned to below Purc~
        ~ase Order)"
            if status$ = "C" then statusdescr$ = "(Cancelled)"
            call "DATEFMT" (date_order$)
            call "DATEFMT" (date_due$)
            part$ = "ACTIVITY: " & str(activity$) & " (" &               ~
                    str(advice_nbr$) & ")"
            gosub'052(14%)     /* Set Default Currency Code Value      */
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ("Saving Purchase Directive, Service Advice")
            buyer$ = userid$
            if f1%(4%) = 0% then gosub get_next_advice_number
            call "READ101" (#4, advice_nbr$, f1%(4%))
            call "DATUNFMT" (date_order$)
            call "DATUNFMT" (date_due$)
            if f1%(4%) = 0% then adj_flag$ = "N"
            part$ = "ACTIVITY: " & str(activity$) & " (" &               ~
                    str(advice_nbr$) & ")"

        put #4 using L35260, /* FILE: VBKVSA                            */~
            status$,        /* Status Indicator                        */~
            buyer$,         /* Buyer/planner code                      */~
            advice_nbr$,    /* Advice Number                           */~
            jobnr$,         /* Job Number                              */~
            step$,          /* RTE Step To Identify A Route Line       */~
            wc$,            /* Work Center ID                          */~
            date_order$,    /* WC Start Date                           */~
            date_due$,      /* WC End Date                             */~
            vendor$,        /* Vendor                                  */~
            activity$,      /* Activity to be performed                */~
            contract$,      /* Purchasing Contract ID                  */~
            po$,            /* Purchase Order Number                   */~
            po_line$,       /* Purchase Order Line Number              */~
            adj_flag$,      /* Automatic Adjustment Flag               */~
            comments$,      /* Comment                                 */~
            qty,            /* Quantity to Buy                         */~
            uom$,           /* Unit of Measure                         */~
            price,          /* Unit Price                              */~
            cost_bucket ,   /* Standard Cost Bucket                    */~
            vend_part$,     /* Vendor Part Number                      */~
            currency$,      /* Currency Code                           */~
            filler$         /* Unused filler area in record (reserved b*/~

            if f1%(4%) = 1% then rewrite #04 else write #04
            if status$ <> "R" then return /* Just released this then */

*          * stuff Cost Info *
            put str(std_costs$,(cost_bucket-1)*8+1,8%) using L31406, price
L31406:     FMT PD(14,4)

*          Build PIP Tag Entry    (useless - I know)
            call "PIPINDEX" (#1, date_order$, ord%, err%)
            convert ord% to str(piptag$,3%,3%), pic(###)
            str(piptag$,,2%) = "RO"
            str(piptag$,6%)  = tagdate$ & time

            seq% = 0%
L31472:     seq% = seq% + 1%
            convert seq% to seq$, pic(000)

        put #02, using L35030, /* FILE: PORLSE                          */~
            status$,        /* P.O. Conformation Flag (Y/N)            */~
            buyer$,         /* Buyer/planner code                      */~
            vendor$,        /* Vendor code                             */~
            part$,          /* Part Number                             */~
            date_order$,    /* Date Ordered (Purchase Order Date)      */~
            seq$,           /* Sequence Number                         */~
            " ",            /* Tag number in level 2 planning          */~
            qty,            /* Quantity to Buy                         */~
            qty*price,      /* Extension Amount (Quantity * Price)     */~
            vend_part$,     /* vendor part number                      */~
            date_due$,      /* P.O. Line Item Due Date                 */~
            uom$,           /* Unit of Measure                         */~
            1,              /* # of Stocking (base) Units which go into*/~
            price,          /* Unit Price                              */~
            price,          /* Total Standard Cost                     */~
            std_costs$,     /* Inventory Costs                         */~
            currency$,      /* Currency Code                           */~
            piptag$,        /* POINTER TO PIP RECORDS CREATED OR STORED*/~
            jobnr$,         /* Production job code                     */~
            contract$,      /* Vendor Blanket PO Contract              */~
            " "             /* Unused filler area in record (reserved b*/

            write #2, data goto L31472
            return


        get_next_advice_number
            readkey$ = "NEXT VSA NUMBER"
            call "READ101" (#1, readkey$, f1%(1%))
            if f1%(1%) = 0% then                                         ~
                advice% = 1%                                             ~
            else                                                         ~
                get #1 using L32080, advice%
L32080:              FMT POS(21), BI(4)
            convert advice% to advice_nbr$, pic(00000000)
            advice% = advice% + 1%
            put #1 using L32120, readkey$, advice%, " ", " "
L32120:         FMT CH(20), BI(4), CH(250), CH(226)
            if f1%(1%) = 0% then write #1 else rewrite #1
            return


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: PORLSE                            */~
            CH(1),          /* P.O. Conformation Flag (Y/N)            */~
            CH(3),          /* Buyer/planner code                      */~
            CH(9),          /* Vendor code                             */~
            CH(25),         /* Part Number                             */~
            CH(6),          /* Date Ordered (Purchase Order Date)      */~
            CH(3),          /* Sequence Number                         */~
            CH(19),         /* Tag number in level 2 planning          */~
            PD(14,4),       /* Quantity to Buy                         */~
            PD(14,4),       /* Extension Amount (Quantity * Price)     */~
            CH(25),         /* vendor part number                      */~
            CH(6),          /* P.O. Line Item Due Date                 */~
            CH(4),          /* Unit of Measure                         */~
            PD(14,4),       /* # of Stocking (base) Units which go into*/~
            PD(14,7),       /* Unit Price                              */~
            PD(14,4),       /* Total Standard Cost                     */~
            CH(96),         /* Inventory Costs                         */~
            CH(4),          /* Currency Code                           */~
            CH(19),         /* POINTER TO PIP RECORDS CREATED OR STORED*/~
            CH(8),          /* Production job code                     */~
            CH(20),         /* Vendor Blanket PO Contract              */~
            CH(204)         /* Unused filler area in record (reserved b*/~

L35260: FMT                 /* FILE: VBKVSA                            */~
            CH(1),          /* Status Indicator                        */~
            CH(3),          /* Buyer/planner code                      */~
            CH(8),          /* Advice Number                           */~
            CH(8),          /* Job Number                              */~
            CH(4),          /* RTE Step To Identify A Route Line       */~
            CH(4),          /* Work Center ID                          */~
            CH(06),         /* WC Start Date                           */~
            CH(06),         /* WC End Date                             */~
            CH(9),          /* Vendor                                  */~
            CH(04),         /* Activity to be performed                */~
            CH(20),         /* Purchasing Contract ID                  */~
            CH(16),         /* Purchase Order Number                   */~
            CH(03),         /* Purchase Order Line Number              */~
            CH(1),          /* Automatic Adjustment Flag               */~
            CH(40),         /* Comment                                 */~
            PD(14,4),       /* Quantity to Buy                         */~
            CH(4),          /* Unit of Measure                         */~
            PD(14,7),       /* Unit Price                              */~
            BI(4),          /* Standard Cost Bucket                    */~
            CH(25),         /* Default Vendor Part Number for Activity */~
            CH(4),          /* Currency Code                           */~
            CH(114)         /* Unused filler area in record (reserved b*/~

        FMT                 /* FILE: HNYACTXF                          */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(4),          /* Activity to be performed                */~
            CH(3),          /* Buyer Class Code                        */~
            CH(4),          /* Unit of Measure                         */~
            PD(14,7),       /* Conversion Factor                       */~
            PD(14,7),       /* Unit Price                              */~
            BI(4),          /* Standard Cost Bucket                    */~
            CH(9),          /* Vendor                                  */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(4),          /* Internal ID to text in TXTFILE          */~
            CH(25),         /* Default Vendor Part Number for Activity */~
            CH(218)         /* Unused filler area in record (reserved b*/~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1

L40095:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Vendor Service Advices           ",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Enter Vendor Service Activity Code:",        ~
               at (05,38), fac(hex(81)),   activity$            , ch(04),~
               at (05,49), fac(hex(8c)),   activitydescr$       , ch(32),~
                                                                         ~
               at (06,02), "Then Press Appropriate Key To...",           ~
                                                                         ~
               at (08,04), fac(hex(8c)) , msg$( 1%)             , ch(77),~
               at (09,04), fac(hex(8c)) , msg$( 2%)             , ch(77),~
               at (10,04), fac(hex(8c)) , msg$( 3%)             , ch(77),~
               at (11,04), fac(hex(8c)) , msg$( 4%)             , ch(77),~
               at (12,04), fac(hex(8c)) , msg$( 5%)             , ch(77),~
               at (13,04), fac(hex(8c)) , msg$( 6%)             , ch(77),~
               at (14,04), fac(hex(8c)) , msg$( 7%)             , ch(77),~
               at (15,04), fac(hex(8c)) , msg$( 8%)             , ch(77),~
               at (16,04), fac(hex(8c)) , msg$( 9%)             , ch(77),~
               at (17,04), fac(hex(8c)) , msg$(10%)             , ch(77),~
               at (18,04), fac(hex(8c)) , msg$(11%)             , ch(77),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40535
                  call "MANUAL" ("POVRELSB") : goto L40095

L40535:        if keyhit% <> 15% then return
                  call "PRNTSCRN" : goto L40095

        set_pf1
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)Toggle Main/Alternate Buyer          " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit        "
            pfkeys$ = hex(0102030405060708090affff0d0e0f1000)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'050(2%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41145,         /* Activity Code     */   ~
                                L41150,         /* Advice Number     */   ~
                                L41145,         /* For Job           */   ~
                                L41145,         /* Route Step        */   ~
                                L41145,         /* Order Date        */   ~
                                L41145,         /* Due Date          */   ~
                                L41145,         /* Vendor            */   ~
                                L41145,         /* Purchase Contract */   ~
                                L41145,         /* Vendor Part Nbr   */   ~
                                L41145,         /* Unit of Work      */   ~
                                L41150,         /* Quantity          */   ~
                                L41150,         /* Vendor Unit Price */   ~
                                L41150,         /* Cost Bucket       */   ~
                                L41145,         /* Currency Code     */   ~
                                L41140          /* Note / Comments   */
              goto L41160

L41140:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41145:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L41150:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41160:     accept                                                       ~
               at (01,02),                                               ~
                "Create Purchase Directives from Vendor Service Advices",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Current Status",                             ~
               at (05,30), fac(hex(8c)),   status$              , ch(01),~
               at (05,49), fac(hex(8c)),   statusdescr$         , ch(32),~
               at (06,02), "Vendor Service Activity",                    ~
               at (06,30), fac(lfac$( 1)), activity$            , ch(04),~
               at (06,49), fac(hex(8c)),   activitydescr$       , ch(32),~
                                                                         ~
               at (07,02), "Vendor Service Advice Nbr",                  ~
               at (07,30), fac(lfac$( 2)), advice_nbr$          , ch(08),~
                                                                         ~
               at (08,02), "For Job",                                    ~
               at (08,30), fac(lfac$( 3)), jobnr$               , ch(08),~
               at (08,49), fac(hex(8c)),   jobnrdescr$          , ch(32),~
                                                                         ~
               at (09,02), "    Route Step",                             ~
               at (09,30), fac(lfac$( 4)), step$                , ch(04),~
                                                                         ~
               at (10,02), "Order Date",                                 ~
               at (10,30), fac(lfac$( 5)), date_order$          , ch(10),~
                                                                         ~
               at (11,02), "Due Date",                                   ~
               at (11,30), fac(lfac$( 6)), date_due$            , ch(10),~
                                                                         ~
               at (12,02), "Vendor",                                     ~
               at (12,30), fac(lfac$( 7)), vendor$              , ch(09),~
               at (12,49), fac(hex(8c)),   vendordescr$         , ch(32),~
                                                                         ~
               at (13,02), "Purchase Contract Number",                   ~
               at (13,30), fac(lfac$( 8)), str(contract$,1%,16%), ch(16),~
               at (13,47), fac(lfac$(8%)), str(contract$,17%,4%), ch(04),~
               at (13,52), fac(hex(8c)),   str(contractdescr$,,29%),     ~
                                                                         ~
               at (14,02), "Vendor Part Number",                         ~
               at (14,30), fac(lfac$( 9)), vend_part$           , ch(25),~
                                                                         ~
               at (15,02), "Unit of Work Measure Code",                  ~
               at (15,30), fac(lfac$(10)), uom$                 , ch(04),~
               at (15,49), fac(hex(8c)),   uomdescr$            , ch(32),~
                                                                         ~
               at (16,02), "Number of Units of Work",                    ~
               at (16,30), fac(lfac$(11)), qty$                 , ch(15),~
                                                                         ~
               at (17,02), "Vendor Unit Price",                          ~
               at (17,30), fac(lfac$(12)), price$               , ch(15),~
                                                                         ~
               at (18,02), "Posts to Cost Bucket #",                     ~
               at (18,30), fac(lfac$(13)), cost_bucket$         , ch(02),~
               at (18,49), fac(hex(8c)),   cost_bucketdescr$    , ch(30),~
                                                                         ~
               at (19,02), "Currency Code",                              ~
               at (19,30), fac(lfac$(14)), currency$            , ch(04),~
               at (19,49), fac(hex(8c)),   currencydescr$       , ch(32),~
                                                                         ~
               at (20,02), "Note / Comments",                            ~
               at (20,30), fac(lfac$(15)), comments$            , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41515
                  call "MANUAL" ("POVRELSB") : goto L41160

L41515:        if keyhit% <> 15 then L41530
                  call "PRNTSCRN" : goto L41160

L41530:        if edit% = 1% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        set_pf2
        if edit% = 2% then L41625     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit        "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41605
                str(pf$(3),64)    = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L41605:     if fieldnr% > 2% then L41615
                str(pf$(2),18,26%) = " "  :  str(pfkeys$, 4%,1%) = hex(ff)
L41615:     return

L41625: if fieldnr% > 0% then L41670  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "(2)Release Advice                       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Cancel Advice                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(010203ffffffffffffffffff0dff0f1000)
            if status$ = "O" then return
               str(pf$(2%),1%,17%), str(pf$(3%),1%,16%) = " "
               str(pfkeys$,2%,2%) = hex(ffff)
               str(pf$(3%),64%) = "(16)Return"
            return
L41670:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51240,         /* Activity Code          */~
                              L51330,         /* Advice Number          */~
                              L51360,         /* For Job                */~
                              L51420,         /* Route Step             */~
                              L51450,         /* Order Date             */~
                              L51490,         /* Due Date               */~
                              L51610,         /* Vendor                 */~
                              L51700,         /* Purchase Contract      */~
                              L51810,         /* Vendor Part Nbr        */~
                              L51840,         /* Unit of Work           */~
                              L51930,         /* Quantity               */~
                              L51970,         /* Vendor Unit Price      */~
                              L52010,         /* Cost Bucket            */~
                              L52090,         /* Currency Code          */~
                              L52210          /* Note / Comments        */
            return
L51240: REM Test for Vendor Service Activity      ACTIVITY$
            plowkey$ = "WC ACTVTY" & activity$
            call "PLOWCODE" (#07, plowkey$, activitydescr$, 9%,.3,f1%(7%))
            if f1%(7%) = 1% then call "PUTPAREN" (activitydescr$)        ~
            else errormsg$ = "ERROR: Activity not on file: " & activity$
            if f1%(7%) = 1% then activity$ = str(plowkey$,10%)
            part$ = "ACTIVITY: " & activity$
            return

L51330: REM Test for Vendor Service Advice Nbr    ADVICE_NBR$
            return

L51360: REM Test for For Job                      JOBNR$
            call "PLOWCODE" (#09, jobnr$, jobnrdescr$, 0%, 0.3, f1%(9%))
            if f1%(9%) = 1% then call "PUTPAREN" (jobnrdescr$)           ~
            else errormsg$ = "ERROR: Job not on file: " & jobnr$
            return

L51420: REM Test for     Route Step               STEP$
            return

L51450: REM Test for Order Date                   DATE_ORDER$
            call "DATEOK" (date_order$, date%, errormsg$)
            return

L51490: REM Test for Due Date                     DATE_DUE$
            call "DATEOK" (date_due$, date%, errormsg$)
            if errormsg$ <> " " then return
            temp_date$ = date_due$
            call "DATUNFMT" (temp_date$)
            call "DATE" addr("G-", basedate$, temp_date$, due%, err%)
            if err% <> 0%  then L51580
            if due% < 0% or due% > 489% then L51580
            return
L51580:     errormsg$ = "Due Date Is Not Within Planning Calendar"
            return

L51610: REM Test for Vendor                       VENDOR$
            call "PLOWCODE" (#03, vendor$, vendordescr$, 0%, .3, f1%(3%))
            if f1%(3%) = 1% then call "PUTPAREN" (vendordescr$)          ~
            else errormsg$ = "ERROR: Vendor not on file: " & vendor$
            if f1%(3%) = 1% then get #3 using L51670, curr_vend$          ~
                            else curr_vend$ = " "
L51670:       FMT POS(528), CH(4)
            return

L51700: REM Test for Purchase Contract Number     CONTRACT$
            contractdescr$ = " "
            if contract$ = " " or fs%(10%) = 0% then return
            call "VPCPIKME" (vendor$, str(contract$,,16%),               ~
                             str(contract$,17%,4%), "A", activity$,      ~
                             date_order$, #10, #03, f1%(10%))

            call "DESCRIBE" (#10, contract$, contractdescr$, 1%, f1%(10%))
            if f1%(10%) = 0% then                                        ~
                 errormsg$ = "ERROR: Contract not on file: " & contract$
            return

L51810: REM Test for Vendor Part Number           VEND_PART$
            return

L51840: REM Test for Unit of Work Measure Code    UOM$
            on uom%+1% goto L51910
            plowkey$ = "UOM      " & uom$
            call "PLOWCODE" (#07, plowkey$, uomdescr$, 9%, .3, f1%(7%))
            if f1%(7%) = 1% then call "PUTPAREN" (uomdescr$)             ~
            else errormsg$ = "ERROR: Unit of Work not on file: " & uom$
            if f1%(7%) = 1% then uom$ = str(plowkey$,10%)
L51910:     return

L51930: REM Test for Number of Units of Work      QTY$
            call "NUMTEST" (qty$, 0, 9e8, errormsg$, -.2, qty)
            return

L51970: REM Test for Vendor Unit Price            PRICE$
            call "NUMTEST" (price$, 0, 9e8, errormsg$, -.7, price)
            return

L52010: REM Test for Posts to Cost Bucket #       COST_BUCKET$
            temp = max(1%, buckets%)   /* Max # of cost buckets */
            call "NUMTEST" (cost_bucket$,1,temp,errormsg$, 0, cost_bucket)
            if errormsg$ <> " " then return
            cost_bucketdescr$ = bucket_ids$(cost_bucket)
            call "PUTPAREN" (cost_bucketdescr$)
            return

L52090: REM Test for Currency Code                CURRENCY$
            if curr_on_flag$ <> "Y" then return
               currencydescr$ = hex(06) & "Select Vendor Currency"
               call "PLOWCODE" (#6,currency$,currencydescr$,0%,.3,f1%(6%))
               if f1%(6%) = 1% then call "PUTPAREN" (currencydescr$)
               if f1%(6%) = 1% then return
                   if currency$ = " " then                               ~
                      errormsg$ = "Currency code cannot be blank."       ~
                   else                                                  ~
                      errormsg$ = "Currency code not found on file."
            return

L52210: REM Test for Note / Comments              COMMENTS$
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
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program

            end
