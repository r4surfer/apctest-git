        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN52                             *~
            *  Creation Date     - 04/27/98                             *~
            *  Last Modified Date- 06/29/01                             *~
            *  Description       - This Program Scans 'BCKLINES to      *~
            *                      determine if a Sales Oder has been   *~
            *                      shipped complete or is a candidate   *~
            *                      for a Backorder.                     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/30/98 ! New Program for (EWD) -                  ! RHH *~
            * 04/30/98 ! Y2K modifications                        ! ERN *~
            * 06/11/98 ! Fix Problem with shp_qty for report      ! RHH *~
            * 06/12/98 ! (EWD001) Mod to Report for RM/BK Sales   ! RHH *~
            *          !   Orders.                                !     *~
            * 07/15/98 ! (EWD002) Mod to Calc Default beginning   ! RHH *~
            *          !   and Ending Delivery Dates +- 14 Days   !     *~
            * 08/20/98 ! (EWD003) Mod Create a New Audit Report   ! RHH *~
            *          !   showing the Entries for the Loads      !     *~
            *          !   Specified.                             !     *~
            * 11/24/98 ! (EWD004) Mod to add a Selection prompt   ! RHH *~
            *          !                                          !     *~
            * 06/29/01 ! (EWD005) Mod to add new File EWDBOLBK to ! CMG *~
            *          !          report and to allow to run      !     *~
            *          !          report by All date or All loads !     *~
            * 04/18/02 ! (EWD006) Mod to change company name      ! TLM *~
            *************************************************************

        dim                                                              ~
            sc_sel$1, sc_sel_d$30,       /* Report Selection (EWD004)  */~
            beg_dte$6, beg_date$10,      /* Beg/End Delivery Date      */~
            bg_date$6,                   /* (EWD002)Calc Default Dates */~
            end_dte$6, end_date$10,      /*                            */~
            beg_load$5, beg_load_d$30,   /* Beginning Load Number      */~
            end_load$5, end_load_d$30,   /* Ending Load Number         */~
            ld_load$5, sav_load$5,       /* Print Load Number          */~
            beg_default$5, end_default$5,/* Defaults based on Date     */~
            def_load$5,                  /* default load               */~
            or_cuscode$9,                /* Customer Code              */~
            or_so$8, or_po$16,           /* Sales Order, Purchase Order*/~
            sav_so$8,                    /* Save SO Number      EWD005 */~
            part$25, part_d$30,          /* Part Number and Description*/~
            ln$3,                        /* S.O. Line Item             */~
            rm_qty$4, or_st$2,           /* Remove Qty and Order Status*/~
            ord_qty$4, shp_qty$4,        /* S.O. Qty and Shipped Qty   */~
            opn_qty$4, sch_qty$4,        /* S.O. Open and Schedule Qty */~
            all_qty$4, pre_qty$4,        /* S.O. Allocate and pre-Inv  */~
            or_key$51, or_rec$170,       /* (APCPLNOR) Key and Record  */~
            bck_key$19, bck_rec$200,     /* (BCKLINES) Key and Record  */~
            rm_key$16,                   /* (EWD001) Removal Primary Ky*/~
            rm_load$5,                   /* (EWD003) Load Number       */~
            rm_so$8,                     /* (EWD003) Sales Order       */~
            rm_ln$3,                     /* (EWD003) Line Item No.     */~
            rm_dte$6, rm_date$8,         /* (EWD003) Date Removed      */~
            rm_usr$3, rm_name$20,        /* (EWD003) Removed By        */~
            rm_desc$9,                   /* (EWD003) Description       */~
            rm_type$2,                   /* (EWD001) Removal Type RM/BK*/~
            line_status$9,               /* (EWD001) Description       */~
            tst_qty$4, tst_qty1$4,       /* Test Quantities            */~
            hdr$50, msg$(3%)79,          /* Askuser - Var's            */~
            dsp_msg$50,                  /* (EWD004)                   */~
            ld_key$15,                   /* Planning Load Key          */~
            x$10,                        /* Date Buffer                */~
            cnt$30,                      /* Activity Display           */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            cursor%(2%),                 /* Cursor location for edit   */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,print_title$46,    /* PF Key Hex Values          */~
            company$40,                  /* Comapny Name and Title     */~
            userid$3, wrk_key$32         /* Current User Id            */

        dim syslib$8,                    /* System library             */~
            sysvol$6,                    /* System Volume              */~
            progvol$6,                   /* Current Program's Volume   */~
            secadm$3,                    /* Is User a Super User ?     */~
            zzid$3,                      /* For who's running this     */~
            zze$4,                       /* Does this guy have clout   */~
            zzr$4,                       /* Does this guy have clout   */~
            zzw$4,                       /* Does this guy have clout   */~
            zzil$8,                      /* His INLIB                  */~
            zzol$8,                      /* His OUTLIB                 */~
            zziv$6,                      /* His INVOL                  */~
            zzov$6                       /* His OUTVOL                 */

        dim                              /*   EWDBOLBK  (EWD005)       */~
            area_code$1,                 /* Area    Code               */~
            area_desc$30,                /* Area    Description        */~
            reas_code$3,                 /* Reason  Code               */~
            reas_desc$30,                /* Reason  Description        */~
            auth_id$3,                   /* Authorized  Id             */~
            auth_name$20,                /* Authorized By Name         */~
            usr_name$20,                 /* User ID Name               */~
            text$(3%)20,                 /* Text Area                  */~
            bk_time$8,                   /* Time Backordered           */~
            bk_key$16,                   /* EWDBOLBK Readkey           */~
            bk_seq$5,                    /* Planning Sort Sequence     */~
            readkey$50,                  /* GENCODES Readkey           */~
            desc$30,                     /* GENCODES Description       */~
            sel$1,                       /* Report Selection           */~
            sel_desc$34,                 /* Report Selection Descript  */~
            scr$(5%)40,                  /* Report Selection Menu      */~
            beg_mod$3, end_mod$3,        /* Begin & End Model          */~
            beg_rea$3, end_rea$3         /* Begin & End Reason         */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim ewd$40, pname$21
            ewd$   = "(EWD) S.O. Backorder Analysis Report    "
            pname$ = "EWDPLN52 - Rev: R7.00"

        REM *************************************************************

            mat f2% = con
            mat fs% = con
            init(" ") rslt$()
                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNOR ! Planning Sales Order Header              *~
            * #2  ! BCKLINES ! Sales Order Line Items                   *~
            * #3  ! GENCODES ! GENERAL SYSTEM CODES FILE                *~
            * #4  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #5  ! APCPLNLD ! Planning Load Master File                *~
            * #6  ! EWDWORK  ! Backorder Work File                      *~
            * #7  ! USERCLMS ! Master User Id File                      *~
            * #10 ! EWDBOLRM ! Line Items Removed from Load             *~
            * #11 ! EWDBOLBK ! Cross-Reference file for Back Orders     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos =    1, keylen =  51,                     ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup

            select #2, "BCKLINES",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   24

            select #4,   "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #5,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15

            select #6,  "EWDWORK ",                                      ~
                        varc,     indexed,  recsize =  228,              ~
                        keypos =    1, keylen =   32
                                                     /* (EWD001) Begin */
            select #10, "EWDBOLRM",                                      ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =  1,   keylen =   16,                    ~
                        alt key 1, keypos =   6, keylen = 11
                                                     /* (EWD001) End   */
            select #7,  "USERLCMS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos =    1, keylen =   3,                     ~
                        alt key 1, keypos =  4, keylen = 30, dup         ~

            select #11, "EWDBOLBK",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    7, keylen =   16,                    ~
                        alt key  1, keypos =   12, keylen =  11,         ~
                            key  2, keypos =    1, keylen =  22,         ~
                            key  3, keypos =    2, keylen =  21

/* (EWD005) */
            filename$ = "APCPLNOR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDBOLRM" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDBOLBK" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
                                                     /* (Ewd001)       */
            mat f1% = zer

            call "EXTRACT" addr("ID", zzid$, "IL", zzil$, "OL", zzol$,   ~
                                "IV", zziv$, "OV", zzov$, "CV", progvol$,~
                                "UE", zze$, "UR", zzr$, "UW", zzw$,      ~
                                "XL", syslib$, "XV", sysvol$)


            if pos(zze$<>hex(ff)) <> 0 then L01000
               if pos(zzr$<>hex(ff)) <> 0 then L01000
                  if pos(zzw$<>hex(ff)) <> 0 then L01000
                     secadm$ = "YES"

L01000:     call "READFDR" addr("USERLCMS", syslib$, sysvol$, 0%, f2%(7))
                if f2%(7) = 0% then L01010

            call "PUTNAMES" addr(#1, "USERLCMS", syslib$, sysvol$)
            call "WORKOPN2" (#7, "OUTSP", 100%, f2%(7))

            close #7
L01010:     call "PUTNAMES" addr(#7, "USERLCMS", syslib$, sysvol$)
            call "WORKOPN2" (#7, "SHARE", 100%, f2%(7))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            call "COMPNAME" (12%, company$, ret%)               /* (EWD006) */
REM            company$ = "   Ellison Window and Doors - Welcome   "
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  8%       /* (EWD004) 11/24/98 */
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
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

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  9% then gosub gen_rpt_removal
                  if keyhit%  = 14% then gosub gen_report
                  if keyhit%  = 16% then goto  exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% >  8% then editpg1/* (EWD004) */
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return
        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return
                                                  /* (EWd004) 11/24/98 */
                                                          /* (EWD005)  */
        scrn1_msg  :  data                                               ~
         "Enter a Report Sort Selection?                               ",~
         "Enter a Report Selection? 1 = Backorders, 2 = Other, 3 = All ",~
         "Enter the Beginning Delivery Date? (1st Delv date for Load)  ",~
         "Enter the Ending Delivery Date? (Last Delv for Load)         ",~
         "Enter the Starting Load Number (Within the Date Range)?      ",~
         "Enter the Ending Load Nuber (Within the Date Range)?         ",~
         "Enter the Beginning and Ending Model Code, ALL, or '?' for a L~
         ~isting?",~
         "Enter the Beginning and Ending Reason Code, ALL, or '?' for a ~
         ~List of Codes?"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_dte$, beg_date$,       ~
                      end_dte$, end_date$, beg_load$, end_load$,         ~
                      beg_load_d$, end_load_d$, x$, msg$(), hdr$,        ~
                      bg_date$, sc_sel$, sc_sel_d$, sel$, sel_desc$,     ~
                      beg_mod$, end_mod$, beg_rea$, end_rea$

            scr$(1%) = "*********< Report Sort Options >********"  /* (EWD005)  */
            scr$(2%) = "* (1) By Load, Customer, SO, Line Item *"
            scr$(3%) = "* (2) By Model, Reason, Area, SO, Line *"
            scr$(4%) = "* (3) By Area, Reason, SO, Line Item   *"
            scr$(5%) = "****************************************"

        return                                   /* (EWD002) Dates     */

        REM *************************************************************~
            *************************************************************


        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
L40080:       gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
                                                   /* (EWD004) 11/24/98  */
              on fieldnr% gosub L40210,                /* Report Sort    */~
                                L40210,                /* Report Select  */~
                                L40210,                /* Beginning Date */~
                                L40210,                /* Ending Date    */~
                                L40210,                /* Beginning Load */~
                                L40210,                /* Ending Load    */~
                                L40210,                /* Beg & End Model*/~
                                L40210                 /* Beg & End Reasn*/
              goto L40240

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40210:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40240:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), ewd$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Report Sort Selection  :",                   ~
               at (05,26), fac(lfac$(1%)), sel$                 , ch(01),~
               at (05,40), fac(hex(84))  , sel_desc$            , ch(33),~
                                                                         ~
               at (06,02), "Report Selection (1-3) :",                   ~
               at (06,26), fac(lfac$(2%)), sc_sel$              , ch(01),~
               at (06,40), fac(hex(84)), sc_sel_d$              , ch(30),~
                                                                         ~
               at (07,02), "Beginning Delivery Date:",                   ~
               at (07,26), fac(lfac$(3%)), beg_date$            , ch(10),~
                                                                         ~
               at (08,02), "Ending Delivery Date   :",                   ~
               at (08,26), fac(lfac$(4%)), end_date$            , ch(10),~
                                                                         ~
               at (09,02), "Beginning Load Number  :",                   ~
               at (09,26), fac(lfac$(5%)), beg_load$            , ch(05),~
               at (09,40), fac(hex(84)), beg_load_d$            , ch(30),~
                                                                         ~
               at (10,02), "Ending Load Number     :",                   ~
               at (10,26), fac(lfac$(6%)), end_load$            , ch(05),~
               at (10,40), fac(hex(84)), end_load_d$            , ch(30),~
                                                                         ~
               at (11,02), "Begin & End Model Code :",                   ~
               at (11,26), fac(lfac$(7%)), beg_mod$             , ch(03),~
               at (11,30), fac(lfac$(7%)), end_mod$             , ch(03),~
                                                                         ~
               at (12,02), "Begin & End Reason Code:",                   ~
               at (12,26), fac(lfac$(8%)), beg_rea$             , ch(03),~
               at (12,30), fac(lfac$(8%)), end_rea$             , ch(03),~
                                                                         ~
                                                                         ~
               at (13,21), fac(hex(84)),   scr$(1%)             , ch(40),~
               at (14,21), fac(hex(84)),   scr$(2%)             , ch(40),~
                                                                         ~
               at (15,21), fac(hex(84)),   scr$(3%)             , ch(40),~
               at (16,21), fac(hex(84)),   scr$(4%)             , ch(40),~
               at (17,21), fac(hex(84)),   scr$(5%)             , ch(40),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then L40805
                  call "PRNTSCRN" : goto L40080

L40805:        if keyhit% <> 12% then goto L40810  /* (EWD001) Hidden */
                  gosub purge_data : goto L40080   /* PF(12) - Key    */

L40810:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41020     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffff0cffff0f1000)
            if fieldnr% = 1% then L40960
                str(pf$(3%),64%)  = " " : str(pfkeys$,16%,1%) = hex(ff)
L40960:     if fieldnr% > 1% then L41000
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41000: return

L41020: if fieldnr% > 0% then L41110  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Print Report"
            pf$(2%) = "                                        " &       ~
                      "(9)Print Audit         (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffff09ffff0cff0e0f1000)
            return
L41110:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50000,                 /* Report Sort  */  ~
                              L50100,                 /* Report Select*/  ~
                              L50170,                 /* Beg Delv Date*/  ~
                              L50260,                 /* End Delv Date*/  ~
                              L50380,                 /* Beg Load     */  ~
                              L50580,                 /* End Load     */  ~
                              L50750,                 /* Beg & End Mod*/  ~
                              L50850                  /* Beg & End Rea*/
        return

L50000: REM Report Selection
            if sel$ <> " " then goto L50010
               sel$ = "1"
L50010:     convert sel$ to sel%, data goto L50020

            convert sel% to sel$, pic(#)
            if sel% < 1% or sel% > 3% then goto L50020
               sel_desc$ = str(scr$(sel%+1%),7%,33%)
        return
L50020:     errormsg$ = "(Error) - Invalid Report Selection?"
            init(" ") sel$
        return
                                                   /* (EWD004) Begin  */
L50100: REM Report Selection                       sc_sel$, sc_sel_d$
            if sc_sel$ <> " " then goto L50110
               sc_sel$ = "3"
L50110:     if sc_sel$ = "1" then sc_sel_d$ = "Backorders only"
            if sc_sel$ = "2" then sc_sel_d$ = "Everything Except Backorders"
            if sc_sel$ = "3" then sc_sel_d$ = "All Adjustments"
        return
                                                   /* (EWD004) End    */
L50170: REM Beginning Delivery Date                BEG_DTE$, BEG_DATE$
            if beg_date$ <> " " then goto L50180
REM               bg_date$ = date : err% = 0%         /* (EWD002) Begin  */
REM               call "DATE" addr("G+",bg_date$,-14%,beg_date$,err%)
                                                   /* (EWD002) End    */
            all_date
                  beg_date$ = "ALL"                /* (EWD005)        */
                  beg_dte$  = "ALL"                /* (EWD005)        */
                  end_date$ = "ALL"                /* (EWD005)        */
                  end_dte$  = "ALL"                /* (EWD005)        */
            return                                 /* (EWD005)        */

L50180:     date% = 0%
            call "DATEOKC" (beg_date$, date%, errormsg$)
            if date% = 0% then goto L50190
            x$ = beg_date$
            call "DATUFMTC"(x$)
            beg_dte$ = str(x$,1%,6%)
            gosub find_default
            beg_default$ = def_load$
        return
L50190:     init(" ") beg_dte$, beg_date$, x$, bg_date$
            errormsg$ = "(Error) Invalid Begining Delivery Date?"
            gosub error_prompt
        return

L50260: REM Ending Delivery Date                   END_DTE$, END_DATE$
            if end_date$ <> " " and end_date$ <> "ALL" then goto L50300
REM               bg_date$ = date : err% = 0%       /* (EWD002) Begin     */
REM               call "DATE" addr("G+",bg_date$,+14%,end_date$,err%)
                                                 /* (EWD002) End       */
              if beg_date$ = "ALL" then goto all_date
                  end_date$ = beg_date$          /* (EWD005)           */


L50300:     date% = 0%
            call "DATEOKC" (end_date$, date%, errormsg$)
            if date% = 0% then goto L50310
            x$ = end_date$
            call "DATUFMTC"(x$)
            end_dte$ = str(x$,1%,6%)
            if end_dte$ < beg_dte$ then goto L50310
            gosub find_default
            end_default$ = def_load$
        return
L50310:     init(" ") end_dte$, end_date$, x$, bg_date$
            errormsg$ = "(Error) Invalid Ending Delivery Date?"
            gosub error_prompt
        return

L50380: REM Beginning Load Number            beg_load$, beg_load_d$
            if beg_load$ <> " " then goto L50410
REM               beg_load$ = beg_default$
            all_load
                  beg_load$ = "ALL"              /* (EWD005)           */
                  end_load$ = "ALL"              /* (EWD005)           */
            return                               /* (EWD005)           */

L50410:     if beg_load$ = "ALL  " then goto all_load
            init(" ") ld_key$
            beg_load% = 0%
            convert beg_load$ to beg_load%, data goto L50420

L50420:     convert beg_load% to beg_load$, pic(00000)
            str(ld_key$,1%,5%) = beg_load$
            read #5,key = ld_key$, using L50450, beg_load_d$,       ~
                                                 eod goto L50540
L50450:        FMT POS(16), CH(30)
        return
L50540:     init(" ") beg_load$, beg_load_d$
            errormsg$ = "(Error) - Invalid Beginning Load Number?"
            gosub error_prompt
        return

L50580: REM Ending Load Number               end_load$, end_load_d$
            if end_load$ <> " " and end_load$ <> "ALL" then goto L50600
REM            end_load$ = end_default$
              if beg_load$ = "ALL" then goto all_load
                  end_load$ = beg_load$          /* (EWD005)           */
            return                               /* (EWD005)           */

L50600:     init(" ") ld_key$
            end_load% = 0%
            convert end_load$ to end_load%, data goto L50605

L50605:     convert end_load% to end_load$, pic(00000)
            str(ld_key$,1%,5%) = end_load$
            read #5,key = ld_key$, using L50450, end_load_d$,      ~
                                                 eod goto L50610
        return
L50610:     init(" ") end_load$, end_load_d$
            errormsg$ = "(Error) - Invalid Ending Load Number?"
            gosub error_prompt
        return

L50750: REM Beginning and Ending Model         BEG_MOD$, END_MOD$
        init(" ") readkey$, desc$
        if beg_mod$ <> " " then lookup_mod
        all_mod
             beg_mod$, end_mod$ = "ALL"
        return
        lookup_mod
            if beg_mod$ = "ALL" then goto all_mod
            str(readkey$,1%,9%) = "MODEL    "
            str(readkey$,10%,3%) = beg_mod$

               call "PLOWCODE" (#3, readkey$, desc$, 9%, .30, f1%(3))
                    if f1%(3) <> 1 then goto L50770

               beg_mod$ = str(readkey$,10%,3%)
               if end_mod$ <> " " then goto end_mod
                  end_mod$ = beg_mod$
               return
        end_mod
           str(readkey$,10%,3%) = end_mod$

           call "PLOWCODE" (#3, readkey$, desc$, 9%, .30, f1%(3))
                 if f1%(3) <> 1 then goto L50870

               end_mod$ = str(readkey$,10%,3%)
           if end_mod$ < beg_mod$ then goto L50770
        return
L50770:     errormsg$ = "(Error) - Invalid Model Code ?"
            gosub error_prompt
            init(" ") readkey$, desc$, beg_mod$, end_mod$
        return


L50850: REM Beginning and Ending Reason Code   BEG_REA$, END_REA$
        init(" ") readkey$, desc$
        if beg_rea$ <> " " then lookup_rea
        all_rea
             beg_rea$, end_rea$ = "ALL"
        return
        lookup_rea
            if beg_rea$ = "ALL" then goto all_rea
            str(readkey$,1%,9%) = "PLAN BKRE"
            if beg_rea$ <> " " then str(readkey$,10%,3%) = beg_rea$

               call "PLOWCODE" (#3, readkey$, desc$, 9%, .30, f1%(3))
                    if f1%(3) <> 1 then goto L50870

               beg_rea$ = str(readkey$,10%,3%)
               if end_rea$ <> " " then goto end_rea
                  end_rea$ = beg_rea$
               return
        end_rea
           str(readkey$,10%,3%) = end_rea$

           call "PLOWCODE" (#3, readkey$, desc$, 9%, .30, f1%(3))
                 if f1%(3) <> 1 then goto L50870

               end_rea$ = str(readkey$,10%,3%)
           if end_rea$ < beg_rea$ then goto L50870
        return
L50870:     errormsg$ = "(Error) - Invalid Reason Code ?"
            gosub error_prompt
            init(" ") readkey$, desc$, beg_rea$, end_rea$
        return



        find_default
            init(" ") or_key$, def_load$
            str(or_key$,1%,6%) = str(x$,1%,6%)
            read #1,key > or_key$, using L50700, def_load$,        ~
                                                 eod goto L50710
L50700:        FMT POS(94), CH(5)
L50710: return

        REM *************************************************************~
            *          F o r m a t   S t a t e m e n t s                *~
            *-----------------------------------------------------------*~
            * Image Statements                                          *~
            *************************************************************


L55065: %+---------------------------------------------------------------~
        ~--------------+

REM L55095 %!---------------------------------------------------------------~
        ~--------------!

L55120: %! Date: ##########    ########################################  ~
        ~   Page: #### !

L55150: %! From: ########## to ##########   #############################~
        ~#    EWDPLN52 !

L55180: %! Beg Load: ##### ###############            End Load: ##### ###~
        ~############  !

L55210: %!Load #####          Customer #########          Sale Ord ######~
        ~##            !
L55240: %!P.O. Num ################                Ln ###         Time: #~
        ~#######       !
L55270: %!Part Num #########################       Descr ################~
        ~##############!
L55280: %!Or Qty ####         Sh Qty ####          Ln Status #########   ~
        ~              !
L55370: %!User Id ###   ####################       Auth Id  ###   #######~
        ~############# !
L55380: %!Area Code #  ##############################     Seq No  #####  ~
        ~              !
L55390: %!Reas Code ###    ##############################                ~
        ~              !
L55400: %!Text: #################### #################### ###############~
        ~#####         !

L55410: %! **Total BackOrdered: ##########     Shipped: ############     ~
        ~              !


L55300: %+---------------------------------------------------------------~
        ~--------------+
L55310: %!---------------------------------------------------------------~
        ~--------------!
L55320: %!Date: ########## @ ########     ############################## ~
        ~    Page: ### !
L55330: %!From: ########## to ##########                                 ~
        ~              !
L55340: %!Beg Load : #####  End Load: #####                              ~
        ~              !
L55350: %!Load !Sales Ord!Line! Qty!Remove Dte!User!<----- Name ------->!~
        ~<-Remove Type>!
L55360: %!#####! ########!### !####!##########! ###!####################!~
        ~   #########  !


        REM *************************************************************~
            *          S p e c i a l   S u b r o u t i n e s            *~
            *-----------------------------------------------------------*~
            * Subroutines                                               *~
            *************************************************************

        print_header                         /* GENERIC REPORT HEADING */
          init(" ") rpt_time$
          call "TIME" (rpt_time$)
          if lcnt% <> 99% then print using L55065
          page_no% = page_no% + 1%
          print page
          print using L55065
          print using L55120, date$, company$, page_no%
          print using L55150, beg_date$, end_date$, print_title$
          print using L55180, beg_load$, beg_load_d$, end_load$,         ~
                              end_load_d$
REM          print using L55095
          if lcnt% <> 99% then print using L55065
          if lcnt% <> 99% then lcnt% = 5%       ~
          else lcnt% = 4%
        return

        print_hdr_removal                        /* (EWD003)           */
          init(" ") rpt_time$
          call "TIME" (rpt_time$)
          if lcnt% <> 99% then print using L55300
          page_no% = page_no% + 1%
          print page
          print using L55300
          print using L55320, date$, rpt_time$, print_title$, page_no%
          print using L55330, beg_date$, end_date$
          print using L55340, beg_load$, end_load$
          print using L55310
          print using L55350
          lcnt% = 6%
        return

        print_detail
          if lcnt% > 56% then gosub print_header
          if sav_so$ = or_so$ then L60000         /*  (EWD005)  */
             print using L55065
             sav_so$ = or_so$
             lcnt% = lcnt% + 1%

                                                            /* (EWD001) - Begin   */
L60000:  print using L55065
REM          if sav_load$ = ld_load$ then goto L60000
REM          print using L55270, ld_load$, or_cuscode$, or_so$, or_po$, ln$, ~
REM                              part$, part_D$, ord_qty$, shp_qty$,         ~
REM                              line_status$

          print using L55210, ld_load$, or_cuscode$, or_so$
          print using L55240, or_po$, ln$, bk_time$
          print using L55270, part$, part_D$
          print using L55280, ord_qty$, shp_qty$, line_status$

          sav_load$ = ld_load$
          lcnt% = lcnt% + 5%
          if lcnt% > 56% then gosub print_header
          goto L60010
REM L60000
REM          print using L55270, " ", or_cuscode$, or_so$, or_po$, ln$,      ~
REM                              part$, part_d$, ord_qty$, shp_qty$,         ~
REM                              line_status$

          print using L55210, ld_load$, or_cuscode$, or_so$
          print using L55240, or_po$, ln$
          print using L55270, part$, part_D$
          print using L55280, ord_qty$, shp_qty$, line_status$
          lcnt% = lcnt% + 4%
          if lcnt% > 56% then gosub print_header
                                                  /* (EWD001) - End     */
                                                          /*  (EWD005)  */
L60010:   if str(line_status$,1%,4%) = "Back" then gosub print_backorder

REM          lcnt% = lcnt% + 5%
        return

        print_backorder                                    /*  (EWD005)  */
           print using L55370, rm_usr$, usr_name$, auth_id$, auth_name$
           print using L55380, area_code$, area_desc$, bk_seq$
           print using L55390, reas_code$, reas_desc$
           print using L55400, text$(1), text$(2), text$(3)
           lcnt% = lcnt% + 4%
           if lcnt% > 56% then gosub print_header
        return

        print_total
           if lcnt% > 56% then gosub print_header
           tot_bck% = 0%
           tot_bck% = tot_ord% - tot_shp%
           print using L55410, tot_bck%, tot_shp%
           print using L55065
           lcnt% = lcnt% + 2%
        return                                           /*  (EWD005)  */


        print_dtl_removal                         /* (EWD003) - Begin   */
          if lcnt% > 56% then gosub print_hdr_removal
             print using L55310
             print using L55360, rm_load$, rm_so$, rm_ln$, rm_qty$,       ~
                                 rm_date$, rm_usr$, rm_name$, rm_desc$
             lcnt% = lcnt% + 2%
        return                                    /* (EWD003)           */

        gen_report
            gosub open_work
            init(" ") dsp_msg$
            tot_shp%, tot_ord% = 0%
            dsp_msg$ = "Analysis of " & sc_sel_d$
            call "SHOSTAT" (dsp_msg$)
            cnt% = 0% : cnt$ = "Line Items Checked [ XXXXXX ]"
            page_no% = 0%
            lcnt%    = 99%
            date$    = date  :  call "DATFMTC" (date$)
            call "SETPRNT" (" ", "EWDB", 2000%, 0%)
            print_title$ = "S.O. Backorder Report by Load"
            call "FMTTITLE" (print_title$, " ", 12%)
            select printer (134)
REM         init(" ") or_key$, sav_load$
            or_key$, sav_load$ = all(hex(00))
            if beg_dte$ = "ALL" then goto gen_next
            str(or_key$,1%,6%) = beg_dte$
        gen_next                           /* (EWD001) - (APCPLNOR)    */
            read #1,key > or_key$, using L61000, or_rec$,                ~
                                                        eod goto gen_done
L61000:        FMT CH(170)
            or_key$ = str(or_rec$,1%,51%)         /* Set Primary Key   */
            cnt% = cnt% + 1%
            if mod(cnt%,100%) <> 0 then goto L61005
                 convert cnt% to str(cnt$,22%,6%), pic(######)

                 print at(03,25);hex(84);cnt$;

L61005:     if end_dte$ = "ALL" then goto L61045   /*  (EWD005)        */

            if str(or_rec$,1%,6%) > end_dte$ then goto gen_done
L61045:     ld_load$ = str(or_rec$,94%,5%)        /* Set Load Number   */

            if beg_load$ = "ALL" then goto L61015   /* (EWD005)        */
            if ld_load$ < beg_load$ then goto gen_next  /* (EWD005)    */
            if ld_load$ > end_load$ then goto gen_next  /* (EWD005)    */

L61015:     or_st$ = str(or_rec$,60%,2%)
                                                  /* Check Load Status */
                                                  /* for all Planned   */
                                                  /* Sales Orders      */
            if or_st$ < "03" then goto gen_next
                                                  /* Check all Sales   */
                                                  /* orders for Back   */
                                                  /* Orders & Removals */
               or_cuscode$ = str(or_rec$,27%,9%)  /* Customer Code     */
               or_so$      = str(or_rec$,52%,8%)  /* Sales Order       */
               or_po$      = str(or_rec$,36%,16%) /* P.O. Number       */

            init(" ") bck_key$                    /* (BCKLINES)        */
            str(bck_key$,1%,8%) = or_so$          /* Set Sales Order   */
         scan_so                                  /* Scan Sales Orders */
            read #2,key > bck_key$, using L61010, bck_rec$,              ~
                                                  eod goto gen_next
L61010:        FMT CH(200)
            if str(bck_rec$,10%,8%) <> or_so$ then goto gen_next
               bck_key$ = str(bck_rec$,10%,19%)  /* primary Key       */
               ln$      = str(bck_rec$,26%,3%)   /* Save Line Item No.*/
               get str(bck_rec$,93%,40%), using L61020, ord_qty, shp_qty,~
                                  opn_qty, sch_qty, all_qty, pre_qty
L61020:           FMT 6*PD(14,4)
                                        /* Primary Test for Backorder */
                                                 /* (EWD001) - Begin  */
               ln%     = 0% : tst_qty = 0.0      /* Initialize        */
               convert ln$ to ln%, data goto L61025
L61025
               ln$ = "   "              /* Note - Leading Space in 1,1*/
               convert ln% to str(ln$,2%,2%), pic(00)
                                                 /* Zero Fill for Two */
               gosub check_removal               /* Check all S.O.'s  */
               gosub check_report                /* (EWD004) Select   */
               if sc_sel% = 0% then goto scan_so /* (EWD004) Skip     */

               if removal% = 0% then goto L61030
                  tst_qty1 = (ord_qty - rm_qty%) /* BOL not yet cut   */
                  if or_st$ > "17" then goto L61035
                     tst_qty = tst_qty1
                     goto L61040                 /* For Removal skip  */

L61030:        if or_st$ < "18" then goto scan_so /* No BOL Cut Yet   */

L61035:           if or_st$ = "18" then tst_qty = (shp_qty + pre_qty)
                  if or_st$ > "18" then tst_qty = shp_qty
                  if removal% = 0% then tst_qty1 = tst_qty

L61040:           if tst_qty1 = tst_qty then goto L61100
                     gosub disp_quantities
                     tst_qty = tst_qty1          /* Removes 'Exist'   */
                     line_status$ = "BOL Error"  /* Removes <> BOL    */
                                                 /* Pass Test then S.O*/
                                                 /* Shipped Complete  */
L61100:           if ord_qty = tst_qty then goto scan_so
                                                 /* Not Valid Until   */
                                                 /* BOL has been Cut  */
                                                 /* S.O. Not Complete */
                  convert tst_qty to shp_qty$, pic(####)
                  convert ord_qty to ord_qty$, pic(####)
                  part$   = str(bck_rec$,32%,25%)  /* MFG Part No.    */
                                                   /*  (EWD005)       */
                  if beg_mod$ = "ALL" then goto L61120
                     if str(part$,1%,3%) > end_mod$ or               ~
                           str(part$,1%,3%) < beg_mod$ then goto scan_so
                                                   /*  (EWD005)       */
L61120:           part_d$ = str(bck_rec$,57%,32%)  /* Description     */

                  gosub check_backorder            /*  (EWD005)       */
                                                   /*  (EWD005)       */
                  if beg_rea$ = "ALL" then goto L61125
                     if reas_code$ > end_rea$ or               ~
                           reas_code$ < beg_rea$ then goto scan_so
                                                   /*  (EWD005)       */
L61125:           gosub set_sort                   /*  (EWD005)       */
                                                 /* (EWDWORK) File    */
                  write #6, using L60130, wrk_key$, ld_load$, or_cuscode$, ~
                            or_so$, ln$, or_po$, part$, part_d$, ord_qty$, ~
                            shp_qty$, line_status$, rm_usr$, area_code$,   ~
                            reas_code$, auth_id$, bk_time$, text$(), bk_seq$

L60130:              FMT CH(32), CH(5), CH(9), CH(8), CH(3), CH(16), CH(25), ~
                         CH(30), CH(4), CH(4), CH(9), CH(3), CH(1), CH(3),   ~
                         CH(3), CH(8), 3*CH(20), CH(5)
                                           /* Write Sorted Report Record*/
                  goto scan_so
        gen_done
            init(" ") wrk_key$             /* Pass(2) Create Report     */
L60200:     read #6,key > wrk_key$, using L60130, wrk_key$, ld_load$,  ~
                            or_cuscode$, or_so$, ln$, or_po$, part$,   ~
                            part_d$, ord_qty$, shp_qty$, line_status$, ~
                            rm_usr$, area_code$, reas_code$, auth_id$, ~
                            bk_time$, text$(), bk_seq$, eod goto L60300

REM L60250         FMT CH(32), CH(16), CH(25), CH(30), CH(4), CH(4), CH(9), CH(3)

REM            ld_load$   = str(wrk_key$,1%,5%)
REM            or_cuscode$= str(wrk_key$,6%,9%)
REM            or_so$     = str(wrk_key$,15%,8%)
REM            ln$        = str(wrk_key$,23%,3%)

            if str(line_status$,1%,4%) <> "Back" then goto not_back

               ord_qty%, shp_qty% = 0%
               convert ord_qty$ to ord_qty%, data goto not_back

               convert shp_qty$ to shp_qty%, data goto not_back

               tot_ord% = tot_ord% +  ord_qty%
               tot_shp% = tot_shp% +  shp_qty%
               gosub get_descr                          /*  (EWD005)       */
               gosub get_names                          /*  (EWD005)       */

not_back
            gosub print_detail             /* Generate Report           */
            goto L60200
L60300
            print using L55065
            gosub print_total
            close printer
            gosub delete_work
        return clear all
        goto inputmode


        gen_rpt_removal                           /* (EWD003) - Begin     */
            gosub open_work
            init(" ") dsp_msg$
            dsp_msg$ = "Audit Report for " & sc_sel_d$
            call "SHOSTAT" (dsp_msg$)
            cnt% = 0% : cnt$ = "Records Checked [ XXXXXX ]"
            page_no% = 0%
            lcnt%    = 99%
            date$    = date  :  call "DATFMTC" (date$)
            call "SETPRNT" (" ", "EWDB", 2000%, 0%)
            print_title$ = "Backorder Audit Report by Load"
            call "FMTTITLE" (print_title$, " ", 12%)
            select printer (134)
            init(" ") rm_key$
            if beg_load$ = "ALL" then goto gen_rpt_nxt     /* (EWD005)   */
            str(rm_key$,1%,5%) = beg_load$
        gen_rpt_nxt
            init(" ") rm_load$, rm_so$, rm_ln$, rm_qty$, rm_dte$, rm_date$,~
                      rm_type$, rm_desc$, rm_usr$, rm_name$
            read #10,key > rm_key$, using L60400, rm_key$, rm_qty%,      ~
                                    rm_dte$, rm_usr$, rm_type$,          ~
                                                   eod goto gen_rpt_done
L60400:        FMT CH(16), BI(2), CH(6), CH(3), CH(2)
            gosub check_report                    /* (EWD004)       */
            if sc_sel% = 0% then goto gen_rpt_nxt /* (EWD004)       */

            rm_load$ = str(rm_key$,1%,5%)
            rm_so$   = str(rm_key$,6%,8%)
            rm_ln$   = str(rm_key$,14%,3%)
            convert rm_qty% to rm_qty$, pic(0000)
            rm_date$ = rm_dte$
            call "DATEFMT" (rm_date$)
            rm_desc$ = "Backorder"
            if rm_type$ = "RM" then rm_desc$ = "*Removed*"
            read #7,key = rm_usr$, using L60410, rm_name$, eod goto L60420
L60410:        FMT XX(3), CH(20)

L60420:     cnt% = cnt% + 1%
            if mod(cnt%,25%) <> 0 then goto L60450
                 convert cnt% to str(cnt$,19%,6%), pic(######)

                 print at(03,25);hex(84);cnt$;

L60450:     if end_load$ = "ALL" then goto L06050          /* (EWD005)   */
            if rm_load$ > end_load$ then goto gen_rpt_done
L06050:     gosub print_dtl_removal             /* Generate Audit Report */
            goto gen_rpt_nxt
        gen_rpt_done
            print using L55300
            close printer
        return clear all
        goto inputmode                          /* (EWD003) - End        */

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        disp_quantities
           return

           convert ord_qty to ord_qty$, pic(####)
           convert shp_qty to shp_qty$, pic(####)
           convert opn_qty to opn_qty$, pic(####)
           convert sch_qty to sch_qty$, pic(####)
           convert all_qty to all_qty$, pic(####)
           convert pre_qty to pre_qty$, pic(####)
           convert rm_qty% to rm_qty$,  pic(####)
           convert tst_qty to tst_qty$, pic(####)
           convert tst_qty1 to tst_qty1$, pic(####)
           comp% = 2%
           hdr$ = "SO= " & or_so$ & " Cus= " & or_cuscode$
           hdr$ = hdr$ &" ST= "& or_st$ & " ld= " &ld_load$
           msg$(1%) = "Ord = "&ord_qty$&"  Shp = "&shp_qty$&" Rm= "&rm_qty$
           msg$(2%) = "Opn = "&opn_qty$&"  Sch = "&sch_qty$&" ts= "&tst_qty$
           msg$(3%) = "All = "&all_qty$&"  Pre = "&pre_qty$&" t1= "&tst_qty1$
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_work

            call "WORKOPEN" (#6, "IO", 500%, f2%)
            if f2% <> 0% then goto L62430
        return
L62430:     call "SHOSTAT" ("Error - Cannot Open (EWDPLN52)") : stop
        return
        delete_work
            call "FILEBGON" (#6)
        return

        check_removal                            /* Set Line Status    */
            removal% = 0% : rm_qty% = 0%         /* for Report Print   */
            init(" ") rm_key$, line_status$, rm_type$
            line_status$        = " B.O.L.  "    /* (SHPACTIN) Change  */
            str(rm_key$,1%,5%)  = ld_load$       /* Load Number        */
            str(rm_key$,6%,8%)  = or_so$         /* Sales order        */
            str(rm_key$,14%,3%) = ln$            /* line Item          */
                                                 /* (EWD005) - ADD USER*/
            read #10,key = rm_key$, using L62450, rm_qty%, rm_usr$,     ~
                                    rm_type$, eod goto check_removal_done
L62450:        FMT POS(17), BI(2), POS(25), CH(3), POS(28), CH(2)
            line_status$ = " Removed "
            if rm_type$ = "BK" then line_status$ = "Backorder"
            removal% = 1%                        /* S.O. Adjustment    */
        check_removal_done                       /* No Adjustments     */

        return

        check_report                             /* (EWD004) - Begin   */
            sc_sel% = 0%
            if sc_sel$ = "1" and rm_type$ = "BK" then goto L62460
            if sc_sel$ = "2" and rm_type$ <> "BK" then goto L62460
            if sc_sel$ = "3" then goto L62460
        return
L62460:     sc_sel% = 1%
        return
                                                 /* (EWD004) - End     */
        purge_data
            if userid$ <> "RHH" then return
            call "SHOSTAT" ("Purging Data")
            cnt$ = "Line Items Checked [ xxxxxxx ]"
            cnt% = 0%
            init(" ") rm_key$
        purge_data_nxt
            read #10,hold,key > rm_key$, using L62500, rm_rec$,           ~
                                        eod goto purge_data_done
L62500:        FMT CH(32)
            rm_key$ = str(rm_rec$,1%,16%)
            cnt% = cnt% + 1%
            if mod(cnt%,100%) <> 0 then goto L62550
               convert cnt% to str(cnt$,22%,7%), pic(#######)
               print at(03,25);hex(84);cnt$;

L62550:     if str(rm_rec$,19%,6%) > end_dte$ then goto purge_data_nxt
               delete #10
               goto purge_data_nxt
        purge_data_done
        return
                                                  /* (EWD001) - End    */


        open_error                                /* (EWD005) - BEGIN  */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
            err% = 0%
        return

        check_backorder
          init(" ") bk_key$, area_code$, reas_code$, auth_id$, bk_time$, ~
                    text$(), area_desc$, reas_desc$, auth_name$, usr_name$
          str(bk_key$,1%,5%) = ld_load$
          str(bk_key$,6%,8%) = or_so$
          str(bk_key$,14%,3%) = ln$

          read #11, key = bk_key$, eod goto backorder_done

             get #11, using L62580, area_code$,  ~
                                    reas_code$,  ~
                                    auth_id$,    ~
                                    bk_time$,    ~
                                    text$(),     ~
                                    bk_seq$

L62580:         FMT CH(1), XX(1), XX(1), CH(3), POS(23), CH(3), CH(8), ~
                    3*CH(20), CH(5)

        return
        backorder_done
        return

        get_descr
          init(" ") readkey$
          str(readkey$,1%,9%) = "PLAN BKAR"
          str(readkey$,10%,1%) = area_code$
          read #3,key = readkey$, using L62590, area_desc$, eod goto area_done
L62590:          FMT POS(25), CH(30)
        area_done
          init(" ") readkey$
          str(readkey$,1%,9%) = "PLAN BKRE"
          str(readkey$,10%,3%) = reas_code$
          read #3,key = readkey$, using L62590, reas_desc$, eod goto reas_done

        reas_done
        return

        get_names
          read #7,key = rm_usr$, using L60410, usr_name$, eod goto auth_name

        auth_name
          read #7,key = auth_id$, using L60410, auth_name$, eod goto name_done

        name_done
        return

        set_sort
            init(" ") wrk_key$
            if sel$ <> "1" then goto not_one
               str(wrk_key$,1%,5%) = ld_load$
               str(wrk_key$,6%,9%) = or_cuscode$
               str(wrk_key$,15%,8%) = or_so$
               str(wrk_key$,23%,3%) = ln$
               str(wrk_key$,26%,1%) = area_code$
               str(wrk_key$,27%,3%) = reas_code$
               str(wrk_key$,30%,3%) = str(part$,1%,3%)
        not_one
            if sel$ <> "2" then goto not_two
               str(wrk_key$,1%,3%) = str(part$,1%,3%)
               str(wrk_key$,4%,3%) = reas_code$
               str(wrk_key$,7%,1%) = area_code$
               str(wrk_key$,8%,8%) = or_so$
               str(wrk_key$,16%,3%) = ln$
               str(wrk_key$,19%,9%) = or_cuscode$
               str(wrk_key$,28%,5%) = ld_load$
        not_two
            if sel$ <> "3" then goto not_three
               str(wrk_key$,1%,1%) = area_code$
               str(wrk_key$,2%,3%) = reas_code$
               str(wrk_key$,5%,8%) = or_so$
               str(wrk_key$,13%,3%) = ln$
               str(wrk_key$,16%,9%) = or_cuscode$
               str(wrk_key$,25%,5%) = ld_load$
               str(wrk_key$,30%,3%) = str(part$,1%,3%)
        not_three
        return
                                                        /* (EWD005) - End    */
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *************************************************************

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end


