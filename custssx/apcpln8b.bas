        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN8B - Program (APCPLN05)        *~
            *  Creation Date     - 05/01/96                             *~
            *  Last Modified Date- 06/09/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Modified By  - Royal H. Hoffman                     *~
            *                                                           *~
            *       Note - Called by (BCKFASTR)                         *~
            *                                                           *~ 
            *  Description       - New Utility to Display Sales Order   *~
            *                      Information. Header and Line Item    *~
            *                      Data can both be Displayed.          *~
            *                      Note - No Line Item No. Then You Get *~
            *                             only S.O. Header Data Only    *~
            *                                                           *~
            *                           - With a Line Item No. You then *~
            *                             Get Both Header and Line Item *~
            *                             Data for Line Item Entered.   *~
            *                                                           *~
            *                           - For (All) Double Displays the *~
            *                             Header Info is on the (Left). *~
            *                             and the Line Item Info is on  *~
            *                             the (Right).                  *~
            *                                                           *~
            *  Code Tables Used  - PLAN REGN, PLAN STAT, ROUTCODE,      *~
            *                      PLAN DELV, PLAN HOWS                 *~
            *                                                           *~
            *  Subroutine Used   - (APCPL3SB) - Display Customer Info.  *~
            *                                                           *~
            *  Special Comments  - PF(7) - Display S.O. Header Text     *~
            *                      PF(8) - Display S.O. Line Item Text  *~
            *                      PF(9) - Display Scheduling Detail    *~
            *                      PF(10)- Display Customer Information *~
            *                                                           *~
            *                      PF(14)- Save Status Code Change for  *~
            *                              Either S.O. or Line Item     *~
            *                              Selected. Controlled by      *~
            *                              ID$() Array - RHH & CEP      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/17/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 10/14/97 ! Mod to add new Subroutine 'DELETE_IT' to ! RHH *~
            *          !   remove a S.O. from New Planning. Caelus!     *~
            *          !   is not touched.                        !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 05/05/98 ! (EWD001) to Lookup Sales Order using the ! RHH *~
            *          !   warranty ID. New File (APCPLNWT)       !     *~
            * 08/18/99 ! (EWD002) - Mod to replace 'OPENCHCK'     ! RHH *~
            *          !            with new 'EWDOPEN' Subroutine !     *~
            *          !            to improve HP speed.          !     *~                                            
            * 06/09/06 ! (AWD003) - Mod for file Change to        ! RHH *~
            *          !            APCPLNWT                      !     *~
            *************************************************************

          sub "APCPLN8B" ( opn%, /* Sales Order Display                */~
                          #2, /* GENCODES-Master Code Tables File      */~
                          #4, /* CUSTOMER-Customer Master Schedule File*/~
                          #6, /* SLMMASTR-Salesman Master File         */~
                          #7) /* TXTFILE -System Text File             */

        dim                              /* (APCPLNOR) - FILE          */~
            or_due$8,                    /* S.O. Due Date/Delivery Date*/~
            or_region$2, or_region_d$30, /* Customer Region Code       */~
            or_route$5, or_route_d$30,   /* Customer Route Code        */~
            or_zip$9,                    /* Customer Zip Code          */~
            or_drop$2,                   /* Customer Drop Number       */~
            or_cuscode$9,or_cuscode_d$30,/* Customer Number            */~
            or_po$16, cuscode$9,         /* Customer P.O. Number       */~
            or_so$8, or_line$2,          /* Customer S.O. Number       */~
            dt_ref$8,                    /* Customer Warranty Number   */~
            dt_key$23, dt_st$2, dt_date$6, /* LOOKUP_DTL               */~
            or_status$2, or_status_d$30, /* Current S.O. Stat PLAN STAT*/~
            sc_part$25, sc_special$10,   /* Part No. , Special Flags   */~
            sc_new_part$25, sc_bar$18,   /* New Part Warr      (AWD003)*/~
            sc_sub_part$20,              /* New Sub Part No.   (AWD003)*/~    
            sc_st$2, sc_st_d$30,         /* Current S.O. Stat PLAN STAT*/~
            or_dte$8,sc_ldte$8,          /* Date Assoc. with Stat Chg  */~
            sav_status$2, sav_dte$8,     /* Save S.O. Status and Date  */~
            sav_desc$30,                 /* Save Status Description    */~
            or_inv$8,                    /* APC Invoice Assoc. with SO */~
            or_chk$8,                    /* APC Check Assoc. with Invoi*/~
            or_sls$4, or_sls_d$30,       /* APC Salesman Code          */~
            or_fob$2, or_fob_d$30,       /* Cust Delivery Cde PLAN DEL?*/~
            or_hows$2, or_hows_d$30,     /* Spec. Instr (PLAN HOWS)    */~
            or_load$5, or_load_d$30,     /* Load N. Sched./Assigned    */~
            or_units$8, sc_units$8,      /* Total Loading Units S.O.   */~
            or_value$8, sc_value$8,      /* Total Price S.O. (Net)     */~
                                         /* Total Cost S.O.            */~
            or_make$4,  sc_make$4,       /* Total Make Quantity S.O.   */~
            or_pull$4,  sc_pull$4,       /* Total Pull Quantity S.O.   */~
            or_date$8,  sc_pull1$4,      /* Date S.O. Created          */~
            or_chg$8,                    /* Date S.O. Last Modified    */~
            or_userid$3,                 /* S.O. Last Modified By User */~
            or_bol$8,                    /* Date B.O.L. Created        */~
            or_text$4, sc_txt$4,         /* S.O. Header Text Id        */~
                                         /* Actual No. Delivery Days   */~
            or_special$10,               /* Special Product Flags      */~
            or_fil$1, ll_key$51,         /* Filler / Primary Key       */~
            sc_key$10, or_rec$170,       /* (APCPLNSC) PRIMARY KEY     */~
            header$79, text$(113%,1%)70, /* Text Array                 */~
            textid$4, txt$4              /* Text Id's                  */

        dim sp$(10%)7,                   /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN    (EWD002)*/~
            ld_dtp1$8, ld_rec$128,       /* Planning Production Date   */~
            ld_dtp2$8,                   /* Planning Completion Date   */~
            ld_dtp3$8,                   /* Planning Loading Date      */~
            hdr$40, msg$(3%)79,          /* Askuser - Var's            */~
            readkey$50, desc$30,         /* Generic Key                */~
            cursor%(2%), scr$55,         /* Cursor location for edit   */~
            edtmessage$79, status$21,    /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3, id$(5%)3           /* Current User Id/Mod Id's   */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%)                     /* = 1 if READ was successful */

        dim sd_key$22, sd_units$2,       /* (APCPLNSD) - Detail File   */~
            d1$(15%)3, d2$(15%)2,        /*                            */~
            d3$(15%)2, d4$(15%)3,        /*                            */~
            d5$(15%)2, d6$(15%)8,        /*                            */~
            d7$(15%)3, pp1$(10%)9,       /*                            */~
            pp2$(10%)9                   /*                            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                            /* (AWD003) */
            apc$   = "(AWD)S.O. Load Lookup Utility 06/09/2006"
            pname$ = "APCPLN8B - Rev: R1.00"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNOR ! (NEW) S.O. Header Histroy                *~
            * #2  ! GENCODES ! Master Code Tables File                  *~
            * #3  ! APCPLNSC ! Planning Master Schedule File            *~
            * #4  ! CUSTOMER ! Customer Master Schedule File            *~
            * #5  ! APCPLNLD ! Load Master File                         *~
            * #6  ! SLMMASTR ! Salesman Master File                     *~
            * #7  ! TXTFILE  ! System Text File                         *~
            * #8  ! APCPLNSD ! S.O. Scheduling Dept. Detail             *~
            * #9  ! APCPLNDT ! Production Master Detail File            *~
            * #10 ! APCPLNWT ! (EWD001) - Warranty S.O. Cross-Ref       *~
            *     !          !                                 (AWD003) *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

            select #3,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33

            select #5,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15

            select #8,  "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =    32,             ~
                        keypos =    1, keylen =  22

            select #9,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

                                                   /* (EWD001) - Begin */
                                                   /* (AWD003)         */
            select #10, "APCPLNWT",                                      ~
                         varc,    indexed,  recsize =  128,              ~
                         keypos =    1, keylen =  8,                     ~
                         alt key 1, keypos =  9, keylen = 10, dup,       ~
                             key 2, keypos =  9, keylen = 18
                                                   /* (AWD003)         */    
                                                   /* (EWD001) - End   */ 
            call "SHOSTAT" ("Initialization")

                                                        /* (EWD002)    */
            filename$ = "APCPLNOR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SLMMASTR" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "TXTFILE" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSD" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNWT" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
                                                        /* (EWD002)    */

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            opn% = 0%

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            pp1$(1%) = "Tempered "   :   id$(1%) = "RHH"
            pp1$(2%) = " Diamond "   :   id$(2%) = "CEP"
            pp1$(3%) = "Spec. Lit"   :   id$(3%) = "   "
            pp1$(4%) = "Wood/Surr"   :   id$(4%) = "   "
            pp1$(5%) = " Sample  "   :   id$(5%) = "   "
            pp1$(6%) = " Display "   :   id_max% = 5%
            pp1$(7%) = " U.P.S.  "
            pp1$(8%) = " Parts"

            pp2$(1%) = "Depart.  "
            pp2$(2%) = "Process  "
            pp2$(3%) = "Shift    "
            pp2$(4%) = "Model Cod"
            pp2$(5%) = "Units Cod"
            pp2$(6%) = " M H P U "
            pp2$(7%) = "Seq No"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 1%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 12% then gosub delete_it
                  if keyhit%  = 14% then gosub dataput
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% <> 5% then fieldnr% = 0% else fieldnr% = 2%
            for j% = 1% to id_max%
                if userid$ = id$(j%) then goto L11200
            next j%
            fieldnr% = 0%

L11200:     if fieldnr% < 2% or fieldnr% > 2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11240:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11240
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11240
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
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
         "Enter the Applicable Lookup Information for at Least (1) '*'?",~
         "Enter a Valid Status Code for Change?                        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, desc$, or_so$,   ~
                      or_line$, or_due$, or_po$, ld_dtp1$, or_inv$,      ~
                      ld_dtp2$, or_chk$, ld_dtp3$, or_cuscode$,          ~
                      or_cuscode_d$, or_load$, or_load_d$, or_drop$,     ~
                      or_region$, or_region_d$, or_route$, or_route_d$,  ~
                      or_sls$, or_sls_d$, or_fob$, or_fob_d$, or_hows$,  ~
                      or_hows_d$, or_units$, or_make$, or_pull$,         ~
                      or_userid$, or_chg$, or_bol$, or_status$, or_dte$, ~
                      or_status_d$, ll_key$, hdr$, msg$(), sc_st$,       ~
                      sc_st_d$, sc_make$, sc_pull$, sc_units$, sc_value$,~
                      or_text$, sc_txt$, sav_status$, sav_dte$, cuscode$,~
                      or_value$, dt_ref$, sc_pull1$, sav_desc$
            for i% = 1% to 10%
              sp$(i%) = "No /No "
            next i%
            or_cost = 0.0 : or_days% = 0%
            line_item% = 0%
            init (hex(ff)) textid$, txt$ , text$()
            call "TXTFUTIL" (#7, f2%(7%), "INTL", textid$)
            scr$="Hdr/Ln Item Disp-*Hdr Disp*                           "
        return

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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
        get #1, using L35040  ,                                           ~
            or_due$,                     /* S.O. Due Date/Delivery Date*/~
            or_region$,                  /* Customer Region Code       */~
            or_route$,                   /* Customer Route Code        */~
            or_zip$,                     /* Customer Zip Code          */~
            or_drop$,                    /* Customer Drop Number       */~
            or_cuscode$,                 /* Customer Number            */~
            or_po$,                      /* Customer P.O. Number       */~
            or_so$,                      /* Customer S.O. Number       */~
            or_status$,                  /* Current S.O. Stat PLAN STAT*/~
            or_dte$,                     /* Date Assoc. with Stat Chg  */~
            or_inv$,                     /* APC Invoice Assoc. with SO */~
            or_chk$,                     /* APC Check Assoc. with Invoi*/~
            or_sls$,                     /* APC Salesman Code          */~
            or_fob$,                     /* Cust Delivery Cde PLAN DEL?*/~
            or_hows$,                    /* Spec. Instr (PLAN HOWS)    */~
            or_load$,                    /* Load N. Sched./Assigned    */~
            or_units,                    /* Total Loading Units S.O.   */~
            or_value,                    /* Total Price S.O. (Net)     */~
            or_cost,                     /* Total Cost S.O.            */~
            or_mak%,                     /* Total Make Quantity S.O.   */~
            or_pul%,                     /* Total Pull Quantity S.O.   */~
            or_date$,                    /* Date S.O. Created          */~
            or_chg$,                     /* Date S.O. Last Modified    */~
            or_userid$,                  /* S.O. Last Modified By User */~
            or_bol$,                     /* Date B.O.L. Created        */~
            or_text$,                    /* S.O. Header Text Id        */~
            or_days%,                    /* Actual No. Delivery Days   */~
            or_special$,                 /* Special Products Flags     */~
            or_fil$                      /* Filler Area                */

            call "DATEFMT" (or_due$)         /* Set Delivery Date      */
            call "DATEFMT" (or_dte$)         /* Set Status Change Date */
            call "DATEFMT" (or_date$)        /* Set S.O. Create Date   */
            call "DATEFMT" (or_chg$)         /* Set Last S.O. Change   */
            call "DATEFMT" (or_bol$)         /* Set Date BOL Created   */

            for i% = 1% to 10%               /* Check Special Flags    */
              if str(or_special$,i%,1%) = "Y" then                       ~
                                              str(sp$(i%),1%,3%) = "Yes"
            next i%

            sav_status$ = or_status$         /* Save S.O. Status Code  */
            gosub lookup_load
            gosub lookup_line_item
            gosub lookup_customer
            gosub lookup_salesman
            gosub lookup_region
            gosub lookup_route
            gosub lookup_fob
            gosub lookup_hows
            gosub lookup_status
            sav_desc$ = or_status_d$         /* Save Status Description*/
                                             /* May Be S.O./Line Item  */

            convert or_units to or_units$, pic(#####.##)
            convert or_value to or_value$, pic(#####.##)
            convert or_mak%  to or_make$,  pic(####)
            convert or_pul%  to or_pull$,  pic(####)

        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            init(" ") sc_key$, or_rec$, or_dte$
            or_dte$ = date
            if line_item% = 1% then goto dataput_line
            call "SHOSTAT" ("Updating Header Status Info Only")
            read #1,hold,key 4% = or_so$, using L35350, or_rec$,          ~
                                                    eod goto dataput_done
            str(or_rec$,60%,2%) = or_status$
            str(or_rec$,62%,8%) = or_dte$
            put #1, using L35350, or_rec$
            rewrite #1
            goto dataput_done
        dataput_line
            call "SHOSTAT" ("Updating Line Item Status Info Only")
            str(sc_key$,1%,8%) = or_so$
            str(sc_key$,9%,2%) = or_line$
            read #3,hold,key = sc_key$, using L35380, or_rec$,            ~
                                                    eod goto dataput_done
            str(or_rec$,110%,2%) = or_status$
            str(or_rec$,112%,8%) = or_dte$
            put #3, using L35380, or_rec$
            rewrite #3
        dataput_done
        return clear all
        goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* APCPLNOR - New File Layout */
L35040:     FMT CH(08),                  /* S.O. Due Date/Delivery Date*/~
                CH(02),                  /* Customer Region Code       */~
                CH(05),                  /* Customer Route Code        */~
                CH(09),                  /* Customer Zip Code          */~
                CH(02),                  /* Customer Drop Number       */~
                CH(09),                  /* Customer Number            */~
                CH(16),                  /* Customer P.O. Number       */~
                CH(08),                  /* Customer S.O. Number       */~
                CH(02),                  /* Current S.O. Stat PLAN STAT*/~
                CH(08),                  /* Date Assoc. with Stat Chg  */~
                CH(08),                  /* APC Invoice Assoc. with SO */~
                CH(08),                  /* APC Check Assoc. with Invoi*/~
                CH(04),                  /* APC Salesman Code          */~
                CH(02),                  /* Cust Delivery Cde PLAN DEL?*/~
                CH(02),                  /* Spec. Instr (PLAN HOWS)    */~
                CH(05),                  /* Load N. Sched./Assigned    */~
                PD(14,4),                /* Total Loading Units S.O.   */~
                PD(14,4),                /* Total Price S.O. (Net)     */~
                PD(14,4),                /* Total Cost S.O.            */~
                BI(2),                   /* Total Make Quantity S.O.   */~
                BI(2),                   /* Total Pull Quantity S.O.   */~
                CH(08),                  /* Date S.O. Created          */~
                CH(08),                  /* Date S.O. Last Modified    */~
                CH(03),                  /* S.O. Last Modified By User */~
                CH(08),                  /* Date B.O.L. Created        */~
                CH(04),                  /* S.O. Header Text Id        */~
                BI(2),                   /* Actual No. Delivery Days   */~
                CH(10),                  /* Special Product Flags      */~
                CH(01)                   /* Filler Area                */

                                         /* APCPLNOR - New File Layout */
L35350:     FMT CH(170)                  /* Total Record               */

                                         /* APCPLNSC and APCPLNLD-File */
L35380:     FMT CH(128)                  /* Total Record               */

                                         /* GENCODES - File            */
L35410:     FMT POS(25), CH(30)          /* Skip Code and Load Descript*/

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40160,         /* Entry Selection      */~
                                L40160          /* Edit S.O./Line Item  */
              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,13), fac(hex(94)), scr$                   , ch(55),~
               at (03,02), "* S.O. or Warranty  :",                      ~
               at (03,25), fac(lfac$(1%)), or_so$               , ch(08),~
               at (03,34), fac(lfac$(1%)), dt_ref$              , ch(08),~
               at (03,43), "Line Item No:",                              ~
               at (03,57), fac(lfac$(1%)), or_line$             , ch(02),~
               at (03,60), "Due Date :",                                 ~
               at (03,72), fac(hex(84)), or_due$                , ch(08),~
               at (04,02), "* Customer P.O. No. :",                      ~
               at (04,25), fac(lfac$(1%)), or_po$               , ch(16),~
               at (04,60), "Prod Date:",                                 ~
               at (04,72), fac(hex(84)), ld_dtp1$               , ch(08),~
               at (05,02), "* APC Invoice Number:",                      ~
               at (05,25), fac(lfac$(1%)), or_inv$              , ch(08),~
               at (05,60), "Comp Date:",                                 ~
               at (05,72), fac(hex(84)), ld_dtp2$               , ch(08),~
               at (06,02), "* APC Check Number  :",                      ~
               at (06,25), fac(lfac$(1%)), or_chk$              , ch(08),~
               at (06,60), "Load Date:",                                 ~
               at (06,72), fac(hex(84)), ld_dtp2$               , ch(08),~
                                                                         ~
               at (07,02), fac(hex(84)), status$                , ch(21),~
               at (07,25), fac(lfac$(2%)), or_status$           , ch(02),~
               at (07,30), fac(hex(84)), or_status_d$           , ch(30),~
               at (07,60), "Stat Date:",                                 ~
               at (07,72), fac(hex(84)), or_dte$                , ch(08),~
               at (08,02), "  Customer Number   :",                      ~
               at (08,25), fac(hex(84)), or_cuscode$            , ch(09),~
               at (08,40), fac(hex(84)), or_cuscode_d$          , ch(30),~
               at (09,02), "  Customer Load No  :",                      ~
               at (09,25), fac(hex(84)), or_load$               , ch(05),~
               at (09,40), fac(hex(84)), or_load_d$             , ch(30),~
               at (10,02), "  Customer Drop No  :",                      ~
               at (10,25), fac(hex(84)), or_drop$               , ch(02),~
               at (11,02), "  Region Code       :",                      ~
               at (11,25), fac(hex(84)), or_region$             , ch(02),~
               at (11,40), fac(hex(84)), or_region_d$           , ch(30),~
               at (12,02), "  Route Code        :",                      ~
               at (12,25), fac(hex(84)), or_route$              , ch(05),~
               at (12,40), fac(hex(84)), or_route_d$            , ch(30),~
               at (13,02), "  Salesman Code     :",                      ~
               at (13,25), fac(hex(84)), or_sls$                , ch(04),~
               at (13,40), fac(hex(84)), or_sls_d$              , ch(30),~
               at (14,02), "  Delivery Code     :",                      ~
               at (14,25), fac(hex(84)), or_fob$                , ch(02),~
               at (14,40), fac(hex(84)), or_fob_d$              , ch(30),~
               at (15,02), "  Special/Howship   :",                      ~
               at (15,25), fac(hex(84)), or_hows$               , ch(02),~
               at (15,40), fac(hex(84)), or_hows_d$             , ch(30),~
                                                                         ~
               at (16,02), "  Loading Units  :",                         ~
               at (16,21), fac(hex(84)), or_units$              , ch(08),~
               at (16,30), fac(hex(84)), sc_units$              , ch(08),~
               at (16,40), "Make Quantity    :",                         ~
               at (16,60), fac(hex(84)), or_make$               , ch(04),~
               at (16,66), fac(hex(84)), sc_make$               , ch(04),~
               at (17,02), "  Total Net Value:",                         ~
               at (17,21), fac(hex(84)), or_value$              , ch(08),~
               at (17,30), fac(hex(84)), sc_value$              , ch(08),~
               at (17,40), "Pull Quantity    :",                         ~
               at (17,60), fac(hex(84)), or_pull$               , ch(04),~
               at (17,66), fac(hex(84)), sc_pull$               , ch(04),~
               at (17,71), fac(hex(84)), sc_pull1$              , ch(04),~
               at (18,02), "  Last Changed By:",                         ~
               at (18,21), fac(hex(84)), or_userid$             , ch(03),~
               at (18,40), "Change Date      :",                         ~
               at (18,60), fac(hex(84)), or_chg$                , ch(08),~
               at (19,02), fac(hex(a4)), pp1$(1%)               , ch(09),~
               at (19,12), fac(hex(a4)), pp1$(2%)               , ch(09),~
               at (19,22), fac(hex(a4)), pp1$(3%)               , ch(09),~
               at (19,32), fac(hex(a4)), pp1$(4%)               , ch(09),~
               at (19,42), fac(hex(a4)), pp1$(5%)               , ch(09),~
               at (19,52), fac(hex(a4)), pp1$(6%)               , ch(09),~
               at (19,62), fac(hex(a4)), pp1$(7%)               , ch(09),~
               at (19,72), fac(hex(a4)), pp1$(8%)               , ch(06),~
                                                                         ~
               at (20,03), fac(hex(84)), sp$(1%)                , ch(07),~
               at (20,13), fac(hex(84)), sp$(2%)                , ch(07),~
               at (20,23), fac(hex(84)), sp$(3%)                , ch(07),~
               at (20,33), fac(hex(84)), sp$(4%)                , ch(07),~
               at (20,43), fac(hex(84)), sp$(5%)                , ch(07),~
               at (20,53), fac(hex(84)), sp$(6%)                , ch(07),~
               at (20,63), fac(hex(84)), sp$(7%)                , ch(07),~
               at (20,73), fac(hex(84)), sp$(8%)                , ch(07),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 7% then goto L41200
                  gosub lookup_header
                  goto L40070

L41200:        if keyhit% <> 8% then goto L41240
                  gosub lookup_line
                  goto L40070

L41240:        if keyhit% <> 9% then goto L41290
                  gosub lookup_detail
                  gosub display_detail
                  goto L40070

L41290:        if keyhit% <> 10% then goto L41330
                  gosub display_customer
                  goto L40070

L41330:        if keyhit% <> 15% then goto L41370
                  call "PRNTSCRN"
                  goto L40070

L41370:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            scr$="Hdr/Ln Item Disp-*Hdr Disp*                           "
            status$ = "  S.O. Header Status:"
        if edit% = 2% then L41560     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "(10)Display Customer                   "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffff090affffff0e0f1000)
            if fieldnr% = 1% then L41540
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L41540:     return

L41560: if fieldnr% > 0% then L41830  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over    (7)S.O. Header Txt     " &        ~
                     "(10)Display Customer   (14)Save Data   "
            pf$(2%)= "                 (8)Line Item Txt       " &        ~
                     "(12)Delete Data        (15)Print Screen"
            pf$(3%)= "                 (9)Review Detail       " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffff0708090aff0cff0e0f1000)
            str(pf$(1%),17%,21%) = hex(84) & "(7)S.O. Header Txt"&hex(8c)
            str(pf$(2%),17%,21%) = hex(84) & "(8)Line Item Txt"&hex(8c)
            gosub'099(or_text$)
            if txt% = 1% then goto L41690
               str(pf$(1%),17%,21%) = " " : str(pfkeys$,7%,1%) = hex(ff)
L41690:     gosub'099(sc_txt$)
            if txt% = 1% then goto L41720
               str(pf$(2%),17%,21%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L41720:     if line_item% = 0% then goto L41760
                                                           /* (AWD003)  */
               str(scr$,18%,25%) = sc_part$ 
               str(scr$,44%,12%) = sc_sub_part$
                                                           /* (AWD003)  */  
               status$ = "  Line Item Status  :"
               goto L41780
L41760:     str(pf$(3%),18%,20%) = " " : str(pfkeys$,9%,1%) = hex(ff)

L41780:     for j% = 1% to id_max%
                if userid$ = id$(j%) then return
            next j%
            str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
            str(pf$(2%),40%,17%) = " " : str(pfkeys$,12%,1%) = hex(ff)
        return
L41830:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            if line_item% = 0% then goto L41940
                                                           /* (AWD003)  */
               str(scr$,18%,25%) = sc_part$ 
               str(scr$,44%,12%) = sc_sub_part$
                                                           /* (AWD003)  */  
               status$ = "  Line Item Status  :"
L41940: return

        REM *************************************************************~
            *               D E T A I L   S C R E E N                   *~
            *************************************************************

        display_detail
L42050:       gosub set_pf2

            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,13), fac(hex(94)), scr$                   , ch(55),~
               at (03,02), "* Customer S.O. No. :",                      ~
               at (03,25), fac(hex(84)), or_so$                 , ch(08),~
               at (03,40), "Line Item No.:",                             ~
               at (03,55), fac(hex(84)), or_line$               , ch(02),~
               at (05,02), fac(hex(a4)), pp2$(1%)               , ch(09),~
               at (05,14), fac(hex(a4)), pp2$(2%)               , ch(09),~
               at (05,26), fac(hex(a4)), pp2$(3%)               , ch(09),~
               at (05,38), fac(hex(a4)), pp2$(4%)               , ch(09),~
               at (05,50), fac(hex(a4)), pp2$(5%)               , ch(09),~
               at (05,62), fac(hex(a4)), pp2$(6%)               , ch(09),~
               at (05,74), fac(hex(a4)), pp2$(7%)               , ch(06),~
                                                                         ~
               at (06,05), fac(hex(84)), d1$( 1%)               , ch(03),~
               at (06,17), fac(hex(84)), d2$( 1%)               , ch(02),~
               at (06,29), fac(hex(84)), d3$( 1%)               , ch(02),~
               at (06,41), fac(hex(84)), d4$( 1%)               , ch(03),~
               at (06,53), fac(hex(84)), d5$( 1%)               , ch(02),~
               at (06,63), fac(hex(84)), d6$( 1%)               , ch(08),~
               at (06,76), fac(hex(84)), d7$( 1%)               , ch(03),~
                                                                         ~
               at (07,05), fac(hex(84)), d1$( 2%)               , ch(03),~
               at (07,17), fac(hex(84)), d2$( 2%)               , ch(02),~
               at (07,29), fac(hex(84)), d3$( 2%)               , ch(02),~
               at (07,41), fac(hex(84)), d4$( 2%)               , ch(03),~
               at (07,53), fac(hex(84)), d5$( 2%)               , ch(02),~
               at (07,63), fac(hex(84)), d6$( 2%)               , ch(08),~
               at (07,76), fac(hex(84)), d7$( 2%)               , ch(03),~
                                                                         ~
               at (08,05), fac(hex(84)), d1$( 3%)               , ch(03),~
               at (08,17), fac(hex(84)), d2$( 3%)               , ch(02),~
               at (08,29), fac(hex(84)), d3$( 3%)               , ch(02),~
               at (08,41), fac(hex(84)), d4$( 3%)               , ch(03),~
               at (08,53), fac(hex(84)), d5$( 3%)               , ch(02),~
               at (08,63), fac(hex(84)), d6$( 3%)               , ch(08),~
               at (08,76), fac(hex(84)), d7$( 3%)               , ch(03),~
                                                                         ~
               at (09,05), fac(hex(84)), d1$( 4%)               , ch(03),~
               at (09,17), fac(hex(84)), d2$( 4%)               , ch(02),~
               at (09,29), fac(hex(84)), d3$( 4%)               , ch(02),~
               at (09,41), fac(hex(84)), d4$( 4%)               , ch(03),~
               at (09,53), fac(hex(84)), d5$( 4%)               , ch(02),~
               at (09,63), fac(hex(84)), d6$( 4%)               , ch(08),~
               at (09,76), fac(hex(84)), d7$( 4%)               , ch(03),~
                                                                         ~
               at (10,05), fac(hex(84)), d1$( 5%)               , ch(03),~
               at (10,17), fac(hex(84)), d2$( 5%)               , ch(02),~
               at (10,29), fac(hex(84)), d3$( 5%)               , ch(02),~
               at (10,41), fac(hex(84)), d4$( 5%)               , ch(03),~
               at (10,53), fac(hex(84)), d5$( 5%)               , ch(02),~
               at (10,63), fac(hex(84)), d6$( 5%)               , ch(08),~
               at (10,76), fac(hex(84)), d7$( 5%)               , ch(03),~
                                                                         ~
               at (11,05), fac(hex(84)), d1$( 6%)               , ch(03),~
               at (11,17), fac(hex(84)), d2$( 6%)               , ch(02),~
               at (11,29), fac(hex(84)), d3$( 6%)               , ch(02),~
               at (11,41), fac(hex(84)), d4$( 6%)               , ch(03),~
               at (11,53), fac(hex(84)), d5$( 6%)               , ch(02),~
               at (11,63), fac(hex(84)), d6$( 6%)               , ch(08),~
               at (11,76), fac(hex(84)), d7$( 6%)               , ch(03),~
                                                                         ~
               at (12,05), fac(hex(84)), d1$( 7%)               , ch(03),~
               at (12,17), fac(hex(84)), d2$( 7%)               , ch(02),~
               at (12,29), fac(hex(84)), d3$( 7%)               , ch(02),~
               at (12,41), fac(hex(84)), d4$( 7%)               , ch(03),~
               at (12,53), fac(hex(84)), d5$( 7%)               , ch(02),~
               at (12,63), fac(hex(84)), d6$( 7%)               , ch(08),~
               at (12,76), fac(hex(84)), d7$( 7%)               , ch(03),~
                                                                         ~
               at (13,05), fac(hex(84)), d1$( 8%)               , ch(03),~
               at (13,17), fac(hex(84)), d2$( 8%)               , ch(02),~
               at (13,29), fac(hex(84)), d3$( 8%)               , ch(02),~
               at (13,41), fac(hex(84)), d4$( 8%)               , ch(03),~
               at (13,53), fac(hex(84)), d5$( 8%)               , ch(02),~
               at (13,63), fac(hex(84)), d6$( 8%)               , ch(08),~
               at (13,76), fac(hex(84)), d7$( 8%)               , ch(03),~
                                                                         ~
               at (14,05), fac(hex(84)), d1$( 9%)               , ch(03),~
               at (14,17), fac(hex(84)), d2$( 9%)               , ch(02),~
               at (14,29), fac(hex(84)), d3$( 9%)               , ch(02),~
               at (14,41), fac(hex(84)), d4$( 9%)               , ch(03),~
               at (14,53), fac(hex(84)), d5$( 9%)               , ch(02),~
               at (14,63), fac(hex(84)), d6$( 9%)               , ch(08),~
               at (14,76), fac(hex(84)), d7$( 9%)               , ch(03),~
                                                                         ~
               at (15,05), fac(hex(84)), d1$(10%)               , ch(03),~
               at (15,17), fac(hex(84)), d2$(10%)               , ch(02),~
               at (15,29), fac(hex(84)), d3$(10%)               , ch(02),~
               at (15,41), fac(hex(84)), d4$(10%)               , ch(03),~
               at (15,53), fac(hex(84)), d5$(10%)               , ch(02),~
               at (15,63), fac(hex(84)), d6$(10%)               , ch(08),~
               at (15,76), fac(hex(84)), d7$(10%)               , ch(03),~
                                                                         ~
               at (16,05), fac(hex(84)), d1$(11%)               , ch(03),~
               at (16,17), fac(hex(84)), d2$(11%)               , ch(02),~
               at (16,29), fac(hex(84)), d3$(11%)               , ch(02),~
               at (16,41), fac(hex(84)), d4$(11%)               , ch(03),~
               at (16,53), fac(hex(84)), d5$(11%)               , ch(02),~
               at (16,63), fac(hex(84)), d6$(11%)               , ch(08),~
               at (16,76), fac(hex(84)), d7$(11%)               , ch(03),~
                                                                         ~
               at (17,05), fac(hex(84)), d1$(12%)               , ch(03),~
               at (17,17), fac(hex(84)), d2$(12%)               , ch(02),~
               at (17,29), fac(hex(84)), d3$(12%)               , ch(02),~
               at (17,41), fac(hex(84)), d4$(12%)               , ch(03),~
               at (17,53), fac(hex(84)), d5$(12%)               , ch(02),~
               at (17,63), fac(hex(84)), d6$(12%)               , ch(08),~
               at (17,76), fac(hex(84)), d7$(12%)               , ch(03),~
                                                                         ~
               at (18,05), fac(hex(84)), d1$(13%)               , ch(03),~
               at (18,17), fac(hex(84)), d2$(13%)               , ch(02),~
               at (18,29), fac(hex(84)), d3$(13%)               , ch(02),~
               at (18,41), fac(hex(84)), d4$(13%)               , ch(03),~
               at (18,53), fac(hex(84)), d5$(13%)               , ch(02),~
               at (18,63), fac(hex(84)), d6$(13%)               , ch(08),~
               at (18,76), fac(hex(84)), d7$(13%)               , ch(03),~
                                                                         ~
               at (19,05), fac(hex(84)), d1$(14%)               , ch(03),~
               at (19,17), fac(hex(84)), d2$(14%)               , ch(02),~
               at (19,29), fac(hex(84)), d3$(14%)               , ch(02),~
               at (19,41), fac(hex(84)), d4$(14%)               , ch(03),~
               at (19,53), fac(hex(84)), d5$(14%)               , ch(02),~
               at (19,63), fac(hex(84)), d6$(14%)               , ch(08),~
               at (19,76), fac(hex(84)), d7$(14%)               , ch(03),~
                                                                         ~
               at (20,05), fac(hex(84)), d1$(15%)               , ch(03),~
               at (20,17), fac(hex(84)), d2$(15%)               , ch(02),~
               at (20,29), fac(hex(84)), d3$(15%)               , ch(02),~
               at (20,41), fac(hex(84)), d4$(15%)               , ch(03),~
               at (20,53), fac(hex(84)), d5$(15%)               , ch(02),~
               at (20,63), fac(hex(84)), d6$(15%)               , ch(08),~
               at (20,76), fac(hex(84)), d7$(15%)               , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L43560
                  call "PRNTSCRN"
                  goto L42050

L43560:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            inpmessage$ =                                                ~
         "Display of Scheduling Department Detail. <Return> to Continue?"

            scr$="Hdr/Ln Item Disp-*Hdr Disp*                           "
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50120,          /* Lookup                */~
                              L50560           /*S.O. Status Change     */
            return

L50120: REM Check Input Fields                    BG_DUE$, BG_DTE$
            if len(dt_ref$) > 4 then gosub lookup_warranty
            if len(or_inv$) > 4 then ff% = 2%
            if len(or_chk$) > 4 then ff% = 3%
            if len(or_so$)  > 4 then ff% = 4%
            if len(or_po$)  > 4 then ff% = 5%

            init(" ") ll_key$
            if ff% = 2% then str(ll_key$,1%,8%)  = or_inv$
            if ff% = 3% then str(ll_key$,1%,8%)  = or_chk$
            if ff% = 4% then str(ll_key$,1%,8%)  = or_so$
            if ff% = 5% then str(ll_key$,1%,16%) = or_po$

            read #1,key ff% = ll_key$, eod goto L50280
            gosub dataload
        return
L50280:     errormsg$ = "(Error) - Info Not on File?? " & sc_bar$
            init(" ") or_inv$, or_chk$, or_so$, or_po$, dt_ref$
            gosub error_prompt
        return

        lookup_header
            header$ = "Lookup S.O. Header Text    "
            textid$ = or_text$
            goto L50400
        lookup_line
            header$ = "Lookup S.O. Line Item Text "
            textid$ = sc_txt$
L50400:     gosub'099(textid$)
            if txt% = 0% then goto L50460
               call "TXTFUTIL" (#7, f2%(7%), "LOAD", textid$)

            call "TXTINSUB" (#7, f2%(7%),"012", header$, textid$,        ~
                                                         text$() )
L50460: return
                                                 /* (AWD003)         */
        lookup_warranty                          /* (EWD001) - Begin */
            read #10,key 0% = dt_ref$, using L50510, or_so$, or_line$,   ~
                             sc_new_part$, sc_sub_part$, eod goto L50530
L50510:        FMT POS(9), CH(8), CH(2), POS(27), CH(25), CH(20)

            str(sc_bar$,1%,8%) = or_so$
            str(sc_bar$,9%,2%) = or_line$
                                                 /* (AWD003)         */
                                                 /* New Sub Part No  */
        return
L50530:     init(" ") or_so$, or_line$, dt_ref$, sc_sub_part$,        ~
            sc_new_part$, sc_bar$
        return                                   /* (EWD001) - End   */

L50560: REM Potiential Status Change
            gosub lookup_status
        return

        lookup_dtl
            init(" ") dt_key$, dt_date$, dt_st$
            str(dt_key$,1%,10%) = sc_key$
            read #9,key > dt_key$, using L50650, dt_key$, dt_date$,       ~
                                                   dt_st$, eod goto L50690
L50650:        FMT POS(24), CH(23), CH(6), POS(64), CH(2)
            if str(dt_key$,1%,10%) <> sc_key$ then return
               sc_st$   = dt_st$
               sc_ldte$ = dt_date$
L50690: return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        lookup_load
            init(" ") ld_rec$, ld_dtp1$, ld_dtp2$, ld_dtp3$, or_load_d$
            read #5,key = or_load$, using L35380, ld_rec$,                ~
                                                 eod goto L60150
            ld_dtp1$   = str(ld_rec$,70%,8%)
            ld_dtp2$   = str(ld_rec$,78%,8%)
            ld_dtp3$   = str(ld_rec$,86%,8%)
            or_load_d$ = str(ld_rec$,16%,30%)
            call "DATEFMT" (ld_dtp1$)
            call "DATEFMT" (ld_dtp2$)
            call "DATEFMT" (ld_dtp3$)
L60150: return

        lookup_line_item
            line_item% = 0%
            init(" ") sc_key$, sc_part$, sc_txt$, sc_st$, sc_st_d$,      ~
                      sc_ldte$, sc_special$, sc_units$, sc_value$,       ~
                      sc_make$, sc_pull$, sav_status$, sav_dte$,         ~
                      sc_pull1$
            sav_status$ = or_status$
            sav_dte$    = or_dte$
            sc_tqty% = 0%  : sc_mqty%  = 0%  : sc_pqty% = 0%
            sc_price = 0.0 : sc_cost   = 0.0 : sc_units = 0.0
            or_line% = 0%  : sc_pqty1% = 0%
            convert or_line$ to or_line%, data goto L60500

            convert or_line% to or_line$, pic(00)

            str(sc_key$,1%,8%) = or_so$
            str(sc_key$,9%,2%) = or_line$
            read #3,key = sc_key$, using L60380, sc_part$, sc_tqty%,      ~
                                   sc_mqty%, sc_pqty%,sc_pqty1%,sc_price,~
                                   sc_cost, sc_units, sc_txt$, sc_st$,   ~
                                   sc_ldte$, sc_special$, eod goto L60500
L60380:        FMT POS(34), CH(25), POS(68), 4*BI(2), 3*PD(14,4), CH(4), ~
                   POS(110), CH(2), CH(6), CH(10)
            line_item% = 1%
            for i% = 1% to 10%               /* Check Special Flags    */
              if str(sc_special$,i%,1%) = "Y" then                       ~
                                              str(sp$(i%),5%,3%) = "Yes"
            next i%
            init(" ") or_status$, or_dte$
            gosub lookup_dtl
            or_status$ = sc_st$
            or_dte$    = sc_ldte$
            call "DATEFMT" (or_dte$)

L60500:     convert sc_units to sc_units$,   pic(#####.##)
            convert sc_price to sc_value$,   pic(#####.##)
            convert sc_mqty% to sc_make$,    pic(####)
            convert sc_pqty% to sc_pull$,    pic(####)
            convert sc_pqty1% to sc_pull1$,  pic(####)
            if line_item% = 0% then or_line$ = " "
        return

        lookup_customer
            init(" ") or_cuscode_d$
            read #4,key = or_cuscode$, using L60620, or_cuscode_d$,       ~
                                                    eod goto L60630
L60620:        FMT POS(253), CH(30)
L60630: return

        lookup_salesman
            init(" ") or_sls_d$
            call "DESCRIBE" (#6, or_sls$, or_sls_d$, 0%, f1%(6))
        return

        lookup_region
            init(" ") readkey$, desc$, or_region_d$
            str(readkey$,1%,9%)   = "PLAN REGN"
            str(readkey$,10%,15%) = or_region$
            read #2,key = readkey$, using L35410, desc$, eod goto L60760
            or_region_d$ = desc$
L60760: return

        lookup_route
            init(" ") readkey$, desc$, or_route_d$
            str(readkey$,1%,9%)   = "PLAN RTE "
            str(readkey$,10%,15%) = or_route$
            read #2,key = readkey$, using L35410, desc$, eod goto L60840
            or_route_d$ = desc$
L60840: return

        lookup_fob
            init(" ") readkey$, desc$, or_fob_d$
            str(readkey$,1%,9%)   = "PLAN DELV"
            str(readkey$,10%,15%) = or_fob$
            read #2,key = readkey$, using L35410, desc$, eod goto L60920
            or_fob_d$ = desc$
L60920: return

        lookup_hows
            init(" ") readkey$, desc$, or_hows_d$
            str(readkey$,1%,9%)   = "PLAN HOWS"
            str(readkey$,10%,15%) = or_hows$
            read #2,key = readkey$, using L35410, desc$, eod goto L61000
            or_hows_d$ = desc$
L61000: return

        lookup_status
            init(" ") readkey$, desc$, or_status_d$
            str(readkey$,1%,9%)   = "PLAN STAT"
            str(readkey$,10%,15%) = or_status$
            read #2,key = readkey$, using L35410, desc$, eod goto L61090
            or_status_d$ = desc$
        return
L61090:     errormsg$ = "(Error) - Invalid Status Code?"
            or_status$   = sav_status$
            or_status_d$ = sav_desc$
            gosub error_prompt
        return

        deffn'099(txt$)
            txt% = 0%
            if txt$ = hex(00000000) or txt$ = hex(ffffffff)              ~
                                              or txt$ = " " then return
            txt% = 1%
        return

                                                           /* (EWD002)  */
        open_error
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                           /* (EWD002)  */ 
        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        lookup_detail
            cc% = 0%
            init(" ") sd_key$, d1$(), d2$(), d3$(), d4$(),d5$(), d6$(),  ~
                      d7$(), sd_units$

            str(sd_key$,1%,8%) = or_so$
            str(sd_key$,9%,2%) = or_line$
        lookup_dtl_nxt
            read #8,key > sd_key$, using L61410, sd_key$, sd_units,       ~
                                   sd_seq%, eod goto lookup_dtl_done
L61410:        FMT CH(22), PD(14,4), BI(1)
            if str(sd_key$,1%,8%) <> or_so$ then goto lookup_dtl_done
            if str(sd_key$,9%,2%) <> or_line$ then goto lookup_dtl_done
               cc% = cc% + 1%
               if cc% > 14% then cc% = 15%
               d1$(cc%) = str(sd_key$,11%,3%)    /* DEPARTMENT CODE    */
               d2$(cc%) = str(sd_key$,14%,2%)    /* PROCESS CODE       */
               d3$(cc%) = str(sd_key$,16%,2%)    /* SHIFT CODE         */
               d4$(cc%) = str(sd_key$,18%,3%)    /* MODEL CODE         */
               d5$(cc%) = str(sd_key$,21%,2%)    /* (PLAN UNIT) CODE   */
               convert sd_units to d6$(cc%),pic(####.###)      /* UPMH */

               convert sd_seq% to d7$(cc%), pic(000)

               goto lookup_dtl_nxt
        lookup_dtl_done

        return

        display_customer
           cuscode$ = or_cuscode$
           call "APCPL3SB" ( cuscode$,   /* Customer Code              */~
                             #4,         /* FILE = (CUSTOMER)          */~
                             #2,         /* FILE = (GENCODES)          */~
                             #6,         /* FILE = (SLMMASTR)          */~
                             #7 )        /* FILE = (TXTFILE )          */
        return

        delete_it
            init(" ") ll_key$
            str(ll_key$,1%,8%) = or_so$
            read #1,hold,key 4% = ll_key$, using L61740, ll_key$,         ~
                                                       eod goto delete_or
L61740:        FMT POS(52), CH(8)
            if str(ll_key$,1%,8%) <> or_so$ then goto delete_or
            delete #1
        delete_or
            init(" ") ll_key$
            str(ll_key$,1%,8%) = or_so$
        delete_sc
            read #3,hold,key > ll_key$, using L61830, ll_key$,            ~
                                                     eod goto delete_dt
L61830:        FMT POS(24), CH(10)
            if str(ll_key$,1%,8%) <> or_so$ then goto delete_dt
               delete #3
               goto delete_sc
        delete_dt
            init(" ") ll_key$
            str(ll_key$,1%,8%) = or_so$
L61900:     read #9,hold,key > ll_key$, using L61920, ll_key$,            ~
                                                     eod goto delete_po
L61920:        FMT POS(24), CH(23)
            if str(ll_key$,1%,8%) <> or_so$ then goto delete_po
               delete #9
               goto L61900
        delete_po
            init(" ") ll_key$
            str(ll_key$,1%,9%)   = or_cuscode$
            str(ll_key$,10%,16%) = or_po$
            read #1,hold,key 1% = ll_key$, using L62020, ll_key$,         ~
                                                        eod goto L62060
L62020:       FMT POS(27), CH(25)
            if str(ll_key$,1%,9%) <> or_cuscode$ and                     ~
               str(ll_key$,10%,16%) <> or_po$ then return
            delete #1
L62060: return clear all
        goto inputmode

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
