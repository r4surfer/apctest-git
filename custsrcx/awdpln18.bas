        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN18                             *~
            *  Creation Date     - 07/19/04                             *~
            *  Last Modified Date- 11/24/2015                           *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - This Program Calculates the daily    *~
            *                      Line Scheduler.  This is run by      *~
            *                      Default Due Date and Department      *~
            *                                                           *~
            *                      This program will also create two    *~
            *                      files so queries may be run in cyber *~
            *                             AWDSCHAL   - ALL Departments  *~
            *                             AWDSCHDP   - Specific Depts   *~
            *                                                           *~
            *  Code Tables Used  - (PLAN DEPT) - Department Codes       *~
            *                                                           *~
            *  Special Comments  - (APCPLN1B) - Subroutine to Lookup    *~
            *                                   and Display Planning    *~
            *                                   Tables.                 *~
            *                      (AWDPLN0B) - Subroutine to Calc      *~
            *                                   Current Planning Dates. *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/19/04 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            * 08/17/04 ! (EWD001) Mod to add not shipped to cyber ! CMG *~
            *          !  file and pricing information            !     *~ 
            * 04/05/05 ! (AWD002) Mod to the reclen of APCPLNSD   ! CMG *~
            * 04/24/06 ! (AWD003) Mod for NE to exclude AT0205    ! CMG *~ 
            *          !   & AT0206 depending on which side       !     *~
            * 11/16/06 ! (AWD004) Mod to add question to screen   ! CMG *~ 
            *          !   & put stock in 102                     !     *~
            *09/17/2013! (AWD005) mod to lookup subpart for cross ! CMG *~
            *          !      dock thermal product                !     *~ 
            *09/17/2013! (SR70514) mod to ask to write infopart to! CMG *~
            *          !      AWDSCHAL & AWDSCHDP                 !     *~ 
            *11/24/2015! (SR67906) automate AWDPLN18 for CraigBray! CMG *~
            *09/28/2021! CR2907 Increase date range for TX only   ! RDB *~
            *************************************************************



        dim hdr$40, msg$(3%)79,          /* Shostat Error Messages     */~
            filename$8,                  /* Used By EWDOPEN            */~
            todaysdate$10,               /* todays date (SR67906)      */~
            date$10,                     /* Todays Date                */~
            bg_due$10, bg_dte$10,        /* Beg/End S.O. Due Date      */~
            ed_due$10, ed_dte$10,        /*                            */~
            bg_dept$3, ed_dept$3,        /* Beg/End Department         */~
            file$1,                      /* Create Cyberquery File?    */~
            comp$1,                      /* Include Completes?         */~
            tab$(10%)10,                 /* Save Code Table Names      */~
            readkey$25,                  /* Generic Key                */~
            desc$30,                     /* TABLE VALUE, DESCRIPTION   */~
            code$15,                     /* Use To Look-Up Table Code  */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */


        dim                              /*                            */~
            cur_yr$2, prv_yr$2,          /* Current and Previous Year  */~
            cur_year$4, prv_year$4,      /* Display versions of above  */~
            cur_wk$2, cur_dy$1,          /* Current Week and Day       */~
            cur_dte$6, cur_date$8,       /* Calc of Prod. Date         */~
            ent_yr$2,                    /* Entry Year                 */~
            ent_year$4,                  /* Entry Year 4 display       */~
            ent_wk$2, ent_dy$1,          /* Entry Week and Day         */~
            ent_dte$6, ent_date$8        /* Entry Calc of Prod. Date   */

       dim                               /*                            */~
            cap_key$10,                  /* AWDCAPTY ReadKey           */~
            cap_key1$16,                 /* AWDCAPTY ReadKey #1        */~
            cap_rec$256,                 /* AWDCAPTY Record            */~
            dt_key1$57,                  /* APCPLNDT Readkey #2        */~
            dt_rec$256,                  /* APCPLNDT Record            */~
            dt_part$25,                  /* APCPLNDT Part Number       */~
            dt_dept$3,                   /* APCPLNDT Department        */~
            dt_date$10,                  /* Formatted Date             */~
            cap_date$6,                  /* AWDCAPTY Date              */~
            cap_year$4,                  /* AWDCAPTY Year              */~
            cap_wk$2,                    /* AWDCAPTY Week              */~
            cap_dy$1,                    /* AWDCAPTY Day               */~
            cap_dept$3,                  /* AWDCAPTY Dept              */~
            cap_filler$232,              /* AWDCAPTY Filler AREA       */~
            sch_key$20,                  /* Scheduler Key              */~ 
            sch_dte$6,                   /* Scheduler Prod Date        */~
            sch_filler$167,              /* Scheduler Filler           */~
            mull$1,                      /* Is the mull or lineline    */~
            wood_code$3                  /* Wood Surround Code         */

        dim                              /*                            */~
            or_key$51,                   /* APCPLNOR ReadKey           */~
            or_so$8,                     /* APCPLNOR SO Number         */~
            or_hows$2,                   /* APCPLNOR Howship           */~
            state$2,                     /* Customer State             */~
            or_region$2,                 /* Customer Region            */~
            or_route$5,                  /* Customer Route             */~
            planned$1,                   /* Planned or Not?            */~
            or_status$2,                 /* APCPLNOR Status            */~
            or_due$6,                    /* APCPLNOR Due Date          */~
            order_dte$6,                 /* APCPLNOR Order Date        */~
            sc_key$10,                   /* APCPLNSC ReadKey           */~
            sc_part$25,                  /* APCPLNSC Part Number       */~
            sd_key$23,                   /* APCPLNSD ReadKey           */~
            sd_dept$3,                   /* APCPLNSD Department        */~
            prd_dte$6,                   /* Calculated Production Date */~
            cut_dte$6,                   /* Caluclated Cutoff Date     */~
            customer$9,                  /* Customer Code              */~
            var_fld3$20,                 /* Variable File 3            */~
            var_fld4$20,                 /* Variable File 3            */~
            var_fld5$20,                 /* Variable File 3            */~
            var_fld6$20                  /* Variable File 3            */


        dim                              /*                            */~
            scr_hdr$79,                  /* Screen Header              */~
            scrdte$10,                   /* Formatted Screen Date      */~
            scrdte$(6000%)79,            /* Screen Display             */~
            screen$(6000%)79             /* Screen Display             */


        dim supp_dept$(10%)3             /* Support Departments        */

        dim schema$8,                    /* (AWD003) Schema            */~
            cust_schema$30               /* (AWD003) Customer Schema   */

        dim scr_cross$1                  /* (AWD004) move data         */
        
        dim                              /* (AWD005)                   */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            subpart$20,                  /* Subpart Number             */~
            infopart$20                  /* Info Part Number           */
        

        dim f2%(25%),                     /* = 0 if the file is open    */~
            f1%(25%),                     /* = 1 if READ was successful */~
            fs%(25%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(25%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$36, pname$21
            apc$   = "  Line Scheduler Display and Report  "
            pname$ = "AWDPLN18 - Rev: R6.04"

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
            * #1  ! AWDCAPTY ! New Effective Units Capacity File        *~
            * #2  ! APCPLNDP ! Planning Master Department File          *~
            * #3  ! GENCODES ! Master Code Tables File                  *~
            * #4  ! APCPLNOR ! Plannin S.O. Header Master               *~
            * #5  ! APCPLNSC ! Planning Master Scheduling File          *~
            * #6  ! APCPLNSD ! Planning S.O. Schedule Dept Detail       *~
            * #7  ! APCPLNDT ! Production Master Detail File            *~
            * #8  ! CUSTOMER ! Customer Master File                     *~
            * #10 ! AWDSCHAL ! Work File for ALL Departments            *~
            * #11 ! AWDSCHDP ! Work File for Specific Departments       *~
            * #12 ! WORKFILE ! Work File to Sort Data                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AWDCAPTY",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 7,    keylen = 10,                      ~
                        alt key 1, keypos =  1, keylen = 16

            select #2,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos = 11,   keylen = 12,                      ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #4,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos =   1,  keylen =  51,                     ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

            select #5,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =  24,  keylen =  10,                     ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33
/* (AWD002) - Mod to key and reclen */
            select #6,  "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =   1,  keylen =  23

            select #7,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup


            select #8,   "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #10, "AWDSCHAL",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen = 20,                      ~
                        alt key 1, keypos = 7,    keylen = 14, dup 


            select #11, "AWDSCHDP",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen = 20,                      ~
                        alt key 1, keypos = 7,    keylen = 14, dup 


            select #12, "AWDSCHED",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 7,    keylen = 10,                      ~
                        alt key 1, keypos =  1, keylen = 16,             ~
                            key 2, keypos = 37, keylen = 10
/* (AWD005) */                            
            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup                            


REM            CALL "SHOSTAT" ("INITIALIZATION")


            filename$ = "AWDCAPTY" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDP" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSD" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error            

/* (AWD003) get schema to know which side you are on */

            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)




              mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)

            date$ = date                     /* Set The Current Date   */
            todaysdate$ = date               /* (SR67906) */
            
            call "DATFMTC" (date$)            

            tab$(1%) = "PLAN DEPT"     :      tab$(2%) = "LINE HOWS" 
            tab$(3%) = "PLAN DELV"     :      tab$(4%) = "PLANINLIN"
            tab$(5%) = "PLAN CUTO"     :      tab$(6%) = "LINE CUST"

            line% = 12%
/* (SR67906) */
            auto% = 0%
            if userid$ = "ATT" then auto% = 1%
            if userid$ = "ACT" then auto% = 2%
            	if auto% <> 0% then gosub automate
            	if auto% <> 0% then goto exit_program	
/* (SR67906\) */            		
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 5%       /* (AWD004) */
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10240
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
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  =  8% then gosub report_analysis

                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11160:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11210:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11210
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11210
                  lastfieldnr% = fieldnr%
            goto L11160

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
         "Enter a Valid Beginning / Ending Due Date ?                  ",~
         "Enter a Valid Beginning / Ending Department or (ALL)?        ",~
         "Enter 'Y' for Create Cyberquery file, 'N' for No?            ",~
         "Enter 'Y' to include items not shipped, 'N' for No?          ",~
         "Enter 'Y' to move Cross Dock to 104, 'N' for No?             "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") bg_due$, ed_due$, bg_dte$, ed_dte$, bg_dept$, ed_dept$,~
                      cur_yr$, prv_yr$, cur_year$, prv_year$, cur_wk$,       ~
                      cur_dy$, cur_dte$, cur_date$, ent_yr$, ent_year$,      ~
                      ent_wk$, ent_dy$, ent_dte$, ent_date$, cap_key$,       ~
                      cap_rec$, dt_key1$, dt_rec$, dt_dept$, dt_date$,       ~
                      cap_date$, cap_year$, cap_wk$, cap_dy$, cap_dept$,     ~
                      or_key$, or_so$, or_hows$, or_status$, sc_key$,        ~
                      sc_part$, sd_key$, sd_dept$, prd_dte$, or_due$,        ~
                      customer$, var_fld3$, var_fld4$, var_fld5$, var_fld6$, ~
                      cut_dte$, state$, or_region$, or_route$, planned$,     ~
                      file$, comp$, scr_cross$, cust_schema$

        return

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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub set_pf1

              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40200,         /* Beg/End Due Date     */~
                                L40200,         /* Beg/End Department   */~
                                L40200,         /* Create File?         */~
                                L40200,         /* Include Complete?    */~
                                L40200          /* Move Cross Dock ?    */

              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(38),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Begin S.O. Due Date  :",                     ~
               at (03,25), fac(lfac$(1%)), bg_due$              , ch(10),~
                                                                         ~
               at (03,40), "Ending S.O. Due Date :",                     ~
               at (03,63), fac(lfac$(1%)), ed_due$              , ch(10),~
                                                                         ~
               at (04,02), "Begin Department     :",                     ~
               at (04,25), fac(lfac$(2%)), bg_dept$             , ch(03),~
                                                                         ~
               at (04,40), "Ending Department    :",                     ~
               at (04,63), fac(lfac$(2%)), ed_dept$             , ch(03),~
                                                                         ~
               at (05,02), "Create Cyber File?   :",                     ~
               at (05,25), fac(lfac$(3%)), file$                , ch(01),~
                                                                         ~
               at (06,02), "Include Completes?   :",                     ~
               at (06,25), fac(lfac$(4%)), comp$                , ch(01),~
                                                                         ~
/*AWD004*/     at (07,02), "Move Cross Dock?     :",                     ~
               at (07,25), fac(lfac$(5%)), scr_cross$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40990     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40950
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40950:     if fieldnr% > 1% then L40970
               str(pf$(1%),18%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40970:     return

L40990: if fieldnr% > 0% then L41100  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "(8)S.O. Analysis                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffff08ffffffffffff0f1000)
            return
L41100:
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return




        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
L41025:       gosub set_pf2

REM           gosub'060(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()


L41030:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Begin S.O. Due Date  :",                     ~
               at (03,25), fac(hex(84)),   bg_due$              , ch(10),~
                                                                         ~
               at (03,40), "Ending S.O. Due Date :",                     ~
               at (03,63), fac(hex(84)),   ed_due$              , ch(10),~
                                                                         ~
               at (04,02), "Begin Department     :",                     ~
               at (04,25), fac(hex(84)),   bg_dept$             , ch(03),~
                                                                         ~
               at (04,40), "Ending Department    :",                     ~
               at (04,63), fac(hex(84)),   ed_dept$             , ch(03),~
                                                                         ~
               at (05,02), "Create Cyber File?   :",                     ~
               at (05,25), fac(hex(84)),   file$                , ch(01),~
                                                                         ~
               at (05,40), "Move Cross Dock?     :",                     ~
               at (05,63), fac(lfac$(5%)), scr_cross$           , ch(01),~
                                                                         ~
               at (06,02), "Include Completes?   :",                     ~
               at (06,25), fac(hex(84)),   comp$                , ch(01),~
                                                                         ~
               at (07,02), fac(hex(84)),   scr_hdr$             , ch(79),~
                                                                         ~
               at (08,02), fac(hex(84)),   scrdte$(scr%+1%)     , ch(79),~
               at (09,02), fac(hex(84)),   screen$(scr%+1%)     , ch(79),~
               at (10,02), fac(hex(84)),   screen$(scr%+2%)     , ch(79),~
               at (11,02), fac(hex(84)),   screen$(scr%+3%)     , ch(79),~
               at (12,02), fac(hex(84)),   screen$(scr%+4%)     , ch(79),~
               at (13,02), fac(hex(84)),   screen$(scr%+5%)     , ch(79),~
               at (14,02), fac(hex(84)),   screen$(scr%+6%)     , ch(79),~
               at (15,02), fac(hex(84)),   screen$(scr%+7%)     , ch(79),~
               at (16,02), fac(hex(84)),   screen$(scr%+8%)     , ch(79),~
               at (17,02), fac(hex(84)),   screen$(scr%+9%)     , ch(79),~
               at (18,02), fac(hex(84)),   screen$(scr%+10%)    , ch(79),~
               at (19,02), fac(hex(84)),   screen$(scr%+11%)    , ch(79),~
               at (20,02), fac(hex(84)),   screen$(scr%+12%)    , ch(79),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 1% then goto L41840
                  goto startover

L41840:
               if keyhit% <> 2% then goto L41850            /* First    */
L41835:           scr% = 0%
                  goto L41025

L41850:        if keyhit% <> 3% then goto L41875            /* Last      */
L41855:           x% = int(val_max% / line%)
                  scr% = (x%*line%)
                  goto L41025

L41875:        if keyhit% <> 4% then goto L41905            /* Previous */
                  if scr% < 15% then goto L41835
                  scr% = scr% - line%
                  if scr% <= 1% then goto L41835
                  goto L41025

L41905:        if keyhit% <> 5% then goto L41930            /* Next     */
                  scr% = scr% + line%
                  if scr% < val_max% then goto L41025
REM                  goto L41855
                     goto L41025
L41930:

               if keyhit% <> 16% then goto L41030
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
           scr_hdr$ = "------------------------------------------------~
                      ~-------------------------------"
           



            pf$(1%) = "(1)Start Over  (4)Previous              " &       ~
                      "                                       "
            pf$(2%) = "(2)First       (5)Next                  " &       ~
                      "                                       "
            pf$(3%) = "(3)Last                                 " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(0102030405ffffffffffffffffffff1000)
              if (scr% + line%) < val_max% then return
                  str(pf$(2%),16%,10%) = " "
                  x% = int(val_max% / line%)
                  scr% = (x%*line%)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Beg/End Due Date      */ ~
                              L50390,         /* Beg/End Region Code   */ ~
                              L50700,         /* Create File?          */ ~
                              L50800,         /* Include Completes?    */ ~
                              L50900          /* Move Cross dock  ?    */ 
            return

L50150: REM Beginning Due/Delivery Date           BG_DUE$, BG_DTE$
            if bg_due$ > " " then goto L50190                          
               bg_due$ = date$                                         

L50190:     date% = 0%
            call "DATEOKC" (bg_due$, date%, errormsg$)                 
            if date% = 0% then goto date_error
            bg_dte$ = bg_due$
            call "DATUFMTC" (bg_dte$)                                  
        REM Ending Due/Delivery Date              ED_DUE$, ED_DTE$
            if len(ed_due$) < 6 then ed_due$ = bg_due$
            date% = 0%
            call "DATEOKC" (ed_due$, date%, errormsg$)                 
            if date% = 0% then goto date_error
            ed_dte$ = ed_due$
            call "DATUFMTC" (ed_dte$)                                  
            if bg_dte$ > ed_dte$ then goto L50330
        return
L50330:     errormsg$ = "(Error) - Invalid Due Date Range??"
        date_error
            gosub error_prompt
            init(" ") bg_due$, bg_dte$, ed_due$, ed_dte$
        return

L50390: REM Beg/End Department Codes              BG_DEPT$, ED_DEPT$
            if bg_dept$ <> " " then goto L50400
               bg_dept$ = "ALL"
               ed_dept$ = "ALL"

L50400:     if str(bg_dept$,1%,1%) <> "A" then goto L50450
               bg_dept$ = "ALL"
               ed_dept$ = "ALL"
               return

L50450:     convert bg_dept$ to x%, data goto L50500

            convert x% to bg_dept$, pic(000)
            code$ = bg_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L50500

            if ed_dept$ <> " " then goto L50550
               ed_dept$ = bg_dept$
L50550:     if str(ed_dept$,1%,1%) <> "A" then goto L50600
               bg_dept$ = "ALL"
               ed_dept$ = "ALL"
               return
               
L50600:     convert ed_dept$ to x%, data goto L50500

            convert x% to ed_dept$, pic(000)
            code$ = ed_dept$
            tab%  = 1% : gosub check_code
            if code% = 0% then goto L50500

        return
L50500:     errormsg$ = "(Error) - Invalid Beg/End   Department Code ??"
            gosub error_prompt
            init(" ") bg_dept$, ed_dept$
        return    
L50700: REM Create Cyberquery File or Not         FILE$ 
            if file$ = " " then file$ = "N"
            if file$ <> "N" and file$ <> "Y" then goto L50790
        return
L50790:     errormsg$ = "(Error) - Invalid File Selection, 'Y' or 'N'??"
            gosub error_prompt
            init(" ") file$
        return
L50800: REM Include Items Not Shipped?            COMP$ 
            if comp$ = " " then comp$ = "N"
            if comp$ <> "N" and comp$ <> "Y" then goto L50890
        return
L50890:     errormsg$ = "(Error) - Invalid File Selection, 'Y' or 'N'??"
            gosub error_prompt
            init(" ") comp$
        return


L50900: REM Move to department 104?               SCR_CROSS$
            if scr_cross$ = " " then scr_cross$ = "Y"
            if scr_cross$ <> "N" and scr_cross$ <> "Y" then goto L50990
        return
L50990:     errormsg$ = "(Error) - Invalid File Selection, 'Y' or 'N'??"
            gosub error_prompt
            init(" ") scr_cross$
        return


        REM *************************************************************~
            *           S p e c i a l   S u b r o u t i n e s           *~
            *************************************************************


        report_analysis
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),0%, rslt$(12%))
            if f2%(12%) <> 0% then goto L00120
                  call "FILEBGON" addr(#12)
L00120:     call "OPENCHCK" (#12, fs%(12%), f2%(12%),200%, rslt$(12%))

            if bg_dept$ = "ALL" then gosub open_sch_all
            if bg_dept$ <> "ALL" then gosub open_sch_dpt
             gosub get_capacity
             gosub load_support
             cap_err% = 0%
             gosub get_planned
             if cap_err% <> 0% then return
             gosub get_open

        return

        open_sch_all
            if file$ = "N" then return
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),0%, rslt$(10%))
            if f2%(10%) <> 0% then goto L00100
                  call "FILEBGON" addr(#10)
L00100:     call "OPENCHCK" (#10, fs%(10%), f2%(10%),20%, rslt$(10%))
        return

        open_sch_dpt
            if file$ = "N" then return
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),0%, rslt$(11%))
            if f2%(11%) <> 0% then goto L00110
                  call "FILEBGON" addr(#11)
L00110:     call "OPENCHCK" (#11, fs%(11%), f2%(11%),20%, rslt$(11%))

        return


        get_capacity                                    /* (SR67906) */
            if auto% = 0% then call "SHOSTAT" (" Loading Capacity ") 
            gosub get_current_week

            init(" ") cap_key$
            str(cap_key$,1%,4%) = cur_year$
            str(cap_key$,5%,2%) = cur_wk$
        cap_nxt
            read #1, key > cap_key$, using L60000, cap_rec$, eod goto cap_done
L60000:              FMT CH(256)

                      cap_key$ = str(cap_rec$,7%,10%)
                      
                      gosub write_cap
                      goto cap_nxt
        cap_done
        return

        get_current_week
            init(" ") ent_dy$, ent_dte$, ent_date$
            call "AWDPLN0B" ( cur_yr$,    /* Current Production Year    */~
                              cur_wk$,    /* Current Production Week    */~
                              cur_dy$,    /* Current Production Day     */~
                              cur_dte$,   /* Current Production Date(6) */~
                              cur_date$,  /* Current Production Date(8) */~
                              ent_yr$,    /* Entry Production Year      */~
                              ent_wk$,    /* Entry Prod Week            */~
                              ent_dy$,    /* Entry Production Day       */~
                              ent_dte$,   /* Entry Production Date (6)  */~
                              ent_date$,  /* Entry Production Date *8)  */~
                              prv_yr$,    /* Previous Year              */~
                              #3,         /* GENCODES                   */~
                              prd_e%    )  /* 0% = No, 1% = Found        */
           
           if prd_e% <> 0% then goto L50375
           convert val(cur_yr$,2) to cur_year$,pic(0000)              
           convert val(ent_yr$,2) to ent_year$,pic(0000)              
           convert val(prv_yr$,2) to prv_year$,pic(0000)  


L50375:            
        return

        write_cap
              cap_date$ = str(cap_rec$,1%,6%)
              cap_year$ = str(cap_rec$,7%,4%)
              cap_wk$   = str(cap_rec$,11%,2%)
              cap_dept$ = str(cap_rec$,13%,3%)
              cap_dy$   = str(cap_rec$,16%,1%)

              get cap_rec$ using L60010, cap
L60010:             FMT POS(17), PD(14,4)
              cap_used% = 0%
              cap_used  = 0.00

              put #12, using L60100, cap_date$, cap_year$, cap_wk$, ~
                                     cap_dept$, cap_dy$, cap,       ~
                                     cap_used%, cap_used, cap_year$,~
                                     cap_wk$, cap_dy$, cap_dept$,   ~
                                     cap_filler$
              write #12

        return

        get_planned
REM            return                                   /* (SR67906) */
            if auto% = 0% then call "SHOSTAT" ("Checking Planned SO")
            dt_key1$ = all(hex(00))
            str(dt_key1$,1%,6%) = cur_dte$
            read #7, key 1% > dt_key1$, using L60000, dt_rec$, eod goto dt_done
                 goto read_dt_first
        plan_nxt
            read #7, using L60000, dt_rec$, eod goto dt_done

read_dt_first:
                                                  /* Status already complete */
                 dt_key1$ = str(dt_rec$,47%,57%)              /*  (EWD001)   */
              if comp$ = "N" and str(dt_rec$,64%,2%) > "11" then goto plan_nxt
              if comp$ = "Y" and str(dt_rec$,64%,2%) >= "18" then goto plan_nxt

                    checked% = checked% + 1%
                    if auto% <> 0% then goto L60110     /* (SR67906) */
                    if mod(checked%,100%) <> 0% then goto L60110
                    convert checked% to str(checked$,1%,8%), pic(########)
                    print at(02,02);hex(84);checked$;
L60110:

REM                 CALL "SHOSTAT" ("CHECKING PLANNED SO --> " & STR(DT_REC$,24%,10%))
                 dt_part$ = str(dt_rec$,189%,25%)
                 dt_dept$ = str(dt_rec$,42%,3%)

                 if bg_dept$ = "ALL" then goto L60020
                    if dt_dept$ < bg_dept$ or dt_dept$ > ed_dept$           ~
                                                         then goto plan_nxt

L60020:
REM                 CALL "SHOSTAT" (" I AM HERE ")  STOP
                 tqty% = 1%
                 gosub calc_eff
                 if str(dt_rec$,64%,2%) < "12" then gosub update_work

                 init(" ") sch_key$, sch_dte$
                 sch_dte$ = str(dt_key1$,1%,6%)
                 str(sch_key$,1%,6%) = sch_dte$
                 str(sch_key$,7%,8%) = str(dt_rec$,24%,8%)
                 convert str(dt_rec$,32%,2%) to cmg%, data goto L60030

L60030:
                 convert cmg% to str(sch_key$,15%,3%), pic(###)
 
                 str(sch_key$,18%,3%) = dt_dept$

                 init(" ") customer$
                 
                 customer$ = str(dt_rec$,124%,9%)
                 init(" ")or_status$
                 or_status$ = str(dt_rec$,64%,2%)
                 gosub lookup_due_date
                 if comp$ = "Y" and or_status$ >= "18" then goto plan_nxt
                 gosub check_mull
                 gosub lookup_cutoff
                                                      /*  (EWD001)  */
                 if str(dt_rec$,64%,2%) < "12" then planned$ = "P"  ~
                 else planned$ = "C" 


                 sch_price   = 0.00
                 for i% = 1% to supp_max%
                     if dt_dept$ = supp_dept$(i%) then goto L60045
                 next i%

REM                 CMG$ = STR(DT_REC$,133%,8%)
REM                    CONVERT STR(DT_REC$,133%,8%) TO SCH_PRICE, DATA GOTO L60035

                 get dt_rec$, using L60035, sch_price

L60035:              FMT POS(133), PD(14,4)    
L60045:
                 if bg_dept$ =  "ALL" then gosub update_sch_all
                 if bg_dept$ <> "ALL" then gosub update_sch_dpt

                      if cap_err% <> 0% then goto dt_done
                      goto plan_nxt
        dt_done
        return


        update_sch_all
               sch% = 10%
               goto update_sch
        update_sch_dpt
               sch% = 11%
               goto update_sch

        update_sch
               if file$ = "N" then return
               sch_tqty%, rec%   = 0% 
               sch_ef_unit = 0.00

               read #sch%, hold, key = sch_key$, eod goto sch_first

                     get #sch%, using L60040, sch_tqty%, sch_ef_unit

L60040:                  FMT POS(69), BI(4), PD(14,4)
                     rec% = 1%
                     tqty%   = tqty%   + sch_tqty%
                     ef_unit = ef_unit + sch_ef_unit
                     delete #sch%
        sch_first
REM                IF REC% = 1% THEN GOTO GOT_CAP
                   if planned$ = "U" then goto got_cap
REM                      CALL "SHOSTAT" ("I AM HERE ") STOP
                      init(" ") sd_dept$
REM                      STR(DT_KEY1$,1%,6%) = SCH_DTE$
                      sd_dept$            = dt_dept$
                      gosub check_cap
got_cap:
                put #sch%, using L60060,                            ~
                                sch_dte$,   /* Production Date    */~
                                str(sch_key$,7%,8%), /* SO Number */~
                                str(sch_key$,15%,3%),/* Line Item */~
                                str(sch_key$,18%,3%),/* Department*/~
                                or_status$, /* Order Status       */~
                                customer$,  /* Customer Code      */~
                                order_dte$, /* Order Date         */~
                                or_due$,    /* Default Due Date   */~
                                dt_part$,   /* Part Number        */~
                                tqty%,      /* Total Qty          */~
                                ef_unit,    /* Effective Units    */~
                                cap,        /* Department Capacity*/~
                                mull$,      /* MULL??             */~
                                or_hows$,   /* How Ship Code      */~
                                state$,     /* Customer State     */~
                                var_fld3$,  /* Customer Cut-Off   */~
                                var_fld4$,  /* Delivery Days      */~
                                or_region$, /* Customer Region    */~
                                or_route$,  /* Customer Route     */~
                                planned$,   /* Planned or Not?    */~
                                sch_price,  /* Line Prices        */~
                                subpart$,   /* (SR70514)          */~
                                infopart$,  /* (SR70514           */~
                                sch_filler$ /* Filler Area        */


                 write #sch%

L60060:          FMT CH(06), CH(08), CH(03), CH(03), CH(02), CH(09),   ~
                     CH(06), CH(06), CH(25), BI(4), PD(14,4), PD(14,4),~ 
                     CH(01), CH(02), CH(02), CH(02), CH(02), CH(02),   ~
                     CH(05), CH(01), PD(14,4), CH(20), CH(20), CH(103)
/* (SR70514) alter fmt stmt for subpart & infopart */                     
                
        return


        lookup_due_date
REM             call "SHOSTAT" (" HERE " )  stop
             init(" ") or_key$
             str(or_key$,1%,8%) = str(dt_rec$,24%,8%)
             read #4, key 4% = or_key$, eod goto due_done
                    get #4, using L60070, or_due$, or_region$, or_route$, ~
                                          or_status$, or_hows$, order_dte$

L60070:              FMT CH(06), POS(09), CH(02), CH(05), POS(60), CH(02), ~
                         POS(92), CH(02), POS(127), CH(06)
   
        due_done
        return

        calc_eff 
            ef_unit = 0.00
            ef_err% = 0%
            call "APCPLNEF" ( dt_part$,   /* Part Number                */~
                              tqty%,      /* Number of planning units IN*/~
                              ef_unit,    /* Number of effective unitOUT*/~
                              ef_err%,    /* Error Code              OUT*/~
                              #3)         /* FILE = (GENCODES)          */

            if ef_err% <> 0% then ef_unit = 0.00

        return   

        update_work
            cap_used% = 0%
            cap, cap_used  = 0.00

            gosub find_week_day
            if cap_err% <> 0% then return

            cap_date$ = str(dt_key1$,1%,6%)
            cap_dept$ = dt_dept$
            cap_used% = cap_used% + tqty%
            cap_used  = cap_used + ef_unit            

            cap% = 0%
            cap_key1$ = all(hex(00))
            cap_key1$ = str(dt_key1$,1%,6%)
            read #12, key 1% > cap_key1$, using L60000, cap_rec$, ~
                                                     eod goto work_done
                   goto work_first
        work_nxt
            read #12, using L60000, cap_rec$, ~
                                                     eod goto work_done
work_first:
                cap_key1$ = str(cap_rec$,1%,16%)

                if str(cap_key1$,13%,3%) <> dt_dept$ then goto work_nxt
                if str(cap_key1$,7%,4%)  <> cap_year$ then goto work_nxt
                if str(cap_key1$,11%,2%) <> cap_wk$ then goto work_nxt
                if str(cap_key1$,16%,1%) <> cap_dy$ then goto work_nxt


                cap_used% = 0%
                cap, cap_used  = 0.00
                get #12, using L60080, cap, cap_used%, cap_used
L60080:              FMT POS(17), PD(14,4), BI(4), PD(14,4)
                cap_used% = cap_used% + tqty%
                cap_used  = cap_used + ef_unit

                   cap% = 1%
        work_done
              if cap% = 0% then goto update_first
                 init(" ") cap_key$
                 cap_key$ = str(cap_rec$,7%,10%)
                 read #12, hold, key = cap_key$, eod goto update_first
                       delete #12
        update_first             
              put #12, using L60100, cap_date$, cap_year$, cap_wk$, ~
                                     cap_dept$, cap_dy$, cap,       ~
                                     cap_used%, cap_used, cap_year$,~
                                     cap_wk$, cap_dy$, cap_dept$,   ~
                                     cap_filler$
L60100:            FMT CH(06), CH(04), CH(02), CH(03), CH(01), PD(14,4), ~
                       BI(4), PD(14,4), CH(04), CH(02), CH(01), CH(03),  ~
                       CH(210)
              write #12

        return


        find_week_day
            cap_key1$ = all(hex(00))
            cap_key1$ = str(dt_key1$,1%,6%)
            read #12, key 1% > cap_key1$, using L60000, cap_rec$, ~
                                                     eod goto work_done

            cap_key1$ = str(cap_rec$,1%,16%)
            if str(cap_key1$,1%,6%) <> str(dt_key1$,1%,6%) then goto week_error
               cap_year$ = str(cap_rec$,7%,4%)
               cap_wk$   = str(cap_rec$,11%,2%)
               cap_dy$   = str(cap_rec$,16%,1%)
        return
        week_error
            call "DATFMTC" (str(dt_key1$,1%,6%), x%, dt_date$)
            errormsg$ = "(Error) - No Capacity for " & dt_date$
            gosub error_prompt
            cap_err% = 1%
        return


        get_open                                         /* (SR67906) */
            if auto% = 0% then call "SHOSTAT" (" Processing OR SO " )
            checked% = 0%
            or_key$ = all(hex(00))
            or_key$ = bg_dte$                       /* #4 - APCPLNOR */
            read #4, key > or_key$, using L60150, or_key$, or_so$,  ~
                                   or_status$, or_hows$, order_dte$,~
                                   eod goto open_done
L60150:               FMT CH(51), CH(08), CH(02), POS(92), CH(02),  ~
                          POS(127), CH(06)
                  goto open_first
        open_nxt                                        /* Reset for each SO # */
            read #4, using L60150, or_key$, or_so$, ~
                                   or_status$, or_hows$, order_dte$, ~
                                   eod goto open_done

open_first:         
                    if str(or_key$,1%,6%) > ed_dte$ then goto open_done
                    if (or_status$ > "01" and or_status$ < "90")      ~
                        or or_status$ = "99"   then goto open_nxt

                    checked% = checked% + 1%
                    if auto% <> 0% then goto L60210    /* (SR67906) */
                    if mod(checked%,100%) <> 0% then goto L60210
                    convert checked% to str(checked$,1%,8%), pic(########)
                    print at(02,02);hex(84);checked$;
L60210:

                    or_region$ = str(or_key$,9%,2%)
                    or_route$  = str(or_key$,11%,5%)
                    tab% = 2%
                    init(" ") code$    :    code$ = or_hows$
                    gosub check_code
                    if code% <> 0% then goto open_nxt
                    tab% = 6%                        /* Customer Code */
                    init(" ") code$    :    code$ = str(or_key$,27%,9%)
                    gosub check_code
/* (AWD003) - BEG */

/* Not in table to skip */
                    if code% = 0% then goto line_cust_default 

                    p%, cust_schema% = 0%
                    p% = pos("-" = desc$)
                    convert str(desc$,p%+1%,2%) to cust_schema%,   ~
                                        data goto line_cust_default

/* AT0205 - schema 1 keep   */ 
/* AT0206 - schema 2 keep   */
/* All other customers skip */
                    if schema% = cust_schema%      ~
                               then code% = 0%
line_cust_default:

/* (AWD003) */


                    if code% <> 0% then goto open_nxt

                    or_due$ = str(or_key$,1%,6%)
                    planned$ = "U"
                    gosub find_day
                    gosub open_sc

                       goto open_nxt
        open_done                                   /* (SR67906) */
            if auto% = 0% then call "SHOSTAT" (" Loading Screen ") 
            if auto% <> 0% then return              /* (SR67906) */
            	
            gosub load_screen
            gosub'102(0%, 1%)                   /* Select Sales Orders */
            init(" ") bg_due$, ed_due$, bg_dept$, ed_dept$, file$
REM            fieldnr% = 1%
REM            keyhit%  = 0%

        return clear all
        goto inputmode

        load_screen
            init(" ") cap_key$, cap_rec$
            cnt% = 0%
            read #12, key > cap_key$, using L60000, cap_rec$,       ~
                                                     eod goto scr_done
                 goto scr_first
        scr_nxt
            read #12, using L60000, cap_rec$,                           ~
                                                     eod goto scr_done
scr_first:
                      cap_key$ = str(cap_rec$,7%,10%)
                      convert str(cap_key$,10%,1%) to prd_day%, data goto L60400
L60400:

                      if prd_day% = 6% or prd_day% = 7% then goto scr_nxt

                      get #12, using L60080, cap, cap_used%, cap_used

                      if cap = 0% then goto scr_nxt
                      if prd_day% = 1% then cnt% = cnt% + 1%

                      if cnt% = 1% then goto L60460
                           if str(cap_rec$,13%,3%) <> "000" then goto L60460 
                              if prd_day% <> 1% then goto L60460
                              cmg% = cnt% / line%
                              cmg% = (cmg% + 1%) * line%
                              cmg% = cmg% - (cnt% - 1%)
                              cnt% = cnt% + cmg%
L60460:
                                                             /* Department  */
                      str(scrdte$(cnt%),1%,7%) = "DEPT   "
                      str(screen$(cnt%),2%,7%) =  str(cap_rec$,13%,3%)

                      if prd_day% <> 1% then goto L60410
                      init(" ") scrdte$
                      scrdte$ = str(cap_rec$,1%,6%)
                      call "DATFMTC" (scrdte$)
                      str(scrdte$(cnt%),8%,10%) = scrdte$
                      convert cap to                                  ~
                           str(screen$(cnt%),9%,4%), pic(####)

                      convert cap_used to                             ~
                          str(screen$(cnt%),14%,6%), pic(####.#)

L60410:

                      if prd_day% <> 2% then goto L60420
                      init(" ") scrdte$
                      scrdte$ = str(cap_rec$,1%,6%)
                      call "DATFMTC" (scrdte$)
                      str(scrdte$(cnt%),23%,10%) = scrdte$
                      convert cap to                                  ~
                           str(screen$(cnt%),23%,4%), pic(####)

                      convert cap_used to                             ~
                          str(screen$(cnt%),29%,6%), pic(####.#)

L60420:

                      if prd_day% <> 3% then goto L60430
                      init(" ") scrdte$
                      scrdte$ = str(cap_rec$,1%,6%)
                      call "DATFMTC" (scrdte$)
                      str(scrdte$(cnt%),38%,10%) = scrdte$
                      convert cap to                                  ~
                           str(screen$(cnt%),38%,4%), pic(####)

                      convert cap_used to                             ~
                          str(screen$(cnt%),44%,6%), pic(####.#)

L60430:

                      if prd_day% <> 4% then goto L60440
                      init(" ") scrdte$
                      scrdte$ = str(cap_rec$,1%,6%)
                      call "DATFMTC" (scrdte$)
                      str(scrdte$(cnt%),53%,10%) = scrdte$
                      convert cap to                                  ~
                           str(screen$(cnt%),53%,4%), pic(####)

                      convert cap_used to                             ~
                          str(screen$(cnt%),59%,6%), pic(####.#)

L60440:

                      if prd_day% <> 5% then goto L60450
                      init(" ") scrdte$
                      scrdte$ = str(cap_rec$,1%,6%)
                      call "DATFMTC" (scrdte$)
                      str(scrdte$(cnt%),68%,10%) = scrdte$
                      convert cap to                                  ~
                           str(screen$(cnt%),68%,4%), pic(####)

                      convert cap_used to                             ~
                          str(screen$(cnt%),74%,6%), pic(####.#)

L60450:
                      goto scr_nxt
        scr_done
             val_max% = cnt%
        return

        find_day
            call "DAY" addr(or_due$, day%)
            day% = day% - 1%    /* So Monday = 1, etc.  */
        return


        open_sc
            init(" ") sc_key$
            sc_key$ = or_so$
            tqty% = 0%
            mqty%, pqty%, pqty1% = 0%               /* (AWD003) */
        sc_nxt
/* (AWD003) add mqty, pqty and pqty1 */
            read #5, key > sc_key$, using L60200, sc_key$, sc_part$, ~
                                                 tqty%, mqty%, pqty%,~
                                                 pqty1%, sch_price,  ~
                                                 eod goto sc_done
L60200:                FMT POS(24), CH(10), CH(25), POS(68), BI(2),  ~
                           BI(2), BI(2), BI(2), POS(76), PD(14,4)


REM                       IF STR(SC_KEY$,1%,8%) <> "B7210041" THEN GOTO NOT_ORDER
                       	
REM                            CALL "SHOSTAT" ("SO # --> " & SC_KEY$)  STOP             	
REM NOT_ORDER
                       sav_price = 0.00
                       sch_price = sch_price / tqty%
                       sav_price = sch_price
                       if str(sc_key$,1%,8%) <> or_so$ then goto sc_done 
                       dt_part$ = sc_part$
                       gosub calc_eff
                       so_inv$  = str(sc_key$,1%,8%)     /* (AWD005) */
                       item_no$ = str(sc_key$,9%,2%)     /* (AWD005) */
                       gosub lookup_sub_part             /* (AWD005) */
                       gosub open_sd
                       goto sc_nxt
        sc_done
        return

        open_sd
            init(" ") sd_key$
            str(sd_key$,1%,10%) = sc_key$
        sd_nxt
            read #6, key > sd_key$, using L60250, sd_key$, eod goto sd_done
L60250:             FMT CH(23)

                  if str(sd_key$,1%,10%) <> str(sc_key$,1%,10%)       ~
                                                   then goto sd_done
                                            /* (AWD002) - Change POS */
                                            /* only include first shft */
                  if str(sd_key$,17%,2%) <> "01" then goto sd_nxt
                  sd_dept$ = str(sd_key$,12%,3%)

                 if bg_dept$ = "ALL" then goto L60260
                    if sd_dept$ < bg_dept$ or sd_dept$ > ed_dept$           ~
                                                         then goto plan_nxt

L60260:

                 gosub check_mull
REM                 CALL "SHOSTAT" (" UPDATE WORK FILE ")
                 supp% = 0%                        /* (AWD003) */
                 for i% = 1% to supp_max%
                     if sd_dept$ = supp_dept$(i%) then sch_price = 0.00
                     if sd_dept$ = supp_dept$(i%) then supp% = 1%
                 next i%
/*(AWD004)*/
                 if scr_cross$ = "N" then goto no_move
/*(AWD003) - begin */
                 if supp% = 0% and pqty1% <> 0% then sd_dept$ = "104"
                 if supp% = 0% and pqty%  <> 0% then sd_dept$ = "102"
/* (AWD005) */                 
                 if str(infopart$,7%,1%) = "3" then sd_dept$ = "103"
/*(AWD003) - END */
no_move:
/*(AWD004)*/

                 gosub calc_prod

                 init(" ") sch_key$, sch_dte$
                 sch_dte$ = prd_dte$
                 str(sch_key$,1%,6%) = sch_dte$
                 str(sch_key$,7%,8%) = or_so$
                 convert str(sc_key$,9%,2%) to cmg%, data goto L60090

L60090:
                 convert cmg% to str(sch_key$,15%,3%), pic(###)
 
                 str(sch_key$,18%,3%) = sd_dept$

                 gosub check_mull
REM                 CALL "SHOSTAT" (" UPDATE WORK FILE ")
                 supp% = 0%                        /* (AWD003) */
                 for i% = 1% to supp_max%
                     if sd_dept$ = supp_dept$(i%) then sch_price = 0.00
                     if sd_dept$ = supp_dept$(i%) then supp% = 1%
                 next i%
/*(AWD003) - begin */
                 if supp% = 0% and pqty1% <> 0% then sd_dept$ = "104"
                 if supp% = 0% and pqty%  <> 0% then sd_dept$ = "102"
/* (AWD005) */                 
                 if str(infopart$,7%,1%) = "3" then sd_dept$ = "103"
/*(AWD003) - END */
*/
                 if bg_dept$ =  "ALL" then gosub update_sch_all
                 if bg_dept$ <> "ALL" then gosub update_sch_dpt
                 sch_price = sav_price

                     goto sd_nxt
        sd_done
        return


        calc_prod
            init(" ") customer$, var_fld3$, var_fld4$, var_fld5$, var_fld6$
            customer$ = str(or_key$,27%,9%)
            gosub lookup_cutoff


            call "DATE" addr( "G+", or_due$, -(delivery_days%), ~
                                                  cut_dte$, err%)


               call "DAY" addr(cut_dte$, cut_day%)
               cut_day% = cut_day% - 1%    /* So Monday = 1, etc.  */
               if cust_day% < cut_day% then gosub adjust_day
               if cust_day% > cut_day% then gosub adjust_day

            nbr_days% = 2%
        prod_nxt
            if str(or_region$,1%,1%) = "H" and var_fld3$ = "08"  ~
                                then nbr_days% = nbr_days% + 1%
                                
            if str(or_region$,1%,1%) = "F" and var_fld3$ = "09"  ~
                                then nbr_days% = nbr_days% + 1%
                                
            if str(or_region$,1%,1%) = "M" and var_fld3$ = "10"  ~
                                then nbr_days% = nbr_days% + 1%
                                
            if str(or_region$,1%,1%) = "N" and var_fld3$ = "10"  ~
                                then nbr_days% = nbr_days% + 1%

            if str(or_region$,1%,1%) = "T" and var_fld3$ = "06"  ~
                                then nbr_days% = nbr_days% + 1%                                
                                
                                                                
            if (cut_day% + nbr_days%) = 6% then               ~
                                     nbr_days% = nbr_days% + 2%
            if (cut_day% + nbr_days%) = 7% then               ~
                                     nbr_days% = nbr_days% + 2%
                                     
                                     


            call "DATE" addr( "G+", cut_dte$, nbr_days%, prd_dte$, err%)
                 dt_key1$ = all(hex(00))
                 dt_key1$ = prd_dte$
                 gosub check_cap
                 if cap% = 0% then goto prod_done
                 if cap% = 2% then goto prod_nxt
                     dt_dept$ = sd_dept$
                     gosub update_work

        prod_done
        return

        adjust_day
          if cust_day% > cut_day% then goto adjust_forward
          if cust_day% < cut_day% then goto adjust_back

        adjust_forward
REM          CALL "SHOSTAT" ("ADJUST DAYS " & VAR_FLD4$)  STOP
          adjust% = 0% 
          adjust% = cust_day% - cut_day%
          delivery_days% = delivery_days% - adjust%
    
          call "DATE" addr( "G+", or_due$, -(delivery_days%), ~
                                                  cut_dte$, err%)
               call "DAY" addr(cut_dte$, cut_day%)
               cut_day% = cut_day% - 1%    /* So Monday = 1, etc.  */
        return
        adjust_back
REM          CALL "SHOSTAT" ("ADJUST DAYS " & VAR_FLD4$)  STOP
          adjust% = 0% 
          adjust% = cut_day% - cust_day%
          delivery_days% = delivery_days% + adjust%
    
          call "DATE" addr( "G+", or_due$, -(delivery_days%), ~
                                                  cut_dte$, err%)
               call "DAY" addr(cut_dte$, cut_day%)
               cut_day% = cut_day% - 1%    /* So Monday = 1, etc.  */
        return

        check_cap
            cap% = 0%
            cap_key1$ = all(hex(00))
            cap_key1$ = str(dt_key1$,1%,6%)
            read #12, key 1% > cap_key1$, using L60000, cap_rec$, ~
                                                     eod goto check_done
                   goto check_first
        check_nxt
            read #12, using L60000, cap_rec$, ~
                                                     eod goto check_done
check_first:
                cap_key1$ = str(cap_rec$,1%,16%)

                if str(cap_key1$,1%,6%) <> str(dt_key1$,1%,6%)          ~ 
                                                       then goto cap_done
                if str(cap_key1$,13%,3%) <> sd_dept$ then goto check_nxt


                cap_used% = 0%
                cap, cap_used  = 0.00
                get #12, using L60080, cap, cap_used%, cap_used
                                           /* Per Craig take out */ 
                                           /* even if over cap   */
                                           /* put in prd bucket  */
REM                REMAIN_CAP% = CAP - CAP_USED%
REM                REMAIN_CAP  = CAP - CAP_USED
REM                IF (REMAIN_CAP% - TQTY%) <= 0% THEN GOTO NEXT_DATE
REM                IF (REMAIN_CAP  - EF_UNIT) <= 0.00 THEN GOTO NEXT_DATE
                      cap% = 1%
        check_done
        return
REM     NEXT_DATE
            nbr_days% = nbr_days% + 1%
            cap% = 2%
        return



        lookup_cutoff
            read #8,key = customer$, using L60300 , state$, var_fld3$,       ~
                           var_fld4$, var_fld5$, var_fld6$, eod goto cutoff_done
L60300:        FMT POS(421), CH(02), POS(860), 4*CH(20)

               if var_fld5$ <> " " then gosub twice_week
               tab% = 3%
               init(" ") code$    :    code$ = var_fld4$
               gosub check_code               
               convert str(desc$,14%,2%) to delivery_days%, data goto L60350

L60350:
               tab% = 5%
               init(" ") code$    :    code$ = var_fld3$
               gosub check_code               
               convert str(desc$,1%,1%) to cust_day%, data goto L60360

L60360:
        cutoff_done
        return


        twice_week
            if (var_fld4$ = var_fld6$) and (var_fld3$ = var_fld6$) then return
                                      /* 3 = Wed; 4 = Thurs; Per Chad */
               if day% = 3% or day% = 4% then var_fld4$ = var_fld6$
               if day% = 3% or day% = 4% then var_fld3$ = var_fld5$
        return

        check_mull
           mull$ = "N"
           init(" ") wood_code$

           if len(dt_part$) = 22% then wood_code$ = str(dt_part$,20%,3%) ~
                                  else wood_code$ = str(dt_part$,23%,3%)
           
rem   Make sure not Special Shape               
           if str(dt_part$,7%,2%) > "99" and wood_code$ < "A00" then return

rem   Make sure this is not an old Code
            if wood_code$ < "A00" then return
            if wood_code$ < "A00" and str(dt_part$,7%,2%) < "A0" then return
            mull$ = "Y"
            init(" ") code$
            code$ = str(dt_part$,1%,3%) & wood_code$
            tab% = 4%
            gosub check_code
            if code% = 1% then mull$ = "I"
        return


        check_code
           code% = 0%
           readkey$ = " "
           str(readkey$,1%,9%)    = tab$(tab%)
           str(readkey$,10%,15%)  = code$
           read #3,key = readkey$, using L60050, desc$, eod goto gen_done
L60050:        FMT POS(25), CH(30)
           code% = 1%
        gen_done
        return

        open_error            
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
            err% = 0%
        return

        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        load_support
            init(" ") supp_dept$()
            supp_max% = 0%
            readkey$ = all(hex(00))
            str(readkey$,1%,9%) = "PLAN SUPP"
        load_support_next
            read #3,key > readkey$, using L60055, readkey$,            ~
                                                 eod goto load_support_done
L60055:        FMT POS(24)
            if str(readkey$,1%,9%) <> "PLAN SUPP" then goto load_support_done
               if str(readkey$,10%,3%) > "090" then load_support_done
                  supp_max% = supp_max% + 1%
                  supp_dept$(supp_max%) = str(readkey$,10%,3%)
                                                       /* Load Dept   */
               goto load_support_next
        load_support_done
        return
                 
        
        lookup_sub_part                              /* (AWD005) - BEG */
             init(" ") flag$, pgm$, bcksubpt_rec$, flds$(), info_flds$(), ~
                       subpart$, infopart$
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1"


            convert so_inv$ to so_inv%, data goto convert_alpha

            convert so_inv% to so_inv$, pic(00000000)

            goto order_converted

convert_alpha:
            convert str(so_inv$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
            convert so_inv% to str(so_inv$,2%,7%), pic(0000000)


order_converted:
            convert item_no$ to item_no%, data goto sub_part2
sub_part2:

            convert item_no% to item_no$, pic(###)


        call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #63,           /* BCKSUBPT File              */~
                          suberr1%)      /* Error Code                 */


            subpart$  = str(bcksubpt_rec$,48%,20%)
            infopart$ = str(bcksubpt_rec$,132%,20%)

            if suberr1% = 0% then return
            str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
            str(bcksubpt_rec$,132%,20%) = "00000000000000000000"
            subpart$ = str(bcksubpt_rec$,48%,20%)
            infopart$ = str(bcksubpt_rec$,132%,20%)
            
            errormsg$ = "AWDBKSUB ERROR = "&so_inv$ & " Line= " & item_no$
            gosub error_prompt
            suberr1% = 0%

        return                                       /* (AWD005) - end */
        
/* (SR67906) */        
        automate
          init(" ") bg_due$, ed_dte$, bg_dept$, ed_dept$, file$, comp$, scr_cross$
          call "DATE" addr("G+",todaysdate$, -30%,bg_dte$,err%)  
          call "DATE" addr("G+",todaysdate$, +30%,ed_dte$,err%)  
/* CR2907 */          
          if schema% = 2% then  ~
               call "DATE" addr("G+",todaysdate$, -14%,bg_dte$,err%) 
          if schema% = 2% then  ~
               call "DATE" addr("G+",todaysdate$, +252%,ed_dte$,err%) 
               
          bg_dept$, ed_dept$ = "ALL"
          file$, comp$, scr_cross$ = "Y"
          gosub report_analysis
          if auto% = 2% then comp$ = "N"
        return
/* (SR67906\) */
        exit_program

         end



