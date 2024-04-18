        REM *************************************************************~
            *                                                           *~
            *  EEEEE  W   W  DDDD   IIIII  NN  N  TTTTT   CCC   M    M  *~
            *  E      W   W  D   D    I    NN  N    T    C   C  MM  MM  *~
            *  EEEE   W   W  D   D    I    N N N    T    C      M MM M  *~
            *  E      W W W  D   D    I    N  NN    T    C   C  M    M  *~
            *  EEEEE   W W   DDDD   IIIII  N   N    T     CCC   M    M  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDINTCM - INTERNAL COMPLAINT TRACKING ENTRY/LOOKUP/REPORT*~
            *                                                           *~
            *   ( Security TABLE = 'CSECURIT1'- User Id's With Access ) *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/22/01 ! New Program for (EWD) - Last Mod Date    ! CMG *~  
            * 01/17/06 ! (PAR000) CR347 mods for sub part         ! CMG *~          
            *************************************************************

        dim                              /* (EWDINTCM) INT COMP FILE   */~
            hdr$40,                      /* ASKUSER HEADER             */~
            msg$(3%)79,                  /* ASKUSER TEXT               */~
            int_comp$1,                  /* Internal Comp "I"          */~            
            blankdate$10,                /* PD empty date              */~
            comp_number$8,               /* COMPLAINT NUMBER           */~
            comp_cuscode$9,              /* CUSTOMER CODE              */~
            cust_name$30,                /* CUSTOMER NAME              */~
            comp_so$8,                   /* SALES ORDER NUMBER         */~
            comp_line$2,                 /* S.O. LINE ITEM             */~
            comp_po$16,                  /* CUSTOMER PO NUMBER         */~
            comp_part$25,                /* PART NUMBER                */~
            comp_part_d$40,              /* PART NUMBER DESCRIPTION    */~
            comp_quan$4,                 /* QUANTITY                   */~
            comp_code$3,                 /* COMPLAINT CODE             */~
            comp_code_desc$32,           /* COMPLAINT CODE DESCRIPTION */~
            comp_init$2,                 /* INITIATOR CODE (INIT CODE) */~
            comp_init_desc$32,           /* INITIATOR CODE DESCRIPTION */~
            comp_init_dte$8,             /* DATE OF COMPLAINT          */~
            comp_init_txt$4,             /* INITIATOR TEXT ID          */~
            comp_init_txt1$3,            /* INITIATOR TEXT ID  YES/NO  */~
            comp_svcs$2,                 /* SERVICE CODE (SVCS CODE)   */~
            comp_svcs_desc$32,           /* SERVICE CODE DESCRIPTION   */~
            comp_crd$2,                  /* Credit   CODE              */~
            comp_crd_desc$32,            /* Credit   CODE DESCRIPTION  */~
            comp_cost$10,                /* COST OF COMPLAINT          */~
            comp_userid$3,               /* LAST MODIFIED BY           */~
            comp_dte$6,                  /* DATE LAST MODIFIED         */~
            comp_slmn$4,                 /* SALESMAN                   */~
            comp_slmn_desc$32,           /* SALESMAN Name              */~
/*PAR000*/  comp_filler$101,             /* FILLER AREA                */~
/*PAR000*/  comp_rec$256,                /* COMPLAINT RECORD           */~
/*PAR000*/  comp_subp$20,                /* Complaint subpart          */~
            comp_key$4,                  /* PRIMARY KEY                */~
            comp_mod$3, comp_mod_d$30,   /* Model and Description      */~
            textid$4,                    /* TEXT ID                    */~
            txt$4,                       /* TEXT ID                    */~
            readkey$50,                  /* GENCODES PRIMARY KEY       */~
            store_key$18,                /* STORE (000)                */~
            comp_no$8,                   /* NEXT COMPLAINT NUMBER      */~
            next_ref$8,                  /* NEXT REFERENCE NUMBER      */~
            descr$64,                    /* PLOWCODE DESCRIPTION       */~
            text$(113%,1%)70,            /* Text Buffer area           */~
            header$79,                   /*                            */~
            next_number$28,              /* Next Complaint Number      */~
            bck_key$25,                  /* BCKMASTR-BCKLINES KEY      */~
            scr$(10%)40,                 /* Report Selection Menu      */~
            beg_date$10, end_date$10,    /* Beginning/Ending Dates     */~
            beg_dte$10, end_dte$10,      /*                            */~
            tst_dte$10,                  /* Test Date                  */~
            beg_mod$3, end_mod$3,        /* Beg/End Model Codes        */~
            type$1,type_desc$32,         /* Summary or Detail          */~
            sel$1,                       /* Report Selection           */~
            sel_desc$34,                 /* Report Selection Descript  */~
            wrk$5,                       /* WRK REC NUMBER             */~
            wrk_key1$22,                 /* WORK SORT KEY              */~
            txt_1$20,                    /* PRINT TEXT DESCRIPTION     */~
            mask$80,                     /* PRINT TEXT MASK            */~
            company$60,                  /* For Report Company Name    */~
            print_title$43,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(21%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            pages$30,                    /* Max number of Pages        */~
            comp_err_dte$8,              /* DATE OF PSC/CSC Error      */~
            comp_ed_id$3,                /* Complaint Editor ID        */~
            comp_en_id$3,                /* Complaint Entry ID         */~
            comp_pr_id$3,                /* Complaint Proofer ID       */~
            comp_wizd$2,                 /* Window Wizard(COMP WIZD)   */~
            comp_wizd_desc$32            /* Window Wizard Complaint    */

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

        dim                              /* PAR000                     */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/26/97 Complaint Tracking System      "
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
            * #1  ! EWDINTCM ! INTERNAL COMPLAINT TRACKING MASTER FILE  *~
            * #3  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #4  ! GENCODES ! SYSTEM MASTER TABLE FILES                *~
            * #5  ! TXTFILE  ! MASTER SYSTEM TEXT FILE                  *~
            * #6  ! STORNAME ! MASTER STORE FILE (COMPLAINT NO ASSGN)   *~
            * #7  ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #8  ! BCKMASTR ! S.O. HEADER MASTER FILE                  *~
            * #9  ! AMTBOMIF ! VALIDTY FILE FOR DESCRIPTIONS            *~
            * #10 ! APCCOMP2 ! APC Work File                            *~
            * #11 ! HNYMASTR ! Part Master File                         *~
            * #15 ! SLMMASTR ! SALESMAN CODES                           *~
            * #16 ! TXTCOMPL ! Complaint Text File                      *~
            * #63 ! BCKSUBPT ! Sub Part File                 (PAR000)   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "EWDINTCM",                                     ~
/*PAR000*/              varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen =    5,                    ~
                        alt key  1, keypos =    6, keylen =  10,         ~
                            key  2, keypos =   16, keylen =  16, dup,    ~
                            key  3, keypos =   32, keylen =  13, dup 

            select #3,   "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =    9,                    ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #6, "SYSFILE2"                                        ~
                       varc, indexed, recsize = 500,                     ~
                       keypos = 1, keylen = 20

            select #7,   "BCKLINES",                                     ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =   19

            select #8,   "BCKMASTR",                                     ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =   25,                    ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #9,   "AMTBOMIF",                                     ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =   32

            select #10,  "APCCOMP2",                                     ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =    1, keylen =    5,                    ~
                        alt key  1, keypos =    6, keylen = 22, dup

            select #11, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #15, "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4


            select #16, "TXTCOMPL",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #17,  "USERINFO",                                     ~
                        varc,     indexed,  recsize =   150,            ~
                        keypos =    1, keylen =  3 

/*PAR000*/
            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup    

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%), f2%(1%),100%, rslt$(1%))

           filename$ = "CUSTOMER" : call "EWDOPEN" (#3, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "TXTFILE" : call "EWDOPEN" (#5, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "SYSFILE2" : call "EWDOPEN" (#6, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "BCKLINES" : call "EWDOPEN" (#7, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "BCKMASTR" : call "EWDOPEN" (#8, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "AMTBOMIF" : call "EWDOPEN" (#9, filename$, err%)
           if err% <> 0% then gosub open_error
           
           call "OPENCHCK" (#10, fs%(10%), f2%(10%),  0%, rslt$(10%))
           
           filename$ = "HNYMASTR" : call "EWDOPEN" (#11, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "SLMMASTR" : call "EWDOPEN" (#15, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "TXTCOMPL" : call "EWDOPEN" (#16, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "USERINFO" : call "EWDOPEN" (#17, filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
           if err% <> 0% then gosub open_error

            mat f1% = zer

            if fs%(10%) = 0 then goto L09000
               call "FILEBGON" addr(#10)

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "DATUFMTC" (blankdate$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            next_number$ = "Next Complaint No.: None    "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  19%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% then goto inputmode_report
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
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
                  if keyhit%  = 12% then gosub delete_complaint
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11140:     if cursor%(1%) <= 12 then fieldnr% = cursor%(1%) - 2%
            if cursor%(2%) > 40% then fieldnr% = fieldnr% + 1%
            
            if cursor%(1%) > 12 then fieldnr% = cursor%(1%) - 1%
            if cursor%(1%) > 14 then fieldnr% = cursor%(1%)
            if cursor%(1%) = 14% and cursor%(2%) > 40% then    ~
                     fieldnr% = 14%
            if fieldnr% < 1% or fieldnr% > 19% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11410:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11410
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11410
                  lastfieldnr% = fieldnr%
            goto L11140


        REM *************************************************************~
            *       I N P U T   M O D E   R E P O R T   S C R E E N     *~
            *************************************************************

        inputmode_report
            gosub initialize_variables

            for fieldnr% = 1% to 10%
L12080:         gosub'061(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12200
L12100:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12180
L12130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'061(fieldnr%)
                         if enabled% = 1% then L12100
                         if fieldnr% = 1% then L12080
                         goto L12130
L12180:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12100
L12200:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12100
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   R E P O R T   S C R E E N      *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg2
L13110:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 10% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'061(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13160:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13160
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13160
                  lastfieldnr% = fieldnr%
            goto L13110

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
           call "SHOSTAT" ("Selecting & Sorting Data")
           call "OPENCHCK" (#10,  fs%(10%), f2%(10%),500%, rslt$(10%))
           wrk% = 0%
           comp_key$ = all(hex(00))
           read #1,key > comp_key$, using L19080, comp_rec$,              ~
                                                      eod goto print_done
           goto L19085
        print_next
           read #1, using L19080, comp_rec$, eod goto print_done
L19080:      FMT CH(150)
L19085:    check% = 0%
           gosub check_data
           if check% = 0% then goto print_next
           wrk_key1$ = all(hex(00))
           on sel% gosub upd_1, upd_2, upd_3, upd_4, upd_5, upd_6, upd_7
           goto print_next
        print_done
           gosub generate_report
           call "FILEBGON" addr(#10)
        return clear all
        goto inputmode

        check_data                                   /* CUSTOMER CODE  */
          if str(comp_cuscode$,1%,3%) = "ALL" then goto L19170
          if str(comp_rec$,36%,9%) = comp_cuscode$ then goto L19170
             return
                                                     /* COMPLAINT CODE */
L19170:   if str(comp_code$,1%,1%) = "A" then goto L19190
          if str(comp_rec$,74%,3%) = comp_code$ then goto L19190
             return
                                                     /* INITIATOR CODE */
L19190:   if str(comp_init$,1%,1%) = "A" then goto L19210
          if str(comp_rec$,77%,2%) = comp_init$ then goto L19210
             return
                                                     /* CSC/PSC CODE   */
L19210:   if str(comp_svcs$,1%,1%) = "A" then goto L19230
          if str(comp_rec$,89%,2%) = comp_svcs$ then goto L19230
             return
                                                     /* Credit   CODE  */
L19230:   if str(comp_crd$,1%,1%) = "A" then goto L19250
          if str(comp_rec$,101%,2%) = comp_crd$ then goto L19250
             return
                                                     /* Wizard CODE    */
L19250:   if str(comp_wizd$,1%,1%) = "A" then goto L19270
          if str(comp_rec$,113%,2%) = comp_wizd$ then goto L19270
              return
                                                     /* COMPLAINT DATE */
L19270:   if str(beg_date$,1%,1%) = "A" then goto L19295
          if str(comp_rec$,79%,6%) < beg_dte$ or str(comp_rec$,79%,6%) > ~
                                            end_dte$ then return

                                                     /* Salesman Code */
L19295:   if str(comp_slmn$,1%,1%) = "A" then goto L19320
          if str(comp_rec$,32%,4%) = comp_slmn$ then goto L19320
             return

                                                     /* MODEL CODES    */
L19320:   if str(beg_mod$,1%,1%) = "A" then goto L19340
          if str(comp_rec$,45%,3%) < beg_mod$ or str(comp_rec$,45%,3%) > ~
                                            end_mod$ then return

L19340:   check% = 1%
        return

        upd_1
           str(wrk_key1$,1%,3%) = str(comp_rec$,82%,3%)  /* COMPLAINT  */
           str(wrk_key1$,4%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,13%,6%)= str(comp_rec$,87%,6%)  /* DATE       */
           goto update_work

        upd_2
           str(wrk_key1$,1%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,10%,3%)= str(comp_rec$,82%,3%)  /* COMPLAINT  */
           str(wrk_key1$,13%,6%)= str(comp_rec$,87%,6%)  /* DATE       */
           goto update_work

        upd_3
           str(wrk_key1$,1%,2%) = str(comp_rec$,121%,2%) /* SOURCE     */
           str(wrk_key1$,3%,3%) = str(comp_rec$,82%,3%)  /* COMPLAINT  */
           str(wrk_key1$,6%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,15%,6%)= str(comp_rec$,87%,6%)  /* DATE       */
           goto update_work

        upd_4
           str(wrk_key1$,1%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,10%,2%)= str(comp_rec$,121%,2%) /* SOURCE     */
           str(wrk_key1$,12%,3%)= str(comp_rec$,82%,3%)  /* COMPLAINT  */
           str(wrk_key1$,15%,6%)= str(comp_rec$,87%,6%)  /* DATE       */
           goto update_work

        upd_5
           str(wrk_key1$,1%,6%) = str(comp_rec$,87%,6%)  /* DATE       */
           str(wrk_key1$,7%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,16%,3%)= str(comp_rec$,82%,3%)  /* COMPLAINT  */
           goto update_work

        upd_6
           str(wrk_key1$,1%,4%) = str(comp_rec$,40%,4%)  /* SALESMAN   */
           str(wrk_key1$,5%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,14%,3%)= str(comp_rec$,82%,3%)  /* COMPLAINT  */
           str(wrk_key1$,17%,6%)= str(comp_rec$,87%,6%)  /* DATE       */
           goto update_work

        upd_7
           str(wrk_key1$,1%,3%) = str(comp_rec$,53%,3%)  /* Model      */
           str(wrk_key1$,4%,3%) = str(comp_rec$,82%,3%)  /* COMPLAINT  */
           str(wrk_key1$,7%,9%) = str(comp_rec$,44%,9%)  /* CUSTOMER   */
           str(wrk_key1$,16%,6%)= str(comp_rec$,87%,6%)  /* DATE       */
           goto update_work

        update_work
           wrk% = wrk% + 1%
           convert wrk% to wrk$, pic(00000)
           write #10, using L19605, wrk$, wrk_key1$, str(comp_rec$,1%,5%)
L19605:      FMT CH(5), CH(22), CH(5)
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        deffn'061(fieldnr%)
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
         "Enter Complaint Number for Look-up, or Leave Blank for Entry.",~
         "Enter a S.O.,'?' to Scan,' ' for LkUp or 'R' for System.(Req)",~
         "Enter a Valid S.O. Line No or Leave Blank for Look-Up.  (Req)",~
         "Enter a Valid Customer PO Number.                  (Optional)",~
         "Enter a Valid Customer Code or '?' for Look-Up     (Optional)",~
         "Enter a Valid Part Number or '?' for Look-Up       (Optional)",~
         "Enter a Valid Quantity.                            (Optional)",~
         "Enter Applicable Complaint Code.                        (Req)",~
         "Enter Applicable Initiator Code.                        (Req)",~
         "Enter, Edit or View Initiator Text. Use PF(8) Key.      (Req)",~
         "Enter Initiator Date.                                   (Req)",~
         "Enter Applicable CSC/PSC Error Code.                    (Req)",~
         "Enter Applicable Credit Code.                           (Req)",~
         "Enter Editor ID Initials.                               (Req)",~
         "Enter Date Error Occurred.                              (Req)",~
         "Enter Entry ID Initials.                                (Req)",~
         "Enter Proofer ID Initials.                              (Req)",~         
         "Enter The Applicable Cost of Complaint.                      "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28440
                inpmessage$ = edtmessage$
                return

L28440
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Report Selection.                              ",~
         "Enter (S) for Summary or (D) for Detail Report.              ",~
         "Enter a Valid Customer Code, (A) for ALL or '?' for Look-Up. ",~
         "Enter a Valid Complaint Code or (A) for All.                 ",~
         "Enter a Valid Initiator Code or (A) for All.                 ",~
         "Enter a Valid PSC/CSC Code or (A) for All.                   ",~
         "Enter a Valid Credit Code or (A) for All.                    ",~
         "Enter a Valid Window Wizard Code or (A) for All.             ",~
         "Enter a Valid Beginning and Ending Complaint Date or (A) All.",~
         "Enter a Valid Beginning and Ending Model Code or (A) for All."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, comp_number$, comp_so$,    ~
                      comp_cuscode$, comp_line$, comp_po$,               ~
                      comp_part$, comp_quan$, comp_code$, comp_init$,    ~
                      comp_init_dte$, comp_init_txt$, comp_svcs$,        ~
                      comp_crd$, comp_userid$, comp_wizd$, comp_wizd_desc$,~
                      comp_dte$, comp_filler$, readkey$, comp_code_desc$,~
                      comp_init_desc$, comp_svcs_desc$, comp_crd_desc$,  ~
                      comp_init_txt1$, comp_no$, descr$, cust_name$,     ~
                      comp_key$, comp_part_d$, sel_desc$, comp_cost$,    ~
                      type$, type_desc$, sel$, sel_desc$, beg_date$,     ~
                      end_date$, beg_dte$, end_dte$, comp_mod$,          ~
                      comp_mod_d$, comp_ed_id$, comp_en_id$, comp_pr_id$,~
                      comp_err_dte$, beg_mod$, end_mod$, int_comp$,      ~
/* PAR000 */          comp_subp$
                    
                      beg_dte$ = blankdate$
                      end_dte$ = blankdate$
                      tst_dte$ = blankdate$

            int_comp$ = "I"
            comp_slmn$ = "0000"
            comp_cost  = 0.0
            store_key$ = "DEFAULTS.STORE.000"
            gosub next_complaint_no
            init (hex(ff)) textid$, comp_init_txt$, text$()
            call "TXTFUTIL" (#16, f2%(16%), "INTL", textid$)

            scr$(1%) = "*********< Report Sort Options >********"
            scr$(2%) = "* (1) By Complaint Code,Customer,Date  *"
            scr$(3%) = "* (2) By Customer,Complaint Code,Date  *"
            scr$(4%) = "* (3) By Wizard,Complaint,Customer,Date*"
            scr$(5%) = "* (4) By Customer,Wizard,Complaint,Date*"
            scr$(6%) = "* (5) By Date,Customer,Complaint       *"
            scr$(7%) = "* (6) By Salesman,Customer,Complaint,DT*"
            scr$(8%) = "* (7) By Model,Complaint,Customer,Date *"
            scr$(9%) = "****************************************"


            readkey$ = all(hex(00))
            readkey$ = "CSECURIT1" & userid$
            call "DESCRIBE" (#4, readkey$, comp_code_desc$, 0%, f1%(4))
            if f1%(4%) = 0% then gosub check_security /* ACCESS DENIED */

            security% = 1%  /*0=Display Only; 1=Normal; 2=Delete Access*/  
            convert str(comp_code_desc$,,1%) to security%,               ~
                data goto L20500

L20500:     comp_code_desc$ = " "
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
            *************************************************************
        dataload
             get #1, using L35040, str(comp_number$,1%,4%), int_comp$,   ~
                                   comp_so$, comp_line$, comp_po$,       ~
                                   comp_slmn$, comp_cuscode$, comp_part$,~
                                   comp_quan$, comp_code$, comp_init$,   ~
                                   str(comp_init_dte$,1%,6%),            ~
                                   comp_init_txt$, comp_svcs$,           ~
                                   str(tst_dte$,1%,6%), comp_ed_id$,     ~
                                   comp_crd$, str(comp_err_dte$,1%,6%),  ~
                                   comp_en_id$,                          ~
                                   comp_wizd$, comp_pr_id$, comp_cost,   ~
                                   comp_userid$, comp_dte$,              ~
                                   comp_subp$, comp_filler$                                   

            gosub unpack_comp_number               
            
            if comp_init_dte$ = blankdate$ then comp_init_dte$ = " "
            if comp_err_dte$  = blankdate$ then comp_err_dte$  = " "
            if comp_dte$      = blankdate$ then comp_dte$      = " "


            gosub L50615                            /* CUSTOMER NAME    */
            gosub L50715                            /* PART DESCRIPTION */
            gosub L50895                            /* COMPLAINT CODE   */
            gosub L50975                            /* INITIATOR CODE   */
            gosub L51055                            /* INITIATOR DATE   */
            gosub L51110                            /* INITIATOR TEXT   */
            gosub L51140                            /* SERVICE CODE     */
            gosub L51260                            /* SERVICE TEXT     */
            gosub L51260                            /* Credit Code      */
            gosub L51350                            /* Editor ID        */
            gosub L51410                            /* Entry ID         */
            gosub L51520                            /* Proofer ID       */
            gosub L51380                            /* Date of Error    */
            gosub L51600                            /* Wizard Code      */

            gosub lookup_model                     /* Model Lookup     */
            errormsg$ = " "
            assign% = 0% : lookup% = 1%
            convert comp_cost to comp_cost$, pic(##,###.##-)

        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *************************************************************
        dataput
              if security% = 0% then return      /* For Safety... */
              if assign%  <> 0% then gosub assign_complaint
              call "DATUNFMT" (comp_init_dte$)
              call "DATUNFMT" (comp_err_dte$)
              gosub pack_comp_number                 
              
              read #1,hold,key = comp_key$, eod goto L31140
                 delete #1
L31140:       comp_userid$ = userid$ : comp_dte$ = date
              put #1, using L35040,str(comp_number$,1%,4%), int_comp$,   ~
                                   comp_so$, comp_line$, comp_po$,       ~
                                   comp_slmn$, comp_cuscode$, comp_part$,~
                                   comp_quan$, comp_code$, comp_init$,   ~
                                   str(comp_init_dte$,1%,6%),            ~
                                   comp_init_txt$, comp_svcs$,           ~
                                   str(blankdate$,1%,6%), comp_ed_id$,   ~
                                   comp_crd$, str(comp_err_dte$,1%,6%),  ~
                                   comp_en_id$,                          ~
                                   comp_wizd$, comp_pr_id$, comp_cost,   ~
                                   comp_userid$, comp_dte$,              ~
                                   comp_subp$, comp_filler$
              write #1
        return clear all
        goto inputmode

        REM *************************************************************~
            *       A S S I G N   C O M P L A I N T   N U M B E R       *~
            *************************************************************

        assign_complaint
            assign%, lookup% = 0%
            read #6,hold,key = store_key$, using L32080, comp_no$, next_ref$, ~
                                                          eod goto L32250
L32080:        FMT POS(262), CH(8), POS(282), CH(8)
            convert comp_no$ to comp_no%, data goto L32250

            convert comp_no% to comp_number$, pic(00000000)
            comp_no% = comp_no% + 1%
            convert comp_no% to comp_no$, pic(00000000)
            if ref% = 0% then goto L32220
               comp_so$   = str(next_ref$,1%,8%)
               comp_line$ = "01"
               convert str(next_ref$,2%,7%) to next_ref%,data goto L32250

               next_ref% = next_ref% + 1%
               convert next_ref% to str(next_ref$,2%,7%), pic(0000000)

L32220:     put #6, using L32080, comp_no$, next_ref$
            rewrite #6
        return
L32250:     stop "(Error) Updating Store (" & store_key$ &               ~
                  ") Complaint No. " & comp_no$
        return clear all
        goto inputmode

        next_complaint_no
            comp_no$ = " "
            read #6,key = store_key$, using L32080, comp_no$, next_ref$, ~
                                                   eod goto L32350
            str(next_number$,21%,8%) = str(comp_no$,1%,8%)
L32350: return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040: FMT                              /* (APCCOMPT) COMPLAINT FILE  */~
            CH(04),                      /* COMPLAINT NUMBER           */~
            CH(01),                      /* Internal Comp "I"          */~            
            CH(08),                      /* SALES ORDER NUMBER         */~
            CH(02),                      /* S.O. LINE ITEM             */~
            CH(16),                      /* CUSTOMER PO NUMBER         */~
            CH(04),                      /* SALESMAN                   */~
            CH(09),                      /* CUSTOMER CODE              */~
            CH(25),                      /* PART NUMBER                */~
            CH(04),                      /* QUANTITY                   */~
            CH(03),                      /* COMPLAINT CODE             */~
            CH(02),                      /* INITIATOR CODE (INIT CODE) */~
            CH(06),                      /* DATE OF COMPLAINT          */~
            CH(04),                      /* INITIATOR TEXT ID          */~
            CH(02),                      /* CSC/PSC CODE (SVCS CODE)   */~
            CH(06),                      /* BLANK   DATE               */~
            CH(04),                      /* ENTRY ID                   */~
            CH(02),                      /* Credit CODE                */~
            CH(06),                      /* COMPLAINT ERROR DATE       */~
            CH(04),                      /* COMPLAINT ENTRY ID         */~
            CH(02),                      /* COMP WIZARD CDE (COMP WIZD)*/~
            CH(04),                      /* COMPLAINT PROOFER ID       */~
            PD(14,4),                    /* COST OF COMPLAINT          */~
            CH(03),                      /* LAST MODIFIED BY           */~
            CH(06),                      /* DATE LAST MODIFIED         */~
/*PAR000*/  CH(20),                      /* subpart                    */~
/*PAR000*/  CH(101)                      /* FILLER AREA                */

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
              on fieldnr% gosub L40330,         /* Complaint Code    */   ~
                                L40320,         /* Sales Order No.   */   ~
                                L40330,         /* S.O. Line Item    */   ~
                                L40320,         /* Customer PO No.   */   ~
                                L40320,         /* Customer Number   */   ~
                                L40320,         /* Part Number       */   ~
                                L40330,         /* Quantity          */   ~
                                L40320,         /* Complaint Code    */   ~
                                L40320,         /* Initiator Code    */   ~
                                L40320,         /* Initiator Text    */   ~
                                L40320,         /* Initiator Date    */   ~
                                L40320,         /* CSC/PSC Error Code*/   ~
                                L40320,         /* Editor ID         */   ~
                                L40320,         /* Initiator Date    */   ~                                
                                L40320,         /* Entry ID          */   ~
                                L40320,         /* Proofer ID        */   ~
                                L40320,         /* Wizard Code       */   ~                                
                                L40320,         /* Credit Code       */   ~                                
                                L40330          /* Cost of complaint */
              goto L40350

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40320:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40330:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40350:     accept                                                       ~
               at (01,02),                                               ~
                  "INTERNAL COMPLAINTS - Entry and Lookup - (EWDINTCM)", ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "* Internal Comp Num :",                      ~
               at (03,25), fac(lfac$(1%)), comp_number$         , ch(08),~
               at (03,34), fac(hex(a4)), int_comp$              , ch(01),~
               at (03,40), fac(hex(a4))  , next_number$         , ch(28),~
               at (03,69), fac(hex(a4)), int_comp$              , ch(01),~               
                                                                         ~
               at (04,02), "* Sales Order Number:",                      ~
               at (04,25), fac(lfac$(2%)), comp_so$             , ch(08),~
                                                                         ~
               at (05,02), "* S.O. Line Item No.:",                      ~
               at (05,25), fac(lfac$(3%)), comp_line$           , ch(02),~
                                                                         ~
               at (06,02), "* Customer PO Number:",                      ~
               at (06,25), fac(lfac$(4%)), comp_po$             , ch(16),~
                                                                         ~
               at (07,02), "  Customer Code     :",                      ~
               at (07,25), fac(lfac$(5%)), comp_cuscode$        , ch(09),~
               at (07,40), fac(hex(84)),   cust_name$           , ch(30),~
               at (07,75), fac(hex(84)),   comp_slmn$           , ch(04),~
                                                                         ~
               at (08,02), "  Part Number       :",                      ~
               at (08,25), fac(lfac$(6%)), comp_part$           , ch(25),~
/*PAR000*/     at (08,52), fac(lfac$(6%)), comp_subp$           , ch(20),~
/*PAR000*/     at (09,52), fac(hex(84)),str(comp_part_d$,1%,28%), ch(28),~
                                                                         ~
               at (09,02), "  Quantity-No.Items :",                      ~
               at (09,25), fac(lfac$(7%)), comp_quan$           , ch(04),~
                                                                         ~
               at (10,02), "  Internal Comp Code:",                      ~
               at (10,25), fac(lfac$(8%)), comp_code$           , ch(03),~
               at (10,40), fac(hex(84)),   comp_code_desc$      , ch(32),~
                                                                         ~
               at (11,02), "  Initiator Code    :",                      ~
               at (11,25), fac(lfac$(9%)), comp_init$           , ch(02),~
               at (11,40), fac(hex(84)),   comp_init_desc$      , ch(32),~
                                                                         ~
               at (12,02), "  Initiator Text    :",                      ~
               at (12,25), fac(lfac$(10%)), comp_init_txt1$     , ch(03),~
               at (12,40), "Date:",                                      ~
               at (12,46), fac(lfac$(11%)), comp_init_dte$      , ch(08),~
                                                                         ~
               at (13,02), "  CSC/PSC Error Code:",                      ~
               at (13,25), fac(lfac$(12%)), comp_svcs$          , ch(02),~
               at (13,40), fac(hex(84)),   comp_svcs_desc$      , ch(32),~
                                                                         ~
               at (14,02), "  Editor Initials   :",                      ~
               at (14,25), fac(lfac$(13%)), comp_ed_id$         , ch(03),~
                                                                         ~
               at (14,40), "Date of Err:",                               ~
               at (14,53), fac(lfac$(14%)), comp_err_dte$       , ch(08),~
                                                                         ~
               at (15,02), "  Entry Initials    :",                      ~
               at (15,25), fac(lfac$(15%)), comp_en_id$         , ch(03),~
                                                                         ~
               at (16,02), "  Proofer Initials  :",                      ~
               at (16,25), fac(lfac$(16%)), comp_pr_id$         , ch(03),~
                                                                         ~
               at (17,02), "  Window Wizard Code: ",                     ~
               at (17,25), fac(lfac$(17%)), comp_wizd$          , ch(02),~
               at (17,40), fac(hex(84)),   comp_wizd_desc$      , ch(32),~
                                                                         ~
               at (18,02), "  Credit  Code      :",                      ~
               at (18,25), fac(lfac$(18%)), comp_crd$           , ch(02),~
               at (18,40), fac(hex(84)),   comp_crd_desc$       , ch(32),~
                                                                         ~
               at (19,02), "  Cost of Complaint :",                      ~
               at (19,25), fac(lfac$(19%)), comp_cost$          , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% = 8% and fieldnr% = 10% then                   ~
                                                  gosub edit_init_text

               if keyhit% <> 15 then goto L41190
                  call "PRNTSCRN"
                  goto L40350

L41190:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41410     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                 (8)Add/Edit Text       " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff08ffffffffff0e0f1000)
            if fieldnr% = 1% then L41340
               str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41340:     if fieldnr% > 1% then L41360
               str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41360:     if fieldnr% = 10% then goto L41390
               str(pf$(3%),18%,26%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L41390:     return

L41410: if fieldnr% > 0% then L41550  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (8)Add/Edit/Display Tex" &        ~
                     "t                      (15)Print Screen"
            pf$(3%)= "                 (12)Delete Complaint   " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffff08ffffff0cffff0f1000)
            if fieldnr% = 10% then goto L41520
               str(pf$(2%),18%,26%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L41520:     if assign% = 0% and security% = 2% then goto L41530           
               str(pf$(3%),18%,26%) = " " : str(pfkeys$,12%,1%) = hex(ff)
L41530:     if security% <> 0% then goto L41540:                          
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41540:     return                                                        
L41550:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (8)Add/Edit/Display Tex" &        ~
                     "t                                      "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffff08ffffffffffffffff00)
            if fieldnr% = 10% then goto L41630
               str(pf$(2%),18%,26%) = " " : str(pfkeys$,8%,1%) = hex(ff)
L41630:     return


        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42250,         /* REPORT SELECTION  */   ~
                                L42240,         /* ENTRY (2)         */   ~
                                L42240,         /* ENTRY (3)         */   ~
                                L42240,         /* ENTRY (4)         */   ~
                                L42240,         /* ENTRY (5)         */   ~
                                L42240,         /* ENTRY (6)         */   ~
                                L42240,         /* ENTRY (7)         */   ~
                                L42240,         /* ENTRY (8)         */   ~
                                L42240,         /* ENTRY (9)         */   ~
                                L42240          /* ENTRY (10)        */
              goto L42270

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42240:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L42250:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42270:     accept                                                       ~
               at (01,02),                                               ~
                  "Complaint Tracking - Report Screen - (APCCOMPT)",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Report Selection    :",                      ~
               at (03,25), fac(lfac$(1%)), sel$                 , ch(01),~
               at (03,40), fac(hex(84))  , sel_desc$            , ch(33),~
                                                                         ~
               at (04,02), "(S)ummary, (D)etail :",                      ~
               at (04,25), fac(lfac$(2%)), type$                , ch(01),~
               at (04,30), fac(hex(84))  , type_desc$           , ch(14),~
               at (04,45), "SLMN:",                                      ~
               at (04,51), fac(lfac$(2%)), comp_slmn$           , ch(04),~
         at (04,56), fac(hex(84))  , str(comp_slmn_desc$,1%,23%), ch(23),~
                                                                         ~
               at (05,02), "Customer Code       :",                      ~
               at (05,25), fac(lfac$(3%)), comp_cuscode$        , ch(09),~
               at (05,40), fac(hex(84))  , cust_name$           , ch(32),~
                                                                         ~
               at (06,02), "Complaint Code,(A)ll:",                      ~
               at (06,25), fac(lfac$(4%)), comp_code$           , ch(03),~
               at (06,40), fac(hex(84))  , comp_code_desc$      , ch(32),~
                                                                         ~
               at (07,02), "Initiator Code,(A)ll:",                      ~
               at (07,25), fac(lfac$(5%)), comp_init$           , ch(02),~
               at (07,40), fac(hex(84))  , comp_init_desc$      , ch(32),~
                                                                         ~
               at (08,02), "CSC/PSC Code,  (A)ll:",                      ~
               at (08,25), fac(lfac$(6%)), comp_svcs$           , ch(02),~
               at (08,40), fac(hex(84))  , comp_svcs_desc$      , ch(32),~
                                                                         ~
               at (09,02), "Credit Code,   (A)ll:",                      ~
               at (09,25), fac(lfac$(7%)), comp_crd$            , ch(02),~
               at (09,40), fac(hex(84))  , comp_crd_desc$       , ch(32),~
                                                                         ~
               at (10,02), "Wizard Code,   (A)ll:",                      ~
               at (10,25), fac(lfac$(8%)), comp_wizd$           , ch(02),~
               at (10,40), fac(hex(84))  , comp_wizd_desc$      , ch(32),~
                                                                         ~
               at (11,02), "Beginning Date      :",                      ~
               at (11,25), fac(lfac$(9%)), beg_date$            , ch(10),~
               at (11,40), "Ending Date         :",                      ~
               at (11,65), fac(lfac$(9%)), end_date$            , ch(10),~
                                                                         ~
               at (12,02), "Beginning Model Code:",                      ~
               at (12,25), fac(lfac$(10%)), beg_mod$            , ch(03),~
               at (12,40), "Ending Model Code   :",                      ~
               at (12,65), fac(lfac$(10%)), end_mod$            , ch(03),~
                                                                         ~
               at (13,21), fac(hex(84)),   scr$(1%)             , ch(40),~
               at (14,21), fac(hex(84)),   scr$(2%)             , ch(40),~
                                                                         ~
               at (15,21), fac(hex(84)),   scr$(3%)             , ch(40),~
               at (16,21), fac(hex(84)),   scr$(4%)             , ch(40),~
               at (17,21), fac(hex(84)),   scr$(5%)             , ch(40),~
                                                                         ~
               at (18,21), fac(hex(84)),   scr$(6%)             , ch(40),~
               at (19,21), fac(hex(84)),   scr$(7%)             , ch(40),~
                                                                         ~
               at (20,21), fac(hex(84)),   scr$(8%)             , ch(40),~
               at (21,21), fac(hex(84)),   scr$(9%)             , ch(40),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L43020
                  call "PRNTSCRN"
                  goto L42270

L43020:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L43190     /*  Input Mode             */
            pf$(1%)= "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(2%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L43150
               str(pf$(2%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L43150:     if fieldnr% > 1% then L43170
               str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L43170:     return

L43190: if fieldnr% > 0% then L43260  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over    (14)Generate Report    " &        ~
                     "                       (15)Print Screen"
            pf$(2%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L43260:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
              on fieldnr% gosub L50160,         /* Complaint Code    */   ~
                                L50250,         /* Sales Order No.   */   ~
                                L50405,         /* S.O. Line Item    */   ~
                                L50500,         /* Customer PO No.   */   ~
                                L50615,         /* Customer Number   */   ~
                                L50715,         /* Part Number       */   ~
                                L50835,         /* Quantity          */   ~
                                L50895,         /* Complaint Code    */   ~
                                L50975,         /* Initiator Code    */   ~
                                L51110,         /* Initiator Text    */   ~
                                L51055,         /* Initiator Date    */   ~
                                L51140,         /* CSC/PSC Err Code  */   ~
                                L51350,         /* Editor ID         */   ~
                                L51380,         /* Date Error Occurred*/  ~
                                L51410,         /* Entry ID          */   ~
                                L51520,         /* Proofer ID        */   ~
                                L51600,         /* Window wizard Code*/   ~
                                L51260,         /* Credit Code       */   ~
                                L51550          /* Cost of Complaint */

            return

L50160: REM COMPLAINT NUMBER                      COMP_NUMBER$
           assign%, ref%, dataload% = 0%
           if comp_number$ <> " " then goto L50190
              if security% <> 0%  then assign% = 1%
              return

L50190:    gosub pack_comp_number 

           read #1,key = comp_key$, eod goto L50230
           dataload% = 1%   :   gosub dataload
           fieldnr% = 20%   :   dataload% = 0%
        return
L50230:    errormsg$ = "(Error) - Complaint Number Not on File?"
           init(" ") comp_number$, comp_key$
        return

L50250: REM SALES ORDER                           COMP_SO$
            ref% = 0%
            if str(comp_so$,1%,1%) = "R" then goto L50315
            if comp_so$ <> " " then L50260                        
              return
L50260:     if comp_so$ <> "?" then goto L50275                    
              comp_so$ = " "                                      
              descr$ = hex(06) & "Select Sales Order"              
              call "PLOWCODE" (#7,comp_so$,descr$,-16%,-.001,f1%(3))
              return
L50275:     convert comp_so$ to comp_so%, data goto L50300

            convert comp_so% to comp_so$, pic(00000000)
            gosub lookup_order
        return
L50300:     errormsg$ = "(Error) - Invalid Sales Order Number?"
            init(" ") comp_so$
        return
L50315:     if len(comp_so$) > 1% then goto L50340
               ref% = 1%
               comp_so$ = str(next_ref$,1%,8%)
               comp_line$ = "01"
        return
L50340:     comp_key1$ = all(hex(00))
            convert str(comp_so$,2%,7%) to comp_so%, data goto L50300

            convert comp_so% to str(comp_so$,2%,7%), pic(0000000)

            comp_line$ = "01"
            str(comp_key1$,1%,8%) = comp_so$
            str(comp_key1$,9%,2%) = comp_line$
            read #1,key 1% = comp_key1$, eod goto L50300
            gosub dataload
            fieldnr% = 21%
        return

L50405: REM SALES ORDER LINE ITEM                 COMP_LINE$
            if comp_so$ <> " " then goto L50420
               return
L50420:     if comp_line$ <> " " then goto L50435
L50425:        comp_line$ = "01"

L50435:     convert comp_line$ to comp_line%, data goto L50425

            convert comp_line% to comp_line$, pic(00)
            comp_key1$ = all(hex(00))
            str(comp_key1$,1%,8%) = comp_so$
            str(comp_key1$,9%,2%) = comp_line$
            read #1,key 1% = comp_key1$, eod goto L50485
            gosub dataload
            fieldnr% = 21%
        return
L50485:     gosub lookup_order
        return

L50500: REM CUSTOMER PO NUMBER                    COMP_PO$
            if comp_po$ <> " " then goto L50515
            if comp_so$ <> " " then return
               fieldnr% = 1%
               return
L50515:     read #1,key 2% = comp_po$, eod goto L50530
            gosub dataload
            fieldnr% = 21%
L50530: return

L50615: REM CUSTOMER NUMBER                       COMP_CUSCODE$
           if str(comp_cuscode$,1%,1%) = "?" then goto L50640
           if comp_cuscode$ <> " " then goto lookup_customer
              goto L50695

L50640:       comp_cuscode$ = " "
              descr$ = hex(06) & "Select Customer Code"
              call "PLOWCODE" (#3,comp_cuscode$, descr$,0%,.30,f1%(3))
              if f1%(3) = 0 then L50695
        lookup_customer
            read #3,key = comp_cuscode$, eod goto L50705
            get #3, using L50675, cust_name$, comp_slmn$
L50675:       FMT POS(10), CH(30), POS(714), CH(04)
          comp_slmn_desc$ = " "
          call "DESCRIBE" (#15,comp_slmn$, comp_slmn_desc$, 0%, f1%(15))
          if f1%(15) <> 0 then return
L50695:      comp_slmn$ = "0000"
             comp_slmn_desc$ = " "
L50705: return

L50715: REM PART NUMBER                           COMP_PART$
            if str(comp_part$,1%,1%) = "?" then goto L50735
            if comp_part$ <> " " then goto L50760
               goto L50820
L50735:      comp_part$ = " "
             comp_part_d$ = hex(06) & "Select a Part for Complaint"
             call "GETCODE" (#11,comp_part$,comp_part_d$,0%,1.32,f1%(11))
             if f1%(11) = 0 then goto L50820

L50760:      if len(comp_part$) <> 3 then goto L50795
                readkey$ = all(hex(00))
                readkey$ = "MODEL    " & str(comp_part$,1%,3%)
                call "DESCRIBE" (#4, readkey$, comp_part_d$, 0%, f1%(4))
                if f1%(4) = 0 then comp_part_d$ = " "
                return

L50795:     call "APCLDSUB" (comp_part$, comp_part_d$, #9, err%)
            if err% <> 0% then goto L50810
        return
L50810:     comp_part_d$ = "Part Number Not in HNYMASTR File"
        return
L50820:     init(" ") comp_part$, comp_part_d$
        return

L50835: REM QUANTITY                              COMP_QUAN$
            if comp_quan$ <> " " then goto L50855
               comp_quan$ = "0001"

L50855:     convert comp_quan$ to comp_quan%, data goto L50875

            convert comp_quan% to comp_quan$, pic(0000)
        return
L50875:     errormsg$ = "(Error) - Invalid Quantity Entered?"
            init(" ") comp_quan$
        return

L50895: REM COMPLAINT CODE                        COMP_CODE$
            if comp_code$ <> " " then goto L50930
               readkey$ = all(hex(00))
               readkey$ = "COMPLAIN1"
               descr$ =hex(06)&"Select a Valid Complaint Code (Required)"
               call "PLOWCODE" (#4, readkey$, descr$, 9%, .30, f1%(4))
               comp_code$ = str(readkey$,10%,3%)
L50930:     readkey$ = all(hex(00))
            readkey$ = "COMPLAIN1" & comp_code$
            call "DESCRIBE" (#4, readkey$, comp_code_desc$, 0%, f1%(4))
            if f1%(4) = 0 then goto L50955
        return
L50955:   errormsg$ = "(Error) - Invalid Complaint Code? (Required)"
          init(" ") comp_code$, comp_code_desc$
        return

L50975: REM INITIATOR CODE                        COMP_INIT$
            if comp_init$ <> " " then goto L51010
               readkey$ = all(hex(00))
               readkey$ = "INIT CODE"
               descr$ =hex(06)&"Select a Valid Initiator Code (Required)"
               call "PLOWCODE" (#4, readkey$, descr$, 9%, .30, f1%(4))
               comp_init$ = str(readkey$,10%,2%)
L51010:     readkey$ = all(hex(00))
            readkey$ = "INIT CODE" & comp_init$
            call "DESCRIBE" (#4, readkey$, comp_init_desc$, 0%, f1%(4))
            if f1%(4) = 0 then goto L51035
        return
L51035:   errormsg$ = "(Error) - Invalid Initiator Code? (Required)"
          init(" ") comp_init$, comp_init_desc$
        return

L51055: REM INITIATOR DATE                        COMP_INIT_DTE$
          if comp_init_dte$ <> " " then goto L51070
             comp_init_dte$ = date
L51070:   date% = 0%
          call "DATEOK" (comp_init_dte$, date%, errormsg$)
          if errormsg$ <> " " then comp_init_dte$ = " "
                                           /* DEFAULT REMAINING FIELDS */
REM          comp_svcs$ = "00"
          comp_cost$ = "0.00"
        return

L51110: REM INITIATOR TEXT FLAG                   COMP_INIT_TXT$
             gosub'099(comp_init_txt$)
             comp_init_txt1$ = "No "
             if txt% = 1% then comp_init_txt1$ = "Yes"
        return

L51140: REM CSC/PSC Error Code                    COMP_SVCS$
            if comp_svcs$ <> " " then goto L51175
               readkey$ = all(hex(00))
               readkey$ = "SVCS CODE"
               descr$ =hex(06)&"Select a Valid PSC/CSC Code (Required)"
               call "PLOWCODE" (#4, readkey$, descr$, 9%, .30, f1%(4))
               comp_svcs$ = str(readkey$,10%,2%)
L51175:     readkey$ = all(hex(00))
            readkey$ = "SVCS CODE" & comp_svcs$
            call "DESCRIBE" (#4, readkey$, comp_svcs_desc$, 0%, f1%(4))
            if f1%(4) = 0 then goto L51200
        return
L51200:   errormsg$ = "(Error) - Invalid CSC/PSC Code? (Required)"
          init(" ") comp_svcs$, comp_svcs_desc$
        return


L51350: REM Editior ID                            COMP_ED_ID$
           readkey$ = all(hex(00))
           readkey$ = comp_ed_id$
           call "DESCRIBE" (#17, readkey$, " ", 0%, f1%(17))
           if f1%(17) = 0 then goto L51370          
        return
L51370:    comp_ed_id$ = "000"
        return

L51380: REM DATE ERROR OCCURRED                   COMP_ERR_DTE$
          if comp_err_dte$ <> " " then goto L51390
             comp_err_dte$ = date
L51390:   date% = 0%
          call "DATEOK" (comp_err_dte$, date%, errormsg$)
          if errormsg$ <> " " then comp_err_dte$ = " "
        return

L51410: REM  Entry ID                             COMP_EN_ID$
           readkey$ = all(hex(00))
           readkey$ = comp_ed_id$
           call "DESCRIBE" (#17, readkey$, " ", 0%, f1%(17))
           if f1%(17) = 0 then goto L51420           
        return
L51420:    comp_en_id$ = "000"
        return

L51520: REM Proofer ID                            COMP_PR_ID$
           readkey$ = all(hex(00))
           readkey$ = comp_ed_id$
           call "DESCRIBE" (#17, readkey$, " ", 0%, f1%(17))
           if f1%(17) = 0 then goto L51530           
        return
L51530:    comp_pr_id$ = "000"
        return

L51600: REM  Window Wizard Code                   COMP_WIZD$           
            if comp_wizd$ <> " " then goto L51620
               readkey$ = all(hex(00))
               readkey$ = "COMP WIZD"
               descr$ =hex(06)&"Select a Valid Window Wizard Code (Required)"
               call "PLOWCODE" (#4, readkey$, descr$, 9%, .30, f1%(4))
               comp_wizd$ = str(readkey$,10%,2%)
L51620:     readkey$ = all(hex(00))
            readkey$ = "COMP WIZD" & comp_wizd$
            call "DESCRIBE" (#4, readkey$, comp_wizd_desc$, 0%, f1%(4))
            if f1%(4) = 0 then goto L51630
        return
L51630:   errormsg$ = "(Error) - Invalid Window Wizard Code? (Required)"
          init(" ") comp_wizd$, comp_wizd_desc$
        return

L51260: REM  CREDIT  CODE                         COMP_CRD$           
            if comp_crd$ <> " " then goto L51270
               readkey$ = all(hex(00))
               readkey$ = "TRCK CODE"
               descr$ =hex(06)&"Select a Valid Credit Code (Required)"
               call "PLOWCODE" (#4, readkey$, descr$, 9%, .30, f1%(4))
               comp_crd$ = str(readkey$,10%,2%)
L51270:     readkey$ = all(hex(00))
            readkey$ = "TRCK CODE" & comp_crd$
            call "DESCRIBE" (#4, readkey$, comp_crd_desc$, 0%, f1%(4))
            if f1%(4) = 0 then goto L51280
        return
L51280:   errormsg$ = "(Error) - Invalid Credit Code? (Required)"
          init(" ") comp_crd$, comp_crd_desc$
        return
        
L51550: REM COST OF COMPLAINT                     COMP_COST$
             comp_cost = 0.0
             convert comp_cost$ to comp_cost, data goto L51590

             comp_cost = round(comp_cost, 2)
             convert comp_cost to comp_cost$, pic(##,###.##-)

        return
L51590:      errormsg$ = "(Error) - Invalid Cost?"
             comp_cost = 0.0 : comp_cost$ = " "
        return


        deffn'152(fieldnr%)
            errormsg$ = " "
              on fieldnr% gosub L51765,         /* Report Selection  */   ~
                                L51830,         /* (S) or (D)        */   ~
                                L51950,         /* Customer Code     */   ~
                                L52005,         /* Complaint Code    */   ~
                                L52055,         /* Initiator Code    */   ~
                                L52105,         /* CSC/PSC Code      */   ~
                                L52155,         /* Credit Code       */   ~
                                L52205,         /* Wizard Code       */   ~
                                L52255,         /* Beg/End Comp. Date*/   ~
                                L52380          /* Beg/End Model Code*/
        return

L51765: REM Report Selection
            if sel$ <> " " then goto L51780
               goto L51810
L51780:     convert sel$ to sel%, data goto L51810

            convert sel% to sel$, pic(#)
            if sel% < 1% or sel% > 7% then goto L51810
               sel_desc$ = str(scr$(sel%+1%),7%,33%)
        return
L51810:     errormsg$ = "(Error) - Invalid Report Selection?"
            init(" ") sel$
        return

L51830: REM Report Type (S or D)
            if type$ <> " " then goto L51845
               type$ = "S"
L51845:     if type$ <> "S" and type$ <> "D" then goto L51865
               if type$ = "S" then type_desc$ = "Summary Report"         ~
                              else type_desc$ = "Detail Report "
        goto L51885
L51865:     errormsg$ = "(Error) - Invalid Report Type?"
            init(" ") type$, type_desc$
        return

L51885: REM SALESMAN CODE
            if str(comp_slmn$,1%,1%) = "A" then goto L51900
            if comp_slmn$ <> " " then goto L51930
L51900:        comp_slmn$ = "ALL"
               comp_slmn_desc$ = " "
               return
            convert comp_slmn$ to comp_slmn%, data goto L51900

            convert comp_slmn% to comp_slmn$, pic(0000)
L51930:     call "DESCRIBE" (#15, comp_slmn$, comp_slmn_desc$,0%,f1%(15))
            if f1%(15) = 0 then goto L51900
        return

L51950: REM Customer Code
            if comp_cuscode$ <> " " then goto L51965
               comp_cuscode$ = "ALL"
L51965:     if len(comp_cuscode$) < 4 and str(comp_cuscode$,1%,1%) = "A" ~
                                              then goto L51985
               gosub L50615
        return
L51985:     comp_cuscode$ = "ALL"
            cust_name$ = "All Customers"
        return

L52005: REM Complaint Code
            if len(comp_code$) = 1 and str(comp_code$,1%,1%) = "A"       ~
                                                          then goto L52035
               gosub L50895
               if f1%(4%) = 0 then goto L52035
        return
L52035:     comp_code$ = "ALL"
            comp_code_desc$ = "All Complaint Codes."
        return

L52055: REM Initiator Code
            if len(comp_init$) = 1 and str(comp_init$,1%,1%) = "A"       ~
                                                          then goto L52085
               gosub L50975
               if f1%(4%) = 0 then goto L52085
        return
L52085:     comp_init$ = "A "
            comp_init_desc$ = "All Initiator Codes."
        return

L52105: REM CSC/PSC Code
            if len(comp_svcs$) = 1 and str(comp_svcs$,1%,1%) = "A"       ~
                                                          then goto L52135
               gosub L51140
               if f1%(4%) = 0 then goto L52135
        return
L52135:     comp_svcs$ = "A "
            comp_svcs_desc$ = "All CSC/PSC Codes."
        return

L52155: REM Credit Code                                      
            if len(comp_crd$) = 1 and str(comp_crd$,1%,1%) = "A"         ~
                                                          then goto L52185
               gosub L51260
               if f1%(4%) = 0 then goto L52185
        return
L52185:     comp_crd$ = "A "
            comp_crd_desc$ = "All Credit Codes."         
        return

L52205: REM Wizard Code
            if len(comp_wizd$) = 1 and str(comp_wizd$,1%,1%) = "A"       ~
                                                          then goto L52235
               gosub L51600
               if f1%(4%) = 0 then goto L52235
        return
L52235:     comp_wizd$ = "ALL"
            comp_wizd_desc$ = "All Wizard Codes."
        return

L52255: REM Beginning and Ending Date
            if str(beg_date$,1%,1%) = "A" then goto L52270
            if beg_date$ <> " " then goto L52280
L52270:        beg_date$, end_date$ = "ALL" & "     "
               return
L52280:     if end_date$ <> " " then goto L52295
               end_date$ = beg_date$

L52295:     call "DATEOKC" (beg_date$, date%, errormsg$)
            if date% <> 0% then goto L52315
               errormsg$ = "(Error) - Invalid Beginning Date?"
               goto L52365
L52315:     call "DATEOKC" (end_date$, date%, errormsg$)
            if date% <> 0% then goto L52335
               errormsg$ = "(Error) - Invalid Ending Date?"
               goto L52365
L52335:     beg_dte$ = beg_date$
            end_dte$ = end_date$
            call "DATUFMTC" (beg_dte$)
            call "DATUFMTC" (end_dte$)
            if end_dte$ >= beg_dte$ then return
               errormsg$ = "(Error) - Invalid Date Range?"
L52365:        beg_date$, end_date$ = blankdate$
        return

L52380: REM Beginning and Ending Model Code
            if str(beg_mod$,1%,1%) = "A" then goto L52395
            if beg_mod$ <> " " then goto L52405
L52395:        beg_mod$, end_mod$ = "ALL"
               return
L52405:     if end_mod$ <> " " then goto L52420
               end_mod$ = beg_mod$

L52420:     if str(beg_mod$,1%,1%) = "A" then goto L52395
               init(" ") readkey$
               str(readkey$,1%,9%)   = "MODEL    "
               str(readkey$,10%,15%) = beg_mod$
               read #4,key = readkey$, eod goto L52465

               str(readkey$,10%,15%) = end_mod$
               read #4,key = readkey$, eod goto L52465
               if beg_mod$ > end_mod$ then goto L52465
        return
L52465:     errormsg$ = "(Error) - Invalid Beg/End Model Code?"
            init(" ") beg_mod$, end_mod$, readkey$
        return

        lookup_model
            init(" ") readkey$, comp_mod$, comp_mod_d$
            comp_mod_d$ = "MFG Product N/A ?"
            comp_mod$ = str(comp_part$,1%,3%)
            str(readkey$,1%,9%)   = "MODEL    "
            str(readkey$,10%,15%) = comp_mod$
            read #4,key = readkey$, using L52525, comp_mod_d$,            ~
                                                 eod goto L52530
L52525:        FMT POS(25), CH(30)
L52530: return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                                             /* HEADER */
L55040: %  ######## @ ########    ###################################    ~
        ~     Page: ###

L55070: %                   Internal Complaint Tracking ##############


L55100: %  +-------------------------------------------------------------~
        ~-------------+

L55130: %  Slmn    :#### ##############################

L55150: %  Model    :### ##############################

L55170: %  Cust  :###### ##############################  ################~
        ~######### ####
L55190: %  Complaint:### ##############################  Cost: ##########

L55210: %  Initiator:##  ##############################  Date: ########  ~
        ~Text: ###
L55230: %  CSC/PSC  :##  ##############################  Date: ########
L55250: %  Credit :##  ##############################    Entry: ###   Edi~
        ~t: ###
L55270: %  Window Wizard :##  ##############################  Proofer: ###           
L55290: %  Comp No. :######### S.O.:######## LINE:## PO:################ 

L55320: %  +-------------------------- #################### -------------~
        ~-------------+
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
          if page_no% > pages% then goto generate_done                      
          page_no% = page_no% + 1%
          print page
          print using L55040, date$, rpt_time$, print_title$, page_no%
          print using L55070, type_desc$
          print using L55100
          lcnt% = 3%
        return

        pt_1
          print using L55170, str(comp_cuscode$,1%,6%), cust_name$,       ~
                                                   comp_part$, comp_quan$
          lcnt% = lcnt% + 1%
          return

        pt_2
          print using L55190, comp_code$, comp_code_desc$, comp_cost$
          lcnt% = lcnt% + 1%
          return

        pt_3
          print using L55210, comp_init$, comp_init_desc$, comp_init_dte$,~
                                                         comp_init_txt1$
          lcnt% = lcnt% + 1%
          return

        pt_4
          print using L55230, comp_svcs$, comp_svcs_desc$, comp_err_dte$
          lcnt% = lcnt% + 1%
          return

        pt_5
          print using L55250, comp_crd$, comp_crd_desc$, comp_en_id$, comp_ed_id$
          lcnt% = lcnt% + 1%
          return

        pt_6
          print using L55270, comp_wizd$, comp_wizd_desc$, comp_pr_id$
          lcnt% = lcnt% + 1%
          return

        pt_7
          print using L55290, comp_number$ & int_comp$, comp_so$, comp_line$, ~
                              comp_po$
          lcnt% = lcnt% + 1%
          return

        pt_8
          print using L55130, comp_slmn$, comp_slmn_desc$
          lcnt% = lcnt% + 1%
          return

        pt_9
          print using L55150, comp_mod$, comp_mod_d$
          lcnt% = lcnt% + 1%
          return


        pt_line
          print using L55100
          lcnt% = lcnt% + 1%
        return

        print_text
          str(mask$,3%,1%)  = "!"
          str(mask$,78%,1%) = "!"
          if str(comp_init_txt1$,1%,1%) = "N" then return
             txt_1$  = "** Initiator Text **"
             textid$ = comp_init_txt$
             gosub print_text_next
          return
          
        print_text_next
            print using L55320, txt_1$
            lcnt% = lcnt% + 1%

            call "TXTPRINT" (#16, f2%(16%),      /* TXTFILE            */~
                             80%,                /* PRINTER LINE WIDTH */~
                             textid$,            /* TEXTID TO PRINT    */~
                             " ",                /* PRINT FLAGS TO INCL*/~
                             5%,                 /* CLMN TO PRINT TEXT */~
                             lcnt%,              /* LINE COUNTER       */~
                             58%,                /* LINES PER PAGE     */~
                             " ",                /* DO NOT PRINT HEADER*/~
                             mask$,              /* LINE MASK          */~
                             status%)            /* FINISHED,IN-PROCESS*/
            if status% = 0% then goto L61070
            gosub print_header
            goto print_text_next
L61070: return

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            print_title$ = sel_desc$
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCCOM", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCCOM", " ", 0%, 1%)
        return

        generate_report
            gosub get_pages                                          
            call "SHOSTAT" ("Printing Report")
            gosub select_printer      :      dataload% = 0%
            wrk_key1$ = all(hex(00))
            read #10,key 1% > wrk_key1$, using L61330, comp_number$,      ~
                                                   eod goto generate_done
            goto L61340
        generate_next
            read #10, using L61330, comp_number$, eod goto generate_done
L61330:        FMT POS(28), CH(5)
L61340:     str(comp_key$,1%,4%) = str(comp_number$,1%,4%)
            str(comp_key$,5%,1%) = " "
            read #1,key = comp_key$, eod goto generate_next
            dataload% = 1%
            gosub dataload
            dataload% = 0%
            if lcnt% > 58% then gosub print_header
            on sel% gosub gen_1, gen_2, gen_3, gen_4, gen_5, gen_6, gen_7
            if type$ = "S" then goto L61400
               status% = 0%           /* Only set when first start TXTPRINT */
               gosub print_text      
L61400:     gosub pt_line
            goto generate_next
        gen_1
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_3                                    /* INITIATOR */
            gosub pt_4                                    /* CSC/PSC   */
            gosub pt_5                                    /* Credit    */
            gosub pt_6                                    /* Wizard    */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_2
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_3                                    /* INITIATOR */
            gosub pt_4                                    /* CSC/PSC   */
            gosub pt_5                                    /* Credit    */
            gosub pt_6                                    /* Wizard    */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_3
            gosub pt_6                                    /* SOURCE    */
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_3                                    /* INITIATOR */
            gosub pt_4                                    /* CSC/PSC   */
            gosub pt_5                                    /* Credit    */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_4
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_6                                    /* SOURCE    */
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_3                                    /* INITIATOR */
            gosub pt_4                                    /* CSC/PSC   */
            gosub pt_5                                    /* Credit    */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_5
            gosub pt_3                                    /* INITIATOR */
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_4                                    /* SERVICE   */
            gosub pt_5                                    /* Credit    */
            gosub pt_6                                    /* Wizard    */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_6
            gosub pt_8                                    /* Salesman  */
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_3                                    /* INITIATOR */
            gosub pt_4                                    /* CSC/PSC   */
            gosub pt_5                                    /* Credit    */
            gosub pt_6                                    /* Wizard    */
            gosub pt_7                                    /* COMP NO.  */
        return

        gen_7
            gosub pt_9                                    /* Model Code*/
            gosub pt_2                                    /* COMPLAINT */
            gosub pt_1                                    /* CUSTOMER  */
            gosub pt_3                                    /* INITIATOR */
            gosub pt_4                                    /* CSC/PSC   */
            gosub pt_5                                    /* Credit    */
            gosub pt_6                                    /* Wizard    */
            gosub pt_7                                    /* COMP NO.  */
        return

        generate_done
            gosub close_printer
        return

        edit_init_text
            gosub'099(comp_init_txt$)
            if txt% = 0% then goto L62230
               call "TXTFUTIL" (#16, f2%(16%), "LOAD", comp_init_txt$)

L62230:     header$ = "Edit Initiator TEXT for " & comp_init_desc$ 
            if security% > 0% then                                         ~
            call "TXTINSUB" (#16, f2%(16%), "012", header$, comp_init_txt$,~
                                                         text$() )
            str(header$,,4%) = "View"
            if security% = 0% then                                         ~
            call "TXTDSPLY" (#16, f2%(16%), "012", header$, comp_init_txt$,~
                                                         text$() )
            gosub L51110
            if edit% <> 2% then fieldnr% = fieldnr% + 1%
            if txt% = 0% then return
               call "TXTFUTIL" (#16, f2%(16%), "SAV2", comp_init_txt$)
        return

        deffn'099(txt$)
            txt% = 0%
            if txt$ = hex(00000000) or txt$ = hex(ffffffff)              ~
                                              or txt$ = " " then return
            txt% = 1%
        return

        delete_complaint
           if security% <> 2% then return   /* For safety... */

            call "SHOSTAT" ("DELETING COMPLAINT ("&comp_number$&")")
            gosub pack_comp_number            
            read #1,hold,key = comp_key$, eod goto L62920
            delete #1
            call "TXTFUTIL" (#16, f2%(16%), "DELE", comp_init_txt$)
L62920: return clear all
        goto inputmode

        lookup_order
            bck_key$ = " "
            str(bck_key$,1%,9%) = comp_so$
            if comp_line$ <> " " then goto L63030
               read #7,key > bck_key$, using L63010, comp_cuscode$,       ~
                                                 bck_key$, eod goto L63160
L63010:           FMT CH(9), CH(19)
               goto L63080
L63030:     convert comp_line% to str(bck_key$,17%,3%), pic(###)
            read #7,key = bck_key$, using L63060, comp_cuscode$,          ~
                                     bck_key$, comp_part$, eod goto L63160
L63060:        FMT CH(9), CH(19), XX(3), CH(25)
               gosub L50715                         /* PART DESCRIPTION */
L63080:     if str(bck_key$,1%,8%) <> comp_so$ then goto L63170

               so_inv$  = str(bck_key$,1%, 8%)
               item_no$ = str(bck_key$,17%,3%)
               gosub lookup_subpart                /* PAR000 */
               comp_subp$ = str(bcksubpt_rec$,48%,20%)

               gosub L50615                         /* CUSTOMER NAME    */
               bck_key$ = " "
               str(bck_key$,1%,9%)  = comp_cuscode$
               str(bck_key$,10%,8%) = comp_so$
            read #8,key = bck_key$, using L63150, comp_po$, eod goto L63160
L63150:        FMT POS(26), CH(16)
L63160: return
L63170:     init(" ") comp_cuscode$, comp_po$, comp_part$, comp_part_d$
        return

        get_pages
            pages% = 300        :  init(" ") pages$
            readkey$ = all(hex(00))
            readkey$ = "CSECURIT1" & "..."   
            call "DESCRIBE" (#4, readkey$, pages$, 0%, f1%(4))
            if f1%(4%) = 0% then goto L63180

            convert str(pages$,28%,3%) to pages%,               ~
                data goto L63180

L63180:     
        return

        check_security
            comp% = 2%
            hdr$ = "******** Access Denied ********"
            msg$(1) = "Currently 'You' do not have Access To Selection!"
            msg$(2) = "           A c c e s s   D e n i e d            "
            msg$(3) = "       Press <RETURN> To Continue !!!!          "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return clear all
        goto exit_program

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
        
        open_error                                                            
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
            err% = 0%
        return

        pack_comp_number                  
            convert comp_number$ to comp_number%, data goto L63500
L63500:

            put str(comp_number$,1%,4%), using L63510, comp_number%
            str(comp_number$,5%,1%) = " "
            str(comp_key$,1%,5%)    = str(comp_number$,1%,5%)
        return

        unpack_comp_number
            get str(comp_number$,1%,4%), using L63510, comp_number%
L63510:             FMT BI(4)
            convert comp_number% to comp_number$, pic(00000000)

        return     


        lookup_subpart                            /* PAR000  */
            init(" ") bcksubpt_rec$, flds$(), info_flds$()
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
                          err1%)         /* Error Code                 */
           
            if err1% <> 0% then                                          ~
                   str(bcksubpt_rec$,48%,20%) = "00000                    "

            if err1% = 0% then return

            errormsg$ = "BCKSUBPT ERR= "&so_inv$ & " Line= " & item_no$  ~
                                      & " Flag= " & flag$


        return                                    /* PAR000 */                       
        

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
