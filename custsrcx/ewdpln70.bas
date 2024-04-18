        REM *************************************************************~
            *                                                           *~
            *  EEEEE  W   W  DDDD   PPPP   L      N   N  77777   000    *~
            *  E      W   W  D   D  P   P  L      NN  N      7  0   0   *~
            *  EEEE   W W W  D   D  PPPP   L      N N N     7   0   0   *~
            *  E      W w W  D   D  P      L      N  NN    7    0   0   *~
            *  EEEEE   W w   DDDD   P      LLLLL  N   N   7      000    *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *  Program Name      - EWDPLN70                             *~
            *  Creation Date     - 05/14/99                             *~
            *  Last Mod Date     - 06/09/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Modified By  - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - (New) Warranty Card Database. Lookup *~
            *                      and data entry.                      *~
            *                                                           *~
            *  Special Notes -  This pgm inputs and updates warranty    *~
            *                   cards. It prints report based on Bg/Ed  *~
            *                   last names, Bg/Ed Zip Codes, and Bg/Ed  *~
            *                   Purchase Date.  Warr ID is auto         *~
            *                   assigned and can not be mod.            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/14/99 ! (New) Program                            ! RHH *~
            * 08/24/99 ! (New) Program -- (Finish Program)        ! CMG *~
            * 10/07/99 ! Mod to add Glass Breakage Warr Flag      ! CMG *~
            *          !                  (EWD0001)               ! CMG *~
            * 06/09/06 ! (PAR001) Mod for change to Warranty File ! RHH *~
            *          !          (APCPLNWT)                      !     *~
            *05/04/2016!(SR74691) mod to rename warranty file     ! CMG *~
            *************************************************************

                                       /* (WARRANTY) New Tracking File  */
        dim wa_key$39, wa_key1$39,       /* Read Keys                   */~
            wa_z$10, wa_d$,              /* Report Zip Code & Date Key  */~
            wa_c$9,                      /* Report Delivery Customer    */~
            wa_cd$10,                    /* Report Window Brand Code Ky */~
            wa_so$8,                     /* EWD Sales Order Number      */~
            wa_warranty$8,               /* EWD Warranty Id             */~
            wa_zip$10,                   /* Customer Zip Code           */~
            wa_last$15,                  /* Customer Last Name          */~
            wa_first$15,                 /* Customer First Name         */~
            wa_init$1,                   /* Customer Middle Initial     */~
            wa_id$8,                 /* Warranty Tracking Id        */~
            wa_date$6,                   /* Customer Purch date mm/dd/yy*/~
            wa_phone$14,                 /* Customer Phone Number       */~
            wa_addr$30,                  /* Customer Address            */~
            wa_city$18,                  /* Customer City               */~
            wa_st$2,                     /* Customer State              */~
            wa_addr_d$30,                /* Dealer's Address            */~
            wa_city_d$18,                /* Dealer's City               */~
            wa_st_d$2,                   /* Dealer's State              */~
            wa_zip_d$10,                 /* Dealer's Zip                */~
            wa_addr_dt$30,               /* Distributors Address        */~
            wa_city_dt$18,               /* Distributors City           */~
            wa_st_dt$2,                  /* Distribitors State          */~
            wa_zip_dt$10,                /* Distributors Zip Code       */~
            wa_total$6,                  /* Total Number of Windows Pur */~
            wa_txt$(5%)30,               /* Comment Lines               */~
            wa_filler$104,               /* Filler area                 */~
            next_number$8,               /* Next Warranty ID            */~
            wa_date1$10,                 /* To format date              */~
            wa_phone_d$14,               /* To format phone             */~
            wa_id1$8,                    /* Next Warranty ID No.        */~
            type$1, type_desc$25,        /* Summary or Detail type      */~
            bg_zip$10, ed_zip$10,        /* Report Lookup Zip Code      */~
            bg_last$15, ed_last$15,      /* Report Lookup Last Name     */~
            bg_date$7, ed_date$7,        /* Report Lookup Purchase Date */~
            bg_cust$9, ed_cust$9,        /* Report Lookup Delivery Cust */~
            sc_code$2,                   /* Report Lookup Brand Code    */~
            wa_part$25,                  /* Sched MFG Part Number       */~
            wa_due$6, wa_due1$10,        /* Sched Cust Delivery Date    */~
            wa_cust$9,                   /* Sched Customer              */~
            wa_trans$1,                  /* Warranty Trans. 0=Warranty  */~
                                         /* 1=Transfer                  */~
            wa_code$2,                   /* Private Label Code          */~
            wa_glass$1                   /* Glass Breakage Code. N=No   */~
                                         /* Y=Yes Warranty              */~

        dim readkey$50,                  /* GENCODES Lookup            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10, userid$3,           /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(30%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            title$40,                    /* REPORT TITLE               */~
            runtime$8                    /* REPORT RUN TIME            */


        dim f2%(20%),                    /* = 0 if the file is open    */~
            f1%(20%),                    /* = 1 if READ was successful */~
            fs%(20%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$32, apc1$36, pname$21, tdesc$23, apc2$27
            apc$    = "Warranty Card Look-Up/Data Entry"
            apc1$   = "Warranty Transfer Look-Up/Data Entry"
            apc2$   = "Warranty Card Report Screen"
            pname$  = "EWDPLN70 - Rev: R7.00"
            tdesc$  = "(0=Original 1=Transfer)"

        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

          REM ***********************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! WARRCARD ! Warranty Card Data Base                  *~
            * #2  ! GENCODES ! System Master Table Files                *~
            * #3  ! APCPLNWT ! Warranty ID Cross-Ref to S.O. Line Item  *~
            *     !          !                                  (PAR001)*~
            * #4  ! APCPLNSC ! Planning Master Scheduling File          *~
            * #5  ! EWDWARR  ! Master Warranty Cross-Ref for History    *~
            * #6  ! EWDHIST  ! Consolidated Sales History               *~
            * #7  ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "WARRCARD",                                     ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =    27, keylen =   40,                   ~
                        alt key  1, keypos =   17, keylen =  50,         ~
                            key  2, keypos =   58, keylen =   9,         ~
                            key  3, keypos =    1, keylen =   8, dup,    ~
                            key  4, keypos =    9, keylen =   8, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =  24
                                                            /* (PAR001) */
            select #3,  "APCPLNWT",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   8,                     ~
                        alt key   1, keypos =    9, keylen =  10, dup,   ~
                            key   2, keypos =    9, keylen =  18

                                                            /* (PAR001) */
            select #4,   "APCPLNSC",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    24, keylen =   10,                   ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

/*SR76707   select #5,  "EWDWARR",                                       ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =    1, keylen =  15,                     ~
                        alt key   1, keypos =   16, keylen =  15, dup   */

/*SR76707   select #6,   "EWDHIST",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    17, keylen =   32,                   ~
                        alt key  1, keypos =   54, keylen =  15,         ~
                            key  2, keypos =   49, keylen =  20,         ~
                            key  3, keypos =   77, keylen =  26,         ~
                            key  4, keypos =    1, keylen =  25, dup    */

            select #7,   "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =     1, keylen =    9,                   ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")
                                       /* Current Year's Data       */
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 10%, rslt$(1%))

            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNWT" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
/*SR76707   filename$ = "EWDWARR"   call "EWDOPEN" (#5, filename$, err%) */
/*          if err% <> 0% then gosub open_error                          */
/*SR76707   filename$ = "EWDHIST"   call "EWDOPEN" (#6, filename$, err%) */
/*          if err% <> 0% then gosub open_error                          */
            filename$ = "CUSTOMER" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error


            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
        next_number$ = "        "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
        gosub initialize_variables

            for fieldnr% = 1% to  13%
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
                  if keyhit%  =  7% then goto inputmode_a
                  if keyhit%  = 12% then gosub delete_warranty
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = (cursor%(1%) -2%)

            if ( cursor%(1%) - 2% ) = 1 then fieldnr% = 1%

            if ( cursor%(1%) - 2% ) = 2% or                                    ~
               ( cursor%(1%) - 2% ) = 3% then fieldnr% = 2%

            if ( cursor%(1%) - 2% ) > 3 then fieldnr% = (cursor%(1%) - 3%)

            if fieldnr% = 1% then editpg1

            if fieldnr% < 1% or fieldnr% > 13% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for report screens.                  *~
            *************************************************************

        inputmode_report
        gosub initialize_variables

            for fieldnr% = 1% to 6%
L10300:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10500
L10350:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
              if keyhit% <>  4% then       L10450
L10400:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10350
                         if fieldnr% = 1% then L10300
                         goto L10400
L10450:               if keyhit% = 16% and fieldnr% = 1% then exit_program

                      if keyhit% <> 0% then       L10350
L10500:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10350
            next fieldnr%


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for report screens.        *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit% <>  0% then       editpg1
L12000:     fieldnr% = (cursor%(1%) -2%)
            if (cursor%(1%) - 2%) >  1% and                                 ~
               (cursor%(1%) - 2%) <  4% then fieldnr% = 2%

            if (cursor%(1%) - 2%) >  3% and                                 ~
               (cursor%(1%) - 2%) <  6% then fieldnr% = 3%

            if (cursor%(1%) - 2%) >  5% and                                 ~
               (cursor%(1%) - 2%) <  8% then fieldnr% = 4%

            if (cursor%(1%) - 2%) >  7% and                                 ~
               (cursor%(1%) - 2%) < 10% then fieldnr% = 5%

            if (cursor%(1%) - 2%) >  9% then fieldnr% = 6%

        if fieldnr% < 1% or fieldnr% > 6% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L12100:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12100
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12100
                  lastfieldnr% = fieldnr%
            goto L12000


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens (Transfers).  *~
            *************************************************************

        inputmode_a
        gosub initialize_transfer

            for fieldnr% = 1% to   6%
L10610:         gosub'053(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10650
L10620:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10640
L10630:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L10620
                         if fieldnr% = 1% then L10610
                         goto L10630
L10640:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10620
L10650:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10620
            next fieldnr%


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 12% then gosub delete_warranty
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg3
L11220:     fieldnr% = (cursor%(1%) -2%)

            if ( cursor%(1%) - 2% ) =  4%  then fieldnr% = 2%
            if ( cursor%(1%) - 2% ) =  5%  then fieldnr% = 3%
            if ( cursor%(1%) - 2% ) =  6%  then fieldnr% = 4%
            if ( cursor%(1%) - 2% ) =  7%  then fieldnr% = 5%
            if ( cursor%(1%) - 2% ) = 13%  then fieldnr% = 6%

            if fieldnr% < 1% or fieldnr% >  6% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L11270:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11270
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry */
                  if errormsg$ <> " " then L11270
                  lastfieldnr% = fieldnr%
            goto L11220


        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************
            print_report
                 gosub select_printer
                 gosub generate_report
            return clear all
            goto inputmode

           select_printer
              call "SHOSTAT" ("Creating Warranty Registration Report")
              runtime$ = " "
              call "TIME" (runtime$)
              select printer (134)
              pageno% = 0%
              lcntr% = 99%
              title$ = "      Warranty Registration Report      "
           return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        deffn'052(fieldnr%)
            enabled% = 1%
        return

        deffn'053(fieldnr%)
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
               read inpmessage$              /* Read Input Message */
            return

        scrn1_msg  :  data                                                 ~
         "Enter Tracking ID to Lookup or Leave Blank to Enter Data?      ",~
         "Enter Warranty ID or SO Number to Look Up or Blank If Unknown? ",~
         "Enter Customer's Last Name, First Name, and Middle Initial?    ",~
         "Enter Customer's Phone Number?                                 ",~
         "Enter Customer's Property Address?                             ",~
         "Enter Customer's City, State, Zip?                             ",~
         "Enter Dealer's Address.                                        ",~
         "Enter Dealer's City, state, Zip?                               ",~
         "Enter Distributor's Address.                                   ",~
         "Enter Distributor's City, State, Zip?                          ",~
         "Enter Total Number of Windows Purchased.                       ",~
         "Enter Date Purchased?                                          ",~
         "Enter Comments.                                                "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28300
                inpmessage$ = edtmessage$
                return

L28300
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
                read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
     "Enter Report Type (S)ummary, (D)etail, or (L)abel?           ",~
         "Enter a Valid Beginning/Ending Last Name or Blank for ALL?   ",~
         "Enter a Valid Beginning/Ending Zip Code or Blank for ALL?    ",~
         "Enter a Valid Beginning/Ending Purchase/Transfer Date or Blank for ALL?",~
         "Enter a Valid Beginning/Ending Delivery Customer or Blank for ALL?    ",~
         "Enter a Valid Window Brand Code or Blank for AA (ALL)?    "


        deffn'070(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28410
                inpmessage$ = edtmessage$
                return

L28410
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
               read inpmessage$              /* Read Input Message */
            return

        scrn3_msg  :  data                                                 ~
         "Enter Warranty Transfer?                                       ",~
         "Enter Customer's Last Name, First Name, and Middle Initial?    ",~
         "Enter Customer's Phone Number?                                 ",~
         "Enter Customer's Property Address?                             ",~
         "Enter Customer's City, State, Zip?                             ",~
         "Enter Date Purchased?                                          "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, wa_id$, wa_zip$, wa_last$,    ~
                      wa_first$, wa_init$, wa_warranty$, wa_so$, wa_date$,  ~
                      wa_phone$, wa_addr$, wa_city$, wa_st$, wa_addr_d$,    ~
                      wa_city_d$, wa_st_d$, wa_zip_d$, wa_addr_dt$,         ~
                      wa_city_dt$, wa_st_dt$, wa_zip_dt$, wa_total$,        ~
                      wa_txt$(), wa_filler$, wa_date1$, wa_phone_d$,        ~
                      wa_id1$, next_number$, type$, type_desc$,             ~
                      bg_zip$, ed_zip$, bg_last$, ed_last$, bg_date$,       ~
                      ed_date$, wa_part$, wa_due$, wa_due1$, wa_cust$,      ~
                      wa_z$, wa_code$, bg_cust$, ed_cust$, sc_code$, wa_c$, ~
                      wa_cd$, wa_glass$

        wa_trans$ = "0"
        wa_glass$ = "N"     /* (EWD0001) */
        gosub next_warranty_no
        return

        initialize_transfer
            init(" ") errormsg$, inpmessage$, wa_zip$, wa_last$,            ~
                      wa_first$, wa_init$, wa_date1$, wa_phone$, wa_addr$,  ~
                      wa_city$, wa_st$, wa_trans$, wa_date$, wa_phone_d$

            wa_trans$ = "1"
            wa_glass$ = "N"     /* (EWD0001) */
        gosub next_warranty_no
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload                         /* (WARRANTY) Tracking        */

            get #1,  using L35040, wa_so$,  /* EWD Sales Order Number   */~
                     wa_warranty$,       /* EWD Warranty Id             */~
                     wa_zip$,            /* Customer Zip Code           */~
                     wa_last$,           /* Customer Last Name          */~
                     wa_first$,          /* Customer First Name         */~
                     wa_init$,           /* Customer Middle Initial     */~
                     wa_id$,             /* Warranty Tracking Id        */~
                     wa_trans$,          /* Warranty Transfer           */~
                     wa_date$,           /* Customer Purch date mm/dd/yy*/~
                     wa_phone$,          /* Customer Phone Number       */~
                     wa_addr$,           /* Customer Address            */~
                     wa_city$,           /* Customer City               */~
                     wa_st$,             /* Customer State              */~
                     wa_addr_d$,         /* Dealer's Address            */~
                     wa_city_d$,         /* Dealer's City               */~
                     wa_st_d$,           /* Dealer's State              */~
                     wa_zip_d$,          /* Dealer's Zip                */~
                     wa_addr_dt$,        /* Distributors Address        */~
                     wa_city_dt$,        /* Distributors City           */~
                     wa_st_dt$,          /* Distribitors State          */~
                     wa_zip_dt$,         /* Distributors Zip Code       */~
                     wa_total%,          /* Total Number of Windows Pur */~
                     wa_txt$(),          /* Comment Lines               */~
                     wa_cust$,           /* Sched Customer              */~
                     wa_due$,            /* Sched Cust Delivery Date    */~
                     wa_part$,           /* Sched MFG Part Number       */~
                     wa_code$,           /* Private Label Code          */~
                     wa_glass$,          /* Glass Breakage Warranty     */~
                     wa_filler$          /* Filler area                 */

/* formats for screen images */

                     wa_phone_d$ = "(xxx) xxx-xxxx"
                     str(wa_phone_d$,2%,3%) = str(wa_phone$,1%,3%)
                     str(wa_phone_d$,7%,3%) = str(wa_phone$,4%,3%)
                     str(wa_phone_d$,11%,4%)= str(wa_phone$,7%,4%)


                convert wa_total% to wa_total$, pic(0000)

            if wa_due$ <> " " then goto L30040
                   goto L30050

L30040:     wa_due1$ = wa_due$
            call "DATFMTC" (wa_due1$)

L30050:     if wa_date$ <> " " then goto L30030
    return

L30030:     wa_date1$ = wa_date$
            call "DATFMTC" (wa_date1$)
                                             /* Purchase Date Convert   */
        return
        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

          dataput                           /* (WARRANTY) Tracking     */
/* if warranty id already exists then delete it record and rewrite record with */
/* same warranty id.  If does not exist then assign warranty */
            init(" ") wa_key$
            wa_key$ = wa_id$ & wa_trans$

            read #1,hold,key 2% = wa_key$, eod goto L31000
                delete #1
                goto L31010

L31000:   if wa_trans$ = "1" then goto L31010
            gosub assign_warranty

L31010:     put  #1,  using L35040, wa_so$, /* EWD Sales Order Number    */~
                     wa_warranty$,       /* EWD Warranty Id             */~
                     wa_zip$,            /* Customer Zip Code           */~
                     wa_last$,           /* Customer Last Name          */~
                     wa_first$,          /* Customer First Name         */~
                     wa_init$,           /* Customer Middle Initial     */~
                     wa_id$,             /* Warranty Tracking Id        */~
                     wa_trans$,          /* Warranty Transfer           */~
                     wa_date$,           /* Customer Purch date mm/dd/yy*/~
                     wa_phone$,          /* Customer Phone Number       */~
                     wa_addr$,           /* Customer Address            */~
                     wa_city$,           /* Customer City               */~
                     wa_st$,             /* Customer State              */~
                     wa_addr_d$,         /* Dealer's Address            */~
                     wa_city_d$,         /* Dealer's City               */~
                     wa_st_d$,           /* Dealer's State              */~
                     wa_zip_d$,          /* Dealer's Zip                */~
                     wa_addr_dt$,        /* Distributors Address        */~
                     wa_city_dt$,        /* Distributors City           */~
                     wa_st_dt$,          /* Distribitors State          */~
                     wa_zip_dt$,         /* Distributors Zip Code       */~
                     wa_total%,          /* Total Number of Windows Pur */~
                     wa_txt$(),          /* Comment Lines               */~
                     wa_cust$,           /* Sched Customer              */~
                     wa_due$,            /* Sched Cust Delivery Date    */~
                     wa_part$,           /* Sched MFG Part Number       */~
                     wa_code$,           /* Private Label Code          */~
                     wa_glass$,          /* Glass Breakage Warranty     */~
                     wa_filler$          /* Filler area                 */


            write  #1, eod goto L31020
        return clear all
        goto inputmode

L31020:     errormsg$ = "(Error) - Unable to Update Warranty for " & wa_id$
            gosub error_prompt
        return

        REM *************************************************************~
            *       A S S I G N       W A R R A N T Y        I D        *~
            *************************************************************

/* wa_id1 = what is written and loaded from file and next wa_id  */
/* wa_id  = wa_id to be assign and written to file.  */

        assign_warranty
            init(" ") readkey$
            str(readkey$,1%,9%)   = "WARRANTY "
            str(readkey$,10%,15%) = "WARRANTY"
            read #2,hold,key = readkey$, using L32080,                    ~
                                                 wa_id1$, eod goto L32250
L32080:        FMT POS(25), CH(8)
            wa_id% = 999999%
            convert wa_id1$ to wa_id1%, data goto L32250

            convert wa_id1% to wa_id$, pic(00000000)

            wa_id1% = wa_id1% + 1%
            convert wa_id1% to wa_id1$, pic(00000000)

            put #2, using L32080, wa_id1$
            rewrite #2
        return
L32250:     errormsg$ = "(Error) Updating Warranty Tracking ID " & wa_id$
            gosub error_prompt
        return clear all
        goto inputmode

        next_warranty_no
            init(" ") readkey$
            str(readkey$,1%,9%)   = "WARRANTY "
            str(readkey$,10%,15%) = "WARRANTY"
            wa_id1$ = " "
            read #2,key = readkey$, using L32080, wa_id1$,               ~
                                                   eod goto L32350
        next_number$ = wa_id1$

L32350: return



        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                       /* (WARRANTY) New Tracking File */
L35040:     FMT      CH(8),              /* EWD Sales Order Number     */~
                     CH(8),              /* EWD Warranty Id            */~
                     CH(10),             /* Customer Zip Code          */~
                     CH(15),             /* Customer Last Name         */~
                     CH(15),             /* Customer First Name        */~
                     CH(1),              /* Customer Middle Initial    */~
                     CH(8),              /* EWD Warranty Tracking Id   */~
                     CH(1),              /* Warranty Transfer          */~
                     CH(7),              /* Customer Purch date mm/yyyy*/~
                     CH(14),             /* Customer Phone Number      */~
                     CH(30),             /* Customer Address           */~
                     CH(18),             /* Customer City              */~
                     CH(2),              /* Customer State             */~
                     CH(30),             /* Dealer's Address           */~
                     CH(18),             /* Dealer's City              */~
                     CH(2),              /* Dealer's State             */~
                     CH(10),             /* Dealer's Zip               */~
                     CH(30),             /* Distributors Address       */~
                     CH(18),             /* Distributors City          */~
                     CH(2),              /* Distribitors State         */~
                     CH(10),             /* Distributors Zip Code      */~
                     BI(2),              /* Total Number of Windows Pur*/~
                     5*CH(30),           /* Comment Lines              */~
                     CH(9),              /* Sched Customer             */~
                     CH(6),              /* Sched Cust Delivery Date   */~
                     CH(25),             /* Sched MFG Part Number      */~
                     CH(2),              /* Private Label Code         */~
                     CH(1),              /* Glass Breakage Warranty    */~
                     CH(60)              /* Filler area                */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                        ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40160,      /* Warranty Tracking ID       */~
                                L40160,      /* EWD Warranty Id            */~
                                             /* EWD Sales Order Number     */~
                                L40160,      /* Customer Last Name         */~
                                             /* Customer First Name        */~
                                             /* Customer Middle Initial    */~
                                L40160,      /* Customer Phone Number      */~
                                L40160,      /* Customer Address           */~
                                L40160,      /* Customer City              */~
                                             /* Customer State             */~
                                             /* Customer Zip Code          */~
                                L40160,      /* Dealer's Address           */~
                                L40160,      /* Dealer's City              */~
                                             /* Dealer's State             */~
                                             /* Dealer's Zip               */~
                                L40160,      /* Distributors Address       */~
                                L40160,      /* Distributors City          */~
                                             /* Distribitors State         */~
                                             /* Distributors Zip Code      */~
                                L40160,      /* Total Number of Windows Pur*/~
                                L40160,      /* Customer Purch date mm/yyyy*/~
                                L40160       /* Comment Line(5)            */

                 goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,25), fac(hex(a4)), apc$                   , ch(32),~
               at (02,02), fac(hex(94)), errormsg$              , ch(58),~
                                                                         ~
               at (02,60), "Next ID:",                                   ~
               at (02,69), fac(hex(84)), next_number$           , ch(08),~
                                                                         ~
               at (03,02), "Warranty Tracking ID:",                      ~
               at (03,25), fac(lfac$(1%)), wa_id$               , ch(08),~
                                     ~
               at (03,40), "Transfer Code:",                             ~
               at (03,60), fac(lfac$(1%)), wa_trans$            , ch(01),~
               at (04,55), fac(hex(8c)), tdesc$                 , ch(23),~
                                                                         ~
               at (04,02), "Warranty ID:",                               ~
               at (04,25), fac(lfac$(2%)), wa_warranty$         , ch(08),~
                                                                         ~
               at (05,02), "Sales Order Number:",                        ~
               at (05,25), fac(lfac$(2%)), wa_so$               , ch(08),~
                                                                         ~
               at (06,02), "Cust. Last, First, I:",                      ~
               at (06,25), fac(lfac$(3%)), wa_last$             , ch(15),~
               at (06,42), fac(lfac$(3%)), wa_first$            , ch(15),~
               at (06,59), fac(lfac$(3%)), wa_init$             , ch(01),~
                                                                         ~
               at (07,02), "Phone Number:",                              ~
               at (07,25), fac(lfac$(4%)), wa_phone_d$          , ch(14),~
                                                                         ~
               at (08,02), "Property Address:",                          ~
               at (08,25), fac(lfac$(5%)), wa_addr$             , ch(30),~
                                                                         ~
               at (09,02), "City, State, Zip:",                          ~
               at (09,25), fac(lfac$(6%)), wa_city$             , ch(18),~
               at (09,44), fac(lfac$(6%)), wa_st$               , ch(02),~
               at (09,48), fac(lfac$(6%)), wa_zip$              , ch(10),~
                                                                         ~
               at (10,02), "Dealers Address:",                           ~
               at (10,25), fac(lfac$(7%)), wa_addr_d$           , ch(30),~
                                                                         ~
               at (11,02), "City, State, Zip:",                          ~
               at (11,25), fac(lfac$(8%)), wa_city_d$           , ch(18),~
               at (11,44), fac(lfac$(8%)), wa_st_d$             , ch(02),~
               at (11,48), fac(lfac$(8%)), wa_zip_d$            , ch(10),~
                                                                         ~
               at (12,02), "Distributor's Address:",                     ~
               at (12,25), fac(lfac$(9%)), wa_addr_dt$          , ch(30),~
                                                                         ~
               at (13,02), "City, State, Zip:",                          ~
               at (13,25), fac(lfac$(10%)), wa_city_dt$         , ch(18),~
               at (13,44), fac(lfac$(10%)), wa_st_dt$           , ch(02),~
               at (13,48), fac(lfac$(10%)), wa_zip_dt$          , ch(10),~
                                                                         ~
               at (14,02), "Windows Purchased:",                         ~
               at (14,25), fac(lfac$(11%)), wa_total$           , ch(04),~
                                                                         ~
               at (14,40), "Breakage Warranty:",                         ~
               at (14,69), fac(lfac$(11%)), wa_glass$           , ch(01),~
                                                                         ~
               at (15,02), "Date Purchased:",                            ~
               at (15,25), fac(lfac$(12%)), wa_date1$           , ch(10),~
                                                                         ~
               at (18,40), "Sched Delivery Customer:",                   ~
               at (18,69), fac(hex(84))  , wa_cust$             , ch(09),~
                                                                         ~
               at (19,40), "Delivery Due Date:",                         ~
               at (19,69), fac(hex(84))  , wa_due1$             , ch(10),~
                                                                         ~
               at (20,40), "Part Number:",                               ~
               at (20,54), fac(hex(84))  , wa_part$             , ch(25),~
                                                                         ~
               at (16,02), "Comments:",                                  ~
               at (16,12), fac(lfac$(13%)), wa_txt$(1%)         , ch(30),~
               at (17,05), fac(lfac$(13%)), wa_txt$(2%)         , ch(30),~
               at (18,05), fac(lfac$(13%)), wa_txt$(3%)         , ch(30),~
               at (19,05), fac(lfac$(13%)), wa_txt$(4%)         , ch(30),~
               at (20,05), fac(lfac$(13%)), wa_txt$(5%)         , ch(30),~
                                                                     ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


           if keyhit% <> 15% then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1

        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over     (4)Previous Field     " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40600
                str(pf$(1%),19%,19%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40600:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                    (12)Delete Warranty"
            pf$(2) = "                                        " &        ~
                     "                    (15)Print Screen   "
            pf$(3) = "(7)Transfer Update                      " &        ~
                     "                    (16)Update         "
            pfkeys$ = hex(01ffffffffff07ffffffff0cffff0f1000)
            return

L40700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen for transfers.             *~
            *************************************************************

         deffn'103(fieldnr%, edit%)
              gosub'070(1%, fieldnr%)
              gosub set_pf3
              if fieldnr% > 0% then init(hex(8c)) lfac$()                        ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40170,      /* Warranty Tracking ID/Trans */~
                                L40170,          /* Customer Last Name         */~
                                         /* Customer First Name        */~
                                         /* Customer Middle Initial    */~
                        L40170,          /* Customer Phone Number      */~
                L40170,          /* Customer Address           */~
                        L40170,          /* Customer City              */~
                                     /* Customer State             */~
                                 /* Customer Zip Code          */~
                    L40170           /* Customer Purch date mm/yyyy*/

                 goto L40175

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40175:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,25), fac(hex(a4)), apc1$                  , ch(36),~
               at (02,02), fac(hex(94)), errormsg$              , ch(58),~
                                                                         ~
               at (02,60), "Next ID:",                                   ~
               at (02,69), fac(hex(84)), next_number$           , ch(08),~
                                                                         ~
               at (03,02), "Warranty Tracking ID:",                      ~
               at (03,25), fac(hex(84)), wa_id$                 , ch(08),~
                                                                         ~
               at (03,40), "Transfer Code:",                             ~
               at (03,60), fac(lfac$(1%)), wa_trans$            , ch(01),~
               at (04,55), fac(hex(84)), tdesc$                 , ch(23),~
                                                                         ~
               at (04,02), "Warranty ID:",                               ~
               at (04,25), fac(hex(84)), wa_warranty$           , ch(08),~
                                                                         ~
               at (05,02), "Sales Order Number:",                        ~
               at (05,25), fac(hex(84)), wa_so$                 , ch(08),~
                                                                         ~
               at (06,02), "Cust. Last, First, I:",                      ~
               at (06,25), fac(lfac$(2%)), wa_last$             , ch(15),~
               at (06,42), fac(lfac$(2%)), wa_first$            , ch(15),~
               at (06,59), fac(lfac$(2%)), wa_init$             , ch(01),~
                                                                         ~
               at (07,02), "Phone Number:",                              ~
               at (07,25), fac(lfac$(3%)), wa_phone_d$          , ch(14),~
                                                                         ~
               at (08,02), "Property Address:",                          ~
               at (08,25), fac(lfac$(4%)), wa_addr$             , ch(30),~
                                                                         ~
               at (09,02), "City, State, Zip:",                          ~
               at (09,25), fac(lfac$(5%)), wa_city$             , ch(18),~
               at (09,44), fac(lfac$(5%)), wa_st$               , ch(02),~
               at (09,48), fac(lfac$(5%)), wa_zip$              , ch(10),~
                                                                         ~
               at (10,02), "Dealers Address:",                           ~
               at (10,25), fac(hex(84)), wa_addr_d$             , ch(30),~
                                                                         ~
               at (11,02), "City, State, Zip:",                          ~
               at (11,25), fac(hex(84)), wa_city_d$             , ch(18),~
               at (11,44), fac(hex(84)), wa_st_d$               , ch(02),~
               at (11,48), fac(hex(84)), wa_zip_d$              , ch(10),~
                                                                         ~
               at (12,02), "Distributor's Address:",                     ~
               at (12,25), fac(hex(84)), wa_addr_dt$            , ch(30),~
                                                                         ~
               at (13,02), "City, State, Zip:",                          ~
               at (13,25), fac(hex(84)), wa_city_dt$            , ch(18),~
               at (13,44), fac(hex(84)), wa_st_dt$              , ch(02),~
               at (13,48), fac(hex(84)), wa_zip_dt$             , ch(10),~
                                                                         ~
               at (14,02), "Windows Purchased:",                         ~
               at (14,25), fac(hex(84)), wa_total$              , ch(04),~
                                                                         ~
               at (14,40), "Breakage Warranty:",                         ~
               at (14,69), fac(hex(84)), wa_glass$              , ch(01),~
                                                                         ~
               at (15,02), "Date Transfered:",                           ~
               at (15,25), fac(lfac$(6%)), wa_date1$            , ch(10),~
                                                                         ~
               at (18,40), "Sched Delivery Customer:",                   ~
               at (18,69), fac(hex(84))  , wa_cust$             , ch(09),~
                                                                         ~
               at (19,40), "Delivery Due Date:",                         ~
               at (19,69), fac(hex(84))  , wa_due1$             , ch(10),~
                                                                         ~
               at (20,40), "Part Number:",                               ~
               at (20,54), fac(hex(84))  , wa_part$             , ch(25),~
                                                                         ~
               at (16,02), "Comments:",                                  ~
               at (16,12), fac(hex(84)), wa_txt$(1%)            , ch(30),~
               at (17,05), fac(hex(84)), wa_txt$(2%)            , ch(30),~
               at (18,05), fac(hex(84)), wa_txt$(3%)            , ch(30),~
               at (19,05), fac(hex(84)), wa_txt$(4%)            , ch(30),~
               at (20,05), fac(hex(84)), wa_txt$(5%)            , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15% then goto L40425
                  call "PRNTSCRN"
                  goto L40175

L40425:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3

        if edit% = 2% then L40615     /*  Input Mode             */
            pf$(1) = "(1)Start Over     (4)Previous Field     " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40575
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40575:     if fieldnr% > 1% then L40605
                str(pf$(1%),19%,19%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40605:     return

L40615: if fieldnr% > 0% then L40705  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                    (12)Delete Warranty"
            pf$(2) = "                                        " &        ~
                     "                    (15)Print Screen   "
            pf$(3) = "                                        " &        ~
                     "                    (16)Update         "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)
            return

L40705:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               R e p o r t   S c r e e n                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                   ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41600,          /* Report Selection Type */~
                                L41600,          /* Beg/End Last Name     */~
                                L41600,          /* Beg/End Zip Code      */~
                                L41600,          /* Beg/End Purchase Date */~
                                L41600,          /* Beg/End Delivery Cust */~
                                L41600           /* Window Brand Code     */

              goto L41630

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41600:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41630:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,25), fac(hex(a4)), apc2$                  , ch(27),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Report Type          :",                     ~
               at (03,25), fac(lfac$(1%)), type$                , ch(01),~
               at (03,30), fac(hex(84))  , type_desc$           , ch(25),~
                                                                         ~
               at (04,02), "Beginning Last Name  :",                     ~
               at (04,25), fac(lfac$(2%)), bg_last$             , ch(15),~
                                                                         ~
               at (05,02), "Ending Last Name     :",                     ~
               at (05,25), fac(lfac$(2%)), ed_last$             , ch(15),~
                                                                         ~
               at (06,02), "Beginning Zip Code   :",                     ~
               at (06,25), fac(lfac$(3%)), bg_zip$              , ch(10),~
                                                                         ~
               at (07,02), "Ending Zip Code      :",                     ~
               at (07,25), fac(lfac$(3%)), ed_zip$              , ch(10),~
                                                                         ~
               at (08,02), "Beginning Date       :",                     ~
               at (08,25), fac(lfac$(4%)), bg_date$             , ch(07),~
                                                                         ~
               at (09,02), "Ending Date          :",                     ~
               at (09,25), fac(lfac$(4%)), ed_date$             , ch(07),~
                                                                         ~
               at (10,02), "Beginning Del. Cust. :",                     ~
               at (10,25), fac(lfac$(5%)), bg_cust$             , ch(09),~
                                                                         ~
               at (11,02), "Ending Del. Cust.    :",                     ~
               at (11,25), fac(lfac$(5%)), ed_cust$             , ch(09),~
                                                                         ~
               at (12,02), "Window Brand Code    :",                     ~
               at (12,25), fac(lfac$(6%)), sc_code$             , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


              if keyhit% <> 15 then goto L42210
                  call "PRNTSCRN"
                  goto L41630

L42210:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
         if edit% = 2% then L42410     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42370
               str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L42370:     if fieldnr% > 1% then L42390
               str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42390:     return

L42410: if fieldnr% > 0% then L42510  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return

L42510:
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            init(" ") errormsg$
            on fieldnr% gosub   L50000,        /* Warranty Tracking ID       */~
                                L50100,        /* EWD Warranty Id            */~
                                               /* EWD Sales Order Number     */~
                                L50200,        /* Customer Last Name         */~
                                               /* Customer First Name        */~
                                               /* Customer Middle Initial    */~
                                L50300,        /* Customer Phone Number      */~
                                L50400,        /* Customer Address           */~
                                L50500,        /* Customer City              */~
                                               /* Customer State             */~
                                               /* Customer Zip Code          */~
                                L50600,        /* Dealer's Address           */~
                                L50700,        /* Dealer's City              */~
                                               /* Dealer's State             */~
                                               /* Dealer's Zip               */~
                                L50800,        /* Distributors Address       */~
                                L50900,        /* Distributors City          */~
                                               /* Distribitors State         */~
                                               /* Distributors Zip Code      */~
                                L51000,        /* Total Number of Windows Pur*/~
                                L51100,        /* Customer Purch date mm/yyyy*/~
                                L51200         /* Comment Line(5)            */
    return

L50000: REM Test for Warranty Tracking ID       WA_ID$
          if wa_trans$ <> "1" and wa_trans$ <> "0" then goto L50030
          if wa_id$ = " " then return
          if wa_trans$ <> "1" then goto L50010

          convert wa_id$ to wa_id%, data goto L50020

          convert wa_id% to wa_id$, pic(00000000)

          wa_key$ = wa_id$ & "1"
          read #1,key 2% = wa_key$, eod goto L50040
          gosub dataload
/* not tested for edit mode b/c can not get to or modify this field anyway          */
REM          fieldnr% = 99%
          goto editpg3
L50040: return

L50010:   convert wa_id$ to wa_id%, data goto L50020

          convert wa_id% to wa_id$, pic(00000000)


          wa_key$ = wa_id$ & "1"
          read #1,key 2% = wa_key$, eod goto L50050
           gosub dataload
/* not tested for edit mode b/c can not get to or modify this field anyway          */
REM        fieldnr% = 99%
           goto editpg3
    return

L50050:   wa_key$ = wa_id$ & "0"
          read #1,key 2% = wa_key$, eod goto L50020
           gosub dataload
          goto editpg1

L50020:   errormsg$ = "(Error) - Warranty ID not on file?"
          gosub error_prompt
          init(" ") wa_id$
    return

L50030:   errormsg$ = "(Error) - Tranfer Code Invalid/Not on file?"
          gosub error_prompt
          init(" ") wa_id$
          wa_trans$ = "0"
    return

L50100: REM Test for Warranty ID            WA_WARRANTY$
          if wa_warranty$ <> " " then goto L50110
          wa_warranty$ = "UNKNOWN "
          goto L50140

L50110:   init(" ") wa_key$
          wa_key$ = wa_warranty$

          read #1,key 4% = wa_key$, using L50190, wa_key$, wa_id$,             ~
                                    eod goto L50120
L50190:   FMT POS(9), CH(8), POS(58), CH(8)

REM   gosub dataload
          goto L50000
/* force into edit mode only if in input mode (edit = 1)                    */
          if edit% = 1% then fieldnr% = 99%
    return
L50120:   if len(wa_warranty$) <> 8 then goto L50130
          gosub lookup_warranty
        return
L50130:   errormsg$ = "(Error) - Invalid Warranty Number?"
          gosub error_prompt
          init(" ") wa_warranty$
    return

L50140: REM Test for Sales Order Number         WA_SO$
          if wa_so$ <> " " then goto L50150
          wa_so$ = "UNKNOWN "
      return
L50150:   init(" ") wa_key$
          wa_key$ = wa_so$

          read #1,key 3% = wa_key$, using L50195, wa_key$, wa_id$,             ~
                                    eod goto L50160
L50195:   FMT CH(8), POS(58), CH(8)

REM   gosub dataload
          goto L50000
          if edit% = 1% then fieldnr% = 99%
    return
L50160:   if len(wa_so$) <> 8 then goto L50170
          gosub lookup_so
        return
L50170:   errormsg$ = "(Error) - Invalid Sales Order Number?"
          gosub error_prompt
          init(" ") wa_so$
    return

L50200: REM Test for Last Name                      WA_LAST$
          if len(wa_last$) < 3 then goto L50290

        REM Test for First Name                     WA_FIRST$
          if len(wa_first$) < 3 then goto L50290

        REM Test for Middle Initial                 WA_INIT$
          init(" ") wa_key$, wa_key1$
          str(wa_key$,1%,15%)  = wa_last$
          str(wa_key$,16%,15%) = wa_first$
          str(wa_key$,31%,1%)  = wa_init$
          read #1,key > wa_key$, using L50265, wa_key1$,                 ~
                                                 eod goto L50270
L50265:        FMT POS(27),CH(39)

          if str(wa_key$,1%,15%)  <> str(wa_key1$,1%,15%) then goto L50270
          if str(wa_key$,16%,15%) <> str(wa_key1$,16%,15%) then goto L50270
          if str(wa_key$,31%,1%)  <> str(wa_key1$,31%,1%) then goto L50270
          gosub dataload
/* force into edit mode only if in input mode (edit = 1)                    */
          if edit% = 1% then fieldnr% = 99%
L50270: return

L50290:   errormsg$ = "(Error) Invalid Name entry?"
          gosub error_prompt
          init(" ") wa_last$, wa_first$, wa_init$
        return

L50300: REM Test for Home Phone Number              WA_PHONE$, WA_PHONE_D$
           if str(wa_phone_d$,1%,1%) = "(" then goto L50330
              wa_phone$ = wa_phone_d$ & "       "
             if len(wa_phone_d$) > 7 then goto L50320         /* Area Code */
                 wa_phone_d$ = "(xxx) "
                 str(wa_phone_d$,7%,3%) = str(wa_phone$,1%,3%)
                 str(wa_phone_d$,11%,4%) = str(wa_phone$,4%,4%)
             return
L50320:      if len(wa_phone_d$) <> 10 then L50340
                wa_phone_d$ = "(xxx) xxx-xxxx"
                str(wa_phone_d$,2%,3%) = str(wa_phone$,1%,3%)
                str(wa_phone_d$,7%,3%) = str(wa_phone$,4%,3%)
                str(wa_phone_d$,11%,4%) = str(wa_phone$,7%,4%)
             return
L50330:
             if str(wa_phone_d$,5%,2%) <> ") " or                             ~
                str(wa_phone_d$,10%,1%) <> "-" then goto L50340
             if len(wa_phone_d$) <> 14 then L50340
             str(wa_phone$,1%,3%) = str(wa_phone_d$,2%,3%)
             str(wa_phone$,4%,3%) = str(wa_phone_d$,7%,3%)
             str(wa_phone$,7%,4%) = str(wa_phone_d$,11%,4%)
        return
L50340:     errormsg$ = "(Error) Invalid Phone Number Entry?"
            gosub error_prompt
            init(" ") wa_phone$, wa_phone_d$
        return

L50400: REM Test for Property Address               WA_ADDR$
            if wa_addr$ <> " " then return
            goto L50410
L50410:   errormsg$ = "(Error) - Must Enter Valid Property Address?"
          gosub error_prompt
          init(" ") wa_addr$
        return

L50500: REM Test for City               WA_CITY$
        if wa_city$ <> " " then goto L50510
         goto L50520
         wa_city% = 0
L50510:  convert wa_city$ to wa_city%, data goto L50530

          goto L50520
L50520:   errormsg$ = "(Error) - Invalid City?"
          gosub error_prompt
          init(" ") wa_city$
        return

L50530: REM Test for State              WA_ST$
        if wa_st$ <> " " then goto L50525
         goto L50540
         wa_st% = 0
L50525:  convert wa_st$ to wa_st%, data goto L50550

         goto L50540
L50540:  errormsg$ = "(Error) - Invalid State?"
         gosub error_prompt
         init(" ") wa_st$
        return

L50550: REM Test for Zip Code               WA_ZIP$
        if wa_zip$ <> " " then goto L50580
         goto L50590
L50580:  x% = len(wa_zip$)
         if x% < 5% then goto L50590
        return
L50590:  errormsg$ = "(Error) - Invalid Zip Code Must be > (5)?"
         gosub error_prompt
         init(" ") wa_zip$
        return

L50600: REM Test for Dealer's Address               WA_ADDR_D$
/* if in edit mode then also need to check city, state, and zip.  Must enter city, */
/* state, and zip first if record already exists.  */
         if edit% = 2% then goto L50700
        return

L50700: REM Test for Dealer's City          WA_CITY_D$
        wa_city_d% = 0
          if wa_addr_d$ = " " then return
          if wa_city_d$ <> " " then goto L50710
          goto L50730
L50710:   convert wa_city_d$ to wa_city_d%, data goto L50720

          goto L50730

L50730:   errormsg$ = "(Error) - Invalid City?"
          gosub error_prompt
          init(" ") wa_city_d$
        return


L50720: REM Test for Dealer's State         WA_ST_D$
        wa_st_d% = 0
         if wa_st_d$ <> " " then goto L50725
            goto L50740

L50725:  convert wa_st_d$ to wa_st_d%, data goto L50750

              goto L50740
L50740:  errormsg$ = "(Error) - Invalid State?"
         gosub error_prompt
         init(" ") wa_st_d$
        return

L50750: REM Test for Dealer's Zip Code          WA_ZIP_D$
         if wa_zip_d$ <> " " then goto L50780
            goto L50790

L50780:   x% = len(wa_zip_d$)
          if x% < 5% then goto L50790
        return
L50790:    errormsg$ = "(Error) - Invalid Zip Code Must be > (5)?"
           gosub error_prompt
           init(" ") wa_zip_d$
        return

L50800: REM Test for Distributor's Address              WA_ADDR_DT$
        if edit% = 2% then goto L50900
        return

L50900: REM Test for Distributor's City         WA_CITY_DT$
        wa_city_dt% = 0
           if wa_addr_dt$  = " " then return
           if wa_city_dt$ <> " " then goto L50910
             goto L50930
L50910:   convert wa_city_dt$ to wa_city_dt%, data goto L50920

           goto L50930
L50930:   errormsg$ = "(Error) - Invalid City?"
          gosub error_prompt
          init(" ") wa_city_dt$
        return

L50920: REM Test for Distributor's State        WA_ST_DT$
        wa_st_dt% = 0
         if wa_st_dt$ <> " " then goto L50925
             goto L50940

L50925:  convert wa_st_dt$ to wa_st_dt%, data goto L50950

            goto L50940
L50940:   errormsg$ = "(Error) - Invalid State?"
          gosub error_prompt
          init(" ") wa_st_dt$
        return


L50950: REM Test for Distributor's Zip Code     WA_ZIP_DT$
           if wa_zip_dt$ <> " " then goto L50980
              goto L50990

L50980:    x% = len(wa_zip_dt$)
           if x% < 5% then goto L50990
        return
L50990:    errormsg$ = "(Error) - Invalid Zip Code Must be > (5)?"
           gosub error_prompt
           init(" ") wa_zip_dt$
        return

/* (EWD0001) */

L51000: REM Test for the Num of Window's Pur and Breakage Warr  WA_TOTAL$, WA_GLASS$
           wa_total% = 0%
           convert wa_total$ to wa_total%, data goto L51010

           if wa_total% < 1% then goto L51010

           convert wa_total% to wa_total$, pic(0000)

           if wa_glass$ <> "N" and wa_glass$ <> "Y" then goto L51020

    return
L51010:    errormsg$ = "(Error) - Invalid Number of Windows Purchased?"
           gosub error_prompt
           init(" ") wa_total$
        return

L51020:    errormsg$ = "(Error) - Invalid Value for Glass Breakage Warranty?"
           gosub error_prompt
           init(" ") wa_total$
           wa_glass$ = "N"
        return
/* (EWD0001) */

L51100: REM Test for Customer Purchase Date             WA_DATE$, WA_DATE1$
        if wa_date1$ <> " " then goto L51110
               goto L51120

L51110:     x$ = wa_date1$
            date% = 0%
            call "DATEOKC" (x$, date%, errormsg$)
            if date% = 0% then return
            wa_date1$ = x$
            call "DATUFMTC" (x$)
            wa_date$ = str(x$,1%,6%)
        return
L51120:     errormsg$ = "(Error) - Invalid Customer Purchase Date?"
            gosub error_prompt
        init(" ") wa_date$, wa_date1$
    return

L51200: REM Test for Comment Lines          WA_TXT$
    return

        REM *************************************************************~
            *                 R E P O R T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on REPORT 1.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51500,         /* Report Selection Type */~
                              L51400,         /* Beg/End Last Name     */~
                              L51300,         /* Beg/End Zip Code      */~
                              L51600,         /* Beg/End Pur/Tran Date */~
                              L51700,         /* Beg/End Delivery Cust */~
                              L51800          /* Window Brand Type     */~

            return

L51500: REM Report Type (S or D)                      TYPE$, TYPE_DESC$
            if type$ <> " " then goto L51510
               type$ = "S"

L51510:     if type$ = "S" then goto L51515
            if type$ = "D" then goto L51515
            if type$ = "L" then goto L51515                            ~
                           else goto L51520

L51515:     if type$ = "S" then type_desc$ = "Summary  Report"
            if type$ = "D" then type_desc$ = "Detail  Report "
            if type$ = "L" then type_desc$ = "Label  Report"
        return
L51520:     errormsg$ = "(Error) - Invalid Report Type?"
            gosub error_prompt
            init(" ") type$, type_desc$
        return

L51400: REM Beginning/Ending Last Name              BG_LAST$,ED_LAST$
        bg_last%, ed_last% = 0

         if bg_last$ <> " " then goto L51420
L51410:       bg_last$, ed_last$ = "ALL"
        return
                                                 /* 1st Check Beginning  */
L51420:   if str(bg_last$,1%,3%) = "ALL" and                                  ~
             len(bg_last$)       = 3% then goto L51410

          if len(bg_last$) < 3 then goto L51435

          convert bg_last$ to bg_last%, data goto L51450

L51435:   errormsg$ = "(Error) - Invalid Last Name?"
          gosub error_prompt
          init(" ") bg_last$, ed_last$
        return
                                               /* Check Ending Last Name */
L51450:   if ed_last$ <> " " then goto L51460
             ed_last$ = bg_last$
        return

L51460:   if len(bg_last$) < 3 then goto L51435

          convert ed_last$ to ed_last%, data goto L51470

          goto L51435

L51470:   if bg_last$ > ed_last$ then goto L51435
        return


L51300: REM Beginning/Ending ZIP CODE                BG_ZIP$,ED_ZIP$

          if bg_zip$ <> " " then goto L51320
L51310:      bg_zip$, ed_zip$ = "ALL"
          return
                                                   /* 1st Check Beginning  */
L51320:   if str(bg_zip$,1%,3%) = "ALL" then goto L51310

              goto L51350

L51330:   errormsg$ = "(Error) - Invalid Zip Code?"
          gosub error_prompt
          init(" ") bg_zip$, ed_zip$
        return
                                              /*  Check Ending  */
L51350:   if ed_zip$ <> " " then goto L51360
             ed_zip$   = bg_zip$
        return

L51360:    if bg_zip$ > ed_zip$ then goto L51330
        return


L51600: REM Beginning/Ending Purchase/Transfer Date           BG_DATE$,ED_DATE$
         bg_date1%, ed_date1% = 0%
         if bg_date$ <> " " then goto L51620
L51610:       bg_date$, ed_date$ = "ALL"
           return
                                                   /* 1st Check Beginning  */
L51620:    if str(bg_date$,1%,3%) = "ALL" then goto L51610
              if str(bg_date$,3%,1%) <> "/" then goto L51640
                 str(bg_date1$,1%,4%) = str(bg_date$,4%,4%)   /* With Format */
                 str(bg_date1$,5%,2%) = str(bg_date$,1%,2%)
              goto L51650

L51640:          str(bg_date1$,1%,4%) = str(bg_date$,3%,4%)   /* Without Format */
                 str(bg_date1$,5%,2%) = str(bg_date$,1%,2%)
               goto L51650

L51630:    errormsg$ = "(Error) - Invalid Purchase Date?"
           gosub error_prompt
           init(" ") bg_date$, ed_date$
        return

L51650:  if ed_date$ <> " " then goto L51660
            ed_date$   = bg_date$

L51660:  if str(ed_date$,3%,1%) <> "/" then goto L51670
           str(ed_date1$,1%,4%) = str(ed_date$,4%,4%)   /* With Format */
           str(ed_date1$,5%,2%) = str(ed_date$,1%,2%)
           goto L51680

L51670:    str(ed_date1$,1%,4%) = str(ed_date$,3%,4%)   /* Without Format */
           str(ed_date1$,5%,2%) = str(ed_date$,1%,2%)

L51680:    convert bg_date1$ to bg_date1%, data goto L51630

           convert ed_date1$ to ed_date1%, data goto L51630

           if bg_date1$ > ed_date1$ then goto L51630
        return

L51700: REM Beginning/Ending Delivery Customer       BG_CUST$,ED_CUST$
        if bg_cust$ <> " " then goto L51710
L51720:    bg_cust$, ed_cust$  = "ALL"
        return

L51710: if bg_cust$ = "ALL" then goto L51720
        if ed_cust$ <> " " then goto L51730
           ed_cust$ = bg_cust$
        return

L51730: if len(bg_cust$) < 5% then goto L51790

        if len(ed_cust$) < 5% then goto L51790
        return

L51790:  errormsg$ = "(Error) - Invalid Delivery Customer?"
         gosub error_prompt
         init(" ") bg_cust$, ed_cust$
        return

L51800: REM Window Brand Code                        SC_CODE$
        if sc_code$ <> " " then goto L51810
           sc_code$ = "AA"
        return

L51810: if sc_code$ = "AA" then return

        convert sc_code$ to sc_code%, data goto L51890

        convert sc_code% to sc_code$, pic(00)

        if sc_code$ < "01" or sc_code$ > "09" then goto L51890
        return

L51890:  errormsg$ = "(Error) - Invalid Delivery Customer?"
         gosub error_prompt
         init(" ") bg_cust$, ed_cust$
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2. (Transfers)          *~
            *************************************************************
        deffn'153(fieldnr%)
            init(" ") errormsg$
            on fieldnr% gosub   L50000,          /* Warranty Tracking ID       */~
                                L50200,          /* Customer Last Name         */~
                             /* Customer First Name        */~
                                                 /* Customer Middle Initial    */~
                        L50300,          /* Customer Phone Number      */~
                L50400,          /* Customer Address           */~
                        L50500,          /* Customer City              */~
                                     /* Customer State             */~
                                 /* Customer Zip Code          */~
                L51100          /* Customer Purch date mm/yyyy*/
    return


        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                                         /* Header Formats */
L55000: %+---------------------------------------------------------------~
        ~--------------+
L55040: %! ######## @ ########      Warranty Master File Report          ~
        ~    Page: ### !
L55070: %!                       Warranty  Tracking   ###############    ~
        ~              !

                                             /* Detail and Summary Formats */
L55280: %! Warranty Tracking ID: ########              Transfer Code:  # ~
        ~              !
L55300: %! Purchase/Transfer Date: ##########                            ~
        ~              !
L55320: %! Customer Name (Last, First, I): ############### ##############~
        ~# #           !
L55330: %! Phone Number: ##############                                  ~
        ~              !
L55340: %! Customer Property Address: ##############################     ~
        ~              !
L55360: %! City, State, Zip: ##################  ##   ##########         ~
        ~              !
L55380: %! Dealer Address: ##############################                ~
        ~              !
L55400: %! City, State, Zip: ##################  ##   ##########         ~
        ~              !
L55420: %! Distributor Address: ##############################           ~
        ~              !
L55440: %! City, State, Zip: ##################  ##   ##########         ~
        ~              !
L55460: %! Total Number of Windows Purchashed: ####                      ~
        ~              !
L55480: %! Warranty ID: ########    Sales Order Number: ########         ~
        ~               !
L55610: %! Cust: #########  Due Date: ##########  Part No.: #############~
        ~############  !
L55520: %! Comments: ##############################                      ~
        ~              !
L55540: %!           ##############################                      ~
        ~              !
L55560: %!           ##############################                      ~
        ~              !
L55580: %!           ##############################                      ~
        ~              !
L55600: %!           ##############################                      ~
        ~              !

                                                         /* Label Format Statements*/
L55620: %   ###############  #  ###############
L55640: %   ##############################
L55660: %   ################## ##  ##########
L55680: %                                                                 !

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        delete_warranty
            call "SHOSTAT" ("DELETING WARRANTY ("&wa_id$&")")
            read #1,hold,key 2% = wa_id$, eod goto L60100
            delete #1
L60100: return clear all
        goto inputmode

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        print_header
           print page
           lcntr% = 3%
           if type$ = "L" then return
           pageno% = pageno% + 1%
           print using L55000
           print using L55040, date$, runtime$, pageno%
           print using L55070, type_desc$
           print using L55000
        return

        print_detail
           if type$ = "S" then goto L60020
           if type$ = "L" then goto L60080
            if lcntr% > 50% or lcntr% = 99% then gosub print_header
              print using L55280, wa_id$, wa_trans$
              print using L55300, wa_date1$
              print using L55320, wa_last$, wa_first$, wa_init$
              print using L55330, wa_phone_d$
              print using L55340, wa_addr$
              print using L55360, wa_city$, wa_st$, wa_zip$
              print using L55380, wa_addr_d$
              print using L55400, wa_city_d$, wa_st_d$, wa_zip_d$
              print using L55420, wa_addr_dt$
              print using L55440, wa_city_dt$, wa_st_dt$, wa_zip_dt$
              print using L55460, wa_total$
              print using L55480, wa_warranty$, wa_so$
              print using L55610, wa_cust$, wa_due1$, wa_part$
              print using L55520, wa_txt$(1%)
              print using L55540, wa_txt$(2%)
              print using L55560, wa_txt$(3%)
              print using L55580, wa_txt$(4%)
              print using L55600, wa_txt$(5%)
              print using L55000
           lcntr% = lcntr% + 19%
        return
                                                      /*  Summary Print   */
L60020:   if lcntr% > 57% or lcntr% = 99% then gosub print_header
              print using L55280, wa_id$, wa_trans$
              print using L55480, wa_warranty$, wa_so$
              print using L55320, wa_last$, wa_first$, wa_init$
              print using L55000
          lcntr% = lcntr% + 4%
        return

                                                     /*  Labels Print   */
L60080:  if lcntr% = 99% then gosub print_header
         print using L55680
         print using L55620, wa_first$, wa_init$, wa_last$
         print using L55640, wa_addr$
         print using L55660, wa_city$, wa_st$, wa_zip$
         print using L55680
       return

/* Generate Report of All or Range of Customer Last Names and/or Zip Codes */

        generate_report
             init(" ") wa_key$, wa_z$, wa_d$, wa_c$
             x_l%, xx_l%, x_z%, xx_z%, x_c%, xx_c% = 0

/* Assigned these values so that only compairing the length that was entered */
/* into the fields of Beg/End Last Name and Beg/End Zip Code.                */
             x_l%   = len(bg_last$)
             xx_l%  = len(ed_last$)
             x_z%   = len(bg_zip$)
             xx_z%  = len(ed_zip$)
             x_c%   = len(bg_cust$)
             xx_c%  = len(ed_cust$)

             if str(bg_last$,1%,3%) = "ALL" and len(bg_last$) = 3%             ~
                                                      then goto L60030
             str(wa_key$,1%,x_l%) = str(bg_last$,1%,x_l%)

L60030:      read #1,key > wa_key$, using L60010, wa_z$, wa_key$, wa_d$, wa_c$, ~
                                    wa_cd$, eod goto generate_done
             goto L60050

        generate_next
           init(" ") wa_due1$
           read #1,  using L60010, wa_z$, wa_key$, wa_d$, wa_c$, wa_cd$,       ~
                                                         eod goto generate_done

L60010:    FMT POS(17), CH(10), CH(15), POS(67), CH(7), POS(410), CH(9), POS(450), CH(2)

L60050:    if str(ed_last$,1%,3%) = "ALL" and len(ed_last$) = 3%               ~
                                                      then goto L60060
           if str(wa_key$,1%,xx_l%) > str(ed_last$,1%,xx_l%)                   ~
                                               then goto generate_done

L60060:    if bg_zip$ = "ALL" then goto L60070
           if str(wa_z$,1%,x_z%) < bg_zip$ or                                  ~
              str(wa_z$,1%,xx_z%) > ed_zip$                                    ~
                                          then goto generate_next

L60070:  if bg_date$ = "ALL" then goto L60090
             wa_d1$  = wa_d$
             call "DATFMTC" (wa_d1$)
             str(wa_test$,1%,4%) = str(wa_d1$,7%,4%)
             str(wa_test$,5%,7%) = str(wa_d1$,1%,2%)
             if wa_test$ < bg_date1$ or wa_test$ > ed_date1$                   ~
                                               then goto generate_next

L60090:  if bg_cust$ = "ALL" then goto L60110
           if str(wa_c$,1%,x_c%)  < str(bg_cust$,1%, x_c%) or                   ~
              str(wa_c$,1%,xx_c%) > str(ed_cust$,1%, xx_c%)                     ~
                                          then goto generate_next

L60110:  if sc_code$ = "AA" then goto scan_data
           if wa_cd$ <> sc_code$ then goto generate_next

        scan_data
          if type$ = "S" then get #1,  using L60040, wa_so$, wa_warranty$,      ~
                                                     wa_last$, wa_first$,       ~
                                                     wa_init$, wa_id$, wa_trans$~
          else gosub dataload
          gosub print_detail
          goto generate_next

L60040: FMT CH(8), CH(8), POS(27), CH(15), CH(15), CH(1), CH(8), CH(1)

        generate_done
            close printer
        return

/* lookup warranty number info from first live files and then history files  */
/* Rec% is 1 if key value found.  Fields assigned values from Rec value      */
/* if lookup warranty then should find values of cust, due date, and part no */
/* if lookup so then should find values of cust and due date (more than one  */
/* warranty number for one so.                                               */
/* if no values returned then the warranty information is questionable --    */
/* (if warranty actually exists)                                             */

        lookup_warranty
            init(" ") wa_key$, wa_key1$
            rec% = 0
            str(wa_key$,1%,8%)  = wa_warranty$
                                                    /* (PAR001)        */
                                                    /* Read Live Files */
            read #3, key 0% = wa_key$, using L60200, wa_key$, wa_key1$,         ~
                              eod goto L60290  /*SR76707  eod goto L60230 */
L60200:     FMT CH(8), CH(10)
                                                    /* (PAR001)        */
            read #4, key = wa_key1$, using L60210, wa_due$, wa_key1$,           ~
                     wa_part$, wa_cust$, eod goto L60290 /*SR76707 eod goto L60230*/
            rec% = 1%
            goto L60250

L60210:     FMT CH(6), POS(24), CH(10), CH(25), CH(9)

                                                     /*  Read History Files  */
L60230:     read #5, key > wa_key$, using L60220, wa_key$, wa_key1$,           ~
                                                  eod goto L60290
L60220:     FMT CH(15), POS(16), CH(15)

            if str(wa_key$,1%,8%) <> wa_warranty$ then goto L60290

            read #6, key 1% = wa_key1$, using L60240, wa_key1$, wa_cust$,      ~
                                        wa_part$, wa_due$, eod goto L60290

           rec% = 1%
L60240:    FMT POS(54), CH(11), POS(77), CH(9), POS(119), CH(25), POS(209), CH(6)

L60250:   if wa_due$ <> " " then goto L60260
               goto L60290

L60260:     wa_due1$ = wa_due$
            call "DATFMTC" (wa_due1$)
            x$ = wa_due1$
            call "DATUFMTC" (x$)
            wa_due$ = str(x$,1%,6%)
L60290:  if rec% = 0% then goto L60295
            wa_so$   = str(wa_key1$,1%,8%)
            gosub lookup_brand
        return
L60295:  wa_so$   = "UNKNOWN "
        return


/* lookup sales order number info from first live files and then history files  */
/* Rec% is 1 if key value found.  Fields assigned values from Rec value      */

         lookup_so
            init(" ") wa_key$, wa_key1$
            rec% = 0
            str(wa_key$,1%,8%)  = wa_so$
                                       /* (PAR001)        */
                                       /* Read Live Files */
            read #3, key 1% > wa_key$, using L60300, wa_key$,                  ~
                              eod goto L60390  /*SR79091 eod goto L60320 */
L60300:     FMT POS(9), CH(10)
                                       /* (PAR001)        */

            if str(wa_key$,1%,8%) <> wa_so$ then goto L60390 /*SR76707 L60320*/


            read #4, key = wa_key$, using L60310, wa_due$, wa_key$,          ~
                           wa_cust$, eod goto L60390 /*SR76707 eod goto L60320*/

            rec% = 1%
            goto L60360

L60310:     FMT CH(6), POS(24), CH(10), POS(59), CH(9)


L60320:     init(" ") wa_key$, wa_key1$
            str(wa_key$,1%,8%)  = wa_so$
                                                      /*  Read History Files  */
            read #5, key 1% > wa_key$, using L60330, wa_key$,                  ~
                                                  eod goto L60390

            if str(wa_key$,1%,8%) <> wa_so$ then goto L60390

L60330:     FMT POS(16), CH(15)

            read #6, key 1% = wa_key$, using L60340, wa_key$, wa_cust$,        ~
                                        wa_due$, eod goto L60390

            rec% = 1%
L60340:    FMT POS(54), CH(8), POS(77), CH(9), POS(209), CH(6)

L60360:    if wa_due$ <> " " then goto L60350
               goto L60390

L60350:     wa_due1$ = wa_due$
            call "DATFMTC" (wa_due1$)


L60390: if rec% = 0% then return
           gosub lookup_brand
        return

        lookup_brand
            if wa_cust$ = "999999" then wa_cust$ = "         "
            init(" ") wa_key$
            wa_key$ = wa_cust$

            read #7, key = wa_key$, using L60400, wa_key$, wa_code$, eod goto L60450
L60400:    FMT CH(9), POS(960), CH(2)

L60450: return


        open_error
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************


    exit_program
          call "SHOSTAT" ("One Moment Please")
            end
