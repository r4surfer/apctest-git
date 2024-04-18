        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN12                             *~
            *  Creation Date     - 02/04/04                             *~
            *  Last Modified Date- 04/28/2015                           *~
            *  Written By        - Christie Gregory                     *~
            *  Last Modified By  - Christie                             *~
            *                                                           *~
            *  Description       - Utility to scan orders that are      *~
            *                      assigned to loads but are not        *~
            *                      planned with inventory levels to     *~
            *                      give planned the ability to assign   *~
            *                      then to department '102'.            *~
            *                      (1) Detail Analysis by S.O.          *~
            *                                                           *~
            *  Code Tables Used  - (         ), (         ), (         )*~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/04/04 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            * 04/05/05 ! (AWD001) - Mod to the reclen of APCPLNSD ! CMG *~
            *04/25/2015! (IM8039) - mod for lowes customer        ! CMG *~
            *01/07/2019! (CR1853) - Filter Pet Mesh               ! RDB *~
            *************************************************************


        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            blankdate$6,                 /* Null Date test             */~ 
            bg_due$10, bg_dte$10,        /* Beg/End S.O. Due Date      */~
            ed_due$10, ed_dte$10,        /*                            */~
            bg_load$5,                   /* Beg/End Load Number        */~
            ed_load$5,                   /*                            */~
            bg_model$3, ed_model$3,      /* Beg/End Model  Codes       */~
            bg_override$1,               /* Override Subpart           */~
            cust_override$1,             /* Override Customer IM8039   */~
            cnt$28, page$16, line1$65,   /* Screen Display             */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            date$8,                      /* Date                       */~
            readkey$50,                  /* Generic Key                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim ld_key$5,                    /* APCPLNLD readkey           */~
            sc_key$33,                   /* APCPLNSC readkey #2        */~
            sc_status$2,                 /* Planning Status            */~
            sc_part$25,                  /* Mfg Part Number            */~
            sc_customer$9,               /* APCPLNSC Customer          */~
            sc_load$5,                   /* APCPLNSC Load              */~
            sc_due$6,                    /* APCPLNSC Due Date          */~
            or_key$8,                    /* APCPLNOR readkey #4        */~
            dp_key$12,                   /* APCPLNDP readkey           */~
            sd_key$23,                   /* APCPLNSD readkey           */~
            pull_flag$1,                 /* Inventory Pull Flag        */~
/*PAR000*/  quan_key$64,                 /* INVQUAN readkey            */~
/*PAR000*/  invmastr_key$45              /* INVMASTR readkey           */


        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

        dim h1$3, h2$5, h3$9, h4$25,     /* Screen Headers             */~
            h5$8, h6$2, h7$6, h8$6,      /* Screen Headers             */~
            c1$(5000%)1,                 /* Include/Exclude            */~
            c2$(5000%)5,                 /* Load Number                */~
            c3$(5000%)9,                 /* Customer                   */~
            c4$(5000%)25,                /* Part Number                */~
            c5$(5000%)8,                 /* Sales Order Number         */~
            c6$(5000%)2,                 /* Sales Order Line           */~
            c7$(5000%)6,                 /* Qty Ordered                */~
            c8$(5000%)6,                 /* Inventory Qty              */~
/*PAR000*/  c9$(5000%)20                 /* Subpart Number             */



        dim                              /* PAR000                     */~
            subp$20,                     /* Subpart number             */~
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
            dim apc$40, pname$21
            apc$   = "(New) Scheduling Stock Analysis Utility "
            pname$ = "AWDPLN12"

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
            * #6  ! APCPLNSD ! Scheduling Department Detail             *~
            * #7  ! APCPLNDP ! Planning Master Department File          *~
/*PAR000*/  * #20 ! INVMASTR ! Inventory Master File                    *~
/*PAR000*/  * #21 ! INVQUAN  ! Inventory Quantities File                *~
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

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #3,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33

            select #4,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos = 1,    keylen =  9,                      ~
                        alt key 1, keypos = 10, keylen = 30, dup,        ~
                            key 2, keypos =424, keylen =  9, dup,        ~
                            key 3, keypos =771, keylen =  9, dup,        ~
                            key 4, keypos =780, keylen =  9, dup,        ~
                            key 5, keypos = 1049, keylen = 9, dup

            select #5,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15


/* (AWD001) - Mod to key and reclen */
            select #6,  "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =  1,   keylen = 23


            select #7,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos = 11,   keylen = 12,                      ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15



* PAR000
            select #20, "INVMASTR",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =  45,                     ~
                        alt key  1, keypos =  122, keylen =   9, dup,    ~
                            key  2, keypos =  110, keylen =   4, dup,    ~
                            key  3, keypos =   46, keylen =  32, dup

            select #21, "INVQUAN",                                       ~
                        varc, indexed,                                   ~
                        recsize =   768,                                 ~
                        keypos =   17, keylen =  64,                     ~
                        alt key  1, keypos =    1, keylen =  64

            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup     


            call "SHOSTAT" ("Initialization")

                                                       

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
            filename$ = "APCPLNSD" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNDP" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error

* PAR000
            filename$ = "INVMASTR" : call "EWDOPEN" (#20, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "INVQUAN " : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error

            mat f1% = zer
            mat fs% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "DATUFMTC" (blankdate$)                                        
            h1$ = "I/E"
            h2$ = "Load "
            h3$ = "Customer "
            h4$ = "<----- Part Number ----->"
            h5$ = "<-S.O.->"
            h6$ = "Ln"
            h7$ = "QTY"
            h8$ = "INV"


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 5%
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
                  if keyhit%  =  8% then gosub so_analysis
                  if keyhit%  =  9% then gosub load_analysis
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11150:     row% = cursor%(1%) : col% = cursor%(2%) /* (IM8039) */
REM            FIELDNR% = CURSOR%(1%) - 2%
            fieldnr% = row% - 2%
            if fieldnr% = 4% and col% > 50% then fieldnr% = 5%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11200:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                  lastfieldnr% = fieldnr%
            goto L11150

        REM *************************************************************~
            *                                                           *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************


        so_analysis
            call "SHOSTAT" ("Loading S.O. Analysis Data")
            analysis% = 0%
            goto L19680
        load_analysis
            call "SHOSTAT" ("Loading Truck Load Data")
            analysis% = 1%

L19680:     init(" ") cnt$, or_key$, sc_key$, c1$(), c2$(), c3$(), c4$(), ~
                      c5$(), c6$(), c7$(), c8$(), rslt$(), c9$()
            drop_seq_max% = 0%                 /* Last Drop Seq Loaded */
            cnt% = 0%
            cnt$ = "Records Scanned [ xxxxxxxx ]"

            val_max%     = 0%  : cc%            = 0%
            on (analysis% + 1%) gosub scan_surge, scan_loads

            gosub'102(1%, 1%)                   /* Select Sales Orders */
            keyhit% = 0%

        return

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
         "Enter a Valid Beginning/Ending Due Date/?                    ",~
         "Enter a Valid Beginning/Ending Load or ALL?                  ",~
         "Enter a Valid Beginning/Ending Model Code or ALL?            ",~
         "Enter a Valid Value 'Y'es or 'N'o                            ",~
         "Enter a Valid Value 'Y'es or 'N'o                            "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28280
                inpmessage$ = edtmessage$
                return

L28280
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Non-Blank Character or New Insert Seq. Number?       "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, bg_due$, bg_dte$,~
                      c1$(), c2$(),  hdr$, msg$(), c3$(), c4$(), c5$(),  ~
                      c6$(), c7$(), c8$(), ed_due$, ed_dte$,             ~
                      bg_model$, ed_model$, bg_load$, ed_load$, c9$(),   ~
                      bg_override$, cust_override$
            analysis% = 0%                       /* Set Default to S.O.*/
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
REM     dataload                        

REM     return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT
        REM RETURN

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************



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
                                L40200,         /* Beg/End Load Number  */~
                                L40200,         /* Beg/End Model Code   */~
                                L40200,         /* Override             */~
                                L40200          /* Override             */
              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Begin S.O. Due Date  :",                     ~
               at (03,25), fac(lfac$(1%)), bg_due$              , ch(10),~
                                                                         ~
               at (03,40), "Ending S.O. Due Date :",                     ~
               at (03,63), fac(lfac$(1%)), ed_due$              , ch(10),~
                                                                         ~
               at (04,02), "Beginning Load       :",                     ~
               at (04,25), fac(lfac$(2%)), bg_load$             , ch(05),~
                                                                         ~
               at (04,40), "Ending Load          :",                     ~
               at (04,63), fac(lfac$(2%)), ed_load$             , ch(05),~
                                                                         ~
               at (05,02), "Beginning Model      :",                     ~
               at (05,25), fac(lfac$(3%)), bg_model$            , ch(03),~
                                                                         ~
               at (05,40), "Ending Model         :",                     ~
               at (05,63), fac(lfac$(3%)), ed_model$            , ch(03),~
                                                                         ~
               at (06,02), "Override Subpart     :",                     ~
               at (06,25), fac(lfac$(4%)), bg_override$         , ch(01),~
/* (IM8039) */                                                           ~
               at (06,40), "Lowes Only           :",                     ~
               at (06,63), fac(lfac$(5%)), cust_override$       , ch(01),~               
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
               str(pf$(3%),40%,20%) = " " : str(pfkeys$,10%,1%) = hex(ff)
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40950:     if fieldnr% > 1% then L40970

L40970:     return

L40990: if fieldnr% > 0% then L41100  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "(8)Inv. Analysis                       "
            pf$(2%) = "                                        " &       ~
                      "(9)Assigned Inv. Analysis              "
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffff0809ffffffff0e0f1000)
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
            *         S T A T U S   D I S P L A Y   S C R E E N         *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
L42025:       gosub set_pf2
              gosub'060(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              lfac$(fieldnr%) = hex(81)                  /* Upper Only */

            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), line1$                 , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(50),~
               at (02,60), fac(hex(8c)), page$                  , ch(16),~
                                                                         ~
               at (03,02), "Beg/End Due Date:",                          ~
               at (03,20), fac(hex(84)), bg_due$                , ch(10),~
               at (03,31), fac(hex(84)), ed_due$                , ch(10),~
                                                                         ~
               at (04,02), "Beg/End Load    :",                          ~
               at (04,20), fac(hex(84)), bg_load$               , ch(05),~
               at (04,31), fac(hex(84)), ed_load$               , ch(05),~
                                                                         ~
               at (05,02), "Beg/End Model   :",                          ~
               at (05,20), fac(hex(84)), bg_model$              , ch(03),~
               at (05,31), fac(hex(84)), ed_model$              , ch(03),~
                                                                         ~
               at (09,02), fac(hex(a4)), h1$                    , ch(03),~
               at (09,06), fac(hex(a4)), h2$                    , ch(05),~
               at (09,12), fac(hex(a4)), h3$                    , ch(09),~
               at (09,22), fac(hex(a4)), h4$                    , ch(25),~
               at (09,48), fac(hex(a4)), h5$                    , ch(08),~
               at (09,57), fac(hex(a4)), h6$                    , ch(02),~
               at (09,60), fac(hex(a4)), h7$                    , ch(06),~
               at (09,67), fac(hex(a4)), h8$                    , ch(06),~
                                                                         ~
               at (10,03), fac(lfac$(1%)), c1$( 1% + kk%)       , ch(01),~
               at (10,06), fac(hex(84)),   c2$( 1% + kk%)       , ch(05),~
               at (10,12), fac(hex(84)),   c3$( 1% + kk%)       , ch(09),~
               at (10,22), fac(hex(84)),   c4$( 1% + kk%)       , ch(25),~
               at (10,48), fac(hex(84)),   c5$( 1% + kk%)       , ch(08),~
               at (10,57), fac(hex(84)),   c6$( 1% + kk%)       , ch(02),~
               at (10,60), fac(hex(84)),   c7$( 1% + kk%)       , ch(06),~
               at (10,67), fac(hex(84)),   c8$( 1% + kk%)       , ch(06),~
                                                                         ~
               at (11,03), fac(lfac$(1%)), c1$( 2% + kk%)       , ch(01),~
               at (11,06), fac(hex(84)),   c2$( 2% + kk%)       , ch(05),~
               at (11,12), fac(hex(84)),   c3$( 2% + kk%)       , ch(09),~
               at (11,22), fac(hex(84)),   c4$( 2% + kk%)       , ch(25),~
               at (11,48), fac(hex(84)),   c5$( 2% + kk%)       , ch(08),~
               at (11,57), fac(hex(84)),   c6$( 2% + kk%)       , ch(02),~
               at (11,60), fac(hex(84)),   c7$( 2% + kk%)       , ch(06),~
               at (11,67), fac(hex(84)),   c8$( 2% + kk%)       , ch(06),~
                                                                         ~
               at (12,03), fac(lfac$(1%)), c1$( 3% + kk%)       , ch(01),~
               at (12,06), fac(hex(84)),   c2$( 3% + kk%)       , ch(05),~
               at (12,12), fac(hex(84)),   c3$( 3% + kk%)       , ch(09),~
               at (12,22), fac(hex(84)),   c4$( 3% + kk%)       , ch(25),~
               at (12,48), fac(hex(84)),   c5$( 3% + kk%)       , ch(08),~
               at (12,57), fac(hex(84)),   c6$( 3% + kk%)       , ch(02),~
               at (12,60), fac(hex(84)),   c7$( 3% + kk%)       , ch(06),~
               at (12,67), fac(hex(84)),   c8$( 3% + kk%)       , ch(06),~
                                                                         ~
               at (13,03), fac(lfac$(1%)), c1$( 4% + kk%)       , ch(01),~
               at (13,06), fac(hex(84)),   c2$( 4% + kk%)       , ch(05),~
               at (13,12), fac(hex(84)),   c3$( 4% + kk%)       , ch(09),~
               at (13,22), fac(hex(84)),   c4$( 4% + kk%)       , ch(25),~
               at (13,48), fac(hex(84)),   c5$( 4% + kk%)       , ch(08),~
               at (13,57), fac(hex(84)),   c6$( 4% + kk%)       , ch(02),~
               at (13,60), fac(hex(84)),   c7$( 4% + kk%)       , ch(06),~
               at (13,67), fac(hex(84)),   c8$( 4% + kk%)       , ch(06),~
                                                                         ~
               at (14,03), fac(lfac$(1%)), c1$( 5% + kk%)       , ch(01),~
               at (14,06), fac(hex(84)),   c2$( 5% + kk%)       , ch(05),~
               at (14,12), fac(hex(84)),   c3$( 5% + kk%)       , ch(09),~
               at (14,22), fac(hex(84)),   c4$( 5% + kk%)       , ch(25),~
               at (14,48), fac(hex(84)),   c5$( 5% + kk%)       , ch(08),~
               at (14,57), fac(hex(84)),   c6$( 5% + kk%)       , ch(02),~
               at (14,60), fac(hex(84)),   c7$( 5% + kk%)       , ch(06),~
               at (14,67), fac(hex(84)),   c8$( 5% + kk%)       , ch(06),~
                                                                         ~
               at (15,03), fac(lfac$(1%)), c1$( 6% + kk%)       , ch(01),~
               at (15,06), fac(hex(84)),   c2$( 6% + kk%)       , ch(05),~
               at (15,12), fac(hex(84)),   c3$( 6% + kk%)       , ch(09),~
               at (15,22), fac(hex(84)),   c4$( 6% + kk%)       , ch(25),~
               at (15,48), fac(hex(84)),   c5$( 6% + kk%)       , ch(08),~
               at (15,57), fac(hex(84)),   c6$( 6% + kk%)       , ch(02),~
               at (15,60), fac(hex(84)),   c7$( 6% + kk%)       , ch(06),~
               at (15,67), fac(hex(84)),   c8$( 6% + kk%)       , ch(06),~
                                                                         ~
               at (16,03), fac(lfac$(1%)), c1$( 7% + kk%)       , ch(01),~
               at (16,06), fac(hex(84)),   c2$( 7% + kk%)       , ch(05),~
               at (16,12), fac(hex(84)),   c3$( 7% + kk%)       , ch(09),~
               at (16,22), fac(hex(84)),   c4$( 7% + kk%)       , ch(25),~
               at (16,48), fac(hex(84)),   c5$( 7% + kk%)       , ch(08),~
               at (16,57), fac(hex(84)),   c6$( 7% + kk%)       , ch(02),~
               at (16,60), fac(hex(84)),   c7$( 7% + kk%)       , ch(06),~
               at (16,67), fac(hex(84)),   c8$( 7% + kk%)       , ch(06),~
                                                                         ~
               at (17,03), fac(lfac$(1%)), c1$( 8% + kk%)       , ch(01),~
               at (17,06), fac(hex(84)),   c2$( 8% + kk%)       , ch(05),~
               at (17,12), fac(hex(84)),   c3$( 8% + kk%)       , ch(09),~
               at (17,22), fac(hex(84)),   c4$( 8% + kk%)       , ch(25),~
               at (17,48), fac(hex(84)),   c5$( 8% + kk%)       , ch(08),~
               at (17,57), fac(hex(84)),   c6$( 8% + kk%)       , ch(02),~
               at (17,60), fac(hex(84)),   c7$( 8% + kk%)       , ch(06),~
               at (17,67), fac(hex(84)),   c8$( 8% + kk%)       , ch(06),~
                                                                         ~
               at (18,03), fac(lfac$(1%)), c1$( 9% + kk%)       , ch(01),~
               at (18,06), fac(hex(84)),   c2$( 9% + kk%)       , ch(05),~
               at (18,12), fac(hex(84)),   c3$( 9% + kk%)       , ch(09),~
               at (18,22), fac(hex(84)),   c4$( 9% + kk%)       , ch(25),~
               at (18,48), fac(hex(84)),   c5$( 9% + kk%)       , ch(08),~
               at (18,57), fac(hex(84)),   c6$( 9% + kk%)       , ch(02),~
               at (18,60), fac(hex(84)),   c7$( 9% + kk%)       , ch(06),~
               at (18,67), fac(hex(84)),   c8$( 9% + kk%)       , ch(06),~
                                                                         ~
               at (19,03), fac(lfac$(1%)), c1$(10% + kk%)       , ch(01),~
               at (19,06), fac(hex(84)),   c2$(10% + kk%)       , ch(05),~
               at (19,12), fac(hex(84)),   c3$(10% + kk%)       , ch(09),~
               at (19,22), fac(hex(84)),   c4$(10% + kk%)       , ch(25),~
               at (19,48), fac(hex(84)),   c5$(10% + kk%)       , ch(08),~
               at (19,57), fac(hex(84)),   c6$(10% + kk%)       , ch(02),~
               at (19,60), fac(hex(84)),   c7$(10% + kk%)       , ch(06),~
               at (19,67), fac(hex(84)),   c8$(10% + kk%)       , ch(06),~
                                                                         ~
               at (20,63), fac(hex(84)),   pg_tot_unit$         , ch(06),~
               at (20,70), fac(hex(84)),   pg_tot_value$        , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then goto L42830            /* Startover*/
                  gosub startover
                  goto L42025

L42830:        if keyhit% <> 2% then goto L42850            /* First    */
L42835:           kk% = 0%
                  goto L42025

L42850:        if keyhit% <> 3% then goto L42875            /* Last      */
L42855:           x% = int(val_max% / 10%)
                  kk% = (x%*10%)
                  goto L42025

L42875:        if keyhit% <> 4% then goto L42905            /* Previous */
                  if kk% < 11% then goto L42835
                  kk% = kk% - 10%
                  if kk% <= 1% then goto L42835
                  goto L42025

L42905:        if keyhit% <> 5% then goto L43045            /* Next     */
                  kk% = kk% + 10%
                  if kk% < val_max% then goto L42025
                  goto L42855

                                   /* Include S.O. 'Non-Blank'         */
                                   /* Skip 'Blank' Process 'Non-Blank' */
L43045:        if keyhit% <> 9%  then goto L43070
                  gosub process_data
                  goto L43190
                                   /* Exclude S.O. 'Non-Blank'         */
                                   /* Skip 'Non-Blank', Process 'Blank'*/
L43070:        if keyhit% <> 10% then goto L43085
                  gosub process_data
                  goto L43190
L43085: 
               

               if keyhit% <> 16% then goto L42025

L43190:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf2
            if analysis% = 0% then                                       ~
            line1$ =                                                     ~
                  "(New) Scheduling S.O. Analysis - Display"             ~
                              else                                       ~
            line1$ =                                                     ~
                  "(New) Scheduling Load Analysis - Display"

            page$ ="Page: xxx of xxx"
            x1% = (kk% / 10%) + 1%
            x2% = (val_max% / 10%) + 1%
            if x2% < 1% then x2% = 1%
            convert x1% to str(page$,7%,3%), pic(###)
            convert x2% to str(page$,14%,3%), pic(###)

            pg_tot_unit = 0.0 : pg_tot_value = 0.0
            for n% = 1% to 10%
                y1 = 0% : x1 = 0.0
                convert c7$(n% + kk%) to y1, data goto L43305
L43305:
                convert c8$(n% + kk%) to x1,  data goto L43315
L43315:
                pg_tot_unit  = pg_tot_unit + y1
                pg_tot_value = pg_tot_value + x1
            next n%
            convert pg_tot_unit to pg_tot_unit$, pic(###.##)
            convert pg_tot_value to pg_tot_value$, pic(######.##-)

            pf$(1%) = "(1)Start Over  (4)Previous              " &       ~
                      "                                       "
            pf$(2%) = "(2)First       (5)Next       (9)Include " &       ~
                      "                                       "
            pf$(3%) = "(3)Last                      (10)Exclude" &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(0102030405ffffff090affffffffff1000)
            gosub check_screen
        return

        check_screen
            if analysis% = 1% then gosub no_i_e


            if val_max% > 10% then goto L43450
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L43450:      if kk% >= 10% then goto L43465
                gosub no_first
                gosub no_prev
L43465:      if (kk% + 10%) <= val_max% then goto L43475
                gosub no_last
L43475:      if kk% <= (val_max% - 10%) then goto L43485
                gosub no_next
L43485: return
        no_i_e
        return
            str(pf$(2%),30%,11%)= " " : str(pfkeys$, 9%,1%) = hex(ff)
            str(pf$(3%),30%,11%)= " " : str(pfkeys$,10%,1%) = hex(ff)
        return
        no_first
            str(pf$(2%),1%,10%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(2%),16%,12%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(3%),1%,10%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(1%),16%,12%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return



        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50000,         /* Beg/End Due Date      */ ~
                              L50150,         /* Beg/End Load          */ ~
                              L50390,         /* Beg/End Model         */ ~
                              L50420,         /* Override              */ ~
                              L50480          /* Override              */ 
                              
            return

L50000: REM Beginning Due/Delivery Date           BG_DUE$, BG_DTE$
            if bg_due$ <> " " then goto L50090                                   
               bg_due$ = date$                                                  

L50090:     date% = 0%
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
            if bg_dte$ > ed_dte$ then goto L50030
        return
L50030:     errormsg$ = "(Error) - Invalid Due Date Range??"
        date_error
            gosub error_prompt
            init(" ") bg_due$, bg_dte$, ed_due$, ed_dte$
        return

L50150: REM Beginning Due/Delivery Date           BG_LOAD$, BG_LOAD$
            if str(bg_load$,1%,5%) <> " " then goto L50190
               bg_load$ = "ALL"

L50190:     if str(bg_load$,1%,3%) = "ALL" then goto end_load
               init(" ") ld_key$ : ld_key$ = bg_load$
               gosub check_load


        end_load
            if str(ed_load$,1%,5%) <> " " then goto L50200
               ed_load$ = bg_load$               


L50200:     if str(ed_load$,1%,3%) = "ALL" then return
               init(" ") ld_key$ : ld_key$ = ed_load$
               gosub check_load
               if bg_load$ > ed_load$ then goto L50330
        return
L50330:     errormsg$ = "(Error) - Invalid Load Range??"
            gosub error_prompt
            init(" ") bg_load$, ed_load$
        return


        check_load
            read #5,key = ld_key$, eod goto load_error
        return
        load_error
          errormsg$="(Error) - Invalid Load Number, Not Defined?"
          gosub error_prompt
          init(" ") ld_key$, bg_load$, ed_load$
        return

L50390: REM Beg/End Region Codes                  BG_MODEL$, ED_MODEL$
            if str(bg_model$,1%,3%) <> " " then goto L50440
               bg_model$ = "ALL"


L50440:     if str(bg_model$,1%,3%) = "ALL" then goto end_model
               init(" ")readkey$
               str(readkey$,1%,9%)   = "MODEL    "
               str(readkey$,10%,15%) = bg_model$
               gosub check_model


            end_model
            if str(ed_model$,1%,3%) <> " " then goto L50450
               ed_model$ = bg_model$


L50450:     if str(ed_model$,1%,3%) = "ALL" then return
               init(" ")readkey$
               str(readkey$,1%,9%)   = "MODEL    "
               str(readkey$,10%,15%) = ed_model$

               gosub check_model
               if bg_model$ > ed_model$ then goto L50490
        return
L50490:                      
          errormsg$="(Error) - Invalid Beg/End Model Number?"
          gosub error_prompt
          init(" ") bg_model$, ed_model$
        return

        check_model
            read #2,key = readkey$, eod goto model_error
        return
        model_error
          errormsg$="(Error) - Invalid Model Number, Not Defined?"
          gosub error_prompt
          init(" ") readkey$, bg_model$, ed_model$
        return
        
L50420: REM Override Subpart                   bg_override$
        if bg_override$ = " " then bg_override$ = "Y"
        if bg_override$ <> "Y" and bg_override$ <> "N" then  ~
                 goto L50460
        return
L50460:                 
          errormsg$="(Error) - Invalid Override Option?"
          gosub error_prompt
          init(" ") bg_override$
       return

       
L50480: REM Override customer                   cust_override$
/* (IM8039) */
        if cust_override$ = " " then cust_override$ = "Y"
        if cust_override$ <> "Y" and cust_override$ <> "N" then ~
                 goto L50500
                            
        if edit% = 1% then fieldnr% = 5%               
        return
L50500:                 
          errormsg$="(Error) - Invalid Customer Override Option?"
          gosub error_prompt
          init(" ") cust_override$
       return       

        REM *************************************************************~
            *************************************************************


        scan_loads
            
        scan_surge
            str(sc_key$,1%,6%) = bg_dte$
            if bg_load$ = "ALL" then goto scan_surge_nxt
               str(sc_key$,9%,2%) = bg_load$
        scan_surge_nxt
          read #3,key 2% > sc_key$, using L61125, sc_key$, sc_part$,     ~
                        sc_customer$, sc_status$, eod goto scan_surge_done
L61125:         FMT CH(33), POS(34), CH(25), POS(59), CH(9), POS(110), CH(02)
          cnt% = cnt% + 1%
          sc_due$  = str(sc_key$,1%,6%)
          sc_load$ = str(sc_key$,7%,5%)

          if mod(cnt%,100%) <> 0% then goto L61155
             convert cnt% to str(cnt$,19%,8%), pic(########)
             print at(02,02);hex(84);cnt$;
REM   NOTE Take out this remark stmt

L61155:   if sc_status$ <> "02" then goto scan_surge_nxt

          if str(sc_key$,1%,6%) > ed_dte$ then goto scan_surge_done
          if str(bg_load$,1%,3%) = "ALL" then goto L61175
             if str(sc_key$,7%,5%) < bg_load$ or                         ~
                str(sc_key$,7%,5%) > ed_load$ then goto scan_surge_nxt
L61175:   if str(bg_model$,1%,3%) = "ALL" then goto L61190
             if str(sc_part$,1%,3%) < bg_model$ or                      ~
                str(sc_part$,1%,3%) > ed_model$ then goto scan_surge_nxt
L61190:   

/* (IM8039) */
          if cust_override$ = "Y" and str(sc_customer$,1%,2%) <> "LO" and ~
               str(sc_customer$,1%,2%) <> "LX" then goto scan_surge_nxt
             
             sc_tqty%, sc_mqty%, sc_pqty%, sc_pqty1% = 0%

             get #3, using L61250, sc_tqty%, sc_mqty%, sc_pqty%, sc_pqty1%   

L61250:         FMT POS(68), 4*BI(2)

                                                    /* Analysis% = 0%    */ 
                                                    /*for scan orders for*/
                                                    /*Stock.             */
                                                    /* Analysis% = 1%    */
                                                    /* for orders already*/
                                                    /* Assigned to stock */
* PAR000
               so_inv$  = str(sc_key$,24%, 8%)
               item_no$ = str(sc_key$,32%, 2%)
               gosub lookup_subpart                
               subp$ = str(bcksubpt_rec$,48%,20%)
               
/* CR1853 */  
               if str(subp$,15%,1%) = "4" then goto scan_surge_nxt  
* PAR000
             
             if analysis% = 1% then gosub scan_loads_screen                ~
                else gosub scan_surge_screen
  
             goto scan_surge_nxt
        scan_surge_done
           val_max% = cc%

           kk% = 0%
        return

        scan_loads_screen
             gosub get_pull_flag
             if pull_flag$ <> "N" then return

             if sc_pqty% = 0% then return
             gosub check_qtys
             gosub load_screen

        return

        scan_surge_screen
             if sc_mqty% = 0% then return
             gosub get_pull_flag
             if pull_flag$ <> "N" then return

             gosub check_qtys

             if (on_hand - pending - sc_mqty%) < 0.00 then return

             gosub load_screen
        return

        load_screen
             cc% = cc% + 1%
             c1$(cc%) = " "

REM             if sc_pqty% <> 0% then c1$(cc%) = "*"   ~
                else c1$(cc%) = " "

             c2$(cc%) = str(sc_key$,7%,5%)               /* Load Number*/
             c3$(cc%) = sc_customer$                     /* Cust  Code */
             c4$(cc%) = sc_part$                         /* Mfg Part   */

             c5$(cc%) = str(sc_key$,24%,8%)              /* So Number  */
             c6$(cc%) = str(sc_key$,32%,2%)              /* line Number*/
             convert sc_mqty% to c7$(cc%), pic(#####0)   /* SC Make Qty*/

             if sc_pqty% <> 0% then convert sc_pqty% to c7$(cc%), pic(#####0)

                                                         /* On Hand Qty */
             convert (on_hand - pending) to c8$(cc%), pic(####0-) 
             
* PAR000
             c9$(cc%) = subp$
        return


        get_pull_flag
                                      /* Check Inventory Pull Flag */
                                      /* Has to set to "N"         */
                                      /* Has to set to "Y" for     */
                                      /* Background task to pull   */
             init(" ") invmastr_key$
             str(invmastr_key$, 1%,25%) = sc_part$
             if bg_override$ = "Y" then               ~
                        str(subp$,10,10) = "0000000000"
                        
             str(invmastr_key$,26%,20%) = subp$
             read #20, key = invmastr_key$, eod goto not_pull

                  get #20, using L61350, pull_flag$

L61350:         FMT POS(606), CH(1)     

        not_pull
        return

        check_qtys
                                      /* Check Qty available to    */
                                      /* See if can be candiate    */
             init(" ") quan_key$      
             on_hand, pending = 0.00
             str(quan_key$, 1%,25%)  = sc_part$
             str(quan_key$,26%,20%)  = subp$
             str(quan_key$,46%, 3%)  = "300"

             read #21, key = quan_key$, eod goto no_qtys
                  get #21, using L61400, on_hand, pending

L61400:             FMT POS(89), PD(14,4), POS(129), PD(14,4)

        no_qtys
        return

        process_data
            call "SHOSTAT" ("Processing Include/Exclude Data")
            cnt% = drop_seq_max%

            for i% = 1% to val_max%
                                              /* Include '*' Skip ' '  */
                if keyhit% =  9% and c1$(i%) = " " then goto L62295
                                              /* Include ' ' Skip '*'  */
                if keyhit% = 10% and c1$(i%) <> " " then goto L62295


REM                    if analysis% = 1% then gosub update_load       ~
                        else gosub update_surge

                    gosub update_apcplnsd
                    gosub update_apcplnsc
                    gosub update_invquan
                    gosub update_header


L62295:     next i%
        return


REM        update_surge
REM           gosub update_apcplnsd
REM           gosub update_apcplnsc
REM           gosub update_hnyquan
REM           gosub update_header

REM        return

REM        update_load
REM           gosub update_apcplnsd
REM           gosub update_apcplnsc
REM           gosub update_hnyquan
REM           gosub update_header
REM        return


        update_apcplnsd
              gosub lookup_apcplndp
              if analysis% <> 1% then goto sd_done


              init(" ") sd_key$
              str(sd_key$,1%,10%) = str(c5$(i%),1%,8%) & str(c6$(i%),1%,2%)

        check_sd_nxt
              read #6, hold, key > sd_key$, using L60100, sd_key$, eod goto sd_done
                    
L60100:              FMT CH(23)
                    if str(sd_key$,1%,8%) <> str(c5$(i%),1%,8%) then goto sd_done
                    if str(sd_key$,9%,2%) <> str(c6$(i%),1%,2%) then goto sd_done
                                               /* (AWD001) - Change POS */
                    if str(sd_key$,12%,3%) <> "102" then goto check_sd_nxt

                        delete #6
                        goto check_sd_nxt

        sd_done
              gosub update_inv_sd
        return

        lookup_apcplndp
                                                           /* Only Set up DP */
                                                           /* If analysis=0  */
              if analysis% = 1% then goto dp_done
               pl_units = 1.000                              /* Set Defaults */
               pl_seq%  = 0 
               init(" ") dp_key$
               str(dp_key$,1%,3%) = "102"                    /* Stock Inventory */
               str(dp_key$,4%,2%) = "01"                     /* Process Code    */
               str(dp_key$,6%,2%) = "01"                     /* Shift Code      */
               
        check_dp_nxt
               read #7, key > dp_key$, using L63000, dp_key$, eod goto dp_done

L63000:               FMT POS(11), CH(12)
               if str(dp_key$,1%,3%) <> "102" then goto dp_done
               if str(dp_key$,4%,2%) <> "01"  then goto dp_done
               if str(dp_key$,6%,2%) <> "01"  then goto dp_done
                                                             /* Check Model Code */
               if str(dp_key$,8%,3%) <> str(c4$(i%),1%,3%) then goto check_dp_nxt
                  get #7, using L63010, pl_units, pl_seq%

L63010:               FMT POS(23), PD(14,4), XX(01), BI(1)                 
        dp_done

        return

        update_inv_sd
              if analysis% <> 0% then return
                                       /* Only Update APCPLNSD if */
                                       /* Analysis = 0            */
              for k% = 1% to 3%
                  convert k% to shift$, pic(00)
                  init(" ") sd_key$
                  str(sd_key$,1%,8%) = c5$(i%)
                  str(sd_key$,9%,2%) = c6$(i%)
                  str(sd_key$,11%,1%) = "9"         /* (AWD001) - supp dept */
                  str(sd_key$,12%,3%) = "102"
                  str(sd_key$,15%,2%) = "01"
                  str(sd_key$,17%,2%) = shift$
                  str(sd_key$,19%,3%) = str(c4$(i%),1%,3%)
                  str(sd_key$,22%,2%) = "01"


                  read #6, hold, key = sd_key$, eod goto no_sd
                             delete #6
              no_sd
                  write #6, using L63200, c5$(i%), c6$(i%), "9", "102", ~
                            "01", shift$, str(c4$(i%),1%,3%), "01",     ~
                             pl_units, pl_seq%, " "

              next k%

L63200:              FMT CH(8), CH(2), CH(1), CH(3), CH(02), CH(02), CH(03), ~
                         CH(02), PD(14,4), BI(1), CH(01)
        return


        update_apcplnsc
            
            init(" ") sc_key$
            str(sc_key$,1%,10%) = str(c5$(i%),1%,8%) & str(c6$(i%),1%,2%)
            read #3, hold, key = sc_key$, eod goto no_sc  

                  get #3, using L62300, sc_tqty%, sc_mqty%, sc_pqty%, sc_pqty1%
                  
                   inv_qty% = 0
                   convert c7$(i%) to inv_qty%, data goto no_sc

                                        /* If Analysis = 0 then take away */
                                        /* from Make Qty and add to pull  */
                                        /* Qty else do opposite           */

                   if analysis% = 0% then sc_mqty% = sc_mqty% - inv_qty%  ~
                      else sc_mqty% = sc_mqty% + inv_qty% 
                   if analysis% = 0% then sc_pqty%  = sc_pqty% + inv_qty% ~
                      else sc_pqty% = sc_pqty% - inv_qty%


            rewrite #3, using L62300, sc_tqty%, sc_mqty%, sc_pqty%, sc_pqty1%
L62300:                 FMT POS(68), BI(2), BI(2), BI(2), BI(2)


        no_sc
        return

        update_invquan
             init(" ") quan_key$    :   inv_qty  =  0.00
* PAR000
             str(quan_key$, 1%,25%)  = c4$(i%)  
             str(quan_key$,26%,20%)  = c9$(i%) 
             str(quan_key$,46%, 3%)  = "300"


             read #21, hold, key = quan_key$, eod goto no_hnyquan

                   get #21, using L62400, inv_qty


                                        /* If Analysis = 0 then take away */
                                        /* from Make Qty and add to pull  */
                                        /* Qty else do opposite           */

                   if analysis% = 0% then inv_qty = inv_qty + inv_qty%   ~
                      else inv_qty = inv_qty - inv_qty%

                  
                   put #21, using L62400, inv_qty

* PAR000
L62400:             FMT POS(129), PD(15,4)
                   rewrite #21
        no_hnyquan

        return


        update_header
            init(" ") or_key$
            or_key$ = str(c5$(i%),1%,8%)
            read #1,hold,key 4% = or_key$, eod goto no_header

                 get #1, using L62405, or_mak%, or_pul%


                                        /* If Analysis = 0 then take away */
                                        /* from Make Qty and add to pull  */
                                        /* Qty else do opposite           */

                 if analysis% = 0% then or_mak% = or_mak% - inv_qty% ~
                    else or_mak% = or_mak% + inv_qty%
                 if analysis% = 0% then or_pul% = or_pul% + inv_qty% ~
                    else or_pul% = or_pul% - inv_qty%

               rewrite #1, using L62405, or_mak%, or_pul%, eod goto L62415
L62405:                   FMT POS(123), BI(2), BI(2)
        no_header
        return
L62415:     errormsg$ = "(Error) - Updating Header (APCPLNOR) " &or_key$
            gosub error_prompt
            init(" ") errormsg$
            goto L62405




                                                          
        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                          

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        lookup_subpart                            /* PAR000  */
            init(" ") flag$, pgm$, bcksubpt_rec$, flds$(), info_flds$()
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
                   str(bcksubpt_rec$,48%,20%) = "00000"

            if err1% = 0% then return

            errormsg$ = "BCKSUBPT ERR= "&so_inv$ & " Line= " & item_no$  ~
                                      & " Flag= " & flag$


        return                                    /* PAR000 */


                            
        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end


