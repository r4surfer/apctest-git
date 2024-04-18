        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN66                             *~
            *  Creation Date     - 05/04/2018                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Ricky Beane                          *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - Utility to scan orders that are      *~
            *                      assigned to loads and painted.       *~
            *                      Allow the planner to change the      *~
            *                      customer (painter) on D orders.      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/04/18 ! New Program for (AWD) - Last Mod Date    ! RDB *~
            * 12/14/21 ! Allow date entry over year end           ! RDB *~
            *************************************************************
        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            blankdate$6,                 /* Null Date test             */~
            bg_due$6, bg_dte$10,         /* Beg/End S.O. Due Date      */~
            ed_due$6, ed_dte$10,         /*                            */~
            bg_load$5,                   /* Beg/End Load Number        */~
            ed_load$5,                   /*                            */~
            mvCust$9,                    /* Customer to move to        */~
            mvCustName$30,               /* Customer Name Display      */~
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
            or_status$2,                 /* Planning Status            */~
            or_customer$9,               /* APCPLNOR Customer          */~
            or_so$8,                     /* APCPLNOR Sales Order       */~
            or_load$8                    /* APCPLNOR Load              */

       
       dim bckline_key$19,               /* BCKLINES readkey           */~ 
           bckline_rec$(3%)100,          /* BCKLINES record            */~
           bckmstr_key$25,               /* BCKMASTER readkey          */~
           bckmstr_rec$(4%)250,          /* BCKMASTER record           */~
           bnkline_key$19,               /* BNKLINES readkey           */~
           bnkline_rec$(3%)100,          /* BNKLINES record            */~
           bnkmstr_key$25,               /* BNKMASTER readkey          */~
           bnkmstr_rec$(4%)250,          /* BNKMASTER record           */~
           trans_number$16,              /* BNKMASTER header trans #   */~
           record_type$1,                /* BNKMASTER header type      */~
           bdate$6,                      /* BNKMASTER date             */~
           btime$6,                      /* BNKMASTER time             */~
           bfiller$1,                    /* BNKMASTER filler           */~
           sc_key$33,                    /* APCPLNSC readkey #2        */~
           sc_rec$128,                   /* APCPLNSC record            */~
           or_key$51,                    /* APCPLNOR readkey           */~
           or_keyX$51,                   /* APCPLNOR record key        */~
           or_rec$170                    /* APCPLNOR record            */
           
        dim cu_key$9,                    /* CUSTOMER record key        */~
            cu_name$30,                  /* CUSTOMER sort name         */~ 
            cu_shp_name$30,              /* CUSTOMER ship-to name      */~
            cu_addr2$30,                 /* CUSTOMER address line      */~
            cu_addr3$30,                 /* CUSTOMER address line      */~
            cu_addr4$30,                 /* CUSTOMER address line      */~
            cu_addr5$30,                 /* CUSTOMER address line      */~
            cu_city$18,                  /* CUSTOMER city              */~
            cu_state$2,                  /* CUSTOMER state             */~ 
            cfiller$1,                   /* CUSTOMER filler column     */~
            cu_zip$9                     /* CUSTOMER zip               */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

        dim h1$3, h2$5, h3$9,            /* Screen Headers             */~
            h4$8, h5$6,                  /* Screen Headers             */~
            c1$(5000%)1,                 /* Include                    */~
            c2$(5000%)5,                 /* Load Number                */~
            c3$(5000%)9,                 /* Customer                   */~
            c4$(5000%)8,                 /* Sales Order Number         */~
            c5$(5000%)6                  /* Make Quantity              */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Scheduling Flex Paint Customer Utility "
            pname$ = "AWDPLN66"

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
            * #1  ! APCPLNSC ! Planning Master Schedule                 *~
            * #2  ! APCPLNOR ! Detail Analysis Sales Orders             *~
            * #3  ! BCKMASTR ! Back Log S.O. Header Master              *~
            * #4  ! BCKLINES ! Back Log Line Item Detail                *~
            * #5  ! BNKMASTR ! Sales Order Master- Headers              *~
            * #6  ! BNKLINES ! Sales Order Master- Detail               *~
            * #7  ! GENCODES ! Master Code Tables File                  *~
            * #8  ! CUSTOMER ! Customer Master                          *~
            * #9  ! APCPLNLD ! Load Master File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33

            select #2, "APCPLNOR",                                       ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

            select #3,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #4,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19,                     ~
                        alt key 1, keypos = 14, keylen = 19

            select #5, "BNKMASTR",                                       ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =   16,                    ~
                        alt key  1, keypos =   18, keylen =  12, dup,    ~
                            key  2, keypos =   31, keylen =  25, dup,    ~
                            key  3, keypos =   17, keylen =  39, dup

            select #6, "BNKLINES",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =   19,                    ~
                        alt key  1, keypos =   46, keylen =  19, dup
                        
            select #7,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #8,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos = 1,    keylen =  9,                      ~
                        alt key 1, keypos = 10, keylen = 30, dup,        ~
                            key 2, keypos =424, keylen =  9, dup,        ~
                            key 3, keypos =771, keylen =  9, dup,        ~
                            key 4, keypos =780, keylen =  9, dup,        ~
                            key 5, keypos = 1049, keylen = 9, dup

            select #9,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15

            call "SHOSTAT" ("Initialization")

            filename$ = "APCPLNSC" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BNKMASTR" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BNKLINES" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#9, filename$, err%)
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
            h4$ = "<-S.O.->"
            h5$ = "QTY"

            init(" ") schema$ 
            schema%, s_err% = 0%
            call "SCHEMA" (schema$, schema%, #7, s_err%)
            
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 3%
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
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L11150:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
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

L19680:     init(" ") cnt$, sc_key$, c1$(), c2$(), c3$(),    ~
                      c4$(), c6$(), c5$(), rslt$()
            drop_seq_max% = 0%                 /* Last Drop Seq Loaded */
            cnt% = 0%
            cnt$ = "Records Scanned [ xxxxxxxx ]"

            val_max%     = 0%  : cc%            = 0%
            gosub scan_load

            gosub'102(1%, 1%)                   /* Select Sales Orders */
            keyhit% = 0%
            
            call "ALLFREE"

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
         "Enter a Valid Customer to Move Production?                   "


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
                      c1$(), c2$(),  hdr$, msg$(), c3$(), c4$(),  ~
                      c5$(), c6$(), ed_due$, ed_dte$,             ~
                      bg_load$, ed_load$, mvCust$, mvCustName$
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
                                L40200          /* Chg To Customer      */

              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
/*                lfac$(fieldnr%) = hex(82)  :  return     Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Begin S.O. Due Date  :",                     ~
               at (03,25), fac(lfac$(1%)), bg_dte$              , ch(10),~
                                                                         ~
               at (03,40), "Ending S.O. Due Date :",                     ~
               at (03,63), fac(lfac$(1%)), ed_dte$              , ch(10),~
                                                                         ~
               at (04,02), "Beginning Load       :",                     ~
               at (04,25), fac(lfac$(2%)), bg_load$             , ch(05),~
                                                                         ~
               at (04,40), "Ending Load          :",                     ~
               at (04,63), fac(lfac$(2%)), ed_load$             , ch(05),~
                                                                         ~
               at (05,02), "Customer to move to  :",                     ~
               at (05,25), fac(lfac$(3%)), mvCust$              , ch(09),~
               at (05,36), fac(hex(8C)), mvCustName$            , ch(30),~
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
                      "(8)Order Analysis                      "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffff08ffffffffff0e0f1000)
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
               at (03,20), fac(hex(84)), bg_dte$                , ch(10),~
               at (03,31), fac(hex(84)), ed_dte$                , ch(10),~
                                                                         ~
               at (04,02), "Beg/End Load    :",                          ~
               at (04,20), fac(hex(84)), bg_load$               , ch(05),~
               at (04,31), fac(hex(84)), ed_load$               , ch(05),~
                                                                         ~
               at (05,02), "Customer to move to :",                      ~
               at (05,24), fac(hex(84)), mvCust$                , ch(09),~
               at (05,36), fac(hex(8C)), mvCustName$            , ch(30),~
                                                                         ~               
               at (08,12), fac(hex(a4)), h1$                    , ch(03),~
               at (08,18), fac(hex(a4)), h2$                    , ch(05),~
               at (08,26), fac(hex(a4)), h3$                    , ch(09),~
               at (08,38), fac(hex(a4)), h4$                    , ch(08),~
               at (08,50), fac(hex(a4)), h5$                    , ch(06),~
                                                                         ~
               at (09,13), fac(lfac$(1%)), c1$( 1% + kk%)       , ch(01),~
               at (09,18), fac(hex(84)),   c2$( 1% + kk%)       , ch(05),~
               at (09,26), fac(hex(84)),   c3$( 1% + kk%)       , ch(09),~
               at (09,38), fac(hex(84)),   c4$( 1% + kk%)       , ch(08),~
               at (09,50), fac(hex(84)),   c5$( 1% + kk%)       , ch(06),~
                                                                         ~
               at (10,13), fac(lfac$(1%)), c1$( 2% + kk%)       , ch(01),~
               at (10,18), fac(hex(84)),   c2$( 2% + kk%)       , ch(05),~
               at (10,26), fac(hex(84)),   c3$( 2% + kk%)       , ch(09),~
               at (10,38), fac(hex(84)),   c4$( 2% + kk%)       , ch(08),~
               at (10,50), fac(hex(84)),   c5$( 2% + kk%)       , ch(06),~
                                                                         ~
               at (11,13), fac(lfac$(1%)), c1$( 3% + kk%)       , ch(01),~
               at (11,18), fac(hex(84)),   c2$( 3% + kk%)       , ch(05),~
               at (11,26), fac(hex(84)),   c3$( 3% + kk%)       , ch(09),~
               at (11,38), fac(hex(84)),   c4$( 3% + kk%)       , ch(08),~
               at (11,50), fac(hex(84)),   c5$( 3% + kk%)       , ch(06),~
                                                                         ~
               at (12,13), fac(lfac$(1%)), c1$( 4% + kk%)       , ch(01),~
               at (12,18), fac(hex(84)),   c2$( 4% + kk%)       , ch(05),~
               at (12,26), fac(hex(84)),   c3$( 4% + kk%)       , ch(09),~
               at (12,38), fac(hex(84)),   c4$( 4% + kk%)       , ch(08),~
               at (12,50), fac(hex(84)),   c5$( 4% + kk%)       , ch(06),~
                                                                         ~
               at (13,13), fac(lfac$(1%)), c1$( 5% + kk%)       , ch(01),~
               at (13,18), fac(hex(84)),   c2$( 5% + kk%)       , ch(05),~
               at (13,26), fac(hex(84)),   c3$( 5% + kk%)       , ch(09),~
               at (13,38), fac(hex(84)),   c4$( 5% + kk%)       , ch(08),~
               at (13,50), fac(hex(84)),   c5$( 5% + kk%)       , ch(06),~
                                                                         ~
               at (14,13), fac(lfac$(1%)), c1$( 6% + kk%)       , ch(01),~
               at (14,18), fac(hex(84)),   c2$( 6% + kk%)       , ch(05),~
               at (14,26), fac(hex(84)),   c3$( 6% + kk%)       , ch(09),~
               at (14,38), fac(hex(84)),   c4$( 6% + kk%)       , ch(08),~
               at (14,50), fac(hex(84)),   c5$( 6% + kk%)       , ch(06),~
                                                                         ~
               at (15,13), fac(lfac$(1%)), c1$( 7% + kk%)       , ch(01),~
               at (15,18), fac(hex(84)),   c2$( 7% + kk%)       , ch(05),~
               at (15,26), fac(hex(84)),   c3$( 7% + kk%)       , ch(09),~
               at (15,38), fac(hex(84)),   c4$( 7% + kk%)       , ch(08),~
               at (15,50), fac(hex(84)),   c5$( 7% + kk%)       , ch(06),~
                                                                         ~
               at (16,13), fac(lfac$(1%)), c1$( 8% + kk%)       , ch(01),~
               at (16,18), fac(hex(84)),   c2$( 8% + kk%)       , ch(05),~
               at (16,26), fac(hex(84)),   c3$( 8% + kk%)       , ch(09),~
               at (16,38), fac(hex(84)),   c4$( 8% + kk%)       , ch(08),~
               at (16,50), fac(hex(84)),   c5$( 8% + kk%)       , ch(06),~
                                                                         ~
               at (17,13), fac(lfac$(1%)), c1$( 9% + kk%)       , ch(01),~
               at (17,18), fac(hex(84)),   c2$( 9% + kk%)       , ch(05),~
               at (17,26), fac(hex(84)),   c3$( 9% + kk%)       , ch(09),~
               at (17,38), fac(hex(84)),   c4$( 9% + kk%)       , ch(08),~
               at (17,50), fac(hex(84)),   c5$( 9% + kk%)       , ch(06),~
                                                                         ~
               at (18,13), fac(lfac$(1%)), c1$(10% + kk%)       , ch(01),~
               at (18,18), fac(hex(84)),   c2$(10% + kk%)       , ch(05),~
               at (18,26), fac(hex(84)),   c3$(10% + kk%)       , ch(09),~
               at (18,38), fac(hex(84)),   c4$(10% + kk%)       , ch(08),~
               at (18,50), fac(hex(84)),   c5$(10% + kk%)       , ch(06),~
                                                                         ~
               at (19,56), fac(hex(84)),   pg_tot_unit$         , ch(06),~
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
L43045:        if keyhit% <> 9%  then goto L43085
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
                  "Paint Scheduling S.O. Analysis - Display"             

            page$ ="Page: xxx of xxx"
            x1% = (kk% / 10%) + 1%
            x2% = (val_max% / 10%) + 1%
            if x2% < 1% then x2% = 1%
            convert x1% to str(page$,7%,3%), pic(###)
            convert x2% to str(page$,14%,3%), pic(###)

            pg_tot_unit = 0.0 
            for n% = 1% to 10%
                y1 = 0% 
                convert c5$(n% + kk%) to y1, data goto L43305
L43305:
                pg_tot_unit  = pg_tot_unit + y1
            next n%
            convert pg_tot_unit to pg_tot_unit$, pic(###.##)

            pf$(1%) = "(1)Start Over  (4)Previous              " &       ~
                      "                                       "
            pf$(2%) = "(2)First       (5)Next      (9)Chg S.O. " &       ~
                      "                                       "
            pf$(3%) = "(3)Last                                 " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(0102030405ffffff09ffffffffffff1000)
            gosub check_screen
        return

        check_screen

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
                              L50200,         /* Beg/End Load          */ ~
                              L50400          /* Move To Customer      */ 
            return

L50000: REM Beginning Due/Delivery Date           BG_DUE$, BG_DTE$
            if bg_dte$ <> " " then goto L50010
               bg_dte$ = date$

L50010:     date% = 0%
            call "DATEOKC" (bg_dte$, date%, errormsg$)
            if date% = 0% then goto date_error
            x$ = bg_dte$
            call "DATUFMTC"(x$)
            bg_due$ = str(x$,1%,6%)
        REM Ending Due/Delivery Date              ED_DUE$, ED_DTE$
            if len(ed_dte$) < 6 then ed_dte$ = bg_dte$
            date% = 0%
            call "DATEOKC" (ed_dte$, date%, errormsg$)
            if date% = 0% then goto date_error
            y$ = ed_dte$
            call "DATUFMTC"(y$)
            ed_due$ = str(y$,1%,6%)
/* CR2731 Change if condition to use format date validation  */
REM            if bg_dte$ > ed_dte$ then goto L50090
            if x$ > y$ then goto L50090
        return
L50090:     errormsg$ = "(Error) - Invalid Due Date Range??"
        date_error
            gosub error_prompt
            init(" ") bg_due$, bg_dte$, ed_due$, ed_dte$
        return

L50200: REM Beginning Due/Delivery Date           BG_LOAD$, BG_LOAD$
            if str(bg_load$,1%,5%) <> " " then goto L50210
               bg_load$ = "ALL"

L50210:     if str(bg_load$,1%,3%) = "ALL" then goto end_load
               init(" ") ld_key$ : ld_key$ = bg_load$
               gosub check_load

               if str(ed_load$,1,3) = "ALL" then ed_load$ = bg_load$

        end_load
            if str(ed_load$,1%,5%) <> " " then goto L50220
               ed_load$ = bg_load$


L50220:     if str(ed_load$,1%,3%) = "ALL" then return
               init(" ") ld_key$ : ld_key$ = ed_load$
               gosub check_load
               if bg_load$ > ed_load$ then goto L50250
        return
L50250:     errormsg$ = "(Error) - Invalid Load Range??"
            gosub error_prompt
            init(" ") bg_load$, ed_load$
        return


        check_load
            read #9, key = ld_key$, eod goto load_error
        return
        load_error
          errormsg$="(Error) - Invalid Load Number, Not Defined?"
          gosub error_prompt
          init(" ") ld_key$, bg_load$, ed_load$
        return
        
L50400: REM Move Customer

          if mvCust$ = " " then goto cust_error
          read #8, key = mvCust$, using L50450, mvCustName$, eod goto cust_error
L50450:              FMT POS(10), CH(30)        
        return

        cust_error        
          errormsg$="(Error) - Invalid Customer?"
          gosub error_prompt
          init(" ") mvCust$
        return

        REM *************************************************************~
            * Load sales orders to the screen                           *~
            *************************************************************
        scan_load
            init(" ") or_rec$, or_key$, or_status$, or_customer$, or_load$ 
             or_mak% = 0%            
                  
            or_key$ = all(hex(00))  
            str(or_key$,1%,6%) = bg_due$            
               
        scan_load_nxt
          read #2, key > or_key$, using L61125, or_keyX$, or_so$,        ~
                         or_status$, or_load$, or_mak%,                  ~   
                              eod goto scan_load_done
                        
L61125:         FMT CH(51), CH(08), CH(02), POS(94), CH(05),  ~
                    POS(123), BI(2)   
                    
          or_key$ = or_keyX$          
          cnt% = cnt% + 1%
          if mod(cnt%,100%) <> 0% then goto L61155
             convert cnt% to str(cnt$,19%,8%), pic(########)
             print at(02,02);hex(84);cnt$;

L61155: 
          or_customer$ = str(or_keyX$,27%,9%)
          or_key$ = or_keyX$
          
          if str(or_key$,1%,6%) > ed_due$ then goto scan_load_done
          if str(bg_load$,1%,3%) = "ALL" then goto L61190
             if or_load$ < bg_load$ or                         ~
                or_load$ > ed_load$ then goto scan_load_nxt
         
L61190:   if or_status$ <> "02" then goto scan_load_nxt
          if str(or_so$, 1%, 1%) <> "D" then goto scan_load_nxt

           if or_mak% < 1% then goto scan_load_nxt

           gosub scan_order_screen

           goto scan_load_nxt
             
        scan_load_done
           val_max% = cc%
           kk% = 0%
        return

        scan_order_screen
          gosub load_screen
        return

        load_screen
             cc% = cc% + 1%
             c1$(cc%) = " "

             c2$(cc%) = or_load$                         /* Load Number*/
             c3$(cc%) = or_customer$                     /* Cust  Code */

             c4$(cc%) = or_so$                           /* So Number  */

             convert or_mak% to c5$(cc%), pic(#####)

        return

        process_data
            call "SHOSTAT" ("Processing Customer Switch")
            cnt% = drop_seq_max%

            for i% = 1% to val_max%
            
                if c1$(i%) = " " then goto L62200
                gosub update_all_files

L62200:     next i%
        return

        REM *************************************************************~
            *  Process customer change for BCKMASTER, BCKLINES, BNKLINES*~
            *  BNKMASTER, APCPLNOR, APCPLNSC                            *~
            *           Driver file APCPLNOR                            *~
            ************************************************************* 
        update_all_files
              init(" ") or_key$, or_rec$
              str(or_key$,1%,8%) = str(c4$(i%),1%,8%) 

/*      process_change_customer           */
              read #2, key 4% = or_key$, using L63100, or_rec$,  ~
                        eod goto or_done

L63100:              FMT CH(170)

              if str(or_rec$,52%,8%) <> str(c4$(i%),1%,8%) then goto or_done
                    
              gosub read_cust
              gosub update_bcklines
              gosub update_bckmastr
              gosub update_bnklines
              gosub update_bnkmastr
              gosub update_apcplnsc
         
              read #2, hold, key 4% = or_key$, using L63100, or_rec$,  ~
                        eod goto or_done
              delete #2
              str(or_rec$,27%,9%) = mvCust$
              put #2, using L63100, or_rec$
              write #2
              
        or_done
        return

        REM *************************************************************~
            * Process all sales orders in BCKLINES                      *~
            *************************************************************
        update_bcklines
            init(" ") bckline_rec$()
            bnkmstr_key$ = hex(00) 
            
            str(bckline_key$,1%,8%) = str(c4$(i%),1%,8%)
            str(bckline_key$,17%,3%) = "  1"
      
            read #4, hold, key >= bckline_key$, using L70100, bckline_rec$(), ~
                 eod goto bckline_done
            
            goto bckline1rec
            
        bckline_nxt
            read #4, hold, using L70100, bckline_rec$(), eod goto bckline_done

L70100:               FMT 3*CH(100) 

bckline1rec:
            if str(bckline_rec$(1%),10%,8%) <> str(c4$(i%),1%,8%) then  ~
                  goto bckline_done

            delete #4
            str(bckline_rec$(1%),1%,9%) = mvCust$
            put #4, using L70100, bckline_rec$()
            write #4
                                
            goto bckline_nxt            

        bckline_done
        return
        
        REM *************************************************************~
            * Process all sales orders in BCKMASTR                      *~
            *************************************************************
        update_bckmastr
            init(" ") bckmstr_key$, bckmstr_rec$()
            
            str(bckmstr_key$,1%,9%)   = str(or_rec$,27%,9%)
            str(bckmstr_key$,10%,8%)  = str(c4$(i%),1%,8%)
            
            read #3, hold, key = bckmstr_key$, using L80100, bckmstr_rec$(), ~
                 eod goto bckmstr_done

L80100:        FMT 4*CH(250)   

            if str(bckmstr_rec$(1%),1%,9%)  <> str(or_rec$,27%,9%) then  ~
                  goto bckmstr_done
            if str(bckmstr_rec$(1%),10%,8%) <> str(c4$(i%),1%,8%) then  ~
                  goto bckmstr_done
            
            delete #3
            str(bckmstr_rec$(1%),1%,9%) = mvCust$
                        
            str(bckmstr_rec$(1%),42%,30%)  = cu_name$
            str(bckmstr_rec$(1%),72%,30%)  = cu_addr2$
            str(bckmstr_rec$(1%),102%,30%) = cu_addr3$            
            str(bckmstr_rec$(1%),132%,30%) = cu_addr4$
            str(bckmstr_rec$(1%),162%,30%) = cu_addr5$
            str(bckmstr_rec$(1%),192%,18%) = cu_city$ 
            str(bckmstr_rec$(1%),210%,2%)  = cu_state$
            str(bckmstr_rec$(1%),212%,1%)  = " "           
            str(bckmstr_rec$(1%),213%,9%)  = cu_zip$
            
            put #3, using L80100, bckmstr_rec$()
            write #3

        bckmstr_done
        return
        
        REM *************************************************************~
            * Process all sales orders in BNKLINES                      *~
            *************************************************************
        update_bnklines
            init(" ") bnkline_rec$()
            bnkline_key$ = hex(00)            
 
            str(bnkline_key$,1%,8%) = str(c4$(i%),1%,8%)
            str(bnkline_key$,17%,3%) = "  1"
            
            read #6, hold, key 1% >= bnkline_key$, using L90100, ~
                  bnkline_rec$(),    eod goto bnkline_done
              goto bnkline1rec
       
        bnkline_nxt
             read #6, hold, using L90100, bnkline_rec$(), eod goto bnkline_done
             
L90100:              FMT 3*CH(100) 
  
bnkline1rec:  
            if str(bnkline_rec$(1%),46%,8%) <> str(c4$(i%),1%,8%) then  ~
                  goto bnkline_done
            
            delete #6
            str(bnkline_rec$(1%),37%,9%) = mvCust$
            put #6, using L90100, bnkline_rec$()
            write #6
           
            goto bnkline_nxt  
            
        bnkline_done
        return
                
        REM *************************************************************~
            * Process all sales orders in BNKMASTR                      *~
            *************************************************************
        update_bnkmastr
            init(" ") bnkmstr_rec$(), trans_number$, ~
                      record_type$, bdate$, btime$, bfiller$
            bnkmstr_key$ = hex(00)    
            
            str(bnkmstr_key$,1%,9%)  = str(or_rec$,27%,9%)
            str(bnkmstr_key$,10%,8%) = str(c4$(i%),1%,8%)
            
            read #5, hold, key 2% >= bnkmstr_key$, using L91100,  ~            
                       trans_number$,                      ~
                       record_type$,                       ~
                       bdate$,                             ~
                       btime$,                             ~
                       bfiller$,                           ~
                       bnkmstr_rec$(),                     ~
                        eod goto bnkmstr_done
              goto bnkmstr1rec
              
        bnkmstr_nxt
            read #5, hold, using L91100,                   ~            
                       trans_number$,                      ~
                       record_type$,                       ~
                       bdate$,                             ~
                       btime$,                             ~
                       bfiller$,                           ~
                       bnkmstr_rec$(),                     ~
                        eod goto bnkmstr_done

L91100:              FMT CH(16), CH(1), CH(6), CH(6), CH(1), 4*CH(250)

bnkmstr1rec: 
            if str(bnkmstr_rec$(1%),1%,8%)  <> str(or_rec$,27%,9%) then  ~
                  goto bnkmstr_done
            if str(bnkmstr_rec$(1%),10%,9%) <>  str(c4$(i%),1%,8%) then  ~
                  goto bnkmstr_done
            
            delete #5
           
            str(bnkmstr_rec$(1%),1%,9%) = mvCust$
            
            str(bnkmstr_rec$(1%),42%,30%) = cu_name$
            str(bnkmstr_rec$(1%),72%,30%) = cu_addr2$
            str(bnkmstr_rec$(1%),102%,30%) = cu_addr3$            
            str(bnkmstr_rec$(1%),132%,30%) = cu_addr4$
            str(bnkmstr_rec$(1%),162%,30%) = cu_addr5$
            str(bnkmstr_rec$(1%),192%,18%) = cu_city$ 
            str(bnkmstr_rec$(1%),210%,2%)  = cu_state$
            str(bnkmstr_rec$(1%),212%,1%)  = " "           
            str(bnkmstr_rec$(1%),213%,9%)  = cu_zip$

            put #5, using L91100, trans_number$,                      ~
                                  record_type$,                       ~
                                  bdate$,                             ~
                                  btime$,                             ~
                                  bfiller$,                           ~
                                  bnkmstr_rec$()
                                  
            write #5 
            
            goto bnkmstr_nxt
            
        bnkmstr_done            
        return
        
        REM *************************************************************~
            * Process all sales orders in APCPLNSC                      *~
            *************************************************************  
        update_apcplnsc
            init(" ") sc_key$, sc_rec$
            str(sc_key$,1%,8%) = str(c4$(i%),1%,8%)
             
        sc_nxt
            read #1, hold, key > sc_key$, using L92100, sc_rec$, ~
                 eod goto sc_done

L92100:              FMT CH(128) 
   
            if str(sc_rec$,24%,8%) <> str(c4$(i%),1%,8%) then  ~
                  goto sc_done
            
            delete #1
            str(sc_rec$,59%,9%) = mvCust$
            put #1, using L90100, sc_rec$
            write #1
           
            sc_key$ = str(sc_rec$,24%,19%)
            goto sc_nxt  
            
        sc_done
        return

        REM *************************************************************~
            *  Retrieve the CUSOMTER information for updates            *~
            *************************************************************         
        read_cust
             init(" ") cu_key$, cu_name$, cu_shp_name$, cu_addr2$, cu_addr3$, ~
                       cu_addr4$, cu_addr5$, cu_city$, cu_state$, cu_zip$
             cust_flag% = 0%
                       
              cu_key$ = mvCust$
              read #8, key = cu_key$, using L93000,  cu_name$, cu_shp_name$, ~
                           cu_addr2$, cu_addr3$, cu_addr4$, cu_addr5$,       ~
                           cu_city$, cu_state$, cfiller$, cu_zip$,           ~
                              eod goto cust_done
                           
L93000:          FMT POS(10), CH(30), POS(253), CH(30), 4*CH(30), CH(18),  ~ 
                     CH(02), CH(01), CH(09)

                cust_flag% = 1%
                
        cust_done
        return        
            
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************         
            
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

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")
            close #1 : close #2 : close #3 : close #4 : close #5  
            close #6 : close #7 : close #8 : close #9
            end
            