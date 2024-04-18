        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN63                             *~
            *  Creation Date     - 07/06/07                             *~
            *  Last Modified Date- 02/11/2016                           *~
            *  Written By        - Christie Gregory                     *~
            *  Last Modified By  - Christie Norman                      *~
            *                                                           *~
            *  Description       - Utility to scan orders that are      *~
            *                      assigned to loads but are not        *~
            *                      planned with of certain models       *~
            *                      plan to different department         *~
            *                      215/413                              *~
            *                      (1) Detail Analysis by S.O.          *~
            *                                                           *~
            *  Code Tables Used  - (         ), (         ), (         )*~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/06/07 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            * 05/27/08 ! (AWD001) mod for cottage/oriel           ! CMG *~
            * 08/06/08 ! (AWD002) mod for width and height limits ! CMG *~
            *04/14/2010! (AWD003) mod to move to dept 005 267C    ! CMG *~
            *02/09/2012! (AWD004) mod to add dept 027 & 036 for   ! CMG *~
            *          !   BMSH                                   !     *~
            *03/01/2012! (AWD005) mod to change primary dept from ! CMG *~
            *          !    dpt 007 to 005 for 267                !     *~
            *04/05/2013! (AWD006) mod for TX flex and schema      ! CMG *~
            *03/23/2015! (AWD007) mod to exclude depts over 100   ! CMG *~
            *07/06/2015! SR66799  mod to exclude support depts    ! PWW *~
            *01/01/2016! (SR67154) mods for NFRC 2016             ! CMN *~
            *02/11/2016! (SR72711) mod to exclue low e 272        ! CMN *~
            *09/13/2017! (CR1111) add color parameter             ! RDB *~
            *01/19/2018! (CR1261) mod for Sill display and dpt 012! RDB *~
            *02/23/2018! (CR1111) add NC part/FGO check to TX     ! RDB *~
            *05/03/2018! (CR1488) do not display flagged orders   ! RDB *~
            *06/27/2018! (CR1573) filter on sash deliminter opt 2 ! RDB *~ 
            *01/02/2019! (CR1846) filter mull opt A out of NC list! RDB *~
            *01/11/2019! (CR1863) Flex TX dept 29 to 70           ! RDB *~
            *04/12/2019!          Flex TX dept 71 to 29           ! RDB *~
            *06/06/2019! (CR2063) Add Screen code 7 to not allow  ! RDB *~
            *10/28/2019! CR2308 Flexscreen model 267 do not flex  ! RDB *~
            *02/28/2020! CR2442 Add ALLFREE call to program       ! RDB *~
            *03/15/2021! CR9999 Add models for sliders and PW     ! RDB *~
            *07/14/2021! CR2882 New dept 053 flex 511 NC          ! RDB *~
            *10/20/2021! CR2802 150 black NC                      ! RDB *~
            *10/26/2021! CR2948 Option for stock only             ! RDB *~
            *10/29/2021! CR2756 eCat user excludes from stock so  ! RDB *~
            *02/10/2022! CR3020 Flex TX 511/551 from 015 to 016   ! RDB *~
            *02/14/2022! CR3023 NC only move model 267 to 007     ! RDB *~
            *03/18/2022! CR3058 NC 511 allow foam, 267 allow gls  ! RDB *~
			*04/21/2022! CR3077 Add Flex Dept/Model Validation    ! RDB *~
			*08/02/2022! CR3145 Restrict dept 007 model 267 size  ! RDB *~
			*08/23/2022! CR3160 Remove model option ALL           ! RDB *~
			*                   All NC Paint like white color     ! RDB *~
            *************************************************************
  

        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            blankdate$6,                 /* Null Date test             */~
            bg_due$10, bg_dte$10,        /* Beg/End S.O. Due Date      */~
            ed_due$10, ed_dte$10,        /*                            */~
            bg_load$5,                   /* Beg/End Load Number        */~
            ed_load$5,                   /*                            */~
            bg_model$3, ed_model$3,      /* Beg/End Model  Codes       */~
            mvDept$3,                    /* Dept to move to  (AWD003)  */~
            chk_clr$1, clr_desc$30,      /* Color  (CR1111)            */~
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
            hg$2,                        /* Hinge Code    (AWD001)     */~
            gl$2,                        /* Glass Code    (SR67154)    */~
            width$4,                     /* Width         (AWD002)     */~
            height$3,                    /* Height        (AWD002)     */~
            sc_customer$9,               /* APCPLNSC Customer          */~
            sc_so$8,                     /* APCPLNSC Sales Order       */~
            sc_ln$2,                     /* APCPLNSC Line Item         */~
            sd_key$23,                   /* APCPLNSD readkey           */~
            sd_rec$64                    /* APCPLNSD record            */
/* CR2948 */            
        dim bck_key$19,                  /* BCKLINES key               */~
            sku$10,                      /* SKU number                 */~
            sku_key$16,                  /* Sku Key                    */~            
            chk_stk$1,                   /* Check Stock Flag Y/N       */~
            bckm_key$25,                 /* BCKMASTR key  CR2756       */~ 
            bck_user_entered$3           /* eCat user entry user check */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

        dim h1$3, h2$5, h3$9, h4$25,     /* Screen Headers             */~
            h5$8, h6$2, h7$6, h8$6,      /* Screen Headers             */~
            h9$3,                        /* Screen Headers   CR1261    */~
            c1$(5000%)1,                 /* Include/Exclude            */~
            c2$(5000%)5,                 /* Load Number                */~
            c3$(5000%)9,                 /* Customer                   */~
            c4$(5000%)25,                /* Part Number                */~
            c5$(5000%)8,                 /* Sales Order Number         */~
            c6$(5000%)2,                 /* Sales Order Line           */~
            c7$(5000%)6,                 /* Make Quantity              */~
            c8$(5000%)10,                /* SubPart                    */~
            c9$(5000%)3                  /* Flange/Fin subpart         */

        dim                              /* PAR000                     */~
            subp$20,                     /* Subpart number             */~
            foam$1,                      /* Foam Subpart  (SR67154)    */~
            spacer$1,                    /* Spacer        (SR67154)    */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            sash$1,                      /* Subpart Sash Accessory 16  */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */
  
           
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Scheduling Flex Analysis Utility "
            pname$ = "AWDPLN63"

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
            * #2  ! GENCODES ! Master Code Tables File                  *~
            * #3  ! APCPLNSC ! Planning Master Schedule File            *~
            * #4  ! CUSTOMER ! Customer Master Schedule File            *~
            * #5  ! APCPLNLD ! Load Master File                         *~
            * #6  ! APCPLNSD ! Scheduling Department Detail             *~
            * #7  ! BCKLINES ! Back Log Line Item File                  *~
            * #8  ! AWDSKUXR ! sku x-ref file                           *~
            * #9  ! BCKMASTR ! Backlog master file                      *~            
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


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



            select #6,  "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =  1,   keylen = 23


            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup
                           
            select #7,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #8,   "AWDSKUXR",                                   ~
                        varc,     indexed,  recsize = 256,              ~
                        keypos =    1, keylen =  16,                    ~
                        alt key  1, keypos =  17, keylen =  20,         ~
                            key  2, keypos =  37, keylen =  45, dup
                            
/*CR2756 */    
            select #9,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup                            
                                                        
            call "SHOSTAT" ("Initialization")

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
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error
/* CR2948 */            
            filename$ = "BCKLINES" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDSKUXR" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error            
/* CR2756*/ filename$ = "BCKMASTR" : call "EWDOPEN" (#9, filename$, err%)
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
            h9$ = "SIL"

            init(" ") schema$ 
            schema%, s_err% = 0%
            call "SCHEMA" (schema$, schema%, #2, s_err%)
            if schema% = 5% then schema% = 2%        /* CR1863 */
            
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 7%
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
L11150:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 7% then editpg1
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

L19680:     init(" ") cnt$, sc_key$, c1$(), c2$(), c3$(), c4$(),    ~
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
         "Enter a Valid Beginning/Ending Model Code?                   ",~
         "Enter a Valid Department to Move Production?                 ",~
         "Enter a Valid Color Code or 0 (zero) for all colors          ",~
         "Enter a Y or N for Sash Limiter                              ",~         
         "Enter a Y or N for Lowes Stock Orders only                   "

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
                      mvDept$, gl$, chk_clr$, clr_desc$, chk_shl$
            analysis% = 0%                       /* Set Default to S.O.*/
            chk_shl$ = "N"                       /* Set default CR1573  */
            chk_stk$ = "N"                       /* Set default CR2948  */
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
                                L40200,         /* Beg/End Model Code   */~
                                L40250,         /* Move Dept (AWD003)   */~
                                L40200,         /* Color (CR1111)       */~
                                L40200,         /* Sash Limiter Y/N     */~
                                L40200          /* Stock option Y/N     */
              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40250:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

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
               at (06,02), "Dept to move to      :",                     ~
               at (06,25), fac(lfac$(4%)), mvDept$              , ch(03),~
/* (CR1111) */                                                           ~  
               at (07,02), "Color                :",                     ~
               at (07,25), fac(lfac$(5%)), chk_clr$             , ch(01),~
               at (07,28), fac(hex(a4)), clr_desc$              , ch(30),~
                                                                         ~
/* CR1573 */                                                             ~
               at (08,02), "Sash Limiter  (Y/N)  :",                     ~
               at (08,25), fac(lfac$(6%)), chk_shl$             , ch(01),~
/* CR2948 */                                                             ~
               at (09,02), "Lowes Stock only(Y/N):",                     ~
               at (09,25), fac(lfac$(7%)), chk_stk$             , ch(01),~   
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
REM  call "ALLFREE"
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
                      "(9)Assigned Order Analysis             "
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
/* (AWD003) */                                                           ~
               at (06,02), "Dept to move to :",                          ~
               at (06,20), fac(hex(84)), mvDept$                , ch(03),~
/* (CR1111) */                                                           ~
               at (07,02), "Color           :",                          ~
               at (07,20), fac(hex(84)), chk_clr$               , ch(01),~
               at (07,28), fac(hex(a4)), clr_desc$              , ch(30),~
/* CR1573 */                                                             ~
               at (08,02), "Sash Limiter    :",                          ~
               at (08,20), fac(hex(84)), chk_shl$               , ch(01),~
/* CR2948 */                                                             ~
               at (08,40), "Lowes Stock Order:",                         ~
               at (08,60), fac(hex(84)), chk_stk$               , ch(01),~      		   
                                                                         ~                                                                       
/* (AWD003) */                                                           ~               
               at (09,02), fac(hex(a4)), h1$                    , ch(03),~
               at (09,06), fac(hex(a4)), h2$                    , ch(05),~
               at (09,12), fac(hex(a4)), h3$                    , ch(09),~
               at (09,22), fac(hex(a4)), h4$                    , ch(25),~
               at (09,48), fac(hex(a4)), h5$                    , ch(08),~
               at (09,57), fac(hex(a4)), h6$                    , ch(02),~
               at (09,60), fac(hex(a4)), h7$                    , ch(06),~
               at (09,67), fac(hex(a4)), h8$                    , ch(06),~
               at (09,78), fac(hex(a4)), h9$                    , ch(03),~
                                                                         ~
               at (10,03), fac(lfac$(1%)), c1$( 1% + kk%)       , ch(01),~
               at (10,06), fac(hex(84)),   c2$( 1% + kk%)       , ch(05),~
               at (10,12), fac(hex(84)),   c3$( 1% + kk%)       , ch(09),~
               at (10,22), fac(hex(84)),   c4$( 1% + kk%)       , ch(25),~
               at (10,48), fac(hex(84)),   c5$( 1% + kk%)       , ch(08),~
               at (10,57), fac(hex(84)),   c6$( 1% + kk%)       , ch(02),~
               at (10,60), fac(hex(84)),   c7$( 1% + kk%)       , ch(06),~
               at (10,67), fac(hex(84)),   c8$( 1% + kk%)       , ch(10),~
               at (10,78), fac(hex(84)),   c9$( 1% + kk%)       , ch(03),~
                                                                         ~
               at (11,03), fac(lfac$(1%)), c1$( 2% + kk%)       , ch(01),~
               at (11,06), fac(hex(84)),   c2$( 2% + kk%)       , ch(05),~
               at (11,12), fac(hex(84)),   c3$( 2% + kk%)       , ch(09),~
               at (11,22), fac(hex(84)),   c4$( 2% + kk%)       , ch(25),~
               at (11,48), fac(hex(84)),   c5$( 2% + kk%)       , ch(08),~
               at (11,57), fac(hex(84)),   c6$( 2% + kk%)       , ch(02),~
               at (11,60), fac(hex(84)),   c7$( 2% + kk%)       , ch(06),~
               at (11,67), fac(hex(84)),   c8$( 2% + kk%)       , ch(10),~
               at (11,78), fac(hex(84)),   c9$( 2% + kk%)       , ch(03),~
                                                                         ~
               at (12,03), fac(lfac$(1%)), c1$( 3% + kk%)       , ch(01),~
               at (12,06), fac(hex(84)),   c2$( 3% + kk%)       , ch(05),~
               at (12,12), fac(hex(84)),   c3$( 3% + kk%)       , ch(09),~
               at (12,22), fac(hex(84)),   c4$( 3% + kk%)       , ch(25),~
               at (12,48), fac(hex(84)),   c5$( 3% + kk%)       , ch(08),~
               at (12,57), fac(hex(84)),   c6$( 3% + kk%)       , ch(02),~
               at (12,60), fac(hex(84)),   c7$( 3% + kk%)       , ch(06),~
               at (12,67), fac(hex(84)),   c8$( 3% + kk%)       , ch(10),~
               at (12,78), fac(hex(84)),   c9$( 3% + kk%)       , ch(03),~
                                                                         ~
               at (13,03), fac(lfac$(1%)), c1$( 4% + kk%)       , ch(01),~
               at (13,06), fac(hex(84)),   c2$( 4% + kk%)       , ch(05),~
               at (13,12), fac(hex(84)),   c3$( 4% + kk%)       , ch(09),~
               at (13,22), fac(hex(84)),   c4$( 4% + kk%)       , ch(25),~
               at (13,48), fac(hex(84)),   c5$( 4% + kk%)       , ch(08),~
               at (13,57), fac(hex(84)),   c6$( 4% + kk%)       , ch(02),~
               at (13,60), fac(hex(84)),   c7$( 4% + kk%)       , ch(06),~
               at (13,67), fac(hex(84)),   c8$( 4% + kk%)       , ch(10),~
               at (13,78), fac(hex(84)),   c9$( 4% + kk%)       , ch(03),~
                                                                         ~
               at (14,03), fac(lfac$(1%)), c1$( 5% + kk%)       , ch(01),~
               at (14,06), fac(hex(84)),   c2$( 5% + kk%)       , ch(05),~
               at (14,12), fac(hex(84)),   c3$( 5% + kk%)       , ch(09),~
               at (14,22), fac(hex(84)),   c4$( 5% + kk%)       , ch(25),~
               at (14,48), fac(hex(84)),   c5$( 5% + kk%)       , ch(08),~
               at (14,57), fac(hex(84)),   c6$( 5% + kk%)       , ch(02),~
               at (14,60), fac(hex(84)),   c7$( 5% + kk%)       , ch(06),~
               at (14,67), fac(hex(84)),   c8$( 5% + kk%)       , ch(10),~
               at (14,78), fac(hex(84)),   c9$( 5% + kk%)       , ch(03),~
                                                                         ~
               at (15,03), fac(lfac$(1%)), c1$( 6% + kk%)       , ch(01),~
               at (15,06), fac(hex(84)),   c2$( 6% + kk%)       , ch(05),~
               at (15,12), fac(hex(84)),   c3$( 6% + kk%)       , ch(09),~
               at (15,22), fac(hex(84)),   c4$( 6% + kk%)       , ch(25),~
               at (15,48), fac(hex(84)),   c5$( 6% + kk%)       , ch(08),~
               at (15,57), fac(hex(84)),   c6$( 6% + kk%)       , ch(02),~
               at (15,60), fac(hex(84)),   c7$( 6% + kk%)       , ch(06),~
               at (15,67), fac(hex(84)),   c8$( 6% + kk%)       , ch(10),~
               at (15,78), fac(hex(84)),   c9$( 6% + kk%)       , ch(03),~
                                                                         ~
               at (16,03), fac(lfac$(1%)), c1$( 7% + kk%)       , ch(01),~
               at (16,06), fac(hex(84)),   c2$( 7% + kk%)       , ch(05),~
               at (16,12), fac(hex(84)),   c3$( 7% + kk%)       , ch(09),~
               at (16,22), fac(hex(84)),   c4$( 7% + kk%)       , ch(25),~
               at (16,48), fac(hex(84)),   c5$( 7% + kk%)       , ch(08),~
               at (16,57), fac(hex(84)),   c6$( 7% + kk%)       , ch(02),~
               at (16,60), fac(hex(84)),   c7$( 7% + kk%)       , ch(06),~
               at (16,67), fac(hex(84)),   c8$( 7% + kk%)       , ch(10),~
               at (16,78), fac(hex(84)),   c9$( 7% + kk%)       , ch(03),~
                                                                         ~
               at (17,03), fac(lfac$(1%)), c1$( 8% + kk%)       , ch(01),~
               at (17,06), fac(hex(84)),   c2$( 8% + kk%)       , ch(05),~
               at (17,12), fac(hex(84)),   c3$( 8% + kk%)       , ch(09),~
               at (17,22), fac(hex(84)),   c4$( 8% + kk%)       , ch(25),~
               at (17,48), fac(hex(84)),   c5$( 8% + kk%)       , ch(08),~
               at (17,57), fac(hex(84)),   c6$( 8% + kk%)       , ch(02),~
               at (17,60), fac(hex(84)),   c7$( 8% + kk%)       , ch(06),~
               at (17,67), fac(hex(84)),   c8$( 8% + kk%)       , ch(10),~
               at (17,78), fac(hex(84)),   c9$( 8% + kk%)       , ch(03),~
                                                                         ~
               at (18,03), fac(lfac$(1%)), c1$( 9% + kk%)       , ch(01),~
               at (18,06), fac(hex(84)),   c2$( 9% + kk%)       , ch(05),~
               at (18,12), fac(hex(84)),   c3$( 9% + kk%)       , ch(09),~
               at (18,22), fac(hex(84)),   c4$( 9% + kk%)       , ch(25),~
               at (18,48), fac(hex(84)),   c5$( 9% + kk%)       , ch(08),~
               at (18,57), fac(hex(84)),   c6$( 9% + kk%)       , ch(02),~
               at (18,60), fac(hex(84)),   c7$( 9% + kk%)       , ch(06),~
               at (18,67), fac(hex(84)),   c8$( 9% + kk%)       , ch(10),~
               at (18,78), fac(hex(84)),   c9$( 9% + kk%)       , ch(03),~
                                                                         ~
               at (19,03), fac(lfac$(1%)), c1$(10% + kk%)       , ch(01),~
               at (19,06), fac(hex(84)),   c2$(10% + kk%)       , ch(05),~
               at (19,12), fac(hex(84)),   c3$(10% + kk%)       , ch(09),~
               at (19,22), fac(hex(84)),   c4$(10% + kk%)       , ch(25),~
               at (19,48), fac(hex(84)),   c5$(10% + kk%)       , ch(08),~
               at (19,57), fac(hex(84)),   c6$(10% + kk%)       , ch(02),~
               at (19,60), fac(hex(84)),   c7$(10% + kk%)       , ch(06),~
               at (19,67), fac(hex(84)),   c8$(10% + kk%)       , ch(10),~
               at (19,78), fac(hex(84)),   c9$(10% + kk%)       , ch(03),~
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
                  upd_flag% = 1%   /* SR66799 */
                  gosub process_data
                  goto L43190
                                   /* Exclude S.O. 'Non-Blank'         */
                                   /* Skip 'Non-Blank', Process 'Blank'*/
L43070:        if keyhit% <> 10% then goto L43085
                  upd_flag% = 1%   /* SR66799 */
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
                pg_tot_unit  = pg_tot_unit + y1
            next n%
            convert pg_tot_unit to pg_tot_unit$, pic(###.##)

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
                              L50500,         /* Move Dept (AWD003)    */ ~
                              L50530,         /* Color  0 = ALL        */ ~
                              L50540,         /* Sash limiter flag     */ ~
                              L50550          /* Stock order flag      */ 
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
/* (AWD004) */
               if str(ed_load$,1,3) = "ALL" then ed_load$ = bg_load$

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
/* CR3160            if str(bg_model$,1%,3%) <> " " then goto L50440  */
/* CR3160                bg_model$ = "ALL"                            */

L50440:     
/* CR3160  if str(bg_model$,1%,3%) = "ALL" then goto end_model */
               init(" ")readkey$
               str(readkey$,1%,9%)   = "MODEL    "
               str(readkey$,10%,15%) = bg_model$
               gosub check_model
/* (AWD004) */
               if str(ed_model$,1,3) = "ALL" then ed_model$ = bg_model$

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
          if keyhit% = 8% or keyhit% = 9% then return
          gosub error_prompt
          init(" ") readkey$, bg_model$, ed_model$
        return
        
L50500: REM Move Department                       mvDept$  (AWD003)
/* Check model to dept */
/* CR3160          if str(bg_model$,1%,3%) = "ALL" then goto L50501  */
/* CR3077 */
        init(" ")readkey$
        str(readkey$,1%,9%)  = "FLEXCHECK"
        str(readkey$,10%,3%) = bg_model$
        str(readkey$,13%,3%) = mvDept$
        read #2,key = readkey$, eod goto L50521
L50501:
        if schema% = 2% then goto inputTXDeptCheck   /* (AWD006) */
/* CR3160        if mvDept$ = " " then mvDept$ = "049"  */

REM        if mvDept$ = "007" or mvDept$ = "036" or mvDept$ = "049" or ~
           mvDept$ = "053" then return 
		   
REM        goto L50520
        return

/* (AWD006) */        
inputTXDeptCheck:
REM        if mvDept$ = " " then goto L50520
REM        if mvDept$ = "013" then return
REM        if mvDept$ = "057" then return
REM        if mvDept$ = "059" then return
REM        if mvDept$ = "030" then return
REM        if mvDept$ = "032" then return
REM        if mvDept$ = "070" then return   /* CR1863  */
REM        if mvDept$ = "029" then return   /* CR1863  */ 
REM        if mvDept$ = "016" then return   /* CR3020  */
REM        goto L50520
        
        return
/* (\AWD006) */
L50520:        
          errormsg$="(Error) - Invalid Move Department?"
          gosub error_prompt
          init(" ") mvDept$
        return
/* CR3077 */
L50521:        
          errormsg$="(Error) - Invalid Model and Move Department?"
          gosub error_prompt
          init(" ") mvDept$
        return
                
L50530:  /* Color parameter  CR1111 */
          clr_desc$ = " "
          if chk_clr$ = "0"  then L50532
          if chk_clr$ > " " then goto L50533
          chk_clr$ = "0" 
L50532:   clr_desc$ = "ALL Colors"
          return
L50533:          
          init(" ")readkey$
          str(readkey$,1%,9%)   = "COLOR"
          str(readkey$,10%,15%) = chk_clr$
          read #2,key = readkey$, using L50534, desc$, eod goto L50535
             clr_desc$ = desc$
          return
L50534:      FMT POS(25), CH(30)
L50535:
          errormsg$="(Error) - Invalid Color Code?"
          gosub error_prompt
          chk_clr$ = "0" : clr_desc$ = "ALL Colors"
        return

L50540:  /* Sash limiter Y/N CR1573 */
        if chk_shl$ <> "Y" and chk_shl$ <> "N" then ~
          errormsg$ ="(Error) - Invalid Sash Flag"
        return

L50550:  /* CR2948 */
REM CR2308       if schema% = 2% then goto notTX
       if chk_stk$ <> "Y" and chk_stk$ <> "N" then ~
          errormsg$ ="(Error) - Invalid Stock Flag"       
       return
notTX:
       chk_stk$ = "N"
       return
	   		
        REM *************************************************************~
            *************************************************************


        scan_loads

        scan_surge
            init(" ") sc_key$, sc_part$, sc_status$, hg$, sc_customer$  
            init(" ") width$, height$, gl$                 /* (AWD002) */
            str(sc_key$,1%,6%) = bg_dte$
            if bg_load$ = "ALL" then goto scan_surge_nxt
               str(sc_key$,7%,5%) = bg_load$
        scan_surge_nxt
          read #3,key 2% > sc_key$, using L61125, sc_key$, sc_part$,     ~
                   sc_customer$, sc_status$, eod goto scan_surge_done
L61125:         FMT CH(33), POS(34), CH(25), CH(09), POS(110), CH(02)
          cnt% = cnt% + 1%
          if mod(cnt%,100%) <> 0% then goto L61155
             convert cnt% to str(cnt$,19%,8%), pic(########)
             print at(02,02);hex(84);cnt$;

L61155:   sc_so$ = str(sc_key$,24,8)
          sc_ln$ = str(sc_key$,32,2)

          if sc_status$ <> "02" then goto scan_surge_nxt

          if str(sc_key$,1%,6%) > ed_dte$ then goto scan_surge_done
          if str(bg_load$,1%,3%) = "ALL" then goto L61175
             if str(sc_key$,7%,5%) < bg_load$ or                         ~
                str(sc_key$,7%,5%) > ed_load$ then goto scan_surge_nxt
L61175:   if str(bg_model$,1%,3%) = "ALL" then goto L61190
             if str(sc_part$,1%,3%) < bg_model$ or                      ~
                str(sc_part$,1%,3%) > ed_model$ then goto scan_surge_nxt
L61190:
                                                    /* only 267's */
/* (AWD004) */
REM             IF STR(SC_PART$,1,3) <> "267" THEN GOTO SCAN_SURGE_NXT
/* (CR1111) + */
             if chk_clr$ = "0" then goto L61194
             if chk_clr$ <> str(sc_part$,4%,1%) then goto scan_surge_nxt
L61194:
/* (CR1111) - */
/* (AWD006) */
             if schema% = 2% then goto surgeTXMdl          
/* (SR72711) */              	
               gl$ = str(sc_part$,5%,2%)
/* CR3058 allow all glass for NC and model 267 */               
               if (gl$ >= "3A" and gl$ <= "3N") and str(sc_part$,1,3) <> "267" ~
                  then goto scan_surge_nxt
/* CR2882 */
               if mvDept$ = "053" then goto L62000   
/* (SR72711) */
/* CR2308 */
               if str(sc_part$,1,3) = "267" and (str(sc_part$,11,1) = "W" or ~
                  str(sc_part$,11,1) = "X") then goto scan_surge_nxt
                  
               if str(sc_part$,1,3) = "267" then goto surge_mdl_found
/* CR1846 mull filter */
/* CR2882 */
L62000:
               if str(sc_part$,1,3) = "511" and str(sc_part$,20%,1%) <> "A" ~
                  and str(sc_part$,23%,1%) <> "A"                          ~
                     then goto surge_mdl_found
				
/* (AWD006) */              
surgeTXMdl:
REM 150 Black for TX
             if str(sc_part$,1,3) = "511" then goto surge_mdl_found
  /* CR3020 */             
             if str(sc_part$,1,3) = "551" then goto surge_mdl_found 
REM 120 Line
             if str(sc_part$,1,3) = "1W1" then goto surge_mdl_found
             if str(sc_part$,1,3) = "1D1" then goto surge_mdl_found
             if str(sc_part$,1,3) = "1H1" then goto surge_mdl_found
             if str(sc_part$,1,3) = "1X1" then goto surge_mdl_found
REM 5700 Line             
             if str(sc_part$,1,3) = "5W1" then goto surge_mdl_found             
             if str(sc_part$,1,3) = "8W1" then goto surge_mdl_found
/* CR9999 */             
             if str(sc_part$,1,3) = "5S2" then goto surge_mdl_found
             if str(sc_part$,1,3) = "5S3" then goto surge_mdl_found
             if str(sc_part$,1,3) = "5PW" then goto surge_mdl_found
             if str(sc_part$,1,3) = "5TP" then goto surge_mdl_found
             if str(sc_part$,1,3) = "8S2" then goto surge_mdl_found
             if str(sc_part$,1,3) = "8S3" then goto surge_mdl_found
             if str(sc_part$,1,3) = "8PW" then goto surge_mdl_found
             if str(sc_part$,1,3) = "8TP" then goto surge_mdl_found
REM 300 Line             
             if str(sc_part$,1,3) = "3W1" then goto surge_mdl_found
             if str(sc_part$,1,3) = "3H1" then goto surge_mdl_found
REM CR1261          
             if str(sc_part$,1,3) = "FH1" then goto surge_mdl_found
/* CR1863 */
             if str(sc_part$,1,3) = "JW1" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JX1" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JDO" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JDH" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JAO" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JAH" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JEO" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JEH" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JFO" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JFH" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JGO" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JGH" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JKO" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JKH" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JAA" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JAB" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JAC" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JAD" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JGB" then goto surge_mdl_found
             if str(sc_part$,1,3) = "JGD" then goto surge_mdl_found
             
             goto scan_surge_nxt
/* (\AWD006) */                
surge_mdl_found:
/* CR2948 Check stock flag */
REM CR3020             if schema% <> 1% then goto proc_cont
             if chk_stk$ <> "Y" then goto proc_cont
/* CR3020 */             
             if str(sc_customer$,1%,2%) <> "LO" and ~
                str(sc_customer$,1%,2%) <> "LX" then goto scan_surge_nxt
             
             init(" ") bck_key$, sku$
             str(bck_key$,1%,16%) = sc_so$
             convert sc_ln$ to sc_ln%
             convert sc_ln% to str(bck_key$,17%,3%), pic(###)
             
             read #7, key = bck_key$, using L12000, sku$, ~
                  eod goto scan_surge_nxt
L12000:    FMT POS(290), CH(10)

            if sku$ = " " then goto scan_surge_nxt
            
            gosub check_ecat_sku            /* CR2756 */
            if ecat% = 1% then goto scan_surge_nxt

            sku_key$ = "X_LO" & sku$ & "   "
            read #8, key = sku_key$, eod goto scan_surge_nxt

            init(" ") readkey$
            str(readkey$,1%,9%) = "SOS SKU"
            str(readkey$,10%,15%) = sku$

            read #2,key = readkey$, eod goto proc_cont
               goto scan_surge_nxt 

proc_cont:            
/* CR1111 move code up to include TX */ 
                                                   /* No Parts   */
             if str(sc_part$,19,1) = " " then goto scan_surge_nxt
             
/* CR2063 do not flex screen 7 added */
             if str(sc_part$,11,1) = "4" or str(sc_part$,11,1) = "5" ~
                or str(sc_part$,11,1) = "6" or str(sc_part$,11,1) = "7" ~
                  then goto scan_surge_nxt   
/* (AWD006) */ 
             if schema% = 2% then goto skipTXOptionCheck
               /* only white CR2802 add black */
/* CR3160 Allow paint for NC */		
             paintclr% = 0%
             if str(sc_part$,4,1) = "5" or str(sc_part$,4,1) = "L" or ~
                str(sc_part$,4,1) = "I" or str(sc_part$,4,1) = "M" or ~
                str(sc_part$,4,1) = "J" or str(sc_part$,4,1) = "N" or ~
                str(sc_part$,4,1) = "K" or str(sc_part$,4,1) = "O" or ~
                str(sc_part$,4,1) = "P"    then paintclr% = 1%
			 if paintclr% = 1% and str(sc_so$,1,1) = "D"   then goto contcolor
				
             if str(sc_part$,4,1) <> "2" and str(sc_part$,4,1) <> "4"  ~
                   then goto scan_surge_nxt
contcolor:				   
/* (AWD001) */
             hg$ = str(sc_part$,9,2)
             if hg$ >= "70" and hg$ <= "97" then goto scan_surge_nxt
/* (AWD001) */

/* (AWD002) */
             width$  = str(sc_part$,13,4)
             height$ = str(sc_part$,17,3)
             if width$ < "0160" then goto scan_surge_nxt
             if height$ < "250" then goto scan_surge_nxt
/* CR3145 */			 
			 if schema% = 1% and height$ <= "265" and mvDept$ = "007"  ~
			     then goto scan_surge_nxt

/* (AWD002) */
skipTXOptionCheck:                           /* (AWD006) */ 
             gosub check_sd
             if analysis% = 1% and order% = 0% then goto scan_surge_nxt

             init(" ")readkey$, errormsg$
             str(readkey$,1%,9%)   = "PLANPARTS"
             str(readkey$,10%,15%) = str(sc_part$,1,3)
             gosub check_model                   /* part found   */
/* (AWD006) */              
REM IF ERRORMSG$ = " " THEN GOTO SCAN_SURGE_NXT
             if errormsg$ = " " and schema% = 1% then goto scan_surge_nxt
                                                 /* No Parts     */
             errormsg$ = " "


             init(" ") sc_customer$
             sc_tqty%, sc_mqty%, sc_pqty%, sc_pqty1% = 0%

             get #3, using L61250, sc_customer$, sc_tqty%, sc_mqty%,      ~
                                   sc_pqty%, sc_pqty1%

L61250:         FMT POS(59), CH(9), POS(68), 4*BI(2)

             if sc_mqty% < 1% then goto scan_surge_nxt
                                                    /* Analysis% = 0%    */
                                                    /*for scan orders for*/
                                                    /*Stock.             */
                                                    /* Analysis% = 1%    */
                                                    /* for orders already*/
                                                    /* Assigned to stock */
               so_inv$  = str(sc_key$,24%, 8%)
               item_no$ = str(sc_key$,32%, 2%)
               gosub lookup_subpart
               subp$ = str(bcksubpt_rec$,48%,20%)
               foam$ = str(subp$,5%,1%)            /* (SR67154) */
               spacer$ = str(subp$,17%,1%)         /* (SR67154) */  
               sash$ = str(subp$,16%,1%)           /* CR1573 */
               series$ = str(bcksubpt_rec$,169%,16%)  /* CR1863 */
               prv_lbl$ = str(bcksubpt_rec$,139%,2%)  /* CR1863 */
               if (prv_lbl$ <> "18" and prv_lbl$ <> "19") ~
                        and str(sc_part$,1,1) = "J" ~
                    then scan_surge_nxt
               if sash$ = "2" and chk_shl$ = "N" then scan_surge_nxt
               if sash$ <> "2" and chk_shl$ = "Y" then scan_surge_nxt
               /* (SR67154) exclude filled profiles from surge */
               if schema% = 2% then goto noCheckNESOptions
			      
/* CR3023 per Chance skip foam wrap code 1 for move to 007 */
                if mvDept$ = "007" and foam$ = "1" ~ 
                    then goto scan_surge_nxt    
/* CR3058 Allow foam if model 511 and dept 053, spacer 3 required now for 511 */  
                if str(sc_part$,1%,3%) = "511" and mvDept$ = "053" then ~
                     goto noCheckNESOptions

                if foam$ = "3" or foam$ = "4" then goto scan_surge_nxt 				
                if spacer$ = "3" then goto scan_surge_nxt
                
noCheckNESOptions:
             if analysis% = 1% then gosub scan_loads_screen                ~
                else gosub scan_order_screen

             goto scan_surge_nxt
        scan_surge_done
           val_max% = cc%

           kk% = 0%
        return

        scan_loads_screen
          gosub load_screen
        return

        scan_order_screen
          gosub load_screen
        return

        load_screen
             if analysis% = 0% and order% = 1% then goto not_display  /*CR1488*/
             cc% = cc% + 1%
             c1$(cc%) = " "

/* CR1488         if analysis% = 0% and order% = 1% then c1$(cc%) = "*"   */

             c2$(cc%) = str(sc_key$,7%,5%)               /* Load Number*/
             c3$(cc%) = sc_customer$                     /* Cust  Code */
             c4$(cc%) = sc_part$                         /* Mfg Part   */

             c5$(cc%) = str(sc_key$,24%,8%)              /* So Number  */
             c6$(cc%) = str(sc_key$,32%,2%)              /* line Number*/

             convert sc_mqty% to c7$(cc%), pic(#####)

             c8$(cc%) = subp$
             c9$(cc%) = "N/A"                               /* CR1261 */
             gosub set_tx_fin_flange                        /* CR1261 */
             
not_display:
        return
/* CR1261 */
        set_tx_fin_flange
             if str(subp$,18%,1%) = "4" then c9$(cc%) = "FLA"
             if str(subp$,18%,1%) = "5" then c9$(cc%) = "F-F"
             if str(subp$,18%,1%) = "6" then c9$(cc%) = "FIN"             
        return


        REM *************************************************************~
            * New eCat validate check     CR2756                        *~
            *************************************************************
            
        check_ecat_sku
               init(" ") bckm_key$
               ecat% = 0%
               
               str(bckm_key$,1%,9%)   = sc_customer$
               str(bckm_key$,10%,16%) = sc_so$
      
               read #9,key = bckm_key$, using L62625, bck_user_entered$, ~
                                                  eod goto L62630
L62625:          FMT POS(836), CH(03)

                  if bck_user_entered$ = "ECT" then ecat% = 1%
L62630:
        return
        
        process_data
            call "SHOSTAT" ("Processing Include/Exclude Data")
            cnt% = drop_seq_max%

            for i% = 1% to val_max%
        /*      if upd_flag% <> 0% then L62295  SR66799 */
                                              /* Include '*' Skip ' '  */
                if keyhit% =  9% and c1$(i%) = " " then goto L62295
                                              /* Include ' ' Skip '*'  */
                if keyhit% = 10% and c1$(i%) <> " " then goto L62295

                gosub update_apcplnsd


L62295:     next i%
        return


 
        update_apcplnsd
              init(" ") sd_key$, sd_rec$
              str(sd_key$,1%,10%) = str(c5$(i%),1%,8%) & str(c6$(i%),1%,2%)

        check_sd_nxt
              read #6, hold, key > sd_key$, using L60100, sd_rec$, eod goto sd_done

L60100:              FMT CH(64)
                    sd_key$ = str(sd_rec$,1,23)

                    if str(sd_key$,1%,8%) <> str(c5$(i%),1%,8%) then goto sd_done
                    if str(sd_key$,9%,2%) <> str(c6$(i%),1%,2%) then goto sd_done
/*SR66799 + */          
                    init(" ")readkey$, errormsg$
                    str(readkey$,1%,9%)   = "PLAN SUPP"
                    str(readkey$,10%,15%) = str(sd_rec$,12,3)
                    read #2,key = readkey$, eod goto cont_check_sd2
                    upd_flag% = 0%
                    goto check_sd_nxt
                cont_check_sd2
/*SR66799 - */        

/* (AWD004) */
REM IF ANALYSIS% = 0% AND (STR(SD_KEY$,12%,3%) <> "007" AND STR(SD_REC$,11%,1%) <> "0") THEN GOTO CHECK_SD_NXT
/* (AWD003) */
/* (AWD005) */
REM IF ANALYSIS% = 0% AND (STR(SD_KEY$,12%,3%) =  "007" AND STR(SD_REC$,11%,1%) = "0") THEN GOTO SURGEDEPT
/* (AWD006) */   
                   if schema% = 2% then goto checkSDTX
                    if analysis% = 0% and (str(sd_key$,12%,3%) =  "005" and    ~
                                str(sd_rec$,11%,1%) = "0") then goto surgeDept
                                
                    if analysis% = 0% and (str(sd_key$,12%,3%) =  "027" and    ~
                                str(sd_rec$,11%,1%) = "0") then goto surgeDept
                                
                    if analysis% = 1% and (str(sd_key$,12%,3%) = "049" and ~
                                str(sd_rec$,11%,1%) = "!") then goto surgeDept
                                
/* CR2882 */                                
                    if analysis% = 1% and (str(sd_key$,12%,3%) = "053" and ~
                                str(sd_rec$,11%,1%) = "!") then goto surgeDept                                
/* (AWD005)  */
REM IF ANALYSIS% = 1% AND (STR(SD_KEY$,12%,3%) = "005" AND STR(SD_REC$,11%,1%) = "!" )THEN GOTO SURGEDEPT
                    if analysis% = 1% and (str(sd_key$,12%,3%) = "007" and ~
                                str(sd_rec$,11%,1%) = "!" )then goto surgeDept
                                
                    if analysis% = 1% and (str(sd_key$,12%,3%) = "036" and ~
                                str(sd_rec$,11%,1%) = "!" )then goto surgeDept
REM IF ANALYSIS% = 1% AND (STR(SD_KEY$,12%,3%) <> "049" AND  STR(SD_REC$,11%,1%) <>  "!")  THEN GOTO CHECK_SD_NXT

                    goto check_sd_nxt
/* (AWD006) */                       
checkSDTX:
                    if analysis% = 0% and (str(sd_key$,12%,3%) =  "031" and    ~
                                str(sd_rec$,11%,1%) = "0") then goto surgeDept
                                
                    if analysis% = 0% and (str(sd_key$,12%,3%) =  "058" and    ~
                                str(sd_rec$,11%,1%) = "0") then goto surgeDept
                                
                    if analysis% = 0% and (str(sd_key$,12%,3%) =  "012" and    ~
                                str(sd_rec$,11%,1%) = "0") then goto surgeDept
                              
                                
                                
                    if analysis% = 1% and (str(sd_key$,12%,3%) = "030" and ~
                                str(sd_rec$,11%,1%) = "!" )then goto surgeDept
                                                                
                    if analysis% = 1% and (str(sd_key$,12%,3%) = "032" and ~
                                str(sd_rec$,11%,1%) = "!" )then goto surgeDept
                                
                    if analysis% = 1% and (str(sd_key$,12%,3%) = "013" and ~
                                str(sd_rec$,11%,1%) = "!" )then goto surgeDept
                                
                    if analysis% = 1% and (str(sd_key$,12%,3%) = "057" and ~
                                str(sd_rec$,11%,1%) = "!" )then goto surgeDept            
                                
                    if analysis% = 1% and (str(sd_key$,12%,3%) = "059" and ~
                                str(sd_rec$,11%,1%) = "!" )then goto surgeDept      
                                
/* (\AWD006) */
/* CR1863 */
                    if analysis% = 0% and (str(sd_key$,12%,3%) =  "029" and   ~
                                str(sd_rec$,19,3) <> "JX1" and                 ~
                                str(sd_rec$,11%,1%) = "0") then goto surgeDept
               
                    if analysis% = 1% and (str(sd_key$,12%,3%) = "070" and    ~
                                str(sd_rec$,19,3) <> "JX1" and                 ~
                                str(sd_rec$,11%,1%) = "!" ) then goto surgeDept
                    
                    if analysis% = 0% and (str(sd_key$,12%,3%) =  "071" and   ~
                                str(sd_rec$,19,3) = "JX1" and                 ~
                                prv_lbl$ = "19" and                           ~
                                str(sd_rec$,11%,1%) = "0") then goto surgeDept                                

                    if analysis% = 1% and (str(sd_key$,12%,3%) = "029" and    ~
                                str(sd_rec$,19,3) = "JX1" and                 ~
                                prv_lbl$ = "19" and                           ~
                                str(sd_rec$,11%,1%) = "!" )then goto surgeDept
/* CR1863 */

surgeDept:
/* (AWD003\) */
                        delete #6


                                      /* make plan first so will not overflow*/
                    if analysis% = 0% then str(sd_rec$,11%,1%) = "!"
                                                           /* new department */
/* (AWD003) */                                            
REM IF ANALYSIS% = 0% THEN STR(SD_REC$,12%,3%) = "049"
               if analysis% = 0% then str(sd_rec$,12%,3%) = mvDept$

                                      /* put back to 1 */
               if analysis% = 1% then str(sd_rec$,11%,1%) = "0"
                                                           /* new department */
/* (AWD004) */
REM  IF ANALYSIS% = 1% THEN STR(SD_REC$,12%,3%) = "007"
/* (AWD005) */
REM  IF ANALYSIS% = 1% AND STR(SD_KEY$,19,3) = "267" THEN STR(SD_REC$,12%,3%) = "007"

                    if analysis% = 1% and str(sd_key$,19,3) = "267"   ~
                                       then str(sd_rec$,12%,3%) = "005"
                                       
                    if analysis% = 1% and str(sd_key$,19,3) <> "267"   ~
                                       then str(sd_rec$,12%,3%) = "027"
/* CR2882 */
                    if analysis% = 1% and str(sd_key$,19,3) = "511"   ~
                                       then str(sd_rec$,12%,3%) = "027"  
					   
/* CR3020 */    
                    if analysis% = 1% and str(sd_key$,19,3) = "511"   ~ 
                       and schema% = 2%                               ~
                                       then str(sd_rec$,12%,3%) = "015"                                       
                    if analysis% = 1% and str(sd_key$,19,3) = "551"   ~ 
                       and schema% = 2%                               ~
                                       then str(sd_rec$,12%,3%) = "015"                                          
/* (AWD006) */
                    if analysis% = 1% and str(sd_key$,19,3) = "1W1"   ~
                                       then str(sd_rec$,12%,3%) = "012"
                    if analysis% = 1% and str(sd_key$,19,3) = "1D1"   ~
                                       then str(sd_rec$,12%,3%) = "012"
                    if analysis% = 1% and str(sd_key$,19,3) = "1H1"   ~
                                       then str(sd_rec$,12%,3%) = "012"
                    if analysis% = 1% and str(sd_key$,19,3) = "1X1"   ~
                                       then str(sd_rec$,12%,3%) = "012"
                                       
                    if analysis% = 1% and str(sd_key$,19,3) = "5W1"   ~
                                       then str(sd_rec$,12%,3%) = "058"
                    if analysis% = 1% and str(sd_key$,19,3) = "8W1"   ~
                                       then str(sd_rec$,12%,3%) = "058"

                    if analysis% = 1% and str(sd_key$,19,3) = "3W1"   ~
                                       then str(sd_rec$,12%,3%) = "031"                                                                              
                    if analysis% = 1% and str(sd_key$,19,3) = "3H1"   ~
                                       then str(sd_rec$,12%,3%) = "031"  
                                   
                                       
/* CR1261 + */      if analysis% = 1% and str(sd_key$,19,3) = "FH1"     ~
                                       then str(sd_rec$,12%,3%) = "012"   
                                       
/* CR1863 */        if analysis% = 1% and str(sd_key$,19,1) = "J" and   ~
                                          str(sd_key$,19,3) <> "JX1"    ~
                                       then str(sd_rec$,12%,3%) = "029"    
                                       
/* CR1863 */        if analysis% = 1% and str(sd_key$,19,3) = "JX1"     ~
                                      and prv_lbl$ = "19"               ~ 
                                       then str(sd_rec$,12%,3%) = "071"                                       
/* (\AWD006) */
                                              
                    put #6, using L60100, sd_rec$

                    write #6, eod goto sd_done

                        goto check_sd_nxt                /* do all 3 shifts  */

        sd_done
        return

/* Analysis% = 0% for scan orders for Flex.             */ 

        check_sd
              init(" ") sd_key$, sd_rec$
              order% = 0%
              str(sd_key$,1%,10%) = str(sc_key$,24,10)
        check_sd_order
              read #6, hold, key > sd_key$, using L60100, sd_rec$, eod goto order_done


                    sd_key$ = str(sd_rec$,1,23)
                    if str(sd_key$,1%,8%) <> str(sc_key$,24%,8%) then goto order_done
                    if str(sd_key$,9%,2%) <> str(sc_key$,32%,2%) then goto order_done
/* (AWD003) */
/* (AWD007) modification to exclude stock and cross dock departments */                    
/* '!' means order line has been flexed */
/* mod not to goto order_done but read all records for an order line to look for department >= 100 */
                    if str(sd_rec$,12,3) >= "100" then order% = 0%
/*SR66799 +                                                                 ~
                    init(" ")readkey$, errormsg$                            ~
                    str(readkey$,1%,9%)   = "PLAN SUPP"                     ~
                    str(readkey$,10%,15%) = str(sd_rec$,12,3)               ~
                    read #2,key = readkey$, eod goto cont_check_sd          ~
                    upd_flag% = 0%                                          ~
                    order% = 0%                                             ~
                    goto check_sd_order                                     ~
                cont_check_sd                                               ~
  SR66799 - */        
                     
            REM                    IF STR(SD_REC$,11,1) <> "!" THEN GOTO ORDER_DONE                    
                    if str(sd_rec$,11,1) <> "!" then goto check_sd_order
                    
                    if analysis% = 1% and str(sd_rec$,12,3) <> mvDept$ ~
                                                         then goto order_done
                    
                     order% = 1%   /*SR66799  keep going looking for depts to exclude */
                     goto check_sd_order
        order_done
        return



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
            init(" ") flag$, pgm$, bcksubpt_rec$, flds$(), info_flds$(),~
                      subp$, foam$, spacer$
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
                   str(bcksubpt_rec$,48%,20%) = "00000000000000000000"

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


