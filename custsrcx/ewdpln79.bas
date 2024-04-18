        REM *************************************************************~
            *                                                           *~
            *  EEEEE  W   W  DDDD   PPPP   L      N   N  77777   999    *~
            *  E      W   W  D   D  P   P  L      NN  N     7   9   9   *~
            *  EEEE   W W W  D   D  PPPP   L      N N N    7     9999   *~
            *  E      WW WW  D   D  P      L      N  NN   7         9   *~
            *  EEEEE  W   W  DDDD   P      LLLLL  N   N  7      9999    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLN79 - Program to scan barcode to Backorder item.     *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/29/01 ! Original                                 ! CMG *~
            * 03/03/06 ! (PAR001) Mod for New Sub Part Number in  ! RHH *~
            *          !    the label file.                       !     *~
            * 12/28/18 ! CR1828 EWDPRDLB cust code changes        ! DES *~
            *05/05/2018! CR Proj  - Trigger Atlas on order remove ! RDB *~
            *12/30/2019! CR2368 - Add free all command            ! RDB *~
            *************************************************************

        dim readkey$50, desc$30,         /* GENERIC KEY                */~
            pfkeys$40,                   /* PF KEYS                    */~
            barcode$18,                  /* Scanned Barcode            */~
            prevcode$18,                 /* Previous Bar Code Entered  */~
            wandchar$1,                  /* Wand Character - Scanner   */~
            xx$(7%)50,                   /* Screen Display area Text   */~
            dt_rec$256, dt_dept$3,       /* Production Detail          */~
            dt_key$23, sav_key$23,       /* (APCPLNDT)-Tracking File   */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            edtmessage$79,               /* Edit screen message        */~
            lfac$(6%)1,                  /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            scrn_title$40,               /* Screen Description         */~
            fld$(3%)30,                  /* Field Text                 */~
            userid$3                     /* Current User Id            */

        dim area_code$1,                 /* Area    Code               */~
            area_desc$30,                /* Area    Description        */~
            reas_code$3,                 /* Reason  Code               */~
            reas_desc$30,                /* Reason  Description        */~
            user_id$3,                   /* User    Id                 */~
            user_desc$30,                /* User    Id Description     */~
            auth_id$3,                   /* Authorized  Id             */~
            auth_desc$30,                /* Authorized Id Description  */~
            text$(3%)20,                 /* Text Area                  */~
            so_num$8,                    /* Sales Order Number         */~
            so_lne$2,                    /* Sales Order Line Number    */~
            cust_code$9,                 /* Customer Code              */~
            po_num$16,                   /* Purchase Order Number      */~
            ld_num$5                     /* Load Number                */ 

        dim                                                              ~
            hdr$40,                      /* ASKUSER HEADER             */~
            filename$8,                  /* Use with EWDOPEN           */~            
            msg$(3%)79                   /* ASKUSER TEXT               */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%), axd$4,             /* = 1 if file open, -1 if it */~
            rslt$(15%)20                 /* Text from file opening     */

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
            * #1  ! APCPLNDT ! (NEW) Planning Tracking File             *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            * #3  ! EWDBOLRM ! Line Items Removed from Load             *~
            * #4  ! EWDBOLBK ! Cross-Reference file for Back Orders     *~
            * #5  ! APCPLNDP ! Master Department File                   *~
            * #6  ! APCPLNAD ! (New) Planning Master Audit File         *~
            * #7  ! USERCLMS ! Caelus Master User Def. (USERLCMS)       *~
            * #8  ! APCPLNSC ! Planning Master Schedule-Old APCLINES    *~
            * #9  ! EWDPRDLB ! New Production Labels                    *~
            * #10 ! APCPLNOR ! Planning S.O. Header History-Old APCORDER*~
            * #11 ! EWDPRDLB ! New Production Labels                    *~
/*CRProj*/  * #23 ! PGORLNTR ! PlyGem ATLaS Trigger Remote Order Line Fi*~   
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen = 24

            select #3 , "EWDBOLRM",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =    1, keylen =   16,                    ~
                        alt key  1, keypos =    6, keylen =  11    

            select #4 , "EWDBOLBK",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    7, keylen =   16,                    ~
                        alt key  1, keypos =   12, keylen =  11,         ~
                            key  2, keypos =    1, keylen =  22,         ~
                            key  3, keypos =    2, keylen =  21

            select #5,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos = 11,   keylen = 12,                      ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15

            select #6,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 19,   keylen = 33,                      ~
                        alt key 1, keypos =  1, keylen = 33

            select #7,  "USERLCMS",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =   4, keylen =  30, dup
   
            select #8, "APCPLNSC",                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33
                                                            /* (PAR001) */
            select #9,  "EWDPRDLB",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23 

            select #10, "APCPLNOR",                                      ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup
                                                            /* (PAR001) */ 
            select #11, "EWDPRDLB",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23 
                        
                                                            /* (PAR001) */
            select #23, "PGORLNTR",                    /* PC Proj  */    ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   21, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  64,         ~
                            key  2, keypos =   54, keylen =  11, dup
                            
            call "SHOSTAT" ("Opening Files, One moment Please?")

            filename$ = "APCPLNDT" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

            call "OPENCHCK" (#3, fs%(3%), f2%(3%),500%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),500%, rslt$(4%))

            filename$ = "APCPLNDP" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNAD" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "USERLCMS" : call "EWDOPEN" (#7, filename$, err%) 
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPRDLB" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPRDLB" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
/* CR Proj */            
            filename$ = "PGORLNTR" : call "EWDOPEN" (#23, filename$, err%)
            if err% <> 0% then gosub open_error

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "         BACK ORDER Entry Program         "
            pname$ = "EWDPLN79 - Rev: R6.04"

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
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
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub inputmode_scan
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11140

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_scan
            gosub initialize_var_screen
            gosub'102(fieldnr%)
            errormsg$ = " "
            if keyhit% <> 16% then goto back_order    
               return clear all
               goto inputmode

            back_order
              gosub'152(fieldnr%)
              if errormsg$ <> " " then inputmode_scan

              call% = 2% 
              scr_code% = 5%                 /* For Back Order */
              for i% = 1% to 2%
                   call "EWDPLA44" (call%,  /* 1 = APCPA44 2=EWDPLN79   */ ~
                                    i%,     /* 1 = Updte St 2= Updte Hdr*/ ~
                                    scr_code%, /* Screen Code Selection */ ~
                                    ld_num$,   /* Load Number           */ ~
                                    barcode$,  /* Bar Code Number       */ ~
                                    user_id$,  /* Userid Entering       */ ~
                                    auth_id$,  /* Authorizing Id        */ ~
                                    area_code$,/* Area Code             */ ~
                                    " ",       /* Found Code - Not Used */ ~
                                    reas_code$,/* Reason Code           */ ~
                                    text$(),   /* Text                  */ ~
                                    #1,        /* APCPLNDT              */ ~
                                    #10,       /* APCPLNOR              */ ~
                                    #8,        /* APCPLNSC              */ ~
                                    #2,        /* GENCODES              */ ~
                                    #6,        /* APCPLNAD              */ ~
                                    #3,        /* EWDBOLRM              */ ~
                                    #11,       /* EWDPRDLB   /* (PAR001)*/ ~
                                    #4,        /* EWDBOLBK              */ ~
                                    #23,       /* PGORLNTR    CR Proj   */ ~
                                    err%       /* Error Code            */ )
              next i%
              if err% <> 0% then goto inputmode_scan

              xx$(1%)  = "Customer Code  :  " & cust_code$
              xx$(2%)  = "Sales Order Num:  " & so_num$
              xx$(3%)  = "Sales Order Lne:  " & so_lne$
              xx$(4%)  = "Load Number    :  " & ld_num$
              prevcode$ = barcode$
              init(" ") barcode$            
            
            goto inputmode
            

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
         return

       REM *************************************************************~
            *      E D I T   M O D E   F O R   L O A D   D E T A I L    *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        

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
         "Enter a Area Code for the Back Order.                        ",~
         "Enter a Reason Code for the Back Order.                      ",~
         "Enter a User ID Code.                                        ",~
         "Enter the Authorizing User ID Code.                          ",~
         "Enter any applicable Text.                                   "


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, area_code$, reas_code$,    ~
                      user_id$, auth_id$, text$(), cust_code$, so_num$,  ~
                      so_lne$, ld_num$, area_desc$, reas_desc$,          ~
                      user_desc$, auth_desc$
         
            call "ALLFREE"                   /* CR2368 */
        return


        initialize_var_screen
            init(" ") scrn_title$, xx$(), dt_rec$, dt_dept$, dt_key$,    ~
                      sav_key$, barcode$, wandchar$, prevcode$, po_num$
            fieldnr% = 1%
            scrn_title$ = "     Back Order Scanning Utility     "
            call "ALLFREE"                    /* CR2368 */

        return
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
              on fieldnr% gosub L40090,         /* Area Code         */   ~
                                L40090,         /* Reason Code       */   ~
                                L40090,         /* UserID            */   ~
                                L40090,         /* Authorized ID     */   ~
                                L40090          /* Text Area         */

              goto L40105

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Area In Plant     :",                        ~
               at (06,25), fac(lfac$(1%)), area_code$           , ch(01),~
               at (06,40), fac(hex(84)), area_desc$             , ch(30),~
                                                                         ~
               at (07,02), "Reason Code       :",                        ~
               at (07,25), fac(lfac$(2%)), reas_code$           , ch(03),~
               at (07,40), fac(hex(84)), reas_desc$             , ch(30),~
                                                                         ~
               at (08,02), "User ID           :",                        ~
               at (08,25), fac(lfac$(3%)), user_id$             , ch(03),~
               at (08,40), fac(hex(84)), user_desc$             , ch(30),~
                                                                         ~
               at (09,02), "Authorized User ID:",                        ~
               at (09,25), fac(lfac$(4%)), auth_id$             , ch(03),~
               at (09,40), fac(hex(84)), auth_desc$             , ch(30),~
                                                                         ~
               at (10,02), "Text              :",                        ~
               at (10,25), fac(lfac$(5%)), text$(1%)            , ch(20),~
               at (11,25), fac(lfac$(5%)), text$(2%)            , ch(20),~
               at (12,25), fac(lfac$(5%)), text$(3%)            , ch(20),~
                                                                         ~
               at (14,16), fac(hex(84)), xx$(1%)                , ch(50),~
               at (15,16), fac(hex(84)), xx$(2%)                , ch(50),~
               at (16,16), fac(hex(84)), xx$(3%)                , ch(50),~
               at (17,16), fac(hex(84)), xx$(4%)                , ch(50),~
               at (18,16), fac(hex(84)), xx$(5%)                , ch(50),~
               at (19,16), fac(hex(84)), xx$(6%)                , ch(50),~
               at (20,16), fac(hex(84)), xx$(7%)                , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40340
                  call "PRNTSCRN"
                  goto L40105

L40340:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40435     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40415
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40415:     if fieldnr% > 1% then L40425
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40425:     return

L40435: if fieldnr% > 0% then L40480  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Scan Data   "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L40480:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return


        REM *************************************************************~
            *       Production, Staging, Loading Display Screen         *~
            *************************************************************

        deffn'102(fieldnr%)
L41500:     gosub set_screen_2
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), scrn_title$            , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(1%)               , ch(25),~
               at (05,30), fac(lfac$(1%)), barcode$             , ch(18),~
               at (05,50), fac(lfac$(2%)), wandchar$            , ch(01),~
                                                                         ~
               at (05,52), fac(hex(84)), fld$(2%)               , ch(04),~
               at (05,57), fac(hex(84)),   prevcode$            , ch(18),~
                                                                         ~
               at (14,16), fac(hex(84)), xx$(1%)                , ch(50),~
               at (15,16), fac(hex(84)), xx$(2%)                , ch(50),~
               at (16,16), fac(hex(84)), xx$(3%)                , ch(50),~
               at (17,16), fac(hex(84)), xx$(4%)                , ch(50),~
               at (18,16), fac(hex(84)), xx$(5%)                , ch(50),~
               at (19,16), fac(hex(84)), xx$(6%)                , ch(50),~
               at (20,16), fac(hex(84)), xx$(7%)                , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L41800
                  gosub startover

L41800:        if keyhit% <> 15% then L41850
                  call "PRNTSCRN"
                  goto L41500

L41850:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_2
            inpmessage$ = "Scan Barcode or Manually Enter Barcode Number"

            lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
            fld$(1%)      = "Barcode Number To Scan  :"
            fld$(2%)      = "Prv:"
            
            pf$(1%) = "(1)Startover                            " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Screen "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50150,         /* Area Code             */ ~
                              L50350,         /* Reason Code           */ ~
                              L50550,         /* User ID               */ ~
                              L50750,         /* Authorized ID         */ ~
                              L50950          /* Text Area             */


            return

L50150: REM Area Code                             AREA_CODE$, AREA_DESC$
        init(" ") readkey$, desc$, area_desc$
            str(readkey$,1%,9%) = "PLAN BKAR"
            if area_code$ <> " " then str(readkey$,10%,1%) = area_code$
               call "PLOWCODE" (#2, readkey$, desc$, 9%, .30, f1%(2))
                    if f1%(2) <> 1 then goto L50270

               area_code$ = str(readkey$,10%,1%)
               area_desc$ = desc$
        return
L50270:     errormsg$ = "(Error) - Invalid Area Code ?"
            gosub error_prompt
            init(" ") readkey$, desc$, area_desc$, area_code$
        return

L50350:  REM Reason Code                          REAS_CODE$, REAS_DESC$
         init(" ") readkey$, desc$, reas_desc$
            str(readkey$,1%,9%) = "PLAN BKRE"
            if reas_code$ <> " " then str(readkey$,10%,3%) = reas_code$ ~
               else str(readkey$,10%,1%) = area_code$
               call "PLOWCODE" (#2, readkey$, desc$, 9%, .30, f1%(2))
                    if f1%(2) <> 1 then goto L50470

               reas_code$ = str(readkey$,10%,3%)
               reas_desc$ = desc$
        return
L50470:     errormsg$ = "(Error) - Invalid Reason Code ?"
            gosub error_prompt
            init(" ") readkey$, desc$, reas_desc$, reas_code$
        return

L50550:  REM User ID                              USER_ID$, USER_DESC$
         init(" ") readkey$, desc$, user_desc$
            if user_id$ <> " " then str(readkey$,1%,3%) = user_id$

               call "GETCODE" (#7, readkey$, desc$, 0%, 0%, f1%(7))
                    if f1%(7) <> 1 then goto L50670

               user_id$ = str(readkey$,1%,3%)
               user_desc$ = desc$
        return         
L50670:     errormsg$ = "(Error) - Invalid User ID   ?"
            gosub error_prompt
            init(" ") readkey$, desc$, user_desc$, user_id$
        return
L50750: REM Authorized User ID                    AUTH_ID$, AUTH_DESC$
         init(" ") readkey$, desc$, auth_desc$
            if auth_id$ <> " " then str(readkey$,1%,3%) = auth_id$

               call "GETCODE" (#7, readkey$, desc$, 0%, 0%, f1%(7))
                    if f1%(7) <> 1 then goto L50770

               auth_id$ = str(readkey$,1%,3%)
               auth_desc$ = desc$
        return         
L50770:     errormsg$ = "(Error) - Invalid Authorized User ID   ?"
            gosub error_prompt
            init(" ") readkey$, desc$, auth_desc$, auth_id$
        return

L50950: REM Text                                  TEXT$()

        return 

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Scanning Screen.               *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            gosub L52500                      /* Barcode Number        */ 

            return

L52500:     str(dt_key$,1%,18%) = barcode$
            str(sav_key$,1%,18%) = str(dt_key$,1%,18%) 

        read_dt_next
            read #1, key > dt_key$, using L52510, dt_rec$, eod goto L52590
L52510:         FMT CH(256)

            str(dt_key$,1%,23%) = str(dt_rec$,24%,23%)

            if str(sav_key$,1%,18%) <> str(dt_key$,1%,18%) then goto L52590
            dt_dept$ = str(dt_rec$,42%,3%)
            gosub check_support
            if supp% = 1% then goto read_dt_next

            so_num$    = str(dt_rec$,24%,8%)
            so_lne$    = str(dt_rec$,32%,2%)
            cust_code$ = str(dt_rec$,124%,9%)
            ld_num$    = str(dt_rec$,1%,5%)
        return
L52590:     errormsg$ = "(Error) - Invalid BarCode   ?"
            gosub error_prompt
            init(" ") dt_key$, sav_key$, dt_rec$, barcode$
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        check_support
           supp% = 0%
           if dt_dept$ = "102" or dt_dept$ = "104" then                  ~
                                                  goto check_support_done
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLAN SUPP"
           str(readkey$,10%,15%) = dt_dept$
           read #5,key = readkey$, eod goto check_support_done
           supp% = 1%
        check_support_done
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

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end


