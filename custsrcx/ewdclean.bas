        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDCLEAN                             *~
            *  Creation Date     - 07/26/99                             *~
            *  Last Modified Date- 04/01/2017                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Modified By  - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Master Purge Utility for down sizing *~
            *                      specific Database files.             *~
            *                                                           *~
            *  DATABASE Files    - (1) EWDLABSP                         *~
            *                      (2) EWDPRDLB                         *~
            *                      (3) EWDPRDQQ                         *~
            *                      (4) AWDPLNRK                         *~
            *                      (5) EWDGLSXX                         *~
            *                      (6) APCEDIMC                         *~
            *                      (7) APCPLNAD                         *~
            *                      (8) SHPCOSTS                         *~
            *                      (9) APCPLNSD                         *~
            *                     (10) APCEMPDT                         *~
            *                     (11) APCEMPMT                         *~
            *                     (12) SHPHDRS                          *~
            *                     (13) SHPLINES                         *~
            *                     (14) AWDAPPLS                         *~
            *                     (15) AWDPLNSR                         *~
            *                     (16) AWDPLNAD                         *~
            *                     (17) PGORATTR                         *~
            *                     (18) PGSCHDTR                         *~
            *                     (19) PGORLNTR                         *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/16/99 ! (New) Program -                          ! RHH *~
            * 03/21/00 ! Mod to add files SHPHDRS and SHPLINES    ! RHH *~
            * 04/27/04 ! Mod to add Appian Shipping Lab AWDAPPLS  ! RHH *~
            *          ! (AWD001)                                 !     *~
            * 04/05/05 ! (AWD002) Mod to the reclen of APCPLNSD   ! CMG *~
            * 03/03/06 ! (AWD003) Mod for the new production label! RHH *~
            *          !    changes. Size Change 640 to 1024      !     *~
            * 09/25/06 ! (AWD004) Mod for file change to AWDAPPLS ! RHH *~
            *          !    for new Job Name and Room Location    !     *~
            *04/01/2017! Mod for AWDPLNSR and AWDPLNAD            ! CMN *~
            *06/19/2017! CR950 Change EWDPLNRK to AWDPLNRK        ! RDB *~
            *02/12/2019! CR-1894 Increase Emp Dep size from 2 to 3! DES *~
            *12/02/2020! CR2735 Add PG Trigger files              ! RDB *~
            *************************************************************

        dim                                                              ~
            sel$(18%)1, sel_d$(30%)40,   /* Purge Selection Data       */~
            sel8$8, sel9$8, cnt$28,      /* Sales order Number         */~
            lookup_key$30,               /* S.O. Lookup Key            */~
            ss$(30%)40,                  /* Purge Descriptive Data     */~
            pg_dte$6,                    /* Today's Date               */~
            pg_dte1$6,                   /* Seven Days Old             */~
            pg_dte2$6,                   /* Fourteen Days Old          */~
            pg_dte3$6,                   /* Thirty Days Old            */~
            pg_dte4$6,                   /* 120 Days                   */~
            pg_dte5$6,                   /* 21 Days Old        (AWD001)*/~
            pg_dte6$6,                   /* 365 Days Old        CR2735 */~
            purge_dte$6,                 /* Read Date for Purge        */~
            purge_key$60,                /* For Purge Utilities        */~
            purge_key1$60,               /* For Purge Util (2)         */~
            purge_tme$4,                 /* Scan time          (AWD001)*/~
            barcode$18,                  /* Production Barcode         */~
            filename$8,                  /* Filename                   */~
            sc_yr$4, sc_wk$2,            /* purge_time_clock           */~
            sc_yr_bi$2,                  /*                            */~
            inv_no$8,                    /* Invoice Number             */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            time$8,                      /* System time                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim prd_dte$6,                   /* Production Date     (AWD004)*/~
            create_dte$6                 /* Label Creation Date (AWD004)*/
            
        dim                              /* CR2735                     */~
            tr_rec$256,                  /* Record length              */~
            tr_key$64                    /* Key of tracking file       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Master Clean-Up/Purge Utility  "
            pname$ = "EWDCLEAN - Rev: 01.00"

        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! EWDLABSP ! Lowe's Special Label Database            *~
            * #2  ! EWDPRDLB ! Master DATABASE for Prod Labels  (AWD003)*~
            * #3  ! EWDPRDQQ ! Store Associated P.O. Totals             *~
            * #4  ! AWDPLNRK ! Master DATABASE for Glass Rack Labels    *~
            * #5  ! EWDGLSXX ! Master Remake Label Database             *~
            * #6  ! APCEDIMC ! Master EDI Control File                  *~
            * #7  ! APCPLNAD ! MFG Master Audit File for Scanning       *~
            * #8  ! SHPCOSTS ! Shipping Detail File                     *~
            * #9  ! APCPLNSD ! Master Planning Schedule File            *~
            * #10 ! APCEMPDT ! Employee Master Detail                   *~
            * #11 ! APCEMPMT ! Employee Master Header Data              *~
            * #12 ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #13 ! SHPLINES ! Shipment Scheduling Lines                *~
            * #14 ! AWDAPPLS ! Appian Shipping Labels           (AWD004)*~
            * #15 ! AWDPLNSR ! Screen Production File    (2017/04/01)   *~
            * #16 ! AWDPLNAD ! MFG Master Audit File EXTRA AUDIT CART   *~
            * #17 ! PGORATTR ! Ply Gem Order Attribute Trigger  CR2735  *~
            * #18 ! PGSCHDTR ! Ply Gem Schedule Header Trigger  CR2735  *~
            * #19 ! PGORLNTR ! Ply Gem Order Line Trigger       CR2735  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1, "EWDLABSP",                                       ~
                        varc,     indexed,  recsize =   102,             ~
                        keypos =    1, keylen = 41,                      ~
                        alt key  1, keypos = 42, keylen =  41
                                                         /* (AWD003)    */
            select #2, "EWDPRDLB",                                       ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23
                                                         /* (AWD003)    */
            select #3, "EWDPRDQQ",                                       ~
                        varc,     indexed,  recsize = 32,                ~
                        keypos =    1, keylen =   18

            select #4, "AWDPLNRK",                                       ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =  12,                    ~
                        alt key  1, keypos = 13, keylen =  14

           select  #5,  "EWDGLSXX",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =  1,   keylen =  32,                     ~
                        alt key  1, keypos  =    33, keylen = 12

           select  #6, "APCEDIMC",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   1 , keylen =  22,                     ~
                        alt key    1 , keypos =  10, keylen =  13, dup,  ~
                            key    2 , keypos = 233, keylen =  06, dup

           select  #7,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 19,   keylen =  33,                     ~
                        alt key  1, keypos  =     1, keylen = 33

            select #8,  "SHPCOSTS",                                      ~
                        varc,     indexed,  recsize =  1500,             ~
                        keypos =    1, keylen =  23
/* (AWD002) - Mod to key and reclen */
            select #9,  "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =    1, keylen =  23

            select #10, "APCEMPDT",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                         keypos =    1, keylen =  18,                    ~
                        alt key 1, keypos = 114, keylen = 6, dup

            select #11, "APCEMPMT",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =  13,                     ~
                        alt key 1, keypos =  4, keylen = 10, dup

            select #12, "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  28

            select #13, "SHPLINES",                                      ~
                        varc,     indexed, recsize  =  600,              ~
                        keypos =   10, keylen =  22
                                                       /* (AWD001)      */
                                                       /* (AWD004)      */
            select #14, "AWDAPPLS",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   20,                    ~
                        alt key 1,  keypos = 21,   keylen =  34, dup ,   ~
                            key 2,  keypos = 23,   keylen =  32, dup ,   ~
                            key 3,  keypos = 56,   keylen =  10, dup
                                                       /* (AWD004)      */
                                                       /* (AWD001)      */

            select #15,  "AWDPLNSR",                                     ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =   42, keylen =   12,                    ~
                        alt key  1, keypos =    7, keylen =  47,         ~
                            key  2, keypos  = 163, keylen =  13,         ~
                            key  3, keypos =   1, keylen =  53,          ~
                            key  4, keypos = 205, keylen =  12, dup

            select #16,  "AWDPLNAD",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 19,   keylen = 33,                      ~
                        alt key 1, keypos =  1, keylen = 33

/* CR2735 */
            select #17, "PGORATTR",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    21, keylen =   44,                   ~
                        alt key 1, keypos = 1, keylen = 64,              ~
                            key 2, keypos = 54, keylen = 11, dup  
                            
            select #18, "PGSCHDTR",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    21, keylen =   44,                   ~
                        alt key 1, keypos = 1, keylen = 64,              ~
                            key 2, keypos = 54, keylen = 11, dup  
                            
            select #19, "PGORLNTR",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    21, keylen =   44,                   ~
                        alt key 1, keypos = 1, keylen = 64,              ~
                            key 2, keypos = 54, keylen = 11, dup  
/* CR2735 */

            call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "EWDLABSP" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPRDLB" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPRDQQ" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDPLNRK" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDGLSXX" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCEDIMC" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNAD" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SHPCOSTS" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSD" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCEMPDT" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCEMPMT" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SHPHDRS" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SHPLINES" : call "EWDOPEN" (#13, filename$, err%)
            if err% <> 0% then gosub open_error
                                                    /* (AWD001)        */
            filename$ = "AWDAPPLS" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error
                                                    /* (AWD001)        */
            filename$ = "AWDPLNSR" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDPLNAD" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error
        REM    call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),  0%,  rslt$(1%))
/* CR2735 */
            call "OPENCHCK" (#17, fs%(17%), f2%(17%),500%, rslt$(17%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%),500%, rslt$(18%))
            call "OPENCHCK" (#19, fs%(19%), f2%(19%),500%, rslt$(19%))
                        
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            call "TIME" (time$)
            pg_dte$ = date
                                               /* More than 7 Days Old  */
            call "DATE" addr("G+",pg_dte$, -7%,pg_dte1$,err%)
                                               /* More than 14 Days Old */
            call "DATE" addr("G+",pg_dte$,-14%,pg_dte2$,err%)
                                               /* More than 30 Days Old */
            call "DATE" addr("G+",pg_dte$,-30%,pg_dte3$,err%)
                                               /* More than 120 Days Old */
            call "DATE" addr("G+",pg_dte$,-120%,pg_dte4$,err%)
                                               /* More than 14 Days Old */
            call "DATE" addr("G+",pg_dte$,-14%,pg_dte5$,err%)
/* CR2735 */
                                               /* More than 365 Days Old */
            call "DATE" addr("G+",pg_dte$, -365%,pg_dte6$,err%)
            
            ss$(1%)  = "(EWDLABSP)-Lowe's Special Labels (14+)  "
            ss$(2%)  = "(EWDPRDLB)-Production MFG Labels (30+)  "
            ss$(3%)  = "(EWDPRDQQ)-Production Tot P.O.s  (7+)   "
            ss$(4%)  = "(AWDPLNRK)-Glass Rack Labels     (7+)   "
            ss$(5%)  = "(EWDGLSXX)-Glass Production Lab's(7+)   "
            ss$(6%)  = "(APCEDIMC)-EDI Master Database   (14+)  "
            ss$(7%)  = "(APCPLNAD)-MFG Scanning Audit File(30+) "
            ss$(8%)  = "(SHPCOSTS)-Link to shplines - S.O.      "
            ss$(9%)  = "(APCPLNSD)-Master Schedule File - S.O.  "
            ss$(10%) = "(APCEMPDT)-Master Time Clock Detail(Wk) "
            ss$(11%) = "(SHPHDRS) -Un-Shipped/Invoiced SO (120+)"
            ss$(12%) = "(AWDAPPLS)-Appian Shipping labels (14+) "
            ss$(13%) = "(AWDPLNSR)-Screen Production Data (30+) "
            ss$(14%) = "(AWDPLNAD)-Cart Auditing          (30+) "
/* CR2735 */
            ss$(15%) = "(PGORATTR)-Atlas Attribute Trigr  (365+) "
            ss$(16%) = "(PGSCHDTR)-Atlas Schedule Trigr   (365+) "
            ss$(17%) = "(PGORLNTR)-Atlas Order Line Trigr (365+) "
/* CR2735 */
                                    
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   17%
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
                      if keyhit% <> 0% then       L10130
L10230:               gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 14% then gosub purge_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 17% then editpg1
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
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
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

        scrn1_msg  :  data                                                   ~
         "Enter a Valid Purge Selection for (EWDLABSP) Lowe's Labels (Y/N)?",~
         "Enter a Valid Purge Selection for (EWDPRDLB) MFG Labels (Y/N)?   ",~
         "Enter a Valid Purge Selection for (EWDPRDQQ) MFG Totals (Y/N)?   ",~
         "Enter a Valid Purge Selection for (AWDPLNRK) Rack Labels (Y/N)?  ",~
         "Enter a Valid Purge Selection for (EWDGLSXX) Glass Labels (Y/N)? ",~
         "Enter a Valid Purge Selection for (APCEDIMC) EDI DATABASE (Y/N)? ",~
         "Enter a Valid Purge Selection for (APCPLNAD) MFG Audit File(Y/N)?",~
         "Enter a Valid Purge Selection for (SHPCOSTS) Link File S.O. No.? ",~
         "Enter a Valid Purge Selection for (APCPLNSD) Master Sched S.O. No.?",~
         "Enter a Valid Purge Selection for (APCEMPDT) Master Time Clock(Y/N)?",~
         "Enter a Valid Purge Selection for (SHPHDRS)  Un Shipped/Invoic(Y/N)?",~
         "Enter a Valid Purge Selection for (AWDAPPLS) Appian Ship Labels(Y/N)?",~
         "Enter a Valid Purge Selection for (AWDPLNSR) Screen Prd Data(Y/N)?",~
         "Enter a Valid Purge Selection for (AWDPLNAD) Cart Audit(Y/N)?",~
/* CR2735 */  ~
         "Enter a Valid Purge Selection for (PGORATTR) Attribute(Y/N)?", ~
         "Enter a Valid Purge Selection for (PGSCHDTR) Schedule(Y/N)?", ~
         "Enter a Valid Purge Selection for (PGORLNTR) Order Line(Y/N)?"
/* CR2735 */                           

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
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sel$(), sel_d$(), sel8$,   ~
                      sel9$, lookup_key$, purge_key$
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************


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
              gosub L40170

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

/* CR2735 Layout adjusted for new files */
L40190:     gosub set_pf1
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,23), fac(hex(a4)), apc$                   , ch(35),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Clean-Up (EWDLABSP) Y/N:",                   ~
               at (03,30), fac(lfac$(1%)), sel$(1%)             , ch(01),~
               at (03,40), fac(hex(84)), sel_d$(1%)             , ch(40),~
                                                                         ~
               at (04,02), "Clean-Up (EWDPRDLB) Y/N:",                   ~
               at (04,30), fac(lfac$(2%)), sel$(2%)             , ch(01),~
               at (04,40), fac(hex(84)), sel_d$(2%)             , ch(40),~
                                                                         ~
               at (05,02), "Clean-Up (EWDPRDQQ) Y/N:",                   ~
               at (05,30), fac(lfac$(3%)), sel$(3%)             , ch(01),~
               at (05,40), fac(hex(84)), sel_d$(3%)             , ch(40),~
                                                                         ~
               at (06,02), "Clean-Up (AWDPLNRK) Y/N:",                   ~
               at (06,30), fac(lfac$(4%)), sel$(4%)             , ch(01),~
               at (06,40), fac(hex(84)), sel_d$(4%)             , ch(40),~
                                                                         ~
               at (07,02), "Clean-Up (EWDGLSXX) Y/N:",                   ~
               at (07,30), fac(lfac$(5%)), sel$(5%)             , ch(01),~
               at (07,40), fac(hex(84)), sel_d$(5%)             , ch(40),~
                                                                         ~
               at (08,02), "Clean-Up (APCEDIMC) Y/N:",                   ~
               at (08,30), fac(lfac$(6%)), sel$(6%)             , ch(01),~
               at (08,40), fac(hex(84)), sel_d$(6%)             , ch(40),~
                                                                         ~
               at (09,02), "Clean-Up (APCPLNAD) Y/N:",                   ~
               at (09,30), fac(lfac$(7%)), sel$(7%)             , ch(01),~
               at (09,40), fac(hex(84)), sel_d$(7%)             , ch(40),~
                                                                         ~
               at (10,02), "Clean-Up (SHPCOSTS)S.O.:",                   ~
               at (10,30), fac(lfac$(8%)), sel8$                , ch(08),~
               at (10,40), fac(hex(84)), sel_d$(8%)             , ch(40),~
                                                                         ~
               at (11,02), "Clean-Up (APCPLNSD)S.O.:",                   ~
               at (11,30), fac(lfac$(9%)), sel9$                , ch(08),~
               at (11,40), fac(hex(84)), sel_d$(9%)             , ch(40),~
                                                                         ~
               at (12,02), "Clean-Up (APCEMPDT) Y/N:",                   ~
               at (12,30), fac(lfac$(10%)), sel$(10%)           , ch(01),~
               at (12,40), fac(hex(84)), sel_d$(10%)            , ch(40),~
                                                                         ~
               at (13,02), "Clean-Up (SHPHDRS) Y/N:",                    ~
               at (13,30), fac(lfac$(11%)), sel$(11%)           , ch(01),~
               at (13,40), fac(hex(84)), sel_d$(11%)            , ch(40),~
                                                                         ~
               at (14,02), "Clean-Up (AWDAPPLS)Y/N:",                    ~
               at (14,30), fac(lfac$(12%)), sel$(12%)           , ch(01),~
               at (14,40), fac(hex(84)), sel_d$(12%)            , ch(40),~
                                                                         ~
               at (15,02), "Clean-Up (AWDPLNSR)Y/N:",                    ~
               at (15,30), fac(lfac$(13%)), sel$(13%)           , ch(01),~
               at (15,40), fac(hex(84)), sel_d$(13%)            , ch(40),~
                                                                         ~
               at (16,02), "Clean-Up (AWDPLNAD)Y/N:",                    ~
               at (16,30), fac(lfac$(14%)), sel$(14%)           , ch(01),~
               at (16,40), fac(hex(84)), sel_d$(14%)            , ch(40),~
                                                                         ~
               at (17,02), "Clean-Up (PGORATTR)Y/N:",                    ~
               at (17,30), fac(lfac$(15%)), sel$(15%)           , ch(01),~
               at (17,40), fac(hex(84)), sel_d$(15%)            , ch(40),~
                                                                         ~
               at (18,02), "Clean-Up (PGSCHDTR)Y/N:",                    ~
               at (18,30), fac(lfac$(16%)), sel$(16%)           , ch(01),~
               at (18,40), fac(hex(84)), sel_d$(16%)            , ch(40),~
                                                                         ~
               at (19,02), "Clean-Up (PGORLNTR)Y/N:",                    ~
               at (19,30), fac(lfac$(17%)), sel$(17%)           , ch(01),~
               at (19,40), fac(hex(84)), sel_d$(17%)            , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15 then goto L40200
                  call "PRNTSCRN"
                  goto L40190

L40200: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

set_pf1:
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                      (14)Purge Data   "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)

            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
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
            if fieldnr% = 8% then gosub L50100
            if fieldnr% = 9% then gosub L50200
            if fieldnr% < 8% then gosub L50010 /* Selection Code Y or N   */
            if fieldnr% > 9% then gosub L50010 /* Y or N                  */

            return

L50010: REM Selection Code                   sel$, sel_d$
            if sel$(fieldnr%) <> " " then goto L50015
                sel$(fieldnr%) = "N"

L50015:     if sel$(fieldnr%) <> "Y" and sel$(fieldnr%) <> "N"        ~
                                         then goto L50020
            if sel$(2%) = "Y" then sel$(3%) = "Y"

            sel_d$(fieldnr%) = str(ss$(fieldnr%),12%)
        return
L50020:     errormsg$ = "(Error) Purge Selection? (Y)es or (N)o"
            gosub error_prompt
            init(" ") sel$(fieldnr%), sel_d$(fieldnr%)
        return

L50100: REM Selection for SHPCOSTS                     sel8$
            lookup_key$ = all(hex(00))
            if sel8$ <> " " then goto L50120
               sel8$ = "00000000"
               goto L50140
L50120:     if sel8$ = "00000000" then goto L50140
            lookup_key$ = sel8$
            read #8,key > lookup_key$, using L50130, lookup_key$,     ~
                                                eod goto L50150
L50130:        FMT CH(23)
            call "SHOSTAT" ("Lookup Key = " & lookup_key$ & "  Sel = " &sel8$)
            stop

            if sel8$ <> str(lookup_key$,1%,8%) then goto L50150
L50140:     sel_d$(8%) = "(SHPCOSTS) Purge less than " & sel8$

        return
L50150:     errormsg$ = "(Error) Invalid Sales Order Entry?"
            gosub error_prompt
            init(" ") sel8$, sel_d$(fieldnr%)
        return


L50200: REM Selection for APCPLNSD                     sel9$
            lookup_key$ = all(hex(00))
            if sel9$ <> " " then goto L50220
               sel9$ = "00000000"
               goto L50240
L50220:     if sel9$ = "00000000" then goto L50240
            lookup_key$ = sel9$
            read #9,key > lookup_key$, using L50230, lookup_key$,     ~
                                                eod goto L50250
L50230:        FMT CH(23)
            call "SHOSTAT" ("Lookup Key = " & lookup_key$ & "  Sel = " &sel9$)
            stop


            if sel9$ <> str(lookup_key$,1%,8%) then goto L50250
L50240:     sel_d$(9%) = "(APCPLNSD) Purge less than " & sel9$

        return
L50250:     errormsg$ = "(Error) Invalid Sales Order Entry?"
            gosub error_prompt
            init(" ") sel9$, sel_d$(fieldnr%)
        return


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        purge_data
            cnt$ = "Records Deleted = [xxxxxxxx]"
            if sel$(1%) = "Y" then gosub purge_ewdlabsp
            if sel$(2%) = "Y" then gosub purge_ewdprdlb
            if sel$(4%) = "Y" then gosub purge_awdplnrk
            if sel$(5%) = "Y" then gosub purge_ewdglsxx
            if sel$(6%) = "Y" then gosub purge_apcedimc
            if sel$(7%) = "Y" then gosub purge_apcplnad
            if sel8$ <> "00000000" then gosub purge_shpcosts
            if sel9$ <> "00000000" then gosub purge_apcplnsd
            if sel$(10%)= "Y" then gosub purge_time_clock
            if sel$(11%)= "Y" then gosub purge_shphdrs
            if sel$(12%)= "Y" then gosub purge_awdappls  /* (AWD001)  */
            if sel$(13%)= "Y" then gosub purge_awdplnsr  /*2017/04/01 */
            if sel$(14%)= "Y" then gosub purge_awdplnad  /*2017/04/01 */
            if sel$(15%)= "Y" then gosub purge_attr      /* CR2735 */
            if sel$(16%)= "Y" then gosub purge_schd      /* CR2735 */
            if sel$(17%)= "Y" then gosub purge_orln      /* CR2735 */
            
        return clear all
        goto editpg1

        purge_ewdlabsp
            call "SHOSTAT" ("Purging Data in " & str(ss$(1%),1%,10%) )
            purge_key$ = all(hex(00))
            cnt% = 0%
        purge_ewdlabsp_nxt
            read #1,hold,key > purge_key$, using L60000, purge_key$,     ~
                                           eod goto purge_ewdlabsp_done
L60000:        FMT CH(41)
            if mod(cnt%,50%) <> 0 then goto L60010
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only Keep (14) Days */
L60010:     if str(purge_key$,1%,6%) > pg_dte2$ then goto purge_ewdlabsp_done
                delete #1
                cnt% = cnt% + 1%
                goto purge_ewdlabsp_nxt
        purge_ewdlabsp_done
           sel_d$(1%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(1%),21%,8%), pic(########)

        return

        purge_ewdprdlb
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(2%),1%,10%) )
            purge_key$ = all(hex(00))
            cnt% = 0% : cnt1% = 0%
        purge_ewdprdlb_nxt                        /* (AWD003) No Change */
            read #2,hold,key > purge_key$, using L60100, purge_key$,     ~
                                   barcode$, eod goto purge_ewdprdlb_done
L60100:        FMT CH(35), POS(278), CH(18)
                                                  /* (AWD003) No Change */
            if mod(cnt%,50%) <> 0 then goto L60110
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                              /* Only Keep (7) Days   */
L60110:     if str(purge_key$,1%,6%) > pg_dte3$ then goto purge_ewdprdlb_done
                delete #2
                cnt% = cnt% + 1%
                gosub purge_ewdprdqq
                goto purge_ewdprdlb_nxt
        purge_ewdprdlb_done
           sel_d$(2%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(2%),21%,8%), pic(########)

           sel_d$(3%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt1% to str(sel_d$(3%),21%,8%), pic(########)

        return

        purge_ewdprdqq
           if sel$(3%) = "N" then return
              read #3,hold,key = barcode$, eod goto L60120
                 delete #3
                 cnt1% = cnt1% + 1%
L60120: return

        purge_awdplnrk
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(4%),1%,10%) )
            purge_key$ = all(hex(00))
            cnt% = 0%
        purge_awdplnrk_nxt
            read #4,hold,key 1% > purge_key$, using L60200, purge_key$,     ~
                                           eod goto purge_awdplnrk_done
L60200:        FMT POS(13), CH(14)
            if mod(cnt%,50%) <> 0 then goto L60210
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only keep (7) Days  */
L60210:     if str(purge_key$,1%,6%) > pg_dte1$ then goto purge_awdplnrk_done
                delete #4
                cnt% = cnt% + 1%
                goto purge_awdplnrk_nxt
        purge_awdplnrk_done
           sel_d$(4%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(4%),21%,8%), pic(########)

        return

        purge_ewdglsxx
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(5%),1%,10%) )
            purge_key$ = all(hex(00))
            cnt% = 0%
        purge_ewdglsxx_nxt
            read #5,hold,key > purge_key$, using L60300, purge_key$,     ~
                                           eod goto purge_ewdglsxx_done
L60300:        FMT CH(32)
            if mod(cnt%,50%) <> 0 then goto L60310
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only Keep (7) Days */
L60310:     if str(purge_key$,2%,6%) > pg_dte1$ then goto purge_ewdglsxx_nxt
                delete #5
                cnt% = cnt% + 1%
                goto purge_ewdglsxx_nxt
        purge_ewdglsxx_done
           sel_d$(5%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(5%),21%,8%), pic(########)

        return

        purge_apcedimc
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(6%),1%,10%) )
            purge_key$ = all(hex(00))
            cnt% = 0%
        purge_apcedimc_nxt
            read #6,hold,key > purge_key$, using L60400, purge_key$,     ~
                                 purge_dte$, eod goto purge_apcedimc_done
L60400:        FMT CH(22), POS(233), CH(6)
            if mod(cnt%,50%) <> 0 then goto L60410
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only Keep (14) Days */
L60410:     if purge_dte$ > pg_dte2$ then goto purge_apcedimc_nxt
                delete #6
                cnt% = cnt% + 1%
                goto purge_apcedimc_nxt
        purge_apcedimc_done
           sel_d$(6%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(6%),21%,8%), pic(########)

        return

        purge_apcplnad
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(7%),1%,10%) )
            purge_key$ =all(hex(00))
            cnt% = 0%
        purge_apcplnad_nxt
            read #7,hold,key > purge_key$, using L60500, purge_key$,     ~
                                           eod goto purge_apcplnad_done
L60500:        FMT POS(19), CH(33)
            if mod(cnt%,50%) <> 0 then goto L60510
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only Keep (30) Days */
L60510:     if str(purge_key$,1%,6%) > pg_dte3$ then goto purge_apcplnad_done
                delete #7
                cnt% = cnt% + 1%
                goto purge_apcplnad_nxt
        purge_apcplnad_done
           sel_d$(7%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(7%),21%,8%), pic(########)

        return

        purge_shpcosts
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(8%),1%,10%) )
            purge_key$ =all(hex(00))
            cnt% = 0%
        purge_shpcosts_nxt
            read #8,hold,key > purge_key$, using L60600, purge_key$,     ~
                                           eod goto purge_shpcosts_done
L60600:        FMT CH(23)
            if mod(cnt%,50%) <> 0 then goto L60610
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only Keep (30) Days */
L60610:     if str(purge_key$,1%,8%) > sel8$ then goto purge_shpcosts_done
                delete #8
                cnt% = cnt% + 1%
                goto purge_shpcosts_nxt
        purge_shpcosts_done
           sel_d$(8%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(8%),21%,8%), pic(########)

        return

        purge_apcplnsd
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(9%),1%,10%) )
            purge_key$ =all(hex(00))
            cnt% = 0%
        purge_apcplnsd_nxt
            read #9,hold,key > purge_key$, using L60700, purge_key$,     ~
                                           eod goto purge_apcplnsd_done
L60700:        FMT CH(23)
            if mod(cnt%,50%) <> 0 then goto L60710
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only Keep (30) Days */
L60710:     if str(purge_key$,1%,8%) > sel9$ then goto purge_apcplnsd_done
                delete #9
                cnt% = cnt% + 1%
                goto purge_apcplnsd_nxt
        purge_apcplnsd_done
           sel_d$(9%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(9%),21%,8%), pic(########)

        return

        purge_time_clock
            cnt$ = "DTL Rec Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(10),1%,10%) )

            sc_yr$ = "2015"
            sc_wk$ = "53"

            cnt% = 0%
            purge_key$ = all(hex(00))

            x% = 0%
            convert sc_yr$ to x%, data goto L60800
L60800:
            put sc_yr_bi$ using L60810, x%
L60810:         FMT BI(2)

        REM     str(purge_key$,1%,2%) = sc_yr_bi$
        purge_dt
             read #10,hold,key > purge_key$, using  L60820, purge_key$,     ~
                                                             eod goto L60860
L60820:          FMT CH(18)
             if mod(cnt1%,50%) <> 0 then goto L60840
                convert cnt% to str(cnt$,20%,8%), pic(########)
                print at(02,26);hex(84);cnt$;

L60840:         if str(purge_key$,1%,2%) < sc_yr_bi$ then goto L60850
                if str(purge_key$,1%,2%) <> sc_yr_bi$ then goto L60860

                if str(purge_key$,3%,2%) > sc_wk$  then goto purge_dt
L60850:                                             /* Purge Detail Data */
                   delete #10
                   cnt% = cnt% + 1%
                   goto purge_dt

L60860:
             cnt$ = "MST Rec Deleted = [xxxxxxxx]"
             purge_key$ = all(hex(00))
        REM     str(purge_key$,1%,2%) = sc_yr_bi$
             call "SHOSTAT" ( "Purging Master Data" )
             read #11,hold,key 1% > purge_key$, using  L60870, purge_key$,  ~
                                                         eod goto purge_done
             goto L60870
        purge_mt
             read #11, hold, using  L60870, purge_key$, eod goto purge_done
L60870:          FMT POS(4), CH(10)
             if mod(cnt%,50%) <> 0 then goto L60880
                print at(03,38);hex(84);"[";cnt%;"]"

L60880:      if str(purge_key$,1%,2%) <  sc_yr_bi$ then goto L60890
             if str(purge_key$,1%,2%) <> sc_yr_bi$ then goto purge_done

             if str(purge_key$,8%,2%) > sc_wk$ then goto purge_mt
L60890:                                           /* Purge Header Data */
                delete #11
                cnt% = cnt% + 1%
                goto purge_mt
        purge_done
           sel_d$(10%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(10),21%,8%), pic(########)
        return

        purge_shphdrs
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(11%),1%,10%) )
            purge_key$ =all(hex(00))
            cnt% = 0%
        purge_shphdrs_nxt
            read #12,hold,key > purge_key$, using L60900, purge_key$,     ~
                          purge_dte$, inv_no$, eod goto purge_shphdrs_done
L60900:        FMT CH(28), XX(3), CH(6), POS(233), CH(8)
            if mod(cnt%,50%) <> 0 then goto L60910
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only Keep (180) Days */
L60910:     if purge_dte$ > pg_dte4$ then goto purge_shphdrs_nxt
                delete #12
                cnt% = cnt% + 1%

                gosub purge_shplines

                goto purge_shphdrs_nxt
        purge_shphdrs_done
           sel_d$(11%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(11%),21%,8%), pic(########)

        return

        purge_shplines
           purge_key1$ = all(hex(00))
           str(purge_key1$,1%,19%) = str(purge_key$,10%,19%)
        purge_shplines_nxt
           read #13,hold,key > purge_key1$, using L61000, purge_key1$, ~
                                            eod goto purge_shplines_done
L61000:       FMT CH(22)
           if str(purge_key1$,1%,19%) <> str(purge_key$,10%,19%) Then  ~
                                                goto purge_shplines_done
              delete #13
              goto purge_shplines_nxt
        purge_shplines_done

        return
                                                   /* (AWD001)          */
                                                   /* (AWD004)          */
        purge_awdappls
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(12%),1%,10%))
            purge_key$ = all(hex(00))
            cnt% = 0% : cnt1% = 0%
        purge_awdappls_nxt
            read #14,hold,key > purge_key$, using L61100, purge_key$,     ~
                        prd_dte$, create_dte$,                            ~
                              eod goto purge_awdappls_done

L61100:        FMT CH(20), POS(629), CH(06), CH(06)

            if mod(cnt%,50%) <> 0 then goto L61110
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
L61110:

            if prd_dte$ = " " then prd_dte$ = all(hex(00))

                                                     /* 30 Days (AWD004) */
            if prd_dte$ > pg_dte3$ then goto purge_awdappls_nxt
                                                     /* 120 Days(AWD004) */
REM            if create_dte$ > pg_dte4$ then goto purge_awdappls_nxt

                delete #14
                cnt% = cnt% + 1%
                goto purge_awdappls_nxt
        purge_awdappls_done
           sel_d$(12%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(12%),21%,8%), pic(########)

        return

/* 2017/04/01 */
        purge_awdplnsr
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(13%),1%,10%) )
            purge_key$ =all(hex(00))
            cnt% = 0%
        purge_awdplnsr_nxt
            read #15,hold,key 3% > purge_key$, using L62100, purge_key$,     ~
                                           eod goto purge_awdplnsr_done
L62100:        FMT CH(53)
            if mod(cnt%,50%) <> 0 then goto L62110
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only Keep (30) Days */
L62110:     if str(purge_key$,1%,6%) > pg_dte3$ then goto purge_awdplnsr_done
                delete #15
                cnt% = cnt% + 1%
                goto purge_awdplnsr_nxt
        purge_awdplnsr_done
           sel_d$(13%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(13%),21%,8%), pic(########)
        return


        purge_awdplnad
            cnt$ = "Records Deleted = [xxxxxxxx]"
            call "SHOSTAT" ("Purging Data in " & str(ss$(14%),1%,10%) )
            purge_key$ =all(hex(00))
            cnt% = 0%
        purge_awdplnad_nxt
            read #16,hold,key > purge_key$, using L60500, purge_key$,     ~
                                           eod goto purge_awdplnad_done
            if mod(cnt%,50%) <> 0 then goto L62140
               convert cnt% to str(cnt$,20%,8%), pic(########)
               print at(02,26);hex(84);cnt$;
                                                /* Only Keep (30) Days */
L62140:     if str(purge_key$,1%,6%) > pg_dte3$ then goto purge_awdplnad_done
                delete #16
                cnt% = cnt% + 1%
                goto purge_awdplnad_nxt
        purge_awdplnad_done
           sel_d$(14%) = "Records Deleted = [ xxxxxxxx ]       "
           convert cnt% to str(sel_d$(14%),21%,8%), pic(########)

        return
/* 2017/04/01 - */

/* CR2735 PG Trigger Purge routines */
REM------------------------------------------------------------------------
REM    Purge attribute processed records                                  -
REM------------------------------------------------------------------------       
       purge_attr 
          call "SHOSTAT" ("Purging Attributes ")
          init(" ") tr_rec$
          tr_key$ = all(hex(00))
          acnt% = 0%
          str(tr_key$,1%,20%) = "ORDERATTR           "     
          str(tr_key$,21%,1%) = "0"
           
          read #17, hold, key 1% > tr_key$, using L01000, tr_rec$, ~
                eod goto L01999
          goto L01100
          
       purge_nxt_attr 
          read #17, hold, using L01000, tr_rec$, eod goto L01999         
L01000:      FMT CH(256)
L01100:
          if str(tr_rec$,21%,1%) <> "1" then goto L01250
          if str(tr_rec$,22%,6%) > pg_dte6$ then goto L01999   

          delete #17 
          acnt% = acnt% + 1
          if mod(acnt%,1000%) <> 0 then goto L01250
          convert acnt% to str(cnt$,20%,8%),  pic(########)        
          print at(02,26);hex(84);cnt$;
          
L01250:   goto purge_nxt_attr
          
L01999:   
           sel_d$(15%) = "Records Deleted = [ xxxxxxxx ]       "
           convert acnt% to str(sel_d$(15%),21%,8%), pic(########)
       return

REM------------------------------------------------------------------------
REM    Purge schedule header processed records                            -
REM------------------------------------------------------------------------       
       purge_schd
          call "SHOSTAT" ("Purging Schedule ")
          init(" ") tr_rec$
          tr_key$ = all(hex(00))
          scnt% = 0%
          str(tr_key$,1%,20%) = "SCHDHDR             "
          str(tr_key$,21%,1%) = "0"
          
          read #18, hold, key 1% > tr_key$, using L01000, tr_rec$, ~
                 eod goto L02999
          goto L02100
                    
       purge_nxt_schd
          read #18, hold, using L01000, tr_rec$, eod goto L02999    
L02100:
          if str(tr_rec$,21%,1%) <> "1" then goto L02250
          if str(tr_rec$,22%,6%) > pg_dte6$ then goto L02999   

          delete #18 
          scnt% = scnt% + 1
          if mod(scnt%,1000%) <> 0 then goto L02250
          convert scnt% to str(cnt$,20%,8%),  pic(########)        
          print at(02,26);hex(84);cnt$;
          
L02250:   goto purge_nxt_schd
          
L02999:   
           sel_d$(16%) = "Records Deleted = [ xxxxxxxx ]       "
           convert scnt% to str(sel_d$(16%),21%,8%), pic(########)
       return
    
REM------------------------------------------------------------------------
REM    Purge order line processed records                                 -
REM------------------------------------------------------------------------       
       purge_orln
          call "SHOSTAT" ("Purging Order Lines ")       
          init(" ") tr_rec$
          tr_key$ = all(hex(00))
          ocnt% = 0%
          str(tr_key$,1%,20%) = "ORDERLINE           "
          str(tr_key$,21%,1%) = "0"
          
          read #19, hold, key 1% > tr_key$, using L01000, tr_rec$, ~
                eod goto L03999
          goto L03100
          
       purge_nxt_orln
          read #19, hold, using L01000, tr_rec$, eod goto L03999    
L03100:
          if str(tr_rec$,21%,1%) <> "1" then goto L03250
          if str(tr_rec$,22%,6%) > pg_dte6$ then goto L03999    

          delete #19 
          ocnt% = ocnt% + 1
          if mod(ocnt%,1000%) <> 0 then goto L03250
          convert ocnt% to str(cnt$,20%,8%),  pic(########)        
          print at(02,26);hex(84);cnt$;

          
L03250:   goto purge_nxt_orln
          
L03999:   
           sel_d$(17%) = "Records Deleted = [ xxxxxxxx ]       "
           convert ocnt% to str(sel_d$(17%),21%,8%), pic(########)
        return

                                                     /* (AWD001)       */
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
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            end

