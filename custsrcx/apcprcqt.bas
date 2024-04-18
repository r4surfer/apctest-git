        REM *************************************************************~
            *                  As Of (03/04/2020)                       *~
            *   AAA   PPPP    CCC   PPPP   RRRR    CCC    QQQ   TTTTT   *~
            *  A   A  P   P  C   C  P   P  R   R  C   C  Q   Q    T     *~
            *  AAAAA  PPPP   C      PPPP   RRRR   C      Q   Q    T     *~
            *  A   A  P      C   C  P      R R    C   C  Q   Q    T     *~
            *  A   A  P       CCC   P      R  R    CCC    QQQ     T     *~
            *                                                Q          *~
            *-----------------------------------------------------------*~
            * APCPRCQT - Price quote for Specified Customer and Part.   *~
            *            Steps (1) The Subroutine (APCPR0SB) is Called. *~
            *                  (2) A Valid MFG Part is Built with       *~
            *                      checking against the Master Validity *~
            *                      File.                                *~
            *                  (3) With a Valid MFG Part the Opening    *~
            *                      and Exact Size are Calculated using  *~
            *                      the Defined Standard Deductions.     *~
            *                  (4) The APC Catalog Price is Calculated  *~
            *                      based on Pricing Definitions.        *~
            *                  (5) If applicable a Customers Special    *~
            *                      Price is Calculated.                 *~
            *                  (6) When Applicable the Customers        *~
            *                      Discount is applied to the Calculated*~
            *                      APC Catalog Price.                   *~
            *                                                           *~
            * Display Screen - Shows the Reference Description and the  *~
            *                  Price on the (Left Side) of Screen for   *~
            *                  the APC Catalog. Shows the Reference     *~
            *                  Description and the Price on the         *~
            *                  (Right Side) of Screen for the Customer  *~
            *                  Special Pricing When Applicable.         *~
            *                                                           *~
            * Subroutines  - (1) APCPR0SB - Build MFG Part              *~
            *                (2) APCPR1SB - Table Lookups               *~
            *                (3) APCPRSUB - Price Calc Driver Sub       *~
            *                (4) APCPR5SB - Calc Standard Deduction     *~
            *                (5) APCPR4SB - Check for EDI Price         *~
            *                (6) APCPRXSB - Private Label Series Code   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/31/94 ! Original                                 ! RHH *~
            * 03/21/97 ! Mods to Upgrade for Unix and New Pricing ! RHH *~
            *          !   Modifications.                         !     *~
            * 10/31/97 ! Check for Upgrade to R6.04.03            ! RHH *~
            * 09/21/98 ! Minor bug fix to display_price - EWD001. ! BWS *~
            *          ! Also mod init of err% to kill false msg. !     *~
            * 05/27/99 ! (EWD001) Verify Promotional Pricing      ! RHH *~
            *          !   and make mods for (New) Grid Promotion !     *~
            * 01/20/00 ! (EWD002) Mods for Samp/Disp/Lit          ! RHH *~
            * 07/19/00 ! (EWD???) Mods to 'APCPRSUB' and 'APCPR2SB'!RHH *~
            *          !   for lock pricing and Coastal Mull      !     *~
            *          !   pricing                                !     *~    
            * 10/25/05 ! (AWD005) CR347 Changes for new subpart   ! CMG *~
            *12/22/2015! (SR67154) Energy Star 2016 Changes       ! CMG *~
            *03/04/2020! (CR2451) 3900 PSE Price                  ! CMN *~
            *************************************************************

        dim                                                              ~
            s_23$8, s_23m$3,             /* New Series and Model       */~
            readkey$24,                  /* DESCRIPTION LOOKUP KEY     */~
            cuscode$9,                   /* Customer Code              */~
            cust_name$30,                /* Customer Name              */~
            cust_type$2,                 /* Customer Type Code         */~
            cust_desc$32,                /* Customer Type Code Descrip */~
            key$47,                      /* Price Code Key             */~
            pc(36%),                     /* Price Code Value           */~
            code$1,                      /* Customer Price Code        */~
            pc_desc$32,                  /* Price Code Description     */~
            part$25,                     /* Part Number                */~
            subpart$20,                  /* subpart            (AWD005)*/~
            part_desc$32,                /* Part Description           */~
            price$12,                    /* Price - Customer Discount  */~
            price_cat$12,                /* Price - Catalog            */~
            mscdescr$64,                 /* Plowcode Header Description*/~
            size$1,                    /* (C)OMPONENT,(O)PENING,(E)XACT*/~
            upd$1,                       /* UPDATE PRICE (Y)ES, (N)O   */~
            sp_desc$(5%)42,              /* Special Pricing Message    */~
            build_msg$(10%)40,           /* Build Special Message      */~
            fl1$200,                     /* Price Filler 1             */~
            fl2$156,                     /* Price Filler 2             */~
            hdr$(3%)40,                  /* Screen Header              */~
            hdr1$20, hdr2$11, hdr3$20,   /* Special Screen Headings Col*/~
            hdr4$11,                     /* Special Screen Headings Col*/~
/*AWD005*/  ref$(30%)2, txt$(30%)20,     /* Ref Type Codes Catalog     */~
/*AWD005*/  ref1$(30%)2, txt1$(30%)20,   /* Ref Type Codes Spec. Cat.  */~
/*AWD005*/  ref_p(30%), txtp$(30%)10,    /* Ref Type Price APC Catalog */~
/*AWD005*/  ref_p1(30%), txtp1$(30%)10,  /* Ref Type Price Special Cust*/~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            mdllfac$1,                   /* Virtual Mdl Screen (CR2451)*/~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            model$16,                    /* virtual Model (CR2451)     */~
            mdl_desc$32,                 /* model desc (CR2451)        */~
            usr_desc$32                  /* User Security (CR2451)     */
            
          

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Customer Service Price Quote Util."
            pname$ = "APCPRCQT - Rev: R6.04"

        REM *************************************************************

            mat f2% = con

            hdr$(1%) = "****************************************"
            hdr$(2%) = "*         Customer Price Quote         *"
            hdr$(3%) = "****************************************"

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CPRPRICE ! Customer Price File (APC) Version        *~
            * #2  ! GENCODES ! General Codes File                       *~
            * #4  ! CUSTOMER ! Customer Master FILE                     *~
            * #5  ! HNYMASTR ! Part Master File                         *~
            * #6  ! APCPCMST ! Pricing Master Definition File           *~
            * #7  ! AWDPCMST ! Pricing Master Definition File           *~
            * #8  ! APCSKUNO ! SKU Number Master File                   *~
            * #9  ! APCPCMSK ! Pricing (Key) and (Value) Definition File*~
            * #10 ! APCPCMSD ! Pricing Master Calc. Definition File     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CPRPRICE"                                       ~
                        varc,     indexed,  recsize = 700,               ~
                        keypos = 1,    keylen =  47

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =  1,   keylen =  24

            select #4,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =  1,   keylen =  9,                      ~
                        alt key  1, keypos  =    10, keylen = 30, dup,   ~
                            key  2, keypos  =   424, keylen =  9, dup,   ~
                            key  3, keypos  =   771, keylen =  9, dup,   ~
                            key  4, keypos  =   780, keylen =  9, dup

            select #5,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen =  25,                     ~
                        alt key  1, keypos  =   102, keylen =  9, dup,   ~
                            key  2, keypos  =    90, keylen =  4, dup,   ~
                            key  3, keypos  =    26, keylen = 32, dup

            select #6,  "APCPCMST"                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 9,    keylen =  53,                     ~
                        alt key  1, keypos  =     1, keylen = 8
                        
            select #7,  "AWDPCMST"                                       ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 9,    keylen =  53,                     ~
                        alt key  1, keypos  =     1, keylen = 8                        

            select #8,  "APCSKUNO"                                       ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos = 1,    keylen =  28,                     ~
                        alt key  1, keypos  =    29, keylen = 28, dup

            select #9,  "APCPCMSK"                                       ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 1,    keylen =   5

            select #10, "APCPCMSD"                                       ~
/*AWD005*/              varc,     indexed,  recsize =  768,              ~
                        keypos = 1,    keylen =   22

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))            
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9, fs%(9%), f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),  0%, rslt$(10%))

            mat f1% = zer

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

            build_msg$(1%) = "(Valid Part and Catalog Price   )"
            build_msg$(2%) = "(Valid Part Catalog and Special )"
            build_msg$(3%) = "(Valid Part and EDI Price       )"
            build_msg$(4%) = "(Valid Part No Price Error Cat. )"
            build_msg$(5%) = "(Valid Part Catalog, Error Spc. )"
            build_msg$(6%) = "(Invalid Part Price, No Price   )"
            build_msg$(7%) = "(Invalid Part Build, No Price   )"

            sp_desc$(1%)   = "(Quote) APC Catalog Price Discounted !!  :"
            sp_desc$(2%)   = "(Quote) Special Customer Catalog Price !!:"
            sp_desc$(3%)   = "(Quote) Special Customer EDI Price !!    :"
            sp_desc$(4%)   = "                                         :"
            sp_desc$(5%)   = "                                         :"

            sp% = 0%
            s_23% = 0%
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  2%             
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
                  if keyhit%  = 16% then goto inputmode
                  if keyhit%  = 32% then gosub save_price
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  2% then editpg1
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
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
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
         "Enter a Valid Customer Code, or <Return> for Look-Up.        ",~
         "Enter a Valid Part Number, or <?> Followed by RETURN, Look-up"


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, key$, cuscode$, part$,     ~
                      price$, cust_type$, cust_desc$, cust_name$,        ~
                      part_desc$, code$, pc_desc$, price_cat$, fl1$,     ~
                      fl2$, size$, upd$, ref$(), ref1$(), txt$(),        ~
                      txt1$(), txtp$(), txtp1$(), model$, mdl_desc$,     ~
                      mdllfac$, usr_desc$

            init(" ") subpart$                             /* (AWD005) */

            mat ref_p  = zer                  /* Zero Ref Catalog Value*/
            mat ref_p1 = zer                  /* Zero Ref Special Value*/
            mat pc = zer                      /* Zero All Prices       */
            upd$   = "N"                      /* No Update By Build    */
            price$     = "0.0000"
            price_cat$ = "0.0000"
/*EWD001*/  err% = 7%                         /* Set For Display       */

REM            cuscode$ = "AA0108"
REM            part$ = "3W12A00000950357576"
REM            subpart$ = "000A0000000000100000"

REM      cuscode$ = "AA0108"
REM      part$ = "FH12010022960520640"
REM      subpart$ = "00000400000000000000"

/* (CR2451) beg */
            mdllfac$ = hex(8c)
            virMdlSec% = 0%
            readkey$ = "PRICE 015" & userid$
            call "DESCRIBE" (#2, readkey$, usr_desc$, 0%, f1%(2))            
           
             convert str(usr_desc$,29%,2%) to virMdlSec%, data goto noPrice015
           

noPrice015: 
/* (CR2451) end */
         
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        REM DATALOAD
        REM RETURN
        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT
        REM RETURN

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

              on fieldnr% gosub L40160,         /* Customer Code     */   ~
                                L40160          /* Part Code         */
/* (CR2451) */
              if virMdlSec% = 99% and fieldnr% = 2% then mdllfac$ = hex(81)                                 
              if virMdlSec% = 99% and fieldnr% <> 2% then mdllfac$ = hex(8c)                                 
              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (02,21), fac(hex(84)), hdr$(1%)               , ch(40),~
               at (03,21), fac(hex(84)), hdr$(2%)               , ch(40),~
               at (04,21), fac(hex(84)), hdr$(3%)               , ch(40),~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Customer Code :",                            ~
               at (06,20), fac(lfac$(1%)), cuscode$             , ch(09),~
               at (06,46), fac(hex(84)), cust_name$             , ch(30),~
/* (SR67154) */                                                          ~
               at (07,02), "Part Number   :",                            ~
               at (07,20), fac(lfac$(2%)), part$                , ch(25),~
               at (07,46), fac(hex(84)), part_desc$             , ch(32),~
/* (SR67154) */                                                          ~
               at (08,02), "Subpart       :",                            ~
               at (08,20), fac(lfac$(2%)),subpart$              , ch(20),~
                                                                         ~
/*(CR2451) */  at (08,46), fac(mdllfac$), model$                , ch(16),~
                                                                         ~
               at (10,02), "Customer Type        :",                     ~
               at (10,25), fac(hex(84)),  cust_type$            , ch(02),~
               at (10,46), fac(hex(84)),  cust_desc$            , ch(32),~
               at (11,02), "Customer Price Code  :",                     ~
               at (11,25), fac(hex(84)),  code$                 , ch(01),~
               at (11,46), fac(hex(84)),  pc_desc$              , ch(32),~
                                                                         ~
               at (13,02), fac(hex(84)),  sp_desc$(sp% + 1)     , ch(42),~
               at (13,46), fac(hex(84)),  price$                , ch(12),~
                                                                         ~
               at (15,02), "Dealer Catalog Price :",                     ~
               at (15,25), fac(hex(84)),  price_cat$            , ch(12),~
                                                                         ~
               at (19,02), fac(hex(94)),  build_msg$(err% + 1)  , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40630
                  gosub display_price
                  goto L40070

L40630:        if keyhit% <> 15% then goto L40670
                  call "PRNTSCRN"
                  goto L40190

L40670:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40860     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40820
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40820:     if fieldnr% > 1% then L40840
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40840:     return

L40860: if fieldnr% > 0% then L40970  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (15)Print Screen"
            pf$(2%)= "                 (9)Display Calc.       " &        ~
                     "                       (16)Exit Price  "
            pf$(3%)= "                                        " &        ~
                     "                       (32)Save Price's"
            pfkeys$ = hex(01ffffffffffffff09ffffffffff0f102000)
            if err% = 0% or err% = 1% or err% = 4% then goto L40960
               str(pf$(3%),64%) = " "  :  str(pfkeys$,17%,1%) = hex(ff)
L40960:     return
L40970:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2%)= "                                        " &        ~
                     "                                       "
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *           S p e c i a l   P r i c e   S c r e e n         *~
            *-----------------------------------------------------------*

        display_price
            scr% = 0%
            gosub format_screen
            gosub set_pf2
L42070:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (02,21), fac(hex(84)), hdr$(1%)               , ch(40),~
               at (03,21), fac(hex(84)), hdr$(2%)               , ch(40),~
               at (04,21), fac(hex(84)), hdr$(3%)               , ch(40),~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Customer Code :",                            ~
               at (06,20), fac(hex(84)), cuscode$               , ch(09),~
/*  (SR67154)  AT (06,46), FAC(HEX(84)), CUST_NAME$             , CH(30), */ ~
               at (06,46), fac(hex(84)), part_desc$             , ch(32),~
                                                                         ~
               at (07,02), "Part Number   :",                            ~
               at (07,20), fac(hex(84)), part$                  , ch(25),~
/*(SR67154)*/  at (07,46), fac(hex(84)), subpart$               , ch(20),~
/*(CR2451)*/   at (07,67), fac(hex(84)), model$                 , ch(16),~
                                                                         ~
               at (08,06), fac(hex(a4)), hdr1$                  , ch(20),~
               at (08,28), fac(hex(a4)), hdr2$                  , ch(11),~
               at (08,41), fac(hex(a4)), hdr3$                  , ch(20),~
               at (08,63), fac(hex(a4)), hdr4$                  , ch(11),~
                                                                         ~
               at (09,06), fac(hex(84)), txt$( 1% + scr% )      , ch(20),~
               at (09,29), fac(hex(84)), txtp$( 1% + scr% )     , ch(11),~
               at (09,41), fac(hex(84)), txt1$( 1% + scr% )     , ch(20),~
               at (09,64), fac(hex(84)), txtp1$( 1% + scr% )    , ch(11),~
                                                                         ~
               at (10,06), fac(hex(84)), txt$( 2% + scr% )      , ch(20),~
               at (10,29), fac(hex(84)), txtp$( 2% + scr% )     , ch(11),~
               at (10,41), fac(hex(84)), txt1$( 2% + scr% )     , ch(20),~
               at (10,64), fac(hex(84)), txtp1$( 2% + scr% )    , ch(11),~
                                                                         ~
               at (11,06), fac(hex(84)), txt$( 3% + scr% )      , ch(20),~
               at (11,29), fac(hex(84)), txtp$( 3% + scr% )     , ch(11),~
               at (11,41), fac(hex(84)), txt1$( 3% + scr% )     , ch(20),~
               at (11,64), fac(hex(84)), txtp1$( 3% + scr% )    , ch(11),~
                                                                         ~
               at (12,06), fac(hex(84)), txt$( 4% + scr% )      , ch(20),~
               at (12,29), fac(hex(84)), txtp$( 4% + scr% )     , ch(11),~
               at (12,41), fac(hex(84)), txt1$( 4% + scr% )     , ch(20),~
               at (12,64), fac(hex(84)), txtp1$( 4% + scr% )    , ch(11),~
                                                                         ~
               at (13,06), fac(hex(84)), txt$( 5% + scr% )      , ch(20),~
               at (13,29), fac(hex(84)), txtp$( 5% + scr% )     , ch(11),~
               at (13,41), fac(hex(84)), txt1$( 5% + scr% )     , ch(20),~
               at (13,64), fac(hex(84)), txtp1$( 5% + scr% )    , ch(11),~
                                                                         ~
               at (14,06), fac(hex(84)), txt$( 6% + scr% )      , ch(20),~
               at (14,29), fac(hex(84)), txtp$( 6% + scr% )     , ch(11),~
               at (14,41), fac(hex(84)), txt1$( 6% + scr% )     , ch(20),~
               at (14,64), fac(hex(84)), txtp1$( 6% + scr% )    , ch(11),~
                                                                         ~
               at (15,06), fac(hex(84)), txt$( 7% + scr% )      , ch(20),~
               at (15,29), fac(hex(84)), txtp$( 7% + scr% )     , ch(11),~
               at (15,41), fac(hex(84)), txt1$( 7% + scr% )     , ch(20),~
               at (15,64), fac(hex(84)), txtp1$( 7% + scr% )    , ch(11),~
                                                                         ~
               at (16,06), fac(hex(84)), txt$( 8% + scr% )      , ch(20),~
               at (16,29), fac(hex(84)), txtp$( 8% + scr% )     , ch(11),~
               at (16,41), fac(hex(84)), txt1$( 8% + scr% )     , ch(20),~
               at (16,64), fac(hex(84)), txtp1$( 8% + scr% )    , ch(11),~
                                                                         ~
               at (17,06), fac(hex(84)), txt$( 9% + scr% )      , ch(20),~
               at (17,29), fac(hex(84)), txtp$( 9% + scr% )     , ch(11),~
               at (17,41), fac(hex(84)), txt1$( 9% + scr% )     , ch(20),~
               at (17,64), fac(hex(84)), txtp1$( 9% + scr% )    , ch(11),~
                                                                         ~
               at (18,06), fac(hex(84)), txt$(10% + scr% )      , ch(20),~
               at (18,29), fac(hex(84)), txtp$(10% + scr% )     , ch(11),~
               at (18,41), fac(hex(84)), txt1$(10% + scr% )     , ch(20),~
               at (18,64), fac(hex(84)), txtp1$(10% + scr% )    , ch(11),~
                                                                         ~
               at (19,06), fac(hex(84)), txt$(11% + scr% )      , ch(20),~
               at (19,29), fac(hex(84)), txtp$(11% + scr% )     , ch(11),~
               at (19,41), fac(hex(84)), txt1$(11% + scr% )     , ch(20),~
               at (19,64), fac(hex(84)), txtp1$(11% + scr% )    , ch(11),~
                                                                         ~
               at (20,06), fac(hex(84)), txt$(12% + scr% )      , ch(20),~
               at (20,29), fac(hex(84)), txtp$(12% + scr% )     , ch(11),~
               at (20,41), fac(hex(84)), txt1$(12% + scr% )     , ch(20),~
               at (20,64), fac(hex(84)), txtp1$(12% + scr% )    , ch(11),~
                                                                         ~
               at (21,06), fac(hex(84)), txt$(13% + scr% )      , ch(20),~
               at (21,29), fac(hex(84)), txtp$(13% + scr% )     , ch(11),~
               at (21,41), fac(hex(84)), txt1$(13% + scr% )     , ch(20),~
               at (21,64), fac(hex(84)), txtp1$(13% + scr% )    , ch(11),~
                                                                         ~
               at (22,06), fac(hex(84)), txt$(14% + scr% )      , ch(20),~
               at (22,29), fac(hex(84)), txtp$(14% + scr% )     , ch(11),~
               at (22,41), fac(hex(84)), txt1$(14% + scr% )     , ch(20),~
               at (22,64), fac(hex(84)), txtp1$(14% + scr% )    , ch(11),~
                                                                         ~
               at (23,06), fac(hex(84)), txt$(15% + scr% )      , ch(20),~
               at (23,29), fac(hex(84)), txtp$(15% + scr% )     , ch(11),~
               at (23,41), fac(hex(84)), txt1$(15% + scr% )     , ch(20),~
               at (23,64), fac(hex(84)), txtp1$(15% + scr% )    , ch(11),~
                                                                         ~
               at (24,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 3% then goto L43120 
                  if scr% = 15% then scr% = 0% ~
                    else scr% = 15%
               
                  goto L42070

L43120:        if keyhit% <> 15% then goto L43130
                  call "PRNTSCRN"
                  goto L42070

L43130:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            hdr1$ = "<Std Calc Ref. Desc>"
            hdr2$ = "APC Catalog"
            hdr3$ = "<Spc Calc Ref. Desc>"
            hdr4$ = "Special Cat"

            inpmessage$ = "Press <Return> To Continue, PF(15) to Print th~
        ~e Screen?"
            pfkeys$ = hex(01ff03ffffffffffffffffffffff0f1000)
        return

        format_screen
            init(" ") txt$(), txtp$(), txt1$(), txtp1$(), readkey$
            str(readkey$,1%,9%) = "PRICE 002"
            for i% = 1% to 30%          /*(AWD005)*/
                if ref$(i%) = "00" then goto L43380
                   str(readkey$,10%,15%) = ref$(i%)
                   read #2,key = readkey$, using L43360, txt$(i%),        ~
                                          /*EWD001*/    eod goto L43380
L43360:                FMT POS(25), CH(20)
                   convert ref_p(i%) to txtp$(i%), pic($#,###.##-)
L43380:         if ref1$(i%) = "00" then goto L43430
                   str(readkey$,10%,15%) = ref1$(i%)
                   read #2,key = readkey$, using L43360, txt1$(i%),       ~
                                                        eod goto L43430
                   convert ref_p1(i%) to txtp1$(i%), pic($#,###.##-)
L43430:     next i%
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,         /* Customer Code         */ ~
                              L50370          /* Part Number           */ 


            return

L50140: REM Customer Code                         CUSCODE$
           if cuscode$ <> " " then goto L50200
              mscdescr$ = hex(06) & "Select Customer Code"
              call "PLOWCODE" (#4, cuscode$, mscdescr$, 0%, .30, f1%(4))
              if f1%(4) = 0 then goto L50340
              goto L50210
L50200:     read #4,key = cuscode$, eod goto L50340
L50210:     get #4, using L50220, cust_name$, code$, cust_type$
L50220:       FMT POS(10), CH(30), POS(525), CH(1), POS(1023), CH(2)


            if code$ >= "A" and code$ <= "Z" then                        ~
               srce% = val(code$) - 64%         /* (01) Thru (26) */     ~
               else  srce% = val(code$) - 21%   /* (27) Thru (36) */

            readkey$ = "PRICECODE" & code$
            call "DESCRIBE" (#2, readkey$, pc_desc$, 0%, f1%(2))
            readkey$ = "CUS TYPES" & cust_type$
            call "DESCRIBE" (#2, readkey$, cust_desc$, 0%, f1%(2))
        return
L50340:     errormsg$ = "Invalid Customer Code? "
        return

L50370: REM Part Number                           PART$
           if part$ <> " " then goto L50420
           if str(part$,1%,1%) <> "?" then goto L50420
              part$ = " "

              mscdescr$ = hex(06) & "Select MFG Part Number"
              call "PLOWCODE" (#5, part$, mscdescr$, 0%, .30, f1%(5))

L50420:     

            gosub virtualMdl                                /* (CR2451) */
            if errormsg$ <> " " then return                 /* (CR2451) */
            gosub build_part
                                         /* (EWD001) - Convert Price,  */
                                         /* either Catalog or Special  */    
            if err% > 4% then goto L50510
                                    /* APC Catalog Price -             */
            convert pc(1) to price_cat$, pic(#######.####)
            price = pc(srce%)       /* Catalog Price with Customer Disc*/
            if sp% > 0% and p1 > 0.1 then price = p1  /* Special Price */

            convert price to price$, pic(#######.####)     /* 0, 1, 2  */
        return
L50510:     errormsg$ = "Invalid Part Number"
            price_cat$, price$ = "      0.0000"
            price = 0.0
        return
virtualMdl:        
        REM Virtual Model                         MODEL$  /*(SR67154)(CR2451)*/
          if model$ = " " then return
          init(" ") readkey$
          readkey$ = "VIRTUALMD" & str(model$,1%,15%)
          call "DESCRIBE" (#2, readkey$, mdl_desc$, 0%, f1%(2))     
          if mdl_desc$ = " " then goto L50610   
        return          
L50610:     errormsg$ = "Invalid Virtual Model"
            price_cat$, price$ = "      0.0000"
            price = 0.0
        return
        
        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *                 S U B R O U T I N E S                     *~
            *************************************************************

        build_part
                                        /* (EWD001) - Check for        */
                                        /* Promotions                  */
L60060:     price = 0.0
            err%  = 0%                  /* Initialize Error Code       */
            size$ = "X"                 /* Set Flag For 'APCPRCQT'     */
            upd$  = "N"                 /* Quote Does Not Update Price */
            p1    = 0.0    /* Spc Customer Price, APC Catalog Always   */
            sp%   = 0%     /* (0%) APC Price Catalog (Only)            */
                           /* (1%) Spc Customer Catalog Price in (P1)  */
                           /* (2%) Spc Customer EDI Price in (P1)      */
                           /* Note - Information from 'PRICECUST' Table*/

            call "APCPR0SB" ( model$,     /* Model Number (SR67154) (CR2451)*/~
                              part$,      /* Part Number to Build    */  ~
                              subpart$,   /* Part Number 1 1-5       */  ~
                              part_desc$, /* Part Description        */  ~
                              size$,      /* (O)pen,(E)xact,(F)NO DED*/  ~
                              upd$,       /* Update Prices (Y)es,(N)o*/  ~
                              pc(),       /* Calculated Prices       */  ~
                              p1,         /* Special Customer Price  */  ~
                              sp%,        /* Special Prc Cd 0,1      */  ~
                              cuscode$,   /* Customer Code           */  ~
                              ref$(),     /* Ref. Type Codes Max = 15*/  ~
                              ref1$(),    /* Ref. Type Codes Spec Cat*/  ~
                              ref_p(),    /* Ref Price Catalog       */  ~
                              ref_p1(),   /* Ref Price Special Cat.  */  ~
                              #1,         /* CPRPRICE File           */  ~
                              #2,         /* GENCODES File           */  ~
                              #4,         /* CUSTOMER File           */  ~
                              #5,         /* HNYMASTR FILE           */  ~
                              #6,         /* APCPCMST File           */  ~
                              #7,         /* AWDPCMST File           */  ~
                              #8,         /* APCSKUNO File           */  ~
                              #9,         /* APCPCMSK File           */  ~
                              #10,        /* APCPCMSD File           */  ~
                              err% )      /* Error Code Values        */
                                          /* 0%=Part and Catalog Prc  */
                                          /* 1%=Part Catalog and Spc  */
                                          /* 2%=Part and EDI Price    */
                                          /* 3%=No Price Error Catalog*/
                                          /* 4%=Catalog, Error Specia */
                                          /* 5%=Invalid Part Price    */
                                          /* 6%=Invalid Part Build    */
            if str(part$,1%,1%) = "+" then goto L60460
REM               if len(part$) < 10% then goto L60630
            if len(part$) < 10% and  str(part$,5%,5%) <> "LABOR" then goto L60630
L60460:     opt% = 1%
            s_23m$ = str(part$,1%,3%)   /* Set Model/Product Code      */
            call "APCPRXSB" (opt%,      /* 0%=Input, 1% = Verify Only  */~
                             cuscode$,  /* Customer Code               */~
                             s_23m$,    /* New Product Model Code      */~
                             s_23$,     /* New Series Code(Description)*/~
                             s_23%,     /* Description Length          */~
                             #4,        /* CUSTOMER                    */~
                             #2,        /* (GENCODES)                  */~
                             x_er% )    /* 0% = OK, 1% = NO CODE'S     */
           if opt% <> 2% then goto L60600   /* Re-Calc Price             */
              str(part$,1%,3%) = s_23m$
              goto L60060

L60600:    if x_er% <> 0% then goto L60630 /* Series Not Applicable     */
              str(part_desc$,1%,8%) = str(s_23$,1%,8%)

L60630: REM if len(part$) < 10% then err% = 6%
        if len(part$) < 10% and str(part$,5%,5%) <> "LABOR" then err% = 6%
        init(" ") s_23$, s_23m$
        if len(model$) <= 3% then model$ = " "
        return

        save_price                             /* Save Prices for Part */
                                               /* When PF(32) Used     */
            if len(part$) < 19 then goto L60800
               call "SHOSTAT" ("Saving Price For ("&part$& ")")
               key$ = " "
               key$ = "C" & part$
               read #1,hold,key = key$, eod goto L60760
               delete #1

L60760:        write #1, using L60780, "C", part$, " ", userid$, date,    ~
                                      pc(), fl1$, fl2$
L60780:          FMT CH(1), CH(25), CH(21), CH(3), CH(6), 36*PD(14,4),   ~
                     CH(200), CH(156)
L60800: return clear all
        goto inputmode

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("Closing Files")
        end
