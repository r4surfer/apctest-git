        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN48                             *~
            *  Creation Date     - 11/17/96                             *~
            *  Last Modified Date- 03/19/2013                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - This Program creates the Screen for  *~
            *                      Manufactured Product Explosion of    *~
            *                      the lineal Parts with their Heights. *~
            *                      There is a Width and a Height Screen.*~
            *                                                           *~
            *  Code Tables Used  - (APCMATOBS)-Obosolete Product No's   *~
            *                                                           *~
            *                    - (EQUATIONS)-<Prod>-<Eq Type>-<Eq No.>*~
            *                                  ie. 6-1-01               *~
            *                                                           *~
            *                                                           *~
            *  Subroutines  - (APCCUTLD) - Loads all Equation References*~
            *                              for a Specified Product Line *~
            *                                                           *~
            *               - (APCCUTCC) - Calculate all Width and      *~
            *                              Height Cuts                  *~
            *                                                           *~
            *      File    -  (APCCUTEQ) - Saw Equation Cross Reference *~
            *                              File                         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/17/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 03/19/06 ! (AWD001) mod for north east casing and   ! CMG *~
            *          !    bullnose with subpart                 !     *~
            * 04/03/08 ! (AWD002) mod for sdlglazing bead         ! CMG *~
            *05/21/2012! (AWD003) mod for additional part data    ! CMG *~
            *03/19/2013! (AWD003) mod to awdcutcc addl data       ! CMG *~
            *05/19/2014! (CUT001) mod to add dim fields to CUTCC  ! CMG *~
            *06/17/2014! (AWD004) mod for operable shapes         ! CMG *~
            *08/11/2017! (CR1002) new top bottom parameter        ! RDB *~
            *************************************************************
 
        dim                                                              ~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Description           */~
            sub_scr$120,                 /* Screen Description AWD001  */~
            sub_prt$60,                  /* Print Description  AWD001  */~
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

        dim                                                              ~
            sa_part$45,                  /* MFG Part Number   (AWD001) */~
            sa_part_desc$30,             /* MFG Part Number Description*/~
            sa_mfg$1,                    /* '0'= MFG Only, or '1'=Parts*/~
            sa_mfg_desc$30,              /* MFG Description            */~
            readkey$24,                  /* Generic Key                */~
            desc$30,                     /* Generic Description        */~
            pd$1,                        /* Product Line               */~
            ct(500%),                    /* Cut Size - DECIMAL         */~
            ct$(500%)9,                  /* Cut Size - WITH FRACTION   */~
            cr$(500%)10,                 /* Cut Raw Material           */~
            cr_addl$(500%)5,             /* Additional Part Data(AWD003)*/~
            cp$(500%)2,                  /* Cut Number of Pieces       */~
            cc$(500%)1,                  /* Linealmate Cut (Y) or (N)  */~
            co$(500%)25,                 /* CUT DESCRIPTIONS           */~
            eq$(500%)8,                  /* Equation Ref Key(s)        */~
            sh$(500%)1,                  /* SASH TYPE ( W, H, N )      */~
/*AWD004*/  s_f$(500%)1,                 /* Sash / Frame               */~
/*AWD004*/  die$(500%)15,                /* Die Number                 */~
/*AWD004*/  adj(500%),                   /* Adjustment amt             */~
            hdr$(2%)60,                  /* CUT HEADER TEXT            */~
            tw$1,                        /* WIDTH PARTS TYPE CODE      */~
            th$1,                        /* HEIGHT PARTS TYPE CODE     */~
/*AWD001*/  tsw$1,                       /* Width subpart type code    */~
/*AWD001*/  tsh$1,                       /* height supart type code    */~
/*AWD001*/  casing$1,                    /* casing option in subpart   */~
            txt$(20)60,                  /* CUT DESCRIPTIONS           */~
            tb_w$(500%)1                 /* T/B cut (CR1002)           */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        dim                              /*(APCPIECE) AND REPORT       */~
            scr_load$5,                  /* Load Number                */~
            scr_desc$30,                 /* Load Description           */~
            sa_key$8                     /* SAW ALT KEY (1)            */
            
        dim sdl$1                        /* (AWD002) SDL Flag          */
         

/* (AWD004) */
        dim dim1es$10, dim2es$, dim3es$        

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Manufactured Prod Lineal Explosion"
            pname$ = "APCPLN48 - Rev: R6.04"

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
            * #1  ! APCCUTEQ ! Saw Optimization Cross-Reference File    *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            * #3  ! INVMASTR ! Part Master File        (AWD001)         *~
            * #4  ! AMTBOMCD ! Master Equation File                     *~
            * #5  ! AMTBOMIF ! Master Part Validity File                *~
            * #7  ! INVQUAN  ! INVENTORY QUANTITIES MASTER    (AWD001)  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCCUTEQ",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  24


                                              /* (AWD001) */
            select #3,  "INVMASTR",                                      ~
                        varc,     indexed,  recsize =  1024,             ~
                        keypos =  1,   keylen =  45,                     ~
                        alt key  1, keypos  =   122, keylen =  9, dup,   ~
                            key  2, keypos  =   110, keylen =  4, dup,   ~
                            key  3, keypos  =    46, keylen = 32, dup

            select #4,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #5,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~

                                              /* (AWD001) */
            select #7,  "INVQUAN",                                       ~
                        varc,     indexed,  recsize =   768,             ~
                        keypos =   17, keylen =  64,                     ~
                        alt key  1, keypos =    1, keylen =  64

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))

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

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  1%
L01750:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L01870
L01770:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L01850
L01800:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L01770
                         if fieldnr% = 1% then L01750
                         goto L01800
L01850:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L01770
L01870:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L01770
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
                  if keyhit%  = 10% then goto process_tubes                  
                  if keyhit%  = 14% then gosub calc_cuts
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg1
L02040:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 1% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L02090:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L02090
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L02090
                  lastfieldnr% = fieldnr%
            goto L02040

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%

            if been_here_before% <> 0% then goto BeenHere
             str(sa_part$,26%,20%) = "00000000000000000000"
/* (AWD004) */             
             convert dim1es to dim1es$, pic(######0.00)
             convert dim2es to dim2es$, pic(######0.00)
             convert dim3es to dim3es$, pic(######0.00)
BeenHere:
/* (\AWD004) */

            been_here_before% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L02380
                inpmessage$ = edtmessage$
                return

L02380
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Manufactured Product Part Number, Min. = 19 Dg."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sa_part$, sa_part_desc$,   ~
                      sa_mfg$, sa_mfg_desc$, pd$, co$(), ct$(), cr$(),   ~
                      cp$(), cc$(), eq$(), readkey$, desc$, hdr$(),      ~
                      txt$(), apc_scr$, apc_prt$, apc_sze$, sa_key$,     ~
                      sh$(), scr_load$, scr_desc$, axd$, cr_addl$(),     ~
                      s_f$(), die$(), dim1es$, dim2es$, dim3es$

            hdr% = 1%

            been_here_before% = 0%
            dim1es, dim2es, dim3es = 0.00  /* (AWD004) */
        return

        REM *************************************************************~
            *************************************************************

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
            * Update Store Data and Part Data                           *~
            *************************************************************
        REM DATAPUT
        REM RETURN 
 
        process_tubes                     /*(AWD001)*/ /* (AWD004) */
           call "AWDPLB60" (sa_part$, dim1es, dim2es, dim3es, #4,#2,#5)
        return clear all
        goto inputmode
        
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
              on fieldnr% gosub L03150           /* Part Number       */

              goto L03180

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L03150:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L03180:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,15), fac(hex(84)), apc_prt$               , ch(60),~
               at (04,02), "Part Number:",                               ~
/*AWD001*/     at (04,15), fac(lfac$(1%)), str(sa_part$,1,25)   , ch(25),~
/*AWD001*/     at (04,45), fac(lfac$(1%)), str(sa_part$,26,20)  , ch(20),~
/*AWD004*/     at (05,15), fac(lfac$(1%)), dim1es$              , ch(10),~
/*AWD004*/     at (05,30), fac(lfac$(1%)), dim2es$              , ch(10),~
                                                                         ~                                                      
               at (05,45), fac(hex(84)), sa_part_desc$          , ch(30),~
               at (07,10), fac(hex(84)), hdr$(hdr%)             , ch(60),~
               at (08,10), fac(hex(84)), txt$( 1%)              , ch(60),~
               at (09,10), fac(hex(84)), txt$( 2%)              , ch(60),~
               at (10,10), fac(hex(84)), txt$( 3%)              , ch(60),~
               at (11,10), fac(hex(84)), txt$( 4%)              , ch(60),~
               at (12,10), fac(hex(84)), txt$( 5%)              , ch(60),~
               at (13,10), fac(hex(84)), txt$( 6%)              , ch(60),~
               at (14,10), fac(hex(84)), txt$( 7%)              , ch(60),~
               at (15,10), fac(hex(84)), txt$( 8%)              , ch(60),~
               at (16,10), fac(hex(84)), txt$( 9%)              , ch(60),~
               at (17,10), fac(hex(84)), txt$(10%)              , ch(60),~
               at (18,10), fac(hex(84)), txt$(11%)              , ch(60),~
               at (19,10), fac(hex(84)), txt$(12%)              , ch(60),~
               at (20,10), fac(hex(84)), txt$(13%)              , ch(60),~
               at (21,10), fac(hex(84)), txt$(14%)              , ch(60),~
               at (22,10), fac(hex(84)), txt$(15%)              , ch(60),~
                                                                         ~
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 3% then goto L03620
                  if hdr% = 1% then goto L03580
                     hdr% = 1%
                     gosub build_screen
                     goto L03180
L03580:           hdr% = 2%
                  gosub build_screen
                  goto L03180

L03620:        if keyhit% <> 15% then goto L03660
                  call "PRNTSCRN"
                  goto L03180

L03660:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L03850      /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(2%) = "(3)Toggle(W/H)   (14)Calc Cuts          " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ff0304ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L03790
                str(pf$(2%),64%)   = " " : str(pfkeys$,16%,1%) = hex(ff)
L03790:     if fieldnr% > 1% then L03810
                str(pf$(1%),18%,20%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L03810:     str(pf$(2%),1% ,15%)=" "  :  str(pfkeys$, 3%,1%) = hex(ff)
            str(pf$(2%),18%,20%)=" "  :  str(pfkeys$,14%,1%) = hex(ff)
            return

L03850: if fieldnr% > 0% then L03920   /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over    (10)Calc Tubes         " &       ~
                      "                       (15)Print Screen"
            pf$(2%) = "(3)Toggle(W/H)   (14)Calc Cuts          " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ff03ffffffffffff0affffff0e0f1000)
            return
L03920:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
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
            on fieldnr% gosub L04120           /* Part Number           */

            return

L04120: REM MFG Part Number                       SA_PART$

          if sa_part$ <> " " then goto L04200
             sa_part_desc$ = " "
             sa_part_desc$ = hex(06) & "Select a MFG Part Number"
             call "GETCODE" (#3,sa_part$, sa_part_desc$, 0%, 1.32,f1%(3))
             if f1%(3) = 0% then goto L04250

L04200:      gosub lookup_description
             if err% <> 0% then goto L04250
                pd$ = str(sa_part$,1%,1%)
                sa_part_desc$ = apc_sze$

/* (AWD004) */
                dim1es, dim2es, dim3es = 0.00
                convert dim1es$ to dim1es, data goto bad_dim1
bad_dim1:
                convert dim2es$ to dim2es, data goto bad_dim2
bad_dim2:
                convert dim3es$ to dim3es, data goto bad_dim3
bad_dim3:

                convert dim1es to dim1es$, pic(######0.00)
                convert dim2es to dim2es$, pic(######0.00)
                convert dim3es to dim3es$, pic(######0.00)                                   
        return
L04250:     errormsg$ = "(Error) - Invalid Part Number."
            sa_part$, sa_part_desc$, pd$ = " "
            str(sa_part$,26,20) = "00000000000000000000"
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        lookup_description
           err% = 0%

*    AWD001

REM    call "APCDESCR" (sa_part$, apc_scr$, apc_prt$, apc_sze$, #5,  ~
                                                                  err% )




           call "AWDDESCR" (str(sa_part$,1,25), str(sa_part$,26,20), ~
                            apc_scr$, apc_prt$, sub_scr$,            ~
                                 sub_prt$, apc_sze$, #5, err% )
        return

        calc_cuts
           call "SHOSTAT" ("Check Saw Equation References")
           init("0") casing$, sdl$     /* (AWD001) */ /* (AWD002) */
           casing$ = str(sa_part$,31%,1%)
           sdl$    = str(sa_part$,33%,1%)
           if str(sa_part$,34%,1%) = "2" then sdl$ = "0"

           if casing$ =  "0" and sdl$ = "0" then gosub get_cuts
           if casing$ <> "0" or sdl$ <> "0" then gosub get_subpart_cuts

           call "SHOSTAT" ("Calculating Saw Cuts for Part")
           sa_mfg% = 0%                            /* MFG WINDOWS ONLY */


           if casing$  = "0" and sdl$ = "0" then gosub calc_part_cuts
           if casing$ <> "0" or sdl$ <> "0" then gosub calc_subpart_cuts


           gosub build_screen
           if kk% <> 0% then return
              errormsg$ =                                                ~
                  "(Error) - No Cut References Defined for Model/Color."
        return

        get_cuts
           tw$ = "1" : th$ = "2"
           call "APCCUTLD" (pd$, cw%, ch%, tw$, th$, #2, err% )
           if err% = 0% then goto L04510
              errormsg$ =                                                ~
                  "(Error) - Saw Cuts not Defined in (EQUATIONS) Table."
              return
L04510:
        return

        get_subpart_cuts
           tw$ = "1" : th$ = "2"
           if casing$ <> "1" then goto not_casing
              tsw$ = "K" : tsh$ = "L"

not_casing
           if casing$ <> "2" then goto not_bullnose
              tsw$ = "M" : tsh$ = "N"

not_bullnose
           call "AWDCUTLD" (pd$, cw%, ch%, csw%, csh%, tw$, th$, tsw$, tsh$,~
                             #2, err% )
           if err% = 0% then goto L04515
              errormsg$ =                                                ~
                  "(Error) - Saw Cuts not Defined in (EQUATIONS) Table."
              return
L04515:

        return


        calc_part_cuts
           sa_mfg% = 0%
           if str(sa_part$,26,1) = "4" then sa_mfg% = 3%
                               /*  (CUT001) */
           call "APCCUTCC" ( str(sa_part$,1,25), dim1es, dim2es, dim3es,    ~
                            sa_mfg%, cw%, ch%, eq$(), ct$(), cr$(), cp$(),  ~
                            cc$(), co$(), ct(), sh$(), tw$, th$, #1, #4, #2,~
                            err%)

        return

        calc_subpart_cuts
           sa_mfg% = 0%
           if str(sa_part$,33,1) = "1" and str(sa_part$,34,1) <> "2" ~
                   then sa_mfg% = 3%    /* (AWD002) sdl */

                                                    /* (AWD003) */
                                                    /* (AWD004) */
           init(" ") tb_w$()   /* CR1002 */                                        
           call "AWDCUTCC" ( sa_part$, dim1es, dim2es, dim3es, sa_mfg%,    ~
                             cw%, ch%, csw%, csh%, eq$(), ct$(), cr$(),    ~
                             cr_addl$(), cp$(), cc$(), co$(), ct(), sh$(), ~
                    tw$, th$, s_f$(), die$(), adj(), tb_w$(), #1, #4, #2, err%)

        return



        build_screen
        REM (1) - 8%,21%  (2) - 32%,10%  (3) - 45%,2%  (4) - 50%,8%

            init(" ") hdr$(), txt$()
            hdr$(1%) =                                                   ~
           "Width <-----Description-----> <-Raw Mat.-> <Qt> <--Size-->Ct"

            hdr$(2%) =                                                   ~
           "Height<-----Description-----> <-Raw Mat.-> <Qt> <--Size-->Ct"

                                                             /* WIDTH  */
           if hdr% = 2% then goto L04900
            kk% = 0%
            for i% = 1% to cw%
                if cc$(i%) = " " then goto L04880
                   kk% = kk% + 1%
                   str(txt$(kk%),2%,4%) = "(00)"
                   convert i% to str(txt$(kk%),3%,2%), pic(00)

                   str(txt$(kk%),8%,21%)  = str(co$(i%),1%,21%)
                   gosub check_obsolete

                   str(txt$(kk%),32%,10%) = cr$(i%)
                   str(txt$(kk%),45%,2%)  = cp$(i%)
                   str(txt$(kk%),50%,9%)  = ct$(i%)
                   str(txt$(kk%),60%,1%)  = cc$(i%)
L04880:     next i%


* AWD001
            j% = cw% + ch%          /* Width and height */
            for i% = 1% to csw%
                if cc$(j% + i%) = " " then goto L04885
                   kk% = kk% + 1%
                   str(txt$(kk%),2%,4%) = "(00)"
                   convert i% to str(txt$(kk%),3%,2%), pic(00)

                   str(txt$(kk%),8%,21%)  = str(co$(j% + i%),1%,21%)
                   gosub check_obsolete

                   str(txt$(kk%),32%,10%) = cr$(j% + i%)
                   str(txt$(kk%),45%,2%)  = cp$(j% + i%)
                   str(txt$(kk%),50%,9%)  = ct$(j% + i%)
                   str(txt$(kk%),60%,1%)  = cc$(j% + i%)
L04885:     next i%


        return



L04900:     j% = cw%                                         /* HEIGHT */
            kk% = 0%
            for i% = 1% to ch%
                if cc$(j% + i%) = " " then goto L05050
                   kk% = kk% + 1%
                   str(txt$(kk%),2%,4%) = "(00)"
                   convert i% to str(txt$(kk%),3%,2%), pic(00)

                   str(txt$(kk%),8%,21%)  = str(co$(j% + i%),1%,21%)
                   gosub check_obsolete

                   str(txt$(kk%),32%,10%) = cr$(j% + i%)
                   str(txt$(kk%),45%,2%)  = cp$(j% + i%)
                   str(txt$(kk%),50%,9%)  = ct$(j% + i%)
                   str(txt$(kk%),60%,1%)  = cc$(j% + i%)
L05050:     next i%

            j% = cw% + ch% + csw%          /* Width + HEIGHT  + subpart width*/
            for i% = 1% to csh%
                if cc$(j% + i%) = " " then goto L05055
                   kk% = kk% + 1%
                   str(txt$(kk%),2%,4%) = "(00)"
                   convert i% to str(txt$(kk%),3%,2%), pic(00)

                   str(txt$(kk%),8%,21%)  = str(co$(j% + i%),1%,21%)
                   gosub check_obsolete

                   str(txt$(kk%),32%,10%) = cr$(j% + i%)
                   str(txt$(kk%),45%,2%)  = cp$(j% + i%)
                   str(txt$(kk%),50%,9%)  = ct$(j% + i%)
                   str(txt$(kk%),60%,1%)  = cc$(j% + i%)
L05055:     next i%
        return

        check_obsolete
          readkey$ = " "
          str(readkey$,1%,9%)   = "APCMATOBS"
          str(readkey$,10%,15%) = cr$(i%)
          if hdr% = 2% then str(readkey$,10%,15%) = cr$(j% + i%)

          read #2,key = readkey$, using L05150, desc$, eod goto L05180
L05150:      FMT POS(25), CH(30)
          cr$(i%) = str(desc$,1%,10%)
          if hdr% = 2% then cr$(j% + i%) = str(desc$,1%,10%)
L05180: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end


