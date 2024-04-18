        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN53                             *~
            *  Creation Date     - 07/06/98                             *~
            *  Last Modified Date- 01/09/08                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Analysis of Stagging and Loading Data*~
            *                      by Date, Userid, and Shift.          *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/06/98 ! (New) Program                            ! RHH *~
            * 11/18/98 ! (EWD001) Mods to Skip Glass Warranty     ! RHH *~
            *          !   Units.                                 !     *~
            * 01/09/08 ! (AWD002) mods for new dept 054 and 074   ! CMG *~
            * 11/09/17 ! (CR1202) add screen scrolling            ! RDB *~
            *          !    change APCUSERS to USERLCMS           !     *~
            *************************************************************

        dim                                                              ~
            ad_key$33, ad_rec$64,        /* Primary Key                */~
            sc_key$10, sav_sc_key$10,    /* (EWD001) Glass Warranty Mod*/~
            sc_part$25,                  /* (EWD001) Part Number       */~ 
            beg_date$10, beg_dte$6,      /* Beginning Date             */~
            end_date$10, end_dte$6,      /* Ending Date                */~
            x$10,                        /* Date Buffer                */~
            id$(99%)3, id_d$(99%)30%,    /* User Id                    */~
            qty%(99%), qty$(99%)6,       /* Scanned Quantities for Id  */~
            t_qty$8,                     /* Total Scanned Units        */~ 
            scr_sel$1, scr_sel_d$30,     /* Selection Stage, Load      */~
            cnt$30, ad_id$3,             /* For Analysis               */~
            title$50, scr_time$8,        /* Analysis Title and Time    */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
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

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Stagging and Loading Analysis"
            pname$ = "EWDPLN53 - Rev: R7.00"

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
            * #1  ! APCPLNAD ! Production Scanning Audit File           *~
            * **  ! APCUSERS ! Same as USERCLMS                         *~
            * #2  ! USERLCMS ! Caelus Master User Def.                  *~
            * #3  ! APCPLNSC ! Planning Master Schedule File            *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 19,   keylen = 33,                      ~
                        alt key 1, keypos =  1, keylen = 33

            select #2,  "USERLCMS",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =   4, keylen =  30, dup

            select #3,  "APCPLNSC",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))

/* CR1202     call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))  */
           call "OPENOLIB" (#2, "SHARE",   f2%(2%),      rslt$(2%), axd$)

            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))

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

            for fieldnr% = 1% to   3%
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
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
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
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        process_data
            call "SHOSTAT" ("Analyizing Data for Loads")
                                                 /* Analyize Staging   */
            init(" ") ad_key$, ad_rec$, id$()    /* and Loading Data   */
            init(" ") sc_key$, sav_sc_key$ 
            cnt$ = "Records Checked [ xxxxxxxx ]" 
            mat qty% = zer
            cnt% = 0% : id_max% = 0%             /* Max Users          */
            str(ad_key$,1%,6%) = beg_dte$        /* set Beginning Date */
            read #1,key > ad_key$, using L19000, ad_rec$,                ~
                                               eod goto process_data_done
L19000:        FMT CH(64)
            goto L19002
        process_data_next
            read #1,using L19000, ad_rec$, eod goto process_data_done
                                               /* Set Primary Key      */
L19002:     ad_key$ = str(ad_rec$,19%,33%)
            cnt% = cnt% + 1%
            if mod(cnt%,100%) <> 0 then goto L19005
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,28%);hex(84);cnt$;

L19005:     if str(ad_key$,1%,6%) > end_dte$ then goto process_data_done
               ad_dept$ = str(ad_rec$,25%,3%)
               gosub glass_warranty          /* (EWD001) Skip Warranty */  
               if hit% = 1% then goto process_data_next
                                             /* (EWD001) - Staging     */  
               if scr_sel$ <> "1" then goto L19007
                  if ad_dept$ <> "106" and ad_dept$ <> "044" then        ~
                                           goto process_data_next
/* (AWD002) */
                  if ad_dept$ <> "106" and ad_dept$ <> "054" then        ~
                                           goto process_data_next
                  if ad_dept$ <> "106" and ad_dept$ <> "074" then        ~
                                           goto process_data_next

/* (AWD002) */
                     if str(ad_rec$,32%,2%) <> "14" then                 ~
                                           goto process_data_next
                     goto L19010
                                             /* (EWD001) - Loading     */ 
L19007:        if ad_dept$ <> "108" then goto process_data_next
                     if str(ad_rec$,32%,2%) <> "16" then                 ~
                                            goto process_data_next
L19010:                                          /* Data Found         */
            ad_id$ = str(ad_rec$,60%,3%)
            if id_max% = 0% then goto L19020
            for kk% = 1% to id_max%
                if id$(kk%) = ad_id$ then goto L19030
            next kk%
L19020:     id_max% = id_max% + 1%
            if id_max% > 98% then id_max% = 99%  /* Max 99 User Id's   */
            kk% = id_max%
            id$(kk%) = ad_id$                    /* Save Login Id's    */
L19030:     qty%(kk%) = qty%(kk%) + 1%
            goto process_data_next
            
        process_data_done
            init(" ") id_d$(), qty$(), title$, scr_time$, t_qty$
            t_qty% = 0%
            for kk% = 1% to id_max%
                gosub load_user           
                convert qty%(kk%) to qty$(kk%), pic(######)
                t_qty% = t_qty% + qty%(kk%)

            next kk%
            id_d$(id_max% + 1%) = "  <<<<<<< END OF DATA >>>>>>>"
            convert t_qty% to t_qty$, pic(########)
            title$ = "Stagging Analysis @ [xxxxxxxx]  Total = [xxxxxxxx]"
            if scr_sel$ = "2" then                                       ~
            title$ = "Loading  Analysis @ [xxxxxxxx]  Total = [xxxxxxxx]"
            call "TIME" (scr_time$)
            str(title$,22%,8%) = scr_time$
            str(title$,42%,8%) = t_qty$
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
         "Enter a Valid Scanning Begin Date?                           ",~
         "Enter a Valid Scanning Ending Date?                          ",~
         "Enter a Valid Selection (1) = Stagging, (2) = Loading        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_dte$, beg_date$,       ~
                      end_dte$, end_date$, ad_id$, id$(), id_d$(),       ~
                      scr_sel$, scr_sel_d$, qty$(), cnt$, title$,        ~
                      scr_time$, t_qty$
            frst% = 0%    /* CR1202 first array number of screen data */
            cpos% = 12%   /* CR1202 last array number of screen data */

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
        REM DATALOAD
        REM RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE


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
              on fieldnr% gosub L40160,          /* Beg Date           */~
                                L40160,          /* End Date           */~
                                L40160           /* Selection          */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (04,02), "Scanning Begin Date:",                       ~
               at (04,24), fac(lfac$(1%)), beg_date$            , ch(10),~
                                                                         ~
               at (05,02), "Scanning End Date  :",                       ~
               at (05,24), fac(lfac$(2%)), end_date$            , ch(10),~
                                                                         ~
               at (06,02), "Selection          :",                       ~
               at (06,24), fac(lfac$(3%)), scr_sel$             , ch(01),~
               at (06,40), fac(hex(84)),   scr_sel_d$           , ch(30),~                    
                                                                         ~
               at (08,16), fac(hex(a4)), title$                 , ch(50),~
                                                                         ~
               at (10,21), fac(hex(84)), id$(1% + frst%)        , ch(03),~
               at (10,27), fac(hex(84)), id_d$(1% + frst%)      , ch(30),~
               at (10,60), fac(hex(84)), qty$(1% + frst%)       , ch(06),~
                                                                 ~  
               at (11,21), fac(hex(84)), id$(2% + frst%)        , ch(03),~
               at (11,27), fac(hex(84)), id_d$(2% + frst%)      , ch(30),~
               at (11,60), fac(hex(84)), qty$(2% + frst%)       , ch(06),~
                                                                 ~  
               at (12,21), fac(hex(84)), id$(3% + frst%)        , ch(03),~
               at (12,27), fac(hex(84)), id_d$(3% + frst%)      , ch(30),~
               at (12,60), fac(hex(84)), qty$(3% + frst%)       , ch(06),~
                                                                 ~  
               at (13,21), fac(hex(84)), id$(4% + frst%)        , ch(03),~
               at (13,27), fac(hex(84)), id_d$(4% + frst%)      , ch(30),~
               at (13,60), fac(hex(84)), qty$(4% + frst%)       , ch(06),~
                                                                 ~  
               at (14,21), fac(hex(84)), id$(5% + frst%)        , ch(03),~
               at (14,27), fac(hex(84)), id_d$(5% + frst%)      , ch(30),~
               at (14,60), fac(hex(84)), qty$(5% + frst%)       , ch(06),~
                                                                 ~  
               at (15,21), fac(hex(84)), id$(6% + frst%)        , ch(03),~
               at (15,27), fac(hex(84)), id_d$(6% + frst%)      , ch(30),~
               at (15,60), fac(hex(84)), qty$(6% + frst%)       , ch(06),~
                                                                 ~  
               at (16,21), fac(hex(84)), id$(7% + frst%)        , ch(03),~
               at (16,27), fac(hex(84)), id_d$(7% + frst%)      , ch(30),~
               at (16,60), fac(hex(84)), qty$(7% + frst%)       , ch(06),~
                                                                 ~  
               at (17,21), fac(hex(84)), id$(8% + frst%)        , ch(03),~
               at (17,27), fac(hex(84)), id_d$(8% + frst%)      , ch(30),~
               at (17,60), fac(hex(84)), qty$(8% + frst%)       , ch(06),~
                                                                 ~  
               at (18,21), fac(hex(84)), id$(9% + frst%)        , ch(03),~
               at (18,27), fac(hex(84)), id_d$(9% + frst%)      , ch(30),~
               at (18,60), fac(hex(84)), qty$(9% + frst%)       , ch(06),~
                                                                 ~
               at (19,21), fac(hex(84)), id$(10% + frst%)       , ch(03),~
               at (19,27), fac(hex(84)), id_d$(10% + frst%)     , ch(30),~
               at (19,60), fac(hex(84)), qty$(10% + frst%)      , ch(06),~
                                                                 ~
               at (20,21), fac(hex(84)), id$(11% + frst%)       , ch(03),~
               at (20,27), fac(hex(84)), id_d$(11% + frst%)     , ch(30),~
               at (20,60), fac(hex(84)), qty$(11% + frst%)      , ch(06),~
                                                                 ~
               at (21,21), fac(hex(84)), id$(12% + frst%)       , ch(03),~
               at (21,27), fac(hex(84)), id_d$(12% + frst%)     , ch(30),~
               at (21,60), fac(hex(84)), qty$(12% + frst%)      , ch(06),~
                                                                         ~     
               at (22,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

/* CR1202 + */               
               if keyhit% <> 4% then goto L40390
                   if cpos% <= 12% then frst% = 0%
                   if cpos% > 12% then frst% = frst% - 12%
                   if frst% < 0% then frst% = 0%
                   cpos% = frst% + 12%
                   goto set_pf1
                   
L40390:        if keyhit% <> 5% then goto L40395
                   if id$(cpos%) = " " or str(id$(cpos%),4%,4%) = "<<<<"  ~ 
                        then goto set_pf1   /* at end of data */
                   frst% = frst% + 12%
                   if frst% > 87% then frst% = 87%
                   cpos% = frst% + 12%
                   goto set_pf1
/* CR1202 - */                   
L40395:        if keyhit% <> 15 then goto L40400
                  call "PRNTSCRN"
                  goto L40190

L40400:        if keyhit% <> 10% then goto L40420
                  frst% = 0%               /* CR1202 initialize on data load */
                  cpos% = 12%              /* CR1202 initialize on data load */
                  gosub process_data
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(2) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(2),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (15)Print Screen"
            pf$(2) = "(4)Previous Scrn  (5) Next Scrn" &        ~
                     "   (10)Process Data             (16)Exit Program"
            pfkeys$ = hex(01ffff0405ffffffffff0affffffff0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
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
            on fieldnr% gosub L50010,        /* Beging Date            */~
                              L50100,        /* Ending Date            */~
                              L50200         /* Selection              */

            return

L50010: REM Beginning Analysis Date                BEG_DTE$, BEG_DATE$
            if len(beg_date$) > 5 then goto L50020
               beg_date$ = date

L50020:     date% = 0%
            call "DATEOKC" (beg_date$, date%, errormsg$)
            if date% = 0% then goto L50030
            x$ = beg_date$
            call "DATUFMTC"(x$)
            beg_dte$ = str(x$,1%,6%)
        return
L50030:     init(" ") beg_dte$, beg_date$, x$
            errormsg$ = "(Error) Invalid Begining Analysis Date?"
            gosub error_prompt
        return 

L50100: REM Ending Delivery Date                   END_DTE$, END_DATE$
            if end_date$ <> " " then goto L50110
               end_date$ = beg_dte$

L50110:     date% = 0%
            call "DATEOKC" (end_date$, date%, errormsg$)
            if date% = 0% then goto L50120
            x$ = end_date$
            call "DATUFMTC"(x$)
            end_dte$ = str(x$,1%,6%)
            if end_dte$ < beg_dte$ then goto L50120
        return
L50120:     init(" ") end_dte$, end_date$, x$
            errormsg$ = "(Error) Invalid Ending Delivery Date?"
            gosub error_prompt
        return

L50200: REM Screen Selection                     scr_sel$, scr_sel_d$
        init(" ") scr_sel_d$
        if scr_sel$ = " " then scr_sel$ = "1"
        if scr_sel$ = "1" then scr_sel_d$ = "<106> = Stagging Analysis "
        if scr_sel$ = "2" then scr_sel_d$ = "<108> = Loading Analysis  "
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
/* CR1202 - rewrite user lookup  */
        load_user     
            read #2,key = id$(kk%), using L61000, id_d$(kk%),  eod goto L61010
L61000:         FMT POS(4), CH(30)
        return
L61010:    id_d$(kk%) = "(Error) Not Valid User Id?"
        return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        glass_warranty                          /* (EWD001) Glass   */
            init(" ") sc_part$
            if sav_sc_key$ = str(ad_rec$,1%,10%) then goto L61030
               sav_sc_key$ = str(ad_rec$,1%,10%)
               hit% = 0%
               sc_key$ = sav_sc_key$
               read #3,key = sc_key$, using L61020, sc_part$,           ~
                                                    eod goto L61030
L61020:           FMT POS(34), CH(25)
               if str(sc_part$,5%,4%) = "WARR" then hit% = 1%
L61030: return            

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            end

