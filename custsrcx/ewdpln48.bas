        REM *************************************************************~
            *  Program Name      - EWDPLN48                             *~
            *  Creation Date     - 07/08/2021                           *~
            *                                                           *~
            *  Description       - Production Labels Printing.          *~
            *                      Prod. Date, Dept. No., Shift & Load  *~
            *                      Range Specified.                     *~
            *                      Barcode Range Specific               *~
            *                      2021 New label layout (CR2860)       *~
            *                                                           *~
            *  Code Tables Used  - PLAN DEPT, PLAN 100, PLANPARTS       *~
            *                                                           *~
            *  Subroutine Used   - EWDPLO48 (Print Production Labels)   *~
            *                      Changed for new Sub Part Number      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *07/08/2021! (New) Program - Copied & Mod EWDPLN42.   ! RDB *~
            *10/19/2021| CR2802 - New 150 black lamainate         ! RDB *~
            *02/08/2022! CR3017 Enhance stock sku check for sizes ! RDB *~
            *************************************************************

        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            count$22, sc_sel$02,         /* Display                    */~
            sc_dept$3, sc_dept_d$30,     /* Department Code & Descr    */~
            sc_prddate$10,               /* Production Date            */~
            sc_end_prddate$10,           /* Ending Production date     */~
            sc_load_fr$5, sc_load_to$5,  /* Load Range                 */~
            sc_shift$2,                  /* Shift Code                 */~
            sc_barcode_fr$18,            /* Beginning Barcode  (EWD002)*/~
            sc_barcode_to$18,            /* Ending Barcode     (EWD002)*/~
            sc_seq_fr$5,                 /* Beginning Sequence  CR2563 */~
            sc_seq_to$5,                 /* Ending Barcode      CR2563 */~ 
            lb_key$35, lb_key1$23,       /* Record Key from LB (PAR001)*/~
            lb_rec$(4%)256,              /* LB Record Data     (PAR001)*/~
            lb_prddate$10,               /* Prod. Date from LB         */~
            lb_dept$3,                   /* Dept. No. from LB          */~
            lb_shift$2,                  /* Shift Code from LB         */~
            lb_load$5,                   /* Load Number from LB        */~
            lb_barcode$18,               /* Production Barcode (EWD002)*/~
            lb_foam$1,                   /* Foam Flag Y,N,0,1  (EWD014)*/~
            lb_part$25,                  /* Part number                */~
            filename$8,                  /* Used by EWDOPEN            */~
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
            userid$3,                    /* Current User Id            */~
            color$1,                     /* Color code     CR2802      */~
            bck_key$25,                  /* BCKMASTR key  CR2756       */~
            custbrcode$2,                /* Custom code abbr 1st 2 chrs*/~
            sc_sel_p$1                   /* Printer Number             */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                            /* (AWD023) */
                                                            /* (AWD025) */
                                                            /* (AWD026) */
                                                            /* (AWD027) */
                                                            /* (PAR001) */
                                                            /* (PAR002) */
                                                            /* (PAR003) */
                                                            /* (PAR004) */
            apc$   = "Generate Lowes Stock Labels- 07-08-2021"
            pname$ = "EWDPLN48 - Rev: R1.00"

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
            * #1  ! APCPLNDT ! Production Master Detail File            *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! EWDPRDLB ! Production Label Data File       (PAR001)*~
            * #6  ! BCKLINES ! Back Log Line Item File                  *~
            * #7  ! AWDSKUXR ! sku x-ref file                           *~
            * #8  ! APCPCMST !                                          *~
            * #9  ! BCKMASTR ! Backlog master file                      *~
            * #63 ! BCKSUBPT ! New Sub Prt Number File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5, "EWDPRDLB",                                       ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23

            select #6, "BCKLINES",                                       ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #7,   "AWDSKUXR",                                   ~
                        varc,     indexed,  recsize = 256,              ~
                        keypos =    1, keylen =  16,                    ~
                        alt key  1, keypos =  17, keylen =  20,         ~
                            key  2, keypos =  37, keylen =  45, dup
/*
 sku#       1- 16 ch(16)
 upc#      17- 36 ch(20)
 part      37- 61 ch(25)
 sub part  62- 81 ch(20)
 model     82- 87 ch(06)
 desc      88-157 ch(70)
filler    158-256 ch(99)
*/
/*AWDPWW*/  select #8, "AWDPCMST",                                      ~
                        varc,     indexed,  recsize =   128,            ~
                        keypos =    9, keylen = 53,                     ~
                        alt key  1, keypos =   1, keylen =  8

            select #9,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

                                                       /* (PAR000)     */


            call "SHOSTAT" ("Opening Files, One Moment Please")
            filename$ = "APCPLNDT" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPRDLB" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDSKUXR" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDPCMST" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
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

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   7%
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
                      if keyhit% = 5% and fieldnr% = 1% then goto run_program
                          
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
                  if keyhit%  = 16% then       dataload
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 6% then editpg1
            col% = cursor%(2%)
            if fieldnr% = 6% and col% = 28 then fieldnr% = 7%
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
            convert sc_sel$ to sc_sel%, data goto L11180
L11180:     sc_sel_p$ = "2"        /* narrow */
            if sc_sel% > 7 then sc_sel_p$ = "1"      /* wide */
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

        scrn1_msg  :  data                                               ~
         "Enter a Production Date Beginning and Ending?                ",~
         "Enter a Specific Department Code or 'ALL'?                   ",~
         "Enter a Sequence Number Range or 'ALL'?                      ",~
         "Enter a Load Range or 'ALL'?                                 ",~
         "Enter a Barcode Range or 'ALL'?                              ",~
         "Enter a Label Type (1 - 7)'                                  ",~
         "Enter a Printer Number (1 or 2)                              "

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
            init(" ") errormsg$, inpmessage$, sc_dept$, sc_dept_d$,      ~
                      sc_prddate$, sc_load_fr$, sc_load_to$, sc_shift$,  ~
                      sc_barcode_fr$, sc_barcode_to$, lb_barcode$,       ~
                      sc_end_prddate$, sc_sel$, sc_sel_p$,               ~
                      sc_seq_fr$, sc_seq_to$           /* CR2563 */
                                                       /* (EWD011)     */
           lbl% = 0%
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload                                         /* (EWD001)   */
            call "SHOSTAT" ("Printing Production Labels...")
            count$ = "Labels Printed (xxxxx)"
            call "DATUFMTC" (sc_prddate$)

            call "DATUFMTC" (sc_end_prddate$)            /* (EWD011)   */

            been_here% = 0%

            lb_key$ = all(hex(00))
            str(lb_key$,1%,6%) = sc_prddate$
          load_next_rec
            read #5, key > lb_key$, using L35040, lb_rec$(),            ~
                                                      eod goto load_done

                                                     /* (AWD025)       */
                                                     /* (PAR001)       */
            lb_key$ = str(lb_rec$(),1%,35%)
                                                     /* (PAR001)       */
            lb_prddate$ = str(lb_key$,1%,6%)
            lb_dept$    = str(lb_key$,12%,3%)
            lb_shift$   = str(lb_key$,15%,2%)
            lb_load$    = str(lb_key$,22%,5%)
            lb_seq$     = str(lb_rec$(),311%,5%)     /* CR2544 */
                                                     /* (PAR001)       */
            lb_barcode$ = str(lb_rec$(),278%,18%)
                                                     /* (PAR001)       */
            lb_foam$    = str(lb_rec$(),601%,1%)     /* (EWD014)       */
                                                     /* (PAR001)       */
                                                     /* (EWD011)       */
            lb_part$    = str(lb_rec$(),523%,25%)
            if lb_prddate$ > sc_end_prddate$ then goto load_done

REM CR2563  if lb_shift$ <> sc_shift$ and sc_shift$ <> "AA"              ~
                then goto load_next_rec

            if sc_dept$ = "ALL" then goto L30000
               if lb_dept$ <> sc_dept$ then goto load_next_rec


L30000:     if str(sc_load_fr$,1%,3%) <> "ALL" then goto L30010
               if pos("AS" = str(lb_load$,1%,1%)) > 0 then               ~
                                            goto load_next_rec
               goto L30020

L30010:     if (lb_load$ < sc_load_fr$ or lb_load$ > sc_load_to$)        ~
                                                  then goto load_next_rec
                                                /* (EWD002) - 07/07/99 */
L30020:     if (lb_barcode$ < sc_barcode_fr$ or lb_barcode$ > sc_barcode_to$)~
                and str(sc_barcode_fr$,1%,3%) <> "ALL" then              ~
                goto load_next_rec
                
            if (lb_seq$ < sc_seq_fr$ or lb_seq$ > sc_seq_to$)        ~
                and str(sc_seq_fr$,1%,3%) <> "ALL" then              ~
                goto load_next_rec              /* CR2563 */
                                                /* (EWD014) - 06/12/03 */
                                                /* Check for Repair    */
            
/* White and Black only colors CR2802 */           
/*********************/
            color$ = str(lb_part$,4%,1%)
            if color$ <> "2" and color$ <> "4" then goto load_next_rec
            
               gosub check_repair
               if lb_foam% = 1% then goto load_next_rec  /* Skip       */
            
            gosub check_ecat_sku
            if ecat% = 1% then goto load_next_rec

            err% = 0%
REM     @@@@@@@@ add sc_sel$ so it can filter by label type.....
            call "EWDPLO48" (been_here%, lb_rec$(), #4, #1, #6, #63, #7, ~
                 #8, sc_sel$, sc_sel_p$, lbl%, err%)
                if err% <> 0% then gosub print_error

REM         lbl% = lbl% + 1%                     /* (EWD015)           */
            if mod(lbl%,25%) <> 0 then goto load_next_rec
               convert lbl% to str(count$,17%,5%), pic(#####)

               call "SHOSTAT" (count$)
            goto load_next_rec

        load_done
                                                /* Finished            */
            call "EWDPLO48" (been_here%, lb_rec$(), #4, #1, #6, #63, #7,    ~
                 #8, sc_sel$, sc_sel_p$, lbl%, 99%)
                if err% <> 0% then gosub print_error
            gosub load_results
            goto inputmode

        check_repair                            /* (EWD014)             */
            lb_foam% = 0%
            if lb_foam$ = "Y" or lb_foam$ = "N" then goto L30110

            p% =  pos("AS" = str(lb_load$,1%,1%))

            if str(sc_barcode_fr$,1%,3%) <> "ALL" then goto L30110

            if p% <> 0% then goto L30110

            lb_foam% = 1%                       /* Skip Label Print     */
L30110: return
                                                /* (EWd014)             */
        
        REM *************************************************************~
            * New eCat validate check     CR2756                        *~
            *   Should not be needed one APCPLN06 goes into production  *~
            *    already planned orders are completed.                  *~
            *************************************************************
            
        check_ecat_sku
               init(" ") bck_key$
               ecat% = 0%
               
               str(bck_key$,1%,9%)   = str(lb_rec$(),727%,9%)
               str(bck_key$,10%,16%) = str(lb_barcode$,1%,8%)
      
               read #9,key = bck_key$, using L30200, bck_user_entered$, ~
                                                  eod goto L30220
L30200:          FMT POS(836), CH(03)

                  if bck_user_entered$ = "ECT" then ecat% = 1%
L30220:
        return
        
        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                                         /* (EWDPRDLB) */
                                                         /* (PAR001)   */
L35040:     FMT 4*CH(256)
                                                         /* (PAR001)   */

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
              on fieldnr% gosub L40170,          /* sc_prddate$        */~
                                L40160,          /* sc_dept$           */~
                                L40160,     /* sc_load_fr$,sc_load_to$ */~
                                L40160,     /* sc_barcode_fr$ & _to$   */~
                                L40160,     /* sc_seq_fr$ _to$ CR2563  */~
                                L40160,     /* selection               */~
                                L40160      /* printer number          */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

/* CR2563  replace shift code with seq range */
L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Prod Beg/End Date:",                         ~
               at (03,25), fac(lfac$(1%)), sc_prddate$          , ch(10),~
               at (03,40), fac(lfac$(1%)), sc_end_prddate$      , ch(10),~
                                                                         ~
               at (04,02), "Dept. Code       :",                         ~
               at (04,25), fac(lfac$(2%)), sc_dept$             , ch(03),~
               at (04,40), fac(hex(84)),   sc_dept_d$           , ch(30),~
                                                                         ~
               at (05,02), "Sequence No Range:",                         ~
               at (05,25), fac(lfac$(3%)), sc_seq_fr$           , ch(05),~
               at (05,45), fac(lfac$(3%)), sc_seq_to$           , ch(05),~
                                                                         ~
               at (06,02), "Load No. Range   :",                         ~
               at (06,25), fac(lfac$(4%)), sc_load_fr$          , ch(05),~
               at (06,45), fac(lfac$(4%)), sc_load_to$          , ch(05),~
                                                                         ~
               at (07,02), "Barcode No. Range:",                         ~
               at (07,25), fac(lfac$(5%)), sc_barcode_fr$       , ch(18),~
               at (07,45), fac(lfac$(5%)), sc_barcode_to$       , ch(18),~
                                                                         ~
               at (08,02), "Label Type       :",                         ~
               at (08,25), fac(lfac$(6%)), sc_sel$              , ch(02),~
                                                                         ~
               at (08,28), fac(lfac$(7%)), sc_sel_p$            , ch(01),~
                                                                         ~
               at (10,02), "      01 = Series 3201 DH",                  ~
               at (11,02), "      02 = Series 3201 DH w/grid",           ~
               at (12,02), "      03 = Series 3050 SH",                  ~
               at (13,02), "      04 = Series 3050 SH w/grid",           ~
               at (14,02), "      05 = Series 3050 Slider",              ~
               at (15,02), "      06 = Series 3100 SH",                  ~
               at (16,02), "      07 = Series 3100 SH w/grid",           ~
               at (17,02), "      08 = Series 450 DHHP",                 ~
               at (18,02), "      09 = Series 450 DHHP w/grid",          ~
               at (19,02), "      10 = Series 105 SH",                   ~
               at (20,02), "      11 = Series 105 SH w/grid",            ~
               at (10,40), "      12 = Series 105 Slider",               ~
               at (11,40), "      13 = Series 150 SH",                   ~
               at (12,40), "      14 = Series 150 SH w/grid",            ~
               at (13,40), "      15 = Series 151 Slider",               ~
               at (14,40), "      16 = Series 130 Slider",               ~
               at (15,40), "      17 = Series 150 Black SH",             ~
               at (16,40), "      18 = Series 150 Black SH w/grid",      ~
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
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                       (5)Bulk Print     "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff0405ffffffffffffffffff0f1000)         
            if fieldnr% = 1% then L40570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                      (16)PRINT LABELS "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
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
            on fieldnr% gosub L50010,        /* Prod Beg/End Date      */~
                              L50050,        /* Department Code        */~
                              L50255,        /* Sequence Range CR2563  */~
                              L50090,        /* Load No. Range         */~
                              L50200,        /* Barcode Range          */~
                              L50300,        /* Label Type             */~
                              N50300         /* Printer Number         */

            return
                                                  /* (EWD011)         */
L50010: Rem Enter a Production Date                sc_prddate$
            call "DATEOKC" (sc_prddate$, 0%, errormsg$)

            if len(sc_end_prddate$) < 5 then                  ~
                                  sc_end_prddate$ = sc_prddate$

            call "DATEOKC" (sc_end_prddate$, 0%, errormsg$)

            if sc_prddate$ > sc_end_prddate$ then goto L50015

            if sc_end_prddate$ < sc_prddate$ then goto L50020
            return
L50015:    errormsg$ = "(Error) Invalid Beginning Production date?"
           gosub error_prompt
           init(" ") sc_prddate$, sc_end_prddate$
        return
L50020:    errormsg$ = "(Error) Invalid Ending Production date?"
           gosub error_prompt
           init(" ") sc_prddate$, sc_end_prddate$
        return


L50050: Rem Enter a Valid Department Code          sc_dept$, sc_dept_d$
            init(" ") sc_dept_d$
            if sc_dept$ <> " " then goto L50055
               sc_dept$ = "ALL"

L50055:     if sc_dept$ <> "ALL" then goto L50060
                sc_dept_d$ = "*** All Departments"
                return
L50060:     gosub check_dept
            if dept% = 0% then goto L50070
               sc_dept_d$ = desc$
        return

L50070:     errormsg$ = "(Error) Invalid Department Code"
            gosub error_prompt
            init(" ") sc_dept$, sc_dept_d$
        return
       
L50090: Rem Enter a Load No. Range               sc_load_fr$, sc_load_to$
            if str(sc_load_fr$,1%,1%) <> " " then goto L50095
               sc_load_fr$ = "ALL  "
               sc_load_to$ = sc_load_fr$

L50095:     if str(sc_load_fr$,1%,3%) <> "ALL" then goto L50098
               sc_load_to$ = "ALL  "
               return
L50098:     if sc_load_to$ = " " then sc_load_to$ = sc_load_fr$
            if sc_load_fr$ > sc_load_to$ then goto L50100
        return
L50100:     errormsg$ = "'TO' Load No. must be > or = 'FROM' Load No."
            gosub error_prompt
            init(" ") sc_load_fr$, sc_load_to$
        return

L50200: Rem Enter a Barcode Range             sc_barcode_fr$, sc_barcode_to$
            if str(sc_barcode_fr$,1%,1%) <> " " then goto L50205
               sc_barcode_fr$ = "ALL Barcodes      "
               sc_barcode_to$ = sc_barcode_fr$

L50205:     if str(sc_barcode_fr$,1%,3%) <> "ALL" then goto L50210
               sc_barcode_to$ = sc_barcode_fr$
                                                   /* (AWD026)        */
               if userid$ = "WWW" then goto L50215
               return

L50210:     if len(sc_barcode_to$) < 3 then                         ~
               sc_barcode_to$ = sc_barcode_fr$

            lb_barcode$ = sc_barcode_fr$
            gosub check_barcode
            if barcode% = 0% then goto L50230

            lb_barcode$ = sc_barcode_to$
            gosub check_barcode
            if barcode% = 0% then goto L50230

            if sc_barcode_fr$ > sc_barcode_to$ then goto L50220
                                                    /* (AWD026)          */
L50215:
            if userid$ <> "WWW" then return
               if str(sc_barcode_fr$,1%,3%) = "ALL" then goto L50250
               if sc_barcode_to$ <> sc_barcode_fr$  then goto L50250
                                                    /* (AWD026)          */
        return
L50220:     errormsg$ = "'TO' Barcode No. must be > or = 'FROM' Barcode No."
            gosub error_prompt
            init(" ") sc_barcode_fr$, sc_barcode_to$
        return
L50230:     errormsg$ = "(Error) Invalid Barcode, or Not on File"
            gosub error_prompt
            init(" ") sc_barcode_fr$, sc_barcode_to$
        return
                                                    /* (AWD026)          */
L50250:     errormsg$ = "(Error) Invalid Barcode, Can Only Print Single Label?"
            gosub error_prompt
            init(" ") sc_barcode_fr$, sc_barcode_to$
        return
                 
/* CR2563 */       
L50255: Rem Enter a Sequence No. Range               sc_seq_fr$, sc_seq_to$
            if str(sc_seq_fr$,1%,1%) <> " " then goto L50257
               sc_seq_fr$ = "ALL  "
               sc_seq_to$ = sc_seq_fr$
               
L50257:     if len(sc_seq_fr$) < 5 and sc_seq_fr$ <> "ALL  " then goto L50261 
            
            if str(sc_seq_fr$,1%,3%) <> "ALL" then goto L50259
               sc_seq_to$ = "ALL  "
               return
L50259:     if sc_seq_to$ = " " then sc_seq_to$ = sc_seq_fr$
            if sc_seq_fr$ > sc_seq_to$ then goto L50260
            if len(sc_seq_to$) < 5 and sc_seq_to$ <> "ALL  " then ~
               sc_seq_to$ = sc_seq_fr$            
        return
L50260:     errormsg$ = "'TO' Sequence No. must be > or = 'FROM' Sequence No."
            gosub error_prompt
            init(" ") sc_seq_fr$, sc_seq_to$
        return 
L50261:     errormsg$ = "'TO' Sequence No. must be 5 digits with leading zeros"
            gosub error_prompt
            init(" ") sc_seq_fr$, sc_seq_to$
        return 
        
                               /* (AWD026)          */
L50300:
            convert sc_sel$ to sc_sel%, data goto L50350

            convert sc_sel% to sc_sel$, pic(00)

            if sc_sel$ < "01" or sc_sel$ > "18" then goto L50350
        return

L50350:    errormsg$ = "(Error) Invalid Label Type (1-18)"
           gosub error_prompt
           init(" ") sc_sel$
        return
N50300:     if sc_sel_p$ = " " then sc_sel_p$ = "1"
            convert sc_sel_p$ to sc_sel_p%, data goto N50350

            convert sc_sel_p% to sc_sel_p$, pic(0)

            if sc_sel_p$ < "1" or sc_sel_p$ > "2" then goto N50350
        return

N50350:    errormsg$ = "(Error) Invalid Printer Number (1 or 2)"
           gosub error_prompt
           init(" ") sc_sel_p$
        return

        check_dept
            dept% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = sc_dept$
            read #4,key = readkey$, using L51000, desc$, eod goto L51010
L51000:        FMT POS(25), CH(30)
            dept% = 1%
L51010: return
                                                    /* (EWD002) -     */
        check_barcode
            barcode% = 0%                           /* (PAR001)       */
            init(" ") lb_key1$
            str(lb_key1$,1%,18%) = lb_barcode$
            read #5,key 1% > lb_key1$, using L52000, lb_key1$, eod goto L52010
L52000:        FMT POS(278), CH(23)
            if str(lb_key1$,1%,18%) <> lb_barcode$ then goto L52010
               barcode% = 1%
L52010: return                                      /* (PAR001)       */

        run_program
           return% = 0% : comp% = 0%
           init(" ") rlib$, rvol$
           run$ = "EWDPLN49"
           rlib$ = "/APCSRC/APCOBJ"
           call "PROCLINK" (run$, rlib$, rvol$, return%, comp%)
           goto exit_program
           
        return
        
        
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error                                    /* (EWD004)        */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                                                      /* (EWD004)        */
        load_results
           k% = 2%
           hdr$     = "***** Label Generation Results *****"
           msg$(1%) = "This run generated xxxxx label(s)."
           msg$(2%) = "---"
           msg$(3%) = "Press <ENTER> to Acknowledge & Continue"
           convert lbl% to str(msg$(1%),20%,5%), pic(####0)
           if lbl% <> 0% then L64100
               msg$(1%) = "NO LABELS GENERATED!!!"
               str(msg$(3%),,13%) = " Press <PF16>"
L64100:    call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if lbl% <> 0% and k% <>  0% then load_results
           if lbl%  = 0% and k% <> 16% then load_results
           lbl% = 0%
        return

        print_error
            hdr$     = "***** Label Printing Error *****"
            msg$(1%) = "ERROR OCCURRED WHILE PRINTING LABELS!!!"
            msg$(2%) = "Return Code (EWDPLN48) = "
            msg$(3%) = "Press <ENTER> to Continue, <PF16> to Exit"

            convert err% to str(msg$(2%),26%,2%), pic(#0)
L64550:     k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
                if k%  =  0% then return
                if k% <> 16% then goto L64550
            return clear all
            goto inputmode

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program

        end

