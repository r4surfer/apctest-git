        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLN99                             *~
            *  Creation Date     - 01/28/97                             *~
            *  Last Modified Date- 05/30/2019                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Utility Program used to Insert a     *~
            *                      Sales Order Defined for Load (A)     *~
            *                      and Move the Sales Order to Load(B). *~
            *                      - (1) Deleate Sales Order from Load-A*~
            *                      - (2) Move Sales Order to Load (B).  *~
            *                      - (3) Rebuild Sort Index             *~
            *                      - (4) Rebuild Drop Seq.              *~
            *                      - (5) Rebuild Seq No.      - N/A     *~
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
            * 01/28/97 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 06/16/03 ! (EWD001) - Mod to not allow to insert onto CMG *~
            *          !          an Appian Load                  !     *~
            * 08/27/08 ! (AWD002) - mod for status check          ! CMG *~
            * 12/20/18 ! (CR1829) - cust code in from & too 6 -> 9! DES *~
            *04/29/2019! (CR2050) Atrium Load Inserts upload to   ! CMN *~
            *          !          send update to ATLaS            !     *~            
            *12/17/2019! (CR2151) - Add all free command          ! RDB *~
			*02/28/2023! CR3268 - No resequence on Appian load    ! RDB *~
            *************************************************************

        dim                                                              ~
            sel$1, sel_d$30,             /* Insert Selection Type      */~
            in1$16, in1_d$28,            /* From Selection             */~
            in2$16, in2_d$28,            /* To Selection               */~
            in3$2, in4$5,                /* Assign Drop Seq. No.       */~
            in1_key$8,                   /* From S.O. for Insert       */~
            in2_key$8,                   /* Too S.O. for Insert        */~
            from$(10%)40, too$(10%)40,   /* For Display Screen         */~
            scr_load$5, scr_load_d$30,   /* Screen Load No. Re-Seq.    */~
            or_rec$170,                  /* S.O. Header History Rec    */~
            or_key$16,                   /* 'OR' and 'DT' Keys         */~
            or_route$5,                  /* HEADER ROUTE CODE          */~
            or_po$16,                    /* HEADER P.O. NUMBER         */~
            dt_key$23,                   /* 'DT' Key                   */~
            dt_so$8,                     /* Customer Sales Order       */~
            dt_cuscode$9, dt_cuscode_d$30, /* Customer Code            */~
            dt_po$16,                    /* Customer Purchase Order No */~
            dt_load$5, dt_load_d$30,     /* Load Number and Description*/~
            dt_drop_seq$5,               /* Drop Sequence No.          */~
            dt_drop$2,                   /* Customer Drop Number       */~
            ld_dtp3$8,                   /* Truck Load Date            */~
            lookup$16,                   /* Reference Load Number      */~
            dt_rec$256,                  /* Planning Detail Record     */~
	        bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            sc_rec$128,                  /* (APCPLNSC) - Line Items    */~
            sc_cuscode$9,                /*                            */~
            sc_so$8,                     /*                            */~
            sc_line$2,                   /*                            */~
            sc_drop_seq$5, seq$5,        /* DROP SEQ. NO.              */~
            sc_drop$2,                   /* CUSTOMER DROP NO           */~
            sc_key1$27, sc_key$10,       /* LINE ITEM LOAD KEY         */~
            sc_load$, new_load$5,        /* S.O. LOAD AND NEW LOAD     */~
            sort_key$60, sav_so$8,       /* USED FOR RE-SEQ.           */~
            sort_key1$60,                /* USED BY (APCPLN9B) SORT    */~
            pl_sort$12,                  /* USED FOR (APCPLNDT) SORT   */~
            pl_key$11,                   /* PRIMARY KEY - (APCPLNUC)   */~
            sav_cuscode$9,               /*                            */~
            hdr$40, msg$(3%)79,          /* ASKUSER Arrays             */~
            cursor%(24%),                /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            ap_type$1,                   /* Appian Load?       (EWD001)*/~
            status$2                     /* Status Check       (AWD002)*/~

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
            rslt$(10%)20                 /* Text from file opening     */
            
         
        dim atlstatus$2,                /* Status code        (CR2050)  */~
            pgmname$10,                 /* Program Name       (CR2050)  */~
            schema$8                    /* Schema             (CR2050)  */            

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Special Utility for SO Insert into Loads"
            pname$ = "APCPLN99 - Rev: R6.04"

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
            * #1  ! APCPLNDT ! Production Master Detail                 *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #3  ! APCPLNOR ! Sales Order Header History               *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! APCPLNLD ! Load Header Information                  *~
            * #6  ! APCPLNSC ! Planning Master Scheduling File          *~
            * #7  ! APCPLNUC ! Planning Master Units Capacity file      *~
            * #8  ! BCKMASTR ! S.O. Master Header File                  *~
            * #9  ! BCKLINES ! S.O. Line Detail File                    *~
            * #10 ! APCRESEQ ! Use to Re-Sequence Load and Assign Drops *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup
REM                         key  4, keypos =   06, keylen =   8, dup

            select #2,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #3,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =   170,             ~
                        keypos =    1, keylen =  51,                     ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =   11, keylen =   5,                     ~
                        alt key  1, keypos =    3, keylen =  13,         ~
                            key  2, keypos =    1, keylen =  15

            select #6,   "APCPLNSC",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #7,   "APCPLNUC",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   11,                    ~
                        alt key  1, keypos =    7, keylen =  11

            select #8,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #9,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #10, "APCRESEQ",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  60

             select #11, "AWDAPPLD"                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  16,         ~
                            key  2, keypos =    2, keylen =  15,         ~
                            key  3, keypos =   17, keylen =  15

            select #12, "PGORLNTR",                    /* (CR2050) */    ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   21, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  64,         ~
                            key  2, keypos =   54, keylen =  11, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))

            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),  0%, rslt$(7%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9, fs%(9%), f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),  0%, rslt$(11%))
/*(CR2050)*/ call "OPENCHCK" (#12, fs%(12%), f2%(12%),  0%, rslt$(12%)) 

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

            schema_err%, schema% = 0%                           /* (CR2050) */
            init(" ") schema$                                   /* (CR2050) */
            call "SCHEMA" (schema$, schema%, #4, schema_err%)   /* (CR2050) */ 

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   5%
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
                      if keyhit% =  9% then goto inputmode_a
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
                  if keyhit%  = 16% then gosub insert_order
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
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
            *  R e s e q    L o a d s   a n d   A s s i g n   D r o p s *~
            *************************************************************

        inputmode_a
            gosub initialize_variables

            for fieldnr% = 1% to   1%
L12160:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12280
L12180:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12260
L12210:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L12180
                         if fieldnr% = 1% then L12160
                         goto L12210
L12260:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12180
L12280:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12180
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub re_seq_load
                  if keyhit%  = 16% then gosub re_seq_route
                  if keyhit% <>  0% then       editpg2
L13120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 1% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13170:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13170
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13170
                  lastfieldnr% = fieldnr%
            goto L13120

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        insert_order
            call "SHOSTAT" ("Inserting S.O.("&in1_key$&") into Load ("&  ~
                                 scr_load$& ")" )
                                 
            atlstatus$ = "04"                          /* (CR2050) */
            pgmname$   = "APCPLN99"                    /* (CR2050) */                                 
            reseq% = 99%
            sc_drop_seq$ = "00000"
            sc_drop$     = "00"
            if in4$ <> "00000" then sc_drop_seq$ = in4$

            sc_so$       = in1_key$              /* Insert Sales Order */
            read #3,hold,key 4% = in2_key$, using L19170, sc_drop$,       ~
                                              new_load$, eod goto L19470
L19170:        FMT POS(25), CH(2), POS(94), CH(5)
            if in3$ <> "00" then sc_drop$ = in3$
            gosub update_header                /* Update with New Load */

            if in4$ = "00000" then goto L19240
               sc_drop_seq$ = in4$
               goto L19300
L19240:     init(" ") sc_key$, sc_rec$
            str(sc_key$,1%,8%) = in2_key$
            read #6,key > sc_key$, using L19350, sc_rec$, eod goto L19300
               if in2_key$ <> str(sc_rec$,24%,8%) then goto L19300
                  sc_drop_seq$ = str(sc_rec$,12%,5%)

L19300:     init(" ") sc_key$, sc_rec$
            str(sc_key$,1%,8%) = sc_so$
        insert_order_nxt
            read #6,key > sc_key$, using L19350, sc_rec$,                 ~
                                                     eod goto insert_done
L19350:        FMT CH(128)
            sc_key$ = str(sc_rec$,24%,10%)
            if sc_so$ <> str(sc_key$,1%,8%) then goto insert_done
               sc_cuscode$ = str(sc_rec$,59%,6%)                /* (CR2050) */
               sc_line$ = str(sc_rec$,32%,2%)
               sc_load$ = str(sc_rec$,7%,5%)
               sc_key1$ = str(sc_rec$,7%,27%)
               gosub update_lines
               if schema% = 2% then gosub updateRemoteOrderLine   /*(CR2050)*/
               goto insert_order_nxt
        insert_done

        return clear all
        goto inputmode
L19470:     errormsg$ = "(Error) - Unable to Insert S.O. -- "& sc_so$
            gosub error_prompt
            goto insert_done

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
         return

        deffn'061(fieldnr%)
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
         "Enter the Applic. Data Lookup,1 = S.O.,2 = Warranty,3 = P.O.?",~
         "Enter the Applic. S.O., Warranty, or P.O. Code to be Moved?  ",~
         "Enter the Applic S.O., Warranty, or P.O. Code for Destination",~
         "Enter the Drop No. to Assign, or <RETURN> for N/A?           "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28250
                inpmessage$ = edtmessage$
                return

L28250
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid load Number to Re-Sequence Drops?              "


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sel$, sel_d$, in1$, in1_d$,~
                      in2$, in2_d$, from$(), too$(), lookup$, or_key$,   ~
                      dt_key$, dt_so$, dt_cuscode$, dt_cuscode_d$,       ~
                      dt_po$, dt_load$, dt_load_d$, in3$, scr_load$,     ~
                      scr_load_d$, in4$, dt_drop_seq$, dt_drop$, ap_type$
            reseq% = 0%
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
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40190,         /* Insert Selection Opt*/ ~
                                L40190,         /* Insert From Data    */ ~
                                L40190,         /* Insert Too Data     */ ~
                                L40190,         /* Customer Drop No.   */ ~
                                L40190          /* Cust Drop Seq No.   */
              goto L40220

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40220:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Insert Option       :",                      ~
               at (04,25), fac(lfac$(1%)), sel$                 , ch(01),~
               at (04,50), fac(hex(84)),   sel_d$               , ch(28),~
                                                                         ~
               at (05,02), "Insert (From Data)  :",                      ~
               at (05,25), fac(lfac$(2%)), in1$                 , ch(16),~
               at (05,50), fac(hex(84)),   in1_d$               , ch(28),~
                                                                         ~
               at (06,02), "Insert (Too Data)   :",                      ~
               at (06,25), fac(lfac$(3%)), in2$                 , ch(16),~
               at (06,50), fac(hex(84)),   in2_d$               , ch(28),~
                                                                         ~
               at (07,02), "Customer Drop No.   :",                      ~
               at (07,25), fac(lfac$(4%)), in3$                 , ch(02),~
                                                                         ~
               at (08,02), "Customer Drop Seq No:",                      ~
               at (08,25), fac(lfac$(5%)), in4$                 , ch(05),~
                                                                         ~
               at (10,02), fac(hex(84)),   from$( 1%)           , ch(40),~
               at (11,02), fac(hex(84)),   from$( 2%)           , ch(40),~
               at (12,02), fac(hex(84)),   from$( 3%)           , ch(40),~
               at (13,02), fac(hex(84)),   from$( 4%)           , ch(40),~
               at (14,02), fac(hex(84)),   from$( 5%)           , ch(40),~
               at (15,02), fac(hex(84)),   from$( 6%)           , ch(40),~
               at (16,02), fac(hex(84)),   from$( 7%)           , ch(40),~
               at (17,02), fac(hex(84)),   from$( 8%)           , ch(40),~
               at (18,02), fac(hex(84)),   from$( 9%)           , ch(40),~
                                                                         ~
               at (10,40), fac(hex(84)),   too$( 1%)            , ch(40),~
               at (11,40), fac(hex(84)),   too$( 2%)            , ch(40),~
               at (12,40), fac(hex(84)),   too$( 3%)            , ch(40),~
               at (13,40), fac(hex(84)),   too$( 4%)            , ch(40),~
               at (14,40), fac(hex(84)),   too$( 5%)            , ch(40),~
               at (15,40), fac(hex(84)),   too$( 6%)            , ch(40),~
               at (16,40), fac(hex(84)),   too$( 7%)            , ch(40),~
               at (17,40), fac(hex(84)),   too$( 8%)            , ch(40),~
               at (18,40), fac(hex(84)),   too$( 9%)            , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40740
                  call "PRNTSCRN"
                  goto L40220

L40740:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40940     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)Resequence Load     " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffffff0e0f1000)
            if fieldnr% = 1% then L40900
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
                str(pf$(3%),18%,26%) = " " : str(pfkeys$,9%,1%) = hex(ff)
L40900:     if fieldnr% > 1% then L40920
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40920:     return

L40940: if fieldnr% > 0% then L41030  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                 (15)Print Screen      "
            pf$(3) = "                                        " &        ~
                     "                 (16)Insert Data       "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L41030:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return


        REM *************************************************************~
            *               S C R E E N   T W O                         *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42190          /* Load Number         */

              goto L42220

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42220:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (05,02), "Production Load No:",                        ~
               at (05,25), fac(lfac$(1%)), scr_load$            , ch(05),~
               at (05,40), fac(hex(84)),   scr_load_d$          , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L42720
                  call "PRNTSCRN"
                  goto L42220

L42720:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42920     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return

L42920: if fieldnr% > 0% then L43010  /*  Edit Mode - Select Fld */

        if ap_type$ = "Y" then goto L43010       /* CR3268 No req-seq appian */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                    (14)Re-Seq by P.O. "
            pf$(2) = "                                        " &        ~
                     "                    (15)Print Screen   "
            pf$(3) = "                                        " &        ~
                     "                    (16)Re-Seq by Route"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L43010:                              /*  Edit Mode - Enabled    */
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
            errormsg$ = " "
            call "ALLFREE" 
            on fieldnr% gosub L50075,         /* Insert Selection      */ ~
                              L50155,         /* Insert From Data      */ ~
                              L50225,         /* Insert Too Data       */ ~
                              L50295,         /* Customer Drop Number  */ ~
                              L50355          /* Customer Drop Seq     */
            return

L50075: REM Insert Data Type Selection            SEL$
            if sel$ <> " " then goto L50090
               sel$ = "1"
L50090:     convert sel$ to sel%, data goto L50130

            if sel% < 1% or sel% > 3% then goto L50130
               if sel$ = "1" then sel_d$ = "Customer Sales Order No. "
               if sel$ = "2" then sel_d$ = "APC Warranty Number      "
               if sel$ = "3" then sel_d$ = "Customer P.O. Number     "

        return
L50130:     errormsg$ = "(Error) - Invalid Insert Data Selection (1-3)?"
            gosub error_prompt
            init(" ") sel$, sel_d$
        return

L50155: REM Insert From Data                      IN1$
            if in1$ <> " " then goto L50170
               goto L50200
L50170:     lookup$ = in1$
            if sel$ = "2" then gosub lookup_dt                           ~
                          else gosub lookup_or
            if lookup% <> 1% then goto L50200
               gosub format_from
        return
L50200:     errormsg$ = "(Error) Invalid From Data Entered for Select?"
            gosub error_prompt
            init(" ") in1$, in1_d$, from$()
        return

L50225: REM Insert Too Data                       IN2$
            if in2$ <> " " then goto L50240
               goto L50270
L50240:     lookup$ = in2$
            if sel$ = "2" then gosub lookup_dt                           ~
                          else gosub lookup_or

            if ap_type$ = "Y" then goto L50275          /*  (EWD001)  */
            if status$ < "03" or status$ > "16" then goto L50280 /*(AWD002)*/
            if lookup% <> 1% then goto L50270
               gosub format_too
        return
L50270:     errormsg$ = "(Error) Invalid Too Data Entered for Select?"
            gosub error_prompt
            init(" ") in2$, in2_d$, too$()
        return
                                                       /*  (EWD001)  */
L50275:     errormsg$ = "(Error) Cannot Insert into Appian Load!"
            gosub error_prompt
            init(" ") in2$, in2_d$, too$()
        return
                                                       /*  (AWD002)  */
L50280:     errormsg$ = "(Error) Cannot Insert Not Planned Or Invoiced Order!"
            gosub error_prompt
            init(" ") in2$, in2_d$, too$()
        return

L50295: REM Customer Drop Number                 IN3$
            if in3$ <> " " then goto L50315
               in3$ = "00"
            in3% = 0%
L50315:     convert in3$ to in3%, data goto L50340

            convert in3% to in3$, pic(00)

        return
L50340:     in3$ = "00"
        return

L50355: REM Customer Drop Seq. Number            IN4$
            if in4$ <> " " then goto L50375
               in4$ = "00000"
            in4% = 0%
L50375:     convert in4$ to in4%, data goto L50400

            convert in4% to in4$, pic(00000)

        return
L50400:     in4$ = "00000"
        return

        lookup_or                                /* Sales Order and    */
            status$ = "00"                       /* (AWD002) */
            lookup% = 0%                         /* P.O. Lookup        */
            init(" ") or_key$, dt_so$, dt_cuscode$, dt_load$, dt_po$,    ~
                      dt_drop_seq$, dt_drop$
            if sel$ <> "1" then goto L50455
               str(or_key$,1%,8%) = lookup$      /* Sales Order Lookup */
               ff% = 4%
               goto L50465
L50455:     str(or_key$,1%,16%) = lookup$        /* P.O. Lookup        */
            ff% = 5%
L50465:     read #3,hold,key ff% = or_key$, using L50475, or_rec$,        ~
                                                  eod goto lookup_or_done
L50475:        FMT CH(170)
            dt_so$      = str(or_rec$,52%,8%)
            dt_cuscode$ = str(or_rec$,27%,9%)
            dt_load$    = str(or_rec$,94%,5%)
            dt_po$      = str(or_rec$,36%,16%)
            status$     = str(or_rec$,60%,2%)      /* (AWD002) */
            gosub lookup_drop_info
            gosub lookup_customer
            gosub lookup_load
            lookup% = 1%
        lookup_or_done
        call "ALLFREE"
        return

        lookup_dt                              /* Warranty Code Lookup */
            status$ = "00"                           /* (AWD002) */
            lookup% = 0%
            init(" ") dt_key$, dt_so$, dt_cuscode$, dt_load$, dt_drop$,  ~
                      dt_drop_seq$
            str(dt_key$,1%,8%) = lookup$
            read #1,hold,key 4% = dt_key$, using L50570, dt_rec$,         ~
                                                  eod goto lookup_dt_done
L50570:        FMT CH(256)
            dt_so$       = str(dt_rec$,24%,8%)
            dt_cuscode$  = str(dt_rec$,124%,9%)
            dt_load$     = str(dt_rec$, 1%,5%)


            read #3,hold,key 4% = dt_so$, using L50605, dt_po$, status$, ~
                                                  eod goto lookup_dt_done
L50605:        FMT POS(36), CH(16), POS(60), CH(02)    /* (AWD002) */
            gosub lookup_drop_info
            gosub lookup_customer
            gosub lookup_load
            lookup% = 1%
        lookup_dt_done
            call "ALLFREE"
        return

        lookup_drop_info
            init(" ") sc_key$
            str(sc_key$,1%,8%) = dt_so$
            read #6,key > sc_key$, using L50670, dt_drop_seq$, dt_drop$,  ~
                                                sc_key$, eod goto L50685
L50670:        FMT POS(12), CH(5), CH(2), POS(24), CH(10)
            if str(sc_key$,1%,8%) <> dt_so$ then goto L50685
        return
L50685:     dt_drop_seq$ = "Error"
            dt_drop$     = "00"
        return

        lookup_customer
            init(" ") dt_cuscode_d$
            read #2,key = dt_cuscode$, using L50725, dt_cuscode_d$,       ~
                                            eod goto lookup_customer_done
L50725:        FMT POS(10), CH(30)
        lookup_customer_done
        return

        lookup_load
            init(" ") dt_load_d$
            read #5,key = dt_load$, using L50765, dt_load_d$, ld_dtp3$,   ~
                                            eod goto lookup_load_done
L50765:        FMT POS(16), CH(30), POS(86), CH(8)
            call "DATEFMT" (ld_dtp3$)
        lookup_load_done
            gosub lookup_app_load                            /* (EWD001) */
        return

        lookup_app_load                                      /*  (EWD001)  */
            init(" ") ap_type$
            read #11, key = dt_load$, using L50800, ap_type$,             ~
                                                  eod goto no_app_load
L50800:                 FMT POS(83), CH(2)
        no_app_load
        return                                               /*  (EWD001)  */

        format_from
            init(" ") from$(), in1_key$
            from$(1%) = "<- Insert Sales Order 'From Data'->"
            from$(2%) = "-----------------------------------"
            from$(3%) = "Load (XXXXX)    :XXXXXXXXXXXXXXXXXXXX "
            from$(4%) = "Cust (XXXXXXXXX):XXXXXXXXXXXXXXXXXXXX "
            from$(5%) = "Sales Order     :XXXXXXXX             "
            from$(6%) = "P.O. Number     :XXXXXXXXXXXXXXXX     "
            from$(7%) = "Loading Date    :XX/XX/XX             "
            from$(8%) = "Curr Drop Seq   :XXXXX                "
            from$(9%) = "Curr Drom No.   :XX                   "

            str(from$(3%),7%,5%)   = dt_load$
            str(from$(3%),18%,20%) = dt_load_d$
            str(from$(4%),7%,9%)   = dt_cuscode$
            str(from$(4%),18%,20%) = dt_cuscode_d$
            str(from$(5%),18%,8%)  = dt_so$
            str(from$(6%),18%,16%) = dt_po$
            str(from$(7%),18%,8%)  = ld_dtp3$
            str(from$(8%),18%,5%)  = dt_drop_seq$
            str(from$(9%),18%,2%)  = dt_drop$
            str(in1_key$,1%,8%)    = dt_so$
        return

        format_too
            init(" ") too$(), in2_key$
            too$(1%) = "<- Insert Sales Order 'Into Data'->"
            too$(2%) = "-----------------------------------"
            too$(3%) = "Load (XXXXX)    :XXXXXXXXXXXXXXXXXXXX "
            too$(4%) = "Cust (XXXXXXXXX):XXXXXXXXXXXXXXXXXXXX "
            too$(5%) = "Sales Order     :XXXXXXXX             "
            too$(6%) = "P.O. Number     :XXXXXXXXXXXXXXXX     "
            too$(7%) = "Loading Date    :XX/XX/XX             "
            too$(8%) = "Curr Drop Seq   :XXXXX                "
            too$(9%) = "Curr Drom No.   :XX                   "

            str(too$(3%),7%,5%)   = dt_load$
            str(too$(3%),18%,20%) = dt_load_d$
            str(too$(4%),7%,9%)   = dt_cuscode$
            str(too$(4%),18%,20%) = dt_cuscode_d$
            str(too$(5%),18%,8%)  = dt_so$
            str(too$(6%),18%,16%) = dt_po$
            str(too$(7%),18%,8%)  = ld_dtp3$
            str(too$(8%),18%,5%)  = dt_drop_seq$
            str(too$(9%),18%,2%)  = dt_drop$
            str(in2_key$,1%,8%)   = dt_so$
            scr_load$ = dt_load$
        return

        REM *************************************************************~
            *          E d i t   L o a d   N u m b e r                  *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52140          /* Load Number           */

            return

L52140: REM Valid Load Number for Re-Seq          SCR_LOAD$
            if scr_load$ <> " " then goto L52170
               goto L52210
L52170:     dt_load$ = scr_load$
            gosub lookup_app_load           /* CR3268 no appian resequencing */
			if ap_type$ = "Y" then goto L52220
            read #5,key = scr_load$, using L52190, scr_load_d$,           ~
                                                           eod goto L52210
L52190:        FMT POS(16), CH(30)
        return
L52210:     errormsg$ = "(Error) - Invalid Load Number??"
            gosub error_prompt
            init(" ") scr_load$, scr_load_d$
        return
L52220:     errormsg$ = "(Error) - Cannot Resequence Appian Routed Load??"
            gosub error_prompt
            init(" ") scr_load$, scr_load_d$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        re_seq_route
            call "SHOSTAT" ("Assign Drops/Re-Seq by Route")
            reseq% = 1%                         /* By Route Flag       */
            goto L60200
        re_seq_load
            reseq% = 0%                         /* By Customer/P.O.    */
            call "SHOSTAT" ("Assign Drops/Re-Seq by Customer")
L60200:     mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            seq% = 0%

            init(" ") sc_key1$                  /* Re-Sequence and     */
            str(sc_key1$,1%,5%) = scr_load$     /* Assign New Drops    */
        re_seq_nxt                              /* File (APCPLNSC)     */
            read #6,key 1% > sc_key1$,using L60290, sc_key1$, sc_cuscode$,~
                                                   eod goto re_seq_update
L60290:        FMT POS(7), CH(27), POS(59), CH(9)
            if str(sc_key1$,1%,5%) <> scr_load$ then goto L60650
               or_route$ = "00000" : or_po$ = "99999"
               sc_drop_seq$ = str(sc_key1$,6%,5%)
               sc_drop$     = str(sc_key1$,11%,2%)

               sc_so$   = str(sc_key1$,18%,8%)
               sc_line$ = str(sc_key1$,26%,2%)
               read #3,key 4% = sc_so$, using L60390, or_route$,          ~
                                          or_po$,    eod goto L60400
L60390:           FMT POS(11), CH(5), POS(36), CH(16)
L60400:        seq% = seq% + 1%
               convert seq% to seq$, pic(00000)

               str(sort_key$,1%,5%)   = scr_load$     /* Load Number   */
               str(sort_key$,6%,5%)   = sc_drop_seq$  /* Seq or Route  */
               str(sort_key$,11%,2%)  = sc_drop$      /* Customer Drop */

               str(sort_key$,13%,9%)  = sc_cuscode$   /* Customer Code */
               str(sort_key$,22%,16%) = or_po$        /* P. O. Number  */
               str(sort_key$,38%,8%)  = sc_so$        /* Sales Order   */
               str(sort_key$,46%,2%)  = sc_line$      /* S.O. Line     */
               str(sort_key$,48%,5%)  = seq$          /* Record No.    */
               str(sort_key$,53%,8%)  = " "           /* Filler        */

               if reseq% = 0% then goto L60580
                  str(sort_key$,6%,5%)   = or_route$  /* Route Code  */
                  str(sort_key$,11%,2%)  = "00"       /* Route Code  */
                  str(sort_key$,22%,16%) = "00000000" /* P.O. =N/A   */
L60580:        put #10, using L60590, sort_key$, sc_key1$, " "
L60590:            FMT CH(60), CH(27), CH(41)
               write #10, eod goto L60620
               goto re_seq_nxt
L60620:     errormsg$ = "(Error) - Re-Seq == "&sc_key1$
            gosub error_prompt
            goto re_seq_nxt
L60650: re_seq_update
            init(" ") sort_key$, sc_key1$, sav_cuscode$, sav_so$
            sc_drop_seq% = 0% : sc_drop% = 0%
        re_seq_assign
            read #10,key > sort_key$, using L60710, sort_key$, sc_key1$,  ~
                                                     eod goto re_seq_done
L60710:        FMT CH(60), CH(27)
            sc_cuscode$ = str(sort_key$,13%,9%)
            sc_so$      = str(sort_key$,38%,8%)
            sc_line$    = str(sort_key$,46%,2%)
            new_load$   = str(sort_key$,1%,5%)
            sc_load$    = new_load$
            if sav_so$ = sc_so$ then goto L60800
               sav_so$ = sc_so$
               sc_drop_seq% = sc_drop_seq% + 10%
               convert sc_drop_seq% to sc_drop_seq$, pic(00000)

L60800:     if sav_cuscode$ = sc_cuscode$ then goto L60850
               sav_cuscode$ = sc_cuscode$
               sc_drop% = sc_drop% + 1%
               convert sc_drop% to sc_drop$, pic(00)

L60850:     gosub update_header
            gosub update_lines
            goto re_seq_assign
        re_seq_done
            gosub delete_work
        return clear all
        goto inputmode

        update_header
            read #3,hold,key 4% = sc_so$,using L61030, or_rec$,           ~
                                                           eod goto L61100
L61030:        FMT CH(170)
               delete #3
            str(or_rec$,25%,2%) = sc_drop$              /* Drop Number */
            str(or_rec$,94%,5%) = new_load$             /* Load Number */
            put #3, using L61030, or_rec$
               write #3, eod goto L61100
        return
L61100:     errormsg$ = "(Error) - Updating Header (APCPLNOR) " &sc_so$
            gosub error_prompt
            init(" ") errormsg$
        return

        update_lines
            read #6,hold,key 1% = sc_key1$, using L61180, sc_rec$,        ~
                                                         eod goto L61280
L61180:        FMT CH(128)
               delete #6
            str(sc_rec$,12%,5%)  = sc_drop_seq$  /* Sched Drop Seq.    */
            str(sc_rec$,17%,2%)  = sc_drop$      /* Sched Drop No.     */
            if reseq% <> 99% then goto L61250
               str(sc_rec$,7%,5%)   = new_load$  /* Curr Assigned Load */
               str(sc_rec$,105%,5%) = sc_load$   /* Orig. Parent Load  */
L61250:     put #6, using L61180, sc_rec$
            write #6, eod goto L61290
        gosub update_dtl
L61280: return
L61290:     errormsg$ = "(Error) - Updating Line (APCPLNSC) " &sc_key1$
            gosub error_prompt
            init(" ") errormsg$
        return

        update_dtl
            init(" ") dt_key$
            str(dt_key$,1%,8%) = sc_so$
            str(dt_key$,9%,2%) = sc_line$
        update_dtl_nxt
            read #1,hold,key > dt_key$, using L61410, dt_rec$,            ~
                                                 eod goto update_dtl_done
L61410:        FMT CH(256)
            dt_key$ = str(dt_rec$,24%,23%)
            if str(dt_key$,1%,8%) <> sc_so$ then return
            if str(dt_key$,9%,2%) <> sc_line$ then return
               delete #1
               str(dt_rec$,1%,5%) = new_load$
               str(dt_rec$,6%,5%) = sc_drop_seq$
               str(dt_rec$,11%,2%)= sc_drop$
               if reseq% = 99% then gosub sort_dtl

               put #1, using L61410, dt_rec$
               write #1, eod goto L61560
               goto update_dtl_nxt
        update_dtl_done
        return
L61560:     errormsg$ = "(Error) - Updating Detail (APCPLNDT) " &dt_key$
            gosub error_prompt
            init(" ") errormsg$
        return

        sort_dtl
              init(" ") pl_key$, pl_sort$, sort_key1$, bcksubpt_rec$
              pl_sort$ = "02678L9A    "
        REM - Build Index (DT_INDEX$)
              str(pl_key$,1%,2%)  = str(dt_rec$,230%,2%)
              str(pl_key$,3%,2%)  = str(dt_rec$,232%,2%)
              str(pl_key$,5%,2%)  = str(dt_rec$,104%,2%)
              str(pl_key$,7%,3%)  = str(dt_rec$,42%,3%)
              str(pl_key$,10%,2%) = str(dt_rec$,45%,2%)
              read #7,key = pl_key$,using L61720, pl_sort$,               ~
                                                 eod goto L61740
L61720:          FMT POS(18), CH(12)

L61740:       call "APCPLN9B" ( "0",         /* Set Flag for Data File */~
                                pl_sort$,    /* Sort Code From APCPLNUC*/~
                                dt_rec$,     /* (APCPLNDT) Record      */~
				bcksubpt_rec$, /* BCKSUBPT Record      */~
                                sort_key1$,  /* Output Index Built     */~
                                #4 )         /* (GENCODES)             */
              str(dt_rec$,66%,30%) = str(sort_key1$,1%,30%)
        return
        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#10,mode$, 500%, f2%)
            if f2% <> 0% then goto L61890
        return
L61890:     errormsg$ = "(Error) Cannot Open Work File (APCRESEQ) ?? "
            gosub error_prompt
            init(" ") errormsg$
        return
        delete_work
            call "FILEBGON" (#10)
        return
        
/* + (CR2050) */
        updateRemoteOrderLine                                 
          call "APCORLNS" (sc_cuscode$, sc_so$, sc_line$, atlstatus$,       ~
                           pgmname$, #12, error%)
          error% = 0%
        return
/* - (CR2050) */        

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
