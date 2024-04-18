        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCRGA30  (Like APCRGA10)            *~
            *  Creation Date     - 01/21/97                             *~
            *  Last Modified Date- 01/11/06                             *~
            *  Description       - This Program Creates the RGA         *~
            *                      Labels by RGA No.                    *~
            *                                                           *~
            *  Special Notes     - Uses NEW Planning Files              *~
            *                    - Compliment program to APCRGA22,      *~
            *                      which prints barcodes for a specified*~
            *                      RGA or range of RGA's with a status  *~
            *                      of "11".                             *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *  Subroutines - (APCRG22B) - The RGA Barcode label print   *~
            *                             routine.                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/21/97 ! New Program for (APC) - Last Mod Date    ! JBF *~
            *          !                                          !     *~
            * 10/31/97 ! Changed Program Version ID To 60403      ! DJD *~
            * 06/15/01 ! Mod to allow alpa in rga number (EWD001) ! CMG *~
            * 01/11/06 ! (PAR000) CR347 mods for sub part         ! CMG *~
            *          !                                          !     *~
            *************************************************************
        dim                                                              ~
            rga_hd_rec$80,               /* RGA Header File Record     */~
/*PAR000*/  rga_dt_rec$(2%)256,          /* RGA Detail File Record     */~
/*PAR000*/  sav_rec$(2%)256,             /* RGA Detail File Save Area  */~
            r_number$6,                  /* RGA No./Item No.           */~
            rga_number$4,                /* RGA No.                    */~
            rga_item$2,                  /* RGA Item No.               */~
            rga_part$25,                 /* RGA Item Part No.          */~
            rga_so$8,                    /* RGA Sales Order No.        */~
            rga_line$2,                  /* RGA Sales Order Line       */~
            rga_cuscode$9,               /* RGA Customer No.           */~
            dtlkey$6,                    /* APCRGADT Read Key          */~
            sc_key$10,                   /* APCPLNSC Read Key          */~
            sc_txt$4,                    /* APCPLNSC Line Text ID      */~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Size Long Form             */~
            sav_number$4,                /* Save RGA No.               */~
            cus_desc$30,                 /* Generic Description        */~
            c_desc$30,                   /* Generic Description        */~
            part_desc$45,                /* Part Description           */~
            text_desc$60,                /* Line Item Text             */~
            text_key$11,                 /* Text File Key              */~
            sav_key1$11,                 /* Text File Save Key         */~
            atext$(2)70,                 /* Text (2) Lines             */~
            dt_flag$1,                   /* Line Item Text as Part Desc*/~
            title$40,                    /* Report Title Field         */~
            beg_nbr$4,                   /* Begin  RGA Number          */~
            end_nbr$4,                   /* Ending RGA Number          */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */~

        dim f2%(11%),                    /* = 0 if the file is open    */~
            f1%(11%),                    /* = 1 if READ was successful */~
            fs%(11%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(11%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 10/31/97 Reprint RGA Barcode Labels   "

        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCRGADT ! RGA Header File                          *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #4  ! APCRGAHD ! RGA Header File                          *~
            * #8  ! APCPLNSC ! Planning Master Scheduling File          *~
            * #9  ! HNYMASTR ! Inventory Master File                    *~
            * #10 ! TXTFILE  ! Text File                                *~
            * #11 ! AMTBOMIF ! Inventory Validity Check                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "APCRGADT",                                      ~
/*PAR000*/              varc,     indexed,  recsize =  512,              ~
                        keypos =   12, keylen =    6,                    ~
                        alt key  1, keypos =  10, keylen =   8,          ~
                            key  2, keypos =   1, keylen =  17

            select #2,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =    1, keylen =    9,                    ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #4,  "APCRGAHD",                                      ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =   12, keylen =    4,                    ~
                        alt key  1, keypos =  10, keylen =   6,          ~
                            key  2, keypos =   1, keylen =  15

            select #8,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #9,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #10, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =   11

            select #11, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%( 1%), f2%( 1%), 0%, rslt$( 1%))
            call "OPENCHCK" (#2,  fs%( 2%), f2%( 2%), 0%, rslt$( 2%))
            call "OPENCHCK" (#4,  fs%( 4%), f2%( 4%), 0%, rslt$( 4%))
            call "OPENCHCK" (#8,  fs%( 8%), f2%( 8%), 0%, rslt$( 8%))
            call "OPENCHCK" (#9,  fs%( 9%), f2%( 9%), 0%, rslt$( 9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            date$ = date
            call "DATEFMT"      (date$)
            call "EXTRACT" addr ("ID", userid$)

            str(line2$,62%) = "APCRGA30: " & str(cms2v$,,8%)
            edtmessage$ = "To Modify Displayed Values, Position Cursor "&~
                          "to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            gosub initialize_variables

            for fieldnr%              =  1% to  2%
L10090:         gosub'051(fieldnr%)        /* Default / Enables */
                     if enabled%      =  0% then L10260

L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%       =  1% then gosub startover
                     if keyhit%      <>  4% then L10220

L10160:              fieldnr%         = max(1%, fieldnr% - 1%)
                     gosub'051(fieldnr%)
                          if enabled% =  1% then L10120
                          if fieldnr% =  1% then L10090
                          goto L10160

L10220:              if  keyhit%      = 16%                              ~
                     and fieldnr%     =  1% then exit_program
                     if keyhit%      <>  0% then L10120

L10260:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                     if errormsg$    <> " " then L10120

            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                if keyhit%            =  1% then gosub startover
                if keyhit%            = 14% then gosub dataput
                if keyhit%            = 16% then exit_program
                if keyhit%           <>  0% then editpg1

L11130:     fieldnr%                  = cursor%(1%) - 6%
            if fieldnr%               <  1%                              ~
            or fieldnr%               >  2% then editpg1
            if fieldnr% = lastfieldnr%      then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled%           =  0% then editpg1

L11200:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                if keyhit%            =  1% then gosub startover
                if keyhit%           <>  0% then L11200

            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                if errormsg$         <> " " then L11200
                lastfieldnr%          = fieldnr%

            goto L11130

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
            if fieldnr% <> 0% then L28100
            inpmessage$ =  edtmessage$
        return

L28100
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
                read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Beginning RGA Number ?                                 ",~
         "Enter Ending RGA Number ?                                    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_nbr$, end_nbr$,        ~
                rga_number$, r_number$, rga_item$, dtlkey$,              ~
                sav_number$, rga_cuscode$, cus_desc$, rga_part$, rga_so$,~
                part_desc$, rga_line$
            rec% = 0%
        return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************
        startover
            u3%    = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "SHOSTAT" ("Creating Barcode Labels")
            gosub select_log_report

            beg_nbr% = 0%                                   /* (EWD001) */
            convert str(beg_nbr$,2%,3%) to beg_nbr%, data goto process_done

            convert beg_nbr% to str(beg_nbr$,2%,3%), pic(000)


            str(dtlkey$,1%,4%) = beg_nbr$
            read #1,key > dtlkey$, using L31120, rga_dt_rec$(),             ~
                eod goto process_done              /* (PAR000) */
L31120:         FMT 2*CH(256)

            goto process_detail

        process_next
/* (PAR000) */
            dtlkey$        =  str(sav_rec$(),12%,6%)
            read #1,key > dtlkey$, using L31120, rga_dt_rec$(),             ~
                eod goto process_done              /* (PAR000) */

        process_detail
/* (PAR000) */
            if str(rga_dt_rec$(),12%,4%) > end_nbr$ then process_done
            if str(rga_dt_rec$(),10%,2%) < "12"     then process_label
                sav_rec$() = rga_dt_rec$()
                goto process_next

        process_label
            rga_cuscode$   =  str(rga_dt_rec$(), 1%, 9%)
            rga_number$    =  str(rga_dt_rec$(),12%, 4%)
            r_number$      =  str(rga_dt_rec$(),12%, 6%)
            rga_item$      =  str(rga_dt_rec$(),16%, 2%)
            rga_part$      =  str(rga_dt_rec$(),28%,25%)
            rga_so$        =  str(rga_dt_rec$(),53%, 8%)
            rga_line$      =  str(rga_dt_rec$(),61%, 2%)
            gosub lookup_customer
            gosub lookup_part
            gosub update_detail
            if sav_number$ <> rga_number$      then gosub update_header
            gosub print_details

            call "APCRG22B"  (r_number$,                                 ~
                              c_desc$,                                   ~
                              rga_cuscode$,                                ~
                              part_desc$)
            rec%           =  1%
            sav_rec$()     =  rga_dt_rec$()                 /* (PAR000) */
            goto process_next

        process_done
            r_number$      = "E O F "
            call "APCRG22B"  (r_number$, " ", " ", " ")

            if rec% = 1%                      then gosub update_header
            print using L55120
            print using L55040
            print using L55130
        return clear all
        goto exit_program

        update_header
            read #4,key = sav_number$, using L31630, rga_hd_rec$,         ~
                eod goto L31740
L31630:         FMT CH(80)

            read   #4,hold,key = sav_number$, eod goto L31740

            delete #4

            str(rga_hd_rec$,10%,2%) = "02"
            put    #4, using L31630, rga_hd_rec$

            write  #4, eod goto L31740

L31740: return

        update_detail
            str(dtlkey$,1%,4%)      = rga_number$
            str(dtlkey$,5%,2%)      = rga_item$
            read #1,hold,key = dtlkey$, eod goto L31880

            delete #1
                                                    /* (PAR000) */
            str(rga_dt_rec$(),10%,2%) = "11"
            put    #1, using L31120, rga_dt_rec$()

            write  #1, eod goto L31880

L31880: return

        select_log_report
            title$  = "** APCRGA30 - RGA Barcode Reprint Log **"
            pageno% = 0%
            lcnt%   = 99%
            call "TIME"    (xtime$)
            call "SETPRNT" (" ", "RG30", 0%, 0%)

            select printer(134)
        return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1

            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub L40175,         /* Start RGA Nbr.    */     ~
                              L40175          /* End   RGA Nbr.    */
            goto L40190

/* EWD001 */    lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40175:         lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:         lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
                at (01,02),                                              ~
                     "APC Building Products  -  Reprint RGA Barcodes",   ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)),   date$               , ch(08),~
                at (02,02), fac(hex(8c)),   line2$              , ch(79),~
                at (03,02), fac(hex(94)),   errormsg$           , ch(79),~
                                                                         ~
                at (07,02), "Beginning RGA Number:",                     ~
                at (07,26), fac(lfac$(1%)), beg_nbr$            , ch(04),~
                                                                         ~
                at (08,02), "Ending    RGA Number:",                     ~
                at (08,26), fac(lfac$(2%)), end_nbr$            , ch(04),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

                if keyhit% <> 15% then L40430
                     call "PRNTSCRN" : goto L40190

L40430:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

        return

        set_pf1
        if edit% = 2% then L40640     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)

            if fieldnr% = 1% then L40600
                str(pf$(3%),64%) = " "     : str(pfkeys$,16%,1%) = hex(ff)
L40600:     if fieldnr% > 1% then L40620
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%)  = hex(ff)
L40620: return

L40640: if fieldnr% > 0% then L40740  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Print Labels"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
        return

L40740:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************
        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50110,                /* Beg. RGA Nbr.   */~
                              L50260                 /* End  RGA Nbr.   */
        return

L50110: REM - Beginning RGA Number
            if beg_nbr$ = " "              then L50220
                                                      /* (EWD001) */
            convert str(beg_nbr$,2%,3%) to beg_nbr%,  data goto L50220

            convert beg_nbr% to str(beg_nbr$,2%,3%),  pic(000)

            read #4,key = beg_nbr$, using L50190, rga_hd_rec$,            ~
                eod goto L50220
L50190:         FMT CH(80)

        return
L50220:     errormsg$ = "(Error) - Invalid Beginning RGA Number ?"
            init(" ") beg_nbr$
        return

L50260: REM - Ending RGA Number
            if end_nbr$ <> " "             then L50300
                end_nbr$ = beg_nbr$
                goto L50380
L50300:     convert end_nbr$ to end_nbr%,  data goto L50390

            convert end_nbr% to end_nbr$,  pic(0000)

            if end_nbr$ < beg_nbr$         then L50390
            read #4,key = end_nbr$, using L50190, rga_hd_rec$,            ~
                eod goto L50390

L50380: return
L50390:     errormsg$ = "(Error) - Invalid Ending RGA Number ?"
            init(" ") end_nbr$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                    /* RGA Log Report Columns          */
L55040: %                                                                ~

                                    /* Header Format                   */
L55060: %  ######## @ ########   ########################################~
        ~   Page: ###
L55080: %  RGA    Item    Customer    Customer Name
L55090: %+ ---- ! ---- ! --------- ! -------------------------------- +
                                    /* Detail Format                   */
L55110: %! #### !  ##  ! ######### ! ################################ !
L55120: %!======!======!===========!==================================!
L55130: %       ********   RGA Barcode Log Finished   *******

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
        print_details
            if lcnt% > 55% then gosub print_header

            if sav_number$ =  " "         then L60110
            if sav_number$ <> rga_number$ then L60100
                init(" ") rga_number$, rga_cuscode$, cus_desc$, rga_item$
                goto L60120
L60100:     print using L55120
L60110:     sav_number$ = rga_number$
L60120:     print using L55110, rga_number$, rga_item$, rga_cuscode$,     ~
                cus_desc$
            lcnt% = lcnt% + 1%
        return

        print_header
            pageno% = pageno% + 1%
            print page
            print using L55060, date$, xtime$, title$, pageno%
            print using L55040
            print using L55080
            print using L55090
            lcnt% = 5%
        return

        lookup_customer
            read #2,key = rga_cuscode$, using L60300, cus_desc$,          ~
                eod goto L60340
L60300:         FMT POS(10), CH(30)

            c_desc$     = cus_desc$
        return
L60340:     init(" ") cus_desc$, c_desc$
        return

        lookup_part                           /* Check HNYMASTR        */
            init(" ") part_desc$, apc_prt$, apc_sze$, apc_scr$, dt_flag$
            read #9,key = rga_part$, using L60410, part_desc$, apc_prt$,  ~
                apc_sze$, eod goto L60440
L60410:         FMT XX(25), CH(32), POS(606), CH(60), CH(20)

            goto L60580
L60440:     err% = 0%
            if len(rga_part$) > 18% then L60530
                part_desc$ = "COMPONENT PART"
                gosub lookup_apcplnsc
                gosub lookup_text

                if dt_flag$ = "Y"   then part_desc$ = text_desc$
                goto L60580

L60530:     call "APCDESCR" (rga_part$, apc_scr$, apc_prt$, apc_sze$,    ~
                             #11, err%)

            str(part_desc$,1%,16%)  = str(apc_prt$,1%,16%)
            str(part_desc$,17%,16%) = str(apc_sze$,1%,16%)
L60580: return

        lookup_apcplnsc
            sc_txt$             = " "
            sc_key$             = all(hex(20))
            str(sc_key$, 1%,8%) = rga_so$
            convert rga_line$ to xx%, data goto L60650
L60650:
            convert xx%       to str(sc_key$,9%,2%), pic(##)

            read #8,key = sc_key$, using L60690, sc_txt$, eod goto L60710
L60690:         FMT POS(100), CH(04)

L60710: return

        lookup_text                           /* Look Up Text Id       */
            init(" ") text_desc$, textid$, text_key$, sav_key1$, atext$()
            textid$ = sc_txt$
            gosub'099(textid$)

            if txt% = 0% then L60950
            text_key$            = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = textid$
            str(text_key$,9%,1%) = "1"
            sav_key1$            = text_key$
            read #10,key > text_key$, eod goto L60950

            get  #10, using L60890, text_key$, atext$()
L60890:         FMT CH(11), POS(64), 2*CH(70)

            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then L60950
            if atext$(1)  <> " " then text_desc$ = str(atext$(1),1%,60%) ~
                                 else text_desc$ = str(atext$(2),1%,60%)
            if text_desc$ <> " " then dt_flag$   = "Y"
L60950: return

        deffn'099(txt$)
            txt% = 0%
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then L61020
            txt% = 1%
L61020: return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *************************************************************
        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
