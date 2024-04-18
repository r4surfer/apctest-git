        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCEDI09                             *~
            *  Creation Date     - 03/10/95                             *~
            *  Last Modified Date- 10/31/97                             *~
            *  Description       - This Program Reports on PO Comments  *~
            *                      Received From Lowes and HQ.          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/10/95 ! New Program for (APC) - Last Mod Date    ! RMT *~
            * 03/16/95 ! Put in Routine to Purge Old Records      ! RMT *~
            * 10/24/97 ! Correct Problems with Report and Purge   ! RHH *~
            *          ! Utility. New process for PURGE_DATA      !     *~
            *          !                                          !     *~
            * 10/31/97 ! Changed version description for 60403    ! DJD *~
            *          !                                          !     *~
            * 06/16/97 ! Y2K for report only; disabled purge      ! ERN *~
            *************************************************************

        dim                                                              ~
            readkey$24,                  /* Gencodes Key and Descript  */~
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
            scr$(10%)40, msg$28

        dim f2%(5%),                     /* = 0 if the file is open    */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim xtime$9,                     /* Report Run Time            */~
            title$40,                    /* Report Title               */~
            ent_trpart$3,ent_trpart_d$30,/* Trading Partn. Entered     */~
            purge_key$33, purge_msg$35,  /* Primary Purge Key          */~
            purge_rec$256,               /* (APCEDIMC) Record          */~
            purge_date$6, purge_dte$10,  /* Purge cutoff date          */~
            ent_date1$10,                /* Begin. PO Date Range       */~
            ent_date2$10,                /* Ending PO Date Range       */~
            sav_date1$10,                /* Begin. Date - Unformatted  */~
            sav_date2$10,                /* Ending Date - Unformatted  */~
            po_date$8,                   /* PO Date                    */~
            trpart_code$3,               /* EDI Trading Partn. Code    */~
            edi_store$6,                 /* Store (customer) Number    */~
            po_num$16,                   /* PO Number                  */~
            comment$79,                  /* PO Comment                 */~
            copy_key$8                   /* Copy Lookup Key            */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 10/31/97 EDI PO Comment Utility          "
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
            * #1  ! APCEDICM ! EDI PO Comment File                      *~
            * #2  ! GENCODES ! System Code Table File                   *~
            * #3  ! APCEDIMC ! EDI Master Control File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCEDICM",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  33,                     ~
                        alt key 1, keypos =  1, keylen = 06, dup,        ~
                            key 2, keypos =  7, keylen = 03, dup,        ~
                            key 3, keypos = 10, keylen = 06, dup,        ~
                            key 4, keypos = 16, keylen = 16, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #3,  "APCEDIMC",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key 1, keypos = 10, keylen = 13, dup,        ~
                            key 2, keypos =233, keylen =  6, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 100%, rslt$(3%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            keep_days% = -45%
            call "DATE" addr("G+", date$, keep_days%, purge_date$, ret%)
            if ret% = 0% then L09140
               call "SHOSTAT" ("Problem Calculating Purge Date") : stop
               goto exit_program
L09140:     call "DATFMTC" (date$)
            purge_dte$ = purge_date$
            call "DATFMTC" (purge_dte$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "APCEDI09: " & str(cms2v$,,8)

            scr$( 1%) = "****************************************"
            scr$( 2%) = "*  Code       Trading Partner          *"
            scr$( 3%) = "* ------     -----------------         *"
            scr$( 4%) = "*   001        Home Quarters           *"
            scr$( 5%) = "*   002        Lowes                   *"
            scr$( 6%) = "*  Blank       ALL Trading Partners    *"
            scr$( 7%) = "****************************************"

        REM *************************************************************~
            *       I N P U T   M O D E   R E P O R T                    ~
            *************************************************************

        inp_rpt
            gosub initialize_variables
            for fieldnr% = 1% to  3%
L10140:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10380
L10180:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10340
L10240:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10180
                         if fieldnr% = 1% then L10140
                         goto L10240
L10340:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10180
L10380:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10180
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
                  if keyhit%  = 12% then gosub purge_data
                  if keyhit%  = 14% then gosub gen_rpt
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11240:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% >  3% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11340:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11340
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11340
                  lastfieldnr% = fieldnr%
            goto L11240

        REM *************************************************************~
            *             P r o c e s s   D a t a                       *~
            *************************************************************

        gen_rpt
            gosub select_printer
            gosub generate_report
       rem  gosub purge_data
            close printer
        return clear all
        goto inp_rpt

        select_printer
            title$ = " * * * EDI Purchase Order Comments * * *"
            pageno% = 0%
            lcnt%   = 99%
            date$ = date
            call "DATFMTC" (date$)
            call "TIME" (xtime$)
            call "SETPRNT" (" ","COMM",0%,0%)
            select printer(134)
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
            if fieldnr% <> 0% then L28220
                inpmessage$ = edtmessage$
                return

L28220
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter P.O. Beginning Date or (ALL)? MM/DD/YYYY Format        ",~
         "Enter P.O. Ending Date or (Return)? MM/DD/YYYY Format        ",~
         "Enter Trading Partner I.D. (Lowes-001, HQ-002, Etc.)?        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, po_date$, trpart_code$,    ~
                      copy_key$, title$, xtime$, comment$, ent_trpart$,  ~
                      readkey$, edi_store$, po_num$, ent_date1$,         ~
                      ent_date2$, sav_date1$, sav_date2$, ent_trpart_d$
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
            goto inp_rpt

        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40170,         /* PO Bef Date Rnge   */  ~
                                L40170,         /* PO Aft Date Rnge   */  ~
                                L40170          /* Trad. Partner Code */
              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "(EWD) EDI Purchase Order Comment Report             ",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,26), fac(hex(84)), purge_msg$             , ch(35),~
               at (05,02), "(EWD) P.O. Begin Date ? :",                  ~
               at (05,30), fac(lfac$(1%)), ent_date1$           , ch(10),~
                                                                         ~
               at (06,02), "(EWD) P.O. Ending Date? :",                  ~
               at (06,30), fac(lfac$(2%)), ent_date2$           , ch(10),~
                                                                         ~
               at (07,02), "(EWD) Trading Partner ? :",                  ~
               at (07,30), fac(lfac$(3%)), ent_trpart$          , ch(03),~
               at (07,40), fac(hex(84)),   ent_trpart_d$        , ch(30),~
                                                                         ~
               at (10,21), fac(hex(84)),   scr$(1%)             , ch(40),~
               at (11,21), fac(hex(84)),   scr$(2%)             , ch(40),~
               at (12,21), fac(hex(84)),   scr$(3%)             , ch(40),~
               at (13,21), fac(hex(84)),   scr$(4%)             , ch(40),~
               at (14,21), fac(hex(84)),   scr$(5%)             , ch(40),~
               at (15,21), fac(hex(84)),   scr$(6%)             , ch(40),~
               at (16,21), fac(hex(84)),   scr$(7%)             , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40570
                  call "PRNTSCRN" : goto L40070

L40570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf1
            purge_msg$ = "Purge Data Prior to ["& purge_dte$ &"]"

        if edit% = 2% then L40780     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40740
               str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40740:     if fieldnr% > 2% then L40760
               str(pf$(2%),18%,20%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40760:     return

L40780: if fieldnr% > 0% then L40890  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "(12)Purge Data         (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffff0cff0e0f1000)
               str(pf$(1%),34%,24%) = " " : str(pfkeys$,12%,1%) = hex(ff)
            if userid$ = "RHH" or userid$ = "DJD" then return
               str(pf$(1%),34%,24%) = " " : str(pfkeys$,12%,1%) = hex(ff)
            return
L40890:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50130,         /* First Date             */~
                              L50260,         /* Second Date            */~
                              L50440          /* Trading Partn. Code    */
            return

L50130: REM Test for First PO Date                ENT_DATE1$
            if ent_date1$ <> " " and ent_date1$ <> blankdate$ then goto L50170
               ent_date1$ = date

L50170:     call "DATEOKC" (ent_date1$, date%, errormsg$)
            if date% = 0% then goto L50220
               sav_date1$ = ent_date1$
               call "DATUFMTC" (sav_date1$)
        return
L50220:     errormsg$ = "Invalid Beginning Date. Please Re-enter"
            init(" ") ent_date1$, sav_date1$
        return

L50260: REM Test for Second PO Date               ENT_DATE2$
            if ent_date2$ <> " " and ent_date2$ <> blankdate$ then goto L50300
               ent_date2$ = ent_date1$

L50300:     call "DATEOKC" (ent_date2$, date%, errormsg$)
            if date% = 0% then goto L50370
               sav_date2$ = ent_date2$
               call "DATUFMTC" (sav_date2$)
        REM Test for Valid Date Range
               if sav_date1$ > sav_date2$ then goto L50400
        return
L50370:     errormsg$ = "Invalid Ending Date. Please Re-enter"
            init(" ") ent_date2$, sav_date2$
        return
L50400:     errormsg$ = "Invalid Date Range. Please Re-enter"
            init(" ") ent_date1$, ent_date2$, sav_date1$, sav_date2$
        return

L50440: REM Test for Trading Partner Code        TRPART_CODE$
            if ent_trpart$ <> " " then goto L50490
L50460:        ent_trpart$   = "ALL"
               ent_trpart_d$ = "(All) Trading Partners"
               return
L50490:     if str(ent_trpart$,1%,1%) = "A" then goto L50460
               str(readkey$,1%,9%)   = "PARTNERS1"
               str(readkey$,10%,15%) = ent_trpart$

            read #2,key = readkey$, using L50550, ent_trpart_d$,          ~
                                                 eod goto L50570
L50550:        FMT POS(25), CH(30)
        return
L50570:     errormsg$ = "Invalid Trading Partner Code?"

            init(" ") ent_trpart$, ent_trpart_d$
        return

        REM *************************************************************~
            *      R e p o r t   F o r m a t   S t a t e m e n t s      *~
            *************************************************************

L55080: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
L55120: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
                                                      /* Header Format */
L55180: %!                    ########################################  #~

L55220: %!######### @ ########                                           ~
        ~                                                      Page: ### !
                                                      /* Detail Header */
L55280: %! PO Date !Partn!  Store  !   P.O. Number   !<------------------~
        ~--------------  Comment  -------------------------------------->!

L55340: %!######## ! ### !  ###### ! ################! ##################~
        ~################################################################!

L55400: %!---------!-----!---------!-----------------!-------------------~
        ~----------------------------------------------------------------!

        REM *************************************************************~
            *            S p e c i a l   S u b r o u t i n e s          *~
            *************************************************************
        purge_data
            call "SHOSTAT" ("Purging Data")

            msg$ = "Comment Rec's Purged xxxxxxx" : cnt% = 0%
            init(" ") purge_key$

        purge_next
            read #1,hold,key > purge_key$, using L60110, purge_rec$,      ~
                                                        eod goto L60220
L60110:        FMT CH(33)
            purge_key$ = purge_rec$
            if str(purge_rec$,1%,6%) > purge_date$ then goto L60220
               cnt% = cnt% + 1%
               if mod(cnt%,50%) <> 0 then goto L60190
                  convert cnt% to str(msg$,22%,7%), pic(#######)
                  call "SHOSTAT" (msg$)

L60190:        delete #1
               goto purge_next

L60220:     init(" ") purge_key$
            msg$ = "EDI Rec's Purged xxxxxxx" : cnt% = 0%
        purge_next1
            read #3,hold,key > purge_key$, using L60270, purge_rec$,      ~
                                                      eod goto purge_done
L60270:        FMT CH(256)
            str(purge_key$,1%,22%) = str(purge_rec$,1%,22%)
            if str(purge_rec$,233%,6%) > purge_date$ then                ~
                                                     goto purge_next1
               cnt% = cnt% + 1%
               if mod(cnt%,50%) <> 0 then goto L60360
                  convert cnt% to str(msg$,18%,7%), pic(#######)
                  call "SHOSTAT" (msg$)

L60360:        delete #3
               goto purge_next1
        purge_done
        return clear all
        goto inp_rpt
        prt_header
            if lcnt% <> 99% then print using L55080
            pageno% = pageno% + 1%
            print page
            print using L55080
            print using L55180, title$
            print using L55220, date$, xtime$, pageno%
            print using L55120
            print using L55280
            lcnt% = 5%
        return

        print_detail
            if lcnt% > 57% then gosub prt_header
            print using L55400
               print using L55340, po_date$, trpart_code$,                ~
                  store_num$, po_num$, comment$
               goto L60600

L60600:     lcnt% = lcnt% + 1%
        return

        generate_report
            call "SHOSTAT" ("Creating Comment Report")
        generate_next
            read #1, using L60680, po_date$, trpart_code$,          ~
               store_num$, po_num$, comment$, eod goto generate_done
L60680:        FMT CH(6), CH(3), CH(6), CH(16), XX(2), CH(79)
            if po_date$ < str(sav_date1$,1%,6%) or po_date$ > str(sav_date2$,1%,6%) then       ~
               goto generate_next
            call "DATEFMT" (po_date$)
            if ent_trpart$ = "ALL" then goto L60750
               if ent_trpart$ <> trpart_code$ then goto generate_next

L60750:     gosub print_detail
            goto generate_next
        generate_done
            print using L55080

        return clear all
    goto exit_program   
        goto inp_rpt

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

        end
