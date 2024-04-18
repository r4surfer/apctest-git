        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   RRRR   PPPP   ZZZZZ  IIIII  PPPP    *~
            *  A   A  P   P  C   C  R   R  P   P     Z     I    P   P   *~
            *  AAAAA  PPPP   C      RRRRR  PPPP     Z      I    PPPP    *~
            *  A   A  P      C   C  R  R   P       Z       I    P       *~
            *  A   A  P       CCC   R   R  P      ZZZZZ  IIIII  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCRPZIP - Special Report by Route Code, Zip Code Customer*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/18/93 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 11/12/97 ! Revision Update For 60403                ! DJD *~
            *************************************************************

        dim                                                              ~
            sel$1,                       /* Report Selection           */~
            sel_desc$30,                 /* Report Sort Description    */~
            beg_rte$15,                  /* Beginning RTE Code         */~
            end_rte$15,                  /* Ending RTE Code            */~
            beg_desc$30,                 /* Beg RTE Desc.              */~
            end_desc$30,                 /* End RTE Desc.              */~
            rte_desc$30,                 /* Route Code Description     */~
            cuscode$9,                   /* Customer Code              */~
            city$18,                     /* City                       */~
            state$2,                     /* State Code                 */~
            zip$9,                       /*                            */~
            rte$15,                      /*                            */~
            cus_name$30,                 /* Customer Name              */~
            wrk_key$40,                  /* Work Key                   */~
            wrk_rec$88,                  /* Work Record                */~
            readkey$24,                  /* Gencodes Key               */~
            desc$32,                     /* Gencodes Description       */~
            company$60,                  /* For Report Company Name    */~
            print_title$60,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(5),                      /* = 0 if the file is open    */~
            f1%(5),                      /* = 1 if READ was successful */~
            fs%(5),                      /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5)20                   /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/12/97 Route Code/Zip Code/Customer Rp"
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
            * #1  ! CUSTOMER ! Customer Master File                     *~
            * #2  ! GENCODES ! SYSTEM MASTER CODE TABLE FILES           *~
            * #3  ! APCWKZIP ! APC Work File                            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #3,  "APCRPZIP",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =  40

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),  0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03),  0%, rslt$(03))

            f1%(1), f1%(2), f1%(3), f1%(4), f1%(5) = 0%
            if fs%(3) <> 0 then call "FILEBGON" addr(#3)

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
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
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

        print_report
            gosub generate_report
        return clear all
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
         "Enter a Valid Report Selection (1), (2), or (3).             ",~
         "Enter a Valid Beginning Route Code, or (A)ll.                ",~
         "Enter a Valid Ending Route Code, or (E)nd.                   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_rte$, end_rte$,        ~
                      beg_desc$, end_desc$, rte_desc$, cuscode$, city$,  ~
                      state$, cus_name$, wrk_key$, wrk_rec$, readkey$,   ~
                      desc$, zip$, rte$, sel$, sel_desc$

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
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                         /* APCRPZIP - Work File       */
            FMT CH(15),                  /* ROUTE CODE    - KEY        */~
                CH(09),                  /* ZIP CODE      - KEY        */~
                CH(09),                  /* CUSTOMER CODE - KEY        */~
                CH(07),                  /* GROWTH                     */~
                CH(30),                  /* CUSTOMER NAME              */~
                CH(18),                  /* CITY                       */~
                CH(02),                  /* STATE                      */~
                CH(09),                  /* ZIP                        */~
                CH(29)                   /* FILLER                     */

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
              on fieldnr% gosub L40190,         /* REPORT SELECTION  */   ~
                                L40180,         /* BEG RTE CODE      */   ~
                                L40180          /* END RTE CODE      */

              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40190:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "Route Code, Zip Code Customer Report",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Report Selection    :",                      ~
               at (06,25), fac(lfac$( 1)), sel$                 , ch(01),~
               at (06,45), fac(hex(84)), sel_desc$              , ch(30),~
                                                                         ~
               at (07,02), "Beginning Routecode :",                      ~
               at (07,25), fac(lfac$( 2)), beg_rte$             , ch(15),~
               at (07,45), fac(hex(84)), beg_desc$              , ch(30),~
                                                                         ~
               at (08,02), "Ending Routecode    :",                      ~
               at (08,25), fac(lfac$( 3)), end_rte$             , ch(15),~
               at (08,45), fac(hex(84)), end_desc$              , ch(30),~
                                                                         ~
               at (10,28),                                               ~
                  "     Report Selections    ",                          ~
               at (11,28),                                               ~
                  "(1) Sort by Route Code    ",                          ~
               at (12,28),                                               ~
                  "(2) Sort by Zip Code      ",                          ~
               at (13,28),                                               ~
                  "(3) Sort by Customer Code ",                          ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40600
                  call "PRNTSCRN"
                  goto L40210

L40600:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40790     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40750
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40750:     if fieldnr% > 1% then L40770
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40770:     return

L40790: if fieldnr% > 0% then L40880  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40880:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50140,         /* Report Selection      */ ~
                              L50270,         /* Beginning Routecode   */ ~
                              L50450          /* Ending Route Code     */

            return

L50140: REM Report Selection                      SEL$, SEL_DESC$
          if sel$ <> " " then goto L50180
             sel$ = "1"

L50180:   if sel$ = "1" then sel_desc$="(1) Report in Route Code Seq."
          if sel$ = "2" then sel_desc$="(2) Report in Zip Code Seq."
          if sel$ = "3" then sel_desc$="(3) Report in Customer Code Seq."
          if sel$ <> "1" and sel$ <> "2" and sel$ <> "2" then goto L50230
        return
L50230:     errormsg$ = "(Error) - Invalid Report Selection?"
            sel$, sel_desc$ = " "
        return

L50270: REM Beginning Routecode                   BEG_RTE$
            beg% = 0%
            if beg_rte$ <> " " then goto L50310
               goto L50320
L50310:     if str(beg_rte$,1%,1%) <> "A" then goto L50350
L50320:        beg_rte$ = "ALL"
               beg_desc$ = "(A)ll Route Codes."
               return
L50350:     gosub lookup_route
            if rte% = 0% then goto L50410
               beg_desc$ = desc$
               convert beg_rte$ to beg%, data goto L50410

        return
L50410:     beg_rte$, beg_desc$ = " "
            errormsg$ = "(Error) - Invalid Beginning Route Code?"
        return

L50450: REM Ending Route Code                     END_RTE$
            end% = 0%
            if end_rte$ <> " " then goto L50490
               goto L50500
L50490:     if str(end_rte$,1%,1%) <> "E" then goto L50530
L50500:        end_rte$ = "END"
               end_desc$ = "(E)nd of Route Codes."
               return
L50530:     gosub lookup_route
            if rte% = 0% then goto L50590
               end_desc$ = desc$
               convert end_rte$ to end%, data goto L50590

        return
L50590:     end_rte$, end_desc$ = " "
            errormsg$ = "(Error) - Invalid Ending Route Code?"
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!######## ########                  ############################~
        ~################################                APCRPZIP: !

L55080: %!User Id: ###                       ############################~
        ~################################              Page: ##### !

L55110: %!Beginning Routecode : ############### ####################     ~
        ~   Ending Routecode: ############### #################### !

L55140: %!                                                    ###########~
        ~##############                                            !

L55170: %!---------------------------------------------------------------~
        ~----------------------------------------------------------!

                                                   /* COLUMN 1 HEADER */
L55210: %!<-- Route Code -->! Zip Code  ! Customer Code !<-- Customer  Na~
        ~me --------------!<--- City --------->! State ! Zip Code  !

L55240: %!------------------!-----------!---------------!----------------~
        ~-----------------!--------------------!-------!-----------!

                                                   /* DETAIL 1      */
L55280: %! ###############  ! ######### !   #########   ! ###############~
        ~################ ! ################## !  ##   ! ######### !

L55310: %+---------------------------------------------------------------~
        ~----------------------------------------------------------+

        print_header
          if lcnt% <> 99% then print using L55310
          rpt_time$ = " "
          call "TIME" (rpt_time$)
          page_no% = page_no% + 1%
          print page
          print using L55310
          print using L55050, date$, rpt_time$, company$
          print using L55080, userid$, print_title$, page_no%
          print using L55110, beg_rte$, str(beg_desc$,1%,20%),            ~
                             end_rte$, str(end_desc$,1%,20%)
          print using L55140, sel_desc$
          print using L55170
          print using L55210
          lcnt% = 7%
        return

        print_detail
          if lcnt% > 58% then gosub print_header
          print using L55240
          print using L55280, rte$, zip$, cuscode$, cus_name$, city$,     ~
                             state$, zip$
          lcnt% = lcnt% + 2%
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            print_title$ = "Routecode/Zip Code Customer Report"
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCZIP", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCLDS", " ", 0%, 1%)
        return

        generate_report
            call "SHOSTAT" ("Scanning Customer Master")
            call "OPENCHCK" (#3, fs%(3), f2%(3), 800%, rslt$(3))
            cnt% = 0%
            cuscode$ = all(hex(00))
            read #1,key > cuscode$, using L60330, cuscode$, cus_name$,    ~
                                    city$, state$, zip$, rte$,           ~
                                                   eod goto generate_done
            goto L60380
        generate_next
            read #1, using L60330, cuscode$, cus_name$, city$, state$,    ~
                                  zip$, rte$, eod goto generate_done

L60330:        FMT CH(9), CH(30), POS(403), CH(18), CH(2), XX(1),        ~
                   CH(9), POS(980), CH(5)
            cnt% = cnt% + 1%
            print at(05,35);hex(84);"[";cnt%;"]"

L60380:     gosub update_work
            goto generate_next
        generate_done
            cnt% = 0%
            call "SHOSTAT" ("Routecode/Zip Report")
            gosub select_printer
            wrk_key$ = all(hex(00))
            read #3,key > wrk_key$, using L60500, wrk_key$, wrk_rec$,     ~
                                                       eod goto work_done
            goto L60550
        work_next
            read #3, using L60500, wrk_key$, wrk_rec$, eod goto work_done
L60500:        FMT CH(40), CH(88)

L60550:     rte$      = str(wrk_rec$,1%,5%)
            zip$      = str(wrk_rec$,16%,9%)
            cuscode$  = str(wrk_rec$,25%,9%)
            cus_name$ = str(wrk_rec$,34%,30%)
            city$     = str(wrk_rec$,64%,18%)
            state$    = str(wrk_rec$,82%,2%)

            if str(rte$,1%,1%)= " " then rte$ = "  Undefined    "
            if str(zip$,1%,1%)= " " then zip$ = "Undefined"
            gosub print_detail
            goto work_next
        work_done
            print using L55310
            gosub close_printer
            call "FILEBGON" addr(#3)
        return

        lookup_route
            rte% = 0%
            readkey$  = "ROUTECODE" & beg_rte$
            call "DESCRIBE" (#2, readkey$, desc$, 0%, f1%(2%))
            if f1%(2%) = 0% then return
            rte% = 1%
        return

        update_work
           x% = 0%
           if str(beg_rte$,1%,1%) = "A" then goto L60870
              convert rte$ to x%, data goto L60840
L60840:
              if x% = 0% then goto L60930
              if x% < beg% then goto L61240
L60870:    if str(end_rte$,1%,1%) = "E" then goto L60930
              convert rte$ to x%, data goto L60890
L60890:
              if x% = 0% then goto L60930
              if x% > end% then goto L61240

L60930:    wrk_key$ = all(hex(00))
           if sel$ <> "1" then goto L61000             /* ROUTE CODE SEQ*/
              str(wrk_key$,1%,15%) = rte$
              str(wrk_key$,16%,9%) = zip$
              str(wrk_key$,25%,9%) = cuscode$
              str(wrk_key$,34%,7%) = " "
              goto L61120
L61000:    if sel$ <> "2" then goto L61070             /* ZIP CODE SEQ  */
              str(wrk_key$,1%,9%)   = zip$
              str(wrk_key$,10%,15%) = rte$
              str(wrk_key$,25%,9%)  = cuscode$
              str(wrk_key$,34%,7%)  = " "
              goto L61120
                                                      /* CUSTOMER CODE */
L61070:    str(wrk_key$,1%,9%)   = cuscode$
           str(wrk_key$,10%,15%) = rte$
           str(wrk_key$,25%,9%)  = zip$
           str(wrk_key$,34%,7%)  = " "

L61120:    wrk_rec$ = " "
           str(wrk_rec$,1%,15%)  = rte$
           str(wrk_rec$,16%,9%)  = zip$
           str(wrk_rec$,25%,9%)  = cuscode$
           str(wrk_rec$,34%,30%) = cus_name$
           str(wrk_rec$,64%,18%) = city$
           str(wrk_rec$,82%,2%)  = state$
           str(wrk_rec$,84%,4%)  = " "

          put #3, using L61220, wrk_key$, wrk_rec$
L61220:      FMT CH(40), CH(88)
          write #3, eod goto L61250
L61240: return
L61250:   stop "(ERROR) - WRITING ---->  " & wrk_key$
          close ws
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
