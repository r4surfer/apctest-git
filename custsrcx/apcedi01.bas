        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   EEEEE  DDDD   IIIII   000     1     *~
            *  A   A  P   P  C   C  E      D   D    I    0   0   11     *~
            *  AAAAA  PPPP   C      EEEE   D   D    I    0   0    1     *~
            *  A   A  P      C   C  E      D   D    I    0   0    1     *~
            *  A   A  P       CCC   EEEEE  DDDD   IIIII   000   11111   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCEDI01 - EDI Trading Partner Maintenance                *~
            *    - EDI Partner DUN'S Number Code                  (15)  *~
            *      - EDI Partner's Store Number (Internal Number) (06)  *~
            *        - APC Store (Customer Number)                (09)  *~
            *          - Filler Area                              (02)  *~
            *                                                           *~
            * Note- The file (APCEDIRF) contains the Trading partner    *~
            *       Cross-Reference to the APC Customer Code. All Stores*~
            *       Associated with a Trading Partner Need to be Entered*~
            *       into this DATABASE. The Trading Partner Code is     *~
            *       Designated in the 'PARTNERS ' Table in GENCODES.    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/04/93 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 10/31/97 ! Changed Version ID To Reflect 060403     ! DJD *~
            *          !                                          !     *~
            * 01/12/98 ! Y2K Changes                              ! DJD *~
            *************************************************************

        dim                                                              ~
            hdr$40,                      /* ASKUSER                    */~
            msg$(3)79,                   /* ASKUSER                    */~
            readkey$24,                  /* GENCODES Key               */~
            partners$32,                 /* GENCODES Description       */~
            edi_key$21,                  /* EDI Primary Key            */~
            edi_code$3,                  /* Gencodes Key               */~
            edi_dun$15,                  /* Partners Trading Id        */~
            edi_store$6,                 /* Partners Store Id          */~
            edi_cust$9,                  /* APC Customer Number        */~
            edi_desc$30,                 /* APC Customer Description   */~
            edi_filler$2,                /* Filler Area                */~
            edi_rec$32,                  /* EDI Record                 */~
            edi_city$18,                 /* Store City                 */~
            edi_state$2,                 /* Store State Code           */~
            edi_zip$9,                   /* Store Zip Code             */~
            company$60,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
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

        dim f2%( 5),                     /* = 0 if the file is open    */~
            f1%( 5),                     /* = 1 if READ was successful */~
            fs%( 5),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$( 5)20                  /* Text from file opening     */

/* <<<<<<<<<< Y2K >>>>>>>>>> */
	dim blankdate$6			 /* Empty Date For Compares    */
/* <<<<<<<<<< Y2K >>>>>>>>>> */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 10/31/97 EWD EDI Trading Partner Maint. "
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
            * #01 ! CUSTOMER ! Master Customer File                     *~
            * #02 ! GENCODES ! System Master Code Tables                *~
            * #03 ! APCEDIRF ! EDI Customer Cross Reference             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =  1,   keylen =  9,                      ~
                        alt key  1, keypos  =    10, keylen = 30, dup,   ~
                            key  2, keypos  =   424, keylen =  9, dup,   ~
                            key  3, keypos  =   771, keylen =  9, dup,   ~
                            key  4, keypos  =   780, keylen =  9, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,  "APCEDIRF",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =    1, keylen =  21,                     ~
                        alt key  1, keypos  = 22, keylen = 9

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),  0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03),500%, rslt$(03))

            f1%(1), f1%(2), f1%(3) = 0%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
/* <<<<<<<<<< Y2K >>>>>>>>>> */
            call "DATUFMTC" (blankdate$)	
/* <<<<<<<<<< Y2K >>>>>>>>>> */

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
            inc% = 1%
        inputmode_a
            for fieldnr% = inc% to  3%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10240
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 14% then gosub print_report
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 12% then gosub delete_store
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
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
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            gosub select_printer
            edi_key$ = all(hex(00))
            str(edi_key$,1%,15%) = edi_dun$
            read #3,key > edi_key$, using L19160, edi_key$, edi_cust$,    ~
                                         edi_filler$, eod goto print_done
            goto L19170
        next_part
            read #3, using L19160, edi_key$, edi_cust$, edi_filler$,      ~
                                                      eod goto print_done
L19160:       FMT CH(21), CH(9), CH(2)
L19170:     if edi_dun$ <> str(edi_key$,1%,15%) then goto print_done
               edi_store$ = str(edi_key$,16%,6%)
               gosub L50480
            gosub print_detail
            goto next_part
        print_done
            print using L55110
            gosub close_printer
        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20190,         /* Trading Partner Code  */ ~
                              L20230,         /* Trading Partner Store */ ~
                              L20270          /* APC Customer Code     */

         return

L20190: REM Trading Partner Code                   EDI_CODE$,EDI_DUN$
        REM EDI_CODE$, EDI_DUN$ = " "
         return

L20230: REM Trading Store Code                     EDI_STORE$
        REM EDI_STORE$ = " "
         return

L20270: REM APC Customer Code                      EDI_CUST$
        REM EDI_CUST$ = " "
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
         "Enter a Valid Trading Partner Code. ( 001 to 999 )           ",~
         "Enter a Valid Trading Store Code.                            ",~
         "Enter a Valid EWD Customer Code. (Assoc. with Trading Store) "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, partners$,       ~
                      edi_key$, edi_code$, edi_dun$, edi_store$,         ~
                      edi_cust$, edi_desc$, edi_rec$, edi_city$,         ~
                      edi_state$, edi_zip$
            edi% = 0%
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
        dataload
          rec% = 0%
          str(edi_key$,1%,15%) = edi_dun$
          str(edi_key$,16%,6%) = edi_store$
          read #3,key = edi_key$, using L35040, edi_dun$, edi_store$,     ~
                                   edi_cust$, edi_filler$, eod goto L30130
          rec% = 1%                                         /* ON FILE */
          gosub L50480                             /* GET CUSTOMER INFO */
L30130: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************

        dataput
          str(edi_key$,1%,15%) = edi_dun$
          str(edi_key$,16%,6%) = edi_store$
          read #3,hold,key = edi_key$, eod goto L31110
            delete #3
L31110:   put #3, using L35040, edi_dun$, edi_store$, edi_cust$,          ~
                                                              edi_filler$
          write #3, eod goto L31210

L31150:   inc% = 2%
          init(" ") edi_store$, edi_cust$, edi_desc$, edi_city$,         ~
                    edi_state$, edi_zip$
        return clear all
        goto inputmode_a

L31210:   stop "(ERROR) UNABLE TO UPDATE (APCEDIRF) FOR - "& edi_key$
        goto L31150

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                       /* APCEDIRF - Partner Cross-Ref */
L35040:     FMT CH(15),                /* DUN'S Number                 */~
                CH(06),                /* Partner Store Number         */~
                CH(09),                /* APC Customer Number          */~
                CH(02)                 /* Filler Area                  */

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
              on fieldnr% gosub L40180,         /* Partners Code     */   ~
                                L40180,         /* Partner Customer  */   ~
                                L40180          /* APC Customer Code */

              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "EDI Partners Cross Reference Utility Program",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,28), fac(hex(84)), partners$              , ch(30),~
                                                                         ~
               at (06,02), "Partner Code :",                             ~
               at (06,20), fac(lfac$( 1)), edi_code$            , ch(03),~
               at (06,30), "Trading Id: ",                               ~
               at (06,42), fac(hex(84)),   edi_dun$             , ch(15),~
               at (07,02), "Store Number :",                             ~
               at (07,20), fac(lfac$( 2)), edi_store$           , ch(06),~
               at (08,02), "Customer Code:",                             ~
               at (08,20), fac(lfac$( 3)), edi_cust$            , ch(09),~
               at (08,30), fac(hex(84)), edi_desc$              , ch(30),~
               at (09,30), fac(hex(84)), edi_city$              , ch(18),~
               at (09,50), fac(hex(84)), edi_state$             , ch(02),~
               at (09,54), fac(hex(84)), edi_zip$               , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40540
                  call "PRNTSCRN"
                  goto L40210

L40540:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40740     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40690
                str(pf$(3),64%)   = " "  :  str(pfkeys$,16%,1%) = hex(ff)
L40690:     if fieldnr% > 1% then L40720
                str(pf$(1),64%)   = " "  :  str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4%,1%) = hex(ff)
L40720:     return

L40740: if fieldnr% > 0% then L40850  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                 (12)Delete Store       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffff0cff0e0f1000)
            if rec% = 1% then return
               str(pf$(2),18%,30%) = " " : str(pfkeys$,12%,1%)=hex(ff)
            return
L40850:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50140,         /* Partner Code          */ ~
                              L50310,         /* Store Number          */ ~
                              L50480          /* APC Customer Code     */

            return

L50140: REM EDI Partner Code                      EDI_CODE$
           edi% = 0%
           if edi_code$ <> " " then goto L50210
L50170:       errormsg$ = "(Error) - Partner Code is Required"
              init(" ") edi_code$, partners$, edi_dun$
              edi% = 0%
              return
L50210:    convert edi_code$ to edi%, data goto L50170

           convert edi% to edi_code$, pic(000)
           init(" ") readkey$
           readkey$ = "PARTNERS " & edi_code$
           read #2,key = readkey$, using L50270, partners$, eod goto L50170
L50270:       FMT POS(25), CH(32)
           edi_dun$ = str(partners$,1%,15%)
        return

L50310: REM Store Number                          EDI_STORE$
           edi_store% = 0%
           if edi_store$ <> " " then goto L50380
L50340:       errormsg$ = "(Error) Partner Store Number Required."
              init(" ") edi_store$
              edi_store% = 0%
              return
L50380:    convert edi_store$ to edi_store%, data goto L50340

           convert edi_store% to edi_store$, pic(000000)

           gosub dataload
           if rec% = 0% then return
              if edit% <> 1% then return
              fieldnr% = 3%
        return

L50480: REM Customer Code                         EDI_CUST$
           if edi_cust$ <> " " then goto L50530
L50500:       errormsg$ = "(Error) EWD Customer Code Required."
              init(" ") edi_cust$, edi_desc$
              return
L50530:    read #1,key = edi_cust$, eod goto L50500
           get #1, using L50550, edi_desc$, edi_city$,edi_state$, edi_zip$
L50550:       FMT POS(10), CH(30), POS(403), CH(18), CH(02), XX(01),CH(9)
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!######## ######## ########################################     ~
        ~     APCEDIRF:!

L55080: %!USER ID: ###           ##############################          ~
        ~   PAGE: #####!

L55110: %+---------------------------------------------------------------~
        ~--------------+

L55140: %!---------------------------------------------------------------~
        ~--------------!

                                                   /* Column Header   */
L55180: %!Store No!Customer Id!<---- DESCRIPTION -----> <----- City -----~
        ~><St><- Zip ->!

L55210: %!--------!-----------!------------------------------------------~
        ~--------------!

                                                   /* Detail Data   */
L55250: %! ###### ! ######### !######################## #################~
        ~# ## #########!

        print_header
          if lcnt% <> 99% then print using L55110
          page_no% = page_no% + 1%
          print page
          print using L55110
          print using L55050, date$, rpt_time$, print_title$
          print using L55080, userid$, partners$, page_no%
          print using L55140
          print using L55180
          lcnt% = 5%
        return

        print_detail
          if lcnt% > 58% then gosub print_header
          print using L55210
          print using L55250, edi_store$, edi_cust$,str(edi_desc$,1%,24%),~
                                         edi_city$, edi_state$, edi_zip$
          lcnt% = lcnt% + 2%
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            call "SHOSTAT" ("Creating EDI Cross-Ref Report")
            page_no% = 0%
            lcnt%    = 99%
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCEDI", " ", 0%, 0%)
            print_title$ = "EWD Trading Partner Cross-Ref"
            call "FMTTITLE" (print_title$, " ", 12%)
            call "FMTTITLE" (partners$, " ", 12%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCEDI", " ", 0%, 1%)
        return

        delete_store
          gosub prompt_user
          if comp% = 0% then goto L60300                /* Exit Process */
             call "SHOSTAT" ("Deleting Store ( " &edi_store$& " )")
             str(edi_key$,1%,15%) = edi_dun$
             str(edi_key$,16%,6%) = edi_store$
             read #3,hold,key = edi_key$, eod goto L60300
               delete #3
L60300: return clear all
        goto inputmode

        prompt_user
            comp% = 2%
            hdr$ = "**EDI Cross-Ref Maintenance**"
            msg$(1) = " **********  Do You Wish to Continue  ********** "
            msg$(2) = "Press <RETURN> To 'Exit Process', or Press Any   "
            msg$(3) = "(PF) Key To Continue...........                  "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
