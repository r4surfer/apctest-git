        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   RRRR   PPPP   TTTTT    1     000    *~
            *  A   A  P   P  C   C  R   R  P   P    T     11    0   0   *~
            *  AAAAA  PPPP   C      RRRRR  PPPP     T      1    0   0   *~
            *  A   A  P      C   C  R  R   P        T      1    0   0   *~
            *  A   A  P       CCC   R   R  P        T    11111   000    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCRPT10 - Print list of Manufactured Parts with BOM'S    *~
            *            or All BOM'S.                                  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/02/91 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 11/12/97 ! Revision Update For 60403                ! DJD *~
            * 03/30/98 ! Y2K modifications                        ! ERN *~
            *************************************************************

        dim                                                              ~
            type$1,                      /* Report Type                */~
            type_desc$30,                /* Report Type Description    */~
            partno1$25,                  /* Beginning Part Number      */~
            partno2$25,                  /* Ending Part Number         */~
            hdr$(3)79,                   /* Select BOM Header          */~
            header$100,                  /* Select Bom Header          */~
            readkey$100,                 /* Generic Key                */~
            bom_rec$150,                 /* BOM Record                 */~
            bom_key$31,                  /* BOM Primary Key            */~
            part$25,                     /*                            */~
            bomid$3,                     /*                            */~
            descr$32,                    /*                            */~
            rteid$3,                     /*                            */~
            dte$10,                      /*                            */~
            company$40,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(10),                     /* = 0 if the file is open    */~
            f1%(10),                     /* = 1 if READ was successful */~
            fs%(10),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/12/97 Pre-Release Version            "
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
            * #01 ! BOMMASTR ! BOM Master File                          *~
            * #02 ! HNYMASTR ! Parts Master File                        *~
            * #03 !          ! Load MASTER FILE                         *~
            * #04 ! GENCODES ! System Master Code Table Files           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "BOMMASTR",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos =   26, keylen =   31,                    ~
                        alt key  1, keypos =    1, keylen =  56

            select #02, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #04, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),  0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))
            call "OPENCHCK" (#04, fs%(04), f2%(04),  0%, rslt$(04))

            f1%(1), f1%(2), f1%(3), f1%(4), f1%(5) = 0%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
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
            on fieldnr% gosub L20130,         /* Report Type         */   ~
                              L20170,         /* Beginning Part No   */   ~
                              L20210          /* Ending Part No.     */
         return

L20130: REM Report Type                            TYPE$
        REM TYPE$ = " "
         return

L20170: REM Beginning Part Number                  PARTNO1$
        REM PARTNO1$ = " "
         return

L20210: REM Ending Part Number                     PARTNO2$
        REM PARTNO2$ = " "
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
         "Enter a Report Type (M)anufactured,(S)ubassembly,(C)omponent.",~
         "Enter a Valid Beginning Part Number, or (ALL).               ",~
         "Enter a Valid Ending Part Number, or (ALL).                  "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, type$, type_desc$,         ~
                      partno1$, partno2$, header$, hdr$(), bom_rec$,     ~
                      bom_key$, readkey$

        return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
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
              on fieldnr% gosub L40170,         /* Report Type       */   ~
                                L40180,         /* Part Range BEG    */   ~
                                L40180          /* Part Range END    */
              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40180:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Bill of Material's (BOM's) Part Listing Report",      ~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Report Listing Type  :",                     ~
               at (06,25), fac(lfac$( 1)), type$                , ch(01),~
               at (06,40), fac(hex(84)), type_desc$             , ch(30),~
                                                                         ~
               at (07,02), "Beginning Part Number:",                     ~
               at (07,25), fac(lfac$( 2)), partno1$             , ch(25),~
                                                                         ~
               at (08,02), "Ending Part Number  :",                      ~
               at (08,25), fac(lfac$( 3)), partno2$             , ch(25),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40490
                  call "PRNTSCRN"
                  goto L40200

L40490:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40680     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40640
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40640:     if fieldnr% > 1% then L40660
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40660:     return

L40680: if fieldnr% > 0% then L40770  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40770:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50130,         /* Report Type           */ ~
                              L50270,         /* Beginning Part        */ ~
                              L50390          /* Ending Part           */
            return

L50130: REM Report Type                           TYPE$,TYPE_DESC$
            if type$ <> " " then goto L50160
               type$ = "M"
L50160:     p% = pos("MSC" = type$)
            if p% = 0% then goto L50230

            if p% = 1% then type_desc$ = "Manufactured Parts (>19)"
            if p% = 2% then type_desc$ = "Sub Assembly Parts (=15)"
            if p% = 3% then type_desc$ = "Component Parts    (<15)"
        return
L50230:     errormsg$ = "(Error) Invalid Report Type. (M,S,C) "
            type$, type_desc$ = " "
        return

L50270: REM Beginning                              PARTNO1$
            if str(partno1$,1%,3%) = "ALL" then goto L50350
            if partno1$ <> " " then return
               gosub select_bom
               if partno1$ <> " " then return
               errormsg$ = "(Error) Invalid Beginning Part Number."
               partno1$ = " "
        return
L50350:     partno1$, partno2$ = "ALL"
            fieldnr% = 3%
        return

L50390: REM Ending                                 PARTNO2$
            if partno2$ = "ALL" then goto L50350
            if partno2$ <> " " then return
               gosub select_bom
               if partno1$ <= partno2$ then return
               errormsg$ = "(Error) Invalid Ending Part Number."
               partno2$ = " "
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* REPORT HEADER */
L55050: %!########## ########   ######################################## ~
        ~       APCRPT10:!

L55080: %!User Id: ###        ########################################   ~
        ~     Page: #####!
                                                   /* COLUMN 1 HEADER */
L55110: %!<------ Part Number ---->!<------ Part Description ------>!B-ID~
        ~!R-ID! Last Mod !

L55130: %!-------------------------!--------------------------------!----~
        ~!----!----------!

L55160: %!#########################!################################! ###~
        ~! ###!########33!
L55180: %+---------------------------------------------------------------~
        ~----------------+

L55210: %!---------------------------------------------------------------~
        ~----------------!

L55240: %! Total Model(###): ##### !                                !    ~
        ~!    !          !

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            page_no% = 0%
            lcnt%    = 99%

            print_title$ = type_desc$
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "FMTTITLE" (company$, " ", 12%)
            call "SETPRNT" ("APCRPT", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCRPT", " ", 0%, 1%)
        return

        generate_report
            call "SHOSTAT" ("Creating BOM Report")
            gosub select_printer
            init(" ") sav_model$
            bom_key$ = all(hex(00))
            if partno1$ <> "ALL" then str(bom_key$,1%,25%) = partno1$
        generate_next
            read #1,key > bom_key$, using L60320, bom_key$,               ~
                                    rteid$, dte%, eod goto generate_done

L60320:        FMT POS(26), CH(31), XX(30), CH(3), POS(100), BI(3)

            if str(bom_key$,29%,3%) = "  0" then goto L60370
               str(bom_key$,29%,3%) = "zzz"
               goto generate_next
L60370:     part$  = str(bom_key$,1%,25%)

            if partno1$ = "ALL" then goto L60410
            if part$ > partno2$ then goto generate_done
L60410:        if p% = 1% and len(part$) < 19% then goto L60470
               if p% = 2% and len(part$) <> 15% then goto L60470
               if p% = 3% and len(part$) > 14% then goto L60470
                  bomid$ = str(bom_key$,26%,3%)
                  call "DESCRIBE" (#2, part$, descr$, 0%, f1%(2))
                  convert dte% to dte$, pic(########)
                  call "DATFMTC" (dte$)
                  gosub print_detail
L60470:           str(bom_key$,29%,3%) = "zzz"
                  goto generate_next
        generate_done
            gosub print_total
            print using L55180
            gosub close_printer
        return

        print_header
          page_no% = page_no% + 1%
          if lcnt% <> 99% then print using L55180
          print page
          print using L55180
          print using L55050, date$, rpt_time$, company$
          print using L55080, userid$, print_title$, page_no%
          print using L55210
          print using L55110
          lcnt% = 5%
        return

        print_detail
          if sav_model$ = " " then  sav_model$ = str(part$,1%,3%)
          if sav_model$ <> str(part$,1%,3%) then gosub print_total

          if lcnt% > 58% then gosub print_header
          print using L55130
          print using L55160, part$, descr$, bomid$, rteid$, dte$
          lcnt% = lcnt% + 2%
          total% = total% + 1%
        return

        print_total
            convert total% to total$, pic(#####)
            print using L55130
            print using L55240, sav_model$, total$
            lcnt% = lcnt% + 2%
            total% = 0%
            sav_model$ = str(part$,1%,3%)
        return

        select_bom
            readkey$ = " "
            hdr$(1), hdr$(3) = "  "
            hdr$(2) = "  Assembly Number        BOMID"  &                ~
                      "   BOM Description"
            header$ = hex(06) & "Select the Bill of Materials to" &      ~
                           " Edit.  Use PF-16 to Cancel Selection"

            call "PLOWCODE" (#1, readkey$, header$, -1028%, -.30, f1%(1),~
                                           hdr$())
            if f1%(1) = 0% then return
               if fieldnr% = 2% then partno1$ = str(readkey$,1%,25%)
               if fieldnr% = 3% then partno2$ = str(readkey$,1%,25%)
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
            *-----------------------------------------------------------*~

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
