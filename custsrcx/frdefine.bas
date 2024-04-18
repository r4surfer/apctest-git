        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF  RRRR   DDDD   EEEEE  FFFFF  IIIII  N   N  EEEEE   *~
            *  F      R   R  D   D  E      F        I    NN  N  E       *~
            *  FFFF   RRRR   D   D  EEE    FFF      I    N N N  EEE     *~
            *  F      R  R   D   D  E      F        I    N  NN  E       *~
            *  F      R   R  DDDD   EEEEE  F      IIIII  N   N  EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FRDEFINE - Manage G/L report specs, used by the Financial *~
            *            Statement print programs.  This is NOT intended*~
            *            to work like a true "Report Generator". The    *~
            *            actual report formats are essentialy FIXED.    *~
            *            The accounts & totaling to be shown on the     *~
            *            report is what floats; this program is used to *~
            *            define that Structure. Note that a report spec *~
            *            created here can be used with any of the report*~
            *            print programs, though results may be ilogical *~
            *            if, say, a Balance Sheet is printed using a    *~
            *            spec that was intended to be ran with a P & L  *~
            *            type report print program.                     *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/13/85 ! ORIGINAL (Re-write)                      ! HES *~
            * 10/12/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/12/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 05/04/89 ! Chg'd FAC of Header1$, added test & CALL ! MLJ *~
            *          ! to GLVALD2 for Local Authority Accts.    !     *~
	    * 03/01/01 ! Changed all arrays from 320 to 500.      ! CMG *~            
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**~

        dim                                                              ~
            acct1$(500)16,               /* FROM ACCOUNT               */~
            acct2$(500)16,               /* TO ACCOUNT                 */~
            acctthru$(500)1,             /* THRU INDICATOR             */~
            base$(500)3,                 /* FROM ACCOUNT               */~
            col$(500)1,                  /* COLUMN INDICATOR           */~
            comp$(500)3,                 /* COMPANY (DBASE POINTER)    */~
            compdescr$30,                /* COMPANY DESCRIPTION        */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr$30,                    /* REPORT DESCRIPTION         */~
            display$(3)79,               /* LINE ITMES FOR EDIT DISPLAY*/~
            dual_books$1,                /* Dual books in effect?      */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filler$11,                   /* FILLER AT END OF RECORD    */~
            fixed$(500)3,                /* RUN TIME OVERRIDE FLAG     */~
            group$(500)6,                /* ACCOUNT GROUPING CODE      */~
            groupdescr$32,               /* ACCOUNT GROUPING CODE DESCR*/~
            header$79,                   /* HEADER FOR SCREEN          */~
            header1$79,                  /* HEADER FOR SCREEN          */~
            hfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inst$(500)2,                 /* INSTRUCTION CODE           */~
            inst_codes$(21)2,            /* VALID INSTRUCTIONS         */~
            inst_names$(21)30,           /* VALID INSTRUCTIONS NAMES   */~
            instdescr$30,                /* FOR DISPLAY                */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line$(500)4,                 /* LINE NUMBER FOR REFERENCE  */~
            message$79,                  /* INPUT MESSAGE              */~
            memo$(500)70,                /* JUNK TEXT                  */~
            paren$3,                     /* DEFAUT PARENTHESIS OPTION  */~
            pfdescr$(3)79,               /* DESCRIPTION OF PFKEYS      */~
            print$(500)3,                /* PRINT CODE                 */~
            print_if$(500)1,             /* PRINT/TOTAL CONSTRAINT FLAG*/~
            range$(14)25,                /* FOR SUMMARY DISPLAY SCREEN */~
            readkey$60,                  /* WORK VARIABLE              */~
            report$3,                    /* REPORT ID.                 */~
            reverse$(500)3,              /* REVERSE SIGN INDICATOR     */~
            round$3,                     /* ROUND OPTION (DEFUALT)     */~
            set$1, setdescr$30,          /* Set of books to use        */~
            setmsg$26,                   /* Screen message for SET     */~
            text$(500)45,                /* TEXT FOR LINE ITEMS        */~
            title$50,                    /* DEFAULT REPORT TITLE       */~
            total$(500)10,               /* TOTALING SPEC FOR LINE     */~
            totaltext$16,                /* SCREEN PROMPT FOR 'TYPE' FI*/~
            type$(500)2,                 /* TEXT FOR LINE ITEMS        */~
            zero$3,                      /* PRINT IF ZERO? (DEFUALT)   */~
            zone$(4,2)2,                 /* START, LENGTH IN ACCOUNT   */~
            zonename$(4)20               /* LABLE FOR ZONE IN ACCOUNT  */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            fs%(64),                     /* FILE STATUS FLAGS          */~
            rslt$(64)20                  /* TEXT FROM FILE OPENING     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! FRNAMES  ! Financial Statement Header File          *~
            * #02 ! FRFORMAT ! Financial Statement Format File          *~
            * #03 ! FRGRPMAS ! G/L Grouping Codes File (Statutory)      *~
            * #04 ! FRGRPMA2 ! G/L Grouping Codes File (Local Authority)*~
            * #10 ! SYSFILE2 ! SYSTEM INFORMATION                       *~
            *************************************************************

            select #01,  "FRNAMES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1, keylen = 3

            select #02,  "FRFORMAT",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1, keylen = 8

            select #03,  "FRGRPMAS",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 6,                          ~
                        alt key 1, keypos = 7, keylen = 30, dup /*DESCR*/

            select #04,  "FRGRPMA2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 6,                          ~
                        alt key 1, keypos = 7, keylen = 30, dup /*DESCR*/

            select  #10, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            call "SHOSTAT" ("Opening data files, one moment please.")

            call "OPENCHCK" (#01, fs%( 1), f2%( 1), 100%, rslt$( 1))
            call "OPENCHCK" (#02, fs%( 2), f2%( 2), 100%, rslt$( 2))
            call "OPENCHCK" (#03, fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (#10, fs%(10), f2%(10),   0%, rslt$(10))
            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#10, "SWITCHS.GL", f1%(10))
                if f1%(10) = 0% then goto L09000
            get #10 using L02550, dual_books$
L02550:         FMT POS(21), CH(1)
            if dual_books$ <> "Y" then goto L09000
                call "OPENCHCK" (#04, fs%( 4), f2%( 4),   0%, rslt$( 4))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            if dual_books$ = "Y"                                         ~
                then setmsg$ = "G/L System to use (1 or 2)"
            date$ = date
            call "DATEFMT" (date$)

            header$ = "     Inst     Accounts/Misc Info     Print? Text (~
        ~if applicable)     %Base Line"

            for i% = 1 to 500
                convert i% to line$(i%), pic(###)
                str(line$(i%),4) = ")"
            next i%

            REM List of valid instructions codes & descriptions
            inst_codes$(01) = "H "
            inst_codes$(02) = "P "
            inst_codes$(03) = "D "
            inst_codes$(04) = "DC"
            inst_codes$(05) = "U "
            inst_codes$(06) = "DU"
            inst_codes$(07) = "PA"
            inst_codes$(08) = "CB"
            inst_codes$(09) = "PO"
            inst_codes$(10) = "YO"
            inst_codes$(11) = "YA"
            inst_codes$(12) = "T1"
            inst_codes$(13) = "T2"
            inst_codes$(14) = "T3"
            inst_codes$(15) = "T4"
            inst_codes$(16) = "T5"
            inst_codes$(17) = "T6"
            inst_codes$(18) = "T7"
            inst_codes$(19) = "T8"
            inst_codes$(20) = "T9"
            inst_codes$(21) = "T0"

            inst_names$(01) = "Set Header Message            "
            inst_names$(02) = "New Page; Print Header        "
            inst_names$(03) = "Print Sub Title               "
            inst_names$(04) = "Print Sub Title Centered      "
            inst_names$(05) = "Print Underlines              "
            inst_names$(06) = "Print Double Underlines       "
            inst_names$(07) = "Period Activity               "
            inst_names$(08) = "Current Balance               "
            inst_names$(09) = "Period's Opening Balance      "
            inst_names$(10) = "Year's Opening Balance        "
            inst_names$(11) = "Current Year Activity         "
            inst_names$(12) = "Manage Total Bucket 1         "
            inst_names$(13) = "Manage Total Bucket 2         "
            inst_names$(14) = "Manage Total Bucket 3         "
            inst_names$(15) = "Manage Total Bucket 4         "
            inst_names$(16) = "Manage Total Bucket 5         "
            inst_names$(17) = "Manage Total Bucket 6         "
            inst_names$(18) = "Manage Total Bucket 7         "
            inst_names$(19) = "Manage Total Bucket 8         "
            inst_names$(20) = "Manage Total Bucket 9         "
            inst_names$(21) = "Manage Total Bucket 0         "

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            gosub L29000  /* Clear Variables For Input */
            maxlines% = 0

            for fieldnr% = 1% to 10%
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10260
L10130:         gosub'101(1%, fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  9 then print_data
                      if keyhit%  = 10 then copy_report
                      if keyhit% <>  4 then L10230
L10180:                   if fieldnr% < 2  then L10130
L10190:                   fieldnr%=fieldnr%-1
                          if fieldnr%=3% and dual_books$<>"Y" then L10190
                          gosub'061(fieldnr%)
                          if enabled% <> 0 then L10130
                          goto L10180
L10230:               if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit%  = 32 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10130
L10260:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
                next fieldnr%
                if maxlines%>0% then summary

*        Get Line Item Data...
L10320:     if maxlines%=500% then summary
            c%=maxlines%+1%
            gosub inputlines
            if keyhit% <> 16% then L10380
                gosub columnone
                goto summary
L10380:     maxlines%=maxlines%+1%
            goto L10320

        inputlines
            instdescr$, groupdescr$, compdescr$ = " "
            for fieldnr% = 1 to 13
                gosub'052(fieldnr%)
                      if enabled% = 0 then L10630
L10460:         gosub'102(3%, fieldnr%)
                      if keyhit% = 16 and fieldnr% = 1% then return
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 14 then gosub show_legend
                      if keyhit% <>  2 then       L10530
                         gosub columnone
                         goto inputlines
L10530:               if keyhit% <>  6 then       L10560
                         gosub prevline
                         goto L10460    /* COULD BE DIRECT TO NEXT  */
L10560:               if keyhit% <>  4 then       L10620
L10570:                  if fieldnr% < 2 then L10460
                         fieldnr% = fieldnr% - 1
                         gosub'052(fieldnr%)
                         if enabled% <> 0 then L10460
                         goto L10570
L10620:               if keyhit% <>  0 then       L10460
L10630:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10460
                next fieldnr%
                return

        columnone
                inst$(c%), comp$(c%), acct1$(c%), acct2$(c%), group$(c%),~
                print$(c%), col$(c%), reverse$(c%), text$(c%), base$(c%),~
                fixed$(c%), memo$(c%), total$(c%), print_if$(c%),        ~
                type$(c%) = " "
            return

        prevline
            if c%=1% then return
                  on fieldnr% goto  L11240,         /* INSTRUCTION      */~
                                         ,         /* TOTAL TYPE       */~
                                    L11260,         /* COMPANY CODE     */~
                                    L11270,         /* ACCOUNTS         */~
                                    L11300,         /* ZONE OVERRIDE    */~
                                    L11310,         /* PRINT?           */~
                                    L11320,         /* PRINT RESTRICTION*/~
                                    L11330,         /* PRINT COLUMN     */~
                                    L11340,         /* REVERSE SIGNS    */~
                                    L11350,         /* TEXT FOR PRINT   */~
                                    L11360,         /* % BASE LINE      */~
                                    L11370,         /* MEMO TEXT        */~
                                    L11380          /* TOTALING FLAGS   */
                  return

L11240:         inst$     (c%) = inst$     (c%-1):return
L11260:         comp$     (c%) = comp$     (c%-1):return
L11270:         group$    (c%) = group$    (c%-1)
                acct1$    (c%) = acct1$    (c%-1)
                acct2$    (c%) = acct2$    (c%-1):return
L11300:         fixed$    (c%) = fixed$    (c%-1):return
L11310:         print$    (c%) = print$    (c%-1):return
L11320:         print_if$ (c%) = print_if$ (c%-1):return
L11330:         col$      (c%) = col$      (c%-1):return
L11340:         reverse$  (c%) = reverse$  (c%-1):return
L11350:         text$     (c%) = text$     (c%-1):return
L11360:         base$     (c%) = base$     (c%-1):return
L11370:         memo$     (c%) = memo$     (c%-1):return
L11380:         total$    (c%) = total$    (c%-1):return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
            b% = 0
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press (ENTER)."
L12100:     gosub'101(2%, 0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       summary
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L12100
            fieldnr% = cursor%(1) - 5
            if fieldnr% <  2 or fieldnr% > 14 then L12100
            if fieldnr% =  3 and dual_books$ <> "Y" then L12100
            if fieldnr% =  4 or fieldnr% =  5 then L12100
            if fieldnr% = 10 or fieldnr% = 11 then L12100
            if fieldnr% > 11 then fieldnr% = fieldnr% - 2
            if fieldnr% >  5 then fieldnr% = fieldnr% - 2

            gosub'061(fieldnr%)
L12230:     gosub'101(7%, fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12230
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L12230
            goto editmode

        summary              /* SUMMARY SCREEN */
            errormsg$ = " "
            insert% = 0
            message$="To Modify Line Item, Tab To Line And Press (ENTER)"

L12350:     gosub'110(0%)
              if keyhit% = 1 then gosub startover
              if keyhit% = 2 then b%=0%
              if keyhit% = 3 then b%=max(0%, maxlines%-12)
              if keyhit% = 4 then b%=max(0%, b%-12)
              if keyhit% = 5 then b%=max(0%, min(b%+12,maxlines%-12))
              if keyhit% = 6 then b%=max(0%, b%-1)
              if keyhit% = 7 then b%=max(0%, min(b%+1,maxlines%-1))
              if keyhit% = 9 then       editmode
              if keyhit% =12 then       L12490
              if keyhit% = 8 then       L12490
              if keyhit% =14 then gosub show_legend
              if keyhit% =16 then       datasave

L12490:       fieldnr% = cursor%(1) - 6
              if fieldnr% < 1 or fieldnr% > 14 then L12350
              if hfac$(fieldnr%) = hex(bc) then L12350
              c%=min(b%+fieldnr%,maxlines%)
              if keyhit%=11 then insertmode
              if c%=0% then L12350
              fieldnr%=c%-b%

              if keyhit%=12 then deletemode
              if keyhit%<>0 then L12350

        edtpg2
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press (ENTER)."
            REM Next Line Will Draw Out The Description For Lines Code(s)
            gosub'152(3%)
L12650:     errormsg$=" "
            gosub'102(4%, 0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then c%=1%
                  if keyhit%  =  3 then c%=max(1%, maxlines%)
                  if keyhit%  =  4 then c%=max(1%,c%-10)
                  if keyhit%  =  5 then c%=max(1%,min(c%+10,maxlines%))
                  if keyhit%  =  6 then c%=max(1%,c%-1%)
                  if keyhit%  =  7 then c%=min(maxlines%,c%+1%)
                  if keyhit%  =  9 then editmode
                  if keyhit% =  14 then gosub show_legend
                  if keyhit%  = 16 then summary
                  if keyhit% <>  0 then edtpg2
            fieldnr% = cursor%(1) - 7
            if fieldnr% < 1 or fieldnr% > 12 then L12650
            if cursor%(1) >  8 then fieldnr% = fieldnr% + 1
            if cursor%(1) > 12 then fieldnr% = fieldnr% + 1
            if cursor%(1) > 13 then fieldnr% = fieldnr% + 1
            if cursor%(1) = 12 and cursor%(2) > 32 then fieldnr% = 7
            if cursor%(1) = 13 and cursor%(2) > 32 then fieldnr% = 9
            if fieldnr% = 13 then L12650
            if fieldnr% = 14 or fieldnr% = 15 then fieldnr% = 13
            if fieldnr% = 1 and cursor%(2) > 59 then fieldnr% = 2

            gosub'062(fieldnr%)
                if enabled% = 0 then edtpg2
L12910:     gosub'102(7%, fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12910
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L12910
            goto edtpg2


        REM *************************************************************~
            *       I N S E R T   &   D E L E T E   L O G I C           *~
            *                                                           *~
            * INSERT & DELETE CODE RESIDES HERE.                        *~
            *************************************************************

        insertmode: insert% = 1
            if maxlines%=500% then summary

            REM Copy all Elements Up One...
            maxlines%=maxlines%+1%
            c% = c% + 1
            if c%=maxlines% then L13200
                roll% = -1
                for temp% = maxlines% to c% step -1
                     gosub roll_lines
                next temp%
                convert c%-1 to temp$, pic(###)
                gosub roll_bases

L13200:     REM Get Line Item Data...
            gosub columnone
            gosub inputlines
            if keyhit% = 16% then delete_line
        goto insertmode

        deletemode
            message$ = "To DELETE Flashing Line, press ENTER.  To Return ~
        ~without Delete, press PF1."
            gosub'112(fieldnr%)
                  if keyhit%=1 then summary
                  if keyhit%<>0 then deletemode

        delete_line
            if c%=maxlines% then L13410
                roll% = 1
                for temp% = c% to maxlines%-1%
                     gosub roll_lines
                next temp%
                convert c% to temp$, pic(###)
                gosub roll_bases
L13410:     c%=maxlines%
            gosub columnone
            maxlines%=maxlines%-1%
            if b% > maxlines% then b% = max(0, maxlines%-1)
            goto summary

        roll_lines
                inst$     (temp%) = inst$     (temp%+roll%)
                type$     (temp%) = type$     (temp%+roll%)
                comp$     (temp%) = comp$     (temp%+roll%)
                group$    (temp%) = group$    (temp%+roll%)
                acct1$    (temp%) = acct1$    (temp%+roll%)
                acct2$    (temp%) = acct2$    (temp%+roll%)
                fixed$    (temp%) = fixed$    (temp%+roll%)
                print$    (temp%) = print$    (temp%+roll%)
                print_if$ (temp%) = print_if$ (temp%+roll%)
                col$      (temp%) = col$      (temp%+roll%)
                reverse$  (temp%) = reverse$  (temp%+roll%)
                text$     (temp%) = text$     (temp%+roll%)
                base$     (temp%) = base$     (temp%+roll%)
                memo$     (temp%) = memo$     (temp%+roll%)
                total$    (temp%) = total$    (temp%+roll%)
        return

        roll_bases
            REM Adjust base line References
            for i% = 1 to maxlines%
              if base$(i%) <= temp$ then L13710  /* NOT EFFECTED */
                   convert base$(i%) to temp%
                   convert temp% - roll% to base$(i%), pic(###)
L13710:     next i%
        return

        REM *************************************************************~
            *       I N P U T   M O D E   F O R   C O P Y               *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        copy_report
            errormsg$, startcode$, endcode$, display$(1) = " "

            for fieldnr% = 1 to  2
                gosub'054(fieldnr%)
                      if enabled% = 0 then L14180
L14120:         gosub'104(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then inputmode
                      if keyhit% <>  0 then       L14120
                gosub'154(fieldnr%)
                      if errormsg$ <> " " then L14120
L14180:         next fieldnr%

            report$ = startcode$
            gosub L30000
            if maxlines% = 0 then inputmode
            report$ = endcode$
            goto summary

        REM *************************************************************~
            *       I N P U T   M O D E   F O R   P R I N T             *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        print_data
            errormsg$, startcode$, endcode$ = " "

            for fieldnr% = 1 to  2
                gosub'053(fieldnr%)
                      if enabled% = 0 then L15180
L15120:         gosub'103(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then inputmode
                      if keyhit% <>  0 then       L15120
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L15120
L15180:         next fieldnr%

        REM *************************************************************~
            *               P R I N T  C O D E S                        *~
            *                                                           *~
            * Prints a report showing accounts tied to each code.       *~
            *************************************************************

            call "SHOSTAT" ("Printing Group/Accounts list")
            if startcode$ <> "ALL" then L16110
                readkey$ = all(hex(00))
                endcode$ = all(hex(ff))
                goto L16140
L16110:     str(readkey$,,3) = str(startcode$,,3) addc all(hex(ff))
            if endcode$ = " " then endcode$ = startcode$

L16140:     call "PLOWNEXT" (#01, readkey$, 0%, f1%(1))
                if f1%(1) = 1 then L16190
L16160:         close printer
                goto inputmode

L16190:     if str(readkey$,,3) > endcode$ then L16160
            gosub L29000
            get #01, using L16220, report$, descr$, title$
L16220:         FMT CH(3), CH(30), CH(50)
            pagenumber% = 0
            pageline% = 987654321

            gosub load_line_items
            for c% = 1 to maxlines%
                gosub form_control
               if acct2$(c%) = " " then acct2$(c%) = acct1$(c%)
               message$ = acct1$(c%) & " - " & acct2$(c%)
               if acct1$(c%) = " " and acct2$(c%) = " " then             ~
                      message$ = "N/A"
               if group$(c%) <> " " then message$= "GROUP: " & group$(c%)

            print using L17360, c%, inst$(c%), message$, fixed$(c%),      ~
                          print$(c%), print_if$(c%), col$(c%),           ~
                          reverse$(c%), text$(c%), base$(c%),            ~
                          str(total$(c%),1,1), str(total$(c%),2,1),      ~
                          str(total$(c%),3,1), str(total$(c%),4,1),      ~
                          str(total$(c%),5,1), str(total$(c%),6,1),      ~
                          str(total$(c%),7,1), str(total$(c%),8,1),      ~
                          str(total$(c%),9,1), str(total$(c%),10)
            tag% = 0
            next c%
            gosub form_control
            if tag% = 0 then print using L17390
            goto L16140

        REM *************************************************************~
            *        P A G E   C O N T R O L   R O U T I N E            *~
            *                                                           *~
            * CONTROLS THE PAGING                                       *~
            *************************************************************

        form_control
                select printer (134)
                pageline% = pageline% + 1
                if pageline% < 58 then return
                   if pagenumber% > 0 and tag% = 0 then print using L17390
                   print page
                   pagenumber% = pagenumber% + 1
                   print using L17230, pagenumber%, date$
                   print using L17260, report$, descr$, title$
                   print using L17290
                   print using L17320
                   print using L17340
                   print using L17390
                   pageline% = 7
                   tag% = 1
                   return

L17230: %PAGE ###    G/L   R E P O R T    F O R M A T   S P E C I F I C A~
        ~ T I O N S                                      DATE: ########

L17260: %REPORT: ###  DESCRIPTION: ##############################  DEFAUL~
        ~T TITLE: ##################################################

L17290: %!---+----+-------------------------+---+---+-----+---+---+------~
        ~---------------------------------------+----+-------------------!

L17320: %!SEQ!INST!ACCOUNT NUMBER RANGE     !IGN!PRT!PRINT!COL!REV!TEXT T~
        ~O PRINT                                !%   !     TOTALING      !
L17340: %!   !    !OR ACCOUNT GROUP CODE    !ZNS!FLG!WHEN !NMB!SGN!      ~
        ~                                       !BASE!      BUCKETS      !
L17360: %!###! ## !#########################!###!###!  #  ! # !###!######~
        ~#######################################!### !# # # # # # # # # #!

L17390: %!---+----+-------------------------+---+---+-----+---+---+------~
        ~---------------------------------------+----+0-1-2-3-4-5-6-7-8-9!

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Writing report " &report$& " to data files")
            REM DELETE OLD REPORT FROM FILE
            readkey$ = str(report$) & hex(0000)
            call "DELETE" (#01, readkey$, 3%)
            call "DELETE" (#02, readkey$, 3%)

            gosub L31000    /* Write New Report To Files */
            lastreport$ = report$
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1% : message$=" "
                  on fieldnr% gosub L20330,         /* REPORT NUMBER    */~
                                    L20370,         /* DESCRIPTION      */~
                                    L20402,         /* Set of books     */~
                                    L20410,         /* DEFAULT TITLE    */~
                                    L20450,         /* DEFAULT PAREN    */~
                                    L20490,         /* ROUND?           */~
                                    L20530,         /* PRINT IF ZERO?   */~
                                    L20580,         /* ZONE ONE         */~
                                    L20580,         /* ZONE TWO         */~
                                    L20580          /* ZONE THREE       */
                  return

            deffn'061(fieldnr%)
                  message$=" "
                  enabled% = 1
                  on fieldnr% gosub L20340,         /* REPORT NUMBER    */~
                                    L20380,         /* DESCRIPTION      */~
                                    L20402,         /* Set of books     */~
                                    L20420,         /* DEFAULT TITLE    */~
                                    L20460,         /* DEFAULT PAREN    */~
                                    L20500,         /* ROUND?           */~
                                    L20550,         /* PRINT IF ZERO?   */~
                                    L20610,         /* ZONE ONE         */~
                                    L20610,         /* ZONE TWO         */~
                                    L20610          /* ZONE THREE       */
                     return

L20330:     REM DEFAULT/ENABLE FOR REPORT NUMBER
L20340:         message$ = "Enter a blank report number to search for des~
        ~ired report."
                return
L20370:     REM DEFAULT/ENABLE FOR DESCRIPTION
L20380:         message$ = "Enter text for quick future reference."
                return
L20402: REM Enable code for Statutory or Local Authority G/L set  SET$
            if dual_books$ <> "Y" then enabled% = 0%
            message$ = "Enter '1' to use Statutory books; '2' to use"   &~
                " Local Authority books"
            return
L20410:     REM DEFAULT/ENABLE FOR DEFAULT TITLE
L20420:         message$= "Report Title for printing on report header (ma~
        ~y be changed at run time)."
                return
L20450:     REM DEFAULT/ENABLE FOR DEFAULT PARENTHESIS OPTION
L20460:         message$ = "Enter 'YES' if you normally will want parenth~
        ~eses rather than minus signs."
                return
L20490:     REM DEFAULT/ENABLE FOR DEFAULT ROUNDING OPTION
L20500:         message$ = "Enter 'YES' if you normally won't print 'cent~
        ~s' for dollar amounts."
                return
L20530:     REM DEFAULT/ENABLE FOR DEFAULT PRINT IF ZERO OPTION
                zero$ = "YES"
L20550:         message$ = "Enter 'NO' if you normally won't print lines ~
        ~when amount(s) on line are all 0."
                return
L20580:     REM DEFAULT/ENABLE FOR ZONE SPECIFICATIONS
                if fieldnr% > 7 and zonename$(fieldnr%-7) = " " then     ~
                                                             enabled% = 0
L20610:         message$ = "Enter Run Time Account selection criteria if ~
        ~desired. See user manual for info."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   L A B O R       *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%:message$=" "
                  on fieldnr% gosub L21400,      /* INSTRUCTION CODE    */~
                                    L21440,      /* TOTAL TYPE CODE     */~
                                    L21500,      /* COMPANY CODE        */~
                                    L21550,      /* ACCOUNTS            */~
                                    L21630,      /* ZONE OVERRIDE       */~
                                    L21730,      /* PRINT?              */~
                                    L21830,      /* PRINT RESTRICTION   */~
                                    L21930,      /* PRINT COLUMN        */~
                                    L22020,      /* REVERSE SIGNS       */~
                                    L22120,      /* TEXT FOR PRINT      */~
                                    L22180,      /* % BASE LINE         */~
                                    L22280,      /* MEMO TEXT           */~
                                    L22330       /* TOTALING FLAGS      */
            return

            deffn'062(fieldnr%)
                  enabled% = 1:message$=" "
                  on fieldnr% goto  L21410,      /* INSTRUCTION CODE    */~
                                    L21440,      /* TOTAL TYPE CODE     */~
                                    L21510,      /* COMPANY CODE        */~
                                    L21560,      /* ACCOUNTS            */~
                                    L21640,      /* ZONE OVERRIDE       */~
                                    L21800,      /* PRINT?              */~
                                    L21840,      /* PRINT RESTRICTION   */~
                                    L21940,      /* PRINT COLUMN        */~
                                    L22030,      /* REVERSE SIGNS       */~
                                    L22160,      /* TEXT FOR PRINT      */~
                                    L22190,      /* % BASE LINE         */~
                                    L22300,      /* MEMO TEXT           */~
                                    L22340       /* TOTALING FLAGS      */
                     return

L21400:     REM DEFAULT/ENABLE FOR INSTRUCTION CODE
L21410:         message$ = "Enter Instruction Code. Using PF14 To Display~
        ~ Valid Instructions"
                return
L21440:     REM DEFAULT/ENABLE FOR TOTAL TYPE CODE
                if str(inst$(c%),,1) <> "T" then enabled% = 0
                if print$(c%) = "NO" then enabled% = 0
                message$ = "This Is For The Balance Sheet Only. Enter Wha~
        ~t 'type' of total is to print"
                return
L21500:     REM DEFAULT/ENABLE FOR COMPANY NUMBER
L21510:         enabled% = 0
                message$ = "Enter The Company To Extract G/L Data From. B~
        ~lank To Use Primary Data Base"
                return
L21550:     REM DEFAULT/ENABLE FOR ACCOUNTS
L21560:         enabled% = 0
                if inst$(c%) = "CB" or inst$(c%) = "PA" then enabled% = 1
                if inst$(c%) = "PO" or inst$(c%) = "YO" then enabled% = 1
                if inst$(c%) = "YA" then enabled% = 1
                message$="Enter A Group Code And All Accts In That Code W~
        ~ill Be Used,OR Enter Accts Nmbrs"
                return
L21630:     REM DEFAULT/ENABLE FOR ZONE OVERRIDE
L21640:         enabled% = 0
                if zonename$() = " " then return
                if inst$(c%) = "CB" or inst$(c%) = "PA" then enabled% = 1
                if inst$(c%) = "PO" or inst$(c%) = "YO" then enabled% = 1
                if inst$(c%) = "YA" then enabled% = 1
                if fixed$(c%)=" " and enabled%=1 then fixed$(c%)="NO"
                message$ = "'YES' Will Force ALL accts Indicated To ALWAY~
        ~S Be Used, Irregardless Of Zoning"
                return
L21730:     REM DEFAULT/ENABLE FOR PRINT?
                enabled% = 0
                if inst$(c%) = "CB" or inst$(c%) = "PA" then enabled% = 1
                if inst$(c%) = "PO" or inst$(c%) = "YO" then enabled% = 1
                if inst$(c%) = "YA" then enabled% = 1
                if str(inst$(c%),,1) = "T" then enabled% = 1
                if enabled% = 0 then print$(c%) = "YES"
L21800:         message$ = "Enter Print Option... Use PF(14) To Display T~
        ~he List Of Options."
                return
L21830:     REM DEFAULT/ENABLE FOR PRINT RESTRICTIONS
L21840:         enabled% = 0
                if print$(c%) = "NO " then return
                if inst$(c%) = "CB" or inst$(c%) = "PA" then enabled% = 1
                if inst$(c%) = "PO" or inst$(c%) = "YO" then enabled% = 1
                if inst$(c%) = "YA" then enabled% = 1
                if str(inst$(c%),,1) = "T" then enabled% = 1
                message$ = "Enter Desired Print Restrictor Codes... Use P~
        ~F(14) To Display List Of Options."
                return
L21930:     REM DEFAULT/ENABLE FOR PRINT COLUMN
L21940:         if print$(c%) = "NO" then enabled% = 0
                if inst$(c%) = "H " then enabled% = 0
                if inst$(c%) = "D " then enabled% = 0
                if inst$(c%) = "DC" then enabled% = 0
                if inst$(c%) = "P " then enabled% = 0
                message$ = "Indicate Column (1-3) To Print Amount(s) In. ~
        ~Is Ignored By P&L Type Drivers."
                return
L22020:     REM DEFAULT/ENABLE FOR REVERSE SIGN INDICTOR
L22030:         enabled% = 0
                if print$(c%) = "NO " then return
                if inst$(c%) = "CB" or inst$(c%) = "PA" then enabled% = 1
                if inst$(c%) = "PO" or inst$(c%) = "YO" then enabled% = 1
                if inst$(c%) = "YA" then enabled% = 1
                if str(inst$(c%),,1) = "T" then enabled% = 1
                message$ = "Enter 'YES' To Reverse Sign On Numbers Before~
        ~ Printing And Adding To Totals"
                return
L22120:     REM DEFAULT/ENABLE FOR TEXT FOR PRINT
                if print$(c%) = "NO " then enabled% = 0
                if print$(c%) = "ALL" then enabled% = 0
                if inst$(c%) = "U " or inst$(c%) = "DU" then enabled% = 0
L22160:         message$ = "Enter Text To Be Printed On The Report Line"
                return
L22180:     REM DEFAULT/ENABLE FOR BASE LINE FOR PERCENTAGE CALULATIONS
L22190:         enabled% = 0
                if print$(c%) = "NO " then return
                if inst$(c%) = "CB" or inst$(c%) = "PA" then enabled% = 1
                if inst$(c%) = "PO" or inst$(c%) = "YO" then enabled% = 1
                if inst$(c%) = "YA" then enabled% = 1
                if str(inst$(c%),,1) = "T" then enabled% = 1
L22250:         message$="If This Report Will Be Used With The % Anal. Re~
        ~port, Enter Base Format Line."
                return
L22280:     REM DEFAULT/ENABLE FOR MEMO TEXT
                if keyhit% = 4 then L22250
L22300:         message$ = "This Is Just A Place For You To Put Notes For~
        ~ Later Reference."
                return
L22330:     REM DEFAULT/ENABLE FOR TOTALING SPECS
L22340:         enabled% = 0
                if inst$(c%) = "CB" or inst$(c%) = "PA" then enabled% = 1
                if inst$(c%) = "PO" or inst$(c%) = "YO" then enabled% = 1
                if inst$(c%) = "YA" then enabled% = 1
                if str(inst$(c%),,1) = "T" then enabled% = 1
                message$ = "Indicate How This Line Is To Effect Totaling ~
        ~Buckets. Use PF(14) For More Info."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 3 OF INPUT. *~
            *************************************************************

            deffn'053(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L23110,         /* STARTING REPORT  */~
                                    L23160          /* ENDING REPORT    */
                     return
L23110:     REM DEFAULT/ENABLE FOR STARTING CODE
            startcode$ = "ALL"
            message$ = "ALL Will Print all Formats On File. To Print Rang~
        ~es, Enter The Starting Format."
                return
L23160:     REM DEFAULT/ENABLE FOR ENDING CODE
            if startcode$ = "ALL" then enabled% = 0
            message$ = "Enter The Last To Print.  Leave Blank To Only Pri~
        ~nt The 'Starting Report' Id."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   4     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 4 OF INPUT. *~
            *************************************************************

            deffn'054(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L24110,         /* COPY FROM        */~
                                    L24160          /* COPY TO          */
                     return
L24110:     REM DEFAULT/ENABLE FOR REPORT TO COPY
            message$ = "Enter The Id. Of The Report That Is To Be Copied"
                return
L24160:     REM DEFAULT/ENABLE FOR COPY TO
            message$ = "Enter Id. For The New Report... It Must Not Be Th~
        ~e Same As An Existing Report"
                return

L29000: REM *************************************************************~
            * INITIALIZATION BLOCK (NEATER THAN CRAMMING AT 10000)      *~
            *************************************************************

            acct1$(),                    /* FROM ACCOUNT               */~
            acct2$(),                    /* TO ACCOUNT                 */~
            acctthru$(),                 /* THRU INDICATOR             */~
            base$(),                     /* FROM ACCOUNT               */~
            col$(),                      /* COLUMN INDICATOR           */~
            comp$(),                     /* COMPANY (DBASE POINTER)    */~
            compdescr$,                  /* COMPANY DESCRIPTION        */~
            descr$,                      /* REPORT DESCRIPTION         */~
            errormsg$,                   /* ERROR MESSAGE              */~
            filler$,                     /* FILLER AT END OF RECORD    */~
            fixed$(),                    /* RUN TIME OVERRIDE FLAG     */~
            group$(),                    /* ACCOUNT GROUPING CODE      */~
            groupdescr$,                 /* ACCOUNT GROUPING CODE DESCR*/~
            inst$(),                     /* INSTRUCTION CODE           */~
            instdescr$,                  /* FOR DISPLAY                */~
            message$,                    /* INPUT MESSAGE              */~
            memo$(),                     /* JUNK TEXT                  */~
            paren$,                      /* DEFAUT PARENTHESIS OPTION  */~
            print$(),                    /* PRINT CODE                 */~
            print_if$(),                 /* PRINT/TOTAL CONSTRAINT FLAG*/~
            report$,                     /* REPORT ID.                 */~
            reverse$(),                  /* REVERSE SIGN INDICATOR     */~
            round$,                      /* ROUND OPTION (DEFUALT)     */~
            set$, setdescr$,             /* Set of books to use        */~
            text$(),                     /* TEXT FOR LINE ITEMS        */~
            title$,                      /* DEFAULT REPORT TITLE       */~
            total$(),                    /* TOTALING SPEC FOR LINE     */~
            totaltext$,                  /* SCREEN PROMPT FOR 'TYPE' FI*/~
            type$(),                     /* TEXT FOR LINE ITEMS        */~
            zero$,                       /* PRINT IF ZERO? (DEFUALT)   */~
            zone$(),                     /* START, LENGTH IN ACCOUNT   */~
            zonename$() = " "            /* LABLE FOR ZONE IN ACCOUNT  */

        return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            ask% = 2%
            call "STARTOVR" (ask%)
            if ask% = 1% then return
            return clear all
            goto L10000

L30000: REM *************************************************************~
            *              L O A D   O L D   R E P O R T                *~
            *                                                           *~
            * LOADS OLD REPORT FROM DISK FILES.                         *~
            *************************************************************

            onfile% = 0

            REM Try And Load Report...
            call "READ100" (#01, report$, f1%(1))
                if f1%(1) = 0 then return       /* New Report */

            REM Load And Format Data, Header First...
            onfile% = 1
            get #01, using L30430, report$, descr$, title$, paren$, zero$,~
                round$, zone$(), zonename$(), set$, filler$
            if set$ <> "2" or dual_books$ <> "Y" then set$ = "1"
            convert set$ to set, data goto L30153
L30153:     gosub describe_set_of_books

        load_line_items
            REM Now Load The Line Items
            maxlines% = 0
            readkey$ = str(report$) & hex(0000)
L30210:     call "PLOWNEXT" (#02%, readkey$, 3%, f1%(2%))
                     if f1%(2) = 0 then return

            c%, maxlines% = maxlines% + 1
            get #02, using L30530, report$, c%, inst$(c%), comp$(c%),     ~
                          group$(c%), acct1$(c%), acct2$(c%),            ~
                          fixed$(c%), print$(c%), print_if$(c%),         ~
                          col$(c%), reverse$(c%), text$(c%), base$(c%),  ~
                          total$(c%), memo$(c%), type$(c%)

            if fixed$(c%) = "Y" then fixed$(c%) = "YES"
            if fixed$(c%) = "N" then fixed$(c%) = "NO "
            if reverse$(c%) = "Y" then reverse$(c%) = "YES"
            if reverse$(c%) = "N" then reverse$(c%) = "NO "
            if print$(c%) = "Y" then print$(c%) = "YES"
            if print$(c%) = "N" then print$(c%) = "NO "
            if print$(c%) = "A" then print$(c%) = "ALL"
            if print$(c%) = "S" then print$(c%) = "SUM"
            if set = 2 then goto L30402
                call "GLFMT" (acct1$(c%))
                call "GLFMT" (acct2$(c%))
                goto L30210
L30402:     call "GLFMT2" (acct1$(c%))
            call "GLFMT2" (acct2$(c%))
            goto L30210

L30430:         FMT /* Record layout for file #01 -- FRNAMES           */~
                    CH(03),              /* REPORT NUMBER              */~
                    CH(30),              /* DESCRIPTION                */~
                    CH(50),              /* TITLE FOR REPORT           */~
                    CH(03),              /* PARENTHESIS FLAG           */~
                    CH(03),              /* XERO FLAG                  */~
                    CH(03),              /* ROUND FLAG                 */~
                    8*CH(2),             /* ZONE POSITIONS             */~
                    4*CH(20),            /* ZONE NAMES                 */~
                    CH(01),              /* Set of books to use        */~
                    CH(11)               /* FILLER FOR FUTURE USE      */

L30530:         FMT /* Record layout for file #02 -- FRFORMAT          */~
                    CH(03),              /* REPORT NUMBER              */~
                    BI(2),               /* SEQUENCE NUMBER            */~
                    CH(02),              /* INSTRUCTION CODE           */~
                    CH(03),              /* COMPANY CODE               */~
                    CH(06),              /* ACCOUNT GROUP CODE         */~
                    CH(16),              /* START ACCOUNT NUMBER       */~
                    CH(16),              /* END ACCOUNT NUMBER         */~
                    CH(01),              /* IGNORE ZONING FLAG         */~
                    CH(01),              /* PRINT FLAG                 */~
                    CH(01),              /* PRINT RESTRICTION          */~
                    CH(01),              /* COLUMN INDICATOR           */~
                    CH(01),              /* REVERSE SIGNS?             */~
                    CH(45),              /* DESCRIPTION                */~
                    CH(03),              /* LINE NUMBER TO BASE %S ON  */~
                    CH(10),              /* TOTALING SPECS             */~
                    CH(70),              /* MEMO SPACE                 */~
                    CH(02),              /* TOTAL TYPE (BAL ONLY)      */~
                    CH(17)               /* FILLER                     */

L31000: REM *************************************************************~
            *        W R I T E   R E P O R T   T O   F I L E S          *~
            *                                                           *~
            * WRITES REPORT SPECS TO DISK.                              *~
            *************************************************************

            if maxlines% = 0 then return

            REM Lines are saved first...
            if maxlines% = 0 then L31200
            for c% = 1 to maxlines%
            if set = 2 then goto L31122
                call "GLUNFMT" (acct1$(c%))
                call "GLUNFMT" (acct2$(c%))
                goto L31130
L31122:     call "GLUNFM2" (acct1$(c%))
            call "GLUNFM2" (acct2$(c%))
L31130:     write #02, using L30530, report$, c%, inst$(c%), comp$(c%),   ~
                            group$(c%), acct1$(c%), acct2$(c%),          ~
                            fixed$(c%), print$(c%), print_if$(c%),       ~
                            col$(c%), reverse$(c%), text$(c%), base$(c%),~
                            total$(c%), memo$(c%), type$(c%), " "
            next c%

L31200:     REM Write the header...
            convert set to set$, pic (#)
            write #01,using L30430,report$, descr$, title$, paren$, zero$,~
                round$, zone$(), zonename$(), set$, filler$
            return

        REM *************************************************************~
            *      I N P U T   /   E D I T   M O D E   P A G E   1      *~
            *                                                           *~
            * SERVES INPUT LOOP AND EDIT MODE FOR PAGE ONE OF DOCUMENT. *~
            *************************************************************

            deffn'101(screen%, fieldnr%)
                  if fieldnr% = 3% and dual_books$ <> "Y" then return
                  gosub set_keys
                  init(hex(84)) hfac$()
                  if fieldnr%=0% then init(hex(86)) str(hfac$(),2)
                  header1$ = "Last Report Updated: XXX"
                  str(header1$,22,3) = lastreport$
                  str(header1$,62) = "FRDEFINE: " & str(cms2v$,,8)
                  display$(1)= "The Following Fields Are Run Time Defau"&~
                             "lts/Specifications..."

                  display$(2)= "Allowed Run Time Account Zoning...   " & ~
                             "Zone Description          " & "Start   " & ~
                             "  Length"


                  on fieldnr% gosub L40360,         /* REPORT NUMBER    */~
                                    L40330,         /* DESCRIPTION      */~
                                    L40390,         /* Set of books     */~
                                    L40330,         /* DEFAULT TITLE    */~
                                    L40360,         /* DEFAULT PAREN    */~
                                    L40360,         /* ROUND?           */~
                                    L40360,         /* PRINT IF ZERO?   */~
                                    L40330,         /* ZONE ONE         */~
                                    L40330,         /* ZONE TWO         */~
                                    L40330          /* ZONE THREE       */
                     goto L40430

L40330:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      hfac$(fieldnr%) = hex(80)
                      return
L40360:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      hfac$(fieldnr%) = hex(81)
                      return
L40390:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      hfac$(fieldnr%) = hex(82)
                      return

L40430: accept                                                           ~
               at (01,02), "Financial Statements Report Specifications", ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header1$                ,ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Report Number",                              ~
               at (06,30), fac(hfac$(1)), report$                ,ch(03),~
                                                                         ~
               at (07,02), "Description (Internal)",                     ~
               at (07,30), fac(hfac$(2)), descr$                , ch(30),~
                                                                         ~
               at (08,02), fac(hex(8c)),   setmsg$              , ch(26),~
               at (08,30), fac(hfac$( 3)), set$                 , ch(01),~
               at (08,33), fac(hex(8c)),   setdescr$            , ch(30),~
                                                                         ~
               at (10,02), fac(hex(ac)), display$(1)             ,ch(79),~
               at (11,03), "Default Report Title",                       ~
               at (11,30), fac(hfac$(4)), title$                , ch(50),~
                                                                         ~
               at (12,03), "Parentheses To Show Negatives?",             ~
               at (12,36), fac(hfac$(5)), paren$                 ,ch(03),~
                                                                         ~
               at (13,03), "Round Amounts To Nearest Dollar?",           ~
               at (13,36), fac(hfac$(6)), round$                 ,ch(03),~
                                                                         ~
               at (14,03), "Print Lines When All Zeroes?",               ~
               at (14,36), fac(hfac$(7)), zero$                  ,ch(03),~
                                                                         ~
               at (16,02), fac(hex(ac)), display$(2)             ,ch(79),~
                                                                         ~
               at (17,03), "First Zone",                                 ~
               at (17,39), fac(hfac$(8)), zonename$(1)           ,ch(20),~
               at (17,66), fac(hfac$(8)), zone$(1,1)             ,ch(02),~
               at (17,77), fac(hfac$(8)), zone$(1,2)             ,ch(02),~
                                                                         ~
               at (18,03), "Second Zone",                                ~
               at (18,39), fac(hfac$(9)), zonename$(2)           ,ch(20),~
               at (18,66), fac(hfac$(9)), zone$(2,1)             ,ch(02),~
               at (18,77), fac(hfac$(9)), zone$(2,2)             ,ch(02),~
                                                                         ~
               at (19,03), "Third Zone",                                 ~
               at (19,39), fac(hfac$(10)), zonename$(3)          ,ch(20),~
               at (19,66), fac(hfac$(10)), zone$(3,1)            ,ch(02),~
               at (19,77), fac(hfac$(10)), zone$(3,2)            ,ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$                ,ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)             ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2)             ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)             ,ch(79),~
                                                                         ~
               keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13 then L41010
                  call "MANUAL" ("FRDEFINE")
                  goto L40430

L41010:        if keyhit% <> 15 then L41050
                  call "PRNTSCRN"
                  goto L40430

L41050:        if fieldnr% <> 0 then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *          I N P U T  /  E D I T  S C R E E N   2           *~
            *                                                           *~
            * MANAGES THE LABOR LINE DETAIL.                            *~
            *************************************************************

            deffn'102(screen%, fieldnr%)
                  gosub set_keys
                  init(hex(84)) lfac$()
                  if fieldnr%=0% then init(hex(86)) lfac$()
                  header1$ = "REPORT: " & report$ & " " & descr$
                  str(header1$,62) = "FRDEFINE: " & str(cms2v$,,8)
            REM Format line display for top of screen
            if fieldnr% > 1 then L42480
            k% = max(0, c%-2)
            display$(), range$() = " "
            save% = c%
            for i% = 1 to 3
                c% = k%+i%
                if c% > maxlines% then L42360
                if inst$(c%) = " " then L42300
                range$(i%)= acct1$(c%)
                if acct2$(c%) <> " " then range$(i%) = range$(i%) &      ~
                                                        " - "& acct2$(c%)
                if group$(c%) <> " " then range$(i%)="Account Group: " & ~
                                                             group$(c%)
                if inst$(c%) = "CB" or inst$(c%) = "PA" then L42300
                if inst$(c%) = "PO" or inst$(c%) = "YO" then L42300
                if inst$(c%) = "YA" then L42300
                     gosub'152(1%) : range$(i%) = instdescr$
                     if errormsg$<>" " then range$(i%)=str(errormsg$,,19)
L42300:         temp$ = hex(8c) : if c% = save% then temp$ = hex(84)
                put display$(i%), using L42340, temp$, line$(c%),         ~
                    inst$(c%), range$(i%), print$(c%), text$(c%),        ~
                    base$(c%)
L42340:         FMT CH(1), CH(4), XX(1), CH(2), XX(3), CH(25), XX(2),    ~
                    CH(3), XX(3), CH(31), XX(1), CH(3)
L42360:     next i%
            c% = save%
            errormsg$ = " "
            gosub'152(1%) /* Reset Instruction Description */
            str(display$(3),,1) = " "
            if fieldnr% > 0 then L42480
                if inst$(c%) = "CB" or inst$(c%) = "PA" then L42460
                if inst$(c%) = "PO" or inst$(c%) = "YO" then L42460
                if str(inst$(c%),,1) = "T" then L42460
                if inst$(c%) <> "YA" then L42480
L42460:         tran(total$(c%), hex(0b20))replacing

L42480:           on fieldnr% gosub L42660,         /* INSTRUCTION      */~
                                    L42660,         /* TOTAL TYPE       */~
                                    L42660,         /* COMPANY CODE     */~
                                    L42660,         /* ACCOUNTS         */~
                                    L42660,         /* ZONE OVERRIDE    */~
                                    L42660,         /* PRINT?           */~
                                    L42660,         /* PRINT RESTRICTION*/~
                                    L42690,         /* PRINT COLUMN     */~
                                    L42660,         /* REVERSE SIGNS    */~
                                    L42630,         /* TEXT FOR PRINT   */~
                                    L42690,         /* % BASE LINE      */~
                                    L42630,         /* MEMO TEXT        */~
                                    L42660          /* TOTALING FLAGS   */
                     goto L42730

L42630:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L42660:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L42690:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L42730: accept                                                           ~
               at (01,02), "Financial Statements Report Specifications", ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header1$                ,ch(79),~
               at (03,02), fac(hex(94)), errormsg$               ,ch(79),~
                                                                         ~
               at (04,02), fac(hex(ac)), header$                 ,ch(79),~
               at (05,02), fac(hex(8c)), display$(1)             ,ch(79),~
               at (06,02), fac(hex(8c)), display$(2)             ,ch(79),~
               at (07,02), fac(hex(ac)), display$(3)             ,ch(79),~
                                                                         ~
               at (08,03), "Instruction Code",                           ~
               at (08,22), fac(lfac$(1)),  inst$(c%)             ,ch(02),~
               at (08,26), fac(hex(8c)),   instdescr$            ,ch(32),~
               at (08,60), fac(hex(8c)),   totaltext$            ,ch(16),~
               at (08,77), fac(lfac$(2)),  type$(c%)             ,ch(02),~
                                                                         ~
               at (09,03), "Company Code",                               ~
               at (09,22), fac(lfac$(3)),  comp$(c%)             ,ch(03),~
               at (09,32), fac(hex(8c)),   compdescr$            ,ch(32),~
                                                                         ~
               at (10,03), "Account Group Code",                         ~
               at (10,22), fac(lfac$(4)),  group$(c%)            ,ch(06),~
               at (10,32), "Or G/L Accounts",                            ~
               at (10,48), fac(lfac$(4)),  acct1$(c%)            ,ch(12),~
               at (10,61), "-",                                          ~
               at (10,64), fac(lfac$(4)),  acct2$(c%)            ,ch(12),~
                                                                         ~
               at (11,03), "Ignore Run Time Zone Selections?",           ~
               at (11,36), fac(lfac$(5)),  fixed$(c%)            ,ch(03),~
                                                                         ~
               at (12,03), "Print Line(s) On Report?",                   ~
               at (12,28), fac(lfac$(6)),  print$(c%)            ,ch(03),~
               at (12,32), "As Long As Amount Is:",                      ~
               at (12,54), fac(lfac$(7)),  print_if$(c%)         ,ch(01),~
               at (12,58), "(+, -, or blank)",                           ~
                                                                         ~
               at (13,03), "Print In Column",                            ~
               at (13,22), fac(lfac$(8)),  col$(c%)              ,ch(01),~
               at (13,32), "Reverse Signs?",                             ~
               at (13,47), fac(lfac$(9)),  reverse$(c%)          ,ch(03),~
                                                                         ~
               at (14,03), "Text To Print",                              ~
               at (14,22), fac(lfac$(10)), text$(c%)             ,ch(45),~
                                                                         ~
               at (15,03), "Line To Base Percentage Calculation On",     ~
               at (15,43), fac(lfac$(11)), base$(c%)             ,ch(03),~
                                                                         ~
               at (16,03), "Memo",                                       ~
               at (16,9), fac(lfac$(12)),  memo$(c%)             ,ch(70),~
                                                                         ~
               at (18,05), "Totaling Control      Bucket #: 0 1 2 3 4 5 6~
        ~ 7 8 9",                                                         ~
               at (19,06), "Specifications",                             ~
               at (19,37), fac(lfac$(13)), str(total$(c%),1,1)   ,ch(01),~
               at (19,39), fac(lfac$(13)), str(total$(c%),2,1)   ,ch(01),~
               at (19,41), fac(lfac$(13)), str(total$(c%),3,1)   ,ch(01),~
               at (19,43), fac(lfac$(13)), str(total$(c%),4,1)   ,ch(01),~
               at (19,45), fac(lfac$(13)), str(total$(c%),5,1)   ,ch(01),~
               at (19,47), fac(lfac$(13)), str(total$(c%),6,1)   ,ch(01),~
               at (19,49), fac(lfac$(13)), str(total$(c%),7,1)   ,ch(01),~
               at (19,51), fac(lfac$(13)), str(total$(c%),8,1)   ,ch(01),~
               at (19,53), fac(lfac$(13)), str(total$(c%),9,1)   ,ch(01),~
               at (19,55), fac(lfac$(13)), str(total$(c%),10,1)  ,ch(01),~
               at (19,58), "(+, -, 0, or blank)",                        ~
                                                                         ~
               at (21,02), fac(hex(a4)), message$                ,ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)             ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2)             ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)             ,ch(79),~
               keys(pfkeys$), key (keyhit%)

               tran(total$(c%), hex(200b))replacing
               if keyhit% <> 13 then L43530
                  call "MANUAL" ("FRDEFINE")
                  goto L42730

L43530:        if keyhit% <> 15 then L43570
                  call "PRNTSCRN"
                  goto L42730

L43570:        if fieldnr% <> 0 or keyhit% > 0 then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *          L I N E   S U M M A R Y   S C R E E N            *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 3 OF DOCUMENT.                    *~
            *************************************************************

            deffn'110(fieldnr%)    /* Scan Mode */
            init (hex(8e)) hfac$()
            init (hex(8c)) lfac$()
            screen% = 5
            goto L44170

            deffn'112(fieldnr%)    /* Delete Mode */
            init (hex(8c)) lfac$(), hfac$()
            hfac$(fieldnr%),lfac$(fieldnr%)=hex(94)
            screen% = 6

L44170:     range$() = " "
            save% = c%
            for i% = 1 to 14
                c% = b%+i%
                if inst$(c%) <> " " and c% <= maxlines% then L44240
                     hfac$(i%), lfac$(i%) = hex(9c)
                     goto L44340
L44240:         range$(i%) = acct1$(c%)
                if acct2$(c%) <> " " then range$(i%) = range$(i%) &      ~
                                                        " - "& acct2$(c%)
                if group$(c%) <> " " then range$(i%)="Account Group: " & ~
                                                             group$(c%)
                if inst$(c%) = "CB" or inst$(c%) = "PA" then L44340
                if inst$(c%) = "PO" or inst$(c%) = "YO" then L44340
                if inst$(c%) = "YA" then L44340
                     gosub'152(1%)
                     range$(i%) = instdescr$
                     if errormsg$<>" " then range$(i%)=str(errormsg$,,19)
L44340:     next i%
            c% = save%
            gosub set_keys
            header1$ = "Report Summary Screen"
            str(header1$,62) = "FRDEFINE: " & str(cms2v$,,8)

L44400:     accept                                                       ~
               at (01,02), "Financial Statements Report Specifications", ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header1$                ,ch(79),~
               at (04,02), "Report Number:",                             ~
               at (04,17), fac(hex(84)),   report$               ,ch(03),~
               at (04,21), fac(hex(8c)),   descr$                ,ch(30),~
               at (06,02), fac(hex(ae)),   header$               ,ch(79),~
                                                                         ~
               at (07,02), fac(hfac$( 1)),  line$  (b%+ 1)       ,ch(04),~
               at (08,02), fac(hfac$( 2)),  line$  (b%+ 2)       ,ch(04),~
               at (09,02), fac(hfac$( 3)),  line$  (b%+ 3)       ,ch(04),~
               at (10,02), fac(hfac$( 4)),  line$  (b%+ 4)       ,ch(04),~
               at (11,02), fac(hfac$( 5)),  line$  (b%+ 5)       ,ch(04),~
               at (12,02), fac(hfac$( 6)),  line$  (b%+ 6)       ,ch(04),~
               at (13,02), fac(hfac$( 7)),  line$  (b%+ 7)       ,ch(04),~
               at (14,02), fac(hfac$( 8)),  line$  (b%+ 8)       ,ch(04),~
               at (15,02), fac(hfac$( 9)),  line$  (b%+ 9)       ,ch(04),~
               at (16,02), fac(hfac$(10)),  line$  (b%+10)       ,ch(04),~
               at (17,02), fac(hfac$(11)),  line$  (b%+11)       ,ch(04),~
               at (18,02), fac(hfac$(12)),  line$  (b%+12)       ,ch(04),~
               at (19,02), fac(hfac$(13)),  line$  (b%+13)       ,ch(04),~
               at (20,02), fac(hfac$(14)),  line$  (b%+14)       ,ch(04),~
                                                                         ~
               at (07,08), fac(lfac$( 1)),  inst$  (b%+ 1)       ,ch(02),~
               at (08,08), fac(lfac$( 2)),  inst$  (b%+ 2)       ,ch(02),~
               at (09,08), fac(lfac$( 3)),  inst$  (b%+ 3)       ,ch(02),~
               at (10,08), fac(lfac$( 4)),  inst$  (b%+ 4)       ,ch(02),~
               at (11,08), fac(lfac$( 5)),  inst$  (b%+ 5)       ,ch(02),~
               at (12,08), fac(lfac$( 6)),  inst$  (b%+ 6)       ,ch(02),~
               at (13,08), fac(lfac$( 7)),  inst$  (b%+ 7)       ,ch(02),~
               at (14,08), fac(lfac$( 8)),  inst$  (b%+ 8)       ,ch(02),~
               at (15,08), fac(lfac$( 9)),  inst$  (b%+ 9)       ,ch(02),~
               at (16,08), fac(lfac$(10)),  inst$  (b%+10)       ,ch(02),~
               at (17,08), fac(lfac$(11)),  inst$  (b%+11)       ,ch(02),~
               at (18,08), fac(lfac$(12)),  inst$  (b%+12)       ,ch(02),~
               at (19,08), fac(lfac$(13)),  inst$  (b%+13)       ,ch(02),~
               at (20,08), fac(lfac$(14)),  inst$  (b%+14)       ,ch(02),~
                                                                         ~
               at (07,12), fac(lfac$( 1)),  range$ ( 1)          ,ch(25),~
               at (08,12), fac(lfac$( 2)),  range$ ( 2)          ,ch(25),~
               at (09,12), fac(lfac$( 3)),  range$ ( 3)          ,ch(25),~
               at (10,12), fac(lfac$( 4)),  range$ ( 4)          ,ch(25),~
               at (11,12), fac(lfac$( 5)),  range$ ( 5)          ,ch(25),~
               at (12,12), fac(lfac$( 6)),  range$ ( 6)          ,ch(25),~
               at (13,12), fac(lfac$( 7)),  range$ ( 7)          ,ch(25),~
               at (14,12), fac(lfac$( 8)),  range$ ( 8)          ,ch(25),~
               at (15,12), fac(lfac$( 9)),  range$ ( 9)          ,ch(25),~
               at (16,12), fac(lfac$(10)),  range$ (10)          ,ch(25),~
               at (17,12), fac(lfac$(11)),  range$ (11)          ,ch(25),~
               at (18,12), fac(lfac$(12)),  range$ (12)          ,ch(25),~
               at (19,12), fac(lfac$(13)),  range$ (13)          ,ch(25),~
               at (20,12), fac(lfac$(14)),  range$ (14)          ,ch(25),~
                                                                         ~
               at (07,40), fac(lfac$( 1)),  print$ (b%+ 1)       ,ch(03),~
               at (08,40), fac(lfac$( 2)),  print$ (b%+ 2)       ,ch(03),~
               at (09,40), fac(lfac$( 3)),  print$ (b%+ 3)       ,ch(03),~
               at (10,40), fac(lfac$( 4)),  print$ (b%+ 4)       ,ch(03),~
               at (11,40), fac(lfac$( 5)),  print$ (b%+ 5)       ,ch(03),~
               at (12,40), fac(lfac$( 6)),  print$ (b%+ 6)       ,ch(03),~
               at (13,40), fac(lfac$( 7)),  print$ (b%+ 7)       ,ch(03),~
               at (14,40), fac(lfac$( 8)),  print$ (b%+ 8)       ,ch(03),~
               at (15,40), fac(lfac$( 9)),  print$ (b%+ 9)       ,ch(03),~
               at (16,40), fac(lfac$(10)),  print$ (b%+10)       ,ch(03),~
               at (17,40), fac(lfac$(11)),  print$ (b%+11)       ,ch(03),~
               at (18,40), fac(lfac$(12)),  print$ (b%+12)       ,ch(03),~
               at (19,40), fac(lfac$(13)),  print$ (b%+13)       ,ch(03),~
               at (20,40), fac(lfac$(14)),  print$ (b%+14)       ,ch(03),~
                                                                         ~
               at (07,46), fac(lfac$( 1)),  text$  (b%+ 1)       ,ch(31),~
               at (08,46), fac(lfac$( 2)),  text$  (b%+ 2)       ,ch(31),~
               at (09,46), fac(lfac$( 3)),  text$  (b%+ 3)       ,ch(31),~
               at (10,46), fac(lfac$( 4)),  text$  (b%+ 4)       ,ch(31),~
               at (11,46), fac(lfac$( 5)),  text$  (b%+ 5)       ,ch(31),~
               at (12,46), fac(lfac$( 6)),  text$  (b%+ 6)       ,ch(31),~
               at (13,46), fac(lfac$( 7)),  text$  (b%+ 7)       ,ch(31),~
               at (14,46), fac(lfac$( 8)),  text$  (b%+ 8)       ,ch(31),~
               at (15,46), fac(lfac$( 9)),  text$  (b%+ 9)       ,ch(31),~
               at (16,46), fac(lfac$(10)),  text$  (b%+10)       ,ch(31),~
               at (17,46), fac(lfac$(11)),  text$  (b%+11)       ,ch(31),~
               at (18,46), fac(lfac$(12)),  text$  (b%+12)       ,ch(31),~
               at (19,46), fac(lfac$(13)),  text$  (b%+13)       ,ch(31),~
               at (20,46), fac(lfac$(14)),  text$  (b%+14)       ,ch(31),~
                                                                         ~
               at (07,78), fac(lfac$( 1)),  base$  (b%+01)       ,ch(03),~
               at (08,78), fac(lfac$( 2)),  base$  (b%+02)       ,ch(03),~
               at (09,78), fac(lfac$( 3)),  base$  (b%+03)       ,ch(03),~
               at (10,78), fac(lfac$( 4)),  base$  (b%+04)       ,ch(03),~
               at (11,78), fac(lfac$( 5)),  base$  (b%+05)       ,ch(03),~
               at (12,78), fac(lfac$( 6)),  base$  (b%+06)       ,ch(03),~
               at (13,78), fac(lfac$( 7)),  base$  (b%+07)       ,ch(03),~
               at (14,78), fac(lfac$( 8)),  base$  (b%+08)       ,ch(03),~
               at (15,78), fac(lfac$( 9)),  base$  (b%+09)       ,ch(03),~
               at (16,78), fac(lfac$(10)),  base$  (b%+10)       ,ch(03),~
               at (17,78), fac(lfac$(11)),  base$  (b%+11)       ,ch(03),~
               at (18,78), fac(lfac$(12)),  base$  (b%+12)       ,ch(03),~
               at (19,78), fac(lfac$(13)),  base$  (b%+13)       ,ch(03),~
               at (20,78), fac(lfac$(14)),  base$  (b%+14)       ,ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$                ,ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1)             ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2)             ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3)             ,ch(79),~
                                                                         ~
               keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13 then L45660
                  call "MANUAL" ("FRDEFINE")
                  goto L44400

L45660:        if keyhit% <> 15 then L45700
                  call "PRNTSCRN"
                  goto L44400

L45700:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                    S C R E E N  III                       *~
            *                                                           *~
            * Handles print range selection.                            *~
            *************************************************************

            REM Input Mode Screen Controler
            deffn'103(fieldnr%)
                  pfdescr$(1) ="(1)Start Over                          "&~
                               "                        (13)Instructions"
                  pfdescr$(2) ="                                       "&~
                               "                        (15)Print Screen"
                  pfdescr$(3) ="                                       "&~
                               "                        (16)CANCEL Print"
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  str(pfdescr$(3),63,1)=hex(84) /* Make Sure They See */
                  header1$ = "Print Format Control Records"
                  str(header1$,62) = "FRDEFINE: " & str(cms2v$,,8)
                  on fieldnr% gosub L46240,         /* START REPORT     */~
                                    L46240          /* END REPORT       */
                     goto L46310

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L46240:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L46310:     accept                                                       ~
               at (01,02), "Financial Statements Report Specifications", ~
               at (01,67), "Date:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), header1$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Starting Report Id.",                        ~
               at (06,30), fac(lfac$( 1)), startcode$           , ch(03),~
               at (07,02), "Ending Report Id.",                          ~
               at (07,30), fac(lfac$( 2)), endcode$             , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13 then L46540
                  call "MANUAL" ("GLGRPINP")
                  goto L46310

L46540:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L46310

        REM *************************************************************~
            *                    S C R E E N  IIII                      *~
            *                                                           *~
            * Handles Copy Function.                                    *~
            *************************************************************

            REM Input Mode Screen Controler
            deffn'104(fieldnr%)
                  pfdescr$(1) ="(1)Start Over                          "&~
                               "                        (13)Instructions"
                  pfdescr$(2) ="                                       "&~
                               "                        (15)Print Screen"
                  pfdescr$(3) ="                                       "&~
                               "                        (16)CANCEL Copy "
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  str(pfdescr$(3),63,1)=hex(84) /* Make Sure They See */
                  header1$ = "Copy Format Control File"
                  str(header1$,62) = "FRDEFINE: " & str(cms2v$,,8)
                  on fieldnr% gosub L47240,         /* COPY FROM        */~
                                    L47240          /* COPY TO          */
                     goto L47310

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L47240:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L47310:     accept                                                       ~
               at (01,02), "Financial Statements Report Specifications", ~
               at (01,67), "Date:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), header1$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Report To Be Copied",                        ~
               at (06,30), fac(lfac$( 1)), startcode$           , ch(03),~
               at (06,35), fac(hex(84)),   display$(1)          , ch(32),~
               at (07,02), "Id. For New Report",                         ~
               at (07,30), fac(lfac$( 2)), endcode$             , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13 then L47550
                  call "MANUAL" ("GLGRPINP")
                  goto L47310

L47550:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L47310

        REM *************************************************************~
            *                S H O W   L E G E N D                      *~
            *                                                           *~
            * Shows Some Summary Documentation From Quick Referencing.  *~
            *************************************************************

        show_legend
        accept                                                           ~
               at (01,03), "                          INSTRUCTION CODES L~
        ~EGEND",                                                          ~
               at (02,03), "Instruction:  H   - Set/Change Header Subtitl~
        ~e.",                                                             ~
               at (03,03), "   Codes   :  P   - Start New Page, Overridin~
        ~g Automatic Form Control.",                                      ~
               at (04,03), "           :  D   - Print Specified Text As A~
        ~ Subtitle In Body Of Report.",                                   ~
               at (05,03), "           : U,DU - Print (Double) Underlines~
        ~ In Numeric Column(s).",                                         ~
               at (06,03), "           :  CB  - Accumulate Current Balanc~
        ~e Of Specified Accounts.",                                       ~
               at (07,03), "           :  PA  - Accumulate Period Activit~
        ~y Of Specified Accounts.",                                       ~
               at (08,03), "           :  PO  - Accumulate Period Opening~
        ~ Balances Of Specified Accounts.",                               ~
               at (09,03), "           :  YO  - Accumulate F.Y. Opening B~
        ~alances Of Specified Accounts.",                                 ~
               at (10,03), "           :  YA  - Accumulate F.Y.T.D. Activ~
        ~ity Of Specified Accounts.",                                     ~
               at (11,03), "           :  T0 to T9 - Manipulate Respectiv~
        ~e Totaling Bucket.",                                             ~
               at (13,03), "Print      :  NO  - This Format Line Is NOT t~
        ~o Generate A Report Line.",                                      ~
               at (14,03), "Options    :  SUM - Print The Sum Of All Indi~
        ~cated Account As ONE Line On.",                                  ~
               at (15,03), "           :        The Report. Text Specifie~
        ~d Will Show As The Description.",                                ~
               at (16,03), "           :  ALL - Print Each Of The Indicat~
        ~ed Account As An Individual Line",                               ~
               at (17,03), "                    On The Report, Showing Ea~
        ~ch Accounts Description.",                                       ~
               at (19,03), "Print      :  '+' - Completely Ignore Line(s)~
        ~ When Not Positive (No Totaling)",                               ~
               at (20,03), "Restiction :  '-' - Completely Ignore Line(s)~
        ~ When Not Negative (No Totaling)",                               ~
               at (21,03), "Codes      :  ' ' - (blank) Ignore This Featu~
        ~re.",                                                            ~
               at (23,03), "Totaling   :  '+' Adds to Bucket, '-' Subtrac~
        ~ts From Bucket, '0' zeros Bucket",                               ~
               at (24,29), "Press (ENTER) To Return",                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (mary%)

               if mary% <> 13 then L48570
                  call "MANUAL" ("GLGRPINP")
                  goto show_legend

L48570:        if mary% <> 15 then return
                  call "PRNTSCRN"
                  goto show_legend

        REM *************************************************************~
            *                    S E T   K E Y S                        *~
            *                                                           *~
            * Sets PF Keys & Descriptions Based On SCREEN%...           *~
            *************************************************************

        set_keys                 /* Cleansliness Is Next To Godliness? */
            on screen% goto L49075,         /* Header, Input Mode       */~
                            L49180,         /* Header, Edit Mode        */~
                            L49225,         /* Line Input Mode          */~
                            L49325,         /* Line Scrn, Edit Mode     */~
                            L49415,         /* Summary, Display Mode    */~
                            L49505,         /* Summary, Delete Mode     */~
                            L49545          /* All Scrns, Edit Field    */

L49075: REM  =-=-=-=-=-=-=-=-=-= Header, Input mode =-=-=-=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)Start Over    (4)Previous Field     (10)Cop~
        ~y Report         (13)Instructions"
            pfdescr$(2) = "                 (9)Print Formats             ~
        ~                 (15)Print Screen"
            pfdescr$(3) = "                                              ~
        ~                 (16)Exit Program"
            pfkeys$ = hex(000104090a0d0f10ffffffffffffffff)
            str(pfdescr$(3),63,1) = hex(84)

*        Flip Off Appropriate Fields
            if fieldnr% = 1 then L49160
                str(pfdescr$(3),63)    = " "    /* Shut Off Exit Optn  */
                str(pfkeys$,8,1) = hex(ff)
                str(pfdescr$(2),18,16) = " "    /* Shut Off Print Optn */
                str(pfkeys$,4,1) = hex(ff)
                str(pfdescr$(1),40,15) = " "    /* Shut Off Copy Option*/
                str(pfkeys$,5,1) = hex(ff)
                goto L49170
L49160:     str(pfdescr$(1),,35) = " "          /* Shut Off Prev Field */
            str(pfkeys$,2,2) = hex(ffff)
L49170: return

L49180: REM =-=-=-=-=-=-=-=-=-= Header, Edit mode =-=-=-=-=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)Start Over                                 ~
        ~                 (13)Instructions"
            pfdescr$(2) = "(2)Review/Edit Report Detail                  ~
        ~                 (15)Print Screen"
            pfdescr$(3) = "                                              ~
        ~                 (16)SAVE Data   "
            pfkeys$ = hex(0001020d0f10ffffffffffffffffffff)
            str(pfdescr$(3),63,1) = hex(84)
        return

L49225: REM =-=-=-=-=-=-=-=-= Line Items, Input mode =-=-=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)Start Over    (4)Previous Field            ~
        ~                 (13)Instructions"
            pfdescr$(2) = "(2)Restart Line  (6)Same As Prev Line  (14)Leg~
        ~end              (15)Print Screen"
            pfdescr$(3) = " "
            pfkeys$ = hex(00010204060d0e0fffffffffffffffff)

*        Flip Off Appropriate Fields
            if c% > 1 then L49280
                str(pfdescr$(2),18,20) = " "    /* Shut Off Prev Line  */
                str(pfkeys$,5,1) = hex(ff)
L49280:     if fieldnr% <> 1 then L49315
                str(pfdescr$(1),18,17) = " "    /* Shut Off Prev Field */
                str(pfkeys$,4,1) = hex(ff)
                str(pfdescr$(3),63) = hex(84) & "(16)EDIT Mode"
                str(pfkeys$,9,1) = hex(10)
                if insert% = 0 then L49315
                    str(pfdescr$(3),63) = hex(84) & "(16)To SUMMARY"
L49315:     return

L49325: REM =-=-=-=-=-=-=-=-= Line Items, Edit mode =-=-=-=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)Start Over    (4/6)Previous Line           ~
        ~                 (13)Instructions"
            pfdescr$(2) = "(2)First Line    (5/7)Next Line        (14)Leg~
        ~end              (15)Print Screen"
            pfdescr$(3) = "(3)Last Line                                  ~
        ~                 (16)To SUMMARY  "
            pfkeys$ = hex(000102040506070d0e0f1003ffffffff)
            str(pfdescr$(3),63,1) = hex(84)

*        Flip Off Appropriate Fields
            if c% > 1 then L49390
                str(pfdescr$(1),18,18)   = " "   /* Shut Off Prev Line */
                str(pfdescr$(2),,13)     = " "  /* Shut Off First Line */
                str(pfkeys$,3,2), str(pfkeys$,6,1) = hex(ffff)
L49390:     if c% < maxlines% then L49405
                str(pfdescr$(2),18,14)   = " "   /* Shut Off Next Line */
                str(pfkeys$,5,1), str(pfkeys$,7,1) = hex(ff)
                str(pfdescr$(3),,12) = " "   /* Shut Off Last Line */
                str(pfkeys$,12,1) = hex(ff)
L49405:     return

L49415: REM =-=-=-=-=-=-=- Summary Screen, Display mode -=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)Start Over  (4/6)Prev Lines   (9)Header    ~
        ~                 (13)Instructions"
            pfdescr$(2) = "(2)First Line  (5/7)Next Lines  (11)Add Line  ~
        ~                 (15)Print Screen"
            pfdescr$(3) = "(3)Last Line                    (12)DELETE Lin~
        ~e                (16)SAVE Data"
            pfkeys$ = hex(00010204050607090b0c0d0e0f1003ffff)
            str(pfdescr$(3),63,1) = hex(84)

*        Flip Off Appropriate Fields
            if b% > 0 then L49480
                str(pfdescr$(2),,13)   = " "    /* Shut Off First Line */
                str(pfdescr$(1),16,15) = " "    /* Shut Off Prev Scrn  */
                str(pfkeys$,3,2) = hex(ffff)
L49480:     if b%+14 < maxlines% then return
                str(pfdescr$(2),16,15) = " "    /* Shut Off Next Scrn  */
                str(pfkeys$,5,1) = hex(ff)
                str(pfdescr$(3),,12) = " "    /* Shut Off Last Line  */
                str(pfkeys$,15,1) = hex(ff)
            return

L49505: REM =-=-=-=-=-=-=- Summary Screen, Delete mode -=-=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)CANCEL Delete Request                      ~
        ~                 (13)Instructions"
            pfdescr$(2) = "(ENTER)DELETE The Flashing Line               ~
        ~                 (15)Print Screen"
            pfdescr$(3) = " "
            pfkeys$ = hex(00010d0fffffffffffffffffffffffff)
        return

L49545: REM =-=-=-=-=-=-=-= All Screens, Field Edit =-=-=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)Start Over                                 ~
        ~                 (13)Instructions"
            pfdescr$(2) = "(ENTER) Validate Modification(s)              ~
        ~                 (15)Print Screen"
            pfdescr$(3) = " "
            pfkeys$ = hex(00010d0e0fffffffffffffffffffffff)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50160,         /* REPORT NUMBER    */~
                                    L50248,         /* DESCRIPTION      */~
                                    L50280,         /* Set of books     */~
                                    L50392,         /* DEFAULT TITLE    */~
                                    L50416,         /* DEFAULT PAREN    */~
                                    L50488,         /* ROUND?           */~
                                    L50560,         /* PRINT IF ZERO?   */~
                                    L50632,         /* ZONE ONE         */~
                                    L50632,         /* ZONE TWO         */~
                                    L50632          /* ZONE THREE       */
                     return

L50160:     REM TEST DATA FOR REPORT NUMBER
                if report$ <> " " then goto L50208
                call "GETCODE" (#01, report$, " ", 0%, 0, f1%(1))
                if f1%(1) <> 0 then L50208
                    errormsg$ = hex(00)
                    return
L50208:         gosub L30000
                if onfile% = 0 then return
                return clear all
                goto editmode

L50248:     REM TEST DATA FOR DESCRIPTION
                if descr$ = " " then errormsg$ = "Field Can't Be Blank"
                return

L50280: REM G/L system to use SET$
            if dual_books$ = "Y" then goto L50304
L50301:         set$, setdescr$ = " " : mast% = 3% : set = 1 : return
L50304:     call "NUMTEST" (set$, 1, 2, errormsg$, 0, set)
            if errormsg$ <> " " then return
        describe_set_of_books
            if dual_books$ <> "Y" then L50301
            setdescr$ = "Statutory"
            mast% = 3%
            if set$ = "1" then goto L50368
                mast% = 4%
                setdescr$ = "Local Authority"
L50368:     call "PUTPAREN" (setdescr$)
            return

L50392:     REM TEST DATA FOR DEFAULT REPORT TITLE
                return

L50416:     REM TEST DATA FOR DEFAULT PARENTHESIS OPTION
                if paren$ = " " then return
                if str(paren$,,1) = "Y" then paren$ = "YES"
                if str(paren$,,1) = "N" then paren$ = "NO"
                if paren$ = "YES" then return
                if paren$ = "NO " then return
                errormsg$ = "Please Enter 'YES','NO' Or Blank: " & paren$
                return

L50488:     REM TEST DATA FOR DEFAULT ROUND OPTION
                if round$ = " " then return
                if str(round$,,1) = "Y" then round$ = "YES"
                if str(round$,,1) = "N" then round$ = "NO"
                if round$ = "YES" then return
                if round$ = "NO " then return
                errormsg$ = "Please Enter 'YES','NO' Or Blank: " & round$
                return

L50560:     REM TEST DATA FOR DEFAULT ZERO OPTION
                if zero$ = " " then return
                if str(zero$,,1) = "Y" then zero$ = "YES"
                if str(zero$,,1) = "N" then zero$ = "NO"
                if zero$ = "YES" then return
                if zero$ = "NO " then return
                errormsg$ = "Please Enter 'YES','NO' Or Blank: " & zero$
                return

L50632:     REM TEST DATA FOR ZONE ONE SPECS
                k% = fieldnr% - 7
                if zonename$(k%) = " " and zone$(k%,1) = " " and         ~
                                           zone$(k%,2) = " " then return
                convert zone$(k%,1) to u3%, data goto L50760
                if u3% < 1 or u3% > 12 then L50760
                convert u3% to zone$(k%,1), pic(##)

                convert zone$(k%,2) to u4%, data goto L50792
                if u4% > 13-u3% or u4% < 1 then L50792
                convert u4% to zone$(k%,2), pic(##)

                if zonename$(k%) = " " then errormsg$ = "Description Of Z~
        ~one Can't Be Blank"
                return

L50760:         errormsg$ = "Start Position In Account Number Must Be Bet~
        ~ween 1 And 12."
                return

L50792:         u4% = 13-u3% : convert u4% to temp$, pic(##)
                errormsg$ = "Length Of Zone In Account Number Must Be Bet~
        ~ween 1 And "& str(temp$,,2) & "."
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2. (Labor Detail)        *~
            *************************************************************

            deffn'152(test%)
            errormsg$ = " "
                  on test%    gosub L51230,         /* INSTRUCTION      */~
                                    L51350,         /* TOTAL TYPE       */~
                                    L51450,         /* COMPANY CODE     */~
                                    L51480,         /* ACCOUNTS         */~
                                    L51730,         /* ZONE OVERRIDE    */~
                                    L51790,         /* PRINT?           */~
                                    L51930,         /* PRINT RESTRICTION*/~
                                    L51970,         /* PRINT COLUMN     */~
                                    L52020,         /* REVERSE SIGNS    */~
                                    L52090,         /* TEXT FOR PRINT   */~
                                    L52110,         /* % BASE LINE      */~
                                    L52260,         /* MEMO TEXT        */~
                                    L52280          /* TOTALING FLAGS   */
                     return

L51230:     REM TEST DATA FOR INSTRUCTION CODE
            instdescr$ = " "
            search inst_codes$() = inst$(c%) to cursor%() step 2
                if cursor%(1) > 0 then L51300
                errormsg$ = "Invalid Instruction Code: " & inst$(c%)
                if inst$(c%) = " " then errormsg$ = hex(00)
                return
L51300:     instdescr$ = inst_names$((cursor%(1)+1)/2)
            totaltext$ = " "
            if str(inst$(c%),,1) = "T" then totaltext$="Total Type Inst:"
            return

L51350:     REM TEST DATA FOR TOTAL TYPE CODE
                if enabled% = 0 then return
                if type$(c%) = " " then type$(c%) = "CB"
                if type$(c%) = "CB" or type$(c%) = "PA" then return
                if type$(c%) = "PO" or type$(c%) = "YO" then return
                if type$(c%) = "YA" then return
                errormsg$ = "For Balance Sheet Only, Enter PO, PA, CB, YO~
        ~, Or YA"
                return

L51450:     REM TEST DATA FOR COMPANY CODE
                return

L51480:     REM TEST DATA FOR ACCOUNT SPECIFICATIONS
            if inst$(c%) <> "CB" and inst$(c%) <> "PA" and               ~
               inst$(c%) <> "PO" and str(inst$(c%),,1) <> "Y" then return
            if acct1$(c%) = " " and acct2$(c%) = " " then L51600
                REM Test Account Number Range...
                if group$(c%) <> " " then L51690
            if dual_books$ = "Y" and set$ = "2" then L51531
                call "GLVALID" (acct1$(c%), "         ", errormsg$)
                     goto L51532
L51531:         call "GLVALD2" (acct1$(c%), "         ", errormsg$)
L51532:              if errormsg$ <> " " then return
                if acct2$(c%) = " " then return
            if dual_books$ = "Y" and set$ = "2" then L51541
                call "GLVALID" (acct2$(c%), "         ", errormsg$)
                     goto L51542
L51541:         call "GLVALD2" (acct2$(c%), "         ", errormsg$)
L51542:              if errormsg$ <> " " then return
                if acct1$(c%) <= acct2$(c%) then return
                errormsg$ = "Starting Account Can't Be Greater Than " &  ~
                            "The Ending Account"
                return

L51600:     REM Test Group Code...
            call "GETCODE" (#mast%, group$(c%), groupdescr$, 1%, 1,      ~
                f1%(mast%))
                if f1%(mast%) = 1 then L51660
                errormsg$ = "Please Enter Either A Group Code Or " &     ~
                            "An Account Range"
                return
L51660:     get #mast%, using L51670, text$(c%)
L51670:     FMT XX(36), CH(45)
            return
L51690:         errormsg$ = "Please Enter Either A Group Code Or " &     ~
                            "An Account Range... Not Both."
                return

L51730:     REM TEST DATA FOR OVERRIDE ZONING?
            if str(fixed$(c%),,1) = "Y" then fixed$(c%) = "YES"
            if str(fixed$(c%),,1) = "N" then fixed$(c%) = "NO "
            if fixed$(c%) = "YES" then return
            if fixed$(c%) = "NO " then return
            if fixed$(c%) = " " then return
                errormsg$ = "Please Enter 'YES' Or 'NO'"
                return
L51790:     REM TEST DATA FOR PRINT OPTION
            if print$(c%) = "NO " then type$(c%) = " "
            if print$(c%) = "NO " then return
            if print$(c%) = "ALL" then return
            if print$(c%) = "SUM" then return
            if print$(c%) <> "YES" then L51910
            if acct2$(c%) <> " " then L51890  /* YES is invalid resonse */
            if group$(c%) <> " " then L51890  /* For These Conditions   */
                if print$(c%) <> "NO" then print$(c%) = "YES"
                return
L51890:     if inst$(c%) <> "CB" and inst$(c%) <> "PA" and               ~
               inst$(c%) <> "PO" and str(inst$(c%),,1) <> "Y" then return
L51910:         errormsg$ = "Please Enter 'SUM', 'ALL', Or 'NO'"
                return
L51930:     REM TEST DATA FOR PRINT OVERRIDE OPTIONS
            if pos(" +-" = print_if$(c%)) = 0 then errormsg$ =           ~
                  "Please Enter '+', '-', Or Blank"
                return
L51970:     REM TEST DATA FOR PRINT COLUMN
            if col$(c%) = " " then col$(c%) = "1"
            if pos("123" = col$(c%)) = 0 then errormsg$ =                ~
                  "Please Enter '1', '2', Or '3': " & col$(c%)
                return
L52020:     REM TEST DATA FOR REVERSE SIGN INDICATOR
            if reverse$(c%)=" " and enabled%=1 then reverse$(c%)="NO"
            if str(reverse$(c%),,1) = "Y" then reverse$(c%) = "YES"
            if str(reverse$(c%),,1) = "N" then reverse$(c%) = "NO "
            if reverse$(c%) = " " then return
            if reverse$(c%) = "YES" then return
            if reverse$(c%) = "NO " then return
                errormsg$ = "Please Enter 'YES' Or 'NO'"
                return
L52090:     REM TEST DATA FOR TEXT FOR PRINT
                return
L52110:     REM TEST DATA FOR BASE LINE FOR PERCENTS
                if base$(c%) = " " then return
                convert base$(c%) to temp%, data goto L52210
                if temp% < 1 or temp% > maxlines% then L52210
                search "T0T1T2T3T4T5T6T7T8T9CBPAPOYOYA" = inst$(temp%) to~
                                                    cursor%() step 2
                if cursor%(1) = 0 then L52230
                if print$(temp%) = "ALL" then L52230
                convert temp% to base$(c%), pic(###)
                return
L52210:         errormsg$ = "Illegal Entry For Percentage Base Line"
                return
L52230:         errormsg$ = "Only A Format Line That Derives A (One) Amou~
        ~nt/Line Can Be Used As A Base Line"
                return
L52260:     REM TEST DATA FOR MEMO TEXT
                return
L52280:     REM TEST DATA FOR TOTALING SPEC
                for i% = 1 to 10
                     if pos(" +-0" = str(total$(c%),i%,1)) > 0 then L52340
                     errormsg$ = "Total Control Specifier Must Be '+'" & ~
                          ", '-', '0', Or Blank: " & str(total$(c%),i%,1)
                     return
L52340:         next i%
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 3.                       *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L53110,         /* STARTING REPORT  */~
                                    L53140          /* ENDING REPORT    */
                     return
L53110:     REM TEST DATA FOR STARTING CODE
            return

L53140:     REM TEST DATA FOR ENDING CODE
            if endcode$ = " " then return
            if startcode$ > endcode$ then errormsg$ = "Ending Report Id. ~
        ~Can't Be Greater Than The Starting Report Id."
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 4.                       *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L54110,         /* COPY FROM        */~
                                    L54140          /* COPY TO          */
                     return
L54110:     REM TEST DATA FOR REPORT TO BE COPIED
            call "GETCODE" (#01, startcode$, str(display$(1),,30), 1%, 0,~
                f1%(1))
            if f1%(1) = 0 then errormsg$ = hex(00)
            return

L54140:     REM TEST DATA FOR NEW REPORT ID.
            call "DESCRIBE" (#01, endcode$, " ", 0%, f1%(1))
                if f1%(1) <> 0 then errormsg$ = "Can't Be The Same As An ~
        ~existing Report: " & endcode$
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")
            end