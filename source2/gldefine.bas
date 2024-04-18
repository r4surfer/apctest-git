        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGGG  L      DDDD   EEEEE  FFFFF  IIIII  N   N  EEEEE   *~
            *  G      L      D   D  E      F        I    NN  N  E       *~
            *  G  GG  L      D   D  EEE    FFF      I    N N N  EEE     *~
            *  G   G  L      D   D  E      F        I    N  NN  E       *~
            *   GGG   LLLLL  DDDD   EEEEE  F      IIIII  N   N  EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLDEFINE - Manage G/L report specs, used by the Financial *~
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
            * 08/10/87 ! ORIGINAL                                 ! RAC *~
            * 12/07/89 ! Added Dual Books functionality, Report   ! MLJ *~
            *          !  deletion capability, expanded legend &  !     *~
            *          !  numerous other fixes.                   !     *~
            * 01/11/90 ! Minor cosmetic "tweeks"                  ! MLJ *~
            * 06/20/90 ! Fixed to allow input/edit of column print! MLJ *~
            *          !  option, can also edit print option on   !     *~
            *          !  header record 1.                        !     *~
            * 06/29/90 ! Fix'd conversion error when using 'N'    ! MLJ *~
            *          !  inst codes, brought up to display std's !     *~
            *          !  when using defaults, corrected spelling.!     *~
            * 10/22/90 ! Moved large arrays to INIT statement     ! MJB *~
            * 10/31/91 ! PRR 12047  Added Prev/Next Year Budgets. ! MLJ *~
            * 01/07/92 ! QC Mods - "SUM" now defaults account #'s,! MLJ *~
            *          !   Corrected acct defaults following "N"  !     *~
            *          !   calculation instruction codes.         !     *~
            * 02/11/93 ! PRR 11616 - Added GLMAIN and GLMAIN2 for ! MLJ *~
            *          !   edit/selection of account codes.       !     *~
            *          ! MISC - Can now input & edit all Zone     !     *~
            *          !   fields, modified default/enables to    !     *~
            *          !   eliminate redundancy, added dummy reads!     *~
            *          !   on #3 & #4 (#MAST%), #5 & #6 (#GL%) for!     *~
            *          !   SES, mods to error messages for program!     *~
            *          !   consistency, fixed numerous implied    !     *~
            *          !   integers.                              !     *~
            * 04/06/93 !Corrected edit of Nx specs when using T0  ! MLJ *~
            *          !   thru T19.                              !     *~
            * 04/13/93 !Clarified message display when deleting   ! MLJ *~
            *          !   report formats.                        !     *~
            * 11/02/93 !MAST% & GL% channels now properly assigned! MLJ *~
            * 04/12/95 ! Chngd testing against 'C' accounts to    ! JDH *~
            *          !   numeric instructions in acct defaults. !     *~
            * 05/23/95 ! Chngd SHOWMSG to SHOSTAT.                ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**~

        dim                                                              ~
            acct1$(320,10)16,            /* FROM ACCOUNT               */~
            acct2$(320,10)16,            /* TO ACCOUNT                 */~
            acctdescr$32,                /* ACCOUNT CODE DESCRIPTION   */~
            acctthru$(320,10)1,          /* THRU INDICATOR             */~
            col$(10)3,                   /* COLUMN INDICATOR           */~
            colcom$(10)3,                /* COLUMN COMMA INDICATOR     */~
            colfmt$(10)1,                /* COLUMN FORMAT              */~
            collit$7,                    /* COLUMN DISPLAY LITERAL     */~
            collgth$(10)2,               /* COLUMN LENGTH              */~
            collgth%(10),                /* Numeric Column Length      */~
            colpos$(10)3,                /* COLUMN POSITION            */~
            colpos%(10),                 /* Numeric Column Position    */~
            coltext$45,                  /* DESCRIPTION COLUMN TEXT    */~
            columns$2,                   /* NUMBER OF COLUMNS          */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr$30,                    /* REPORT DESCRIPTION         */~
            display$(3)79,               /* LINE ITMES FOR EDIT DISPLAY*/~
            dual_books$1,                /* DUAL BOOKS USAGE FLAG      */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filler$12,                   /* FILLER AT END OF RECORD    */~
            fixed$(320,10)3,             /* RUN TIME OVERRIDE FLAG     */~
            group$(320,10)6,             /* ACCOUNT GROUPING CODE      */~
            groupdescr$32,               /* ACCOUNT GROUPING CODE DESCR*/~
            header$79,                   /* HEADER FOR SCREEN          */~
            header1$79,                  /* HEADER FOR SCREEN          */~
            hfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inst$(320,10)4,              /* INSTRUCTION CODE           */~
            inst_codes$(57)4,            /* VALID INSTRUCTIONS         */~
            inst_names$(57)30,           /* VALID INSTRUCTIONS NAMES   */~
            instdescr$30,                /* FOR DISPLAY                */~
            length_msg$79,               /* CAUTION MSG FOR COLUMNS    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line$(320)4,                 /* LINE NUMBER FOR REFERENCE  */~
            message$79,                  /* INPUT MESSAGE              */~
            paren$3,                     /* DEFAUT PARENTHESIS OPTION  */~
            pfdescr$(2)79,               /* DESCRIPTION OF PFKEYS      */~
            print$(320,10)3,             /* PRINT CODE                 */~
            print_if$(320,10)1,          /* PRINT/TOTAL CONSTRAINT FLAG*/~
            range$(14)25,                /* FOR SUMMARY DISPLAY SCREEN */~
            readkey$60,                  /* WORK VARIABLE              */~
            report$3,                    /* REPORT ID.                 */~
            reverse$(320,10)3,           /* REVERSE SIGN INDICATOR     */~
            round$3,                     /* ROUND OPTION (DEFAULT)     */~
            setmsg$26,                   /* SCREEN MESSAGE FOR BOOKS   */~
            set$1,                       /* SET OF BOOKS TO USE        */~
            setdescr$30,                 /* BOOK DESCRIPTION           */~
            text$(320,10)45,             /* TEXT FOR LINE ITEMS        */~
            text1$(10)16,                /* TEXT FOR COLUMNS LINE 1    */~
            text2$(10)16,                /* TEXT FOR COLUMNS LINE 2    */~
            title$50,                    /* DEFAULT REPORT TITLE       */~
            total$(320,10)20,            /* TOTALING SPEC FOR LINE     */~
            zero$3,                      /* PRINT IF ZERO? (DEFAULT)   */~
            zone$(4,2)2,                 /* START, LENGTH IN ACCOUNT   */~
            zonename$(4)20               /* LABLE FOR ZONE IN ACCOUNT  */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            fs%(64),                     /* FILE STATUS FLAGS          */~
            rslt$(64)20                  /* TEST FROM FILE OPEN        */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch finalization of R6.04.00  "
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
            * # 1 ! GLNAMES  ! Financial Statement Header File          *~
            * # 2 ! GLFORMAT ! Financial Statement Format File          *~
            * # 3 ! FRGRPMAS ! G/L Grouping Codes File                  *~
            * # 4 ! FRGRPMA2 ! G/L Group Codes (local Authority)        *~
            * # 5 ! GLMAIN   ! General Ledger Main File                 *~
            * # 6 ! GLMAIN2  ! G. L. chart of accounts for local auth.  *~
            * #10 ! SYSFILE2 ! System Information                       *~
            *************************************************************

            select #1,  "GLNAMES",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 3

            select #2,  "GLFORMAT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1400,                                  ~
                        keypos = 1, keylen = 9

            select #3,  "FRGRPMAS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 6,                          ~
                        alt key 1, keypos = 7, keylen = 30, dup

            select #4,  "FRGRPMA2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 1, keylen = 6,                          ~
                        alt key 1, keypos = 7, keylen = 30, dup

            select #5,  "GLMAIN",                                        ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 9

            select #6,  "GLMAIN2",                                       ~
                        varc, indexed, recsize = 300,                    ~
                        keypos = 1, keylen = 9

            select #10, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            call "SHOSTAT" ("Opening data files, one moment please.")

            call "OPENCHCK" (#1,  fs%( 1%), f2%( 1%), 100%, rslt$( 1%))
            call "OPENCHCK" (#2,  fs%( 2%), f2%( 2%), 100%, rslt$( 2%))
            call "OPENCHCK" (#3,  fs%( 3%), f2%( 3%),   0%, rslt$( 3%))
            call "OPENCHCK" (#5,  fs%( 5%), f2%( 5%),   0%, rslt$( 5%))
            call "OPENCHCK" (#6,  fs%( 6%), f2%( 6%),   0%, rslt$( 6%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),   0%, rslt$(10%))

        REM Dummy Reads for SES Documentor...
            readkey$ = all(hex(00))
                call "READ104" (#3, readkey$, f1%(3%))
            readkey$ = all(hex(00))
                call "READ104" (#4, readkey$, f1%(4%))
            readkey$ = all(hex(00))
                call "READ104" (#5, readkey$, f1%(5%))
            readkey$ = all(hex(00))
                call "READ104" (#6, readkey$, f1%(6%))

            dual_books$ = "N"
              call "READ100" (#10, "SWITCHS.GL", f1%(10%))
              if f1%(10%) = 0% then L09000
                  get #10 using L02400, dual_books$
L02400:               FMT POS(21), CH(1)
              if dual_books$ <> "Y" then L09000
                  call "OPENCHCK" (#4, fs%(4%), f2%(4%), 0%, rslt$(4%))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            if dual_books$ = "Y" then                                    ~
                setmsg$ = "G/L System to use (1 or 2)"
            date$ = date
            call "DATEFMT" (date$)
            length_msg$= "CAUTION:  'LENGTH' dictates length of column la~
        ~bel and contents."
            collit$ = "COLUMN:"

            header$ = "     Inst     Accounts/Misc Info     Print? Text (~
        ~if applicable)               "

            for i% = 1 to 320%
                convert i% to line$(i%), pic(###)
                str(line$(i%),4%) = ")"
            next i%

*        Valid instructions codes & descriptions...
            inst_codes$(01%) = "H   "
            inst_codes$(02%) = "P   "
            inst_codes$(03%) = "D   "
            inst_codes$(04%) = "DC  "
            inst_codes$(05%) = "U   "
            inst_codes$(06%) = "DU  "
            inst_codes$(07%) = "PA  "
            inst_codes$(08%) = "CB  "
            inst_codes$(09%) = "PO  "
            inst_codes$(10%) = "YO  "
            inst_codes$(11%) = "YA  "
            inst_codes$(12%) = "T0  "
            inst_codes$(13%) = "T1  "
            inst_codes$(14%) = "T2  "
            inst_codes$(15%) = "T3  "
            inst_codes$(16%) = "T4  "
            inst_codes$(17%) = "T5  "
            inst_codes$(18%) = "T6  "
            inst_codes$(19%) = "T7  "
            inst_codes$(20%) = "T8  "
            inst_codes$(21%) = "T9  "
            inst_codes$(22%) = "T10 "
            inst_codes$(23%) = "T11 "
            inst_codes$(24%) = "T12 "
            inst_codes$(25%) = "T13 "
            inst_codes$(26%) = "T14 "
            inst_codes$(27%) = "T15 "
            inst_codes$(28%) = "T16 "
            inst_codes$(29%) = "T17 "
            inst_codes$(30%) = "T18 "
            inst_codes$(31%) = "T19 "
            inst_codes$(32%) = "PPO "
            inst_codes$(33%) = "PPA "
            inst_codes$(34%) = "LCB "
            inst_codes$(35%) = "LYO "
            inst_codes$(36%) = "LYA "
            inst_codes$(37%) = "LPO "
            inst_codes$(38%) = "LPA "
            inst_codes$(39%) = "BCB "
            inst_codes$(40%) = "BYA "
            inst_codes$(41%) = "BPO "
            inst_codes$(42%) = "BPA "
            inst_codes$(43%) = "BYE "
            inst_codes$(44%) = "N+  "
            inst_codes$(45%) = "N-  "
            inst_codes$(46%) = "N/  "
            inst_codes$(47%) = "N*  "
            inst_codes$(48%) = "N%  "
            inst_codes$(48%) = "N%  "
            inst_codes$(49%) = "PBPO"
            inst_codes$(50%) = "PBPA"
            inst_codes$(51%) = "PBYA"
            inst_codes$(52%) = "PBCB"
            inst_codes$(53%) = "PBYE"
            inst_codes$(54%) = "NBPO"
            inst_codes$(55%) = "NBPA"
            inst_codes$(56%) = "NBYA"
            inst_codes$(57%) = "NBCB"

            inst_names$(01%) = "Set Header Message            "
            inst_names$(02%) = "New Page; Print Header        "
            inst_names$(03%) = "Print Sub Title               "
            inst_names$(04%) = "Print Sub Title Centered      "
            inst_names$(05%) = "Print Underlines              "
            inst_names$(06%) = "Print Double Underlines       "
            inst_names$(07%) = "Curr Period Ativity           "
            inst_names$(08%) = "Curr Period Current Balance   "
            inst_names$(09%) = "Curr Period Opening Balance   "
            inst_names$(10%) = "Curr Year Opening Balance     "
            inst_names$(11%) = "Curr Year Activiy             "
            inst_names$(12%) = "Manage Total Bucket 0         "
            inst_names$(13%) = "Manage Total Bucket 1         "
            inst_names$(14%) = "Manage Total Bucket 2         "
            inst_names$(15%) = "Manage Total Bucket 3         "
            inst_names$(16%) = "Manage Total Bucket 4         "
            inst_names$(17%) = "Manage Total Bucket 5         "
            inst_names$(18%) = "Manage Total Bucket 6         "
            inst_names$(19%) = "Manage Total Bucket 7         "
            inst_names$(20%) = "Manage Total Bucket 8         "
            inst_names$(21%) = "Manage Total Bucket 9         "
            inst_names$(22%) = "Manage Total Bucket 10        "
            inst_names$(23%) = "Manage Total Bucket 11        "
            inst_names$(24%) = "Manage Total Bucket 12        "
            inst_names$(25%) = "Manage Total Bucket 13        "
            inst_names$(26%) = "Manage Total Bucket 14        "
            inst_names$(27%) = "Manage Total Bucket 15        "
            inst_names$(28%) = "Manage Total Bucket 16        "
            inst_names$(29%) = "Manage Total Bucket 17        "
            inst_names$(30%) = "Manage Total Bucket 18        "
            inst_names$(31%) = "Manage Total Bucket 19        "
            inst_names$(32%) = "Prev Period Opening Balance   "
            inst_names$(33%) = "Prev Period Activity          "
            inst_names$(34%) = "Prev Year Current Balance     "
            inst_names$(35%) = "Prev Year Opening Balance     "
            inst_names$(36%) = "Prev Year Activity            "
            inst_names$(37%) = "Prev Year Period Opening Bal  "
            inst_names$(38%) = "Prev Year Period Activity     "
            inst_names$(39%) = "Curr Bgt Year Current Balance "
            inst_names$(40%) = "Curr Bgt Year Activity        "
            inst_names$(41%) = "Curr Bgt Period Opening Bal   "
            inst_names$(42%) = "Curr Bgt Period Activity      "
            inst_names$(43%) = "Curr Bgt Year-End Balance     "
            inst_names$(44%) = "Add Calculation               "
            inst_names$(45%) = "Subtract Calculation          "
            inst_names$(46%) = "Divide Calculation            "
            inst_names$(47%) = "Multiplication Calculation    "
            inst_names$(48%) = "Percent Calculation           "
            inst_names$(49%) = "Prev Year Bgt Per Opening Bal "
            inst_names$(50%) = "Prev Year Bgt Per Activity    "
            inst_names$(51%) = "Prev Year Bgt Year Activity   "
            inst_names$(52%) = "Prev Year Bgt Year Current Bal"
            inst_names$(53%) = "Prev Year Bgt Year Y-E Bal    "
            inst_names$(54%) = "Next Year Bgt Per Opening Bal "
            inst_names$(55%) = "Next Year Bgt Per Activity    "
            inst_names$(56%) = "Next Year Bgt Year Activity   "
            inst_names$(55%) = "Next Year Bgt Year Currnet Bal"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            gosub L29000                   /* Clear Variables For Input */
            maxlines% = 0%

            for fieldnr% = 1% to 10%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10280
L10130:         gosub'101(1%, fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  9% then print_data
                      if keyhit%  = 10% then copy_report
                      if keyhit% <>  4% then L10230

L10180:               if fieldnr% < 2%  then L10130
L10190:                   fieldnr% = fieldnr% - 1%
                      if fieldnr% = 3% and dual_books$ <> "Y" then L10190
                          gosub'051(fieldnr%)
                      if enabled% <> 0% then L10130
                          goto L10180
L10230:               if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit%  = 32% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10130
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
L10280:         next fieldnr%
                if maxlines% > 0% then summary

*        Get Column Definition Data...
            for fieldnr% = 1% to 12%
                if fieldnr% > 1% and fieldnr% > columns% + 2%  then L11010
                gosub'055(fieldnr%)
                      if enabled% = 0% then L10670
L10540:         gosub'105(8%, fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10660
L10590:                   if fieldnr% < 2%  then L10540
                          fieldnr% = fieldnr% - 1%
                          gosub'051(fieldnr%)
                          if enabled% <> 0% then L10540
                          goto L10590
L10660:               if keyhit% <>  0% then       L10540
L10670:         gosub'155(fieldnr%)
                      if errormsg$ <> " " then L10540
                next fieldnr%

L11010
*        Get Line Item Data...
L11020:     if maxlines% = 320% then summary
            c% = maxlines% + 1%
            gosub inputlines
            if keyhit% <> 16% then L11080
                gosub columnone
                goto summary
L11080:     maxlines% = maxlines% + 1%
            goto L11020

        inputlines
            instdescr$, groupdescr$ = " "
            for colnr% = 1% to columns%
                for fieldnr% = 1% to 8%
                    gosub'052(colnr%, fieldnr%)
                          if enabled% = 0% then L11340
L11170:             gosub'102(3%, colnr%, fieldnr%)
                          if keyhit% = 16% and fieldnr% = 1% then return
                          if keyhit%  =  1% then gosub startover
                          if keyhit%  = 14% then gosub show_legend
                          if keyhit% <>  2% then       L11240
                             gosub columnone
                             goto inputlines
L11240:                   if keyhit% <>  6% then       L11270
                             gosub prevline
                             goto L11170
L11270:                   if keyhit% <>  4% then       L11330
L11280:                      if fieldnr% < 2% then L11170
                             fieldnr% = fieldnr% - 1%
                             gosub'052(colnr%, fieldnr%)
                             if enabled% <> 0% then L11170
                             goto L11280
L11330:                   if keyhit% <>  0% then       L11170
L11340:             gosub'152(colnr%, fieldnr%)
                          if errormsg$ <> " " then L11170
                    next fieldnr%
                next colnr%
                return

        columnone
                str(inst$(),  ((c%-1%) *  40%) +1%, 40%) = " "
                str(acct1$(), ((c%-1%) * 160%) +1%, 160%) = " "
                str(acct2$(), ((c%-1%) * 160%) +1%, 160%) = " "
                str(group$(), ((c%-1%) *  60%) +1%, 60%) = " "
                str(print$(), ((c%-1%) *  30%) +1%, 30%) = " "
                str(reverse$(), ((c%-1%) * 30%) +1%, 30%) = " "
                str(text$(), ((c%-1%) * 450%) +1%, 450%) = " "
                str(fixed$(), ((c%-1%) * 30%) +1%, 30%) = " "
                str(total$(), ((c%-1%) * 200%) +1%,200%) = " "
                str(print_if$(), ((c%-1%) * 10%) +1%, 10%) = " "
            return

        prevline
            if c%=1% then return
                  on fieldnr% goto  L11650,         /* INSTRUCTION      */~
                                    L11665,         /* ACCOUNTS         */~
                                    L11690,         /* ZONE OVERRIDE    */~
                                    L11700,         /* PRINT?           */~
                                    L11710,         /* PRINT RESTRICTION*/~
                                    L11720,         /* REVERSE SIGNS    */~
                                    L11730,         /* TEXT FOR PRINT   */~
                                    L11740          /* TOTALING FLAGS   */
                  return

L11650:         inst$     (c%, colnr%) = inst$     (c%-1%, colnr%):return
L11665:         group$    (c%, colnr%) = group$    (c%-1%, colnr%)
                acct1$    (c%, colnr%) = acct1$    (c%-1%, colnr%)
                acct2$    (c%, colnr%) = acct2$    (c%-1%, colnr%):return
L11690:         fixed$    (c%, colnr%) = fixed$    (c%-1%, colnr%):return
L11700:         print$    (c%, colnr%) = print$    (c%-1%, colnr%):return
L11710:         print_if$ (c%, colnr%) = print_if$ (c%-1%, colnr%):return
L11720:         reverse$  (c%, colnr%) = reverse$  (c%-1%, colnr%):return
L11730:         text$     (c%, colnr%) = text$     (c%-1%, colnr%):return
L11740:         total$    (c%, colnr%) = total$    (c%-1%, colnr%):return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
            b% = 0%
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press (ENTER)."
L12068:     gosub'101(2%, 0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       summary
                  if keyhit%  =  5% then       editpg1a
                  if keyhit%  = 12% then       delete_rpt
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       L12068
            fieldnr% = cursor%(1%) - 5%
            if fieldnr% <  2% or fieldnr% > 14% then L12068
            if fieldnr% =  3% and dual_books$ <> "Y" then L12068
            if fieldnr% =  4% or fieldnr% =  5% then L12068
*          IF FIELDNR% = 10% THEN 12068
            if fieldnr% > 10% then fieldnr% = fieldnr% - 2%
            if fieldnr% >  4% then fieldnr% = fieldnr% - 2%

            gosub'051(fieldnr%)
L12100:     gosub'101(7%, fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12100
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L12100
            goto editmode

        delete_rpt           /* DELETE ENTIRE REPORT FORMAT */
            errormsg$ = "Press ENTER to delete -OR- PF(1) to cancel del"&~
                        "ete"
L12126:     gosub'101(2%, 0%)
                if keyhit% <> 1% then L12130
                    errormsg$ = " "  :  goto L12068
L12130:         if keyhit% <> 0% then goto L12126
            readkey$ = str(report$) & hex(0000)
                call "DELETE" (#1, readkey$, 3%)
                call "DELETE" (#2, readkey$, 3%)
            errormsg$ = " "
            goto inputmode

        summary              /* SUMMARY SCREEN */
            errormsg$ = " "
            insert% = 0%
            message$="To Modify Line Item, Tab To Line And Press (ENTER)."

L12350:     gosub'110(0%)
              if keyhit% = 1% then gosub startover
              if keyhit% = 2% then b%=0%
              if keyhit% = 3% then b%=max(0%, maxlines%-12%)
              if keyhit% = 4% then b%=max(0%, b%-12%)
              if keyhit% = 5% then b%=max(0%, min(b%+12%,maxlines%-12%))
              if keyhit% = 6% then b%=max(0%, b%-1%)
              if keyhit% = 7% then b%=max(0%, min(b%+1%,maxlines%-1%))
              if keyhit% = 9% then       editmode
              if keyhit% =12% then       L12490
              if keyhit% = 8% then       L12490
              if keyhit% =14% then gosub show_legend
              if keyhit% =16% then       datasave

L12490:       fieldnr% = cursor%(1%) - 6%
              if fieldnr% < 0% or fieldnr% > 14% then L12350
              if hfac$(fieldnr%) = hex(bc) then L12350
              c%=min(b%+fieldnr%,maxlines%)
              if keyhit%=11% then insertmode
              if c%=0% then L12350
              fieldnr%=c%-b%

              if keyhit%=12% then deletemode
              if keyhit%<>0% then L12350

*        Edit page 2...
            colnr% = 1%
L12610:     message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press (ENTER)."

*        Next Line Will Draw Out The Description For Line Codes...
            gosub'152(colnr%, 3%)
L12650:     errormsg$=" "
            gosub'102(4%, colnr%, 0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then c%=1%
                  if keyhit%  =  3% then c%=max(1%, maxlines%)
                  if keyhit%  =  4% then c%=max(1%,c%-10%)
                  if keyhit%  =  5% then c%=max(1%,min(c%+10%,maxlines%))
                  if keyhit%  =  6% then c%=max(1%,c%-1%)
                  if keyhit%  =  7% then c%=max(1%,min(c%+1%,maxlines%))
                  if keyhit%  = 20% then colnr%=max(1%,colnr%-1%)
                  if keyhit%  = 21% then colnr%=max(1%,min(colnr%+1,     ~
                                                          columns%))
                  if keyhit%  =  9% then editmode
                  if keyhit% =  14% then gosub show_legend
                  if keyhit%  = 16% then summary
                  if keyhit% <>  0% then L12610
            fieldnr% = cursor%(1%) - 8%
            if fieldnr% < 1% or fieldnr% > 12% then L12650
            if fieldnr% > 1% then fieldnr% = fieldnr% - 1%
            if fieldnr% > 4% then fieldnr% = fieldnr% + 1%
            if fieldnr% > 8%  then fieldnr% = 8%
            if fieldnr% = 4% and cursor%(2%) > 32% then fieldnr% = 5%

            gosub'052(colnr%, fieldnr%)
                if enabled% = 0% then L12610
L12910:     gosub'102(7%, colnr%, fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12910
L12940:     gosub'152(colnr%, fieldnr%)
                if fieldnr% = 1% and str(inst$(c%,r%),1%,1%) = "N" and   ~
                     str(inst$(c%,r%),3%,1%) = " " then L12945
                if errormsg$ <> " " then L12910 else goto L12610
L12945:             fieldnr% = 2%
                    goto L12940

        editpg1a
            b% = 0%
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press (ENTER)."
L12975:     gosub'105(9%, 0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       summary
                  if keyhit%  =  4% then       editmode
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       L12975

            fieldnr% = cursor%(1%) - 4%
            if fieldnr% <  1% or fieldnr% > columns% + 4%  then L12975
            if fieldnr% >  4% then fieldnr% =  fieldnr% - 2%

            gosub'055(fieldnr%)
               if enabled% = 0% then editpg1a
L12990:     gosub'105(7%, fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12990
            gosub'155(fieldnr%)
                  if errormsg$ <> " " then L12990
            goto editpg1a
        REM *************************************************************~
            *       I N S E R T   &   D E L E T E   L O G I C           *~
            *                                                           *~
            * INSERT & DELETE CODE RESIDES HERE.                        *~
            *************************************************************

        insertmode: insert% = 1%
            if maxlines%=320% then summary

*        Copy all Elements Up One...
            maxlines%=maxlines%+1%
            c% = c% + 1%
            if c%=maxlines% then L13200
                roll% = -1%
                for temp% = maxlines% to c% step -1%
                     gosub roll_lines
                next temp%
                convert c%-1% to temp$, pic(###)

L13200
*        Get Line Item Data...
            gosub columnone
            gosub inputlines
            if keyhit% = 16% then delete_line
            goto insertmode

        deletemode
            message$ = "To DELETE Flashing Line, Press ENTER.  To Return ~
        ~Without Delete, Press PF1."
            gosub'112(fieldnr%)
                  if keyhit%=1% then summary
                  if keyhit%<>0% then deletemode

        delete_line
            if c%=maxlines% then L13410
                roll% = 1%
                for temp% = c% to maxlines%-1%
                     gosub roll_lines
                next temp%
                convert c% to temp$, pic(###)
L13410:     c%=maxlines%
            gosub columnone
            maxlines%=maxlines%-1%
            if b% > maxlines% then b% = max(0%, maxlines%-1%)
            goto summary

        roll_lines
            for colnr% = 1% to columns%
              inst$     (temp%, colnr%) = inst$     (temp%+roll%, colnr%)
              group$    (temp%, colnr%) = group$    (temp%+roll%, colnr%)
              acct1$    (temp%, colnr%) = acct1$    (temp%+roll%, colnr%)
              acct2$    (temp%, colnr%) = acct2$    (temp%+roll%, colnr%)
              fixed$    (temp%, colnr%) = fixed$    (temp%+roll%, colnr%)
              print$    (temp%, colnr%) = print$    (temp%+roll%, colnr%)
              print_if$ (temp%, colnr%) = print_if$ (temp%+roll%, colnr%)
              reverse$  (temp%, colnr%) = reverse$  (temp%+roll%, colnr%)
              text$     (temp%, colnr%) = text$     (temp%+roll%, colnr%)
              total$    (temp%, colnr%) = total$    (temp%+roll%, colnr%)
            next colnr%
        return


        REM *************************************************************~
            *       I N P U T   M O D E   F O R   C O P Y               *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        copy_report
            errormsg$, startcode$, endcode$, display$(1%) = " "

            for fieldnr% = 1% to  2%
                gosub'054(fieldnr%)
                      if enabled% = 0% then L14180
L14120:         gosub'104(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then inputmode
                      if keyhit% <>  0% then       L14120
                gosub'154(fieldnr%)
                      if errormsg$ <> " " then L14120
L14180:         next fieldnr%

            report$ = startcode$
            gosub L30000
            if maxlines% = 0% then inputmode
            report$ = endcode$
            goto summary

        REM *************************************************************~
            *       I N P U T   M O D E   F O R   P R I N T             *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        print_data
            errormsg$, startcode$, endcode$ = " "

            for fieldnr% = 1% to  2%
                gosub'053(fieldnr%)
                      if enabled% = 0% then L15180
L15120:         gosub'103(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 16% then inputmode
                      if keyhit% <>  0% then       L15120
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L15120
L15180:         next fieldnr%

        REM *************************************************************~
            *               P R I N T  C O D E S                        *~
            *                                                           *~
            * Prints a report showing accounts tied to each code.       *~
            *************************************************************

            print at(05,02); hex(84); "Print In Progress"
            if startcode$ <> "ALL" then L16110
                readkey$ = all(hex(00))
                endcode$ = all(hex(ff))
                goto L16140
L16110:     str(readkey$,,3%) = str(startcode$,,3%) addc all(hex(ff))
            if endcode$ = " " then endcode$ = startcode$

L16140:     call "PLOWNEXT" (#1, readkey$, 0%, f1%(1%))
                if f1%(1%) = 1% then L16190
L16160:         close printer
                goto inputmode

L16190:     if str(readkey$,,3%) > endcode$ then L16160
            gosub L29000
            get #1, using L16220, report$, descr$, title$, columns$
L16220:     FMT CH(3), CH(30), CH(50), XX(105), CH(2)
            convert columns$ to columns%
            pagenumber% = 0%
            pageline% = 987654321%

            gosub load_line_items
            for c% = 1% to maxlines%
            for r% = 1% to columns%
               gosub form_control
               if acct2$(c%,r%) = " " then acct2$(c%,r%) = acct1$(c%,r%)
               message$ = acct1$(c%,r%) & " - " & acct2$(c%,r%)
               if acct1$(c%,r%) = " " and acct2$(c%,r%) = " " then       ~
                      message$ = "N/A"
               if group$(c%,r%) <> " " then message$= "GROUP: " &        ~
                      group$(c%,r%)

            print using L17360, c%, r%, inst$(c%,r%), message$,           ~
                   fixed$(c%,r%), print$(c%,r%), print_if$(c%,r%),       ~
                   reverse$(c%,r%), text$(c%,r%),                        ~
                   str(total$(c%,r%),1%,1%),  str(total$(c%,r%),2%,1%),  ~
                   str(total$(c%,r%),3%,1%),  str(total$(c%,r%),4%,1%),  ~
                   str(total$(c%,r%),5%,1%),  str(total$(c%,r%),6%,1%),  ~
                   str(total$(c%,r%),7%,1%),  str(total$(c%,r%),8%,1%),  ~
                   str(total$(c%,r%),9%,1%),  str(total$(c%,r%),10%,1%), ~
                   str(total$(c%,r%),11%,1%), str(total$(c%,r%),12%,1%), ~
                   str(total$(c%,r%),13%,1%), str(total$(c%,r%),14%,1%), ~
                   str(total$(c%,r%),15%,1%), str(total$(c%,r%),16%,1%), ~
                   str(total$(c%,r%),17%,1%), str(total$(c%,r%),18%,1%), ~
                   str(total$(c%,r%),19%,1%), str(total$(c%,r%),20%,1%)
            next r%
            tag% = 0%
            next c%
            gosub form_control
            if tag% = 0% then print using L17390
            goto L16140

        REM *************************************************************~
            *        P A G E   C O N T R O L   R O U T I N E            *~
            *                                                           *~
            * CONTROLS THE PAGING                                       *~
            *************************************************************

        form_control
                select printer (134)
                pageline% = pageline% + 2%
                if pageline% < 58% then return
                   if pagenumber% > 0% and tag% =0% then print using L17390
                   print page
                   pagenumber% = pagenumber% + 1%
                   print using L17230, pagenumber%, date$
                   print using L17260, report$, descr$, title$
                   print using L17290
                   print using L17320
                   print using L17340
                   print using L17390
                   pageline% = 7%
                   tag% = 1%
                   return

L17230: %PAGE ###    G/L   R E P O R T    F O R M A T   S P E C I F I C A~
        ~ T I O N S                                         DATE: ########

L17260: %REPORT: ###  DESCRIPTION: ##############################  DEFAUL~
        ~T TITLE: ##################################################

L17290: %!---+---+----+-------------------------+---+---+---+---+--------~
        ~-------------------------+---------------------------------------~
        ~!

L17320: %!SEQ!COL!INST!ACCOUNT NUMBER RANGE     !IGN!PRT!PRT!REV!TEXT TO ~
        ~ PRINT                   !                TOTALING               ~
        ~!

L17340: %!   !   !    !OR ACCOUNT GROUP CODE    !ZNS!FLG!WHN!SGN!        ~
        ~                         !                 BUCKETS   (0-19)      ~
        ~!

L17360: %!###!###!####!#########################!###!###! # !###!########~
        ~#########################!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#~
        ~!

L17390: %!---+---+----+-------------------------+---+---+---+---+--------~
        ~-------------------------+0+1+2+3+4+5+6+7+8+9+0+1+2+3+4+5+6+7+8+9~
        ~!


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave

            message$ = "Writing Report: " & report$ & " To Data Files"
            call "SHOSTAT" (message$)

*        Delete old report from file...
            readkey$ = str(report$) & hex(0000)
            call "DELETE" (#1, readkey$, 3%)
            call "DELETE" (#2, readkey$, 3%)

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
                                    L20380,         /* SET OF BOOKS     */~
                                    L20410,         /* DEFAULT TITLE    */~
                                    L20450,         /* DEFAULT PAREN    */~
                                    L20480,         /* ROUND?           */~
                                    L20510,         /* PRINT IF ZERO?   */~
                                    L20580,         /* ZONE ONE         */~
                                    L20580,         /* ZONE TWO         */~
                                    L20580          /* ZONE THREE       */
                  return

L20330
*        Default/Enable - Report Number...
                message$ = "Enter A Blank Report Number To Search For Des~
        ~ired Report."
                return

L20370
*        Default/Enable - Description...
                message$ = "Please Enter Some Text For Quick Future Refer~
        ~ences."
                return

L20380
*        Default/Enable - Local Authority...
                if dual_books$ <> "Y" then enabled% = 0%
                message$ = "Enter '1' To Use STATUTORY Books; '2' To Use ~
        ~LOCAL AUTHORITY Books."
                return

L20410
*        Default/Enable - Report Title...
                message$= "Enter Title To Be Used As Report Header.  This~
        ~ Can Be Changed At Run Time."
                return

L20450
*        Default/Enable - Parenthesis Option...
                if paren$ <> " " then L20458
                    paren$ = "NO"
                    goto L20459
L20458:         if paren$ = "YES" then L20464
L20459:             message$ = "Enter 'YES' If You Want Parenthesis Rat"&~
                               "her Than Minus Signs."
                    return
L20464:         message$ = "Enter 'NO' If You Want Minus Signs Rather T"&~
                           "han Parenthesis."
                return

L20480
*        Default/Enable - Rounding Option...
                if round$ <> " " then L20488
                    round$ = "NO "
                    goto L20490
L20488:         if round$ = "YES" then L20496
L20490:             message$ = "Enter 'YES' If You Do NOT Want To Print"&~
                               " 'Cents' For Dollar Amounts."
                    return
L20496:         message$ = "Enter 'NO' If You Want To Print 'Cents' For"&~
                           " Dollar Amounts."
                return

L20510
*        Default/Enable - Print When Zero Option...
                if zero$ <> " " then L20518
                    zero$ = "YES"
                    goto L20520
L20518:         if zero$ = "NO" then L20526
L20520:             message$ = "Enter 'NO' If You Do NOT Want To Print "&~
                               "Lines When Amount(s) On Line Are Zero."
                    return
L20526:         message$ = "Enter 'YES' If You Want To Print Lines When"&~
                           "Amount(s) On Line Are Zero."
                return


L20580
*        Default/Enable - Zone Specifications...
                message$ = "Enter Run Time Account Selection Criteria ("&~
                           "Optional)."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'052(r%, fieldnr%)
                  enabled% = 1%:message$=" "
                  on fieldnr% gosub L21400,      /* INSTRUCTION CODE    */~
                                    L21550,      /* ACCOUNTS            */~
                                    L21720,      /* ZONE OVERRIDE       */~
                                    L21800,      /* PRINT?              */~
                                    L21960,      /* PRINT RESTRICTION   */~
                                    L22030,      /* REVERSE SIGNS       */~
                                    L22100,      /* TEXT FOR PRINT      */~
                                    L22250       /* TOTALING FLAGS      */
            return

L21400
*        Default/Enable - Instruction Code...
                if r% = 1% then L21410
                if inst$(c%,1%) = "H" or str(inst$(c%,1%),1%,2%) ="P "or ~
                   inst$(c%,1%) = "D" or inst$(c%,1%) = "DC"             ~
                   then enabled% = 0%
L21410:         message$ = "Enter Instruction Code. Use PF14 To Display V~
        ~alid Instructions."
                return

L21550
*        Default/Enable - Account Selection...
                enabled% = 0%
                gosub set_accum_enable
                if str(inst$(c%,r%),,1%) = "T" then enabled% = 0%
                if inst$(c%,r%) = " " then enabled% = 0%
                message$="Enter A Group Code To Use All Accts In That Cod~
        ~e, OR Enter Acct Numbers."
                if r% = 1% then L21705
                for p% = 2% to r%
                    if str(inst$(c%,p%),1%,1%) = "N" and                 ~
                       str(inst$(c%,p%),3%,1%) = " " then L21700
                    if print$(c%,p%-1%)= "ALL" or print$(c%,p%-1%)= "SUM"~
                       then L21652
                    goto L21700
L21652:             back% = 1%
                    init(" ") acct1$(c%,p%), acct1$(c%,p%), group$(c%,p%)
L21656:             if str(inst$(c%,p%-back%),1%,1%) <> "N" or           ~
                       str(inst$(c%,p%-back%),3%,1%) <> " " then L21665
                        back% = back% + 1%
                    if back% > r% then L21690
                    goto L21656
L21665:                 acct1$(c%,p%) = acct1$(c%,p%-back%)
                        acct2$(c%,p%) = acct2$(c%,p%-back%)
                        group$(c%,p%) = group$(c%,p%-back%)
L21690:                 enabled% = 0%
L21700:         next p%
L21705:         if str(inst$(c%,r%),1%,1%) = "N"  and                    ~
                       str(inst$(c%,r%),3%,1%) =" " then L21711 else L21715
L21711:             enabled% = 1%
                    message$ = "Enter Columns, Totals, Or Numbers To Be U~
        ~sed In Calculations."
L21715:         return

L21720
*        Default/Enable - Zone Override...
                enabled% = 0%
                if zonename$() = " " then return
                gosub set_accum_enable
                if fixed$(c%,r%)=" "and enabled%=1 then fixed$(c%,r%)="NO"
                message$ = "'YES' Will Force ALL Accts Indicated To ALLWA~
        ~YS Be Used, Regardless Of Zoning."
                return

L21800
*        Default/Enable - Print?...
                enabled% = 0%
                gosub set_accum_enable
                if enabled% = 0% then print$(c%,r%) = "YES"
                if inst$(c%,r%)=" " then print$(c%,r%) = "NO"
                message$ = "Enter Print Option... Use PF(14) To Display T~
        ~he List Of Options."
                if r% = 1% then L21950
                for p% = 1% to r%-1%
                    if print$(c%,p%) = "SUM" or print$(c%,p%) = "ALL"    ~
                       then L21920
                    goto L21940
L21920:             print$(c%,r%) = print$(c%,p%)
                    enabled% = 1%
L21940:         next p%
L21950:         return

L21960
*        Default/Enable - Print Restrictions...
                enabled% = 0%
                if print$(c%,r%) = "NO " then return
                gosub set_accum_enable
                message$ = "Enter Desired Print Restrictor Codes... Use P~
        ~F(14) To Display List Of Options."
                return

L22030
*        Default/Enable - Reverse Sign Indicator...
                enabled% = 0%
*              IF PRINT$(C%,R%) = "NO " THEN RETURN
                gosub set_accum_enable
                message$ = "Enter 'YES' To Reverse Sign On Numbers Before~
        ~ Printing And Adding To Totals."
                return

L22100
*        Default/Enable - Text to print...
                if group$(c%,r%) <> " " and print$(c%,r%) = "ALL"        ~
                    then text$(c%,r%) = " "
                if print$(c%,r%) = "NO " then enabled% = 0%
                if print$(c%,r%) = "NO " then text$(c%,r%) = " "
                if inst$(c%,r%)="U " or inst$(c%,r%)="DU" then enabled%=0%
                message$ = "Enter Text To Be Printed On The Report Line."
                if r% = 1% then L22230
                for p% = 1% to r%-1%
                    if print$(c%,p%) = "SUM" or print$(c%,p%) = "ALL"    ~
                       then L22200
                    goto L22220
L22200:             text$(c%,r%) = text$(c%,p%)
                    enabled% = 0%
L22220:         next p%
L22230:         return

L22250
*        Default/Enable - Totaling Specifications...
                enabled% = 0%
                gosub set_accum_enable
                message$ = "Indicate How This Line Is To Effect Totaling ~
        ~Buckets. Use PF(14) For More Info."
                return

        set_accum_enable
            search inst_codes$() = str(inst$(c%, r%)) to cursor%() step 4%
                if cursor%(1) > 24% then enabled% = 1%
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 3 OF INPUT. *~
            *************************************************************

            deffn'053(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L23110,         /* STARTING REPORT  */~
                                    L23160          /* ENDING REPORT    */
                     return

L23110
*        Default/Enable - Stating Account...
            startcode$ = "ALL"
            message$ = "'ALL' Will Print All Formats.  To Print Ranges, E~
        ~nter The Starting Report."
                return

L23160
*        Default/Enable - Ending Account...
            if startcode$ = "ALL" then enabled% = 0%
            message$ = "Enter The Last To Print.  Leave Blank To Only Pri~
        ~nt The 'Starting Report'."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   4     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 4 OF INPUT. *~
            *************************************************************

            deffn'054(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L24110,         /* COPY FROM        */~
                                    L24160          /* COPY TO          */
                     return

L24110
*        Default/Enable - Report To Be Copied...
            message$ = "Enter The ID Of The Report That Is To Be Copied."
                return

L24160
*        Default/Enable - Copy To...
            message$ = "Enter ID For The New Report.  It Must Not Be The ~
        ~Same As An Existing Report."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1A    *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1A OF INPUT.*~
            *************************************************************

            deffn'055(fieldnr%)
                enabled% = 1%
                  on fieldnr% gosub L25300,         /* NUMBER OF COLUMNS*/~
                                    L25400,         /* COL DESC TEXT    */~
                                    L25500,         /* COLUMN 1         */~
                                    L25500,         /* COLUMN 2         */~
                                    L25500,         /* COLUMN 3         */~
                                    L25500,         /* COLUMN 4         */~
                                    L25500,         /* COLUMN 5         */~
                                    L25500,         /* COLUMN 6         */~
                                    L25500,         /* COLUMN 7         */~
                                    L25500,         /* COLUMN 8         */~
                                    L25500,         /* COLUMN 9         */~
                                    L25500          /* COLUMN 10        */
                return

L25300
*        Default/Enable - Number of Columns...
            message$ = " Enter The Number Of Columns Desired."
            return

L25400
*        Default/Enable - Column Description Text...
            message$ = "Enter Description (Optional)."
            return

L25500
*        Default/Enable - Column Information...
            if fieldnr% - 2% > columns% then enabled% = 0%
            message$ = "Enter Column Information."
            return

L29000: REM *************************************************************~
            * INITIALIZATION BLOCK (NEATER THAN CRAMMING AT 10000)      *~
            *************************************************************

            descr$,                      /* REPORT DESCRIPTION         */~
            errormsg$,                   /* ERROR MESSAGE              */~
            filler$,                     /* FILLER AT END OF RECORD    */~
            groupdescr$,                 /* ACCOUNT GROUPING CODE DESCR*/~
            instdescr$,                  /* FOR DISPLAY                */~
            message$,                    /* INPUT MESSAGE              */~
            paren$,                      /* DEFAUT PARENTHESIS OPTION  */~
            report$,                     /* REPORT ID.                 */~
            round$,                      /* ROUND OPTION (DEFALUT)     */~
            setdescr$,                   /* G/L BOOKS DESCRIPTION      */~
            title$,                      /* DEFAULT REPORT TITLE       */~
            zero$,                       /* PRINT IF ZERO? (DEFAULT)   */~
            zone$(),                     /* START, LENGTH IN ACCOUNT   */~
            zonename$() = " "            /* LABLE FOR ZONE IN ACCOUNT  */

            col$(),                      /* COLUMN INDICATOR           */~
            colcom$(),                   /* COLUMN COMMA INDICATOR     */~
            colfmt$(),                   /* COLUMN COMMA INDICATOR     */~
            collgth$(),                  /* COLUMN LENGTH              */~
            colpos$(),                   /* COLUMN POSITION            */~
            coltext$,                    /* COLUMN TEXT                */~
            columns$,                    /* NUMBER OF COLUMNS          */~
            text1$(),                    /* COLUMN TEXT LINE 1         */~
            text2$() = " "               /* COLUMN TEXT LINE 2         */

            init (" ")                                                   ~
            acct1$(),                    /* FROM ACCOUNT               */~
            acct2$(),                    /* TO ACCOUNT                 */~
            acctthru$(),                 /* THRU INDICATOR             */~
            fixed$(),                    /* RUN TIME OVERRIDE FLAG     */~
            group$(),                    /* ACCOUNT GROUPING CODE      */~
            inst$(),                     /* INSTRUCTION CODE           */~
            print$(),                    /* PRINT CODE                 */~
            print_if$(),                 /* PRINT/TOTAL CONSTRAINT FLAG*/~
            reverse$(),                  /* REVERSE SIGN INDICATOR     */~
            text$(),                     /* TEXT FOR LINE ITEMS        */~
            total$()                     /* TOTALING SPEC FOR LINE     */

            set$ = "1"                   /* BOOKS DEFAULT = STATUTORY  */
            mast% = 3%                   /* STAT DEFAULT  - FRGRPMAS   */
            gl%   = 5%                   /* STAT DEFAULT  - GLMAIN2    */

            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

L29580: startover

            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                if u3% <> 0% then L29580

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *              L O A D   O L D   R E P O R T                *~
            *                                                           *~
            * LOADS OLD REPORT FROM DISK FILES.                         *~
            *************************************************************

            onfile% = 0%

*        Try And Load Report...
            call "READ100" (#1, report$, f1%(1%))
                if f1%(1%) = 0% then return       /* New Report */

*        Load And Format Data, Header First...
            onfile% = 1%
            get #1, using L30430, report$, descr$, title$, paren$, zero$, ~
                                 round$, zone$(), zonename$(), columns$, ~
                                 coltext$, colpos$(), collgth$(),        ~
                                 colfmt$(), colcom$(), text1$(),         ~
                                 text2$(), set$
            if dual_books$ = "Y" then L30150
                set$ = "1"
                convert set$ to set, data goto L30152
                goto L30158
L30150:     if set$ <> "1" then L30154
                setdescr$ = "Statutory"
L30152:         mast% = 3%  :  gl% = 5%
                goto L30156
L30154:     setdescr$ = "Local Authority"
            mast% = 4%  :  gl% = 6%
L30156:     call "PUTPAREN" (setdescr$)
            convert set$ to set, data goto L30158
L30158:     gosub L55220

            for c% = 1% to columns%
                convert colpos$(c%) to colpos%(c%)
                convert collgth$(c%) to collgth%(c%)
            next c%

        load_line_items
            maxlines% = 0%
            readkey$ = str(report$) & hex(0000)
L30210:     call "PLOWNEXT" (#2%, readkey$, 3%, f1%(2%))
                     if f1%(2%) = 0% then return

            c%, maxlines% = maxlines% + 1%
            get #2, using L30530, report$, c%,                            ~
                str(inst$(),((c%-1%)* 40%)+1%,40%),                      ~
                str(group$(),((c%-1%)* 60%)+1%,60%),                     ~
                str(acct1$(),((c%-1%)*160%)+1%,160%),                    ~
                str(acct2$(),((c%-1%)*160%)+1%,160%),                    ~
                str(fixed$(),((c%-1%)* 30%)+1%,30%),                     ~
                str(print$(),((c%-1%)* 30%)+1%,30%),                     ~
                str(reverse$(),((c%-1%)* 30%)+1%,30%),                   ~
                str(text$(),((c%-1%)*450%)+1%,450%),                     ~
                str(total$(),((c%-1%)*200%)+1%,200%),                    ~
                str(print_if$(),((c%-1%)* 10%)+1%,10%)
            for colnr% = 1% to columns%
                if str(inst$(c%, colnr%),1%,1%) = "N" and                ~
                   str(inst$(c%, colnr%),3%,1%) = " " then L30405
                call "GLFMT" (acct1$(c%, colnr%))
                call "GLFMT" (acct2$(c%, colnr%))
L30405:     next colnr%
            goto L30210

L30430:         FMT CH(03),              /* REPORT NUMBER              */~
                    CH(30),              /* DESCRIPTION                */~
                    CH(50),              /* TITLE FOR REPORT           */~
                    CH(03),              /* PARENTHESIS FLAG           */~
                    CH(03),              /* XERO FLAG                  */~
                    CH(03),              /* ROUND FLAG                 */~
                    8*CH(2),             /* ZONE POSITIONS             */~
                    4*CH(20),            /* ZONE NAMES                 */~
                    CH(2),               /* Number of Columns          */~
                    CH(45),              /* Column Description Text    */~
                    10*CH(3),            /* Column Positions           */~
                    10*CH(2),            /* Column Lengths             */~
                    10*CH(1),            /* Column Formats             */~
                    10*CH(3),            /* Column Comma Indicators    */~
                    10*CH(16),           /* Column Text line 1         */~
                    10*CH(16),           /* Column Text line 2         */~
                    CH(01)               /* SET OF BOOKS TO USE        */


L30530:         FMT CH(03),              /* REPORT NUMBER              */~
                    BI(2),               /* SEQUENCE NUMBER            */~
                    CH(40),              /* INSTRUCTION CODE           */~
                    CH(60),              /* ACCOUNT GROUP CODE         */~
                    CH(160),             /* START ACCOUNT NUMBER       */~
                    CH(160),             /* END ACCOUNT NUMBER         */~
                    CH(30),              /* IGNORE ZONING FLAG         */~
                    CH(30),              /* PRINT FLAG                 */~
                    CH(30),              /* REVERSE SIGNS?             */~
                    CH(450),             /* DESCRIPTION                */~
                    CH(200),             /* TOTALING SPECS             */~
                    CH(10),              /* PRINT IF FLAG              */~
                    CH(225)              /* FILLER                     */

L31000: REM *************************************************************~
            *        W R I T E   R E P O R T   T O   F I L E S          *~
            *                                                           *~
            * WRITES REPORT SPECS TO DISK.                              *~
            *************************************************************

*        Lines are saved first...
            if maxlines% = 0% then L31200
            for c% = 1% to maxlines%
                for colnr% = 1% to columns%
                    if str(inst$(c%,colnr%),1%,1%) = "N" and             ~
                       str(inst$(c%,colnr%),3%,1%) = " " then L31125
                            if set = 2 then goto L31122
                    call "GLUNFMT" (acct1$(c%, colnr%))
                    call "GLUNFMT" (acct2$(c%, colnr%))
                    goto L31125
L31122:                 call "GLUNFM2" (acct1$(c%, colnr%))
                        call "GLUNFM2" (acct2$(c%, colnr%))
L31125:         next colnr%

            write #2, using L30530, report$, c%,                          ~
                str(inst$(),((c%-1%)* 40%)+1%,40%),                      ~
                str(group$(),((c%-1%)* 60%)+1%,60%),                     ~
                str(acct1$(),((c%-1%)*160%)+1%,160%),                    ~
                str(acct2$(),((c%-1%)*160%)+1%,160%),                    ~
                str(fixed$(),((c%-1%)* 30%)+1%,30%),                     ~
                str(print$(),((c%-1%)* 30%)+1%,30%),                     ~
                str(reverse$(),((c%-1%)* 30%)+1%,30%),                   ~
                str(text$(),((c%-1%)*450%)+1%,450%),                     ~
                str(total$(),((c%-1%)*200%)+1%,200%),                    ~
                str(print_if$(),((c%-1%)* 10%)+1%,10%), " "
            next c%

L31200
*        Write header...
            convert set to set$, pic(#)
            write #1,using L30430,report$, descr$, title$, paren$, zero$, ~
                                 round$, zone$(), zonename$(), columns$, ~
                                 coltext$, colpos$(), collgth$(),        ~
                                 colfmt$(), colcom$(), text1$(),         ~
                                 text2$(), set$
            return

        REM *************************************************************~
            *      I N P U T   /   E D I T   M O D E   P A G E   1      *~
            *                                                           *~
            * SERVES INPUT LOOP AND EDIT MODE FOR PAGE ONE OF DOCUMENT. *~
            *************************************************************

            deffn'101(screen%, fieldnr%)
                  if fieldnr% = 3% and dual_books$ <> "Y" then return
                  gosub set_keys
                  init(hex(8c)) hfac$()
                  if fieldnr%=0% then init(hex(86)) str(hfac$(),2%)
                  header1$ = "Last Report Updated: " & lastreport$
                  str(header1$,62%) = "GLDEFINE: " & cms2v$

                  display$(1%)="The Following Fields Are Run Time Defau"&~
                             "lts/Specifications..." & hex(8c)

                  display$(2%)= "Allowed Run Time Account Zoning...  " & ~
                             hex(ac) & "Zone Description    " & hex(8c)  ~
                             & "    " & hex(ac) & "Start" & hex(8c) &    ~
                             "   " & hex(ac) & "Length"


                  on fieldnr% gosub L40360,         /* REPORT NUMBER    */~
                                    L40330,         /* DESCRIPTION      */~
                                    L40390,         /* SET OF BOOKS     */~
                                    L40330,         /* DEFAULT TITLE    */~
                                    L40360,         /* DEFAULT PAREN    */~
                                    L40360,         /* ROUND?           */~
                                    L40360,         /* PRINT IF ZERO?   */~
                                    L40330,         /* ZONE ONE         */~
                                    L40330,         /* ZONE TWO         */~
                                    L40330          /* ZONE THREE       */
                     goto L40430

L40330
*        Set FAC'S for Upper/Lower case input...
                      hfac$(fieldnr%) = hex(80)
                      return
L40360
*        Set FAC'S for Upper case only input...
                      hfac$(fieldnr%) = hex(81)
                      return
L40390
*        Set FAC'S for Numeric only input...
                      hfac$(fieldnr%) = hex(82)
                      return

L40430: accept                                                           ~
               at (01,02), "Financial Statements Report Specifications", ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header1$                ,ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "REPORT NUMBER",                              ~
               at (06,30), fac(hfac$(1%)), report$               ,ch(03),~
                                                                         ~
               at (07,02), "Description (Internal)",                     ~
               at (07,30), fac(hfac$(2%)), descr$               , ch(30),~
                                                                         ~
               at (08,02), fac(hex(8c)),  setmsg$               , ch(26),~
               at (08,30), fac(hfac$(3%)), set$                 , ch(01),~
               at (08,33), fac(hex(8c)),  setdescr$             , ch(30),~
                                                                         ~
               at (10,02), fac(hex(ac)), display$(1%)            ,ch(79),~
                                                                         ~
               at (11,02), "Default Department Title",                   ~
               at (11,30), fac(hfac$(4%)), title$               , ch(50),~
                                                                         ~
               at (12,02), "Parenthesis To Show Negatives?",             ~
               at (12,36), fac(hfac$(5%)), paren$                ,ch(03),~
                                                                         ~
               at (13,02), "Round Amounts To Nearest Dollar?",           ~
               at (13,36), fac(hfac$(6%)), round$                ,ch(03),~
                                                                         ~
               at (14,02), "Print Lines When All Zeroes?",               ~
               at (14,36), fac(hfac$(7%)), zero$                 ,ch(03),~
                                                                         ~
               at (16,02), fac(hex(8c)), display$(2%)            ,ch(79),~
                                                                         ~
               at (17,02), "First Zone",                                 ~
               at (17,39), fac(hfac$(8%)), zonename$(1%)         ,ch(20),~
               at (17,66), fac(hfac$(8%)), zone$(1%,1%)          ,ch(02),~
               at (17,77), fac(hfac$(8%)), zone$(1%,2%)          ,ch(02),~
                                                                         ~
               at (18,02), "Second Zone",                                ~
               at (18,39), fac(hfac$(9%)), zonename$(2%)         ,ch(20),~
               at (18,66), fac(hfac$(9%)), zone$(2%,1%)          ,ch(02),~
               at (18,77), fac(hfac$(9%)), zone$(2%,2%)          ,ch(02),~
                                                                         ~
               at (19,02), "Third Zone",                                 ~
               at (19,39), fac(hfac$(10%)), zonename$(3%)        ,ch(20),~
               at (19,66), fac(hfac$(10%)), zone$(3%,1%)         ,ch(02),~
               at (19,77), fac(hfac$(10%)), zone$(3%,2%)         ,ch(02),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$                ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1%)            ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2%)            ,ch(79),~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L40950
                  call "MANUAL" ("GLDEFINE")
                  goto L40430

L40950:        if keyhit% <> 15% then L40990
                  call "PRNTSCRN"
                  goto L40430

L40990:        if fieldnr% <> 0% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *          I N P U T  /  E D I T  S C R E E N   2           *~
            *                                                           *~
            * MANAGES THE LINE DETAIL                                   *~
            *************************************************************

            deffn'105(screen%, fieldnr%)
                gosub set_keys
                init(hex(8c)) hfac$()
                header1$ = "Last Report Updated: " & lastreport$
                str(header1$,62%) = "GLDEFINE: " & cms2v$
                if fieldnr%=0% then init(hex(86)) hfac$()

                  on fieldnr% gosub L41390,         /* Number of Columns*/~
                                    L41360,         /* Desc Column Text */~
                                    L41360,         /* Column 1         */~
                                    L41360,         /* Column 2         */~
                                    L41360,         /* Column 3         */~
                                    L41360,         /* Column 4         */~
                                    L41360,         /* Column 5         */~
                                    L41360,         /* Column 6         */~
                                    L41360,         /* Column 7         */~
                                    L41360,         /* Column 8         */~
                                    L41360,         /* Column 9         */~
                                    L41360          /* Column 10        */
                     goto L41430

*        Set FAC'S for Upper/Lower case input...
                      hfac$(fieldnr%) = hex(80)
                      return
L41360
*        Set FAC'S for Upper case only input...
                      hfac$(fieldnr%) = hex(81)
                      return
L41390
*        Set FAC'S for Numeric only input...
                      hfac$(fieldnr%) = hex(82)
                      return

L41430: accept                                                           ~
               at (01,02), "Financial Statements Report Specifications", ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header1$                ,ch(79),~
               at (03,02), fac(hex(94)), errormsg$               ,ch(79),~
                                                                         ~
               at (05,02), "Number of Columns",                          ~
               at (05,20), fac(hfac$(1%)), columns$              ,ch(02),~
                                                                         ~
               at (06,02), "Description Column Text",                    ~
               at (06,26), fac(hfac$(2%)), coltext$              ,ch(16),~
                                                                         ~
               at (08,02), "COL  POSITION  LENGTH  FORMAT  COMMA?     TEX~
        ~T LINE 1        TEXT LINE 2",                                    ~
               at (09,02), fac(hex(8c)), col$(1%)                ,ch(03),~
               at (10,02), fac(hex(8c)), col$(2%)                ,ch(03),~
               at (11,02), fac(hex(8c)), col$(3%)                ,ch(03),~
               at (12,02), fac(hex(8c)), col$(4%)                ,ch(03),~
               at (13,02), fac(hex(8c)), col$(5%)                ,ch(03),~
               at (14,02), fac(hex(8c)), col$(6%)                ,ch(03),~
               at (15,02), fac(hex(8c)), col$(7%)                ,ch(03),~
               at (16,02), fac(hex(8c)), col$(8%)                ,ch(03),~
               at (17,02), fac(hex(8c)), col$(9%)                ,ch(03),~
               at (18,02), fac(hex(8c)), col$(10%)               ,ch(03),~
               at (09,10), fac(hfac$(3%)), colpos$(1%)           ,ch(03),~
               at (10,10), fac(hfac$(4%)), colpos$(2%)           ,ch(03),~
               at (11,10), fac(hfac$(5%)), colpos$(3%)           ,ch(03),~
               at (12,10), fac(hfac$(6%)), colpos$(4%)           ,ch(03),~
               at (13,10), fac(hfac$(7%)), colpos$(5%)           ,ch(03),~
               at (14,10), fac(hfac$(8%)), colpos$(6%)           ,ch(03),~
               at (15,10), fac(hfac$(9%)), colpos$(7%)           ,ch(03),~
               at (16,10), fac(hfac$(10%)), colpos$(8%)          ,ch(03),~
               at (17,10), fac(hfac$(11%)), colpos$(9%)          ,ch(03),~
               at (18,10), fac(hfac$(12%)), colpos$(10%)         ,ch(03),~
               at (09,19), fac(hfac$(3%)), collgth$(1%)          ,ch(02),~
               at (10,19), fac(hfac$(4%)), collgth$(2%)          ,ch(02),~
               at (11,19), fac(hfac$(5%)), collgth$(3%)          ,ch(02),~
               at (12,19), fac(hfac$(6%)), collgth$(4%)          ,ch(02),~
               at (13,19), fac(hfac$(7%)), collgth$(5%)          ,ch(02),~
               at (14,19), fac(hfac$(8%)), collgth$(6%)          ,ch(02),~
               at (15,19), fac(hfac$(9%)), collgth$(7%)          ,ch(02),~
               at (16,19), fac(hfac$(10%)), collgth$(8%)         ,ch(02),~
               at (17,19), fac(hfac$(11%)), collgth$(9%)         ,ch(02),~
               at (18,19), fac(hfac$(12%)), collgth$(10%)        ,ch(02),~
               at (09,27), fac(hfac$(3%)), colfmt$(1%)           ,ch(01),~
               at (10,27), fac(hfac$(4%)), colfmt$(2%)           ,ch(01),~
               at (11,27), fac(hfac$(5%)), colfmt$(3%)           ,ch(01),~
               at (12,27), fac(hfac$(6%)), colfmt$(4%)           ,ch(01),~
               at (13,27), fac(hfac$(7%)), colfmt$(5%)           ,ch(01),~
               at (14,27), fac(hfac$(8%)), colfmt$(6%)           ,ch(01),~
               at (15,27), fac(hfac$(9%)), colfmt$(7%)           ,ch(01),~
               at (16,27), fac(hfac$(10%)), colfmt$(8%)          ,ch(01),~
               at (17,27), fac(hfac$(11%)), colfmt$(9%)          ,ch(01),~
               at (18,27), fac(hfac$(12%)), colfmt$(10%)         ,ch(01),~
               at (09,36), fac(hfac$(3%)), colcom$(1%)           ,ch(03),~
               at (10,36), fac(hfac$(4%)), colcom$(2%)           ,ch(03),~
               at (11,36), fac(hfac$(5%)), colcom$(3%)           ,ch(03),~
               at (12,36), fac(hfac$(6%)), colcom$(4%)           ,ch(03),~
               at (13,36), fac(hfac$(7%)), colcom$(5%)           ,ch(03),~
               at (14,36), fac(hfac$(8%)), colcom$(6%)           ,ch(03),~
               at (15,36), fac(hfac$(9%)), colcom$(7%)           ,ch(03),~
               at (16,36), fac(hfac$(10%)), colcom$(8%)          ,ch(03),~
               at (17,36), fac(hfac$(11%)), colcom$(9%)          ,ch(03),~
               at (18,36), fac(hfac$(12%)), colcom$(10%)         ,ch(03),~
               at (09,42), fac(hfac$(3%)), text1$(1%)            ,ch(16),~
               at (10,42), fac(hfac$(4%)), text1$(2%)            ,ch(16),~
               at (11,42), fac(hfac$(5%)), text1$(3%)            ,ch(16),~
               at (12,42), fac(hfac$(6%)), text1$(4%)            ,ch(16),~
               at (13,42), fac(hfac$(7%)), text1$(5%)            ,ch(16),~
               at (14,42), fac(hfac$(8%)), text1$(6%)            ,ch(16),~
               at (15,42), fac(hfac$(9%)), text1$(7%)            ,ch(16),~
               at (16,42), fac(hfac$(10%)), text1$(8%)           ,ch(16),~
               at (17,42), fac(hfac$(11%)), text1$(9%)           ,ch(16),~
               at (18,42), fac(hfac$(12%)), text1$(10%)          ,ch(16),~
               at (09,61), fac(hfac$(3%)), text2$(1%)            ,ch(16),~
               at (10,61), fac(hfac$(4%)), text2$(2%)            ,ch(16),~
               at (11,61), fac(hfac$(5%)), text2$(3%)            ,ch(16),~
               at (12,61), fac(hfac$(6%)), text2$(4%)            ,ch(16),~
               at (13,61), fac(hfac$(7%)), text2$(5%)            ,ch(16),~
               at (14,61), fac(hfac$(8%)), text2$(6%)            ,ch(16),~
               at (15,61), fac(hfac$(9%)), text2$(7%)            ,ch(16),~
               at (16,61), fac(hfac$(10%)), text2$(8%)           ,ch(16),~
               at (17,61), fac(hfac$(11%)), text2$(9%)           ,ch(16),~
               at (18,61), fac(hfac$(12%)), text2$(10%)          ,ch(16),~
                                                                         ~
               at (20,02), fac(hex(84)), length_msg$             ,ch(79),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$                ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1%)            ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2%)            ,ch(79),~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L41910
                  call "MANUAL" ("GLDEFINE")
                  goto L41430

L41910:        if keyhit% <> 15% then L41950
                  call "PRNTSCRN"
                  goto L41430

L41950:        if fieldnr% <> 0% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *          I N P U T  /  E D I T  S C R E E N   2           *~
            *                                                           *~
            * MANAGES LINE DETAIL                                       *~
            *************************************************************

            deffn'102(screen%, r%, fieldnr%)
                  gosub set_keys
                  convert r% to r$, pic(##)
                  init(hex(8c)) lfac$()
                  if fieldnr%=0% then init(hex(86)) lfac$()
                  header1$ = "REPORT:" & hex(a4) & report$ & hex(ac) &   ~
                                                                   descr$
                  str(header1$,62%) = "GLDEFINE: " & cms2v$

*        Format line display for top of screen...
            if fieldnr% > 1% then L42480
            k% = max(0%, c%-2%)
            display$(), range$() = " "
            save% = c%
            for i% = 1% to 3%
                c% = k%+i%
                if c% > maxlines% then L42360
                if inst$(c%,r%) = " " then L42300
                range$(i%)= acct1$(c%,r%)
                if acct2$(c%,r%) <> " " then range$(i%)=range$(i%)       ~
                                         &  " - "& acct2$(c%,r%)
                if group$(c%,r%) <> " " then range$(i%) =                ~
                                         "Account Group: " & group$(c%,r%)
                if range$(i%) <> " " then L42300
                     gosub'152(r%,1%) : range$(i%) = instdescr$
                     if errormsg$<>" " then range$(i%)=str(errormsg$,,19%)
L42300:         temp$ = hex(8c) : if c% = save% then temp$ = hex(84)
                put display$(i%), using L42340, temp$, line$(c%),         ~
                    inst$(c%,r%), range$(i%), print$(c%,r%), text$(c%,r%)

L42340:         FMT CH(1), CH(4), XX(2), CH(4), XX(3), CH(25), XX(2),    ~
                    CH(3), XX(3), CH(31)
L42360:     next i%

            c% = save%
            errormsg$ = " "
            gosub'152(r%, 1%) /* Reset Instruction Description */
            str(display$(3%),,1%) = " "
            if fieldnr% > 0% then L42480
                if str(inst$(c%,r%),,1%) = "T" then L42460
L42460:         tran(total$(c%,r%), hex(0b20))replacing

L42480:           on fieldnr% gosub L42660,         /* INSTRUCTION      */~
                                    L42660,         /* ACCOUNTS         */~
                                    L42660,         /* ZONE OVERRIDE    */~
                                    L42660,         /* PRINT?           */~
                                    L42660,         /* PRINT RESTRICTION*/~
                                    L42660,         /* REVERSE SIGNS    */~
                                    L42630,         /* TEXT FOR PRINT   */~
                                    L42660          /* TOTALING FLAGS   */
                     goto L42730

L42630
*        Set FAC'S for Upper/Lower case input...
                      lfac$(fieldnr%) = hex(80)
                      return
L42660
*        Set FAC'S for upper case only input...
                      lfac$(fieldnr%) = hex(81)
                      return
*        Set FAC'S for Numeric only input...
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
               at (05,02), fac(hex(8c)), display$(1%)            ,ch(79),~
               at (06,02), fac(hex(8c)), display$(2%)            ,ch(79),~
               at (07,02), fac(hex(ac)), display$(3%)            ,ch(79),~
                                                                         ~
               at (08,68), fac(hex(84)),   collit$               ,ch(07),~
               at (08,77), fac(hex(84)),   r$                    ,ch(02),~
                                                                         ~
               at (09,03), "Instruction Code",                           ~
               at (09,22), fac(lfac$(1%)),  inst$(c%, r%)        ,ch(04),~
               at (09,27), fac(hex(8c)),   instdescr$            ,ch(32),~
                                                                         ~
               at (11,03), "Account Group Code",                         ~
               at (11,22), fac(lfac$(2%)),  group$(c%, r%)       ,ch(06),~
               at (11,32), "Or G/L Accounts",                            ~
               at (11,48), fac(lfac$(2%)),  acct1$(c%, r%)       ,ch(12),~
               at (11,61), "-",                                          ~
               at (11,64), fac(lfac$(2%)),  acct2$(c%, r%)       ,ch(12),~
                                                                         ~
               at (12,03), "Ignore Run Time Zone Selections?",           ~
               at (12,36), fac(lfac$(3%)),  fixed$(c%, r%)       ,ch(03),~
                                                                         ~
               at (13,03), "Print Column On Report?",                    ~
               at (13,27), fac(lfac$(4%)),  print$(c%, r%)       ,ch(03),~
               at (13,32), "As Long As Amount Is:",                      ~
               at (13,54), fac(lfac$(5%)),  print_if$(c%, r%)    ,ch(01),~
               at (13,58), "(+, -, or blank)",                           ~
                                                                         ~
               at (14,03), "Reverse Signs?",                             ~
               at (14,22), fac(lfac$(6%)),  reverse$(c%, r%)     ,ch(03),~
                                                                         ~
               at (15,03), "Text To Print",                              ~
               at (15,22), fac(lfac$(07%)), text$(c%, r%)        ,ch(45),~
                                                                         ~
               at (18,03), "Totaling Control Specifications:",           ~
               at (19,08), "Bucket #:  0  1  2  3  4  5  6  7  8  9 10 11~
        ~ 12 13 14 15 16 17 18 19 ",                                      ~
              at (20,19),fac(lfac$(8%)),str(total$(c%,r%),1%,1%) ,ch(01),~
              at (20,22),fac(lfac$(8%)),str(total$(c%,r%),2%,1%) ,ch(01),~
              at (20,25),fac(lfac$(8%)),str(total$(c%,r%),3%,1%) ,ch(01),~
              at (20,28),fac(lfac$(8%)),str(total$(c%,r%),4%,1%) ,ch(01),~
              at (20,31),fac(lfac$(8%)),str(total$(c%,r%),5%,1%) ,ch(01),~
              at (20,34),fac(lfac$(8%)),str(total$(c%,r%),6%,1%) ,ch(01),~
              at (20,37),fac(lfac$(8%)),str(total$(c%,r%),7%,1%) ,ch(01),~
              at (20,40),fac(lfac$(8%)),str(total$(c%,r%),8%,1%) ,ch(01),~
              at (20,43),fac(lfac$(8%)),str(total$(c%,r%),9%,1%) ,ch(01),~
              at (20,46),fac(lfac$(8%)),str(total$(c%,r%),10%,1%),ch(01),~
              at (20,49),fac(lfac$(8%)),str(total$(c%,r%),11%,1%),ch(01),~
              at (20,52),fac(lfac$(8%)),str(total$(c%,r%),12%,1%),ch(01),~
              at (20,55),fac(lfac$(8%)),str(total$(c%,r%),13%,1%),ch(01),~
              at (20,58),fac(lfac$(8%)),str(total$(c%,r%),14%,1%),ch(01),~
              at (20,61),fac(lfac$(8%)),str(total$(c%,r%),15%,1%),ch(01),~
              at (20,64),fac(lfac$(8%)),str(total$(c%,r%),16%,1%),ch(01),~
              at (20,67),fac(lfac$(8%)),str(total$(c%,r%),17%,1%),ch(01),~
              at (20,70),fac(lfac$(8%)),str(total$(c%,r%),18%,1%),ch(01),~
              at (20,73),fac(lfac$(8%)),str(total$(c%,r%),19%,1%),ch(01),~
              at (20,76),fac(lfac$(8%)),str(total$(c%,r%),20%,1%),ch(01),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$                ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1%)            ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2%)            ,ch(79),~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               tran(total$(c%,r%), hex(200b))replacing
               if keyhit% <> 13% then L43530
                  call "MANUAL" ("GLDEFINE")
                  goto L42730

L43530:        if keyhit% <> 15% then L43570
                  call "PRNTSCRN"
                  goto L42730

L43570:        if fieldnr% <> 0% or keyhit% > 0% then return
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
            screen% = 5%
            goto L44170

            deffn'112(fieldnr%)    /* Delete Mode */
            init (hex(8c)) lfac$(), hfac$()
            hfac$(fieldnr%),lfac$(fieldnr%)=hex(94)
            screen% = 6%

L44170:     range$() = " "
            save% = c%
            for i% = 1% to 14%
                c% = b%+i%
                if /*INST$(C%,1%) <> " " AND*/ c% <= maxlines% then L44240
                     hfac$(i%), lfac$(i%) = hex(9c)
                     goto L44340
L44240:         range$(i%) = acct1$(c%,1%)
                if acct2$(c%,1%) <> " " then range$(i%) = range$(i%) &   ~
                                                    " - "& acct2$(c%,1%)
                if group$(c%,1%)<>" " then range$(i%)="Account Group: " &~
                                                           group$(c%,1%)
                     if range$(i%) <> " " then L44340
                     gosub'152(1%, 1%)
                     range$(i%) = instdescr$
                     if errormsg$<>" " then range$(i%)=str(errormsg$,,19)
L44340:     next i%

            c% = save%
            gosub set_keys
            header1$ = "Report Summary Screen "
            str(header1$,62%) = "GLDEFINE: " & cms2v$

L44400:     accept                                                       ~
               at (01,02), "Financial Statements Report Specifications", ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header1$                ,ch(79),~
               at (04,02), "REPORT NUMBER:",                             ~
               at (04,17), fac(hex(84)),   report$               ,ch(03),~
               at (04,21), fac(hex(8c)),   descr$                ,ch(30),~
               at (06,02), fac(hex(ae)),   header$               ,ch(79),~
                                                                         ~
               at (07,02), fac(hfac$( 1%)),  line$  (b%+ 1%)     ,ch(04),~
               at (08,02), fac(hfac$( 2%)),  line$  (b%+ 2%)     ,ch(04),~
               at (09,02), fac(hfac$( 3%)),  line$  (b%+ 3%)     ,ch(04),~
               at (10,02), fac(hfac$( 4%)),  line$  (b%+ 4%)     ,ch(04),~
               at (11,02), fac(hfac$( 5%)),  line$  (b%+ 5%)     ,ch(04),~
               at (12,02), fac(hfac$( 6%)),  line$  (b%+ 6%)     ,ch(04),~
               at (13,02), fac(hfac$( 7%)),  line$  (b%+ 7%)     ,ch(04),~
               at (14,02), fac(hfac$( 8%)),  line$  (b%+ 8%)     ,ch(04),~
               at (15,02), fac(hfac$( 9%)),  line$  (b%+ 9%)     ,ch(04),~
               at (16,02), fac(hfac$(10%)),  line$  (b%+10%)     ,ch(04),~
               at (17,02), fac(hfac$(11%)),  line$  (b%+11%)     ,ch(04),~
               at (18,02), fac(hfac$(12%)),  line$  (b%+12%)     ,ch(04),~
               at (19,02), fac(hfac$(13%)),  line$  (b%+13%)     ,ch(04),~
               at (20,02), fac(hfac$(14%)),  line$  (b%+14%)     ,ch(04),~
                                                                         ~
               at (07,08), fac(lfac$( 1)),  inst$  (b%+ 1,1%)    ,ch(04),~
               at (08,08), fac(lfac$( 2)),  inst$  (b%+ 2,1%)    ,ch(04),~
               at (09,08), fac(lfac$( 3)),  inst$  (b%+ 3,1%)    ,ch(04),~
               at (10,08), fac(lfac$( 4)),  inst$  (b%+ 4,1%)    ,ch(04),~
               at (11,08), fac(lfac$( 5)),  inst$  (b%+ 5,1%)    ,ch(04),~
               at (12,08), fac(lfac$( 6)),  inst$  (b%+ 6,1%)    ,ch(04),~
               at (13,08), fac(lfac$( 7)),  inst$  (b%+ 7,1%)    ,ch(04),~
               at (14,08), fac(lfac$( 8)),  inst$  (b%+ 8,1%)    ,ch(04),~
               at (15,08), fac(lfac$( 9)),  inst$  (b%+ 9,1%)    ,ch(04),~
               at (16,08), fac(lfac$(10)),  inst$  (b%+10,1%)    ,ch(04),~
               at (17,08), fac(lfac$(11)),  inst$  (b%+11,1%)    ,ch(04),~
               at (18,08), fac(lfac$(12)),  inst$  (b%+12,1%)    ,ch(04),~
               at (19,08), fac(lfac$(13)),  inst$  (b%+13,1%)    ,ch(04),~
               at (20,08), fac(lfac$(14)),  inst$  (b%+14,1%)    ,ch(04),~
                                                                         ~
               at (07,13), fac(lfac$( 1%)),  range$ ( 1%)        ,ch(25),~
               at (08,13), fac(lfac$( 2%)),  range$ ( 2%)        ,ch(25),~
               at (09,13), fac(lfac$( 3%)),  range$ ( 3%)        ,ch(25),~
               at (10,13), fac(lfac$( 4%)),  range$ ( 4%)        ,ch(25),~
               at (11,13), fac(lfac$( 5%)),  range$ ( 5%)        ,ch(25),~
               at (12,13), fac(lfac$( 6%)),  range$ ( 6%)        ,ch(25),~
               at (13,13), fac(lfac$( 7%)),  range$ ( 7%)        ,ch(25),~
               at (14,13), fac(lfac$( 8%)),  range$ ( 8%)        ,ch(25),~
               at (15,13), fac(lfac$( 9%)),  range$ ( 9%)        ,ch(25),~
               at (16,13), fac(lfac$(10%)),  range$ (10%)        ,ch(25),~
               at (17,13), fac(lfac$(11%)),  range$ (11%)        ,ch(25),~
               at (18,13), fac(lfac$(12%)),  range$ (12%)        ,ch(25),~
               at (19,13), fac(lfac$(13%)),  range$ (13%)        ,ch(25),~
               at (20,13), fac(lfac$(14%)),  range$ (14%)        ,ch(25),~
                                                                         ~
               at (07,40), fac(lfac$( 1)),  print$ (b%+ 1,1%)    ,ch(03),~
               at (08,40), fac(lfac$( 2)),  print$ (b%+ 2,1%)    ,ch(03),~
               at (09,40), fac(lfac$( 3)),  print$ (b%+ 3,1%)    ,ch(03),~
               at (10,40), fac(lfac$( 4)),  print$ (b%+ 4,1%)    ,ch(03),~
               at (11,40), fac(lfac$( 5)),  print$ (b%+ 5,1%)    ,ch(03),~
               at (12,40), fac(lfac$( 6)),  print$ (b%+ 6,1%)    ,ch(03),~
               at (13,40), fac(lfac$( 7)),  print$ (b%+ 7,1%)    ,ch(03),~
               at (14,40), fac(lfac$( 8)),  print$ (b%+ 8,1%)    ,ch(03),~
               at (15,40), fac(lfac$( 9)),  print$ (b%+ 9,1%)    ,ch(03),~
               at (16,40), fac(lfac$(10)),  print$ (b%+10,1%)    ,ch(03),~
               at (17,40), fac(lfac$(11)),  print$ (b%+11,1%)    ,ch(03),~
               at (18,40), fac(lfac$(12)),  print$ (b%+12,1%)    ,ch(03),~
               at (19,40), fac(lfac$(13)),  print$ (b%+13,1%)    ,ch(03),~
               at (20,40), fac(lfac$(14)),  print$ (b%+14,1%)    ,ch(03),~
                                                                         ~
               at (07,46), fac(lfac$( 1%)),  text$  (b%+ 1%,1%)  ,ch(31),~
               at (08,46), fac(lfac$( 2%)),  text$  (b%+ 2%,1%)  ,ch(31),~
               at (09,46), fac(lfac$( 3%)),  text$  (b%+ 3%,1%)  ,ch(31),~
               at (10,46), fac(lfac$( 4%)),  text$  (b%+ 4%,1%)  ,ch(31),~
               at (11,46), fac(lfac$( 5%)),  text$  (b%+ 5%,1%)  ,ch(31),~
               at (12,46), fac(lfac$( 6%)),  text$  (b%+ 6%,1%)  ,ch(31),~
               at (13,46), fac(lfac$( 7%)),  text$  (b%+ 7%,1%)  ,ch(31),~
               at (14,46), fac(lfac$( 8%)),  text$  (b%+ 8%,1%)  ,ch(31),~
               at (15,46), fac(lfac$( 9%)),  text$  (b%+ 9%,1%)  ,ch(31),~
               at (16,46), fac(lfac$(10%)),  text$  (b%+10%,1%)  ,ch(31),~
               at (17,46), fac(lfac$(11%)),  text$  (b%+11%,1%)  ,ch(31),~
               at (18,46), fac(lfac$(12%)),  text$  (b%+12%,1%)  ,ch(31),~
               at (19,46), fac(lfac$(13%)),  text$  (b%+13%,1%)  ,ch(31),~
               at (20,46), fac(lfac$(14%)),  text$  (b%+14%,1%)  ,ch(31),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$                ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1%)            ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2%)            ,ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L45660
                  call "MANUAL" ("GLDEFINE")
                  goto L44400

L45660:        if keyhit% <> 15% then L45700
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

*        Input Mode Screen Controler...
            deffn'103(fieldnr%)
                  pfdescr$(1%)="(1)Start Over                          "&~
                               "     (13)Instructions   (15)Print Screen"
                  pfdescr$(2%)="                                       "&~
                               "                (16)Cancel Print Request"
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  str(pfdescr$(2%),55%,1%)=hex(84)
                  header$ = "Print Format Control Records"
                  str(header$,62%) = "GLDEFINE: " & cms2v$
                  on fieldnr% gosub L46240,         /* START REPORT     */~
                                    L46240          /* END REPORT       */
                  goto L46310

*        Sst FAC'S for Upper/Lower case input...
                      lfac$(fieldnr%) = hex(80)
                      return
L46240
*        Set FAC'S for Upper case only input...
                      lfac$(fieldnr%) = hex(81)
                      return
*        Set FAC'S for Numeric only input...
                      lfac$(fieldnr%) = hex(82)
                      return

L46310:     accept                                                       ~
               at (01,02), "Financial Statements Report Specifications", ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Starting Report Id.",                                 ~
               at (06,30), fac(lfac$( 1%)), startcode$          , ch(03),~
               at (07,02),                                               ~
                  "Ending Report Id.",                                   ~
               at (07,30), fac(lfac$( 2%)), endcode$            , ch(03),~
                                                                         ~
               at (22,02), fac(hex(a4)),   message$             , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(1%)         , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(2%)         , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L46540
                  call "MANUAL" ("GLGRPINP")
                  goto L46310

L46540:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L46310

        REM *************************************************************~
            *                    S C R E E N  IIII                      *~
            *                                                           *~
            * Handles Copy Function.                                    *~
            *************************************************************

*        Input Mode Screen Controler...
            deffn'104(fieldnr%)
                  pfdescr$(1%)="(1)Start Over                          "&~
                               "     (13)Instructions   (15)Print Screen"
                  pfdescr$(2%)="                                       "&~
                               "                 (16)Cancel Copy Request"
                  pfkeys$ = hex(00010d0f10)
                  init(hex(8c)) lfac$()
                  str(pfdescr$(2%),55%,1%)=hex(84)
                  header$ = "Copy Format Control File"
                  on fieldnr% gosub L47240,         /* COPY FROM        */~
                                    L47240          /* COPY TO          */
                  goto L47310

*        Set FAC'S for Upper/Lower case Input...
                      lfac$(fieldnr%) = hex(80)
                      return
L47240
*        Set FAC'S for Upper case only input...
                      lfac$(fieldnr%) = hex(81)
                      return
*        Set FAC'S for Numeric only input...
                      lfac$(fieldnr%) = hex(82)
                      return

L47310:     accept                                                       ~
               at (01,02), "Financial Statements Report Specifications", ~
               at (01,67), "DATE:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Report To Be Copied",                                 ~
               at (06,30), fac(lfac$(1%)), startcode$           , ch(03),~
               at (06,35), fac(hex(84)),   display$(1%)         , ch(34),~
               at (07,02),                                               ~
                  "Id. For New Report",                                  ~
               at (07,30), fac(lfac$(2%)), endcode$             , ch(03),~
                                                                         ~
               at (22,02), fac(hex(a4)),   message$             , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(1%)         , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(2%)         , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L47550
                  call "MANUAL" ("GLGRPINP")
                  goto L47310

L47550:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L47310

        REM *************************************************************~
            *                S H O W   L E G E N D                      *~
            *                                                           *~
            * Shows Some Summary Documentation For Quick Referencing.   *~
            *************************************************************

        show_legend

        accept                                                           ~
            at (01,72), "Page 1",                                        ~
            at (02,29), "INSTRUCTION CODE LEGEND",                       ~
            at (04,02), "Instruction :",                                 ~
            at (04,19), "H  - Set Header Message",                       ~
            at (04,47), "P   - New Page; Print Header",                  ~
            at (05,02), "   Codes    :",                                 ~
            at (05,19), "D  - Print Subtitle",                           ~
            at (05,47), "DC  - Print Subtitle Centered",                 ~
            at (06,02), "            :",                                 ~
            at (06,19), "U  - Print Underlines",                         ~
            at (06,47), "DU  - Print Double Underlines",                 ~
            at (08,35), "Opening                Current   Year-End",     ~
            at (09,35), "Balance    Activity    Balance    Balance",     ~
            at (10,02), "Current Period              :",                 ~
            at (10,36), "  PO          PA          CB          -",       ~
            at (11,02), "Previous Period             :",                 ~
            at (11,36), " PPO         PPA           -          -",       ~
            at (12,02), "Current Year                :",                 ~
            at (12,36), "  YO          YA           -          -",       ~
            at (13,02), "Previous Year               :",                 ~
            at (13,36), " LYO         LYA         LCB          -",       ~
            at (14,02), "Period Last Year            :",                 ~
            at (14,36), " LPO         LPA           -          -",       ~
            at (15,02), "Current Budget Period       :",                 ~
            at (15,36), " BPO         BPA           -          -",       ~
            at (16,02), "Current Budget Year         :",                 ~
            at (16,36), "   -         BYA         BCB        BYE",       ~
            at (17,02), "Previous Year Budget Period :",                 ~
            at (17,36), "PBPO        PBPA           -          -",       ~
            at (18,02), "Previous Year Budget Year   :",                 ~
            at (18,36), "   -        PBYA        PBCB       PBYE",       ~
            at (19,02), "Next Year Budget Period     :",                 ~
            at (19,36), "NBPO        NBPA           -          -",       ~
            at (20,02), "Next Year Budget Year       :",                 ~
            at (20,36), "   -        NBYA        NBCB          -",       ~
            at (22,14), "Press PF-5 For Next Page of Legend, ENTER to Ret~
        ~urn",                                                            ~
                                                                         ~
               keys(hex(00050d0f)),                                      ~
               key (mary%)

               if mary% <> 13% then L48134
                  call "MANUAL" ("GLGRPINP")
                  goto show_legend

L48134:        if mary% <> 15% then L48142
                  call "PRNTSCRN"
                  goto show_legend

L48142:        if mary% <> 5% then return

L48200: accept                                                           ~
            at (01,72), "Page 2",                                        ~
            at (02,29), "INSTRUCTION CODE LEGEND",                       ~
            at (05,02), "Calculations    :",                             ~
            at (05,21), "N+, N-, N/, N* & N% - Calculations on columns or~
        ~ totals",                                                        ~
            at (06,02), "                :",                             ~
            at (06,21), "T0 to T19 - Manipulate respective totalling buck~
        ~ets",                                                            ~
            at (07,02), "                :",                             ~
            at (07,21), "'+' ADD to bucket, '-' SUBTRACT from bucket, '0'~
        ~ ZERO",                                                          ~
            at (10,02), "    Print       :",                             ~
            at (10,21), "NO  - This format line is NOT to generate a repo~
        ~rt line",                                                        ~
            at (11,02), "   Options      :",                             ~
            at (11,21), "SUM - Print sum of all indicated accounts on ONE~
        ~ line",                                                          ~
            at (12,02), "                :",                             ~
            at (12,21), "ALL - Print EACH indicated account as an individ~
        ~ual line",                                                       ~
            at (15,02), "    Print       :",                             ~
            at (15,21), "'+' - Ignore line(s) when NOT POSITIVE (no total~
        ~ling)",                                                          ~
            at (16,02), " Restrictions   :",                             ~
            at (16,21), "'-' - Ignore line(s) when NOT NEGATIVE (no total~
        ~ling)",                                                          ~
            at (17,02), "                :",                             ~
            at (17,21), "' ' - Ignore this feature",                     ~
            at (22,12), "Press PF-4 For Previous Page of Legend, ENTER to~
        ~ Return",                                                        ~
                                                                         ~
               keys(hex(00040d0f)),                                      ~
               key (mary%)

               if mary% <> 13% then L48660
                  call "MANUAL" ("GLGRPINP")
                  goto L48200

L48660:        if mary% <> 15% then L48700
                  call "PRNTSCRN"
                  goto L48200

L48700:        if mary% <> 4% then return
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
                            L49545,         /* All Scrns, Edit Field    */~
                            L49590,         /* Header 2 Input Mode      */~
                            L49800          /* Header 2 Edit Mode       */

L49075
*        ...................... Header, Input mode .....................
            pfdescr$(1%)= "(1)Start Over    (4)Previous Field     (10)Cop~
        ~y Report         (15)Print Screen"
            pfdescr$(2%)= "                 (9)Print Formats      (13)Ins~
        ~tructions        (16)Exit Program"
            pfkeys$ = hex(000104090a0d0f10ffffffffffffffff)
            str(pfdescr$(2%),63%,1%) = hex(84)

*        Turn OFF appropriate fields...
            if fieldnr% = 1% then L49160
                str(pfdescr$(2%),63%)    = " "  /* Shut Off Exit Optn  */
                str(pfkeys$,8%,1%) = hex(ff)
                str(pfdescr$(2%),18%,16%) = " " /* Shut Off Print Optn */
                str(pfkeys$,4%,1%) = hex(ff)
                str(pfdescr$(1%),40%,15%) = " " /* Shut Off Copy Option*/
                str(pfkeys$,5%,1%) = hex(ff)
                goto L49170
L49160:     str(pfdescr$(1%),,35%) = " "        /* Shut Off Prev Field */
            str(pfkeys$,2%,2%) = hex(ffff)
L49170: return

L49180
*        .................... Header, Edit mode ........................
            pfdescr$(1%)= "(1)Start Over  (5)Next Screen          (13)Ins~
        ~tructions        (15)Print Screen"
            pfdescr$(2%)= "(2)Review/Edit Report Detail     (12)Delete Re~
        ~port                (16)Save Data"
            pfkeys$ = hex(0001020c0d0f1005ffffffffffffffff)
            str(pfdescr$(2%),66%,1%) = hex(84)
            if screen% = 2% then L49215
                str(pfdescr$(2%),35%,17%) = " "
                str(pfkeys$,4%,1%) = hex(ff)
L49215: return

L49225
*        .................. Line Items, Input mode .....................
            pfdescr$(1%)= "(1)Start Over    (4)Previous Field     (13)Ins~
        ~tructions        (15)Print Screen"
            pfdescr$(2%)= "(2)Restart Line  (6)Same As Prev Line  (14)Leg~
        ~end                              "
            pfkeys$ = hex(00010204060d0e0fffffffffffffffff)

*        Turn OFF appropriate fields...
            if c% > 1% then L49280
                str(pfdescr$(2%),18%,20%) = " " /* Shut Off Prev Line  */
                str(pfkeys$,5%,1%) = hex(ff)
L49280:     if fieldnr% <> 1% then L49315
                str(pfdescr$(1%),18%,17%) = " " /* Shut Off Prev Field */
                str(pfkeys$,4%,1%) = hex(ff)
                str(pfdescr$(2%),66%) = hex(84) & "(16)Edit Mode"
                str(pfkeys$,9%,1%) = hex(10)
                if insert% = 0% then L49315
                  str(pfdescr$(2%),58%) = hex(84)&"(16)Return To Summary"
L49315: return

L49325
*        ................... Line Items, Edit mode .....................
            pfdescr$(1%)= "(1)Start Over (4/6)Previous Line (20)Prev Col ~
        ~(13)Instructions (15)Print Screen"
            pfdescr$(2%)= "(2)First Line (5/7)Next Line     (21)Next Col ~
        ~(14)Legend  (16)Return To Summary"
            pfkeys$ = hex(000102040506070d0e0f10031415ffff)
            str(pfdescr$(2%),58%,1%) = hex(84)

*        Turn OFF appropriate fields...
            if c% > 1% then L49390
                str(pfdescr$(1%),15%,18%) = " "  /* Shut Off Prev Line */
                str(pfdescr$(2%),,13%)    = " " /* Shut Off First Line */
                str(pfkeys$,3%,2%), str(pfkeys$,6%,1%) = hex(ffff)
L49390:     if c% < maxlines% then L49401
                str(pfdescr$(2%),15%,14%)  = " " /* Shut Off Next Line */
                str(pfkeys$,5%,1%), str(pfkeys$,7%,1%) = hex(ff)
L49401:     if r% > 1% then L49404
                str(pfdescr$(1%),34%,12%)  = " " /* Shut Off Prev Col  */
                str(pfkeys$,13%,1%) = hex(ff)
L49404:     if r% < columns% then L49410
                str(pfdescr$(2%),34%,13%)  = " " /* Shut Off Next Line */
                str(pfkeys$,14%,1%) = hex(ff)
L49410: return

L49415
*        ................ Summary Screen, Display mode .................
            pfdescr$(1%)= "(1)Start Over  (4/6)Prev Lines  (9)Header     ~
        ~(12)Delete Line  (15)Print Screen"
            pfdescr$(2%)= "(2)First Line  (5/7)Next Lines  (11)Add Line  ~
        ~(13)Instructions    (16)Save Data"
            pfkeys$ = hex(00010204050607090b0c0d0e0f1003ffff)
            str(pfdescr$(2%),66%,1%) = hex(84)

*        Turn OFF appropriate fields...
            if b% > 0% then L49480
                str(pfdescr$(2%),,13%)   = " "  /* Shut Off First Line */
                str(pfdescr$(1%),16%,15%) = " "  /* Shut Off Prev Scrn  */
                str(pfkeys$,3%,2%) = hex(ffff)
L49480:     if b%+14% < maxlines% then return
                str(pfdescr$(2%),16%,15%) = " "  /* Shut Off Next Scrn  */
                str(pfkeys$,5%,1%) = hex(ff)
        return

L49505
*        ................ Summary Screen, Delete mode ..................
            pfdescr$(1%)= "(1)Cancel Delete Request                      ~
        ~                 (15)Print Screen"
            pfdescr$(2%)= "(ENTER)Delete The Flashing Line             (1~
        ~3)Instructions                   "
            pfkeys$ = hex(00010d0fffffffffffffffffffffffff)
        return

L49545
*        ................. All Screens, Field Edit .....................
            pfdescr$(1%)= "(1)Start Over                          (13)Ins~
        ~tructions        (15)Print Screen"
            pfdescr$(2%)= "(ENTER) Validate Modification(s)              ~
        ~                                 "
            pfkeys$ = hex(00010d0e0fffffffffffffffffffffff)
        return

L49590
*        .................... Header 2, Input mode .....................
            pfdescr$(1%)= "(1)Start Over    (4)Previous Field            ~
        ~                 (15)Print Screen"
            pfdescr$(2%)= "                                       (13)Ins~
        ~tructions        (16)Exit Program"
            pfkeys$ = hex(0001040a0d0fffffffffffffffffffff)
            str(pfdescr$(2%),63%,1%) = hex(84)

        REM Turn OFF appropriate fields...
            if fieldnr% <> 1% then L49780
            str(pfdescr$(1%),,35%) = " "        /* Shut Off Prev Field */
            str(pfkeys$,2%,2%) = hex(ffff)
L49780: return

L49800
*        ................... Header 2, Edit mode .......................
            pfdescr$(1%)= "(1)Start Over  (4)Previous Screen      (13)Ins~
        ~tructions        (15)Print Screen"
            pfdescr$(2%)= "(2)Review/Edit Report Detail                  ~
        ~                    (16)Save Data"
            pfkeys$ = hex(000102040d0f1005ffffffffffffffff)
            str(pfdescr$(2%),66%,1%) = hex(84)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50190,         /* REPORT NUMBER    */~
                                    L50300,         /* DESCRIPTION      */~
                                    L50310,         /* SET OF BOOKS     */~
                                    L50340,         /* DEFAULT TITLE    */~
                                    L50370,         /* DEFAULT PAREN    */~
                                    L50440,         /* ROUND?           */~
                                    L50510,         /* PRINT IF ZERO?   */~
                                    L50580,         /* ZONE ONE         */~
                                    L50580,         /* ZONE TWO         */~
                                    L50580          /* ZONE THREE       */
                     return

L50190
*        Test data - Report Number...
                if report$ <> " " then goto L50250
                call "GETCODE" (#1, report$, " ", 0%, 0, f1%(1%))
                if f1%(1%) <> 0% then L50250
                    errormsg$ = hex(00)
                    return
L50250:         gosub L30000
                if onfile% = 0% then return
                return clear all
                goto editmode

L50300
*        Test data - Description...
                if descr$ = " " then errormsg$ = "Description CANNOT be"&~
                                                 " blank"
                return

L50310
*        Test data - G/L Books to Use...
             if dual_books$ <> "Y" then return
                 if set$ = "1" or set$ = "2" then L50318
                      errormsg$ = "G/L System MUST be '1' or '2'"
                      return
L50318:          if set$ <> "1" then L50322
                      setdescr$ = "Statutory"
                      mast% = 3%  :  gl% = 5%
                      goto L50326
L50322:          setdescr$ = "Local Authority"
                 mast% = 4%  :  gl% = 4%
L50326:          call "PUTPAREN" (setdescr$)
                 return

L50340
*         Test data - Default Report Title...
                return

L50370
*         Test data - default parenthesis option...
                if paren$ = " " then paren$ = "NO"
                if str(paren$,,1%) = "Y" then paren$ = "YES"
                if str(paren$,,1%) = "N" then paren$ = "NO"
                if paren$ = "YES" then return
                if paren$ = "NO " then return
                errormsg$ = "Parenthesis option MUST be 'YES' or 'NO'"
                return

L50440
*        Test data - Default Rounding Option...
                if round$ = " " then round$ = "NO"
                if str(round$,,1%) = "Y" then round$ = "YES"
                if str(round$,,1%) = "N" then round$ = "NO"
                if round$ = "YES" then return
                if round$ = "NO " then return
                errormsg$ = "Rounding option MUST be 'YES' or 'NO'"
                return

L50510
*        Test data - Default Print When Zero Option...
                if zero$ = " " then zero$ = "YES"
                if str(zero$,,1%) = "Y" then zero$ = "YES"
                if str(zero$,,1%) = "N" then zero$ = "NO"
                if zero$ = "YES" then return
                if zero$ = "NO " then return
                errormsg$ = "Print When Zero option MUST be 'YES' or 'NO'"
                return

L50580
*        Test data - Zone Specifications...
                k% = fieldnr% - 7%
                if zonename$(k%) = " " and zone$(k%,1%) = " " and        ~
                                           zone$(k%,2%) = " " then return
                convert zone$(k%,1%) to u3%, data goto L50740
                    if u3% < 1% or u3% > 12% then L50740
                convert u3% to zone$(k%,1%), pic(##)

                convert zone$(k%,2%) to u4%, data goto L50780
                    if u4% > 13%-u3% or u4% < 1% then L50780
                convert u4% to zone$(k%,2%), pic(##)

                if zonename$(k%) = " " then errormsg$ = "Zone Descripti"&~
                    "on CANNOT be blank"
                return

L50740:         errormsg$ = "Start position MUST be between 1 and 12"
                return

L50780:         u4% = 13% -u3% : convert u4% to temp$, pic(##)
                errormsg$ = "Length MUST be between 1 and "              ~
                             & str(temp$,,2%)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(r%, test%)
            errormsg$ = " "
                  on test%    gosub L51230,         /* INSTRUCTION      */~
                                    L51480,         /* ACCOUNTS         */~
                                    L51970,         /* ZONE OVERRIDE    */~
                                    L52050,         /* PRINT?           */~
                                    L52260,         /* PRINT RESTRICTION*/~
                                    L52300,         /* REVERSE SIGNS    */~
                                    L52390,         /* TEXT FOR PRINT   */~
                                    L52410          /* TOTALING FLAGS   */
                     return

L51230
*        Test data - Instruction Code...
            instdescr$ = " "
            search inst_codes$() = str(inst$(c%, r%)) to cursor%() step 4%
                if cursor%(1%) > 0% then L51300
            if r% = 1% and columns% = 1% then return
                if inst$(c%, r%) = " " then return
            errormsg$ = "Instruction Code is NOT valid, use PF14 to dis"&~
                        "play valid codes"
                return
L51300:     instdescr$ = inst_names$((cursor%(1%)+3%)/4%)
                if r% = 1% then return
            if inst$(c%, r%) = "H" or str(inst$(c%, r%),1%,2%) = "P " or ~
               inst$(c%, r%) = "D" or inst$(c%, r%) = "DC" then          ~
               errormsg$ = "Instruction Code is valid for column 1 ONLY"
            return

L51480
*        Test data - Account Specifications...
            if inst$(c%, r%) = "H" or str(inst$(c%, r%),1%,2%) = "P " or ~
               inst$(c%, r%) = "D" or inst$(c%, r%) = "DC" or            ~
               inst$(c%, r%) = "U" or inst$(c%, r%) = "DU" or            ~
               inst$(c%, r%) = " " then return
            if str(inst$(c%, r%),1%,1%) = "N" and                        ~
               str(inst$(c%, r%),3%,1%) = " " then L51720
            if str(inst$(c%, r%),,1%) = "T" then return
*        Pretest group code and account range...
            if group$(c%,r%) = " " and acct1$(c%,r%) = " " and           ~
               acct2$(c%,r%) = " " then L51600
            if group$(c%,r%) <> " " and acct1$(c%,r%) <> " " and         ~
               acct2$(c%,r%) <> " " then L51512 else L51515
L51512:            errormsg$ = "You MUST enter either a Group Code or G"&~
                               "/L Account Range, NOT both"
                   return
L51515:     if group$(c%,r%) <> " " then L51600

*        Test data - Account Number Range...
            if acct1$(c%,r%) = "?" then acct1$(c%,r%) = " "
            acctdescr$ = hex(0684) & "Select Starting G/L Account"
            if set$ = "1" then gl% = 5% else gl% = 6%
            call "GETCODE" (#gl%,acct1$(c%,r%),acctdescr$,1%,0,f1%(gl%))
                if f1%(gl%) = 1% then L51534
                    errormsg$ = "You MUST enter a Group Code or G/L Acc"&~
                                "ount Range"
                    return
L51534:     if acct2$(c%,r%) = "?" then acct2$(c%,r%) = " "
            acctdescr$ = hex(0684) & "Select Ending G/L Account"
            call "GETCODE" (#gl%,acct2$(c%,r%),acctdescr$,1%,0,f1%(gl%))
                if f1%(gl%) = 1% then L51544
                     acct2$(c%,r%) = acct1$(c%,r%)
L51544:     if acct1$(c%, r%) <= acct2$(c%, r%) then return
                errormsg$ = "Starting Account CANNOT be greater than En"&~
                            "ding Account"
                return

L51600
*        Test data - Account Group Code...
            if group$(c%,r%) = "?" then group$(c%,r%) = " "
            groupdescr$ = hex(0684) & "Select Account Group Code"
            call "GETCODE" (#mast%, group$(c%,r%), groupdescr$, 1%, 1,   ~
                           f1%(mast%))
            if f1%(mast%) = 1% then L51660
                errormsg$ = "You MUST enter a Group Code or G/L Account"&~
                            " Range"
                return
L51660:     get #mast%, using L51670, text$(c%,r%)
L51670:         FMT XX(36), CH(45)
            return

L51720
*        Test - Calculation Specifications...
            group$(c%,r%) = " "
            if acct1$(c%, r%) = " " and acct2$(c%, r%) = " " then L51965
            if str(acct1$(c%, r%),,1%) = "C" then L51820
            search inst_codes$()=str(acct1$(c%,r%),,4) to cursor%() step 4
            if cursor%(1%) > 44% and cursor%(1%) < 125% then L51770
            convert acct1$(c%, r%) to acct, data goto L51910 /* ERROR */
L51770:     if str(acct2$(c%, r%),,1%) = "C" then L51860
            search inst_codes$()=str(acct2$(c%,r%),,4) to cursor%() step 4
            if cursor%(1) > 44% and cursor%(1) < 125% then return
            convert acct2$(c%, r%) to acct, data goto L51940 /* ERROR */
            return

L51820:     convert str(acct1$(c%,r%),2%,2%) to acct1%, data goto L51910
            if acct1% < r% and acct1% > 0% then L51770
            errormsg$ = "Column Number MUST be LESS than the current co"&~
                        "lumn"
            return

L51860:     convert str(acct2$(c%,r%),2,2) to acct2%, data goto L51940
            if acct2% < r% and acct2% > 0% then return
            errormsg$ = "Column Number MUST be LESS than the current co"&~
                        "lumn"
            return

L51910:     errormsg$ = "Field 1 MUST be 'C'olumn and #, 'T'otal and # "&~
                        "or a numeric value"
            return

L51940:     errormsg$ = "Field 2 MUST be 'C'olumn and #, 'T'otal and # "&~
                        "or a numeric value"
            return

L51965:     errormsg$ = "Instruction Code REQUIRES entry of calculation"&~
                        " parameters"
            return

L51970
*        Test data - Override Zoning?...
            if str(fixed$(c%, r%),,1%) = "Y" then fixed$(c%, r%) = "YES"
            if str(fixed$(c%, r%),,1%) = "N" then fixed$(c%, r%) = "NO "
            if fixed$(c%, r%) = "YES" then return
            if fixed$(c%, r%) = "NO " then return
            if fixed$(c%, r%) = " " then return
                errormsg$ = "Ignore Run Time Zone Selections MUST be 'Y"&~
                            "ES' or 'NO'"
                return

L52050
*        Test data - Print Option...
            if print$(c%, r%) = "NO " then return
            if print$(c%, r%) = "ALL" then L52195
            if print$(c%, r%) = "SUM" then L52195
            if print$(c%, r%) <> "YES" then L52180
            if acct2$(c%, r%) <> " " then L52140  /* YES is invalid resp*/
            if group$(c%, r%) <> " " then L52140  /* For These Conditions*/
                if print$(c%, r%) <> "NO" then print$(c%, r%) = "YES"
                return

L52140:     if inst$(c%, r%) = "H" or str(inst$(c%,r%),1%,2%) = "P " or  ~
               inst$(c%, r%) = "D" or inst$(c%, r%) = "DC" or            ~
               inst$(c%, r%) = "U" or inst$(c%, r%) = "DU"               ~
                then return
L52180:         errormsg$ = "Print Column option MUST be 'SUM', 'ALL' o"&~
                            "r 'NO'"
                return

L52195:     if r% = 1% then L52200
                if print$(c%,r%) = print$(c%,p%) then return
                   errormsg$ = "Print Column option MUST be '"          &~
                                print$(c%,p%) & "' or 'NO'"
                   return
L52200:     for p% = r% to columns%
                if (print$(c%,p%) = "NO" or print$(c%,p%) = "YES")       ~
                   and p% <> r% then L52250
                print$(c%,p%) = print$(c%,r%)
            next p%
L52250:     return

L52260
*        Test data - Print Override Options...
            if pos(" +-" = print_if$(c%, r%)) = 0 then errormsg$ =       ~
                  "Print As Long As option MUST be '+', '-' or Blank"
                return

L52300
*        Test data - Reverse Sign Indicator...
            if reverse$(c%,r%)=" "and enabled%=1 then reverse$(c%,r%)="NO"
            if str(reverse$(c%,r%),,1%) = "Y" then reverse$(c%, r%)="YES"
            if str(reverse$(c%,r%),,1%) = "N" then reverse$(c%,r%)="NO "
            if reverse$(c%, r%) = " " then return
            if reverse$(c%, r%) = "YES" then return
            if reverse$(c%, r%) = "NO " then return
                errormsg$ = "Reverse Sign Indicator MUST be 'YES' or 'NO'"
                return

L52390
*        Test data - Text to Print?...
                return

L52410
*        Test data - Totaling specifications...
                for i% = 1% to 20%
                   if pos(" +-0" = str(total$(c%,r%),i%,1)) > 0 then L52470
                   errormsg$ = "Totaling Control MUST be '+', '-', '0' "&~
                               " or Blank"
                     return
L52470:         next i%
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

L53110
*        Test data - Starting Report ID...
            return

L53140
*        Test data - Ending Report ID...
            if endcode$ = " " then return
            if startcode$ > endcode$ then errormsg$ = "Starting Report "&~
                          "MUST be LESS than Ending Report"
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
L54110
*        Test data - Report to be Copied...
            call "GETCODE"(#1,startcode$,str(display$(1),,32),1%,0,f1%(1))
                if f1%(1%) = 0% then errormsg$ = hex(00)
            return

L54140
*        Test data - New Report ID...
            call "DESCRIBE" (#1, endcode$, " ", 0%, f1%(1%))
                if f1%(1%) <> 0% then errormsg$="New Report ID already "&~
                    "exists"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1A.                      *~
            *************************************************************

            deffn'155(fieldnr%)
                errormsg$ = " "
                  on fieldnr% gosub L55220,         /* NUMBER OF COLUMNS*/~
                                    L55330,         /* COL DESC TEXT    */~
                                    L55360,         /* COLUMN 1         */~
                                    L55360,         /* COLUMN 2         */~
                                    L55360,         /* COLUMN 3         */~
                                    L55360,         /* COLUMN 4         */~
                                    L55360,         /* COLUMN 5         */~
                                    L55360,         /* COLUMN 6         */~
                                    L55360,         /* COLUMN 7         */~
                                    L55360,         /* COLUMN 8         */~
                                    L55360,         /* COLUMN 9         */~
                                    L55360          /* COLUMN 10        */
                return

L55220
*        Test data - Number of Columns...
            save% = columns%
            call "NUMTEST" (columns$, 1, 10, errormsg$, 0, columns)
                if errormsg$ <> " " then return
            col$() = " "
            columns% = columns
            for limit% = 1% to columns%
                convert limit% to limit$, pic(##)
                col$(limit%) = limit$ & ")"
            next limit%
            if save% <= columns% then L55310
            for p% = columns% + 1% to save%
               colpos$(p%), collgth$(p%), colfmt$(p%), colcom$(p%),      ~
               text1$(p%), text2$(p%) = " "
            next p%
L55310:     return

L55330
*        Test data - Column Description Text...
            return

L55360
*        Test data - Column Information...
            t% = fieldnr% - 2%
            convert colpos$(t%) to colpos%(t%), data goto L55500
            if colpos%(t%) > 125% then L55532
                if t% > 1% then L55420
                    if colpos%(t%) < 18% then L55520
            goto L55430
L55420:     if colpos%(t%) < (colpos%(t%-1)+1%+collgth%(t%-1)) then L55540
L55430:         convert collgth$(t%) to collgth%(t%), data goto L55590
            if collgth%(t%) < 5% or collgth%(t%) > 16% then L55610
                if (colpos%(t%)+collgth%(t%)) > 132% then L55622
                    if t% = columns% then L55450
            if colpos$(t%+1) <> " " and colpos%(t%+1) < (colpos%(t%)+1%  ~
               +collgth%(t%)) then L55582
L55450:         if colfmt$(t%) <> "N" and colfmt$(t%) <> "%" and         ~
                   colfmt$(t%) <> "$" then L55630
            if colcom$(t%) = "YES" or colcom$(t%) = "NO" then return

            errormsg$ = "Comma Indicator MUST be 'YES' or 'NO'"
            return

L55500:     errormsg$ = "Invalid Column Number: " & colpos$(t%)
            return

L55520:     errormsg$ = "Column Position MUST be GREATER than 17"
            return

L55532:     errormsg$ = "Column Position MUST be LESS than 126"
            return

L55540:     convert t% to t$, pic(#)
            convert (colpos%(t%-1)+collgth%(t%-1)) to t1$, pic(###)
            errormsg$ = "Column " & t$ & " Position MUST be GREATER than"~
        &" " & t1$
            return

L55582:     convert (colpos%(t%+1)-collgth%(t%)) to t1$, pic(###)
            errormsg$ = "Based on Column Length of " & collgth$(t%) &    ~
                        ", Column Position MUST be LESS than " & t1$
            return

L55590:     errormsg$ = "Invalid Column Length: " & collgth$(t%)
            return

L55610:     errormsg$ = "Column Length MUST be between 5 and 16"
            return

L55622:     errormsg$ = "Column Position + Length MUST be LESS than 133"
            return

L55630:     errormsg$ = "Column Format MUST be 'N'umeric, '$' or '%'"
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

            call "SHOSTAT" ("Releasing Files, One Moment Please")

            for u3% = 1% to 64%
                if f2%(u3%) = 0% then close # u3%
                next u3%
            end
