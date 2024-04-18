        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   OOO    OOO   BBBB   Y   Y  PPPP    AAA   RRRR   TTTTT   *~
            *  O   O  O   O  B   B  Y   Y  P   P  A   A  R   R    T     *~
            *  O   O  O   O  BBBB    YYY   PPPP   AAAAA  RRRR     T     *~
            *  O   O  O   O  B   B    Y    P      A   A  R   R    T     *~
            *   OOO    OOO   BBBB     Y    P      A   A  R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * OOBYPART - LIST PARTS ON ORDER, OR EXPECTED, VIA THE PIPIN*~
            *            THUS IT CAN GRAB EXPECTED INVENTORY FROM PO'S, *~
            *            QC, JOBS OR PROJECTS.....BUT ONLY FOR NORMALLY *~
            *            STOCKED, PLANNED PARTS                         *~
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
            * 06/22/83 ! ORIGINAL                                 ! KEN *~
            * 01/17/84 ! Added On-Order Test & Msg for FIRSTPART$ ! ECR *~
            * 01/31/84 ! Added Last Part to range & printed rprt  ! BLT *~
            * 12/26/85 ! File format changes- VBKMASTR, VBKLINES, ! ERN *~
            *          !   VBKBUFFR, VBKBUF2; Clean up screens    !     *~
            *          !   Also caught V2PMASTR & V2PLINES.       !     *~
            * 10/26/88 ! Initilize UNITPRCE to prevent carry over ! KAB *~
            * 08/15/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            blankline$79,                /* BLANKLINE                  */~
            beforedate$10,               /* ON OR BEFORE DATE          */~
            bdate%(1),                   /* GOT TO BE ARRAY FOR SEARCH */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            duedate$(1000)8,             /* DUE DATES                  */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            firstpart$25,                /* FIRST PART NUMBER          */~
            header1$79,                  /* HEADER                     */~
            header2$79,                  /* HEADER                     */~
            hdrdate$45,                                                  ~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            jb$1,                        /* SHOW JOBS                  */~
            lastpart$25,                                                 ~
            line2$79,                                                    ~
            descrmsg$27,                                                 ~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            part$25,                     /* CURRENT PART               */~
            partdescr$34,                /* PART DESCRIPTION           */~
            pipintag$(1000)19,           /* SOURCE TAGS                */~
            plowkey$100,                                                 ~
            po$1,                        /* SHOW PURCHASE ORDERS       */~
            pr$1,                        /* SHOW PROJECTS              */~
            qc$1,                        /* SHOW QUALITY CONTROL       */~
            original$(1000)10,           /* ORIGINAL ORDER             */~
            onorder$(1000)10,            /* STILL ON ORDER             */~
            recvd$(1000)10,              /* RECV'D TO DATE             */~
            readkey$100,                 /* KEY FOR PLOWING            */~
            savefirst$25, savelast$25,   /* SAVE PART NUMBER RANGES    */~
            total$(1000)10,              /* LINE TOTAL                 */~
            totonorder$12,               /* Total on Order             */~
            totextension$12,             /* Total Extension            */~
            userid$3,                    /* USER IDENT                 */~
            unitprce$(1000)10,           /* UNITPRCE                   */~
            vencode$(1000)9,             /* VENDOR CODES               */~
            yymmdd$(490)6

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! USERINFO ! Users Default Information File           *~
            * #2  ! PIPIN    ! Planned inventory additions detail       *~
            * #3  ! VBKMASTR ! Backlog main header file                 *~
            * #4  ! VBKLINES ! Backlog line item file                   *~
            * #8  ! JBMASTR2 ! Production job master file               *~
            * #9  ! HNYMASTR ! Inventory Master File                    *~
            * #10 ! CALMASTR ! CAL. MASTER FILE                         *~
            * #12 ! JOBMASTR ! PROJECT MASTER                           *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =   3                      ~

            select #2,  "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #3,  "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1030,                                 ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   10, keylen =  16

            select #4,  "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =  28                      ~

            select #8,  "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~

            select #9,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

           select #10, "CALMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos = 1, keylen = 2

            select #12, "JOBMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =   8                      ~

            call "SHOSTAT"  ("LINKING TO THE DATA BASE TO ANALYZE THE ON-~
        ~ORDER POSITION")

            call "OPENFILE" (#1,  "SHARE", f2%(1 ), rslt$(1 ), axd$(1 ))
            call "OPENFILE" (#2,  "SHARE", f2%(2 ), rslt$(2 ), axd$(2 ))
            call "OPENFILE" (#3,  "SHARE", f2%(3 ), rslt$(3 ), axd$(3 ))
            call "OPENFILE" (#4,  "SHARE", f2%(4 ), rslt$(4 ), axd$(4 ))
            call "OPENFILE" (#8,  "SHARE", f2%(8 ), rslt$(8 ), axd$(8 ))
            call "OPENFILE" (#9,  "SHARE", f2%(9 ), rslt$(9 ), axd$(9 ))
            call "OPENFILE" (#10, "SHARE", f2%(10), rslt$(10), axd$(10))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            call "EXTRACT" addr("ID",userid$)
            call "READ100" (#1, userid$, f1%(1))
                if f1%(1)=0 then L65000

            header1$=                                                    ~
        "DUE DATE   SOURCE          LN# VENDOR       ORIGINAL   RECEIVED ~
        ~   ON ORDER"

            header2$=                                                    ~
        "DUE DATE   SOURCE          LN# VENDOR       ON ORDER    U/PRICE ~
        ~  EXTENSION"

            call "READ100" (#10, "10", f1%(10))
                if f1%(10) <> 1 then goto L10000
            get #10, using L09270 , str(yymmdd$(),1,1470)
L09270:         FMT XX(2), CH(1470)

            call "READ100" (#10, "11", f1%(10))
            get #10, using L09270 , str(yymmdd$(),1471,1470)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, blankline$,                ~
            beforedate$,                                                 ~
            duedate$(),                  /* DUE DATES                  */~
            firstpart$,                  /* FIRST PART NUMBER          */~
            lastpart$,                                                   ~
            jb$,                         /* SHOW JOBS                  */~
            part$,                       /* CURRENT PART               */~
            partdescr$,                  /* PART DESCRIPTION           */~
            pipintag$(),                 /* SOURCE TAGS                */~
            po$,                         /* SHOW PURCHASE ORDERS       */~
            pr$,                         /* SHOW PROJECTS              */~
            qc$,                         /* SHOW QUALITY CONTROL       */~
            original$(),                 /* ORIGINAL ORDER             */~
            onorder$(),                  /* STILL ON ORDER             */~
            recvd$(),                    /* RECV'D TO DATE             */~
            total$(),                                                    ~
            unitprce$(),                                                 ~
            vencode$()                   /* VENDOR CODES               */

            pagenumber% = 0%
            screen%=1

            beforedate$, firstpart$="ALL"
            po$,qc$,jb$,pr$="Y"
            bdate%(1)=999

            for fieldnr% = 1 to  7
L10230:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then       L10230
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10230
                next fieldnr%

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then L11900
                  if keyhit%  =  9 then L14000
                  if keyhit%  = 16 then L65000
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  7 then L11060

L11130:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11130
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11130
            goto L11060


L11900:     init (hex(00)) readkey$
            savefirst$ = firstpart$ : savelast$ = lastpart$
            if firstpart$="ALL" then init (hex(20)) firstpart$
            if lastpart$<firstpart$ then lastpart$=firstpart$
            if firstpart$=" " then init (hex(ff)) lastpart$
            if lastpart$=" " then lastpart$=firstpart$
            str(readkey$,1,25)=str(firstpart$,1,25) addc all(hex(ff))

L12000: REM *************************************************************~
            *             S H 0 W   D A T A   O N   F I L E             *~
            *                                                           *~
            * CONTROLS DISPLAY OF RECORDS FROM DATA FILES.              *~
            *************************************************************

            totonorder, tottotal=0

            init (" ") duedate$(), pipintag$(), vencode$(), original$(), ~
                       recvd$(), onorder$(), unitprce$(), total$(),part$,~
                       partdescr$
            max%=0
            call "PLOWALTS" (#2, readkey$, 1%, 0%, f1%(2))
            if f1%(2)=0 then L12800
            get #2, using L35030, part$, datein%, pipintag$(max%+1),onorder
            call "DESCRIBE" (#9, part$, partdescr$, 1%, f1%(9))
            goto L12195
L12170:     call "PLOWALTS" (#2, readkey$, 1%, 25%, f1%(2))
            if f1%(2)=0 then L12900
            get #2, using L35030, part$, datein%, pipintag$(max%+1),onorder
L12195:     if datein%>bdate%(1) then L12170
            if part$>lastpart$ then L11000
            gosub L12300
            if ok%=0 then L12170

            duedate$(max%)=yymmdd$(datein%)
            call "DATEFMT" (duedate$(max%))
            onorder=round(onorder,2%)
            totonorder=totonorder+onorder
            call "CONVERT" (onorder, 2.2, onorder$(max%))
            goto L12170

L12300:     ok%=0
            if str(pipintag$(max%+1),1,2)<>"PO" or po$<>"Y" then L12400
L12306:     ok%,max%=max%+1
            call "REDALT0" (#3, str(pipintag$(max%),3,14)&"  ",1%,f1%(3))
               if f1%(3)=0 then return
            get #3, using L12330, vencode$(max%)
L12330:         FMT CH(9)
            call"READ100" (#4,                                           ~
                     str(vencode$(max%),1,9)&str(pipintag$(max%),3,14)&  ~
                     "  "&str(pipintag$(max%),17,3), f1%(4))
            if str(pipintag$(max%),,2) = "PO" then                       ~
                             descrmsg$ = "ON ORDER FROM "&vencode$(max%)
            if str(pipintag$(max%),,2) = "QC" then                       ~
                      descrmsg$ = "IN QC - PO"&str(pipintag$(max%),3,17)
            if f1%(4)=0 then return
            get #4, using L12348, original,unitprce
L12348:         FMT XX(92), PD(14,4), XX(16), PD(14,7)
            total=round(unitprce*onorder,2%)
            tottotal=tottotal+total
            call "CONVERT"(unitprce,2.2,unitprce$(max%))
            call "CONVERT"(total,2.2,total$(max%))
            if str(pipintag$(max%),,2) = "QC" then return
            original=round(original,2%)
            recvd=round(original-onorder,2%)
            call "CONVERT"(original,2.2,original$(max%))
            call "CONVERT"(recvd,2.2,recvd$(max%))
            return

L12400:     if str(pipintag$(max%+1),1,2)<>"QC" or qc$<>"Y" then L12500
               goto L12306

L12500:  if str(pipintag$(max%+1),1,9)<>"JOB ORDER" or jb$<>"Y" then L12600
            ok%,max%=max%+1
            call "READ100" (#8, str(pipintag$(max%),12,8), f1%(8))
            if f1%(8)=0 then return
            get #8, using L12550, descrmsg$, original
L12550:         FMT XX(8),CH(27),XX(47),PD(14,4)
            recvd=round(original-onorder,2%)
            original=round(original,2%)
            call "CONVERT" (recvd,2.2,recvd$(max%))
            call "CONVERT" (original,2.2,original$(max%))
            return

L12600:     if str(pipintag$(max%+1),1,8)<>"JOB(PR):" or pr$<>"Y"        ~
                                                              then return
            ok%,max%=max%+1
            call "READ100" (#12, str(pipintag$(max%),12,8), f1%(12))
               if f1%(12) = 0% then return
                   get #12, using L12614, descrmsg$
L12614:                FMT XX(8),CH(27)
            return

L12800:     max%=2
            pipintag$(max%)="END OF FILE"
            goto L13000

L12900:     if max%=0 then L12000
            pipintag$(max%+1)=" "
            max%=max%+2
            pipintag$(max%)="TOTAL"
            call "CONVERT" (totonorder,2.2,onorder$(max%))
            call "CONVERT" (tottotal,2.2,total$(max%))

L13000:     line%=0
L13010:     if screen%=2 then gosub L43000 else gosub L42000
                if keyhit%  =  0  then       L12000
                if keyhit%  =  1  then gosub startover
                if keyhit%  =  2  then line%=0
                if keyhit%  =  3  then line%=max(0,max%-5)
                if keyhit%  =  4  then line%=max(0,line%-15)
                if keyhit%  =  5  then line%=min(line%+15,max(0,max%-5))
                if keyhit%  =  6  then line%=max(0,line%-1)
                if keyhit%  =  7  then line%=min(line%+1,max(0,max%-5))
                if keyhit%  =  8  and screen%=1 then L13150
                if keyhit%  =  8  and screen%=2 then L13170
                if keyhit% <>  9  then L13120
                                         firstpart$ = savefirst$
                                         lastpart$  = savelast$
                                         goto L11000
L13120:         if keyhit%  = 16  then L65000
            goto L13010

L13150:         screen%=2
                goto L13010
L13170:         screen%=1
                goto L13010

L14000: REM *************************************************************~
            *          P R I N T  O U T  P I P I N  I T E M S           *~
            *************************************************************
            call "SETPRNT" ("HNY002", " ", 0%, 0%)
            call "SHOSTAT" ("Now Printing, One Moment Please")

            init (hex(00)) plowkey$
            if firstpart$="ALL" then init (hex(20)) firstpart$
            if lastpart$<firstpart$ then lastpart$=firstpart$
            if firstpart$=" " then init (hex(ff)) lastpart$
            if lastpart$=" " then lastpart$=firstpart$
            str(plowkey$,1,25)=str(firstpart$,1,25) addc all(hex(ff))

            ky% = 1%
            break% = 25%
L15100:     line%=1000%
            call "PLOWALTS" (#2,plowkey$,ky%,0%,f1%(2))
               if f1%(2) = 0% then exit_print
                 go to L15700
L15500:     call "PLOWALTS" (#2,plowkey$,ky%,break%,f1%(2))
               if f1%(2)<>1% then break_print_routine
L15700:     get #2, using L35030,part$,datein%, pipintag$(1), onorder
            if lastpart$="ALL" then L16000
            if part$>lastpart$ then exit_print
L16000:     if datein%>bdate%(1) then L15500
            ok%,max%=0%
            gosub L12300
            if ok%=0% then L15500
               if line%> 55% then gosub new_page
            duedate$(1)=yymmdd$(datein%)
            call "DATEFMT" (duedate$(1))
            onorder = round(onorder,2%)
            extension = round(onorder*unitprce,2%)
            call "CONVERT" (onorder,2.2,onorder$(1))
            call "CONVERT" (unitprce,2.2,unitprce$(1))
            call "CONVERT" (extension,2.2,total$(1))
            totonorder = totonorder+onorder
            totextension = totextension + extension
            print using L61000, duedate$(1),pipintag$(1),vencode$(1),     ~
                        descrmsg$, original$(1), recvd$(1), onorder$(1), ~
                        unitprce$(1), total$(1)
            init (" ") duedate$(1),pipintag$(1),vencode$(1),descrmsg$,   ~
                       original$(1), recvd$(1), onorder$(1),             ~
                       unitprce$(1), total$(1)
            unitprce = 0
            line%=line%+1%
            descrmsg$ = " "
            goto L15500

        break_print_routine
            if line% = 1000% then L15100
            print
            print
            call "CONVERT" (totonorder, 2.2, totonorder$)
            call "CONVERT" (totextension, 2.2, totextension$)
            print using L62300, totonorder$,totextension$
            totonorder,totextension=0
            go to L15100
        exit_print
            close printer
               go to inputmode
        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%  /* PUT MSG AREA AT BOTTOM OF SCREEN  */
            call "STARTOVR" (keyhit1%)
                if keyhit1% = 1% then return

                return clear all
                goto inputmode

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR FILES.                              *~
            *************************************************************

L35030: FMT                      /* FILE: PIPIN                        */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date in subscript for PIP          */~
            CH(19),              /* Tag number in level 2 planning     */~
            PD(14,4),            /* Quantity of something in packed de */~
            XX(4)                /* Date to start as a date subscript  */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  if fieldnr% <1 or fieldnr%>7 then L40240
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
            line2$ = "Date: " & date$
            str(line2$,74) = "Page 1"

L40240:     accept                                                       ~
               at (01,02),                                               ~
                  "ON ORDER LIST BY PART",                               ~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "First Part Number",                                   ~
               at (06,30), fac(lfac$( 1)), firstpart$           , ch(25),~
               at (07,02),                                               ~
                  "Ending Part Number",                                  ~
               at (07,30), fac(lfac$( 2)), lastpart$            , ch(25),~
               at (08,02),                                               ~
                  "Show Purchase Orders",                                ~
               at (08,30), fac(lfac$( 3)), po$                  , ch(01),~
               at (09,02),                                               ~
                  "Show Quality Control",                                ~
               at (09,30), fac(lfac$( 4)), qc$                  , ch(01),~
               at (10,02),                                               ~
                  "Show Jobs",                                           ~
               at (10,30), fac(lfac$( 5)), jb$                  , ch(01),~
               at (11,02),                                               ~
                  "Show Projects",                                       ~
               at (11,30), fac(lfac$( 6)), pr$                  , ch(01),~
               at (12,02),                                               ~
                  "On or Before Date",                                   ~
               at (12,30), fac(lfac$( 7)), beforedate$          , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40650
                  call "MANUAL" ("OOBYPART")
                  goto L40240

L40650:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40240

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  if fieldnr%<1 or fieldnr%>7 then L41240
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
            line2$ = "Date: " & date$ : str(line2$,74) = "Page 1"

L41240:     accept                                                       ~
               at (01,02),                                               ~
                  "ON ORDER LIST BY PART",                               ~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "First Part Number",                                   ~
               at (06,30), fac(lfac$( 1)), firstpart$           , ch(25),~
               at (07,02),                                               ~
                  "Ending Part Number",                                  ~
               at (07,30), fac(lfac$( 2)), lastpart$            , ch(25),~
               at (08,02),                                               ~
                  "Show Purchase Orders",                                ~
               at (08,30), fac(lfac$( 3)), po$                  , ch(01),~
               at (09,02),                                               ~
                  "Show Quality Control",                                ~
               at (09,30), fac(lfac$( 4)), qc$                  , ch(01),~
               at (10,02),                                               ~
                  "Show Jobs",                                           ~
               at (10,30), fac(lfac$( 5)), jb$                  , ch(01),~
               at (11,02),                                               ~
                  "Show Projects",                                       ~
               at (11,30), fac(lfac$( 6)), pr$                  , ch(01),~
               at (12,02),                                               ~
                  "On or Before Date",                                   ~
               at (12,30), fac(lfac$( 7)), beforedate$          , ch(10),~
                                                                         ~
               at (20,02), fac(hex(84)),   inpmessage$          , ch(79),~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (24,35),                                               ~
                  "(9)PRINT REPORT BY PART",                             ~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,02),                                               ~
                  "(2)SHOW DETAIL FOR EACH PART",                        ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(000102090d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L41650
                  call "MANUAL" ("OOBYPART")
                  goto L41240

L41650:        if keyhit% <> 15 then L41690
                  call "PRNTSCRN"
                  goto L41240

L41690:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                          u3% = u3%
               return

L42000: REM *************************************************************~
            *                                                           *~
            *       DISPLAY   SCREEN FOR PARTS ON ORDER                 *~
            *                                                           *~
            *************************************************************

             accept                                                      ~
                at (01,02),                                              ~
             "ON ORDER BY PART LIST-ON OR BEFORE: ",                     ~
                at (01,38), fac(hex(84)), beforedate$           , ch(10),~
                at (02,02),                                              ~
             "DATE:",                                                    ~
                at (02,10), fac(hex(84)), date$                 , ch(08),~
                at (03,02),                                              ~
             "PART:",                                                    ~
                at (03,10), fac(hex(84)), part$                 , ch(25),~
                at (03,38), fac(hex(84)), partdescr$            , ch(34),~
                at (04,02), fac(hex(ac)), header1$              , ch(79),~
                                                                         ~
                at (05,02), fac(hex(84)), duedate$ (line%+ 1)   , ch(08),~
                at (06,02), fac(hex(84)), duedate$ (line%+ 2)   , ch(08),~
                at (07,02), fac(hex(84)), duedate$ (line%+ 3)   , ch(08),~
                at (08,02), fac(hex(84)), duedate$ (line%+ 4)   , ch(08),~
                at (09,02), fac(hex(84)), duedate$ (line%+ 5)   , ch(08),~
                at (10,02), fac(hex(84)), duedate$ (line%+ 6)   , ch(08),~
                at (11,02), fac(hex(84)), duedate$ (line%+ 7)   , ch(08),~
                at (12,02), fac(hex(84)), duedate$ (line%+ 8)   , ch(08),~
                at (13,02), fac(hex(84)), duedate$ (line%+ 9)   , ch(08),~
                at (14,02), fac(hex(84)), duedate$ (line%+10)   , ch(08),~
                at (15,02), fac(hex(84)), duedate$ (line%+11)   , ch(08),~
                at (16,02), fac(hex(84)), duedate$ (line%+12)   , ch(08),~
                at (17,02), fac(hex(84)), duedate$ (line%+13)   , ch(08),~
                at (18,02), fac(hex(84)), duedate$ (line%+14)   , ch(08),~
                at (19,02), fac(hex(84)), duedate$ (line%+15)   , ch(08),~
                                                                         ~
                at (05,13), fac(hex(84)), pipintag$(line%+ 1)   , ch(19),~
                at (06,13), fac(hex(84)), pipintag$(line%+ 2)   , ch(19),~
                at (07,13), fac(hex(84)), pipintag$(line%+ 3)   , ch(19),~
                at (08,13), fac(hex(84)), pipintag$(line%+ 4)   , ch(19),~
                at (09,13), fac(hex(84)), pipintag$(line%+ 5)   , ch(19),~
                at (10,13), fac(hex(84)), pipintag$(line%+ 6)   , ch(19),~
                at (11,13), fac(hex(84)), pipintag$(line%+ 7)   , ch(19),~
                at (12,13), fac(hex(84)), pipintag$(line%+ 8)   , ch(19),~
                at (13,13), fac(hex(84)), pipintag$(line%+ 9)   , ch(19),~
                at (14,13), fac(hex(84)), pipintag$(line%+10)   , ch(19),~
                at (15,13), fac(hex(84)), pipintag$(line%+11)   , ch(19),~
                at (16,13), fac(hex(84)), pipintag$(line%+12)   , ch(19),~
                at (17,13), fac(hex(84)), pipintag$(line%+13)   , ch(19),~
                at (18,13), fac(hex(84)), pipintag$(line%+14)   , ch(19),~
                at (19,13), fac(hex(84)), pipintag$(line%+15)   , ch(19),~
                                                                         ~
                at (05,33), fac(hex(84)), vencode$ (line%+ 1)   , ch(09),~
                at (05,33), fac(hex(84)), vencode$ (line%+ 1)   , ch(09),~
                at (06,33), fac(hex(84)), vencode$ (line%+ 2)   , ch(09),~
                at (07,33), fac(hex(84)), vencode$ (line%+ 3)   , ch(09),~
                at (08,33), fac(hex(84)), vencode$ (line%+ 4)   , ch(09),~
                at (09,33), fac(hex(84)), vencode$ (line%+ 5)   , ch(09),~
                at (10,33), fac(hex(84)), vencode$ (line%+ 6)   , ch(09),~
                at (11,33), fac(hex(84)), vencode$ (line%+ 7)   , ch(09),~
                at (12,33), fac(hex(84)), vencode$ (line%+ 8)   , ch(09),~
                at (13,33), fac(hex(84)), vencode$ (line%+ 9)   , ch(09),~
                at (14,33), fac(hex(84)), vencode$ (line%+10)   , ch(09),~
                at (15,33), fac(hex(84)), vencode$ (line%+11)   , ch(09),~
                at (16,33), fac(hex(84)), vencode$ (line%+12)   , ch(09),~
                at (17,33), fac(hex(84)), vencode$ (line%+13)   , ch(09),~
                at (18,33), fac(hex(84)), vencode$ (line%+14)   , ch(09),~
                at (19,33), fac(hex(84)), vencode$ (line%+15)   , ch(09),~
                                                                         ~
                at (05,44), fac(hex(84)), original$(line%+ 1)   , ch(10),~
                at (06,44), fac(hex(84)), original$(line%+ 2)   , ch(10),~
                at (07,44), fac(hex(84)), original$(line%+ 3)   , ch(10),~
                at (08,44), fac(hex(84)), original$(line%+ 4)   , ch(10),~
                at (09,44), fac(hex(84)), original$(line%+ 5)   , ch(10),~
                at (10,44), fac(hex(84)), original$(line%+ 6)   , ch(10),~
                at (11,44), fac(hex(84)), original$(line%+ 7)   , ch(10),~
                at (12,44), fac(hex(84)), original$(line%+ 8)   , ch(10),~
                at (13,44), fac(hex(84)), original$(line%+ 9)   , ch(10),~
                at (14,44), fac(hex(84)), original$(line%+10)   , ch(10),~
                at (15,44), fac(hex(84)), original$(line%+11)   , ch(10),~
                at (16,44), fac(hex(84)), original$(line%+12)   , ch(10),~
                at (17,44), fac(hex(84)), original$(line%+13)   , ch(10),~
                at (18,44), fac(hex(84)), original$(line%+14)   , ch(10),~
                at (19,44), fac(hex(84)), original$(line%+15)   , ch(10),~
                                                                         ~
                at (05,55), fac(hex(84)), recvd$   (line%+ 1)   , ch(10),~
                at (06,55), fac(hex(84)), recvd$   (line%+ 2)   , ch(10),~
                at (07,55), fac(hex(84)), recvd$   (line%+ 3)   , ch(10),~
                at (08,55), fac(hex(84)), recvd$   (line%+ 4)   , ch(10),~
                at (09,55), fac(hex(84)), recvd$   (line%+ 5)   , ch(10),~
                at (10,55), fac(hex(84)), recvd$   (line%+ 6)   , ch(10),~
                at (11,55), fac(hex(84)), recvd$   (line%+ 7)   , ch(10),~
                at (12,55), fac(hex(84)), recvd$   (line%+ 8)   , ch(10),~
                at (13,55), fac(hex(84)), recvd$   (line%+ 9)   , ch(10),~
                at (14,55), fac(hex(84)), recvd$   (line%+10)   , ch(10),~
                at (15,55), fac(hex(84)), recvd$   (line%+11)   , ch(10),~
                at (16,55), fac(hex(84)), recvd$   (line%+12)   , ch(10),~
                at (17,55), fac(hex(84)), recvd$   (line%+13)   , ch(10),~
                at (18,55), fac(hex(84)), recvd$   (line%+14)   , ch(10),~
                at (19,55), fac(hex(84)), recvd$   (line%+15)   , ch(10),~
                                                                         ~
                at (05,67), fac(hex(84)), onorder$ (line% +1)   , ch(10),~
                at (06,67), fac(hex(84)), onorder$ (line%+ 2)   , ch(10),~
                at (07,67), fac(hex(84)), onorder$ (line%+ 3)   , ch(10),~
                at (08,67), fac(hex(84)), onorder$ (line%+ 4)   , ch(10),~
                at (09,67), fac(hex(84)), onorder$ (line%+ 5)   , ch(10),~
                at (10,67), fac(hex(84)), onorder$ (line%+ 6)   , ch(10),~
                at (11,67), fac(hex(84)), onorder$ (line%+ 7)   , ch(10),~
                at (12,67), fac(hex(84)), onorder$ (line%+ 8)   , ch(10),~
                at (13,67), fac(hex(84)), onorder$ (line%+ 9)   , ch(10),~
                at (14,67), fac(hex(84)), onorder$ (line%+10)   , ch(10),~
                at (15,67), fac(hex(84)), onorder$ (line%+11)   , ch(10),~
                at (16,67), fac(hex(84)), onorder$ (line%+12)   , ch(10),~
                at (17,67), fac(hex(84)), onorder$ (line%+13)   , ch(10),~
                at (18,67), fac(hex(84)), onorder$ (line%+14)   , ch(10),~
                at (19,67), fac(hex(84)), onorder$ (line%+15)   , ch(10),~
                                                                         ~
                                                                         ~
                at (21,02), fac(hex(ac)), blankline$            , ch(79),~
                at (22,02),                                              ~
        "(RETURN)Next Part  (1)Start Over                             (13~
        ~)Instructions",                                                  ~
                at (23,02),                                              ~
        "(2)First  (4)Prev  (6)Down          (8)Extensions            (15~
        ~)Print Screen",                                                  ~
                at (24,02),                                              ~
        "(3)Last   (5)Next  (7)Up            (9)Selections            (16~
        ~)Exit Program",                                                  ~
                                                                         ~
            keys(hex(000102030405060708090d0f10)),                       ~
            key(keyhit%)

            if keyhit% <> 13 then L42965
                call "MANUAL" ("OOBYPART")
                goto L42000

L42965:     if keyhit% <> 15 then return
                call "PRNTSCRN"
                goto L42000


L43000: REM *************************************************************~
            *                                                           *~
            *       DISPLAY SCREEN FOR PARTS ON ORDER-EXTENSIONS        *~
            *                                                           *~
            *************************************************************

             accept                                                      ~
                at (01,02),                                              ~
             "ON ORDER BY PART LIST-ON OR BEFORE: ",                     ~
                at (01,38), fac(hex(84)), beforedate$           , ch(10),~
                at (02,02),                                              ~
             "DATE:",                                                    ~
                at (02,10), fac(hex(84)), date$                 , ch(08),~
                at (03,02),                                              ~
             "PART:",                                                    ~
                at (03,10), fac(hex(84)), part$                 , ch(25),~
                at (03,38), fac(hex(84)), partdescr$            , ch(34),~
                at (04,02), fac(hex(ac)), header2$              , ch(79),~
                                                                         ~
                at (05,02), fac(hex(84)), duedate$ (line%+ 1)   , ch(08),~
                at (06,02), fac(hex(84)), duedate$ (line%+ 2)   , ch(08),~
                at (07,02), fac(hex(84)), duedate$ (line%+ 3)   , ch(08),~
                at (08,02), fac(hex(84)), duedate$ (line%+ 4)   , ch(08),~
                at (09,02), fac(hex(84)), duedate$ (line%+ 5)   , ch(08),~
                at (10,02), fac(hex(84)), duedate$ (line%+ 6)   , ch(08),~
                at (11,02), fac(hex(84)), duedate$ (line%+ 7)   , ch(08),~
                at (12,02), fac(hex(84)), duedate$ (line%+ 8)   , ch(08),~
                at (13,02), fac(hex(84)), duedate$ (line%+ 9)   , ch(08),~
                at (14,02), fac(hex(84)), duedate$ (line%+10)   , ch(08),~
                at (15,02), fac(hex(84)), duedate$ (line%+11)   , ch(08),~
                at (16,02), fac(hex(84)), duedate$ (line%+12)   , ch(08),~
                at (17,02), fac(hex(84)), duedate$ (line%+13)   , ch(08),~
                at (18,02), fac(hex(84)), duedate$ (line%+14)   , ch(08),~
                at (19,02), fac(hex(84)), duedate$ (line%+15)   , ch(08),~
                                                                         ~
                at (05,13), fac(hex(84)), pipintag$(line%+ 1)   , ch(19),~
                at (06,13), fac(hex(84)), pipintag$(line%+ 2)   , ch(19),~
                at (07,13), fac(hex(84)), pipintag$(line%+ 3)   , ch(19),~
                at (08,13), fac(hex(84)), pipintag$(line%+ 4)   , ch(19),~
                at (09,13), fac(hex(84)), pipintag$(line%+ 5)   , ch(19),~
                at (10,13), fac(hex(84)), pipintag$(line%+ 6)   , ch(19),~
                at (11,13), fac(hex(84)), pipintag$(line%+ 7)   , ch(19),~
                at (12,13), fac(hex(84)), pipintag$(line%+ 8)   , ch(19),~
                at (13,13), fac(hex(84)), pipintag$(line%+ 9)   , ch(19),~
                at (14,13), fac(hex(84)), pipintag$(line%+10)   , ch(19),~
                at (15,13), fac(hex(84)), pipintag$(line%+11)   , ch(19),~
                at (16,13), fac(hex(84)), pipintag$(line%+12)   , ch(19),~
                at (17,13), fac(hex(84)), pipintag$(line%+13)   , ch(19),~
                at (18,13), fac(hex(84)), pipintag$(line%+14)   , ch(19),~
                at (19,13), fac(hex(84)), pipintag$(line%+15)   , ch(19),~
                                                                         ~
                at (05,33), fac(hex(84)), vencode$ (line%+ 1)   , ch(09),~
                at (05,33), fac(hex(84)), vencode$ (line%+ 1)   , ch(09),~
                at (06,33), fac(hex(84)), vencode$ (line%+ 2)   , ch(09),~
                at (07,33), fac(hex(84)), vencode$ (line%+ 3)   , ch(09),~
                at (08,33), fac(hex(84)), vencode$ (line%+ 4)   , ch(09),~
                at (09,33), fac(hex(84)), vencode$ (line%+ 5)   , ch(09),~
                at (10,33), fac(hex(84)), vencode$ (line%+ 6)   , ch(09),~
                at (11,33), fac(hex(84)), vencode$ (line%+ 7)   , ch(09),~
                at (12,33), fac(hex(84)), vencode$ (line%+ 8)   , ch(09),~
                at (13,33), fac(hex(84)), vencode$ (line%+ 9)   , ch(09),~
                at (14,33), fac(hex(84)), vencode$ (line%+10)   , ch(09),~
                at (15,33), fac(hex(84)), vencode$ (line%+11)   , ch(09),~
                at (16,33), fac(hex(84)), vencode$ (line%+12)   , ch(09),~
                at (17,33), fac(hex(84)), vencode$ (line%+13)   , ch(09),~
                at (18,33), fac(hex(84)), vencode$ (line%+14)   , ch(09),~
                at (19,33), fac(hex(84)), vencode$ (line%+15)   , ch(09),~
                                                                         ~
                at (05,44), fac(hex(84)), onorder$ (line%+ 1)   , ch(10),~
                at (06,44), fac(hex(84)), onorder$ (line%+ 2)   , ch(10),~
                at (07,44), fac(hex(84)), onorder$ (line%+ 3)   , ch(10),~
                at (08,44), fac(hex(84)), onorder$ (line%+ 4)   , ch(10),~
                at (09,44), fac(hex(84)), onorder$ (line%+ 5)   , ch(10),~
                at (10,44), fac(hex(84)), onorder$ (line%+ 6)   , ch(10),~
                at (11,44), fac(hex(84)), onorder$ (line%+ 7)   , ch(10),~
                at (12,44), fac(hex(84)), onorder$ (line%+ 8)   , ch(10),~
                at (13,44), fac(hex(84)), onorder$ (line%+ 9)   , ch(10),~
                at (14,44), fac(hex(84)), onorder$ (line%+10)   , ch(10),~
                at (15,44), fac(hex(84)), onorder$ (line%+11)   , ch(10),~
                at (16,44), fac(hex(84)), onorder$ (line%+12)   , ch(10),~
                at (17,44), fac(hex(84)), onorder$ (line%+13)   , ch(10),~
                at (18,44), fac(hex(84)), onorder$ (line%+14)   , ch(10),~
                at (19,44), fac(hex(84)), onorder$ (line%+15)   , ch(10),~
                                                                         ~
                at (05,55), fac(hex(84)), unitprce$(line%+ 1)   , ch(10),~
                at (06,55), fac(hex(84)), unitprce$(line%+ 2)   , ch(10),~
                at (07,55), fac(hex(84)), unitprce$(line%+ 3)   , ch(10),~
                at (08,55), fac(hex(84)), unitprce$(line%+ 4)   , ch(10),~
                at (09,55), fac(hex(84)), unitprce$(line%+ 5)   , ch(10),~
                at (10,55), fac(hex(84)), unitprce$(line%+ 6)   , ch(10),~
                at (11,55), fac(hex(84)), unitprce$(line%+ 7)   , ch(10),~
                at (12,55), fac(hex(84)), unitprce$(line%+ 8)   , ch(10),~
                at (13,55), fac(hex(84)), unitprce$(line%+ 9)   , ch(10),~
                at (14,55), fac(hex(84)), unitprce$(line%+10)   , ch(10),~
                at (15,55), fac(hex(84)), unitprce$(line%+11)   , ch(10),~
                at (16,55), fac(hex(84)), unitprce$(line%+12)   , ch(10),~
                at (17,55), fac(hex(84)), unitprce$(line%+13)   , ch(10),~
                at (18,55), fac(hex(84)), unitprce$(line%+14)   , ch(10),~
                at (19,55), fac(hex(84)), unitprce$(line%+15)   , ch(10),~
                                                                         ~
                at (05,67), fac(hex(84)), total$   (line% +1)   , ch(10),~
                at (06,67), fac(hex(84)), total$   (line%+ 2)   , ch(10),~
                at (07,67), fac(hex(84)), total$   (line%+ 3)   , ch(10),~
                at (08,67), fac(hex(84)), total$   (line%+ 4)   , ch(10),~
                at (09,67), fac(hex(84)), total$   (line%+ 5)   , ch(10),~
                at (10,67), fac(hex(84)), total$   (line%+ 6)   , ch(10),~
                at (11,67), fac(hex(84)), total$   (line%+ 7)   , ch(10),~
                at (12,67), fac(hex(84)), total$   (line%+ 8)   , ch(10),~
                at (13,67), fac(hex(84)), total$   (line%+ 9)   , ch(10),~
                at (14,67), fac(hex(84)), total$   (line%+10)   , ch(10),~
                at (15,67), fac(hex(84)), total$   (line%+11)   , ch(10),~
                at (16,67), fac(hex(84)), total$   (line%+12)   , ch(10),~
                at (17,67), fac(hex(84)), total$   (line%+13)   , ch(10),~
                at (18,67), fac(hex(84)), total$   (line%+14)   , ch(10),~
                at (19,67), fac(hex(84)), total$   (line%+15)   , ch(10),~
                                                                         ~
                                                                         ~
                at (21,02), fac(hex(ac)), blankline$            , ch(79),~
                at (22,02),                                              ~
        "(RETURN)Next Part  (1)Start Over                             (13~
        ~)Instructions",                                                  ~
                at (23,02),                                              ~
        "(2)First  (4)Prev  (6)Down          (8)Show Units            (15~
        ~)Print Screen",                                                  ~
                at (24,02),                                              ~
        "(3)Last   (5)Next  (7)Up            (9)Selections            (16~
        ~)Exit Program",                                                  ~
                                                                         ~
            keys(hex(000102030405060708090d0f10)),                       ~
            key(keyhit%)

            if keyhit% <> 13 then L44290
                call "MANUAL" ("OOBYPART")
                goto L43000

L44290:     if keyhit% <> 15 then return
                call "PRNTSCRN"
                goto L43000


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* FIRST PART NUMBER*/~
                                    L50140,         /* LAST PART NUMBER */~
                                    L50200,         /* PO'S             */~
                                    L50300,         /* QC?              */~
                                    L50400,         /* JOBS?            */~
                                    L50500,         /* PROJECTS?        */~
                                    L50600          /* ON OR BEFORE     */
                     return
L50100:     REM TEST DATA FOR FIRST PART NUMBER
                inpmessage$ = " "
                max%=0%
                if firstpart$="ALL" or firstpart$=" " then return
                call "READ100" (#9, firstpart$, f1%(9))
                if f1%(9)<>0 then L50116
                     inpmessage$="First Part not on File."
                     return
L50116:            init(hex(00)) readkey$
                   str(readkey$,1,25)=str(firstpart$,1,25)
L50120:            call "PLOWALTS" (#2, readkey$, 1%, 25%, f1%(2))
                   if f1%(2) <> 0 then L50128
                     inpmessage$="First Part Not On Order: " & firstpart$
                     return
L50128:            get #2, using L50130, pipintag$(1)
L50130:                FMT XX(29), CH(19)
                   gosub L12300
                   if ok%<>0 then return
                   goto L50120

L50140:     REM TEST DATA FOR LAST PART NUMBER
                if lastpart$ = " " then lastpart$ = firstpart$
                if lastpart$ < firstpart$ then L50148
                return
L50148:         errormsg$="Ending Part Number Cannot Come Before First Pa~
        ~rt Number"
                return
L50200:     REM TEST DATA FOR SHOW PURCHASE ORDERS
            if po$="Y" or po$="N" then return
                errormsg$="Must Answer 'Y' Or 'N'"
                return
L50300:     REM TEST DATA FOR SHOW QUALITY CONTROL
            if qc$="Y" or qc$="N" then return
                errormsg$="Must Answer 'Y' Or 'N'"
                return
L50400:     REM TEST DATA FOR SHOW JOBS
            if jb$="Y" or jb$="N" then return
                errormsg$="Must Answer 'Y' Or 'N'"
                return
L50500:     REM TEST DATA FOR SHOW PROJECTS
            if pr$="Y" or pr$="N" then return
                errormsg$="Must Answer 'Y' Or 'N'"
                return
L50600:     REM TEST DATA FOR ON OR BEFORE
            bdate%(1)=0
            if beforedate$ = " " or beforedate$ = blankdate$ ~
                                 or beforedate$ = "ALL" then L50680
            call "DATEOKC" (beforedate$, bdate%(1), errormsg$)
                if errormsg$<>" " then return
            bdate%(1)=0
            temp$=beforedate$
            call "DATUFMTC" (temp$)
            search str(yymmdd$(),1)=str(temp$,1,6) to bdate%() step 6
            if bdate%(1)=0 then L50680
            bdate%(1)=1+int(bdate%(1)/6)
            return
L50680:     bdate%(1)=999
            return

        new_page
                   select printer (134)
                   print page
                   line% = 14
                   pagenumber% = pagenumber% + 1
              call "DESCRIBE" (#9,str(plowkey$,1,25),partdescr$,0%,f1%(9))
              call "DATE" addr ("HD", hdrdate$)
                   print using L60000, pagenumber%, hdrdate$
                   print using L60300
              if beforedate$ <> "ALL" and beforedate$ <> " " ~
                                      and beforedate$ <> blankdate$ then L54600
                   print using L60500
                   go to L54900
L54600:            print using L60400, beforedate$
L54900:            print
                   print using L60800, str(plowkey$,1,25),partdescr$
                   print
                   print using L61300
                   print using L61600
                   print using L61900
                   print
                   return
L60000: %PAGE #####              ANALYSIS OF THE ON-ORDER POSITION OF INV~
        ~ENTORY                 ##########################################~
        ~###
L60300: %                                 BY INVENTORY PART NUMBER
L60400: %     HNY002              FOR ALL DATES TO ########## INCLUSIVE
L60500: %                                 WITH ALL DATES INCLUDED
        %                        PART USEAGE ITEMS ARE GROUPED BY #######~
        ~#####
L60800: %PART NUMBER:  #########################     ####################~
        ~############
L61000: % ########   ###################   #########  ###################~
        ~########  ##########  ##########  ##########  ##########  #######~
        ~###
L61300: %================================================================~
        ~=================================================================~
        ~===
L61600: %!  DATE  !  TO BE RECEIVED FOR   ! VENDOR   ! DESCRIPTION       ~
        ~         !   ORIGINAL!   RECEIVED!   ON ORDER! UNIT PRICE! EXTENS~
        ~ION
L61900: %!--------+-----------------------+----------+-------------------~
        ~---------+-----------+-----------+-----------+-----------!-------~
        ~---

L62300: %                                      TOTAL                     ~
        ~                                ############            #########~
        ~###
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


            call "SHOSTAT" ("Closing files, one moment please")
            call "SETPRNT" ("HNY002", " ", 0%, 1%)

            end
