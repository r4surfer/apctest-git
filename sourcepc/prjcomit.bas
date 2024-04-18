        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  PPPP   RRRR   JJJJJ   CCC    OOO   M   M  IIIII  TTTTT   *~
            *  P   P  R   R    J    C   C  O   O  MM MM    I      T     *~
            *  PPPP   RRRR     J    C      O   O  M M M    I      T     *~
            *  P      R   R  J J    C   C  O   O  M   M    I      T     *~
            *  P      R   R   J      CCC    OOO   M   M  IIIII    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRJCOMIT - Manually produce or update pipout records for  *~
            *            a Project.        Thus enabling the pick slip  *~
            *            and parts issue functions.  Also for a project,*~
            *            (via neg quantity) produce a pipin for returned*~
            *            or produced materials.                         *~
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
            * 06/28/83 ! ORIGINAL                                 ! KEN *~
            * 04/03/87 ! Extended arrays to 1600, fixed dsave bug ! HES *~
            * 06/15/87 ! JBMASTR2, HNYMASTR record length changes.! JIM *~
            *          !   Modernized STARTOVER.                  !     *~
            * 03/13/92 ! PRR 12305.  Renamed JOBCOMIT to PRJCOMIT ! SID *~
            *          !   Standardized the screen, fixed the     !     *~
            *          !   negative qty crashed.  Added 'ALLFREE' !     *~
            *          !   Blank out PF Keys When in edit mode    !     *~
            *          !   per standard.                          !     *~
            *************************************************************~

        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr$30,                    /* Plowcode Description       */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            header$79,                   /* HEADER                     */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            intagnr$48,                                                  ~
            prjqty$10,                   /* QUANTITY                   */~
            prjdstr$10,                  /* DESCRIPTOR                 */~
            prjnr$8,                     /* Project Number             */~
            prjnrdescr$32,               /* Project Description        */~
            jp$1,                        /* Project Flag               */~
            lfac$(20,4)1,                /* FIELD ATTRIBUTE CHARACTERS */~
            pfkeys$(2)18,                /* PF KEYS ACTIVE             */~
            plowkey$99,                  /*                            */~
            scrn_temp1$79, scrn_temp2$79,/* SCREEN$() Dummy Variables  */~
            screen$(2,2)79,              /* SCREEN MESSAGES            */~
            parta$25,                    /* PART TO BUILD              */~
            partadescr$34,               /* PART TO BUILD              */~
            part$(1610)25,                                               ~
            partdescr$(1610)32,                                          ~
            outdate%(1610),                                              ~
            outdate$(1610)8,                                             ~
            newdate%(1610),                                              ~
            newquantity(1610),                                           ~
            quantity(1610),                                              ~
            quantity$(1610)10,                                           ~
            outtagnr$56,                                                 ~
            ser%(1),                                                     ~
            type$3,                                                      ~
            pipin$60,                                                    ~
            systime$8,                                                   ~
            yymmdd$(490)6

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! USERINFO ! Users Default Information File           *~
            * #02 ! JBMASTR2 ! Production job master file               *~
            * #03 ! JOBMASTR ! WIP/JC Job Master File                   *~
            * #04 ! PIPOUT   ! Planned inventory use detail rec.        *~
            * #05 ! PIPIN    ! Planned inventory additions detail       *~
            * #06 ! HNYMASTR ! Inventory Master File                    *~
            * #07 ! CALMASTR ! CALENDAR  MASTER FILE                    *~
            * #08 ! PIPMASTR ! PLANNED INVENTORY MASTER FILE            *~
            * #09 ! SFCUM2   ! NET CUMULATIVE SALES FORECASTS           *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  150,                                  ~
                        keypos =    1, keylen =   3                      ~

            select #02, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~

            select #03, "JOBMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =   8                      ~

            select #04, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #05, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #06, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #07, "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos =    1, keylen =   2                      ~

            select #08, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =   25,                    ~
                        alternate key 1, keypos=1, keylen=26

            select #09, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =   25

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1,  0%, 0%,   0%, " ")
            call "OPENCHCK" (#2,  0%, 0%,   0%, " ")
            call "OPENCHCK" (#3,  0%, 0%,   0%, " ")
            call "OPENCHCK" (#4,  0%, 0%, 200%, " ")
            call "OPENCHCK" (#5,  0%, 0%, 200%, " ")
            call "OPENCHCK" (#6,  0%, 0%,   0%, " ")
            call "OPENCHCK" (#7,  0%, 0%,   0%, " ")
            call "OPENCHCK" (#8,  0%, 0%, 100%, " ")
            call "OPENCHCK" (#9,  0%, 0%,   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Delete An Item, Set Quantity To Zero"

            header$="Part Number               Description               ~
        ~         Quantity   Date"

            init (" ") yymmdd$ ()

            maxitems% = dim(part$(),1) - 10%

            call "READ100" (#7,"10",f1%(7))
              if f1%(7)=0 then L09190
                 get #7, using L09220, str(yymmdd$ (),1)
L09190:     call "READ100" (#7, "11", f1%(7))
              if f1%(7)=0 then L09240
                 get #7, using L09220, str(yymmdd$ (),1471)
L09220:     FMT XX(2), CH(1470)

L09240:     pfkeys$(1)=hex(000102040d0f10ffffffffffffffffffffff)
            pfkeys$(2)=hex(00010203040506070b0c0d0f10ffffffffff)

            screen$(1,1)="(1)Start Over                                  ~
        ~                (15)Print Screen"
            screen$(2,1)="(2)First Column     (4)Line Above              ~
        ~(13)Instructions   (16)Edit Mode"
            screen$(1,2)="(1)Start Over   (3)Last   (5)Next   (7)Up     (~
        ~12)Delete       (15)Print Screen"
            screen$(2,2)="  (2)First        (4)Prev   (6)Down   (11)Inser~
        ~t (13)Instructions (16)Save Data"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode

            call "ALLFREE"

            init(" ") errormsg$,inpmessage$,prjqty$,prjnr$,prjnrdescr$,  ~
                      jp$,parta$,partadescr$,part$(),partdescr$(),       ~
                      quantity$(),outdate$()
            mat quantity = zer
            mat outdate% = zer
            mat newquantity = zer
            mat newdate% = zer

                prjdstr$="Project#"
                inpmessage$ = "Enter '?' to See Project on File."
L10120:         gosub'101
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       L65000
                      if keyhit% <>  0 then       L10120
                gosub'151
                      if errormsg$ <> " " then L10120
                if max%>0 then editlines

            line%, screenline%, c%=0
        inputlines
            screenline%=screenline%+1
            if screenline%<13 then L10270
                line%=line%+12
                screenline%=1
L10270:     c%=line%+screenline%
            if max%>maxitems% then editlines
            gosub L10300
            if keyhit%=16 then editlines
            goto inputlines

L10300:     for fieldnr%=1 to 4

            if  fieldnr%=2 then L10410
L10330:     gosub'201(screenline%,fieldnr%)
                if keyhit% =  1 then gosub startover
                if keyhit% =  2 then       columnone
                if keyhit% =  4 then gosub lineabove
                if keyhit% = 16 and fieldnr% = 1 then return
                if keyhit%<> 0 and keyhit% <> 4 then L10330
            gosub'152(c%,fieldnr%)
                if errormsg$<>" " then L10330
L10410:     next fieldnr%
                max%=max%+1
                return

        columnone
            init (" ") part$(c%),partdescr$(c%),quantity$(c%),outdate$(c%)
            errormsg$=" "
            goto L10300

        lineabove
            if c%=1 then return
            on fieldnr% goto L10520,L10530,L10540,L10550
                return
L10520:     part$(c%)=part$(c%-1):return
L10530:     partdescr$(c%)=partdescr$(c%-1):return
L10540:     quantity$(c%)=quantity$(c%-1):return
L10550:     outdate$(c%)=outdate$(c%-1):return

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************
        editlines
            if max%=0 then inputmode

            errormsg$=" "
            line%,c%,screenline%=0
L11100:     gosub'202(0%,0%)
                  if keyhit% =  1 then gosub startover
                  if keyhit% =  2 then line%=0
                  if keyhit% =  3 then line%=max(0, max%-4)
                  if keyhit% =  4 then line%=max(line%-12,0)
                  if keyhit% =  5 then line%=min(line%+12,max(max%-4,0))
                  if keyhit% =  6 then line%=max(0,line%-1)
                  if keyhit% =  7 then line%=min(line%+1,max(0,max%-4))
                  if keyhit% = 11 then insertline
                  if keyhit% = 16 then datasave
                  if keyhit% <> 0 and keyhit% <> 12 then L11100

            screenline%=cursor%(1)-7
            if screenline%<1 or screenline%>12 then editlines
            c%=screenline%+line%
            if c%>max% then editlines
            if keyhit%=12 then deleteline
            fieldnr%=0
            if cursor%(2)>60 then fieldnr%=3
            if cursor%(2)>71 then fieldnr%=4
            if fieldnr%=0 then L11370
            scrn_temp1$ = screen$(1,2) : scrn_temp2$ = screen$(2,2)
            str(screen$(1,2),14), screen$(2,2) = " "
L11300:     gosub'202(screenline%,fieldnr%)
                if keyhit% =  1 then gosub startover
                if keyhit% <> 0 then L11300
            gosub'152(c%,fieldnr%)
            if errormsg$<>" " then L11300
            screen$(1,2) = scrn_temp1$ : screen$(2,2) = scrn_temp2$
            goto editlines

L11370:     scrn_temp1$ = screen$(1,2) : scrn_temp2$ = screen$(2,2)
            for fieldnr%=3 to 4
            str(screen$(1,2),14), screen$(2,2) = " "
L11380:     gosub'202(screenline%,fieldnr%)
                if keyhit% =  1 then gosub startover
                if keyhit% <> 0 then L11380
            gosub'152(c%,fieldnr%)
            if errormsg$<>" " then L11380
            next fieldnr%
            screen$(1,2) = scrn_temp1$ : screen$(2,2) = scrn_temp2$
            goto editlines

        insertline
            if max%>maxitems% then editlines
            screenline%=min(12, max(cursor%(1)-6,1))
            if screenline%<1 or screenline%>12  then editlines
L11500:     c%=min(screenline%+line%,max%+1)
            if c%<=max% then L11560
            screenline%=c%-line%
            if c%>maxitems% then editlines
            goto L11650

L11560:     for i%=max% to c% step -1
                part$     (i%+1)=part$     (i%)
                partdescr$(i%+1)=partdescr$(i%)
                quantity$ (i%+1)=quantity$ (i%)
                outdate$  (i%+1)=outdate$  (i%)
                quantity  (i%+1)=quantity  (i%)
                outdate%  (i%+1)=outdate%  (i%)
             newquantity  (i%+1)=newquantity  (i%)
                newdate%  (i%+1)=newdate%  (i%)
            next i%

L11650:         init (" ") part$(c%),partdescr$(c%),quantity$(c%),       ~
                           outdate$(c%)
                newdate%(c%),outdate%(c%)=0
                newquantity(c%),quantity(c%)=0

            gosub L10300
            if max%>maxitems% then editlines
            if keyhit%=16 then L11760
            screenline%=screenline%+1
            if screenline%<13 then L11500
                line%=line%+12
                screenline%=1
            goto L11500

L11760:     if c% > max% then editlines
            for i%=c% to max%
                part$     (i%)  =part$     (i%+1)
                partdescr$(i%)  =partdescr$(i%+1)
                quantity$ (i%)  =quantity$ (i%+1)
                outdate$  (i%)  =outdate$  (i%+1)
                quantity  (i%)  =quantity  (i%+1)
                outdate%  (i%)  =outdate%  (i%+1)
             newquantity  (i%)  =newquantity  (i%+1)
                newdate%  (i%)  =newdate%  (i%+1)
            next i%
                init (" ") part$(max%+1),partdescr$(max%+1),             ~
                           quantity$(max%+1),outdate$(max%+1)
                newdate%(max%+1), outdate%(max%+1)=0
                newquantity(max%+1), quantity(max%+1)=0
                goto editlines

        deleteline
            gosub'201(screenline%,5%)
                if keyhit%<>0 then editlines
            quantity$(c%)=" "
            gosub'152(c%,3%)
            goto editlines

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L31000
            goto inputmode

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover
            u3% = 2% /* OPEN WINDOW AT BOTTOM */
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *        READ DATA FROM PIPOUT  (PRESENT KIT LIST)          *~
            *                                                           *~
            * FORMAT DATA FOR DISPLAY AND EDIT.                         *~
            *************************************************************

            max%=0
            if jp$="J" then outtagnr$="JOB ORDER: " & str(prjnr$,1,8)
            if jp$="P" then outtagnr$="JOB(PROJ): " & str(prjnr$,1,8)
L30085:     if max%>maxitems% then return
            call "PLOWNEXT" (#4, outtagnr$, 19%, f1%(4))
            if f1%(4)=0 then L30190
            max%=max%+1
            get #4,using L30120,part$(max%),outdate%(max%),quantity(max%)
L30120:         FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
            outdate$(max%)=yymmdd$(outdate%(max%))
            call "DATEFMT" (outdate$(max%))
            call "GETCODE" (#6,part$(max%),partdescr$(max%),0%,99,f1%(6))
            quantity(max%)=round(quantity(max%),2)
            call "CONVERT" (quantity(max%), 0.2, quantity$(max%))
            newdate%(max%)=outdate%(max%)
            newquantity(max%)=quantity(max%)
            goto L30085

L30190:     if jp$="J" then return
            intagnr$="JOB(PR):" & str(prjnr$,1,8) & hex(000000)
L30205:     if max%>maxitems% then return
            call "PLOWNEXT" (#5, intagnr$, 16%, f1%(5))
            if f1%(5)=0 then return
            max%=max%+1
            get #5,using L30240,part$(max%),outdate%(max%), quantity(max%)
L30240:         FMT CH(25), BI(4),XX(19),PD(14,4)
            outdate$(max%)=yymmdd$(outdate%(max%))
            quantity(max%)= round(-quantity(max%),2%)
            newquantity(max%)=quantity(max%)
            newdate%(max%)=outdate%(max%)
            call "GETCODE" (#6,part$(max%),partdescr$(max%),0%,99,f1%(6))
            call "DATEFMT" (outdate$(max%))
            call "CONVERT" (quantity(max%), 0.2, quantity$(max%))
            goto L30205

L31000: REM *************************************************************~
            *  DATA SAVE SECTION - UPDATE PIP WITH NEW INFO             *~
            *                                                           *~
            * REPLACES OLD WITH NEW IF NEEDED.                          *~
            *************************************************************

            if max%=0 then inputmode

            call "SHOSTAT" ("Updating Files, One Moment Please")

            if jp$="J" then outtagnr$="JOB ORDER: " & str(prjnr$,1,8)
            if jp$="P" then outtagnr$="JOB(PROJ): " & str(prjnr$,1,8)
            pipinseq%=0

            t% = maxitems% + 10%  /* Little work spot */
L31140:     call "PLOWNXT1" (#4, outtagnr$, 19%, f1%(4))
            if f1%(4)=0 then L31230
            get #4,using L31170, part$(t%), outdate%(t%), quantity(t%)
L31170:         FMT XX(19), CH(25), BI(4), XX(8), PD(14,4)
            delete #4
            call "PIPFLAGS"(part$(t%),1%,outdate%(t%),quantity(t%),#8,#9)
            goto L31140

L31230:     if jp$="J" then L31340
            intagnr$="JOB(PR):" & str(prjnr$,1,8) & hex(000000)
L31250:     call "PLOWNXT1" (#5, intagnr$, 16%, f1%(5))
            if f1%(5)=0 then L31340
            get #5, using L31280, part$(t%), outdate%(t%), quantity(t%)
L31280:         FMT CH(25), BI(4),XX(19),PD(14,4)
            delete #5
            call "PIPFLAGS" (part$(t%), 1%, outdate%(t%), -quantity(t%), ~
                          #8, #9)
            goto L31250

L31340:     for i%=1 to max%

            if abs(newquantity(i%))<.0001 then L31820
            if jp$="J" then L31390
            if newquantity(i%)<0 then L31590

L31390:     systime$=time
            temp=0
            put str(outtagnr$,20,30),using L31430,part$(i%),newdate%(i%), ~
                                         hex(00)
L31430:         FMT CH(25),BI(4),CH(1)
            call "PLOWNXT1" (#4, outtagnr$,48%,f1%(4))
            if f1%(4)=0 then L31500
            get #4, using L31470, systime$, temp
L31470:         FMT XX(48),CH(8),PD(14,4)

L31500:     temp=round(newquantity(i%)+temp,2%)
            put   #4, using L35265, str(outtagnr$,1,19), part$(i%),       ~
                newdate%(i%), systime$, temp
            if f1%(4)=0 then write #4 else rewrite #4

            call "PIPFLAGS" (part$(i%), 1%,newdate%(i%),-newquantity(i%),~
                          #8, #9)
            goto L31820

L31590:     temp=0
            put str(pipin$,1,46) using L31620, part$(i%), newdate%(i%),   ~
                               str(intagnr$,1,16), hex(00)
L31620:         FMT CH(25), BI(4), CH(16), CH(1)
            call "PLOWALTS" (#5, pipin$, 1%, 45%, f1%(5))
            if f1%(5)=0 then L31710
            get #5, using L31660, intagnr$, temp
L31660:         FMT XX(29), CH(19), PD(14,4)
            call "READ101" (#5,intagnr$,f1%(5))
            goto L31750

L31710:     pipinseq%=pipinseq%+1
            put str(intagnr$,17,3), using L31730, pipinseq%
L31730:         FMT PIC(###)

L31750:     temp=round(temp-newquantity(i%),2%)
            put   #5, using L35300, part$(i%), newdate%(i%), intagnr$,    ~
                                            temp ,newdate%(i%)
            if f1%(5)=0 then write #5 else rewrite #5

            call "PIPFLAGS" (part$(i%), 1%,newdate%(i%),-newquantity(i%),~
                          #8, #9)
L31820:     next i%

            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35265: FMT                      /* FILE: PIPOUT                       */~
            CH(19),              /* Tag number in level 2 planning     */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date out of PIP in date subscript  */~
            CH(8),               /* Time from the system clock         */~
            PD(14,4)             /* Quantity                           */~

L35300: FMT                      /* FILE: PIPIN                        */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date in subscript for PIP          */~
            CH(19),              /* Tag number in level 2 planning     */~
            PD(14,4),            /* Quantity                           */~
            BI(4)                /* Date to start as a date subscript  */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101

L40240:     accept                                                       ~
               at (01,02), "Manually Manage Project Kit Lists",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,02), fac(hex(8c)), prjdstr$               , ch(10),~
               at (03,17), fac(hex(81)), prjnr$                 , ch(08),~
               at (03,43), fac(hex(8c)), prjnrdescr$            , ch(32),~
               at (04,02),                                               ~
                  "Part To Build",                                       ~
               at (04,17), fac(hex(84)), parta$                 , ch(25),~
               at (04,43), fac(hex(8c)), partadescr$            , ch(34),~
               at (05,02),                                               ~
                  "Quantity",                                            ~
               at (05,17), fac(hex(84)), prjqty$                , ch(10),~
               at (06,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02),                                               ~
                  "P.F. Keys Active:",                                   ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,45),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40670
                  call "MANUAL" ("PRJCOMIT")
                  goto L40240

L40670:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40240

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'202(screenline%,fieldnr%)
                  screen%=2
                  if fieldnr% = 3 then                                   ~
                             call "STRING" addr("LJ", quantity$(c%), 10%)
                  init (hex(86)) lfac$()
                  if fieldnr% = 0 then  goto L41080  else goto L41070

            deffn'201(screenline%,fieldnr%)
                  screen%=1
L41070:           if fieldnr%=5 then init(hex(8c)) lfac$() else          ~
                                                    init(hex(84)) lfac$()
L41080:           on fieldnr% gosub L41170,         /* PART             */~
                                    L41170,         /* DESCRIPTION      */~
                                    L41200,         /* QUANTITY         */~
                                    L41170,         /* DATE OUT         */~
                                    L41230          /* SPEC DELETE      */
                     goto L41240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(screenline%,fieldnr%) = hex(80)
                      return
L41170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(screenline%,fieldnr%) = hex(81)
                      return
L41200:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(screenline%,fieldnr%) = hex(82)
                      return
L41230:           REM SET BLINKING FAC FOR QUANTITY
                      init (hex(94)) str(lfac$(),4*screenline%-3%,4)
                      return
L41240:     accept                                                       ~
               at (01,02), "Manually Manage Project Kit Lists",          ~
               at (01,66), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "Page 1",                                              ~
               at (03,02), fac(hex(8c)), prjdstr$               , ch(10),~
               at (03,17), fac(hex(84)), prjnr$                 , ch(08),~
               at (03,43), fac(hex(8c)), prjnrdescr$            , ch(32),~
               at (04,02),                                               ~
                  "Part To Build",                                       ~
               at (04,17), fac(hex(84)), parta$                 , ch(25),~
               at (04,43), fac(hex(8c)), partadescr$            , ch(27),~
               at (05,02),                                               ~
                  "Quantity",                                            ~
               at (05,17), fac(hex(84)), prjqty$                , ch(10),~
               at (06,02), fac(hex(94)), errormsg$              , ch(79),~
               at (07,02), fac(hex(ac)), header$                , ch(79),~
                                                                         ~
               at (08,02), fac(lfac$( 1, 1)), part$( 1+line%)   , ch(25),~
               at (09,02), fac(lfac$( 2, 1)), part$( 2+line%)   , ch(25),~
               at (10,02), fac(lfac$( 3, 1)), part$( 3+line%)   , ch(25),~
               at (11,02), fac(lfac$( 4, 1)), part$( 4+line%)   , ch(25),~
               at (12,02), fac(lfac$( 5, 1)), part$( 5+line%)   , ch(25),~
               at (13,02), fac(lfac$( 6, 1)), part$( 6+line%)   , ch(25),~
               at (14,02), fac(lfac$( 7, 1)), part$( 7+line%)   , ch(25),~
               at (15,02), fac(lfac$( 8, 1)), part$( 8+line%)   , ch(25),~
               at (16,02), fac(lfac$( 9, 1)), part$( 9+line%)   , ch(25),~
               at (17,02), fac(lfac$(10, 1)), part$(10+line%)   , ch(25),~
               at (18,02), fac(lfac$(11, 1)), part$(11+line%)   , ch(25),~
               at (19,02), fac(lfac$(12, 1)), part$(12+line%)   , ch(25),~
                                                                         ~
               at (08,28), fac(lfac$( 1, 2)),partdescr$( 1+line%),ch(32),~
               at (09,28), fac(lfac$( 2, 2)),partdescr$( 2+line%),ch(32),~
               at (10,28), fac(lfac$( 3, 2)),partdescr$( 3+line%),ch(32),~
               at (11,28), fac(lfac$( 4, 2)),partdescr$( 4+line%),ch(32),~
               at (12,28), fac(lfac$( 5, 2)),partdescr$( 5+line%),ch(32),~
               at (13,28), fac(lfac$( 6, 2)),partdescr$( 6+line%),ch(32),~
               at (14,28), fac(lfac$( 7, 2)),partdescr$( 7+line%),ch(32),~
               at (15,28), fac(lfac$( 8, 2)),partdescr$( 8+line%),ch(32),~
               at (16,28), fac(lfac$( 9, 2)),partdescr$( 9+line%),ch(32),~
               at (17,28), fac(lfac$(10, 2)),partdescr$(10+line%),ch(32),~
               at (18,28), fac(lfac$(11, 2)),partdescr$(11+line%),ch(32),~
               at (19,28), fac(lfac$(12, 2)),partdescr$(12+line%),ch(32),~
                                                                         ~
               at (08,61), fac(lfac$( 1, 3)), quantity$( 1+line%),ch(10),~
               at (09,61), fac(lfac$( 2, 3)), quantity$( 2+line%),ch(10),~
               at (10,61), fac(lfac$( 3, 3)), quantity$( 3+line%),ch(10),~
               at (11,61), fac(lfac$( 4, 3)), quantity$( 4+line%),ch(10),~
               at (12,61), fac(lfac$( 5, 3)), quantity$( 5+line%),ch(10),~
               at (13,61), fac(lfac$( 6, 3)), quantity$( 6+line%),ch(10),~
               at (14,61), fac(lfac$( 7, 3)), quantity$( 7+line%),ch(10),~
               at (15,61), fac(lfac$( 8, 3)), quantity$( 8+line%),ch(10),~
               at (16,61), fac(lfac$( 9, 3)), quantity$( 9+line%),ch(10),~
               at (17,61), fac(lfac$(10, 3)), quantity$(10+line%),ch(10),~
               at (18,61), fac(lfac$(11, 3)), quantity$(11+line%),ch(10),~
               at (19,61), fac(lfac$(12, 3)), quantity$(12+line%),ch(10),~
                                                                         ~
               at (08,72), fac(lfac$( 1, 4)), outdate$ ( 1+line%),ch(08),~
               at (09,72), fac(lfac$( 2, 4)), outdate$ ( 2+line%),ch(08),~
               at (10,72), fac(lfac$( 3, 4)), outdate$ ( 3+line%),ch(08),~
               at (11,72), fac(lfac$( 4, 4)), outdate$ ( 4+line%),ch(08),~
               at (12,72), fac(lfac$( 5, 4)), outdate$ ( 5+line%),ch(08),~
               at (13,72), fac(lfac$( 6, 4)), outdate$ ( 6+line%),ch(08),~
               at (14,72), fac(lfac$( 7, 4)), outdate$ ( 7+line%),ch(08),~
               at (15,72), fac(lfac$( 8, 4)), outdate$ ( 8+line%),ch(08),~
               at (16,72), fac(lfac$( 9, 4)), outdate$ ( 9+line%),ch(08),~
               at (17,72), fac(lfac$(10, 4)), outdate$ (10+line%),ch(08),~
               at (18,72), fac(lfac$(11, 4)), outdate$ (11+line%),ch(08),~
               at (19,72), fac(lfac$(12, 4)), outdate$ (12+line%),ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. Keys Active:",                                   ~
               at (23,02), fac(hex(8c)), screen$(1,screen%)     , ch(79),~
               at (24,02), fac(hex(8c)), screen$(2,screen%)     , ch(79),~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 13 then L41995
                  call "MANUAL" ("PRJCOMIT")
                  goto L41240

L41995:        if keyhit% <> 15 then L42011
                  call "PRNTSCRN"
                  goto L41240

L42011:        if screen%=1 then return
               close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151
                  errormsg$ = " "
            REM TEST DATA FOR PROJECT
                call "READ100" (#2, prjnr$, f1%(2))
                if f1%(2) = 0 then L50220           /* TRY PROJECT      */
                get #2, using L50120, prjnrdescr$, parta$, prjqty
L50120:         FMT XX(8), CH(30), XX(19), CH(25), PD(14,4)
                call "GETCODE" (#6, parta$, partadescr$, 1%, 99, f1%(6))
                if f1%(6) <>0 then L50170
                     errormsg$="Part To Build Not On File"
                     return
L50170:         call "CONVERT" (prjqty, -0.2, prjqty$)
                prjdstr$="Job Order#"
                jp$="J"
                goto L30000                      /* TRY TO LOAD KIT LIST */

L50220:     REM TEST DATA FOR PROJ
                call "READ100" (#3, prjnr$, f1%(3))
                if f1%(3) = 0 then L50330           /* NO BANANA        */
L50250:         get #3, using L50260, prjnrdescr$, parta$, prjqty
L50260:         FMT XX(8), CH(30), XX(21), CH(25), PD(14,4)
                call "GETCODE" (#6, parta$, partadescr$, 1%, 99, f1%(6))
                call "CONVERT" (prjqty, -0.2, prjqty$)
                prjdstr$="Project#"
                jp$="P"
                goto L30000                      /* TRY TO LOAD KIT LIST */

L50330:     REM NO LUCK TODAY
                init(" ") plowkey$
                descr$ = hex(06) & "Select Project Number"
                call "PLOWCODE" (#3, plowkey$, descr$, 0%, .30, f1%(3))
                  if f1%(3) = 0% then L50390
                     prjnr$ = str(plowkey$,1,8)
                     goto L50250
L50390:         errormsg$ = "Project Number Not On File"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE LINE ITEMS                             *~
            *************************************************************

            deffn'152(c%,fieldnr%)
            errormsg$=" "
            on fieldnr% gosub L51140,               /* PART             */~
                              L51280,               /* DESCRIPTION      */~
                              L51310,               /* QUANTITY         */~
                              L51460                /* DATE OUT         */
            return

L51140: REM TEST DATA FOR PART
            if jp$="P" or part$(c%)<>parta$ then L51180
                errormsg$ = "Cannot Commit 'Part To Build' To Project"
                return
L51180:     call "GETCODE" (#6, part$(c%), partdescr$(c%), 0%, 0, f1%(6))
                if f1%(6)<>0 then L51220
                errormsg$="Part Not On File"
                return
L51220:     get #6, using L51230, type$
L51230:         FMT POS(180), CH(3)
            if type$ > "199" then return
                errormsg$ = "Not A Planned Part - Type = " & type$
                return

L51280: REM TEST DATA FOR DESCRIPTION
            return

L51310: REM TEST DATA FOR QUANTITY
            call "NUMTEST" (quantity$(c%), -9e7, 9e7, errormsg$, -0.2,   ~
                                                         newquantity(c%))
                if errormsg$ <> " " then return
            if jp$="P" or newquantity(c%) >= 0 then return
            REM Yield from a Project, check part type...
                call "READ100" (#6, part$(c%), f1%(6))
                get #6, using L51390, type$
L51390:         FMT POS(180), CH(3)
                convert type$ to type%, data goto L51410
L51410:         if type% > 489% and type% < 500% then return
                if type% > 789% and type% < 800% then return
                   errormsg$="Invalid Quantity For Project"
                   return

L51460: REM TEST DATA FOR DATE
            call "DATEOK" (outdate$(c%), ser%(1), errormsg$)
            if errormsg$<>" " then return
            temp$=outdate$(c%)
            call "DATUNFMT" (temp$)
            search str(yymmdd$(),1)=str(temp$,1,6) to ser%() step 6
            if ser%(1)<>0 then L51550
                errormsg$ = "Date Outside Planning Range"
                return
L51550:     newdate%(c%)=1+(ser%(1)/6%)
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
