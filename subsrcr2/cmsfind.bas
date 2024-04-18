        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  CCCC   M   M  SSSS   FFFFF  IIIII  N   N  DDDD           *~
            *  C      MM MM  S      F        I    NN  N  D   D          *~
            *  C      M M M  SSSS   FFFF     I    N N N  D   D          *~
            *  C      M   M      S  F        I    N  NN  D   D          *~
            *  CCCC   M   M  SSSS   F      IIIII  N   N  DDDD           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CMSFIND  -  Specialized GETCODE for SES searching by type *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/27/85 ! ORIGINAL (CONED FROM GETCODE)            ! HES *~
            * 07/23/85 ! ADDED CALL TO CMSSECUR                   ! GLW *~
            * 09/05/86 ! Major changes to only show those codes   !     *~
            *          !   actually accessible by the user from   !     *~
            *          !   his menu's.                            !     *~
            *************************************************************

            sub "CMSFIND" (#1,           /* INTDOC01 FILE              */~
                           #2,           /* INTDOC02 FILE              */~
                                                                         ~
                          key$,          /* COMING IN: RECORD TO SEARCH*/~
                                         /* FOR. GOING OUT: RECORD     */~
                                         /* SELECTED, OR UNCHANGED IF  */~
                                         /* KEY PASSED IN IS ON FILE   */~
                                                                         ~
                          descr$,        /* Code description           */~
                                                                         ~
                          altkeydescr,   /* FLOATING POINT WHERE THE   */~
                                         /* INTEGER PORTION IS THE ALT */~
                                         /* KEY NUMBER OF DESCRIPTION, */~
                                         /* TAKEN DIRECTLY OFF THE     */~
                                         /* FILES SELECT STATEMENT IN  */~
                                         /* THE CALLING PROGRAM.       */~
                                         /* *ZERO* INDICATES DESCRIPT  */~
                                         /* IS NOT AN ALTERNATE KEY.   */~
                                         /* FRACTION OF NUMBER IS THE  */~
                                         /* BREAK POINT.               */~
                                                                         ~
                          slib$(),       /* SES LIBRARIES       MUST   */~
                                         /* BY SET UP AS PARENT IN     */~
                                         /* BLANK IF NO RESTRICTION    */~
                                                                         ~
                          sec$,          /* Current User Security Codes*/~
                                                                         ~
                          f1%)           /* RETURN STATUS FOR RECORD.  */~
                                         /* 1 = RECORD FOUND/SELECTED  */~
                                         /* 0 = REC NOT ON FILE. ONLY  */~
                                         /* POSSIBLE IF PF 16 IS PRESSD*/~

            dim bfac$1,                  /* SCREEN FAC                 */~
                blankline$79,            /* WORK                       */~
                bline$79,                /* Top Line Of Screen         */~
                c%(2),                   /* CURSOR POSITION            */~
                descr$50,                /* Code Description           */~
                fac$(16)1,               /* SCREEN FACS                */~
                f1%(2),                  /* READ Status flags          */~
                flag$(16)1,              /* WHAT ONE DO YOU WANT?      */~
                i$(24)80,                /* SCREEN IMAGE               */~
                pgmsec$5,                /* Program Security Code      */~
                keys$16,                 /* Active Pf Keys             */~
                slib$(4)6,               /* SES 'Library' Codes        */~
                readkey$99,              /* KEY FOR PLOW               */~
                screen$(16)77,           /* VARIABLE FOR ACCEPT        */~
                p%(2),                   /* Search Receiver Array      */~
                primary$(30,1)1,         /* CODE TO SEARCH FOR         */~
                pfac$1,                  /* SCREEN FAC                 */~
                plowkey$99,              /* MISCELLANEOUS PLOW KEY     */~
                obs$1,                   /* Obsolete flag              */~
                last_version$6,          /* SES Version Number         */~
                work$100,                /* WORK AREA FOR TRANSFER     */~
                search$30,               /* Search Message             */~
                seqnr$3,                 /* File Sequence Number       */~
                text$50,                 /* DESCRIPTION WORK AREA.     */~
                title$79,                /* SCREEN TITLE               */~
                type$16,                 /* FORMATTED TYPE             */~
                ufbkl$1,                 /* KEY LENGTH THIS FILE       */~
                ufbkd$2,                 /* KEY DISPLACEMENT REL TO 0  */~
                key$50,                  /* READ KEY                   */~
                file$8,                  /* RECEIVER FOR FILE NAME     */~
                lib$8,                   /* RECEIVER FOR LIBRARY NAME  */~
                vol$6                    /* RECEIVER FOR VOLUME NAME   */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.16.07 01/05/87 OS 7.10 Compatibility           "
        REM *************************************************************
            init (hex(00)) ufbkl$, ufbkd$
            file$ = " "
            f1% = 0%
            call "GETNAMES" addr(#1, file$, lib$, vol$)
            call "GETUFBKL" addr(#1, ufbkl$)       /* KEY LENGTH (BIN) */
            call "GETUFBDK" addr(#1, ufbkd$)       /* KEY DISPL. (BIN) */
            disp% = 1+val(ufbkl$)+val(ufbkd$,2)
            if disp% > 468% then end
            key%    = int(altkeydescr)
            breakp% = min(50, mod(altkeydescr*100,100)+.1)
            if str(key$,breakp%+1) = " " then L01380

*       *** If it's on file, then wip right back with minimum effort ****
            call "REDALT0" (#1, key$, key%, f1%)
            if f1% = 0% then L01380
            if slib$() = " " then all_done
                REM See if this element is a parent in SLIB$ in INTDOC02
                for x% = 1% to dim(slib$(),1)
                    work$ = str(key$,breakp%+1,16) & str(slib$(x%))
                    call "PLOWALTS" (#2, work$, 2%, 22%, f2%)
                    if f2% > 0% then all_done
                next x%
L01380: REM *************************************************************~
            *              M A I N   P L O W   L O O P                  *~
            *-----------------------------------------------------------*~
            * CODE PASSED IS NOT ON FILE, FIND & SHOW'EM WHAT IS....    *~
            *************************************************************~

*        INITIALIZE VARIABLES
            if altkeydescr<0 or altkeydescr>=17 then all_done
            sublen% = len(str(key(#1,key%))) - breakp%
            anyhits%, c%, seached_once% = 0
            screen$(), text$ = " "
            blankline$ = "Active P.F. Keys Are :"
            type$ = str(key$,,breakp%)
            for i% = 2 to breakp%
                if str(key$,i%,1)<"A" or str(key$,i%,1)>"Z" then L01700
                str(type$,i%,1) = str(key$,i%,1) or " "
L01700:     next i%
            search$ = "(8)Search For " & type$
            search$ = search$ & ":"
            slen% = len(search$)
            bline$ = " PF Key 16 may be used to end display and return wi~
        ~th no selection."
            if slib$() = " " then L01960
            bline$ = " Shown Below Are " & str(key$,,breakp%)
            if dim(slib$(),1) = 1% then                                  ~
            bline$ = bline$ &"S Existing In Library:"& hex(84)& slib$(1) ~
            else                                                         ~
            bline$ = bline$ &"S Accessible From Your Menu's"

L01960: REM Split logic... try to get close to the desired record
            readkey$ = key$
            str(readkey$,breakp%+1,sublen%) =                            ~
                            str(key$,breakp%+1,sublen%) addc all(hex(ff))

        REM Redimension variables to passed/extracted key lengths
            mat redim primary$(min(30,sublen%),1)1
            primary$() = str(key$,breakp%+1)
            if type$ = "Library" then mat redim primary$(6,1)1

L02160:         FMT POS(882), CH(5), POS(936), CH(1)

        plow_codes
            call "PLOWALTS" (#1, readkey$, key%, breakp%, f1%)
                 if f1% = 0 then show_codes

            get #1 using L02160, pgmsec$, obs$
            if obs$ = "Y" then plow_codes
            pgmsec$ = boolb sec$
            if pos(pgmsec$ <> hex(ff)) > 0% then plow_codes
            if slib$() = " " then L02800
        REM See if this element is a component in SLIB$ in INTDOC2
            work$ = str(readkey$,breakp%+1,16)
            z% = 0%
L02440:     call "PLOWALTS" (#2, work$, 1%, 16%, f2%)
            if f2% = 0% then plow_parents
            z% = 1%  : seqnr$ = str(key(#2,0),29%,3%)
            call "READ100" (#1, str(work$,23%,16%), f1%(1))
            if f1%(1) = 0% then L02560
            if str(key(#1,1),,8%) <> "MENU" then L02440
L02560:     search slib$() = str(work$,39%,6%) to p%() step 6
            if p%(1) = 0% then L02440
            plowkey$ = key(#2,2)
            last_version$ = str(plowkey$, 39%,6%)
L02640:     call "PLOWALTS" (#2, plowkey$, 2%, 38%, f1%(2))
            if f1%(2) = 0% then L02742
            last_version$ = str(plowkey$,39%,6%)
            goto L02640
L02742:     plowkey$ = str(work$,,16%) & "      " &                      ~
                       str(plowkey$,,16%) & str(plowkey$,17%,6%) &       ~
                       str(plowkey$,,16%) & str(last_version$,,6%) &     ~
                       str(plowkey$,17%,6%) & hex(000000)
            call "PLOWALTS" (#2, plowkey$, 1%, 72%, f1%(2))
            if f1%(2) = 0% then L02440
L02800:     c% = c% + 1
            anyhits% = 1
            gosub describe_it
            screen$(c%) = str(readkey$,breakp%+1,sublen%) & "   " & text$
            if c% = 16 then show_codes
            goto plow_codes

        plow_parents
            on z% goto plow_codes
            if key(#1) <> str(readkey$,9%) then                          ~
               call "READ100" (#1, str(readkey$,9%), f1%(1))

            if str(key(#1,1),,8%) <> "MENU" then plow_codes
            plowkey$ = str(readkey$,breakp%+1,sublen%)
L03060:     call "PLOWALTS" (#2, plowkey$, 2%, 16%,f1%(2))
            if f1%(2) = 0% then plow_codes
            search slib$() = str(plowkey$,17%,6%) to p%() step 6
            if p%(1) = 0% then L03060
            goto L02800

        describe_it
            call "READ100" (#1, str(readkey$,9%), f1%(1))
            if f1%(1) = 0% then return
            get #1, work$                     /* record work area */
            text$ = str(work$, disp%, 30%)
            if file$ = "INTDOC01" then text$ = str(work$, disp%, 50%)

*          IF FILE$ = "XXXXXXXX" THEN TEXT$ = STR(WORK$, X, BREAKP%)

        REM This is how to access a file that's description does not
*          immediatly follow the primary key.
        return

        show_codes
            x% = 0
            flag$() = all(hex(0b))
            init(hex(9c)) pfac$, fac$()
            bfac$ = hex(ac)
            keys$ = hex(000205080f2010)
            blankline$ = "Active P.F. Keys Are :"
            if c% + anyhits% > 0 then L03720  /* Anything To Show? */
        REM Don't want end of file msg to come up if we just came in
            if seached_once% = 1 then all_done
            seached_once% = 1
            if pos(str(key$,breakp%+1) > hex(20)) <> 0 then L05460
        rem Nothing Meets Criteria : goto all_done

L03720:     if c% = 0 then str(screen$(8),27) = "****  END OF FILE ****" ~
                            else str(fac$(),,c%) = all(hex(86))

        REM Set Up Title Line...
            title$ = "  " & type$
            title$ = title$ & " Name"
            str(title$,6+sublen%) = type$
            title$ = title$ & " Description"

        REM Tack On The File Name...
            str(title$,66) = "FILE: " & file$

L03960:     flag$(01) = hex(0b)
        accept                                                           ~
           at(01,02), "Postion cursor (Tab) to line and press (RETURN) to~
        ~ return with that code",                                         ~
           at(02,02), fac(hex(8c)), bline$,                              ~
           at(03,02), fac(hex(ac)),  title$                      ,ch(79),~
                                                                         ~
           at(05,02), fac(hex(82)),  flag$(1)                    ,ch(01),~
           at(05,02), fac(fac$(01)), flag$(1)                    ,ch(01),~
           at(06,02), fac(fac$(02)), flag$(2)                    ,ch(01),~
           at(07,02), fac(fac$(03)), flag$(3)                    ,ch(01),~
           at(08,02), fac(fac$(04)), flag$(4)                    ,ch(01),~
           at(09,02), fac(fac$(05)), flag$(5)                    ,ch(01),~
           at(10,02), fac(fac$(06)), flag$(6)                    ,ch(01),~
           at(11,02), fac(fac$(07)), flag$(7)                    ,ch(01),~
           at(12,02), fac(fac$(08)), flag$(8)                    ,ch(01),~
           at(13,02), fac(fac$(09)), flag$(9)                    ,ch(01),~
           at(14,02), fac(fac$(10)), flag$(10)                   ,ch(01),~
           at(15,02), fac(fac$(11)), flag$(11)                   ,ch(01),~
           at(16,02), fac(fac$(12)), flag$(12)                   ,ch(01),~
           at(17,02), fac(fac$(13)), flag$(13)                   ,ch(01),~
           at(18,02), fac(fac$(14)), flag$(14)                   ,ch(01),~
           at(19,02), fac(fac$(15)), flag$(15)                   ,ch(01),~
           at(20,02), fac(fac$(16)), flag$(16)                   ,ch(01),~
                                                                         ~
           at(05,04), fac(hex(8c)), screen$(1)                   ,ch(77),~
           at(06,04), fac(hex(8c)), screen$(2)                   ,ch(77),~
           at(07,04), fac(hex(8c)), screen$(3)                   ,ch(77),~
           at(08,04), fac(hex(8c)), screen$(4)                   ,ch(77),~
           at(09,04), fac(hex(8c)), screen$(5)                   ,ch(77),~
           at(10,04), fac(hex(8c)), screen$(6)                   ,ch(77),~
           at(11,04), fac(hex(8c)), screen$(7)                   ,ch(77),~
           at(12,04), fac(hex(8c)), screen$(8)                   ,ch(77),~
           at(13,04), fac(hex(8c)), screen$(9)                   ,ch(77),~
           at(14,04), fac(hex(8c)), screen$(10)                  ,ch(77),~
           at(15,04), fac(hex(8c)), screen$(11)                  ,ch(77),~
           at(16,04), fac(hex(8c)), screen$(12)                  ,ch(77),~
           at(17,04), fac(hex(8c)), screen$(13)                  ,ch(77),~
           at(18,04), fac(hex(8c)), screen$(14)                  ,ch(77),~
           at(19,04), fac(hex(8c)), screen$(15)                  ,ch(77),~
           at(20,04), fac(hex(8c)), screen$(16)                  ,ch(77),~
                                                                         ~
           at(22,02), fac(bfac$), blankline$                     ,ch(79),~
           at(23,02), "(2)First  (5)Next Screen",                        ~
           at(23,46), "(15)Print Screen",                                ~
           at(23,64), "(16)Cancel & Exit",                               ~
           at(24,02), fac(hex(8c)), str(search$,,slen%),                 ~
                      fac(pfac$), str(primary$()),                       ~
           at(24,48), "Cursor & RETURN: Return With Code",               ~
                                                                         ~
            keys(keys$),                                                 ~
            key(keyhit%)
            if keyhit% = 16 then all_done   /* PROCESSING ABORTED */
            if keyhit% = 32 then all_done   /* PROCESSING ABORTED */

            if keyhit% <> 15 then L05140
               call "PRNTSCRN"
               goto  L03960

L05140:     if keyhit% <> 8 then L05340
               pfac$ = hex(81)   /* Enable For Search */
               blankline$="Type As Much Of The " & type$
               blankline$ = blankline$ &                                 ~
                                 " Name As Is Known, Then Press (RETURN)"
               keys$ = hex(000d0f10)
               tran(fac$(), hex(8c86))replacing
               bfac$ = hex(a4)
               goto  L03960

L05340:     if keyhit% <> 0 then L05460    /* Search Mode? */
               if pfac$ <> hex(81) then L05460
                  str(readkey$,breakp%+1, pos(-str(primary$())           ~
                       >= hex(00))) = str(primary$())   addc all(hex(ff))
                  goto L05580

L05460:     if keyhit% = 2 or c% = 0 then                                ~
                                str(readkey$,breakp%+1) = all(hex(00))
            if c% = 0 then screen$() = " "
            if c% = 0 then plow_codes

            if keyhit% = 0 then L05640
L05580:        screen$() = " " : c% = 0
               goto plow_codes

L05640:     close ws
            call "SCREEN" addr("C", x%, "I", i$(), c%())
            x% = c%(1) - 4
            if x% > c% or x% < 1 then L03960

        REM Pass Selection Back
            str(key$,breakp%+1) = str(screen$(x%),,sublen%)

        all_done
            close ws
            call "REDALT0" (#1, key$, key%, f1%)
            if f1% > 0% then descr$ = key(#1,3)
        end
