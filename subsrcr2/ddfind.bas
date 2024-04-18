        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  DDDD   DDDD   FFFFF  IIIII  N   N  DDDD                  *~
            *  D   D  D   D  F        I    NN  N  D   D                 *~
            *  D   D  D   D  FFFF     I    N N N  D   D                 *~
            *  D   D  D   D  F        I    N  NN  D   D                 *~
            *  DDDD   DDDD   F      IIIII  N   N  DDDD                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DDFIND   -  Specialized GETCODE for SES searching by type *~
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
            * 04/04/85 ! ORIGINAL (CLONED 95% FROM HES's SASFIND) ! LDJ *~
            *************************************************************

            sub "DDFIND"  (#1,           /* INTDOC01 FILE              */~
                           #2,           /* INTDOC02 FILE              */~
                                                                         ~
                          key$,          /* COMING IN: RECORD TO SEARCH*/~
                                         /* FOR. GOING OUT: RECORD     */~
                                         /* SELECTED, OR UNCHANGED IF  */~
                                         /* KEY PASSED IN IS ON FILE   */~
                                         /* First 8 bytes of Key must  */~
                                         /* be Type to search for!     */~
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
                          slib$,         /* SES LIBRARY ELEMENT MUST   */~
                                         /* BY SET UP AS PARENT IN     */~
                                         /* BLANK IF NO RESTRICTION    */~
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
                flag$(16)1,              /* WHAT ONE DO YOU WANT?      */~
                i$(24)80,                /* SCREEN IMAGE               */~
                keys$16,                 /* Active Pf Keys             */~
                slib$6,                  /* SES 'Library' Code         */~
                readkey$50,              /* KEY FOR PLOW               */~
                screen$(16)77,           /* VARIABLE FOR ACCEPT        */~
                primary$(30,1)1,         /* CODE TO SEARCH FOR         */~
                pfac$1,                  /* SCREEN FAC                 */~
                work$100,                /* WORK AREA FOR TRANSFER     */~
                search$30,               /* Search Message             */~
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
            cms2v$ = "04.16.02 05/22/86 Sales Quote, Ven. Price & Misc  "
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
            if str(key$,breakp%+1) = " " then L09900

*       *** IF IT'S ON FILE, THEN WIP RIGHT BACK WITH MINIMUM EFFORT ****
            call "REDALT0" (#1, key$, key%, f1%)
            if f1% = 0% then L09900
            if slib$ = " " then all_done
                REM See if this element is a parent in SLIB$ in INTDOC02
                work$ = str(key$,breakp%+1,16) & str(slib$)
                call "PLOWALTS" (#2, work$, 2%, 22%, f2%)
                if f2% > 0% then all_done

L09900: REM *************************************************************~
            *              M A I N   P L O W   L O O P                  *~
            *-----------------------------------------------------------*~
            * CODE PASSED IS NOT ON FILE, FIND & SHOW'EM WHAT IS....    *~
            *************************************************************~

*        INITIALIZE VARIABLES
            if altkeydescr<0 or altkeydescr>=17 then all_done
            anyhits%, c%, seached_once% = 0
            screen$(), text$ = " "
            blankline$ = "Active P.F. Keys Are :"
            type$ = str(key$,,breakp%)
            for i% = 2 to breakp%
                if str(key$,i%,1)<"A" or str(key$,i%,1)>"Z" then L11400
                str(type$,i%,1) = str(key$,i%,1) or " "
L11400:     next i%
            search$ = "(8)Search For " & type$
            search$ = search$ & ":"
            slen% = len(search$)
            bline$ = " PF Key 16 may be used to end display and return wi~
        ~th no selection."
            if slib$ = " " then L12400
            bline$ = " Shown Below Are " & str(key$,,breakp%)
            bline$ = bline$ &"S Existing In Library:"& hex(84)& slib$

L12400
*        SPLIT LOGIC... TRY TO GET CLOSE TO THE DESIRED RECORD
            readkey$ = key$
            str(readkey$,breakp%+1,val(ufbkl$)) =                        ~
                        str(key$,breakp%+1,val(ufbkl$)) addc all(hex(ff))

*        REDIMENSION VARIABLES TO PASSED/EXTRACTED KEY LENGTHS
            mat redim primary$(min(30,val(ufbkl$)-breakp%),1)1
            primary$() = str(key$,breakp%+1)

        plow_codes
            call "PLOWALTS" (#1, readkey$, key%, breakp%, f1%)
                 if f1% = 0 then show_codes

L13700:     if slib$ = " " then L14300
                REM See if this element is a parent in SLIB$ in INTDOC02
                work$ = str(readkey$,breakp%+1,16) & str(slib$)
                call "PLOWALTS" (#2, work$, 2%, 22%, f2%)
                     if f2% = 0 then L15000

L14300:     c% = c% + 1
            anyhits% = 1
            gosub describe_it
            screen$(c%) = str(readkey$,breakp%+1,val(ufbkl$,1)) &        ~
                                                            "   " & text$
            if c% = 16 then show_codes

L15000: read_next
            read #1, eod goto show_codes
            if str(key(#1, key%),,breakp%) <> str(readkey$,,breakp%)     ~
                                                          then show_codes
            readkey$ = key(#1, key%)
            goto L13700

        describe_it
            get #1, work$                     /* RECORD WORK AREA */
            text$ = str(work$, disp%, 30%)
            if file$ = "INTDOC01" then text$ = str(work$, disp%, 50%)

*          IF FILE$ = "XXXXXXXX" THEN TEXT$ = STR(WORK$, X, BREAKP%)

*          THIS IS HOW TO ACCESS A FILE THAT'S DESCRIPTION DOES NOT
*          IMMEDIATLY FOLLOW THE PRIMARY KEY.
        return

        show_codes
            x% = 0
            flag$() = all(hex(0b))
            init(hex(9c)) pfac$, fac$()
            bfac$ = hex(ac)
            keys$ = hex(000205080f2010)
            blankline$ = "Active P.F. Keys Are :"
            if c% + anyhits% > 0 then L41400 /* Anything To Show? */
            REM Don't want end of file msg to come up if we just came in
            if seached_once% = 1 then all_done
            seached_once% = 1
            if pos(str(key$,breakp%+1) > hex(20)) <> 0 then L49800
            rem Nothing Meets Criteria : goto all_done

L41400:     if c% = 0 then str(screen$(8),27) = "****  END OF FILE ****" ~
                            else str(fac$(),,c%) = all(hex(86))

            REM Set Up Title Line...
            title$ = "  " & type$
            title$ = title$ & " Name"
            str(title$,6+val(ufbkl$)) = type$
            title$ = title$ & " Description"

            REM Tack On The Screen Name...
            str(title$,65%) = "DDFIND:" & str(cms2v$,,8%)

L42600: accept                                                           ~
           at(01,02), "Postion cursor (Tab) to line and press (RETURN) to~
        ~ return with that code",                                         ~
           at(02,02), fac(hex(8c)), bline$,                              ~
           at(03,02), fac(hex(ac)),  title$                      ,ch(79),~
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

            if keyhit% <> 15 then L48200
               call "PRNTSCRN"
               goto  L42600

L48200:     if keyhit% <> 8 then L49200
               pfac$ = hex(81)   /* Enable For Search */
               blankline$="Type As Much Of The " & type$
               blankline$ = blankline$ &                                 ~
                                 " Name As Is Known, Then Press (RETURN)"
               keys$ = hex(000d0f10)
               tran(fac$(), hex(8c86))replacing
               bfac$ = hex(a4)
               goto  L42600

L49200:     if keyhit% <> 0 then L49800   /* Search Mode? */
               if pfac$ <> hex(81) then L49800
                  str(readkey$,breakp%+1, pos(-str(primary$())           ~
                       >= hex(00))) = str(primary$())   addc all(hex(ff))
                  goto L50400

L49800:     if keyhit% = 2 or c% = 0 then                                ~
                                str(readkey$,breakp%+1) = all(hex(00))
            if c% = 0 then screen$() = " "
            if c% = 0 then plow_codes

            if keyhit% = 0 then L50800
L50400:        screen$() = " " : c% = 0
               if keyhit% = 5 then read_next
               goto plow_codes

L50800:     close ws
            call "SCREEN" addr("C", x%, "I", i$(), c%())
            x% = c%(1) - 4
            if x% > c% or x% < 1 then L42600

            REM Pass Selection Back
            str(key$,breakp%+1) = str(screen$(x%),,val(ufbkl$))

        all_done
            close ws
            call "REDALT0" (#1, key$, key%, f1%)
            if f1% > 0% then descr$ = key(#1,3)
            end
