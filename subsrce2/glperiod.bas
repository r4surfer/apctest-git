        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   L      PPPP   EEEEE  RRRR   IIIII   OOO   DDDD    *~
            *  G      L      P   P  E      R   R    I    O   O  D   D   *~
            *  G      L      PPPP   EEE    RRRR     I    O   O  D   D   *~
            *  G  GG  L      P      E      R   R    I    O   O  D   D   *~
            *   GGG   LLLLL  P      EEEEE  R   R  IIIII   OOO   DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLPERIOD -  Specialized GETCODE for selecting GL period.  *~
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
            * 04/18/86 ! ORIGINAL                                 ! HES *~
	    * 06/25/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

            sub "GLPERIOD" (#1,          /* SYSFILE2 CHANNEL           */~
                           period%,      /* PERIOD RETURNED            */~
                           descr$,       /* Period description         */~
                           paren%,       /* NON ZERO PUTS PAREN ON DESC*/~
                           mode%,        /* 0 = All Periods are allowed*/~
                                         /* 1 = All Up to Current      */~
                                         /* 2 = Only Periods Open      */~
                           return%)      /* RETURN STATUS FOR SELECTION*/~
                                         /* 1 = PERIOD SELECTED        */~
                                         /* 0 = PERIOD NOT SELECTED,   */~
                                         /* POSSIBLE IF PF 16 IS PRESSD*/~

            dim bfac$1,                  /* SCREEN FAC                 */~
                begdates$(17)8,          /* Period Beginning Dates     */~
                blankline$79,            /* WORK                       */~
		blankdate$8,             /* Blank date for comparison  */~
                bline$79,                /* Top Line Of Screen         */~
                c%(2),                   /* CURSOR POSITION            */~
                descr$50,                /* Code Description           */~
                enddates$(17)8,          /* Period Ending Dates        */~
                fac$(16)1,               /* SCREEN FACS                */~
                flag$(16)1,              /* WHAT ONE DO YOU WANT?      */~
                i$(24)80,                /* SCREEN IMAGE               */~
                screen$(16)77,           /* VARIABLE FOR ACCEPT        */~
                title$79                 /* SCREEN TITLE               */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * Initializes information necessary for program.            *~
            *************************************************************

            return% = 0%
            if begdates$() <> " " then L09170
            mode% = min(max(mode%, 0), 2)

            REM Load Fiscal Year Data...
            call "READ100" (#1, "FISCAL DATES", f1%)
                if f1% = 0% then end
            get #1 using L09150, nperiods%, begdates$(), open%, enddates$()
L09150:     FMT XX(20), BI(2), 17*CH(8), BI(2), XX(120), 17*CH(8)

L09170:     REM Set up screen body...
	    blankdate$ = " "
	    call "DATUFMTC" (blankdate$)
            screen$() = " " : c% = 0
            for i% = 1% to 16%
                if begdates$(i%) = " " ~
		   or begdates$(i%) = blankdate$ then L09460
                if mode% = 0 then L09270   /* Allow selection of any */
                     if mode% <> 0 and open% = 12 and i% = 14 then L09270
                     if mode% <> 0 and i% > open% + 1 then L09460
                     if mode% = 2 and open% = 14 and i% = 12 then L09270
                     if mode% = 2 and i% < open% - 1 then L09460

L09270:         x%, c% = c% + 1
                str(screen$(c%), 6) = begdates$(i%) & "   Thru " &       ~
                                                            enddates$(i%)
                call "DATEFMT" (str(screen$(c%),6,8))
                call "DATEFMT" (str(screen$(c%),20,8))
                convert i% to str(screen$(c%),,2), pic(##)
                str(screen$(c%),3,1) = ")"
                if i% = period% then all_done   /* Direct Hit */
                if mode% = 2 then L09460
                if i% = open% then screen$(c%) = screen$(c%) &           ~
                                               "   (Current Period Open)"
                if i% = open%-1 then screen$(c%) = screen$(c%) &         ~
                                               "   (Also Open)"
                if i% = open%+1 then screen$(c%) = screen$(c%) &         ~
                                               "   (Also Open)"
                if i% = 12 and open% = 14 and nperiods% = 12 then        ~
                             screen$(c%) = screen$(c%) & "   (Also Open)"
                if i% = 14 and open% = 12 and nperiods% = 12 then        ~
                             screen$(c%) = screen$(c%) & "   (Also Open)"
L09460:     next i%

        REM *************************************************************~
            *                 S H O W   P E R I O D S                   *~
            *-----------------------------------------------------------*~
            * PERIOD PASSED IS NOT VALID, FIND & SHOW'EM WHAT IS....    *~
            *************************************************************~

*        INITIALIZE VARIABLES
            blankline$ = "Active P.F. Keys Are :"
            bline$ = " PF Key 16 may be used to end display and return wi~
        ~th no selection."
            if str(descr$,,1)=hex(06) then bline$=hex(84) & str(descr$,2)
            flag$() = all(hex(0b))
            fac$() = all(hex(9c))
            str(fac$(),,c%) = all(hex(86))
            bfac$ = hex(ac)
            blankline$ = "Active P.F. Keys Are :"

            REM Set Up Title Line...
            title$ = "  Period   Description"

            REM Tack On The Screen Name...
            str(title$,63%) = "GLPERIOD:" & str(cms2v$,,8%)

L10230: accept                                                           ~
           at(01,02), "Postion cursor (Tab) to line and press (RETURN) to~
        ~ return with that Period",                                       ~
           at(02,02), fac(hex(8c)),  bline$                      ,ch(79),~
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
           at(23,02), "Cursor & RETURN: Return With Period",             ~
           at(23,44), "(13)Instructions",                                ~
           at(23,64), "(15)Print Screen",                                ~
           at(24,64), "(16)Cancel & Exit",                               ~
                                                                         ~
            keys(hex(000f2010)),                                         ~
            key(keyhit%)
            if keyhit% = 16 then end        /* PROCESSING ABORTED */
            if keyhit% = 32 then end        /* PROCESSING ABORTED */

            if keyhit% <> 13 then L10780
               call "MANUAL" ("GLPERIOD")
               goto  L10230

L10780:     if keyhit% <> 15 then L10820
               call "PRNTSCRN"
               goto  L10230

L10820:     if keyhit% <> 0 then L10230
            close ws
            call "SCREEN" addr("C", x%, "I", i$(), c%())
            x% = c%(1) - 4
            if x% > c% or x% < 1 then L10230

        all_done
            REM Pass Selection Back
            descr$ = str(screen$(x%),6,22)
            convert str(screen$(x%),,2) to period%, data goto L10230
            if paren% <> 0 then call "PUTPAREN" (descr$)
            return% = 1
            end
