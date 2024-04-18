        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  DDDD   DDDD    SSS   EEEEE   AAA   RRRR    CCC   H   H   *~
            *  D   D  D   D  S      E      A   A  R   R  C   C  H   H   *~
            *  D   D  D   D   SSS   EEEE   AAAAA  RRRR   C      HHHHH   *~
            *  D   D  D   D      S  E      A   A  R  R   C   C  H   H   *~
            *  DDDD   DDDD    SSS   EEEEE  A   A  R   R   CCC   H   H   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DDSEARCH -  Subroutine to list the components of a        *~
            *             specific System Elements Structure showing    *~
            *             their names & descriptions.  User may select  *~
            *             one of the descriptions shown for optional    *~
            *             carry-back to the calling application.        *~
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
            * 04/04/85 ! ORIGINAL (CLONED 80% FROM HES's SASFIND) ! LDJ *~
            *************************************************************

            sub "DDSEARCH" (#1,          /* INTDOC01 FILE              */~
                            #2,          /* INTDOC02 FILE              */~
                            lib$,        /* S.E.S. Library             */~
                            master$,     /* Parent Structure Name      */~
                            vers$,       /* Parent Structure Version   */~
                                         /*   if blank will use latest */~
                                         /*   version                  */~
                            key$,        /* Coming In: Component Key   */~
                                         /*   Name to search for       */~
                                         /* Going Out: Key value found/*/~
                                         /*   selected or unchanged if */~
                                         /*   none found               */~
                            descr$,      /* Component Dictionary Descr */~
                            f1%)         /* Return Status:             */~
                                         /* 1 = Record found/selected  */~
                                         /* 0 = Invalid, not-on-file,  */~
                                         /*     or PF 16 was pressed   */

            dim bfac$1,                  /* SCREEN FAC                 */~
                blankline$79,            /* WORK                       */~
                bline$79,                /* Top Line Of Screen         */~
                c%(2),                   /* CURSOR POSITION            */~
                descr$50,                /* Code Description           */~
                f1%(2),                  /* Record Status flags        */~
                fac$(16)1,               /* SCREEN FACS                */~
                flag$(16)1,              /* WHAT ONE DO YOU WANT?      */~
                i$(24)80,                /* SCREEN IMAGE               */~
                keys$16,                 /* Active Pf Keys             */~
                key$50,                  /* READ KEY                   */~
                library$6,               /* SES 'Library' Code         */~
                parent$16,               /* Parent Structure name      */~
                pfac$1,                  /* SCREEN FAC                 */~
                readkey$100,             /* KEY FOR PLOW               */~
                screen$(16)77,           /* VARIABLE FOR ACCEPT        */~
                text$50,                 /* DESCRIPTION WORK AREA.     */~
                title$79,                /* SCREEN TITLE               */~
                version$6                /* Parent Structure Version # */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.16.02 05/22/86 Sales Quote, Ven. Price & Misc  "
        REM *************************************************************
            f1% = 0%
            library$ = lib$
            parent$ = master$
            version$ = vers$
            if version$ > " " then L09000
            REM *** Find Latest Version ***
               readkey$ = str(library$) & parent$
L08940:        call "PLOWALTS" (#2, readkey$, 3%, 22%, f1%(2))
               if f1%(2) = 0% then L08980
               version$ = str(key(#2,3),23%,6%)
               goto L08940
L08980:        if version$ = " " then end          /* No Structure     */
               vers$ = version$
               goto L09400
L09000:     REM *** does passed in version exist ***
               readkey$ = str(library$) & str(parent$) & version$
               call "REDALT0" (#2, readkey$, 3%, f1%(2))
               if f1%(2) = 0% then end
L09400:     readkey$ = str(parent$) & str(version$) & str(library$) &    ~
                       hex(000001)
            if key$ = " " then L09900

*       *** IF IT'S ON FILE, THEN WIP RIGHT BACK WITH MINIMUM EFFORT ****
L09600:     call "PLOWNEXT" (#2, readkey$, 28%, f1%(2))
            if f1%(2) = 0% then L09900
            if str(key(#2,1),,16%) = key$ then successful_exit
            goto L09600

L09900: REM *************************************************************~
            *              M A I N   P L O W   L O O P                  *~
            *-----------------------------------------------------------*~
            * CODE PASSED IS NOT ON FILE, FIND & SHOW'EM WHAT IS....    *~
            *************************************************************~

*        INITIALIZE VARIABLES
            anyhits%, c%, seached_once% = 0
            screen$(), text$ = " "
            blankline$ = "Active P.F. Keys Are :"
        REM SLEN% = LEN(SEARCH$)
            bline$ = " PF Key 16 may be used to end display and return wi~
        ~th no selection."
            bline$ = " Shown Below Are The Valid Selections Defined In Th~
        ~e Dictionary For This Entry"

            str(readkey$,29%) = hex(000001)

        plow_codes
            call "PLOWNEXT" (#2, readkey$, 28%, f1%(2))
                 if f1%(2) = 0% then show_codes
            call "READ100" (#1, str(key(#2,1),,16%), f1%(1))
                 if f1%(1) > 0% then text$ = key(#1,3) else text$ = " "
            c% = c% + 1
            anyhits% = 1
            screen$(c%) = str(key(#2,1),,22%)  &  "   " & text$
            if c% > 15 then show_codes
            goto plow_codes

        show_codes
            x% = 0
            flag$() = all(hex(0b))
            init(hex(9c)) pfac$, fac$()
            bfac$ = hex(ac)
            keys$ = hex(0002050f2010)
            blankline$ = "Active P.F. Keys Are :"
            if c% + anyhits% > 0 then L41400 /* Anything To Show? */
            REM Don't want end of file msg to come up if we just came in
            if seached_once% = 1 then all_done
            seached_once% = 1
            if pos(key$ > hex(20)) <> 0 then L49800
            rem Nothing Meets Criteria : goto all_done

L41400:     if c% = 0 then str(screen$(8),27) = "****  END OF FILE ****" ~
                            else str(fac$(),,c%) = all(hex(86))

            REM Set Up Title Line...
            title$ = "  " & "Code Name"
            str(title$,26%) = " Description"

            REM Tack On The Screen Name...
            str(title$,63) = "DDSEARCH:" & str(cms2v$,,8%)

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
           at(24,48), "Cursor & RETURN: Return With Code",               ~
                                                                         ~
            keys(keys$),                                                 ~
            key(keyhit%)
            if keyhit% = 16 then all_done   /* PROCESSING ABORTED */
            if keyhit% = 32 then all_done   /* PROCESSING ABORTED */

            if keyhit% <> 15 then L49800
               call "PRNTSCRN"
               goto  L42600

L49800:     if keyhit% = 2 or c% = 0 then                                ~
                              str(readkey$,29%) = hex(000001)
            if c% = 0 then screen$() = " "
            if c% = 0 then plow_codes

            if keyhit% = 0 then L50800
               screen$() = " " : c% = 0
         REM   IF KEYHIT% = 5 THEN READ_NEXT
               goto plow_codes

L50800:     close ws
            call "SCREEN" addr("C", x%, "I", i$(), c%())
            x% = c%(1) - 4
            if x% > c% or x% < 1 then L42600

            REM Pass Selection Back
            key$ = str(screen$(x%),,22%)

        successful_exit
            call "READ100" (#1, str(key$,,16%), f1%(1))
            if f1%(1) > 0% then descr$ = key(#1,3)
            f1% = 1%

        all_done
            close ws
            end
