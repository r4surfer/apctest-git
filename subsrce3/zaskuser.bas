        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *    A     SSS   K  K   U   U   SSS   EEEEE  RRRR           *~
            *   A A   S      K K    U   U  S      E      R   R          *~
            *  AAAAA   SSS   KK     U   U   SSS   EEEE   RRRR           *~
            *  A   A      S  K K    U   U      S  E      R   R          *~
            *  A   A   SSS   K  K    UUU    SSS   EEEEE  R   R          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ASKUSER  - General routine for requesting Yes/No reply    *~
            *            from User.  Overlays message window onto       *~
            *            current screen & waits for user response (PF   *~
            *            Key(s) designated in text from calling program.*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/05/85 ! Original                                 ! LDJ *~
            * 09/29/86 ! Change for O.S. 7.10 incompatibility.    ! LDJ *~
            *          !   Call to SCREEN no longer supports      !     *~
            *          !   passing the Workstation UFB directly.  !     *~
            * 04/19/89 ! O.S. 7.20/30 Comapatability (WSXIO).     ! KAB *~
            * 03/19/92 ! No more "CT"s or TRAN (REPLACING).       ! KAB *~
            *          ! Now accepts 42 char header (68 top or    !     *~
            *          !   bottom) without blinking entire line.  !     *~
            * 11/22/95 ! Added GUI effects (Box Drawn)            ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "ZASKUSER" (keyhit%, hdr$, pf1$, mid$, pf2$)

        dim                                                              ~
            hdr$40,                      /* Message header             */~
            mid$80,                      /* Middle Line (usually -OR-) */~
            mm$80, mm40$40,              /* Work Variable              */~
            pf2$80,                      /* PF 2   Key Prompt          */~
            pf1$80                       /* PF 1   Key Prompt          */~


        dim                              /* WSXIO/Screen IO Variables  */~
            order$(1)4,                  /* Screen Order Area          */~
            s$(24)80,                    /* Screen Text Array          */~
            iosw_receiver$8              /* IO Word Status Bytes       */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "
        REM *************************************************************

            select #5,  "CRTFILE",                                       ~
                        consec,                                          ~
                        recsize = 1924


        REM *** Close the Workstation & Reopen it Under WSXIO ***
            close ws
            call "CHECKGUI" addr(gui%)
            call "WSXIO" addr("O", 255%, "Y", #5, x%)
*          IF X% > 0% THEN END

         REM *** Set Up Header ***
            if hdr$ = " " then hdr$ = "*** ASK USER ***"

         REM *** First Read the Prior Screen & Dim it/Turn Off Tabs ***
            order$(1%) = hex(01000000)
            call "WSXIO" addr("X", #5, hex(40), order$(), s$(), 1920%,   ~
                     iosw_receiver$)
            x% = 0%
L09130:     y% = x% + pos(str(s$(), x% + 1%) > hex(7f))
            on y% - x% + 1% goto L09210
                str(s$(), y%, 1%) = and hex(fd)
                str(s$(), y%, 1%) = or  hex(0c)
                x% = y%
                if x% < 1920% then L09130


L09210:     if keyhit% < 0% or keyhit% > 2% then keyhit% = 0%

        REM *** Now Set Up the Window to Display ***
            if keyhit% <> 0% then L11000

        REM *** Now Set Up the Window to Display, Middle ***
            str(s$(08%), 17%,  1%)=hex(84)
            str(s$(08%), 18%, 46%)=all(hex(0b))
            str(s$(08%), 64%,  1%)=hex(8c)
            for x% = 09% to 16%
                str(s$(x%), 17%, 46%) = hex(840b)
                str(s$(x%), 63%,  2%) =  hex(0b8c)
            next x%
            str(s$(17%), 17%,  1%)=hex(84)
            str(s$(17%), 18%, 46%)=all(hex(0b))
            str(s$(17%), 64%,  1%)=hex(8c)
            l% = min(2% + len(hdr$), 44%)
            if gui% = 0% then y% = 8% else y% = 9%
            str(s$(y%),41% - (l%/2%), l%) =                              ~
                                 hex(94) & str(hdr$,,l%-2%) & hex(84)
            mm$ = pf1$ : y% = 10%
            gosub L10250
            mm$ = mid$ : y% = 13%
            gosub L10250
            mm$ = pf2$ : y% = 15%
            gosub L10250
            r1% = 8% : c1% = 18% : r2% = 17% : c2% = 63%
            goto L20000

L10250:     mm40$ = str(mm$,,40)
            if mm40$ <> " " then L10290
               m% = 40%
               goto L10420
L10290:     x% = pos(mm40$ <> " ")
            m% = pos(-str(mm40$,,40%) = " ")
            if m% <> 0% then L10350
               m% = 40%
               l% = 40%
               goto L10390
L10350:     if m% >= x% then L10380
               m% = x% - 1%
               goto L10420
L10380:     l% = pos(-str(mm40$,,m%) <> " ")
L10390:     l% = l% - x% + 1%
            str(s$(y%), 41% - ((l%)/2%), l%) = str(mm$, x%, l%)

L10420:     m% = m% + 1% : y% = y% + 1%
            if str(mm$, m%) = " " then return
            x% = pos(str(mm$, m%) <> " ")
            x% = x% + m% - 1%
            l% = min(len(mm$), 80%)
            l% = min(l% - x% + 1%, 40%)
            str(s$(y%), 41% - ((l%)/2%), l%) = str(mm$, x%, l%)
            return

L11000: REM *** Now Set Up the Window to Display, Top or Bottom ***
            if keyhit% = 1% then y% = 2% else y% = 20%
            r1% = y%-1% : c1% = 2% : r2% = y% + 4% : c2% = 80%
            str(s$(r1%), 1%,  1%) = hex(84)
            str(s$(r1%), 2%, 79%) = all(hex(0b))
            for x% = y% to  y% + 3%
                str(s$(x%),  1%, 79%) = hex(840b)
                str(s$(x%), 80%,  1%) = hex(0b)
            next x%
            str(s$(r2%), 1%,  1%) = hex(84)
            str(s$(r2%), 2%, 79%) = all(hex(0b))

            l% = min(2% + len(hdr$), 70%)
            str(s$(y%),41% - (l%/2%), l%) =                              ~
                                 hex(94) & str(hdr$,,l%-2%) & hex(84)

            y% = y% + 1% : mm$ = pf1$
            gosub L11500
            y% = y% + 1% : mm$ = mid$
            gosub L11500
            y% = y% + 1% : mm$ = pf2$
            gosub L11500
            goto L20000

L11500:     x% = pos(mm$ <> " ")
               if x% <=  0% then return
               if x% >= 78% then return
            l% = min(len(mm$),77%)
            l% = l% - x% + 1%
            str(s$(y%),41% - ((l%-1%)/2%), l%) = str(mm$, x%, l%)
            return

L20000: REM *** Now Write the Screen ***
            order$(1%) = hex(01a00101)
            call "WSXIO" addr("X", #5, hex(80), order$(), s$(), 1920%,   ~
                              iosw_receiver$)
            if gui% = 1% then call "DRAWBOX" (r1%,c1%,r2%,c2%)
            REM *** OK Lets Read It ***
            order$(1%) = hex(01000000)
            call "WSXIO" addr("X", #5, hex(40), order$(), s$(), 0%,      ~
                              iosw_receiver$)

            keyhit% = val(str(iosw_receiver$,3%,1%)) - 64%
            if keyhit% > 32% then keyhit% = keyhit% - 16%

            if keyhit% <> 15% then L20190
              call "WSXIO" addr("C", #5)
              call "SCREEN" addr("C", x%, "P", "                      ")
              call "WSXIO" addr("O", 255%, "Y", #5, x%)
              goto L20000

L20190:     call "WSXIO" addr("C", #5)

            end
