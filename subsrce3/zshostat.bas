        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   H   H   OOO    SSS   TTTTT    A    TTTTT          *~
            *  S      H   H  O   O  S        T     A A     T            *~
            *   SSS   HHHHH  O   O   SSS     T    AAAAA    T            *~
            *      S  H   H  O   O      S    T    A   A    T            *~
            *   SSS   H   H   OOO    SSS     T    A   A    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHOSTAT  - General routine for display of a message up to *~
            *            80 characters in length.  Used throughout the  *~
            *            CMS System.  Calling syntax is;                *~
            *            CALL "SHOSTAT" ("abc....") or ( String Var )   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/13/82 ! ORIGINAL                                 ! GLW *~
            * 08/15/85 ! Modified to Give a "Windowing" Effect    ! LDJ *~
            *          !   using WSXIO,  Message length lengthened!     *~
            *          !   from 79 characters to 80 characters.   !     *~
            * 10/22/85 ! Simplified, no FAC limit                 ! KAB *~
            * 06/29/87 ! Complicated again to properly handle     ! LDJ *~
            *          !   hidden FACs.  Shows program name.      !     *~
            *          !   Also now shows date & time also.       !     *~
            * 04/19/89 ! O.S. 7.20/30 Comapatability (WSXIO).     ! KAB *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 03/17/95 ! Provide for path rather than program     ! KAB *~
            *          !   library on unix - xparent on Wang      !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

           sub "ZSHOSTAT" (msg$)

           dim msg$80, mm$80, fgbg$1, program$8, pgmpath$80

        dim                              /* WSXIO/Screen IO Variables  */~
            order$(1)4,                  /* Screen Order Area          */~
            s$(24)80,                    /* Screen Text Array          */~
            iosw_receiver$8,             /* IO Word Status Bytes       */~
            date$8,                      /* Current Date               */~
            inlib$8,                     /* Database                   */~
            proglib$8,                   /* Program Library            */~
            time$8,                      /* Current Time               */~
            testfac$1                    /* Test for Hidden FAC's      */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch finalization of R6.04.00  "
        REM *************************************************************

            select #5,  "CRTFILE",                                       ~
                        consec,                                          ~
                        recsize = 1924

            if fgbg$ <> " " then  L12300
            call "EXTRACT" addr("TT", fgbg$,                             ~
                   "CF", program$, "CL", proglib$, "IL", inlib$)
            call "GETPGPTH" addr(pgmpath$)
            if pgmpath$ = " " then L12300
            t% = pos(pgmpath$ < hex(20))
            if t% = 0% then L12260
               str(pgmpath$,t%) = " "
L12260:     t% = pos(-str(pgmpath$) = "/")
            if t% > 0% then str(pgmpath$,t%) = " "                       ~
                       else pgmpath$ = "No Directory Specified"
            if pgmpath$ = "." then pgmpath$ = "Current Working Directory"
L12300:     if fgbg$ = "B" then end   /* Background Task */
            time$ = time : str(time$,7%)=" ":call "TIMEOK" (time$, 0, " ")
            date$ = date : call "DATEFMT" (date$)

            REM *** Close the Workstation & Reopen it Under WSXIO ***
            close ws
            call "WSXIO" addr("O", 255%, "Y", #5, x%)
*              IF X% > 0% THEN END

            REM *** First Read the Prior Screen & Dim it/Turn Off Tabs ***
            order$(1) = hex(01000000)
            call "WSXIO" addr("X", #5, hex(40), order$(1), s$(), 1920%,  ~
                     iosw_receiver$)

            x% = 0%
L14420:     y% = x% + pos(str(s$(), x% + 1%) > hex(7f))
            on y% - x% + 1% goto L14600
                testfac$ = str(s$(),y%,1%) and hex(98)
                if testfac$ = hex(98) then L14460
                str(s$(),y%,1%) = and hex(fd)   /* '02' OFF */
                str(s$(),y%,1%) = or  hex(0c)   /* '0C' ON  */
L14460:         x% = y%
                if x% < 1920% then L14420

L14600:     REM *** Now Set Up the Window to Display ***
            x% = 8% : gosub find_fac_value
            str(s$(08%),17%,1%)  = hex(84)
            str(s$(08%),18%,46%) = all(hex(0b))
            str(s$(08%),64%,1%)  = testfac$
            for x% = 09% to 16%
                gosub find_fac_value
                str(s$(x%),17%,46%) = hex(840b)
                str(s$(x%),63%,2%)  = hex(0b) & testfac$
            next x%
            x% = 17% : gosub find_fac_value
            str(s$(17%),17%,1%)  = hex(84)
            str(s$(17%),18%,46%) = all(hex(0b))
            str(s$(17%),64%,1%)  = testfac$

            str(s$(09%),19%,44%)=hex(ac)& program$ & " Status as of " &  ~
                                   time$ & " on " & date$ & hex(84)
            str(s$(15%),19%,01%) = hex(ac)
            str(s$(15%),62%,01%) = hex(84)

            if pgmpath$ = " " then L15151
            t% = max(1%, len(pgmpath$) - 27%)
            str(s$(16%),19%,43%)=hex(8c) & str(pgmpath$,t%,28%)        & ~
                                 " Db = " & inlib$ & hex(84)
            goto L15160

L15151:     str(s$(16%),19%,43%)=hex(8c) & "Running from " & proglib$ &  ~
                 ", Database = " & inlib$ & hex(84)

L15160:     mm$ = msg$
            x% = pos(-str(mm$,,40%) = " ")
            if x% = 0% then x% = 40%
            str(s$(12%),21%,40%) = str(mm$,,x%)
            call "STRING" addr("CT", str(s$(12%),21%,40%), 40%)
            str(s$(13%),21%,40%) = str(mm$,x%+1%)
            call "STRING" addr("CT", str(s$(13%),21%,40%), 40%)

            REM *** Now Write the Screen, Close the WS, and Exit ***
            order$(1) = hex(01000000)
            call "WSXIO" addr("X", #5, hex(80), order$(1), s$(), 1920%,  ~
                              iosw_receiver$)

            call "WSXIO" addr("C", #5)

            end

        find_fac_value
            REM *** X% = Row to Search On ***
            y% = pos(-str(s$(x%),,64%) > hex(7f))
            if y% = 0% then testfac$ = hex(8c)                           ~
                       else testfac$ = str(s$(x%),y%,1%)
            return
