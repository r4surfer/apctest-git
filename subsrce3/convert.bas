        REM *************************************************************~
            *                                                           *~
            *    CCC    OOO   N   N  V   V  EEEEE  RRRR   TTTTT         *~
            *   C   C  O   O  NN  N  V   V  E      R   R    T           *~
            *   C      O   O  N N N  V   V  EEEE   RRRR     T           *~
            *   C   C  O   O  N  NN   V V   E      R  R     T           *~
            *    CCC    OOO   N   N    V    EEEEE  R   R    T           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CONVERT  - Converts floating point variable to a string.  *~
            *            Format Specification Variable works as         *~
            *            follows... integer portion represents the      *~
            *            number of places to guarantee, the decimal     *~
            *            portion is the maximum places allowable. The   *~
            *            number is always rounded if it has more places *~
            *            than allowed.  Dimensioned length of output    *~
            *            variable (as determined by calling program)    *~
            *            governs the size of the number that can be     *~
            *            converted (i.e. not limited to 10).            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 10/30/84 ! ORIGINAL (SWIPED FROM NUMTEST)           ! HES *~
            * 09/12/85 ! Don't call 'NUMFMT' anymore              ! HES *~
            * 11/03/86 ! Prettied up header documentation,        ! LDJ *~
            *          !   Changed to no longer return '#' if too !     *~
            *          !   many decimals to fit in NUMBER$.  Now  !     *~
            *          !   rounds to most significant digits that !     *~
            *          !   will fit if necessary to.              !     *~
            *************************************************************

            sub "CONVERT" (number, format, number$)
            dim tmp$26

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.16.07 01/05/87 OS 7.10 Compatibility           "
        REM *************************************************************

            REM Set up our format control variables...
            maxplaces = round((abs(format) - int(abs(format)))*10,0)
            minplaces = int(abs(format))
            if minplaces > 0 then minplaces = minplaces + 1
            len% = len(str(number$))
            number = round(number, maxplaces)
            n = number

L03370:     REM Format Number As Requested...
              convert n to tmp$, pic(-#############0.##########)
              call "STRING" addr ("LJ", tmp$, 26%)
              str(tmp$, max(pos(tmp$ = ".") + minplaces,                 ~
                                             pos(-tmp$ > "0") + 1)) = " "
              convert str(tmp$,,len%) to tmp, data goto L03460
              if abs(n - tmp) < .1^(maxplaces+1) then L03490
                 maxplaces = maxplaces - 1
                 if maxplaces < max(0, minplaces - 1) then L03460
                    n = round(n, maxplaces)
                    goto L03370
L03460:          number$ = all ("#")
                 end

L03490:       REM All Clear If Here...
              number$ = tmp$
              if format >=0 then call "STRING" addr ("RJ", number$, len%)
              rem THE:end
