        REM *************************************************************~
            *                                                           *~
            *   N   N  U   U  M   M  TTTTT  EEEEE   SSS  TTTTT          *~
            *   NN  N  U   U  MM MM    T    E      S       T            *~
            *   N N N  U   U  M M M    T    EEEE    SSS    T            *~
            *   N  NN  U   U  M   M    T    E          S   T            *~
            *   N   N   UUU   M   M    T    EEEEE   SSS    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * NUMTEST  - SPECIAL NUMBER TESTER FOR DATA ENTRY. THE      *~
            *            FORMAT SPECIFIER VARIABLE (FL NMBR) WORKS AS   *~
            *            FOLLOWS... INTEGER PORTION REPRESENTS THE      *~
            *            NUMBER OF PLACES TO GUARANTEE, THE DECIMAL     *~
            *            PORTION IS THE MAXIMUM PLACES ALLOWABLE. THE   *~
            *            NUMBER IS ALWAYS ROUNDED IF IT HAS MORE PLACES *~
            *            THEN ALLOWED. THE RANGE IS SELF EXPLANITORY.   *~
            *            CAN'T DEAL WITH MORE THEN 15 SIGNIFICANT CHARS *~
            *            AND/OR 10 TO THE RIGHT OF DECIMAL.             *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 03/21/84 ! ORIGINAL (SWIPED FROM NUMSMASH)          ! HES *~
            * 04/30/85 ! Add right justify option                 ! HES *~
            * 06/03/85 ! Make Justify default 'L' (like orig was) ! HES *~
            * 09/11/85 ! Fixed bug in rounding when R justifing,  ! HES *~
            *          ! Eliminated call to (huge) NUMFMT         !     *~
            * 01/21/86 ! Remove space smash, default range = all  ! HES *~
            *************************************************************

            sub "NUMTEST" (number$, low, high, error$, format, number)
            dim error$79, tmp$26

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.19.00 03/13/87 Serial number, Lot tracking     "
        REM *************************************************************
            error$ = " "
            if number$ = " " then number$ = "0"
            len% = len(str(number$))

            REM Now the best test for validity, the good old...
            convert number$ to number, data goto L06800

            REM Set up our format control variables...
            maxplaces = round((abs(format) - int(abs(format)))*10,0)
            number = round(number, maxplaces)
            low    = round(low   , maxplaces)
            high   = round(high  , maxplaces)
            if format = 0 then format = .0001
            if low = 0 and high = 0 then L06500

            if number >= low then L05900
                error$ = "Number Can't Be Less Than"
                call "CONVERT" (low, -abs(format), str(error$,27,len%))
                goto L06200

L05900:     if number <= high then L06500
                error$ = "Number Can't Be Greater Than"
                call "CONVERT" (high, -abs(format), str(error$,30,len%))
L06200:         error$ = error$ & ": " & number$
                end

L06500:     REM Format Number As Requested...
              call "CONVERT" (number, -format, str(tmp$,,len%))
                if pos(tmp$ = "#") = 0 then L07100
L06800:          error$ = "Invalid Numeric Entry: " & number$
                 end

L07100:       REM All Clear If Here...
              number$ = tmp$
              rem THE:end
