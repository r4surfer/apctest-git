        REM *************************************************************~
            *                                                           *~
            *  TTTTT  IIIII  M   M  EEEEE   OOO   K   K                 *~
            *    T      I    MM MM  E      O   O  K  K                  *~
            *    T      I    M M M  EEEE   O   O  KKK                   *~
            *    T      I    M   M  E      O   O  K  K                  *~
            *    T    IIIII  M   M  EEEEE   OOO   K   K                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TIMEOK - Tests a time to make sure it is in the correct   *~
            *          format.  Returns time normalized in input string *~
            *          plus error message text usable by calling        *~
            *          program, if any errors. Note that the time       *~
            *          may be five (13;02) characters or 8 (23;50;23)   *~
            *          characters, the DIMensioned length of the passed *~
            *          string will determine what a valid input time is.*~
            *          Also Note that the colon(s) are optional, however*~
            *          If they aren't entered, leading zeroes are       *~
            *          required. (1201 is valid, 121 is not)...This may *~
            *          by a useless feature, but who knows?             *~
            *          Time is also returned in floating point as a     *~
            *          decimal value, the format is HH.XXXX, base 10.   *~
            *          (Where XXXX are 10,000's of an hour).            *~
            *          When I say ";" you know what I mean, I just can't*~
            *          say it here!!!!                                  *~
            *        - Send in "fmt" in PASS$, and the value in U will  *~
            *          be converted to clock time and returnd in PASS$. *~
            *        - Enter time as HHMM followed by literal 'P', and  *~
            *          time will be convert to military of a PM time.    ~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+-------------------WHAT-------------------+-WHO-*~
            * 09/14/84 ! Original (DATEOK Semi-Clone)             ! HES *~
            * 11/04/85 ! Corrected above documentation so I could ! LDJ *~
            *          !   understand it.                         !     *~
            * 02/24/86 ! That must have been some task, LDJ!      ! HES *~
            * 02/24/86 ! Added Ability To Convert Dec Time To Clk ! HES *~
            * 10/11/90 ! Out, Out Damn Stop (Sorry, MacBeth)      ! KAB *~
            *************************************************************

            sub "TIMEOK" (pass$, u, error$)
            dim u2$8, error$79, u(3)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            error$ =" "
            u2$ = pass$
            mat u = zer
            if pass$ = "fmt" then L16300
            u, pm% = 0
            if str(u2$,5) = "P" then pm% = 1
            if str(u2$,5) = "P" then str(u2$,5), str(pass$,5) = " "

        REM TEST FOR NUMBER OF CHARACTERS ON INPUT
            l% = min(8,len(str(pass$)))
*          IF L%<>5  AND L%<>8 THEN STOP "INVALID STRING LENGTH ON CALL"
            if l% = 5% then L11100
            if l% = 8% then L11100
                  error$ = "Invalid String Length on CALL"
                  call "GETPARM" addr("I ", "A",                         ~
                                      "TIMEOK  ", " ", "0001", "TIMEOK", ~
                     2%,                                                 ~
                     "Invalid String Length on CALL              ",  44%,~
                     "Press ENTER to acknowledge and EXIT Program",  44%)
                  end
L11100:     if len(u2$) >= 4 then L11200
                  error$ = "Time Must Be At Least 4 Characters: " & pass$
                  end
L11200:     if l% = 8 and len(u2$) > 5 then L11600
            if pos(str(u2$,pos(u2$=":")+1)=":")>0 then L11600 /* WHAT? */
            if pos(str(u2$,,l%) = ":") > 0 then u2$ = u2$ & ":00" else   ~
                                                         u2$ = u2$ & "00"
L11600:     if len(u2$) = l% then L12900
            if len(u2$) >= (l% - abs(l%-6)) - 1 then L12200
                  error$ = "Time Must Be At Least * Characters: " & pass$
                  convert (l%-abs(l%-6)) - 1 to str(error$,23,1), pic(#)
                  end

L12200: REM IF 6 CHARACTERS, ASSUME FREE FORMAT INPUT AND FORMAT
            if pos(str(u2$,,len(u2$)) < "0") > 0 then L18100
            if pos(str(u2$,,l%) > ":") > 0 then L18100
            if pos(str(u2$,,l%) = ":") > 0 then L12900
            temp$ = str(u2$,,2) & ":" & str(u2$,3,2) &":"& str(u2$,5)
            u2$ = str(temp$,,l%)

L12900: REM NOW TIME IS PROBABLY IN HH;MM(;SS) FORMAT...TEST & NORMALIZE
             u% = pos(u2$=":")
             if u% < 2 then L18400
             if u% < 4 then L13600
L13300:         error$="There Is A ':' Missing In The Time: " & pass$
                end

L13600:      convert str(u2$,,u%-1) to u(1), data goto L18400
             str(u2$,,u%) = " "
             u% = pos(u2$=":")
             if u% > 1 then L14200
             if l% <> 5 then L13300

L14200:     if u% = 0 then u% = l% + 1
            convert str(u2$,,u%-1) to u(2), data goto L18400
            str(u2$,,u%) = " "
            if pm% = 0 then L14900
                u(1) = u(1) + 12
                if u(1) = 24 then u(1) = 0

L14900:     if u(1) >= 0 and u(1) < 24 then L15300
                error$ = "Hour Must Be Between 0 And 23: " & pass$
                end

L15300:     if u(2) < 60 then L15700
                error$ = "Minutes Must Be Between 0 And 59: " & pass$
                end

L15700:     if l% = 5 then L17090
            convert u2$ to u(3), data goto L18400
            if u(3) < 60 then L17090
                error$ = "Seconds Must Be Between 0 And 59: " & pass$
                end

L16300:     REM Return time formatted feature (PASS$ = "fmt")
                u(1) = int(u)
                u = u - u(1)
                u(2), u(3) = u * 60
                u(2) = round(u(2),0)
                if u(2) < 60 then L16800
                     u(2), u(3) = 0
                     u(1) = u(1) + 1
L16800:         u(3) = u(3) - u(2)
                if u(3) <= 0 then L17090
                     u(3) = round(u(3)*60, 0)
                     if u(3) < 60 then L17050
                          u(3) = 0
                          u(2) = u(2) + 1
                          goto L17090
L17050:             if len(str(pass$)) <> 5% or u(3) < 30 then L17090
                          u(3) = 0
                          u(2) = u(2) + 1

L17090:     u2$ = "  :  :  "
            convert u(1) to str(u2$,,2),  pic(00)
            convert u(2) to str(u2$,4,2), pic(00)
            convert u(3) to str(u2$,7,2), pic(00)
            pass$ = u2$
            u = u(1) + u(2)/60 + u(3)/3600
            end

L18100:     error$ = "Invalid characters in time value: " & pass$
                 end

L18400:     error$ = "Invalid format for time value: " & pass$
                 end

