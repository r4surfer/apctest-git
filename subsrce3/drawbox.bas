        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  DDDD   RRRR    AAA   W   W  BBBB    OOO   X   X          *~
            *  D   D  R   R  A   A  W   W  B   B  O   O   X X           *~
            *  D   D  RRRR   AAAAA  W W W  BBBB   O   O    X            *~
            *  D   D  R   R  A   A  WW WW  B   B  O   O   X X           *~
            *  DDDD   R   R  A   A  W   W  BBBB    OOO   X   X          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DRAWBOX    General routine for drawing a box (using VT    *~
            *            terminal cursor positioning and linedraw       *~
            *            characters).  Only works when in GUI           *~
            *            mode (see CHECKGUI).   Uses SENDCMD routine.   *~
            *            Calling arguments are;                         *~
            *                                                           *~
            *    CALL "DRAWBOX" (BEG_ROW%, BEG_COL%, END_ROW%, END_COL%)*~
            *                                                           *~
            *            Given the top-left and bottom-right coordinates*~
            *            DRAWBOX will perform the line drawing for you. *~
            *            If BEG_ROW% is Negative then assumes you wnt to*~
            *            'UnDraw' a box around the specified Coordinates*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/16/95 ! Original                                 ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


            sub "DRAWBOX" (beg_row%, beg_col%, end_row%, end_col%)

        dim                                                              ~
            command$100,             /* Message Box Title Bar Contents */~
            c$1,                     /* Character Set;0=Graphic,B=ASCII*/~
            h$1,                     /* Horizontal Line Character      */~
            v$1,                     /* Vertical Line Character        */~
            ul$1,ur$1,bl$1,br$1      /* Corner Characters              */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "
        REM *************************************************************

            call "CHECKGUI" addr(gui%)
            if gui% = 0% then end_routine
            undraw% = sgn(beg_row%) * (-1%)
            h$ = "q" : v$ = "x" : ul$ = "l" : ur$ = "k" : bl$="m": br$="j"
            if undraw% = 1% then h$,v$,ul$,ur$,bl$,br$ = " "
            if undraw% = 1% then c$ = "B" else c$ = "0" /* CharSet */
            REM *** Tell Workstation to Save Cursor Position ***
            call "SENDCMD" (hex(1b37))
            REM *** now draw a pretty box using VT terminal ESC codes ***
            REM *** Position Cursor First & do the top ***
            command$ = hex(1b) & "[01;01H" & hex(1b) & "(" & c$ & ul$
            convert beg_row% to str(command$,3%,2%),pic(00)
            convert beg_col% to str(command$,6%,2%),pic(00)
            str(command$,13%,end_col% - beg_col% - 1%) = all(h$)
            str(command$,len(command$)+1%,1%) = ur$
            call "SENDCMD" (str(command$,,len(command$)))
            REM *** Paint the sides of the Box ***
            for x% = beg_row% + 1% to end_row% - 1%
                command$ = hex(1b) & "[08;01H" & v$ & hex(1b) & "[08;80H"~
                         & v$
                convert x% to str(command$,3%,2%),pic(00)
                str(command$,12%,2%) = str(command$,3%,2%)
                convert beg_col% to str(command$,6%,2%),pic(00)
                convert end_col% to str(command$,15%,2%),pic(00)
                call "SENDCMD" (str(command$,,len(command$)))
            next x%
            REM *** Paint the botton ***
            command$ = hex(1b) & "[17;18H" & bl$
            convert end_row% to str(command$,3%,2%),pic(00)
            convert beg_col% to str(command$,6%,2%),pic(00)
            str(command$,10%,end_col% - beg_col% - 1%) = all(h$)
            str(command$,len(command$)+1%) = br$ & hex(1b) & "(B"
            call "SENDCMD" (str(command$,,len(command$)))

            REM *** Tell Workstation to Restore Cursor Position ***
            call "SENDCMD" (hex(1b38))

        end_routine
            end
