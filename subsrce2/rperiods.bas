        REM *************************************************************~
            *                                                           *~
            *   RRRR   PPPP   EEEEE  RRRR   IIIII   OOO   DDDD    SSS   *~
            *   R   R  P   P  E      R   R    I    O   O  D   D  S      *~
            *   RRRR   PPPP   EEEE   RRRR     I    O   O  D   D   SSS   *~
            *   R   R  P      E      R   R    I    O   O  D   D      S  *~
            *   R   R  P      EEEEE  R   R  IIIII   OOO   DDDD    SSS   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RPERIODS - SETS AGING DATES FOR RECEIVABLES REPORTS.      *~
            *            RETURNS RESULTS IN YYMMDD FORM FOR COMPUTATIONS*~
            *            AND CH(10) FORMATTED FOR REPORT TITLES.        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+------------------------------------------+-WHO-*~
            * 04/23/80 ! ORIGINAL                                 ! BCW *~
            *************************************************************

        sub "RPERIODS" (recdate$, dates$(), descr$())

        dim days%(5),                    /* NUMBER OF DAYS IN EA PERIOD*/~
            dates$(5)6,                  /* YYMMDD AGED DATES FOR COMP.*/~
            descr$(5)10                  /* FORMATTED DATES FOR TITLES */

            restore 1
            mat read days%
                data 0, 30, 60, 90, 120
            for day% = 1 to 4
                call "DATE" addr("G+", recdate$, -days%(day%+1),         ~
                                           dates$(day%), u%)
                put descr$(day%),using L10130,days%(day%)+1,days%(day%+1)
L10130:             %### TO ###
                next day%

            u%=u%  /* NOW COMPILE......*/
            dates$(5) = all("0")
            put descr$(5), using L10180, days%(5)
L10180:             %OVER  ####
            end
