        dim f2%(05),                     /* = 0 if the file is open    */~
            f1%(05)                      /* = 1 if READ was successful */

        dim readkey$20,                  /* EasyDate ReadKey           */~
            date$6,                      /* EASYDATE todaysdate        */~
            startweek$6,                 /* EASYDATE start of week     */~
            yesterday$6,                 /* EASYDATE yesterday         */~
            lastweek$6,                  /* EASYDATE last week         */~
            twodayago$6,                 /* EASYDATE two days ago      */~
            prev_yr_st$6,                /* EASYDATE Prev Year Start   */~
            prev_yr_end$6,               /* EASYDATE Prev Year End     */~
            curr_yr_st$6,                /* EASYDATE Curr Year Start   */~
            curr_yr_end$6,               /* EASYDATE Curr Year End     */~
            datefmt$10,                  /* Formated Date              */~
            dateconv$8,                  /* Conversion Date            */~
            year$4                       /* Year                       */

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "EASYDATE",                                      ~
                        varc, indexed,                                   ~
                        recsize =  128,                                  ~
                        keypos =     1, keylen =  20

            call "OPENCHCK" (#1, 0%, f2%(1), 10%, " ")

            init(" ") readkey$, year$
            date$, startweek$, yesterday$, lastweek$, twodayago$,      ~
            prev_yr_st$, prev_yr_end$, curr_yr_st$, curr_yr_end$,      ~
            datefmt$, dateconv$ = all(hex(00))

            readkey$ = "EASYDATE"
            date$ = date
            call "DAY" addr(date$, day%)
            day% = (day% -1) * -1
            call "DATE" addr("G+",date$,day%,startweek$,err%)
            call "DATE" addr("G+",date$,-1%,yesterday$,err%)
            call "DATE" addr("G+",date$,-7%,lastweek$,err%)
            call "DATE" addr("G+",date$,-2%,twodayago$,err%)

            datefmt$ = all(hex(00))
            datefmt$ = date
            call "DATFMTC" (datefmt$)
            year$ = str(datefmt$,7%,4%)

            datefmt$ = all(hex(00))
            datefmt$ = "01/01/" & year$
            call "DATUFMTC" (datefmt$)
            curr_yr_st$ = datefmt$

            datefmt$ = all(hex(00))
            datefmt$ = "12/31/" & year$
            call "DATUFMTC" (datefmt$)
            curr_yr_end$ = datefmt$

            year% = 0%
            convert year$ to year%, data goto badYear
              year% = year% - 1%

              convert year% to year$, pic(0000)

              datefmt$ = all(hex(00))
              str(datefmt$,1%,10%) = "01/01/" & year$
              call "DATUFMTC" (datefmt$)
              prev_yr_st$ = datefmt$

              datefmt$ = all(hex(00))
              str(datefmt$,1%,10%) = "12/31/" & year$
              call "DATUFMTC" (datefmt$)
              prev_yr_end$ = datefmt$

badYear:
            read #1, hold, key = readkey$, eod goto addRec

            put #1, using FMTDTE, date$, startweek$, yesterday$, lastweek$, ~
                          twodayago$, prev_yr_st$, prev_yr_end$, curr_yr_st$, ~
                          curr_yr_end$
FMTDTE:         FMT POS(21), 9*CH(06)

            rewrite #1, eod goto exitPgm
              goto exitPgm

addRec:

            put #1, using FMTADD, readkey$, date$, startweek$, yesterday$, ~
                          lastweek$, twodayago$, prev_yr_st$, prev_yr_end$, ~
                          curr_yr_st$, curr_yr_end$
FMTADD:         FMT CH(20), 9*CH(06)
            write #1, eod goto exitPgm

exitPgm:
            end




