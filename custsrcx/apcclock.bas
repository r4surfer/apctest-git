        REM *************************************************************~
            *  (APCCLOCK)- Display the Time. Change to Every 5 Seconds  *~
            *                                                           *~
            *      ( As Of 02/23/2000 - RHH Check for R6.04.03 )        *~
            *-----------------------------------------------------------*~
            *10/12/2010 ! (AWD001) - mod for AES TX time          ! CMG *~
            *************************************************************

            dim a$(12%)10, b$(12%)10, c$(12%)10, d$(12%)10, e$(12%)10,   ~
                f$(12%)10, g$(12%)10, h$(12%)10, i$(12%)10, j$(12%)10,   ~
                hh$(6%,12%)10, db$(12%)1, clock$10, ww%(3%,2%),          ~
                am$(10%)35, pm$(10%)35, ap$(10%)35
/* (AWD001)*/
            dim userid$3                 /* Current User Id            */

            gosub initialize
            call "EXTRACT" addr("ID", userid$)
        main_loop
            init(" ") hh$(), ap$()
            clock$ = time : ap% = 0%           /* Set Time from System */

            if userid$ <> "TXT" then goto notTXEmp
            cmg% = 0%                         /* Check for 00 Midnight */
            if str(clock$,1,2) = "00" then str(clock$,1,2) = "24" 
            convert clock$ to cmg%, data goto badTime
               cmg% = cmg% - 1000000   /* Substract 1 hour from time */

            convert cmg% to clock$, pic(00000000)

badTime:

notTXEmp:
            convert str(clock$,1%,4%) to clock%, data goto L00200
L00200:
            check% = clock%
            if check% <= 1200% then goto L00260
               check% = check% - 1200%
               ap% = 1%

L00260:     if check% < 100% then check% = check% + 1200%
            convert check% to str(clock$,1%,4%), pic(0000)
            for k% = 1% to 6%                  /* Build Six Digit Time */
                tt% = 0%                       /* for Display in Sec's */
                convert str(clock$,k%,1%) to tt%, data goto L00310
L00310:
                on (tt% + 1%) gosub load_0, load_1, load_2, load_3,      ~
                                  load_4, load_5, load_6, load_7, load_8,~
                                  load_9
            next k%

            gosub display_time                 /*Display Time on Screen*/
            gosub check_shift                  /*Check for Shift Change*/
         goto main_loop
        end

        check_shift
           er% = 0%
           sleep% = 5%                      /* Refresh every 5 Seconds */
           for j% = 1% to 3%
               if clock% >= ww%(j%,1%) and clock% <= ww%(j%,2%) then     ~
                                                     sleep% = 5%
           next j%
           call "EWDSLEEP" (sleep%, er%)
        return

        load_0                                 /* Load Zero Digit      */
           for i% = 1% to 12% : hh$(k%,i%) = a$(i%) : next i% : return
        load_1                                 /* Load One's Digit     */
           for i% = 1% to 12% : hh$(k%,i%) = b$(i%) : next i% : return
        load_2                                 /* Load Two's Digit     */
           for i% = 1% to 12% : hh$(k%,i%) = c$(i%) : next i% : return
        load_3                                 /* Load Three's Digit   */
           for i% = 1% to 12% : hh$(k%,i%) = d$(i%) : next i% : return
        load_4                                 /* Load Four's digit    */
           for i% = 1% to 12% : hh$(k%,i%) = e$(i%) : next i% : return
        load_5                                 /* Load Five's Digit    */
           for i% = 1% to 12% : hh$(k%,i%) = f$(i%) : next i% : return
        load_6                                 /* Load Six's Digit     */
           for i% = 1% to 12% : hh$(k%,i%) = g$(i%) : next i% : return
        load_7                                 /* Load Seven's Digit   */
           for i% = 1% to 12% : hh$(k%,i%) = h$(i%) : next i% : return
        load_8                                 /* Load Eight's Digit   */
           for i% = 1% to 12% : hh$(k%,i%) = i$(i%) : next i% : return
        load_9                                 /* Load Nine's Digit    */
           for i% = 1% to 12% : hh$(k%,i%) = j$(i%) : next i% : return

        display_time
          if ap% = 0% then copy am$() to ap$()                           ~
                      else copy pm$() to ap$()

          display                                                        ~
            at (02,01),hex(84),hh$(1%,1%),at (03,01),hex(84),hh$(1%,2%), ~
            at (02,14),hex(84),hh$(2%,1%),at (03,14),hex(84),hh$(2%,2%), ~
            at (02,29),hex(84),hh$(3%,1%),at (03,29),hex(84),hh$(3%,2%), ~
            at (02,42),hex(84),hh$(4%,1%),at (03,42),hex(84),hh$(4%,2%), ~
            at (02,56),hex(84),hh$(5%,1%),at (03,56),hex(84),hh$(5%,2%), ~
            at (02,69),hex(84),hh$(6%,1%),at (03,69),hex(84),hh$(6%,2%), ~
                                                                         ~
            at (04,01),hex(84),hh$(1%,3%),at (05,01),hex(84),hh$(1%,4%), ~
            at (04,14),hex(84),hh$(2%,3%),at (05,14),hex(84),hh$(2%,4%), ~
            at (04,29),hex(84),hh$(3%,3%),at (05,29),hex(84),hh$(3%,4%), ~
            at (04,42),hex(84),hh$(4%,3%),at (05,42),hex(84),hh$(4%,4%), ~
            at (04,56),hex(84),hh$(5%,3%),at (05,56),hex(84),hh$(5%,4%), ~
            at (04,69),hex(84),hh$(6%,3%),at (05,69),hex(84),hh$(6%,4%), ~
                                                                         ~
            at (06,01),hex(84),hh$(1%,5%),at (07,01),hex(84),hh$(1%,6%), ~
            at (06,14),hex(84),hh$(2%,5%),at (07,14),hex(84),hh$(2%,6%), ~
            at (06,29),hex(84),hh$(3%,5%),at (07,29),hex(84),hh$(3%,6%), ~
            at (06,42),hex(84),hh$(4%,5%),at (07,42),hex(84),hh$(4%,6%), ~
            at (06,56),hex(84),hh$(5%,5%),at (07,56),hex(84),hh$(5%,6%), ~
            at (06,69),hex(84),hh$(6%,5%),at (07,69),hex(84),hh$(6%,6%), ~
                                                                         ~
            at (08,01),hex(84),hh$(1%,7%),at (09,01),hex(84),hh$(1%,8%), ~
            at (08,14),hex(84),hh$(2%,7%),at (09,14),hex(84),hh$(2%,8%), ~
            at (08,29),hex(84),hh$(3%,7%),at (09,29),hex(84),hh$(3%,8%), ~
            at (08,42),hex(84),hh$(4%,7%),at (09,42),hex(84),hh$(4%,8%), ~
            at (08,56),hex(84),hh$(5%,7%),at (09,56),hex(84),hh$(5%,8%), ~
            at (08,69),hex(84),hh$(6%,7%),at (09,69),hex(84),hh$(6%,8%), ~
                                                                         ~
            at (10,01),hex(84),hh$(1%,9%),at (11,01),hex(84),hh$(1%,10%),~
            at (10,14),hex(84),hh$(2%,9%),at (11,14),hex(84),hh$(2%,10%),~
            at (10,29),hex(84),hh$(3%,9%),at (11,29),hex(84),hh$(3%,10%),~
            at (10,42),hex(84),hh$(4%,9%),at (11,42),hex(84),hh$(4%,10%),~
            at (10,56),hex(84),hh$(5%,9%),at (11,56),hex(84),hh$(5%,10%),~
            at (10,69),hex(84),hh$(6%,9%),at (11,69),hex(84),hh$(6%,10%),~
                                                                         ~
           at (12,01),hex(84),hh$(1%,11%),at (13,01),hex(84),hh$(1%,12%),~
           at (12,14),hex(84),hh$(2%,11%),at (13,14),hex(84),hh$(2%,12%),~
           at (12,29),hex(84),hh$(3%,11%),at (13,29),hex(84),hh$(3%,12%),~
           at (12,42),hex(84),hh$(4%,11%),at (13,42),hex(84),hh$(4%,12%),~
           at (12,56),hex(84),hh$(5%,11%),at (13,56),hex(84),hh$(5%,12%),~
           at (12,69),hex(84),hh$(6%,11%),at (13,69),hex(84),hh$(6%,12%),~
                                                                         ~
               at (02,27),db$(1%), at (02,54),db$(1%),                   ~
               at (03,27),db$(2%), at (03,54),db$(2%),                   ~
               at (04,27),db$(3%), at (04,54),db$(3%),                   ~
               at (05,27),db$(4%), at (05,54),db$(4%),                   ~
               at (06,27),db$(5%), at (06,54),db$(5%),                   ~
               at (07,27),db$(6%), at (07,54),db$(6%),                   ~
               at (08,27),db$(7%), at (08,54),db$(7%),                   ~
               at (09,27),db$(8%), at (09,54),db$(8%),                   ~
               at (10,27),db$(9%), at (10,54),db$(9%),                   ~
               at (11,27),db$(10), at (11,54),db$(10%),                  ~
               at (12,27),db$(11), at (12,54),db$(11%),                  ~
               at (13,27),db$(12), at (13,54),db$(12%),                  ~
                                                                         ~
           at (15,23),hex(84),ap$( 1%),                                  ~
           at (16,23),hex(84),ap$( 2%),                                  ~
           at (17,23),hex(84),ap$( 3%),                                  ~
           at (18,23),hex(84),ap$( 4%),                                  ~
           at (19,23),hex(84),ap$( 5%),                                  ~
           at (20,23),hex(84),ap$( 6%),                                  ~
           at (21,23),hex(84),ap$( 7%),                                  ~
           at (22,23),hex(84),ap$( 8%),                                  ~
           at (23,23),hex(84),ap$( 9%),                                  ~
           at (24,23),hex(84),ap$(10%)
        return

        initialize

         ww%(1%,1%) =  645% : ww%(1%,2%) =  705%   /* 1st Shift Window */
         ww%(2%,1%) = 1445% : ww%(2%,2%) = 1505%   /* 2nd Shift Window */
         ww%(3%,1%) = 2245% : ww%(3%,2%) = 2305%   /* 3rd Shift Window */

         a$(1%) = "0000000000" : b$(1%) = "    111   " : db$(1%) = " "
         a$(2%) = "0000000000" : b$(2%) = "   1111   " : db$(2%) = " "
         a$(3%) = "000    000" : b$(3%) = "  11111   " : db$(3%) = " "
         a$(4%) = "000    000" : b$(4%) = " 111111   " : db$(4%) = "@"
         a$(5%) = "000    000" : b$(5%) = "1111111   " : db$(5%) = "@"
         a$(6%) = "000    000" : b$(6%) = "   1111   " : db$(6%) = " "
         a$(7%) = "000    000" : b$(7%) = "   1111   " : db$(7%) = " "
         a$(8%) = "000    000" : b$(8%) = "   1111   " : db$(8%) = "@"
         a$(9%) = "000    000" : b$(9%) = "   1111   " : db$(9%) = "@"
         a$(10%)= "000    000" : b$(10%)= "   1111   " : db$(10%)= " "
         a$(11%)= "0000000000" : b$(11%)= "1111111111" : db$(11%)= " "
         a$(12%)= "0000000000" : b$(12%)= "1111111111" : db$(12%)= " "

         c$(1%) = " 22222222 " : d$(1%) = "3333333333"
         c$(2%) = "2222222222" : d$(2%) = "3333333333"
         c$(3%) = "222    222" : d$(3%) = "333    333"
         c$(4%) = "       222" : d$(4%) = "       333"
         c$(5%) = "       222" : d$(5%) = "       333"
         c$(6%) = " 222222222" : d$(6%) = "  33333333"
         c$(7%) = "222222222 " : d$(7%) = "  33333333"
         c$(8%) = "222       " : d$(8%) = "       333"
         c$(9%) = "222       " : d$(9%) = "       333"
         c$(10%)= "222       " : d$(10%)= "333    333"
         c$(11%)= "2222222222" : d$(11%)= "3333333333"
         c$(12%)= " 22222222 " : d$(12%)= "3333333333"

         e$(1%) = "444   444 " : f$(1%) = "5555555555"
         e$(2%) = "444   444 " : f$(2%) = "5555555555"
         e$(3%) = "444   444 " : f$(3%) = "555       "
         e$(4%) = "444   444 " : f$(4%) = "555       "
         e$(5%) = "444   444 " : f$(5%) = "555       "
         e$(6%) = "4444444444" : f$(6%) = "55555555  "
         e$(7%) = "4444444444" : f$(7%) = "555555555 "
         e$(8%) = "      444 " : f$(8%) = "      5555"
         e$(9%) = "      444 " : f$(9%) = "      5555"
         e$(10%)= "      444 " : f$(10%)= "      5555"
         e$(11%)= "      444 " : f$(11%)= "555555555 "
         e$(12%)= "      444 " : f$(12%)= "55555555  "

         g$(1%) = " 66666666 " : h$(1%) = "7777777777"
         g$(2%) = "6666666666" : h$(2%) = "7777777777"
         g$(3%) = "666       " : h$(3%) = "       777"
         g$(4%) = "666       " : h$(4%) = "      777 "
         g$(5%) = "666       " : h$(5%) = "     777  "
         g$(6%) = "6666666666" : h$(6%) = "    777   "
         g$(7%) = "6666666666" : h$(7%) = "   777    "
         g$(8%) = "666    666" : h$(8%) = "  777     "
         g$(9%) = "666    666" : h$(9%) = " 777      "
         g$(10%)= "666    666" : h$(10%)= "777       "
         g$(11%)= "6666666666" : h$(11%)= "777       "
         g$(12%)= " 66666666 " : h$(12%)= "777       "

         i$(1%) = " 88888888 " : j$(1%) = " 99999999 "
         i$(2%) = "8888888888" : j$(2%) = "9999999999"
         i$(3%) = "888    888" : j$(3%) = "999    999"
         i$(4%) = "888    888" : j$(4%) = "999    999"
         i$(5%) = "888    888" : j$(5%) = "999    999"
         i$(6%) = "8888888888" : j$(6%) = "9999999999"
         i$(7%) = "8888888888" : j$(7%) = "9999999999"
         i$(8%) = "888    888" : j$(8%) = "       999"
         i$(9%) = "888    888" : j$(9%) = "       999"
         i$(10%)= "888    888" : j$(10%)= "       999"
         i$(11%)= "8888888888" : j$(11%)= "9999999999"
         i$(12%)= " 88888888 " : j$(12%)= " 99999999 "

         am$( 1%) = "  AAAAAAAAAAA       MMMM       MMMM"
         am$( 2%) = " AAAAAAAAAAAAA      MMMMM     MMMMM"
         am$( 3%) = "AAAA       AAAA     MMMMMM   MMMMMM"
         am$( 4%) = "AAAA       AAAA     MMMMMMMMMMMMMMM"
         am$( 5%) = "AAAAAAAAAAAAAAA     MMMM MMMMM MMMM"
         am$( 6%) = "AAAAAAAAAAAAAAA     MMMM  MMM  MMMM"
         am$( 7%) = "AAAA       AAAA     MMMM   M   MMMM"
         am$( 8%) = "AAAA       AAAA     MMMM       MMMM"
         am$( 9%) = "AAAA       AAAA     MMMM       MMMM"
         am$(10%) = "AAAA       AAAA     MMMM       MMMM"

         pm$( 1%) = "PPPPPPPPPPPP        MMMM       MMMM"
         pm$( 2%) = "PPPPPPPPPPPPPP      MMMMM     MMMMM"
         pm$( 3%) = "PPPP       PPPP     MMMMMM   MMMMMM"
         pm$( 4%) = "PPPP       PPPP     MMMMMMMMMMMMMMM"
         pm$( 5%) = "PPPPPPPPPPPPPP      MMMM MMMMM MMMM"
         pm$( 6%) = "PPPPPPPPPPPP        MMMM  MMM  MMMM"
         pm$( 7%) = "PPPP                MMMM   M   MMMM"
         pm$( 8%) = "PPPP                MMMM       MMMM"
         pm$( 9%) = "PPPP                MMMM       MMMM"
         pm$(10%) = "PPPP                MMMM       MMMM"

        return
