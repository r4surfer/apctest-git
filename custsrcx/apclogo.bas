        dim txt$(25)70, pfkeys$32, i$(24)80, cursor%(2), rpt_time$8
        dim s_txt$(4)33, hh$2, mm$2, ap$2, company$60
            s_txt$(1) = "Good Morning - Have a Nice Day   "
            s_txt$(2) = "Good Afternoon - Have a Nice Day "
            s_txt$(3) = "Good Evening - Don't Work To Late"
            s_txt$(4) = "Good Evening - Your Working Late "
            s_txt% = 1%
            rpt_time$ = " "
            call "COMPNAME" (12%, company$, ret%)
            call "TIME" (rpt_time$)
            hh$ = str(rpt_time$,1%,2%)
            mm$ = str(rpt_time$,4%,2%)
            ap$ = str(rpt_time$,7%,2%)
            hh% = 0%
            convert hh$ to hh%, data goto L00150
L00150:
            if ap$ <> "AM" then goto L00210
               s_txt% = 1%
               if hh% < 6% or hh% = 12% then s_txt% = 4%
               goto L00240

L00210:     s_txt% = 2%
            if hh% > 6% and hh% <> 12% then s_txt% = 3%

L00240: keyhit%, u3% = 0%

        pfkeys$ = hex(ffffffffffffffffffffffffffffffff00)
        txt$( 1)=                                                        ~
        "    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$    "
        txt$( 2)=                                                        ~
        "   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   "
        txt$( 3)=                                                        ~
        "  $$                                                        $$"
        txt$( 4)=                                                        ~
        " $$                                                          $$"
        txt$( 5)=                                                        ~
        "$$      AAAAAAAAAAAA   WWW          WWW    DDDDDDDDDDD        $$"
        txt$( 6)=                                                        ~
        "$$      AAA      AAA   WWW          WWW    DDD        DD      $$"
	txt$( 7)=                                                        ~	
        "$$      AAA      AAA   WWW          WWW    DDD         DDD    $$"
	txt$( 8)=                                                        ~
        "$$      AAA      AAA   WWW          WWW    DDD         DDD    $$"
	txt$( 9)=                                                        ~
        "$$      AAA      AAA   WWW          WWW    DDD         DDD    $$"
	txt$(10)=                                                        ~
        "$$      AAAAAAAAAAAA   WWW          WWW    DDD         DDD    $$"
	txt$(11)=                                                      ~
        "$$      AAA      AAA   WWW    WW    WWW    DDD         DDD    $$"
	txt$(12)=                                                        ~
        "$$      AAA      AAA   WWW   W  W   WWW    DDD         DDD    $$"	
	txt$(13)=                                                        ~
        "$$      AAA      AAA   WWW  W    W  WWW    DDD         DDD    $$"
	txt$(14)=                                                        ~
        "$$      AAA      AAA   WWW W      W WWW    DDD        DD      $$"
	txt$(15)=                                                        ~
        "$$      AAA      AAA   WWWW        WWWW    DDDDDDDDDDDD       $$"
        txt$(16)=                                                        ~
        "$$ ---------------------------------------------------------- $$"
        txt$(17)=                                                        ~
        " $$                 Atrium Window and Door                    $$"
        txt$(18)=                                                        ~
        "  $$                                                        $$  "
        txt$(19)=                                                        ~
        "   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$   "
        txt$(20)=                                                        ~
        "    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$    "

         accept                                                          ~
             at (02,09), fac(hex(84)), txt$( 1%)               , ch(65), ~
             at (03,09), fac(hex(84)), txt$( 2%)               , ch(65), ~
             at (04,09), fac(hex(84)), txt$( 3%)               , ch(65), ~
             at (05,09), fac(hex(84)), txt$( 4%)               , ch(65), ~
             at (06,09), fac(hex(84)), txt$( 5%)               , ch(65), ~
             at (07,09), fac(hex(84)), txt$( 6%)               , ch(65), ~
             at (08,09), fac(hex(84)), txt$( 7%)               , ch(65), ~
             at (09,09), fac(hex(84)), txt$( 8%)               , ch(65), ~
             at (10,09), fac(hex(84)), txt$( 9%)               , ch(65), ~
             at (11,09), fac(hex(84)), txt$(10%)               , ch(65), ~
             at (12,09), fac(hex(84)), txt$(11%)               , ch(65), ~
             at (13,09), fac(hex(84)), txt$(12%)               , ch(65), ~
             at (14,09), fac(hex(84)), txt$(13%)               , ch(65), ~
             at (15,09), fac(hex(84)), txt$(14%)               , ch(65), ~
             at (16,09), fac(hex(84)), txt$(15%)               , ch(65), ~
             at (17,09), fac(hex(84)), txt$(16%)               , ch(65), ~
             at (18,09), fac(hex(84)), txt$(17%)               , ch(65), ~
             at (19,09), fac(hex(84)), txt$(18%)               , ch(65), ~
             at (20,09), fac(hex(84)), txt$(19%)               , ch(65), ~
             at (21,09), fac(hex(84)), txt$(20%)               , ch(65), ~
                                                                         ~
             at (22,26), fac(hex(84)), s_txt$(s_txt%)          , ch(33), ~
             at (24,27), "Press Any Key to Continue!!!!",                ~
             keys(pfkeys$), key(keyhit%)

             close ws
             call "SCREEN" addr("C", u3%, "I", i$(), cursor%())

        end

