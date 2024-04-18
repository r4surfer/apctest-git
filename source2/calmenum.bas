        REM *************************************************************~
            *                                                           *~
            *   CCC    AAA   L      M   M  EEEEE  N   N  U     U  M   M *~
            *  C   C  A   A  L      M M M  E      NN  N  U     U  M M M *~
            *  C      AAAAA  L      M   M  EEEE   N N N  U     U  M   M *~
            *  C   C  A   A  L      M   M  E      N  NN  U     U  M   M *~
            *   CCC   A   A  LLLLL  M   M  EEEEE  N   N    UUU    M   M *~
            *                                                           *~
            *         THIS IS THE LEVEL-II VERSION OF CALMENUM          *~
            *-----------------------------------------------------------*~
            * CALMENUM - THIS PROGRAM PROVIDES ALL DATE TO DATE         *~
            *            CONVERSIONS, MM/DD/YY, YYDDD, & PLAN DAY       *~
            *       ALSO PRINTS A CALENDAR WHICH CROSS REFERENCES       *~
            *            ALL OF THESE DATES                             *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/19/82 ! ORIGINAL                                 ! GLW *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/26/83 ! CHANGED TO LEVEL-II W/PROD CALENDAR      ! GLW *~
	    * 06/07/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim         date$8,                                      tgd$8,  ~
           tpd$4, tjd$7, tgd30$8, tpd30$4, tjd30$7, tgdb30$8, tpdb30$4,  ~
           tjdb30$7, daysfrom$3, gd1$8, pd1$4, jd1$7, daysago$3,         ~
           gd2$8, pd2$4, jd2$7,                                          ~
           gd3$8, pd3$4, jd3$7,                                          ~
           gd4$8, pd4$4, jd4$7,                                          ~
           gd5$8, pd5$4, jd5$7,                                          ~
           error$79, jdo$7, hdate$45

        dim                                                              ~
            fm$2,                                                        ~
            fmn$10,                                                      ~
            prt_date$8,                  /* print fmt date             */~
            yymmdd$(490)6,                                               ~
            dow$(490)3,                                                  ~
            yy%(490),                                                    ~
            mm%(490),                                                    ~
            dd%(490),                                                    ~
            mwoy%(490),                                                  ~
            fwoy%(490),                                                  ~
            cqoy%(490),                                                  ~
            fqoy%(490)

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

           call "SHOSTAT" ("LINKING TO DATA BASE FOR CALENDAR FUNCTIONS")

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM INFORMATION FILE FOR FISCAL YEAR  *~
            * #12 ! CALMASTR ! PRODUCTION CALENDAR, 490 CONSECUTIVE DAYS*~
            *************************************************************

            select # 1, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos =   1, keylen =  20

           select #12, "CALMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos = 1, keylen = 2

           call "OPENFILE" (# 1, "SHARE", f2%( 1), rslt$( 1), axd$( 1))
           call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
           if f2%(1) <> 0% or f2%(12) <> 0% then goto L03810

            init(" ") yymmdd$(), dow$(),  start$, end$,  startdescr$,~
                      enddescr$,  left$, target$, targetdescr$

           hit% = 1%
           date$ = date

            call "READ100" (#1, "FISCAL YEAR BEGINS  ", f1%(1))
                 if f1%(1) = 1 then goto L00970
                fm$ = "12"
                fmn$ = "UNKNOWN "
                goto L01000

L00970:     get #1, using L00980  , fm$, fmn$
L00980:             FMT XX(20), CH(02), CH(10)

L01000:     call "READ100" (#1, "MONTHS OPEN         ", f1%(1))
                 if f1%(1) <> 1% then goto L03810
           get #1, using L01030    , firstday$
L01030:    FMT XX(32), CH(6)

           call "DATE" addr("G-", firstday$, date$, d%, r%)
           d% = d% + 1%
           convert 490% -  d% to left$, pic(###)

           call "DATE" addr("GJ", date$, tjd$, r%)
           call "DATE" addr("G+", date$,  30%,  tgd30$, r%)
           call "DATE" addr("J+", tjd$,   30%,  tjd30$, r%)
           call "DATE" addr("G+", date$, -30%, tgdb30$, r%)
           call "DATE" addr("J+", tjd$,  -30%, tjdb30$, r%)
           tgd$ = date
           call "DATEFMT" (tgd$)
           call "DATEFMT" (tgd30$)
           call "DATEFMT" (tgdb30$)
           call "DATE" addr("HD", hdate$)
           convert d%       to   tpd$, pic(####)
           convert d% + 30% to tpd30$, pic(####)
           convert max(0%, d% - 30%) to tpdb30$, pic(####)
	   call "DATJULCV" (tjd$)
	   call "DATJULCV" (tjd30$)
	   call "DATJULCV" (tjdb30$)


L01230: accept                                                           ~
           at(01,02), "CALENDAR CALCULATIONS ON",                        ~
           at(01,34), fac(hex(85)), hdate$, ch(45),                      ~
           at(04,57), "GREGORIAN",                                       ~
           at(03,68), "PLAN",                                            ~
           at(04,68), " DAY",                                            ~
           at(04,74), "JULIAN",                                          ~
           at(05,02), "TODAY IS........................................",~
           at(05,58), fac(hex(8d)), tgd$, ch(8),                         ~
           at(05,68), fac(hex(8d)), tpd$, ch(4),                         ~
           at(05,75), fac(hex(8d)), str(tjd$, 3%, 5%), ch(5),            ~
           at(06,02), "30 DAYS FROM NOW WILL BE........................",~
           at(06,58), fac(hex(8d)), tgd30$, ch(8),                       ~
           at(06,68), fac(hex(8d)), tpd30$, ch(4),                       ~
           at(06,75), fac(hex(8d)), str(tjd30$, 3%, 5%), ch(5),          ~
           at(07,02), "30 DAYS AGO WAS ................................",~
           at(07,58), fac(hex(8d)), tgdb30$, ch(8),                      ~
           at(07,68), fac(hex(8d)), tpdb30$, ch(4),                      ~
           at(07,75), fac(hex(8d)), str(tjdb30$, 3%, 5%), ch(5),         ~
           at(08,02), "================================================",~
           at(10,07),"ENTER THE DESIRED CONVERSION",                     ~
           at(12,02), "F- 1 WHAT WILL THE DATE BE",                     ~
           at(12,30), fac(hex(82)), daysfrom$, ch(3),                    ~
           at(12,34), "DAYS FROM NOW?",                                  ~
           at(12,58), fac(hex(8d)), gd1$, ch(8),                         ~
           at(12,68), fac(hex(8d)), pd1$, ch(4),                         ~
           at(12,75), fac(hex(8d)), str(jd1$, 3%, 5%), ch(5),            ~
           at(13,02), "F- 2 WHAT WAS THE DATE"    ,                     ~
           at(13,26), fac(hex(82)), daysago$, ch(3),                     ~
           at(13,30), "DAYS AGO?",                                       ~
           at(13,58), fac(hex(8d)), gd2$, ch(8),                         ~
           at(13,68), fac(hex(8d)), pd2$, ch(4),                         ~
           at(13,75), fac(hex(8d)), str(jd2$, 3%, 5%), ch(5),            ~
           at(14,02), "F- 3 WHAT IS THE PLAN DAY FOR A GREGORIAN DATE?",~
           at(14,58), fac(hex(81)), gd3$, ch(8),                         ~
           at(14,75), fac(hex(8d)), str(jd3$, 3%, 5%), ch(5),            ~
           at(14,68), fac(hex(8d)), pd3$, ch(4),                         ~
           at(15,02), "F- 4 WHAT IS THE PLAN DAY FOR A JULIAN DATE?",   ~
           at(15,68), fac(hex(8d)), pd4$, ch(4),                         ~
           at(15,58), fac(hex(8d)), gd4$, ch(8),                         ~
           at(15,75), fac(hex(82)), str(jd4$, 3%, 5%), ch(5),            ~
           at(16,02), "F- 5 WHAT ARE THE DATES FOR A PLAN DAY?",        ~
           at(16,69), fac(hex(82)), pd5$, ch(3),                         ~
           at(16,75), fac(hex(8d)), str(jd5$, 3%, 5%), ch(5),            ~
           at(16,58), fac(hex(8d)), gd5$, ch(8),                         ~
           at(18,02), "================================================",~
           at(19,02), fac(hex(95)), error$, ch(79),                      ~
           at(20,02), "F-12 PRINT A 490 DAY PLANNING CALENDAR",         ~
           at(21,02), "F-13 INSTRUCTIONS",                              ~
           at(23,02), "F-15 PRINT THIS SCREEN",                         ~
           at(24,02), "F-16 EXIT",                                      ~
           keys(hex(01020304050c0d0f10)), key(keyhit%)

           if keyhit% =  1 then goto calc1
           if keyhit% =  2 then goto calc2
           if keyhit% =  3 then goto calc3
           if keyhit% =  4 then goto calc4
           if keyhit% =  5 then goto calc5
           if keyhit% = 12 then gosub print_calendar
           if keyhit% <> 13 then L01780
                call "MANUAL" ("CALMENUM")
                goto L01230

L01780:    if keyhit% = 16 then goto L03810
           if keyhit% <> 15 then goto L01230
                call "PRNTSCRN"
                goto   L01230

        calc1  : init(" ") error$
           convert daysfrom$ to dd%, data goto baddays
           call "DATE" addr("G+", date$, dd%, gd1$, r%)
           call "DATE" addr("GJ", date$, jdo$, r%)
           call "DATE" addr("J+", jdo$, dd%, jd1$, r%)
           convert min(d% + dd%, 490%) to pd1$, pic(####)
           call "DATEFMT" (gd1$)
	   call "DATJULCV" (jd1$)
           goto L01230

        baddays
           error$ = "ILLEGAL ENTRY FOR NUMBER OF DAYS"
           goto L01230

        calc2   : init(" ") error$
           convert daysago$ to dd% , data goto baddays
           call "DATE" addr("G+", date$,-dd%, gd2$, r%)
           call "DATE" addr("GJ", date$, jdo$, r%)
           call "DATE" addr("J+", jdo$,-dd%, jd2$, r%)
           convert max(0%, d% - dd%) to pd2$, pic(####)
           call "DATEFMT" (gd2$)
	   call "DATJULCV" (jd2$)
           goto L01230


        calc3  : init(" ")error$
           call "DATEOK" (gd3$, r%, error$)
           if error$ = " " then goto L02100
           goto L01230
L02100:    call "DATUNFMT" (gd3$)
           call "DATE" addr("G-", date$, gd3$, dd%, r%)
           if dd% > 0% then goto  L02150
           convert max(0%, d% + dd%) to pd3$, pic(####)
           goto L02165
L02150:    convert min(490%, d% + dd%) to pd3$, pic(####)
L02165:    call "DATE" addr("GJ", gd3$, jd3$, r%)
           call "DATEFMT" (gd3$)
	   call "DATJULCV" (jd3$)
           goto L01230


        calc4  : init(" ") error$
	   call "DATJULCV" (jd4$)
           call "DATE" addr("JG", jd4$, gd4$, r%)
           if r% = 8% then goto L02300
           call "DATE" addr("G-", firstday$, gd4$, dd%, r%)
           dd% = dd% + 1%
           if dd% > 0% then goto  L02270
           convert max(0%,  dd%) to pd4$, pic(####)
           goto L02280
L02270:    convert min(490%, dd%) to pd4$, pic(####)
L02280:    call "DATEFMT" (gd4$)
           call "DATJULCV" (jd4$)
           goto L01230
L02300:    error$ = "ILLEGAL INPUT DATE, PLEASE RESPECIFY"
           goto L01230

        calc5  : init(" ") error$
           convert pd5$ to pd5%, data goto L02342
           if pd5% > 490% or pd5% < 1% then goto L02342
           call "DATE" addr("G+", firstday$, pd5% - 1%, gd5$, r%)
           call "DATE" addr("GJ", gd5$,  jd5$, r%)
           call "DATEFMT" (gd5$)
           convert pd5% to pd5$, pic(000)
	   call "DATJULCV" (jd5$)
           goto L01230
L02342:    error$ = "ILLEGAL INPUT DATE, PLEASE RESPECIFY"
           goto L01230

        print_calendar
           call "SHOWMSG"("LOADING AND PRINTING CALENDAR")
           if hit% <> 0% then gosub L02830

            call "SHOWMSG" ("PRINTING A PRODUCTION CALENDAR")
            linecnt% = 1000%             /* TRIGGER NEW PAGE */
            select printer(134)

L02440: % PERIOD   DATE    DAY-OF-WEEK   YEAR     MONTH     DAY     WK-OF~
        ~-YR    WK-OF-YR   QRT-OF-YEAR    QTR-OF-YR
L02460: %                                                           1ST-M~
        ~ON     1ST-DAY    CALENDAR       FISCAL
L02480: %   ###  ########      ###       ####      ##        ##        ##~
        ~         ##          #             #
L02500: %  PRODUCTION CALENDAR FOR ######################################~
        ~#################
            for i = 1 to 490
               gosub newpage
               prt_date$ = yymmdd$(i)
               call "DATEFMT" (prt_date$)
               linecnt% = linecnt% + 1%
               print using L02480 , i, prt_date$, dow$(i), yy%(i), mm%(i),~
                     dd%(i), mwoy%(i), fwoy%(i), cqoy%(i), fqoy%(i)
            next i

            close printer

            return

        newpage
            if linecnt% < 55% then return
            call "DATE" addr("HD", hdate$)
            print page
            print using L02500 ,    hdate$
            print
            print
            print using L02440
            print using L02460
            print
            linecnt% = 7%
            return

L02830: rem**************************************************************~
            *      l o a d   o l d   calmastr   d a t a                 *~
            *************************************************************

            mat yy% = zer          : mat cqoy% = zer
            mat mm% = zer          : mat fqoy% = zer
            mat dd% = zer
            mat mwoy% = zer
            mat fwoy% = zer

            hit = 0
            call "SHOWMSG" ("LOADING PRODUCTION CALENDAR")
            call "READ100" (#12, "10", f1%(12))
                if f1%(12) = 1 then goto L02990
                hit = 1
                return
L02990:     get #12, using L03000 , str(yymmdd$(),1,1470)
L03000:         FMT XX(2), CH(1470)

            call "READ100" (#12, "11", f1%(12))
                if f1%(12) = 1 then goto L03060
                hit = 1
                return
L03060:     get #12, using L03070 , str(yymmdd$(),1471,1470)
L03070:         FMT XX(2), CH(1470)

            call "READ100" (#12, "20", f1%(12))
                if f1%(12) = 1 then goto L03130
                hit = 1
                return
L03130:     get #12, using L03140 , yy%()
L03140:         FMT XX(2), 490*BI(4)

            call "READ100" (#12, "30", f1%(12))
                if f1%(12) = 1 then goto L03200
                hit = 1
                return
L03200:     get #12, using L03210 , mm%()
L03210:         FMT XX(2), 490*BI(4)

            call "READ100" (#12, "40", f1%(12))
                if f1%(12) = 1 then goto L03270
                hit = 1
                return
L03270:     get #12, using L03280 , dd%()
L03280:         FMT XX(2), 490*BI(4)

            call "READ100" (#12, "50", f1%(12))
                if f1%(12) = 1 then goto L03340
                hit = 1
                return
L03340:     get #12, using L03350 , dow$()
L03350:         FMT XX(2), 490*CH(3)

            call "READ100" (#12, "60", f1%(12))
                if f1%(12) = 1 then goto L03410
                hit = 1
                return
L03410:     get #12, using L03420 , mwoy%()
L03420:         FMT XX(2), 490*BI(4)

            call "READ100" (#12, "61", f1%(12))
                if f1%(12) = 1 then goto L03480
                hit = 1
                return
L03480:     get #12, using L03490 , fwoy%()
L03490:         FMT XX(2), 490*BI(4)

            call "READ100" (#12, "70", f1%(12))
                if f1%(12) = 1 then goto L03550
                hit = 1
                return
L03550:     get #12, using L03560 , cqoy%()
L03560:         FMT XX(2), 490*BI(4)

            call "READ100" (#12, "71", f1%(12))
                if f1%(12) = 1 then goto L03620
                hit = 1
                return
L03620:     get #12, using L03630 , fqoy%()
L03630:         FMT XX(2), 490*BI(4)


            REM NOW GET THE STARTING & ENDING DATES & NUM DAYS LEFT

            str(startdescr$,1,6), start$ = yymmdd$(1)
            call "DATEFMT" ( startdescr$ )
            str(enddescr$,1,6), end$ = yymmdd$(490)
            call "DATEFMT" ( enddescr$ )
            call "DATE" addr("G-", date, yymmdd$(490), dl%, r%)
            convert dl% to left$, pic(###)
            if dl% > 50% then goto L03770
L03770:     target$      = start$
            targetdescr$ = startdescr$
            return

L03810: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("DATA BASE INTEGRITY CHECK IN PROCESS")

            end
