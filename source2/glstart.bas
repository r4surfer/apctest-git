        REM *************************************************************~
            *                                                           *~
            *   GSS   L       SSS   TTTTT   AAA   RRRR   TTTTT          *~
            *  G   G  L      S        T    A   A  R   R    T            *~
            *  G      L       SSS     T    AAAAA  RRRR     T            *~
            *  G  GG  L          S    T    A   A  R   R    T            *~
            *   GGG   LLLLL   SSS     T    A   A  R   R    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLSTART - CHECKS TO SEE WHETHER WE REALLY WANT TO CLOSE   *~
            *           THE PERIOD.                                     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/04/81 ! ORIGINAL                                 ! TEM *~
            * 07/12/84 ! CHANGED 'CLOSING' TO 'ROLLING'           ! BLT *~
            * 03/04/86 ! Change for unformatted Fiscal Dates      ! ERN *~
            * 08/14/86 ! Added check to ensure that no records    ! ERN *~
            *          ! exist in SHPHNYTF for month to be closed !     *~
            * 05/14/87 ! Standard Cost Changes.                   ! ERN *~
            * 09/21/87 ! Miscellaneous minor mods & modernizations! JIM *~
            * 12/20/90 ! Added check of the GLBATCH file.         ! RAC *~
            * 06/25/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            blankline$79,                /* BLANK LINE FOR INPUT SCREEN*/~
            code$4,                      /* CLOSE CODE                 */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dates$(32)8,                 /* Period Ending Dates        */~
            line2$79,                    /* Second Line of Screen      */~
            month$(12)20,                /* NAME OF MONTHS             */~
            plowkey$50,                  /* A Plow Key                 */~
            postdate$6,                  /* SHPHNYTF Post Date         */~
            prompt$(4)29,                /* Screen Prompts             */~
            rollperiod$50,               /* Which period to roll       */~
            tdate$8,                     /* Temp. Date String          */~
            temp$12,                     /* MONTHS OPEN LIST           */~
            yes$3                        /* SAFEGUARD QUESTION         */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20                  /* RETURN CODE FROM "FILEOPEN"*/


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! GET CUSTOMER DEFAULT INFO FROM HERE.     *~
            * #02 ! SHPHNYTF ! Shipping-Inventory Transaction File      *~
            * #03 ! UPDSESSN ! Update Session Control File              *~
            * #04 ! GLBATCH  ! Batch Status file                        *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc                                             ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #02,  "SHPHNYTF",                                     ~
                        varc, indexed, recsize = 572,                    ~
                        keypos = 1, keylen = 46,                         ~
                        alt key 1, keypos = 47, keylen = 80

            select #03,  "UPDSESSN",                                     ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =  4,   keylen = 17,                      ~
                        alt key  1, keypos =     1, keylen =  20

            select #04, "GLBATCH",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    5, keylen =   9,                     ~
                        alt key  1, keypos =    1, keylen =  13,         ~
                            key  2, keypos =   22, keylen =   8, dup     ~

        call "SHOSTAT" ("Opening files, one moment please")
            rslt$(1) = "REQUIRED"
            call "OPENCHCK" (#01, 0%, f2%(1), 0%, rslt$(1))
            if f2%(1) <> 0 then L65000

            call "OPENCHCK" (#02, 0%, f2%(2), 0%, rslt$(2))
            call "OPENCHCK" (#03, 0%, f2%(3), 0%, rslt$(3))
            call "OPENCHCK" (#04, 0%, f2%(4), 0%, rslt$(4))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * INITIALIZATION OF CERTAIN NECESSARY SYSTEM VARIABLES.     *~
            *************************************************************

            call "READ100" (#01, "FISCAL DATES", f1%(1))
            if f1%(1) = 0% then L09140
                get #01, using L09090, monthopen%, dates$()
L09090:              FMT XX(22), XX(136), BI(2), 32*CH(8)
                if monthopen% = 17% then L09140
                tdate$ = dates$(monthopen%+15)
		call "DATEFMT" (tdate$, 0%, temp$)
                convert str(temp$,5%,2%) to t%, data goto L09140
                call "DATECONV" (temp$)
                goto L09210
L09140:              u3% = 0%
                     call "ASKUSER" (u3%, "FISCAL DATES ERROR",          ~
                     "An error has occurred regarding the Fiscal Dates.",~
                       "This problem must be corrected to continue.",    ~
                       "Press (RETURN) to exit this program." )
                     goto L65000

L09210
*        Verify that there are no A/R Inventory Transactions remaining
            plowkey$ = "1"
L09230:     call "PLOWNEXT" (#02, plowkey$, 0%, f1%(2))
            if f1%(2) = 0% then L09370
                if str(plowkey$,2,1) = hex(00) or                        ~
                   str(plowkey$,2,1) = hex(ff) then L09230
                     get #02 using L09280, postdate$
L09280:                   FMT XX(47), CH(6)
                     if postdate$ > temp$ then L09230
                          u3% = 0%
                          call "ASKUSER" (u3%, "TRANSACTIONS PENDING",   ~
                            "Shipping Transactions still exist for the", ~
                            "Period to be closed (run program SHPJURNL)",~
                            "Press (RETURN) to exit this program." )
                          goto L65000

L09370
*        Verify that no Sessions still exist for Month
            plowkey$ = hex(00)
L09390:     call "PLOWNEXT" (#03, plowkey$, 0%, f1%(3))
            if f1%(3) = 0% then L09530
                get #03 using L09420, yes$, postdate$
L09420:              FMT CH(3), POS(41), CH(6)
                if yes$ = hex(ffffff) then L09390
                if postdate$ > temp$  then L09390
                     u3% = 0%
                     call "ASKUSER" (u3%, "SESSIONS STILL OPEN",         ~
                          "Data Entry Session still exists with Posting",~
                          "Date for the Period to be closed.",           ~
                          "Press (RETURN) to exit this program." )
                     goto L65000


L09530
*        Test for Batches open in GLBATCH file
            plowkey$ = "0        " & hex(00000000)
L09550:     call "PLOWALTS" (#4, plowkey$, 1%, 1%, f1%(4))
               if f1%(4) = 0% then L09680
            get #4 using L09580, pstseq%, postdate$
L09580:     FMT POS(10), BI(4), XX(2), CH(6)
            convert pstseq% to pstseq$, pic(#########)
               if postdate$ > temp$  then L09550
                     call "ASKUSER" (u3%, "BATCH IN PROCESS",            ~
                          "Batch not completed for module "              ~
                           & str(plowkey$,5,2) & ", journal "            ~
                           & str(plowkey$,7,3),  "and posting sequence " ~
                           & pstseq$ & " for the Period to be closed.",  ~
                          "Press (RETURN) to exit this program." )
                     goto L65000

L09680:     if str(plowkey$,1,1) = "1" then L09750
            plowkey$ = "1        " & hex(00000000)
            goto L09550

L09750:     date$ = date  :  call "DATEFMT" (date$)

            month$( 1) = "January    "
            month$( 2) = "February   "
            month$( 3) = "March      "
            month$( 4) = "April      "
            month$( 5) = "May        "
            month$( 6) = "June       "
            month$( 7) = "July       "
            month$( 8) = "August     "
            month$( 9) = "September  "
            month$(10) = "October    "
            month$(11) = "November   "
            month$(12) = "December   "

            tdate$ = temp$
		    call "DATEFMT" (tdate$, 0%, temp$)
            month$(t%) = month$(t%) & " " & str(temp$,7%,2%) & ", " &    ~
                                                          str(temp$,1%,4%)
            call "DATECONV" (temp$)
            rollperiod$ = "Close Period Ending: " & month$(t%)
            call "STRING" addr("CT", rollperiod$, 50%)


        REM *************************************************************~
            *        I N P U T   S A F E T Y   F O R   I N P U T        *~
            * --------------------------------------------------------- *~
            * INPUT SAFETY FOR ROLLING PERIOD INFORMATION.              *~
            *************************************************************

            prompt$(1) = "  ARE YOU SURE YOU WISH TO   "
            prompt$(2) = "     CLOSE THE PERIOD??      "
            prompt$(3) = "Any response other than 'YES'"
            prompt$(4) = " will NOT Close the Period.  "
L10100:     gosub L40000
                  if keyhit%  = 16 then       L65000
                  if keyhit% <>  0 then       L10100
                     yes$ = input$
                     if yes$ <> "YES" then L65000

            input$ = " "
            prompt$(1) = "  ENTER THE PASSWORD OR YOU  "
            prompt$(2) = "     CANNOT CONTINUE....     "
            prompt$(3) = " An incorrectly entered Code "
            prompt$(4) = "  will NOT Close the Period. "
            gosub L40000
            code$ = input$
            goto L65000

L40000: REM *************************************************************~
            *              S A F E G U A R D   S C R E E N              *~
            * --------------------------------------------------------- *~
            * Asks us (1) IF we REALLY want to Roll the Period and then *~
            * (2) for the secret code word to allow use to continue.    *~
            * (Say the secret word and collect $100).                   *~
            *************************************************************

            str(line2$,62) = "GLSTART: " & str(cms2v$,,8)

L40100:     accept                                                       ~
               at (01,02), "G/L Period-End Close",                       ~
               at (01,65), "Today: ",                                    ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (05,16), fac(hex(84)), rollperiod$            , ch(50),~
               at (07,25), "*********************************",          ~
               at (08,25), "*                               *",          ~
               at (09,25), "*                               *",          ~
               at (10,25), "* ----------------------------- *",          ~
               at (11,25), "*                               *",          ~
               at (12,25), "* ----------------------------- *",          ~
               at (13,25), "*                               *",          ~
               at (14,25), "*                               *",          ~
               at (15,25), "*********************************",          ~
               at (08,27), fac(hex(84)), prompt$(1)             , ch(29),~
               at (09,27), fac(hex(84)), prompt$(2)             , ch(29),~
               at (13,27), fac(hex(84)), prompt$(3)             , ch(29),~
               at (14,27), fac(hex(84)), prompt$(4)             , ch(29),~
                                                                         ~
               at (11,39), fac(hex(81)), input$                 , ch(04),~
                                                                         ~
               at (21,02), fac(hex(ac)), blankline$             , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                      keys(hex(000d0f10)), key (keyhit%)


               if keyhit% <> 13 then L40440
                  call "MANUAL" ("GLSTART")
                  goto L40100

L40440:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40100

L65000: REM *************************************************************~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One moment please")
            if yes$ = "YES" and code$ = "ROLL" then end 1  else  end 0

