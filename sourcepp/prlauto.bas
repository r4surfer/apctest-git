        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L       AAA   U   U  TTTTT   OOO           *~
            *  P   P  R   R  L      A   A  U   U    T    O   O          *~
            *  PPPP   RRRR   L      AAAAA  U   U    T    O   O          *~
            *  P      R   R  L      A   A  U   U    T    O   O          *~
            *  P      R   R  LLLLL  A   A   UUU     T     OOO           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLAUTO  - INPUTS PAYROLL AUTOMATICALLY FOR ALL EMPLOYEES *~
            *            WHOSE MASTER RECORD INDICATES THAT THIS SHOULD *~
            *            BE DONE.                                       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/18/81 ! ORIGINAL                                 ! TEM *~
            * 10/19/81 ! RETURN CODE IF NO                        ! TEM *~
            * 06/28/85 ! Check personnel file to validate status  ! HES *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *************************************************************

        dim                                                              ~
            blankline$79,                /* SCREEN VARIABLE            */~
            date$8,                      /* SCREEN FORMATTED DATE      */~
            emprec$(20)7,                /* ENTIRE MASTER RECORD       */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            readkey$50,                  /* PLOW KEY FOR EMPLOYEE FILE */~
            readkey1$50,                 /* PLOW KEY FOR EARNINGS      */~
            status$1,                    /* Employee Status            */~
            type$1,                      /* PAY FREQUENCY TYPE OF EMP. */~
            yes$3                        /* THE ANSWER                 */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
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
            * # 4 ! EMPMASTR ! EMPLOYEE MASTER FILE                     *~
            * # 5 ! EMPEARN1 ! EMPLOYEE EARNINGS RECORD FILE            *~
            * # 7 ! PERMASTR ! EMPLOYEE PERSONNEL FILE                  *~
            *************************************************************

            select #4,  "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup


            select #5, "EMPEARN1",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 200,                                    ~
                       keypos = 1, keylen = 15,                          ~
                       alt key 1, keypos = 16, keylen = 28

            select #7, "PERMASTR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 950,                                    ~
                       keypos = 39, keylen = 12,                         ~
                       alt key  1, keypos =  28, keylen = 23,            ~
                           key  2, keypos =   2, keylen = 49,            ~
                           key  3, keypos =   1, keylen = 50


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "OPENFILE" (#4, "SHARE", f2%(4), rslt$(4), axd$(4))
            call "OPENFILE" (#5, "SHARE", f2%(5), rslt$(5), axd$(5))
            call "OPENFILE" (#7, "SHARE", f2%(7), rslt$(7), axd$(7))

        REM *************************************************************~
            *        I N I T I A L I Z A T I O N                        *~
            *                                                           *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            blankline$ = "1=Weekly 2=Biweekly 3=Semimonthly 4=Monthly 5=Q~
        ~uarterly 6=Semiannual 7=Annual"

        REM *************************************************************~
            *   M A I N   P R O G R A M                                 *~
            *                                                           *~
            * PLOWS THROUGH EMPLOYEE FILE, LOOKING FOR AUTO-EMPLOYEE    *~
            *************************************************************
            flag = 0

            REM CHECK IF OPERATOR HIT CORRECT KEY
L10080:         gosub L40000
                     if keyhit% = 16 then L65000
                     if keyhit% <> 0 then L10080
                if yes$ <> "YES" then L65000
                if type$ >= "1" and type$ <= "7" then L10160
                   errormsg$ = "Invalid Entry For Employee Type: " & type$
                   go to L10080

L10160:     blankline$ = "Calculating Automatic Payroll For Type "       ~
                           & type$ & " Employees"
            call "SHOWMSG" (blankline$)

            REM PLOW THROUGH EMPLOYEE FILE
L10210:         call "PLOWNEXT" (#4, readkey$, 0%, f1%(4))
                   if f1%(4) = 0 then L65000
                      get #4, using L10240, str(emprec$(), 1)
L10240:                      FMT CH(136)
                      if str(emprec$(), 57, 1) <> "Y" then L10210 /*AUTO?*/
                      if str(emprec$(), 19, 1) <> type$ then L10210
                      REM Insure employee status is 'Current'...
                      call "READ100" (#7, readkey$, f1%(7))
                         if f1%(7) = 0 then L10210
                      get #7, status$
                      if status$ <> "C" then L10210
                      flag = 1
                      readkey1$ = str(emprec$(), 1, 12)
                      earned% = 0
                      gosub L11000
                      if earned% = 0 then L10210
                         call "READ101" (#4, readkey$, f1%(4))
                         if f1%(4) = 0 then L10210
                            get #4, using L10240, str(emprec$(), 1)
                            str(emprec$(), 70, 1) = "Y" /* OK TO DED NOW*/
                            put #4, using L10240, str(emprec$(),,136)
                            rewrite #4
                            go to L10210

L11000: REM *************************************************************~
            * PLOW THROUGH EARNINGS FILE UPDATING AUTO PAYROLLS         *~
            *************************************************************

            REM PLOW THROUGH EARNINGS FILE
L11050:         call "PLOWNXT1" (#5, readkey1$, 12%, f1%(5))
                     if f1%(5) = 0 then return
                     get #5, using L11080, str(earnrec$(), 1)
L11080:                      FMT CH(200)
                     str(earnrec$(), 85, 16) = str(earnrec$(), 69, 16)
                     put #5, using L11080, str(earnrec$(), 1)
                     rewrite #5
                     earned% = 1
                     go to L11050

L40000: REM *************************************************************~
            *              S A F E G U A R D   S C R E E N              *~
            *                                                           *~
            * ASKS US IF WE *REALLY* WANT TO INPUT AUTOMATIC PAYROLL.   *~
            * WE'RE IN BACKGROUND MODE, THEN IT ASSUMES WE KNOW WHAT    *~
            * WE'RE DOING AND DOES IT FOR US.                           *~
            *************************************************************

L40080:     accept                                                       ~
               at (01,02),                                               ~
                  "ENTER AUTOMATIC PAYROLL INTO EARNINGS BUFFER",        ~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (07,25), "*********************************",          ~
               at (08,25), "*    ARE YOU SURE YOU WISH TO   *",          ~
               at (09,25), "*     RUN AUTOMATIC PAYROLL?    *",          ~
               at (10,25), "*                               *",          ~
               at (11,25), "*                               *",          ~
               at (12,25), "*                               *",          ~
               at (13,25), "* ANY RESPONSE OTHER THAN 'YES' *",          ~
               at (14,25), "*     WILL EXIT THIS PROGRAM    *",          ~
               at (15,25), "*********************************",          ~
                                                                         ~
               at (11,39), fac(hex(81)), yes$                   , ch(03),~
                                                                         ~
               at (17,26), "FOR WHICH TYPE OF EMPLOYEES?",               ~
               at (17,56), fac(hex(81)), type$                  , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(000f10)),                                        ~
               key (keyhit%)

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40080

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOWMSG" ("ONE MOMENT PLEASE")

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%

            end flag
