        REM *************************************************************~
            *                                                           *~
            *  DDDD   EEEEE  M   M  EEEEE  X   X  PPPP   L       SSS    *~
            *  D   D  E      MM MM  E       X X   P   P  L      S       *~
            *  D   D  EEEE   M M M  EEEE     X    PPPP   L       SSS    *~
            *  D   D  E      M   M  E       X X   P      L          S   *~
            *  DDDD   EEEEE  M   M  EEEEE  X   X  P      LLLLL   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DEMEXPLS - PRINTS OR DISPLAYS A PARTS EXPLOSION OF THE BOM*~
            *            FOR A SINGLE PART, AND PRINTS EXPLOSIONS FOR A *~
            *            RANGE OF PARTS.                                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/25/83 ! ORIGINAL -- COPIED BOMEXPLS, MODIFIED AND! HES *~
            *          ! RENAMED IT, CREATING A NEW REPORT        !     *~
            * 03/14/84 ! CORRECT BOM ID PROCESSING, DBL PRINTING  ! ECR *~
            *          ! ALSO, SCRATCH WORKFILES, NO EXIT AFTR PRT!     *~
            * 10/11/85 ! CLONED FROM BOMEXPLR                     ! KAB *~
            * 12/11/87 ! Cleaned up severity 4 errors             ! JDH *~
            * 12/16/87 ! Make Explosion screen somewhat more std  ! JDH *~
            * 04/19/88 ! Test for cancelled WOs to fix loop bug   ! JDH *~
            *************************************************************

        sub "DEMEXPLS" (demcode$,                                        ~
                               #4,       /* HNYMASTR UFB CHANNEL       */~
                               #5)       /* PIPCROSS UFB CHANNEL       */~

        dim                                                              ~
            demcode$19,                  /* ASSEMBLY PART NUMBER       */~
            demcodekey$100,              /* ASSEMBLY PART NUMBER - BOM */~
            component$19,                /* COMPONENT PART NUMBER      */~
            componentkey$100,            /* COMPONENT PART NUMBER - BOM*/~
            count%(15),                  /* ITEM COUNT OF 15 LEVELS    */~
            date$8,                      /* CALENDAR DATE FOR SHOW OFF */~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT INFO    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line$(20)79,                 /* TEXT TO DISPLAY ON SCREEN  */~
            maxlevels$2,                 /* MAX # OF LEVELS TO SHOW    */~
            p$(15)100,                   /* READ KEYS FOR 15 LEVELS    */~
            part$25,                     /* DUMMY ARGUMENT PART #      */~
            partkey$100,                 /* DUMMY ARGUMENT PART # - BOM*/~
            print$100,                   /* ARRAYS TO PRINT INFO WITH  */~
            tempdescr$34,                /* TEMPORARY PART DESCRIPTION */~
            pipstart$8,                  /*                            */~
            pipend$8,                    /*                            */~
            qtyu$10,                     /*                            */~
            qtyp$10,                     /*                            */~
            record$200,                  /*                            */~
            tttle$79,                    /* TITLE FOR DISPLAY ENTRIES  */~
            tttledescr$79,               /* SHOW WHAT PART WE'RE DOING */~
            workfile$8,                  /* WORK FILE NAME             */~
            worklib$8,                   /* WORK LIBRARY NAME          */~
            workvol$6                    /* WORK VOLUME NAME           */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01932
            cms2v$ = "05.01.00 07/01/88 General Release R5.01.00        "
L01932: REM *************************************************************
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
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE (NAME, DESCRIPTION)*~
            * # 5 ! PIPCROSS ! PLANNING CROSS REFERENCE                 *~
            * # 9 ! SORTWORK ! WORK FILE FOR SORT ROUTINE.              *~
            *************************************************************


            select # 9, "SORTWORK",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  1, keylen = 71

            call "SHOSTAT" ("One Moment Please")


            call "WORKOPEN" (# 9, "IO   ", 1000%, f2%(9))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES ALL THE VARIABLES NEEDED TO DO THE REPORT.    *~
            *************************************************************

            REM DATE INITIALIZATION
                date$ = date
                call "DATEFMT" (date$)

            tttle$ = "                                                   ~
        ~      ! QUANTITY !   SIZE   "

        init(" ") errormsg$, inpmessage$
                  maxlevels% = 15%
        goto L10210

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************
        REM Bypass input mode to make a subroutine out of program        ~
        INPUTMODE                                                        ~
            INIT(" ") ERRORMSG$, INPMESSAGE$, DEMCODE$, MAXLEVELS$       ~
                                                                         ~
            FOR FIELDNR% = 1 TO  2                                       ~
                GOSUB'161(FIELDNR%)                                      ~
                      IF ENABLED% = 0 THEN 10190                         ~
                GOSUB'201(FIELDNR%)                                      ~
                      IF KEYHIT%  =  1 THEN GOSUB STARTOVER              ~
                      IF KEYHIT%  = 16 AND FIELDNR% = 1 THEN 65000       ~
                      IF KEYHIT% <>  0 THEN       10120                  ~
                GOSUB'151(FIELDNR%)                                      ~
                      IF ERRORMSG$ <> " " THEN 10120                     ~
                NEXT FIELDNR%

L10210:     gosub L13000

L12000: REM *************************************************************~
            *          D I S P L A Y   P L O W   R O U T I N E          *~
            *                                                           *~
            * PLOWS THROUGH THE BILL OF MATERIALS FOR THE ASSEMBLY PART *~
            * NUMBER AND PRINTS                                         *~
            *************************************************************

            l%, displayline% = 0
            init(" ") line$(), tttledescr$, tttle$
            REM SET TITLE DESCRIPTION FOR SCREEN SHOWING.

            tttledescr$ = "SCHEDULED ACTIVITY FOR " & str(demcode$,1,16)
            tttledescr$ = tttledescr$ & " " & str(demcode$,17,3)
            tttle$ = "SOURCE (WORK ORDER, JOB ORDER, P.O. OR P.O. ADVICE"
            str(tttle$,59,10) = "    NEEDED"
            str(tttle$,70,10) = "   PROCURE"

            init (hex(00)) demcodekey$
            str(demcodekey$,1,19) = str(demcode$,1,19)

            gosub'7(demcodekey$)        /* P$ = PART NUMBER TO BOM    */

            if displayline% <> 0 then gosub L12288  /* DISPLAY PARTIAL  */
L12078:     u3% = 2%
            call "ASKUSER" (u3%,"*** END OF DATA ***","Press PF 2 to " & ~
                      "Display from Start.","Press PF 16 to Exit"," ")
               if u3% = 2% then L12000
               if u3% <> 16% then L12078
               goto L65000

            deffn'7(partkey$)
                  l% = l% + 1
                  p$(l%) = partkey$
                  count%(l%) = 0         /* SEQUENCE NUMBER FOR PRINT  */

L12108:           call "PLOWNEXT" (#9, p$(l%), 19%, f1%(9))
                       if f1%(9) = 0 then L12152
                  gosub L30000            /* LOAD PEG INFO              */

                  init (hex(00))  componentkey$
                  str(componentkey$,1,19) = str(p$(l%),20,19)
                  count%(l%) = count%(l%) + 1
                  gosub L12168            /* PROCESS PRINT ENTRY        */
                  if str(componentkey$,4,9) = "CANCELLED" then L12108
                  if l% < maxlevels% then gosub'7(componentkey$)
                                         /* DO COMPS IF NOT AT BOTTOM  */
                  goto L12108

L12152:           REM END ROUTINE GRACEFULLY.
                      l% = l% - 1
                      return

L12168:     REM FILL SCREEN ENTRIES, AND DISPLAY WHEN FULL.
                displayline% = displayline% + 1
                if displayline% > 20 then gosub L12288
                   put line$(displayline%), using L12276, " "
                   put str(line$(displayline%), 2 * l% - 1, 30),         ~
                        using L12264, count%(l%), component$
                   str(line$(displayline%),59,10) = qtyu$
                   str(line$(displayline%),70,10) = qtyp$
                   displayline% = displayline% + 1
                   call "RJUSTIFY" (tempdescr$)
                   put line$(displayline%), using L12276, tempdescr$
                   return

L12264: %###. #########################
        %                                                         !######~
        ~####!##########

L12276: %                       ##################################!      ~
        ~    !

L12288:     REM DISPLAY CONTROLLER FOR FULL SCREEN.  HANDLES P.F. KEYS.
L12292:         gosub L41000
                      if keyhit%  =  1 then       startover
                      if keyhit%  =  2 then       L12000
                      if keyhit% <> 14 then       L12328
                         infomsg$ = "Printing Explosion For" & hex(84)   ~
                                         & demcode$
                         call "SHOWMSG" (infomsg$)
                         gosub'200(demcode$)
                         close printer
                         goto L41000
L12328:         if keyhit%  = 16 then       L65000
                if keyhit% <>  5 then       L12292
                   init (" ") line$()
                   displayline% = 1%
                   return

L13000: REM *************************************************************~
            * S E L E C T   R E C O R D S   I N T O   S O R T   F I L E *~
            *                                                           *~
            * SELECTS RECORDS FROM THE INPUT DATABASE AND PUTS THEM     *~
            * INTO THE SORT FILE IF THEY SATISFY THE SORT REQUIREMENTS. *~
            * THE ROUTINE WORKS ACCORDING TO THE TECHNIQUES SET FORTH   *~
            *                                                           *~
            * RATHER THAN PLOWING THROUGH THE INVENTORY MASTER LOOKING  *~
            * AT EACH RECORD AND THROWING OUT THOSE WITH NO BILL OF     *~
            * MATERIALS RECORDS, WE LOOK THROUGH THE BILL OF MATERIALS  *~
            * FILE, LOAD THE CORRESPONDING PART NUMBER, AND SELECT THAT.*~
            * THAT WAY, WE DO NOT HAVE TO LOOK AT AS MANY PARTS AND WE  *~
            * CAN SEE ONLY THOSE THAT WE WANT.                          *~
            *************************************************************

           REM NOW CLOSE THE PRINTER, SCRATCH WORKFILE, AND START AGAIN
               close printer
               call "GETNAMES" addr (#9, workfile$, worklib$, workvol$)
               close #9   :    f2%(9) = 1%
               call "SCRATCH" addr ("F", workfile$, worklib$, workvol$,  ~
                                    "B", " ", u3%)
                                              u3% = u3%
            call "SHOWMSG"  ("Selecting Records...One Moment Please")
            call "WORKOPEN" (# 9, "IO   ", 1000%, f2%(9))

            REM LOAD BILL OF MATERIALS RECORD.
                init (hex(00)) demcodekey$
                str(demcodekey$,1,19) = str(demcode$,1,19)

L13604:         call "PLOWNEXT" (#5, demcodekey$, 19%, f1%(5))
                     if f1%(5) = 0 then return

                get #5, using L13635, record$
L13635:             FMT CH(150)
                if str(record$,39,19) <> " " then                        ~
                        str(record$,1,19) = str(record$,39,19)
                write #9, using L13635, record$
                goto L13604

        REM *************************************************************~
            * P R I N T   P A R T S   E X P L O S I O N   F O R   O N E *~
            *                           P A R T                         *~
            *                                                           *~
            * PRINTS THE PARTS EXPLOSION FOR ONE PART NUMBER.  THIS     *~
            * ROUTINE GETS CALLED FROM THE DISPLAY CONTROLLER FOR ONE - *~
            * PART AND ALSO FROM THE RESULTS OF THE SORT ROUTINE.       *~
            *************************************************************

            deffn'200(demcode$)

            l% = 0
            line% = 1000: page% = 0

            init (hex(00)) demcodekey$
            str(demcodekey$,1,19) = str(demcode$,1,19)

            gosub'8(demcodekey$)         /* P$ = PART NUMBER TO BOM    */
            print using L19752            /* PRINT TAG LINE WHEN DONE.  */
            return                       /* GET NEXT PART TO BOM       */

            deffn'8(partkey$)
                  l% = l% + 1
                  p$(l%) = partkey$
                  count%(l%) = 0         /* SEQUENCE NUMBER FOR PRINT  */

L19200:           call "PLOWNEXT" (#9, p$(l%), 19%, f1%(9))
                       if f1%(9) = 0 then L19288
                  gosub L30000            /* LOAD BOM RECORD & INFO.    */

                  init (hex(00))  componentkey$
                  str(componentkey$,1,19) = str(component$,1,19)
                  count%(l%) = count%(l%) + 1
                  gosub L19320            /* PROCESS PRINT ENTRY        */
                  if str(componentkey$,4,9) = "CANCELLED" then L19200
                  if l% < maxlevels% then gosub'8(componentkey$)
                                         /* DO COMPS IF NOT AT BOTTOM  */
                  goto L19200

L19288:           REM END ROUTINE GRACEFULLY.
                      l% = l% - 1
                      return

L19320:     REM ROUTINE TO PRINT ENTRY JUST LOADED.
                init(" ") print$
                gosub L19512              /* PAGE CONTROL SUBROUTINE    */
                put str(print$, 2 * l% - 1, 30),                         ~
                        using L19352, count%(l%), component$
L19352:                       %###. #########################

                   call "RJUSTIFY" (tempdescr$)
                   str(print$,54,34) = tempdescr$
                print using L19800, print$, qtyu$, qtyp$, pipstart$,pipend$
                return

L19512:     REM PAGE CONTROL ROUTINE.  FAIRLY SIMPLE, NO TAG LINE TRICKS.
                select printer(134)
                line% = line% + 1
                if line% < 60 then return
                   print page
                   page% = page% + 1
                   print using L19720, page%, demcode$, date$
                   print
                   print using L19752
                   print using L19768
                   print using L19752
                   line% = 5
            return


L19720: %PAGE#####        SCHEDULED ACTIVITY EXPLOSION FOR: #############~
        ~############                                            ########

L19752: %+---------------------------------------------------------------~
        ~------------------------+----------+----------+--------+--------+

L19768: %! SCHEDULED ACTIVITIES                                          ~
        ~                        !   NEEDED !  PROCURE !  START !     END!

L19800: %!###############################################################~
        ~########################!##########!##########!########!########!

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100,         /* ASSEMBLY PART #  */~
                                    L20200          /* NUMBER OF LEVELS */
                     return
L20100:     REM DEFAULT/ENABLE FOR ASSEMBLY PART NUMBER
                enabled% = 1
                return
L20200:     REM DEFAULT/ENABLE FOR NUMBER OF LEVELS
                enabled% = 1
         inpmessage$ = "The Maximum PRINTABLE Number Of Levels Is 15."
                maxlevels$ = "15"
                return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then L41000
            return clear all
            goto L65000

L30000: REM *************************************************************~
            * L O A D   B I L L   O F   M A T E R I A L S   R E C O R D *~
            *                                                           *~
            * LOADS THE BILL OF MATERIALS RECORD, WITH ITS ALL-IMPORTANT*~
            * QUANTITY AND SIZE FIELDS FROM THE BILL OF MATERIALS FILE. *~
            *************************************************************

            get #9 using L30500, component$, part$, qtyp, qtyu, pipstart$,~
                                  pipend$

            call "DESCRIBE" (#4, part$, tempdescr$, 1%, f1%(4))
            call "DATEFMT" (pipstart$)
            call "DATEFMT" (pipend$)
            call "NUMPRINT" (qtyu, 2, qtyu$)
            call "NUMPRINT" (qtyp, 2, qtyp$)
            return

L30500:         FMT XX(19),       /* PIPOUT                            */~
                    CH(19),       /* PIPIN                             */~
                    XX(19),       /* PIPOUT AGAIN                      */~
                    XX(14),       /* SYSDATE/SYSTIME                   */~
                    XX(1),        /* PEG OPTION                        */~
                    CH(25),       /* PART                              */~
                    PD(14,4),     /* QTY TO PROCURE                    */~
                    PD(14,4),     /* QTY TO USE                        */~
                    XX(1),        /* DEMANT TYPE                       */~
                    XX(1),        /* DEMAND PRIORITY                   */~
                    CH(6),        /* START ACTIVITY                    */~
                    CH(6)         /* FINISH DATE                       */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40150,         /* ASSEMBLY PART #  */~
                                    L40180          /* NUMBER OF LEVELS */
                     goto L40220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40180:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "PRINT SCHEDULED ACTIVITY EXPLOSIONS",                 ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "DEMAND CODE AND LINE",                                ~
               at (06,30), fac(lfac$( 1)), str(demcode$,1,16)   , ch(16),~
               at (06,48), fac(lfac$( 1)), str(demcode$,17,3)   , ch( 3),~
               at (07,02),                                               ~
                  "NUMBER OF LEVELS",                                    ~
               at (07,30), fac(lfac$( 2)), maxlevels$           , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40530
                  call "MANUAL" ("DEMEXPLS")
                  goto L40220

L40530:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40220

L41000: REM *************************************************************~
            *       P A R T S   E X P L O S I O N   D I S P L A Y       *~
            *                                                           *~
            * PARTS EXPLOSION DISPLAY SCREEN.  NO BIG DEAL, SINCE ALL   *~
            * THE COLUMNS ARE FORMATTED IN THE PRINT ROUTINE.           *~
            *************************************************************

            accept                                                       ~
               at (01,02),                                               ~
                  "(1)Start Over (2)First(5)Next(14)Print Report(15)Print~
        ~ Screen(16)Exit Program",                                        ~
                                                                         ~
               at (03,02), fac(hex(84)), tttledescr$            , ch(79),~
               at (04,02), fac(hex(a4)), tttle$                 , ch(79),~
               at (05,02), fac(hex(8c)), line$( 1)              , ch(79),~
               at (06,02), fac(hex(8c)), line$( 2)              , ch(79),~
               at (07,02), fac(hex(8c)), line$( 3)              , ch(79),~
               at (08,02), fac(hex(8c)), line$( 4)              , ch(79),~
               at (09,02), fac(hex(8c)), line$( 5)              , ch(79),~
               at (10,02), fac(hex(8c)), line$( 6)              , ch(79),~
               at (11,02), fac(hex(8c)), line$( 7)              , ch(79),~
               at (12,02), fac(hex(8c)), line$( 8)              , ch(79),~
               at (13,02), fac(hex(8c)), line$( 9)              , ch(79),~
               at (14,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (15,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(15)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(16)              , ch(79),~
               at (21,02), fac(hex(8c)), line$(17)              , ch(79),~
               at (22,02), fac(hex(8c)), line$(18)              , ch(79),~
               at (23,02), fac(hex(8c)), line$(19)              , ch(79),~
               at (24,02), fac(hex(8c)), line$(20)              , ch(79),~
                                                                         ~
               keys(hex(ff0102050e0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41000

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* DEMAND CODE/LINE */~
                                    L50200          /* NUMBER OF LEVELS */
                     return
L50100:     REM TEST DATA FOR DEMAND CODE/LINE
                call "RJUSTIFY" (str(demcode$,17,3))
                init (hex(00)) demcodekey$
                str(demcodekey$,1,19) = str(demcode$,1,19)
                call "PLOWNEXT" (#5, demcodekey$, 19%, f1%(5))
                     if f1%(5) <> 0% then return
                errormsg$ = "NO PEGGING FOUND FOR THIS DEMAND & LINE"
                return

L50200:     REM TEST DATA FOR NUMBER OF LEVELS
                if maxlevels$ = " " then maxlevels$ = "1"
                call "NUMVALID" (maxlevels$, err%, 1)
                if err% = 0 then L50235
                   errormsg$ = "Illegal Entry For Maximum Number Of Level~
        ~s: "                          & maxlevels$
                   return
L50235:         convert maxlevels$ to maxlevels%
                if maxlevels% >= 1 and maxlevels% <= 15 then return
                   errormsg$ = "Maximum Number Of Levels Must Be Between ~
        ~1 And 15: " & maxlevels$
                   return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            close #9
            end
