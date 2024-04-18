        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  PPPP   RRRR   L      EEEEE  M   M  PPPP    SSS   BBBB    *~
            *  P   P  R   R  L      E      MM MM  P   P  S      B   B   *~
            *  PPPP   RRRR   L      EEEE   M M M  PPPP    SSS   BBBB    *~
            *  P      R   R  L      E      M   M  P          S  B   B   *~
            *  P      R   R  LLLLL  EEEEE  M   M  P       SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLEMPSB - Displays employee's personnel information.     *~
            *            Written to reduce the size of PRLEMPIN ('cus of*~
            *            compiler limitations) but may be used where    *~
            *            ever appropriate.                              *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/22/88 ! Original                                 ! ERN *~
            * 12/18/89 ! Removed PF-8 and RETURN as Return Keys   ! MJB *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "PRLEMPSB" (#1)              /* PERMASTR.  Employee data   */
                                         /*    must already be loaded! */

        dim                                                              ~
            date$8,                      /* Date for screen display    */~
            inpmessage$79,               /* Informational Message      */~
            line2$79                     /* Screen Line #2             */

        dim                      /* PERSONNEL STUFF                    */~
            empcode$,            /* Employee Number                    */~
            status$1,            /* General purpose status indicator   */~
            lname$15,            /* Last name of person - part of pers */~
            fname$10,            /* First name of person               */~
            mname$1,             /* Middle name of person              */~
            ssnumber$11,         /* Social security number             */~
            telephone$10,        /* Telephone number                   */~
            street1$30,          /* Street address line 1              */~
            street2$30,          /* Street address line 2              */~
            city$20,             /* City in address                    */~
            county$20,           /* County in address                  */~
            state$2,             /* State in address                   */~
            zip$9,               /* Zip code in address                */~
            notify$30,           /* emergency contact                  */~
            empephon$10,         /* emergency contact's phone number   */~
            empereln$16,         /* Emergency contacts relationship to */~
            gender$1,            /* Gender of a person                 */~
            birthdate$10,        /* birth date                         */~
            eeoc$3,              /* minority (eeoc compliance) code    */~
            marital$1,           /* Marital status - personnel system  */~
            dependants$2,        /* Number of dependants - personnel s */~
            physical$16,         /* A persons physical status - handyc */~
            military$16,         /* A persons military status - vet, r */~
            citizenship$16,      /* Citizenship status - personnel sys */~
            passport$16,         /* Passport status - personnel system */~
            union$16,            /* Union status - personnel system    */~
            bonding$16,          /* Bonding status - personnel system  */~
            curjob$16,           /* Current job title - personnel syst */~
            curjobeeo$16,        /* EEO class of current job - personn */~
            curdept$4,           /* Current department                 */~
            curshift$1,          /* Current shift worked               */~
            cursupr$16,          /* Current supervisor                 */~
            origdate$10,         /* Original date hired                */~
            sendate$10,          /* Seniority date                     */~
            rehiredate$10,       /* Last date rehired                  */~
            lastermdate$10,      /* Last date terminated               */~
            lasttermreason$30,   /* Reason for last termination        */~
            lasttermjob$16       /* Job held when last terminated      */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! PERMASTR ! Personnel master file-ties to EMPMASTR i *~
            *************************************************************~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            str(line2$,62) = "PRLEMPSB: " & str(cms2v$,,8)

            gosub load_data

            inpmessage$ = "Personnel data can't be modified from here" & ~
                          ", it is for review only."


        REM *************************************************************~
            *                D I S P L A Y    S C R E E N S             *~
            *************************************************************

            goto L40000    /* All little different, hey?                */


        REM *************************************************************~
            *                    L O A D   D A T A                      *~
            *************************************************************
        load_data

          get #1 using L30570,    /* FILE: PERMASTR                     */~
            status$,             /* General purpose status indicator   */~
            lname$,              /* Last name of person - part of pers */~
            fname$,              /* First name of person               */~
            mname$,              /* Middle name of person              */~
            ssnumber$,           /* Social security number             */~
            empcode$,            /* Employee Number                    */~
            telephone$,          /* Telephone number                   */~
            street1$,            /* Street address line 1              */~
            street2$,            /* Street address line 2              */~
            city$,               /* City in address                    */~
            county$,             /* County in address                  */~
            state$,              /* State in address                   */~
            zip$,                /* Zip code in address                */~
            notify$,             /* emergency contact                  */~
            empephon$,           /* emergency contact's phone number   */~
            empereln$,           /* Emergency contacts relationship to */~
            gender$,             /* Gender of a person                 */~
            birthdate$,          /* birth date                         */~
            eeoc$,               /* minority (eeoc compliance) code    */~
            marital$,            /* Marital status - personnel system  */~
            dependants$,         /* Number of dependants - personnel s */~
            physical$,           /* A persons physical status - handyc */~
            military$,           /* A persons military status - vet, r */~
            citizenship$,        /* Citizenship status - personnel sys */~
            passport$,           /* Passport status - personnel system */~
            union$,              /* Union status - personnel system    */~
            bonding$,            /* Bonding status - personnel system  */~
            curjob$,             /* Current job title - personnel syst */~
            curjobeeo$,          /* EEO class of current job - personn */~
            curdept$,            /* Current department                 */~
            curshift$,           /* Current shift worked               */~
            cursupr$,            /* Current supervisor                 */~
            origdate$,           /* Original date hired                */~
            sendate$,            /* Seniority date                     */~
            rehiredate$,         /* Last date rehired                  */~
            lastermdate$,        /* Last date terminated               */~
            lasttermreason$,     /* Reason for last termination        */~
            lasttermjob$         /* Job held when last terminated      */

            empstatus$ = "On Leave"
            if status$ = "N" then empstatus$ = "Terminated"
            if status$ = "C" then empstatus$ = "Active"

            call "DATFMTC" (birthdate$)
            call "DATFMTC" (rehiredate$)
            call "DATFMTC" (sendate$)
            call "DATFMTC" (lastermdate$)
            call "DATFMTC" (origdate$)
            if mname$ <> " " then empname$=fname$&" "&mname$&". "&lname$ ~
                                 else  empname$ = fname$ & " " &  lname$
        return

L30570: FMT                      /* FILE: PERMASTR                     */~
            CH(1),               /* General purpose status indicator   */~
            CH(15),              /* Last name of person - part of pers */~
            CH(10),              /* First name of person               */~
            CH(1),               /* Middle name of person              */~
            CH(11),              /* Social security number             */~
            CH(12),              /* employee code                      */~
            CH(10),              /* Telephone number                   */~
            CH(30),              /* Street address line 1              */~
            CH(30),              /* Street address line 2              */~
            CH(20),              /* City in address                    */~
            CH(20),              /* County in address                  */~
            CH(2),               /* State in address                   */~
            CH(9),               /* Zip code in address                */~
            CH(30),              /* emergency contact                  */~
            CH(10),              /* emergency contact's phone number   */~
            CH(16),              /* Emergency contacts relationship to */~
            CH(1),               /* Gender of a person                 */~
            CH(6),               /* birth date                         */~
            CH(3),               /* minority (eeoc compliance) code    */~
            CH(1),               /* Marital status - personnel system  */~
            CH(2),               /* Number of dependants - personnel s */~
            CH(16),              /* A persons physical status - handyc */~
            CH(16),              /* A persons military status - vet, r */~
            CH(16),              /* Citizenship status - personnel sys */~
            CH(16),              /* Passport status - personnel system */~
            CH(16),              /* Union status - personnel system    */~
            CH(16),              /* Bonding status - personnel system  */~
            CH(16),              /* Current job title - personnel syst */~
            CH(16),              /* EEO class of current job - personn */~
            CH(4),               /* Current department                 */~
            CH(1),               /* Current shift worked               */~
            CH(16),              /* Current supervisor                 */~
            CH(6),               /* Original date hired                */~
            CH(6),               /* Seniority date                     */~
            CH(6),               /* Last date rehired                  */~
            CH(6),               /* Last date terminated               */~
            CH(30),              /* Reason for last termination        */~
            CH(16),              /* Job held when last terminated      */~
            CH(250),             /* filler for rest of record or inter */~
            CH(241)              /* filler for rest of record or inter */~


L40000: REM *************************************************************~
            *                    S C R E E N   1                        *~
            *************************************************************
        page_one

            str(line2$,,61) = "This Employee: " & empcode$

L40150:     accept                                                       ~
               at (01,02), "Personnel Data For Employee",                ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (05,02), "Current Status",                             ~
               at (05,30), fac(hex(84)), empstatus$             , ch(10),~
               at (06,02), "Employee's Name",                            ~
               at (06,30), fac(hex(84)), empname$               , ch(48),~
               at (07,02), "Social Security Number",                     ~
               at (07,30), fac(hex(84)), ssnumber$              , ch(11),~
               at (08,02), "Street Address",                             ~
               at (08,30), fac(hex(84)), street1$               , ch(30),~
               at (08,61), fac(hex(84)), street2$               , ch(20),~
               at (09,02), "City, County, State & Zip",                  ~
               at (09,30), fac(hex(84)), city$                  , ch(20),~
               at (09,51), fac(hex(84)), county$                , ch(17),~
               at (09,69), fac(hex(84)), state$                 , ch(02),~
               at (09,72), fac(hex(84)), zip$                   , ch(09),~
               at (10,02), "Telephone Number",                           ~
               at (10,30), fac(hex(84)), str(telephone$,1,3)    , ch(03),~
               at (10,34), "-", at (10,40), "-",                         ~
               at (10,36), fac(hex(84)), str(telephone$,4,3)    , ch(03),~
               at (10,42), fac(hex(84)), str(telephone$,7,4)    , ch(04),~
               at (11,02), "Gender",                                     ~
               at (11,30), fac(hex(84)), gender$                , ch(01),~
               at (12,02), "Birthdate",                                  ~
               at (12,30), fac(hex(84)), birthdate$             , ch(10),~
               at (13,02), "EEO Code",                                   ~
               at (13,30), fac(hex(84)), eeoc$                  , ch(03),~
               at (14,02), "Marital Status",                             ~
               at (14,30), fac(hex(84)), marital$               , ch(01),~
               at (15,02), "Number of Dependents",                       ~
               at (15,30), fac(hex(84)), dependants$            , ch(02),~
               at (16,02), "Notify In Case Of Emergency",                ~
               at (16,30), fac(hex(84)), notify$                , ch(30),~
               at (17,02), "Telephone - Relationship",                   ~
               at (17,30), fac(hex(84)), str(empephon$ ,1,3)    , ch(03),~
               at (17,34), "-", at (17,40), "-",                         ~
               at (17,36), fac(hex(84)), str(empephon$ ,4,3)    , ch(03),~
               at (17,40), "-",                                          ~
               at (17,42), fac(hex(84)), str(empephon$ ,7,4)    , ch(04),~
               at (17,48), fac(hex(84)), empereln$              , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
                                                                         ~
               at (23,25), "(5)Next Page",                               ~
               at (23,65), "(15)Print Screen",                           ~
                                                                         ~
               at (24,65), "(16)Return",                                 ~
                   keys(hex(ff05ff0d0f10)), key (button%)

               if button% = 5% then page_two

               if button% <> 13% then L40760
                  call "MANUAL" ("PRLEMPSB")
                  goto L40150

L40760:        if button% <> 15% then L40800
                  call "PRNTSCRN"
                  goto L40150

L40800:        if button% = 16% then exit_program else L40150



        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   2       *~
            *-----------------------------------------------------------*~
            * Screen for editing page 2 of document.                    *~
            *************************************************************

        page_two

L40920:     accept                                                       ~
               at (01,02), "Personnel Data For Employee",                ~
               at (01,50), "Page 2    Todays Date:",                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (05,02), "Physical Status",                            ~
               at (05,30), fac(hex(84)), physical$              , ch(16),~
               at (06,02), "Military Status",                            ~
               at (06,30), fac(hex(84)), military$              , ch(16),~
               at (07,02), "Citizenship Status",                         ~
               at (07,30), fac(hex(84)), citizenship$           , ch(16),~
               at (08,02), "Passport Status",                            ~
               at (08,30), fac(hex(84)), passport$              , ch(16),~
               at (09,02), "Union Status",                               ~
               at (09,30), fac(hex(84)), union$                 , ch(16),~
               at (10,02), "Bonding Status",                             ~
               at (10,30), fac(hex(84)), bonding$               , ch(16),~
               at (11,02), "Current Job Title",                          ~
               at (11,30), fac(hex(84)), curjob$                , ch(16),~
               at (12,02), "EEO Class of Current Job",                   ~
               at (12,30), fac(hex(84)), curjobeeo$             , ch(03),~
               at (13,02), "Cur Dept, Shift, Supervisor",                ~
               at (13,30), fac(hex(84)), curdept$               , ch(04),~
               at (13,37), fac(hex(84)), curshift$              , ch(01),~
               at (13,40), fac(hex(84)), cursupr$               , ch(16),~
               at (14,02), "Original Hire Date",                         ~
               at (14,30), fac(hex(84)), origdate$              , ch(10),~
               at (15,02), "Seniority Date",                             ~
               at (15,30), fac(hex(84)), sendate$               , ch(10),~
               at (16,02), "Rehire Date",                                ~
               at (16,30), fac(hex(84)), rehiredate$            , ch(10),~
               at (17,02), "Last Termination Date",                      ~
               at (17,30), fac(hex(84)), lastermdate$           , ch(10),~
               at (18,02), "Last Termination Job Title",                 ~
               at (18,30), fac(hex(84)), lasttermjob$           , ch(16),~
               at (19,02), "Last Termination Reason",                    ~
               at (19,30), fac(hex(84)), lasttermreason$        , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
                                                                         ~
               at (23,25), "(4)Prev Page",                               ~
               at (23,65), "(15)Print Screen",                           ~
                                                                         ~
               at (24,65), "(16)Return",                                 ~
                   keys(hex(ff0104ff0d0f10)),  key (button%)

               if button% = 4% then page_one

               if button% <> 13% then L41470
                  call "MANUAL" ("PRLEMPSB")
                  goto L40920

L41470:        if button% <> 15% then L41510
                  call "PRNTSCRN"
                  goto L40920

L41510:        if button% = 16% then exit_program else page_two



        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
