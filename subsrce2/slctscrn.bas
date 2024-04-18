        REM *************************************************************~
            *                                                           *~
            *   SSS   L       CCC   TTTTT   SSS    CCC   RRRR   N   N   *~
            *  S      L      C   C    T    S      C   C  R   R  NN  N   *~
            *   SSS   L      C        T     SSS   C      RRRR   N N N   *~
            *      S  L      C   C    T        S  C   C  R   R  N  NN   *~
            *   SSS   LLLLL   CCC     T     SSS    CCC   R   R  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SLCTSCRN - INPUTS SELECT PARAMETERS.  ARGUMENTS INCLUDE   *~
            *            THE SELECT LISTS AND THE TITLE TO DISPLAY.     *~
            *            THIS IS ONE OF THE GENERALIZED ROUTINES PART OF*~
            *            THE ***SUPER SELECTOR***                       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/17/81 ! ORIGINAL                                 ! BCW *~
            * 06/21/82 ! ADDED "S" SINGLE PARAMETER FORMAT        ! ECR *~
            * 06/27/86 ! Changed Screen Format To New Standards   ! MJB *~
            *************************************************************

         sub "SLCTSCRN" (title$, errormsg$, inpmessage$, keyhit%, instr$)

        com                                                              ~
            extlen%(15),                 /* EXTERNAL FIELD LENGTHS     */~
            field%(15),                  /* SELECTABLE FIELD LIST      */~
            format$(15)1,                /* DATA TYPE FORMAT CODES     */~
            from$(15)25,                 /* LOW RANGE DATA TEST ITEMS  */~
            fromnr(15),                  /* LOW RANGE NUMERIC TEST ITEM*/~
            length%(15),                 /* INTERNAL FIELD LENGTHS     */~
            position%(15),               /* POSITION IN REC (FROM 1)   */~
            prompt$(15)25,               /* FIELD NAME PROMPT          */~
            record$(12)250,              /* 3 RECORDS * 1000 CHARS EA. */~
            record%(15),                 /* WHICH OF 3 RECORDS IT'S IN */~
            to$(15)25,                   /* HI VALUE RANGE DATA TEST   */~
            tonr(15)                     /* HI RANGE NUMERIC RANGE TEST*/~

        dim                                                              ~
            date$8,                      /* TODAY'S CALENDAR DATE      */~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            fac$(30)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            inpmessage$79,               /* INPUT SCREEN MESSAGE TEXT  */~
            instr$18,                    /* Name & Rel Level of Caller */~
                                         /* Enlarged field to 16 char  */~
                                         /*  to allow passing of the   */~
                                         /*  version number in pos 11- */~
                                         /*  18 for display on line 2  */~
            line2$79,                    /* Screen Line #2             */~
            title$40                     /* TITLE AT TOP OF SCREEN     */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.16.03 07/24/86 Receiver and Miscellaneous      "

            date$ = date
            call "DATEFMT" (date$)

            str(line2$,61,18) = instr$

        REM *************************************************************~
            *       I N P U T   S E L E C T   P A R A M E T E R S       *~
            *                                                           *~
            * ROUTINE TO INPUT SELECT PARAMETERS FOR USE IN THE SELECT  *~
            * PASS.                                                     *~
            *************************************************************

            init(hex(8c)) fac$()

            for temp% = 1 to 15
                if prompt$(temp%) = " " then L40700
                   fac$(temp%) = hex(81)
                   if format$(temp%) = "S" then L40700                    ~
                      else fac$(temp% + 15) = hex(81)
L40700:     next temp%

L40800:     accept                                                       ~
               at (01,02), fac(hex(8c)), title$                 , ch(40),~
               at (01,66),                                               ~
                  "DATE:",                                               ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(8c)), prompt$( 1),                    ~
               at (07,02), fac(hex(8c)), prompt$( 2),                    ~
               at (08,02), fac(hex(8c)), prompt$( 3),                    ~
               at (09,02), fac(hex(8c)), prompt$( 4),                    ~
               at (10,02), fac(hex(8c)), prompt$( 5),                    ~
               at (11,02), fac(hex(8c)), prompt$( 6),                    ~
               at (12,02), fac(hex(8c)), prompt$( 7),                    ~
               at (13,02), fac(hex(8c)), prompt$( 8),                    ~
               at (14,02), fac(hex(8c)), prompt$( 9),                    ~
               at (15,02), fac(hex(8c)), prompt$(10),                    ~
               at (16,02), fac(hex(8c)), prompt$(11),                    ~
               at (17,02), fac(hex(8c)), prompt$(12),                    ~
               at (18,02), fac(hex(8c)), prompt$(13),                    ~
               at (19,02), fac(hex(8c)), prompt$(14),                    ~
               at (20,02), fac(hex(8c)), prompt$(15),                    ~
                                                                         ~
               at (06,30), fac(fac$( 1)), str(from$( 1), 1, extlen%( 1)),~
               at (07,30), fac(fac$( 2)), str(from$( 2), 1, extlen%( 2)),~
               at (08,30), fac(fac$( 3)), str(from$( 3), 1, extlen%( 3)),~
               at (09,30), fac(fac$( 4)), str(from$( 4), 1, extlen%( 4)),~
               at (10,30), fac(fac$( 5)), str(from$( 5), 1, extlen%( 5)),~
               at (11,30), fac(fac$( 6)), str(from$( 6), 1, extlen%( 6)),~
               at (12,30), fac(fac$( 7)), str(from$( 7), 1, extlen%( 7)),~
               at (13,30), fac(fac$( 8)), str(from$( 8), 1, extlen%( 8)),~
               at (14,30), fac(fac$( 9)), str(from$( 9), 1, extlen%( 9)),~
               at (15,30), fac(fac$(10)), str(from$(10), 1, extlen%(10)),~
               at (16,30), fac(fac$(11)), str(from$(11), 1, extlen%(11)),~
               at (17,30), fac(fac$(12)), str(from$(12), 1, extlen%(12)),~
               at (18,30), fac(fac$(13)), str(from$(13), 1, extlen%(13)),~
               at (19,30), fac(fac$(14)), str(from$(14), 1, extlen%(14)),~
               at (20,30), fac(fac$(15)), str(from$(15), 1, extlen%(15)),~
                                                                         ~
               at (06,56), fac(fac$(16)), str(to$  ( 1), 1, extlen%( 1)),~
               at (07,56), fac(fac$(17)), str(to$  ( 2), 1, extlen%( 2)),~
               at (08,56), fac(fac$(18)), str(to$  ( 3), 1, extlen%( 3)),~
               at (09,56), fac(fac$(19)), str(to$  ( 4), 1, extlen%( 4)),~
               at (10,56), fac(fac$(20)), str(to$  ( 5), 1, extlen%( 5)),~
               at (11,56), fac(fac$(21)), str(to$  ( 6), 1, extlen%( 6)),~
               at (12,56), fac(fac$(22)), str(to$  ( 7), 1, extlen%( 7)),~
               at (13,56), fac(fac$(23)), str(to$  ( 8), 1, extlen%( 8)),~
               at (14,56), fac(fac$(24)), str(to$  ( 9), 1, extlen%( 9)),~
               at (15,56), fac(fac$(25)), str(to$  (10), 1, extlen%(10)),~
               at (16,56), fac(fac$(26)), str(to$  (11), 1, extlen%(11)),~
               at (17,56), fac(fac$(27)), str(to$  (12), 1, extlen%(12)),~
               at (18,56), fac(fac$(28)), str(to$  (13), 1, extlen%(13)),~
               at (19,56), fac(fac$(29)), str(to$  (14), 1, extlen%(14)),~
               at (20,56), fac(fac$(30)), str(to$  (15), 1, extlen%(15)),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L44450
                 if instr$ = " " then L40800
                 call "MANUAL" (str(instr$,1,8))
                 goto L40800

L44450:        if keyhit% <> 15 then end
                  call "PRNTSCRN"
                  goto L40800

