        REM *************************************************************~
            *                                                           *~
            *   SSS   L       CCC   TTTTT  PPPP    AAA    SSS    SSS    *~
            *  S      L      C   C    T    P   P  A   A  S      S       *~
            *   SSS   L      C        T    PPPP   AAAAA   SSS    SSS    *~
            *      S  L      C   C    T    P      A   A      S      S   *~
            *   SSS   LLLLL   CCC     T    P      A   A   SSS    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SLCTPASS - THIS ***SUPER SELECTOR*** ROUTINE LOOKS AT A   *~
            *            SET OF 3 RECORDS IN RECORD$() AND SEES IF THEY *~
            *            MEET THE CRITERIA FOR SELECTION.  IF NO, A ZERO*~
            *            SELECT% IS RETURNED.  IF YES, NON-ZERO SELECT% *~
            *            SO WE WRITE RECORD TO WORK FILE.               *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/17/81 ! ORIGINAL                                 ! BCW *~
            * 06/21/82 ! ADDED "S" (SINGLE PARAMETER) OPTION      ! ECR *~
            *************************************************************

            sub "SLCTPASS" (maxfields%, select%)

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
            field$25

            select% = 0

            if maxfields% = 0 then L11340
            for temp% = 1 to maxfields%
                field% = field%(temp%)
                field$ = str(record$(), 1000 * (record%(field%)-1)       ~
                                + position%(field%), length%(field%))
                if format$(field%) = "N" then L11130
                if format$(field%) = "S" then L11290

            REM ROUTINE TO SELECT ALPHABETIC COMPARISONS.
                if field$ < from$(field%) or field$ > to$(field%)        ~
                   then end                 /* DON'T PICK RECORD*/
                   go to L11320

L11130:     REM ROUTINE TO SELECT NUMERIC FLOATING POINT COMPARISONS
                get field$, using L11150, field
L11150:             FMT PD(14,4)
                if from$(field%) = "FIRST" then L11220
                   if to$(field%) = "LAST" then L11250
                      if field < fromnr(field%) or                       ~
                         field > tonr(field%)                            ~
                         then end
                      goto L11330
L11220:            REM HANDLES CASE FOR <= A NUMBER
                       if field < tonr(temp%) then end
                          goto L11330
L11250:            REM HANDLES CASE FOR >= A NUMBER
                       if field > fromnr(temp%) then end
                          goto L11320

L11290:     REM ROUTINE TO SELECT A SINGLE PARAMETER
                if field$ <> from$(field%) then end

L11320:     REM DROP THROUGH FOR NEXT FIELD IN RECORD.
L11330:         next temp%
L11340:         select% = 1
                end
