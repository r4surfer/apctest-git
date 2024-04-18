        REM *************************************************************~
            *                                                           *~
            *   SSS   L       CCC   TTTTT  TTTTT  EEEEE   SSS   TTTTT   *~
            *  S      L      C   C    T      T    E      S        T     *~
            *   SSS   L      C        T      T    EEEE    SSS     T     *~
            *      S  L      C   C    T      T    E          S    T     *~
            *   SSS   LLLLL   CCC     T      T    EEEEE   SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SLCTTEST - TESTS THE RANGE PARAMETERS IN THE SELECT LISTS *~
            *            FOR VALIDITY, RETURNING ERRORMSG$ NONBLANK IF  *~
            *            BAD, AND NORMALIZING THE VALUES, RETURNING     *~
            *            BLANK ERRORMSG$ IF O.K.  THIS IS PART OF       *~
            *            THE GENERALIZED ***SUPER SELECTOR***           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/17/81 ! ORIGINAL                                 ! BCW *~
            * 06/21/82 ! ADDED "S" (SINGLE PARAMETER) FORMAT      ! ECR *~
            * 12/14/82 ! FIX BRANCH @ 10700                       ! ECR *~
            * 06/25/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

            sub "SLCTTEST" (errormsg$, maxfields%)

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
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            from$25,                     /* TEMP FROM FIELD VALUE      */~
            to$25                        /* TO FIELD TEMP VALUE        */~

        rem *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto cms_start
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        rem *************************************************************
cms_start
            gosub L50000
                  if errormsg$ <> " " then end

            REM ROUTINE TO NORMALIZE KEYS FOR SELECTION.
                maxfields% = 0
                for temp% = 1 to 15
                    if prompt$(temp%) = " " then end
                    if from$(temp%) = " "   then L10840
                    if from$(temp%) = "ALL" then L10840
                       maxfields% = maxfields% + 1           /* PUSH IT*/
                       field%(maxfields%) = temp%
                       if format$(temp%) = "D" then L10670
                       if format$(temp%) = "N" then L10770
                       if format$(temp%) = "S" then L10840
                    REM ROUTINE TO NORMALIZE FOR ALPHANUMERIC INFORMATION
                        if from$(temp%) <> "FIRST" then L10620
                           init(hex(00)) from$(temp%)
L10620:                 if to$(temp%) <> "LAST" then L10640
                           init(hex(ff)) to$(temp%)
L10640:                 if from$(temp%) <> "FIRST" and to$(temp%) = " "  ~
                           then to$(temp%) = from$(temp%)
                        goto L10840
L10670:             REM ROUTINE TO NORMALIZE FOR DATE INFORMATION
                        if from$(temp%) <> "FIRST" then L10700
                           from$(temp%) = "19000101"
                           call "DATECONV" (from$(temp%))
L10700:                 if to$  (temp%) <> "LAST" then L10720
                           to$  (temp%) = "20991231"
                           call "DATECONV" (to$  (temp%))
L10720:                 if from$(temp%) <> "FIRST" and to$(temp%) = " "  ~
                           then to$(temp%) = from$(temp%)
                        call "DATUFMTC" (str(from$(temp%),1%,extlen%(temp%)))
                        call "DATUFMTC" (str(to$  (temp%),1%,extlen%(temp%)))
                        goto L10840
L10770:             REM ROUTINE TO NORMALIZE FOR NUMERIC INFORMATION
                        if from$(temp%) <> "FIRST" and to$(temp%) = " "  ~
                           then to$(temp%) = from$(temp%)
                        if from$(temp%) <> "FIRST"                       ~
                           then convert from$(temp%) to fromnr(temp%)
                        if to$  (temp%) <> "LAST"                        ~
                           then convert to$  (temp%) to tonr(temp%)
L10840:             next temp%
            end

L50000: REM *************************************************************~
            *         T E S T   S O R T   S C R E E N   D A T A         *~
            *                                                           *~
            * TESTS THE SORT SCREEN DATA FOR VALIDITY.  MAKES SURE THAT *~
            * THERE ARE EXACTLY BETWEEN 1 AND 3 KEY NUMBERS ENTERED,    *~
            * AND THAT THEY ARE IN THE RIGHT ORDER.  MAKE SURE THAT ALL *~
            * THE SORT KEYS ARE ALPHABETICALLY CORRECT.                 *~
            *************************************************************

            errormsg$ = " "

            for temp% = 1 to 15
                if prompt$(temp%) = " " then return
                   if from$(temp%) = "ALL" then L50095
                      if format$(temp%) = "U" then gosub L50200
                      if format$(temp%) = "L" then gosub L50200
                      if format$(temp%) = "D" then gosub L50300
                      if format$(temp%) = "N" then gosub L50400
                      if format$(temp%) = "S" then gosub L50500
                if errormsg$ <> " " then return
L50095:         next temp%
                return

L50200:     REM TEST DATA FOR FIELDS THAT HAVE ALPHA INFORMATION.
                if from$(temp%) <> "FIRST" or to$(temp%) <> " "          ~
                   then L50230
                   errormsg$ = "No End Parameter Specified For 'FIRST' Pa~
        ~rameter In " & prompt$(temp%) & "!!"
                   return
L50230:         if from$(temp%) = "FIRST" or to$(temp%) = "LAST"         ~
                   then return
                if to$(temp%) = " " then return
                if from$(temp%) <= to$(temp%) then return
                   errormsg$ = "'FROM' Field Must Be Less Than 'TO' Field~
        ~ In " & prompt$(temp%) & "!!"
                   return
L50300:     REM TEST DATA FOR FIELDS THAT HAVE DATE INFORMATION.
                call "SPCSMASH" (from$(temp%))
                call "SPCSMASH" (to$  (temp%))
                if from$(temp%) <> "FIRST" or to$(temp%) <> " "          ~
                   then L50324
                   errormsg$ = "No End Parameter Specified For 'FIRST' Pa~
        ~rameter In " & prompt$(temp%) & "!!"
                   return
L50324:         if from$(temp%) = "FIRST" then L50345
                   call "DATEOKC" (str(from$(temp%),1%,extlen%(temp%)), date%, errormsg$)
                   if errormsg$ = " " then L50345
                      errormsg$ = "Illegal Entry For 'FROM' Field Of "   ~
                                         & prompt$(temp%) & ": "         ~
                                         & to$(temp%)
                      return
L50345:         if to$  (temp%) = "LAST"  then return
                if to$  (temp%) = " "     then return
                   call "DATEOKC" (str(to$  (temp%),1%,extlen%(temp%)), date%, errormsg$)
                   if errormsg$ = " " then L50366
                      errormsg$ = "Illegal Entry For 'TO' Field Of "     ~
                                         & prompt$(temp%) & ": "         ~
                                         & to$(temp%)
                      return
L50366:         REM HANDLE TO MAKE SURE FIRST < LAST.
                    if from$(temp%) = "FIRST" then return
                    if to$  (temp%) = "LAST"  then return
                    from$ = from$(temp%)
                    call "DATUFMTC" (str(from$,1%,extlen%(temp%)))
                    to$   = to$  (temp%)
                    call "DATUFMTC" (str(to$  ,1%,extlen%(temp%)))
                    if from$ <= to$ then return
                       errormsg$ = "'FROM' Date Must Be Less Than 'TO' Da~
        ~te In " & prompt$(temp%) & "!!"
                       return
L50400:     REM TEST DATA FOR FIELDS THAT HAVE NUMERIC INFORMATION.
                call "SPCSMASH" (from$(temp%))
                call "SPCSMASH" (to$  (temp%))
                if from$(temp%) <> "FIRST" or to$(temp%) <> " "          ~
                   then L50424
                   errormsg$ = "No End Parameter Specified For 'FIRST' Pa~
        ~rameter In " & prompt$(temp%) & "!!"
                   return
L50424:         if from$(temp%) = "FIRST" then L50445
                   call "NUMVALID" (from$(temp%), err%, .5)
                   if err% = 0 then L50445
                      errormsg$ = "Illegal Entry For 'FROM' Field Of "   ~
                                         & prompt$(temp%) & ": "         ~
                                         & to$(temp%)
                      return
L50445:         if to$  (temp%) = "LAST"  then return
                if to$  (temp%) = " "     then return
                   call "NUMVALID" (to$(temp%), err%, .5)
                   if err% = 0 then L50469
                      errormsg$ = "Illegal Entry For 'TO' Field Of "     ~
                                         & prompt$(temp%) & ": "         ~
                                         & to$(temp%)
                      return
L50469:         REM HANDLE TO MAKE SURE FIRST < LAST.
                    if from$(temp%) = "FIRST" then return
                    if to$  (temp%) = "LAST"  then return
                    convert from$(temp%) to fromnr
                    convert to$(temp%) to tonr
                    if fromnr <= tonr then return
                       errormsg$ = "'FROM' Number Must Be Less Than 'TO' ~
        ~Number In " & prompt$(temp%) & "!!"
                       return

L50500:     REM TEST DATA FOR SINGLE PARAMTER ("S") FORMAT
                if from$(temp%) <> " " then return
                   errormsg$ = "No parameter specified for "             ~
                               & prompt$(temp%) & "!!"
                   return

