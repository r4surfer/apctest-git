        REM *************************************************************~
            *                                                           *~
            *   SSS   L       CCC   TTTTT   SSS    OOO   RRRR   TTTTT   *~
            *  S      L      C   C    T    S      O   O  R   R    T     *~
            *   SSS   L      C        T     SSS   O   O  RRRR     T     *~
            *      S  L      C   C    T        S  O   O  R   R    T     *~
            *   SSS   LLLLL   CCC     T     SSS    OOO   R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SLCTSORT - PART OF THE ***SUPER SELECTOR*** GENERALIZED   *~
            *            CODE, THIS ROUTINE CLOSES THE SORT WORK FILE,  *~
            *            SORTS IT WITH A KEY FROM 1 TO THE ARGUMENT,    *~
            *            REPLACING THE WORK FILE, AND OPENS THE FILE    *~
            *            AGAIN.                                         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/17/81 ! ORIGINAL                                 ! BCW *~
            * 03/19/92 ! Use 'SORTCALL' instead of 'LINK' to SORT ! KAB *~
            *          ! Tell someone if sort fails, other than   !     *~
            *          !    for zero records                      !     *~
            *          ! Will Now be able to  sort 'PRINTER' type !     *~
            *          !    files.                                !     *~
            *************************************************************

            sub "SLCTSORT" (#9, keylen%)

        dim                                                              ~
            sort$120,                    /* SORTCALL Parameter         */~
            workfile$8,                  /* WORK FILE FILE NAME        */~
            worklib$8,                   /* WORK FILE LIBRARY NAME     */~
            workvol$6                    /* WORK FILE VOLUME NAME      */~

        REM *************************************************************~
            *         M A K E   A C T U A L   S O R T   C A L L         *~
            *                                                           *~
            * DOES ALL THE PUTPARMS NECESSARY TO DO THE CALL TO THE     *~
            * SYSTEM SORT ROUTINE.  REOPEN THE KEY FILE IN INPUT MODE.  *~
            * THEN, READ EACH ENTRY IN THE KEY FILE AND CALL THE PRINT  *~
            * ROUTINE FOR IT.  AT END OF THE FILE, DELETE THE THING.    *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L12140
            cms2v$ = "R6.01.04 05/19/92 UNIX Compatibility Changes      "
L12140: REM *************************************************************
            convert keylen% to str(keylen$,,3), pic(000)

            call "GETNAMES" addr (#9, workfile$, worklib$, workvol$)
            close #9

            init (" ") sort$
            str(sort$,  1, 8) = str(workfile$,  1, 8)  /* In File      */
            str(sort$,  9, 8) = str(worklib$ ,  1, 8)  /* In Library   */
            str(sort$, 17, 6) = str(workvol$ ,  1, 6)  /* In volume    */
            str(sort$, 23,22) = str(sort$    ,  1, 22) /* Out = In     */
            str(sort$, 45, 4) = "0001"             /* Key Position     */
            str(sort$, 49, 3) = str(keylen$,,3)    /* Key Length       */
            str(sort$, 52, 2) = "CA"               /* Char / Ascending */

            call "SORTCALL" addr(sort$, ret%)
                if ret%  = 0% then L12330      /* A - OK Fine           */
                if ret% <> 4% then L12440      /* Empty, Caller's Job   */

L12330: REM Reopen the Work File for Input.
                call "PUTNAMES" addr (#9, workfile$, worklib$, workvol$)
                call "WORKOPN2" (#9, "INPUT", 0%, f2%) : f2% = 0%
            end

L12440: REM Sort Failed - Tell Somebody at least.

            put str(sort$, 55, 7) using L12470, ret%
L12470:                                 %Rtn ###

            ret% = 1%
            call "ASKUSER" (ret%, "**** SORT ERROR ****",                ~
                            "Sort Routine Failure - Parameters:",        ~
                            str(sort$, 1, 62),                           ~
                            "Press Any PF Key to Continue")

            goto L12330

            end
