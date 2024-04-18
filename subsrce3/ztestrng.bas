        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  ZZZZZ TTTTT  EEEEE   SSS   TTTTT  RRRR   N   N   GGG     *~
            *     Z    T    E      S        T    R   R  NN  N  G        *~
            *    Z     T    EEEE    SSS     T    RRRR   N N N  G GGG    *~
            *   Z      T    E          S    T    R   R  N  NN  G   G    *~
            *  ZZZZZ   T    EEEEE   SSS     T    R   R  N   N   GGG     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ZTESTRNG - Tests range parameter variables.  Allowed      *~
            *            entry of ALL, FIRST, LAST or range.            *~
            *            Wildcard '?' at end of string denotes user's   *~
            *            desire to get a selection screen (GETCODE).    *~
            *            Wildcard is of no use if channel not passed in.*~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/16/86 ! Original                                 ! ERN *~
            * 07/09/92 ! If '?'(Wildcard) is the last character   ! JDH *~
            *          !   and channel was passed in, uses GETCODE!     *~
            * 11/16/92 ! Clone of old TESTRNGE w/Z added.         ! JDH *~
            * 04/11/94 ! Added GENCODES selection.                ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ZTESTRNG"   (dfrom$, dto$,  /* From/To for display        */~
                           from$,  to$,  /* From/To for plow           */~
                          errormsg$,     /* Error Message              */~
                          #1,            /* File for GETCODE/PLOWCODE  */~
                          gencodeskey$)  /* KEY to GENCODES            */

            dim key$79,                  /* Key for GETCODE            */~
                key_descr$79,            /* Key Description for GETCODE*/~
                wild_from$1,             /* Does From use wildcard?    */~
                wild_to$1                /* Does To use wildcard?      */

            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "

        REM *************************************************************~
            *             P R E F O R M   T E S T                       *~
            *-----------------------------------------------------------*~
            * Test the data we are here for to test.                    *~
            *************************************************************

            call "NARGS" addr(args%) /* # of arguments passed in.  If */
                                     /* 6 then channel passed         */
                                     /* for GETCODE selection screen. */
                                     /* 7 then GENCODEs KEY passed    */
                                     /* for PLOWCODE selection screen.*/

            from$, to$, errormsg$ = " "
            wild_from$, wild_to$ = "N"

            if args% = 5% then L10080
                if str(dfrom$,len(dfrom$),1%) <> "?" then L10072
                    wild_from$ = "Y"
                    str(dfrom$,len(dfrom$),1%) = hex(00)
L10072:         if str(dto$,len(dto$),1%) <> "?" then L10080
                    wild_to$ = "Y"
                    str(dto$,len(dto$),1%) = hex(00)

L10080:     if wild_from$ = "Y" then L10150
            if wild_to$   = "Y" then L10120
            if dfrom$ = " " and dto$ = " " then dfrom$ = "ALL"
            if dto$   = "ALL"   then dfrom$ = "ALL"
L10120:     if dfrom$ = "ALL"   then dto$   = " "
            if dfrom$ = "ALL"   then L10200
            if dfrom$ = "FIRST" then L10190
            if wild_to$ = "Y" then L10155
            if dto$   = " "     then dto$   = dfrom$
L10150:     if dto$   = "LAST"  then L10190
L10155:     gosub search_for_wildcard
            if dfrom$ <= dto$  then L10200
                errormsg$ = "FROM must be less than or equal to TO."
                end
L10190:     gosub search_for_wildcard
            if dfrom$ = " " and dto$ = " " then dfrom$ = "ALL"
L10200:     from$ = dfrom$  :  to$ = dto$
            if from$ <> "ALL" and from$ <> "FIRST" then L10240
                from$ = all(hex(00))
                goto L10250
L10240:     from$ = addc all(hex(ff))
L10250:     if dfrom$ = "ALL" or dto$ = "LAST" then to$ = all(hex(ff))

            end

        search_for_wildcard
*        Was channel passed in?
            if args% = 5% then return
*        Is the file open?
            call "GETUFBS1" addr (#1, f1%)
            if f1% = 0% then return

*        Is the file HNYMASTR?
            call "GETPRNAM" addr (#1, file$)
            if file$ = "HNYMASTR" then length = .32 else length = .30

*        Search FROM for wildcard
            if wild_from$ <> "Y" then L10440
                key$ = dfrom$
                key_descr$ = hex(0684) & "Select Starting Point for Range"
                gosub select_code
                if f1% <> 0% then L10425
                     str(dfrom$,len(dfrom$),1%) = "?" : goto L10430
L10425:         dfrom$ = key$
L10430:         if dto$ = " " and wild_to$ <> "Y" then dto$ = dfrom$

L10440
*        Search TO for wildcard
            if wild_to$ <> "Y" then return
                key$ = dto$
                key_descr$ = hex(0684) & "Select Ending Point for Range"
                gosub select_code
                if f1% <> 0% then L10485
                     str(dto$,len(dto$),1%) = "?" : goto L10490
L10485:         dto$ = key$
L10490:         return

        select_code
            if args% = 7% then L10590
                call "ZGETCODE" (#1, key$, key_descr$, 0%, length, f1%)
L10535:         if f1% = 0% then errormsg$ = "Invalid code."
                return
L10590:     key$ = str(gencodeskey$,,9%) & key$
            call "ZPLOWCOD" (#1, key$, key_descr$, 9%, .3, f1%)
            key$ = str(key$,10%)
            goto L10535

