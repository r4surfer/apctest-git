        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  TTTTT  X   X  TTTTT  FFFFF  U   U  TTTTT  IIIII  L       *~
            *    T     X X     T    F      U   U    T      I    L       *~
            *    T      X      T    FFF    U   U    T      I    L       *~
            *    T     X X     T    F      U   U    T      I    L       *~
            *    T    X   X    T    F       UUU     T    IIIII  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TXTFUTIL - SYSTEM TEXT FILE UTILITIES.                    *~
            *            This routine handles various file manipulation *~
            *            functions for the file TXTFILE. See below for  *~
            *            a list of functions and notes regarding each.  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/12/85 ! ORIGINAL                                 ! RN2 *~
            * 03/23/92 ! Rehost.  Text file may allow 1 - 28 lines! KAB *~
            *          !   of text per record.  Monitors record   !     *~
            *          !   length to determine how many.          !     *~
            * 07/14/93 ! COPY, WCPY not working on UNIX,          ! KAB *~
            *          !   Change READ100 to PLOWALT (VS loophole)!     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "TXTFUTIL"   (#1, f201%,     /* Channel, status of TXTFILE */~
                          func$,         /* Command (see below)        */~
                          textid$)       /* ID of text to Toss         */~


*        FUNC$ tells the subroutine what is it is expected to do.
*              Possible functions are listed below.  Based on function,
*              TEXT$ may not be required.
*
*                                                                 TEXT$
*        INTL  Clears work area for current User (Initializes).    NO
*
*        LOAD  Copies text from Buffer/Master to Work area.        YES
*
*        XOUT  Resets all text records for the TEXT specified      YES
*              to #-lines = 0.
*
*        SAVE  Tosses text from the work area for the current      NO
*              user to the buffer area.  Clears the work area.
*
*        SAV2  Same as SAVE except moves text directly to Master.  NO
*
*        DELE  Deletes the specified TEXT from the Master          YES
*
*        TOSS  Moves the Text specified from the Buffer to         YES
*              the Master. The contents are also removed
*              from the buffer.
*
*        TOS2  Moves the Text from the Work area to the Master.    YES
*              Contents of Work area are removed.
*
*        COPY  Copies Text ID specified from Master to a new       YES
*              Text ID in the Work Area.  TEXT$ is returned as
*              the new ID (or all $FF$ if no text in Master).
*              (You're welcome Kenny).
*
*        WCPY  Copies Text ID specified from Work Area to a new    YES
*              Text ID in the Work Area.  TEXT$ is returned as
*              the new ID (or all $FF$ if no text in Work Area).
*              Original text is deleted from work area.
*

        dim                                                              ~
            filler64$64,                 /* Filler and Header Area     */~
            from$8,                      /* File ID, User, Text ID.    */~
            func$4,                      /* Function to be executed    */~
            plowkey$25,                  /* Plow key                   */~
            plowkey2$25,                 /* Another Plowkey            */~
            plowkey3$25,                 /* Yet another Plowkey        */~
            readkey$100,                 /* Read Key Variable          */~
            text$4,                      /* Text ID                    */~
            textid$4,                    /* Text ID                    */~
            to$8,                        /* File ID, User, Text ID.    */~
            type$1,                      /* Text Type Code             */~
            user$3,                      /* He who the text tosses for */~
            work$(28)70                  /* Work Area                  */

        dim f1%(01)                      /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! TXTFILE  ! System Text File                         *~
            *************************************************************~

            text$ = textid$

*        Open TXTFILE.

          if openstatus% = 0% then                                       ~
             call "OPENCHCK" (#1, openstatus%, f201%, 0%, " ")
          if openstatus% < 0% then L65000
             call "GETUFBRS" addr(#1, readkey$)
             textsize% = val(str(readkey$,,2),2)
             textsize% = textsize% - 64%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", user$)

            delete%, sav2% = 0%

        REM *************************************************************~
            *           F U N C T I O N    D R I V E R S                *~
            *************************************************************

            if func$ = "INTL" then intl
            if func$ = "LOAD" then load
            if func$ = "XOUT" then xout
            if func$ = "SAVE" then save
            if func$ = "SAV2" then sav2
            if func$ = "DELE" then dele
            if func$ = "TOSS" then toss
            if func$ = "TOS2" then tos2
            if func$ = "COPY" then tcpy
            if func$ = "WCPY" then wcpy
            goto L65000

        intl
*        Clear any "W" records for this user.
            plowkey$ = "W" & str(user$)
            call "DELETE" (#1, plowkey$, 4%)
            goto L65000


        load
*        Copy the 'latest' version of the text into the work area.
            plowkey$ = "B   " & str(text$) & hex(000000)
L11030:     call "PLOWNEXT" (#1, plowkey$, 8%, f1%(1))
            if f1%(1) = 1% then L11080
                if str(plowkey$,,1) = "M" then L65000  /* Not on file   */
                str(plowkey$,,1)    = "M"  : goto L11030

L11080:     from$   = str(plowkey$,,8)   /* File ID, bbb, Text ID      */
            to$     = "W" & str(user$) & str(text$)
            delete% = 0%
            gosub moveit
            goto L65000


        xout    /* Performs logical delete in Work area by setting     */
                /* #-of-lines equal to zero.                           */
            plowkey$ = "W" & str(user$) & str(text$) & hex(000000)
L11520:     call "PLOWNXT1" (#1, plowkey$, 8%, f1%(1))
            if f1%(1) = 0% then L65000
                if str(plowkey$,10,2) <> hex(0001) then L11600
                     put #1 using L11560, 0%
L11560:                   FMT POS(15), BI(2)
                     rewrite #1
                     goto L11520

L11600:         delete #1      /* Delete if other than first record    */
                goto L11520     /* for each type.                       */


        save    /* Toss User's Work area to the Buffer area.           */
            init(hex(00)) plowkey3$
            str(plowkey3$,1,4) = "W" & str(user$)

L12030:     call "PLOWNEXT" (#1, plowkey3$, 4%, f1%(1))
            if f1%(1) = 0% then L65000
                get #1 using L12060, text$
L12060:              FMT XX(4), CH(4)
                from$    = "W" & str(user$) & str(text$)
                to$      = "B" & "   "      & str(text$)
                if sav2% = 1% then str(to$,,1) = "M"
                delete%  = 1%
                gosub moveit
                goto L12030


        sav2    /* Toss User's Work area directly to Master area.      */
            sav2% = 1%
            goto save

        dele    /* Physically remove TEXT from Master area.            */
            plowkey$ = "M   " & str(text$)
            call "DELETE" (#1, plowkey$, 8%)
            goto L65000


        toss    /* Move text from Buffer to Master area                */
            from$   = "B   " & str(text$)
            to$     = "M   " & str(text$)
            delete% = 1%
            gosub moveit
            goto L65000


        tos2    /* Move text from  Work Area to Master area            */
            from$   = "W" & str(user$) & str(text$)
            to$     = "M" & "   "      & str(text$)
            delete% = 1%
            gosub moveit
            goto L65000


        tcpy    /* Copy Text in Master to a new ID in the work area    */
            from$   = "M" & "   "      & str(text$)
            plowkey$ = str(from$) & hex(000000)
            call "PLOWNEXT" (#1, plowkey$, 8%, f1%(1))
            if f1%(1) = 1% then L12780
                text$ = all(hex(ff))     /* No text in Master  */
                goto L65000
L12780:     gosub next_id
            /* Now Copy the text to the new Text ID          */
                to$     = "W" & str(user$) & str(text$)
                delete% = 0%
                gosub moveit
                goto L65000

        wcpy    /* Copy Text in work area to a new ID in the work area */
            from$   = "W" & str(user$) & str(text$)
            plowkey$ = str(from$) & hex(000000)
            call "PLOWNEXT" (#1, plowkey$, 8%, f1%(1))
            if f1%(1) = 1% then L12910
                text$ = all(hex(ff))     /* No text in Work Area  */
                goto L65000
L12910:     gosub next_id
            /* Now Copy the text to the new Text ID          */
                to$     = "W" & str(user$) & str(text$)
                delete% = 1%
                gosub moveit
                goto L65000

        next_id
            /* Get Next Text ID                              */
                readkey$ = "N"
L13030:         call "READ101" (#1, readkey$, f1%(1))
                if f1%(1) = 1% then L13100
                     put filler64$ using L13060, "N", 1%, " "
L13060:                  FMT CH(11), BI(4), CH(49)
                     write #1, str(filler64$,,64),                       ~
                               str(work$(),,textsize%), eod goto L13030
                     goto L13030
L13100:         get #1 using L13110, nextid%
L13110:              FMT XX(11), BI(4)
L13120:         text$   = bin(nextid%, 4)
                nextid% = nextid% + 1%
                if str(text$) = hex(00000000) then L13120
                if str(text$) = hex(20202020) then L13120
                if str(text$) = hex(ffffffff) then L13120
                put #1 using L13180, nextid%
L13180:              FMT POS(12), BI(4)
                rewrite #1
                return

        REM *************************************************************~
            *   S U B R O U T I N E S                                   *~
            *************************************************************


        moveit  /* Move text indicated by FROM to area specified in TO */
*        Loop thru to get a type at a time.
        plowkey$ = str(from$) & hex(000000)
L20030: call "PLOWNEXT" (#1, plowkey$, 8%, f1%(1))
        if f1%(1) = 0% then return
            type$ = str(plowkey$,9,1)
            gosub moveit2
            str(plowkey$,10) = hex(ffff)
            goto L20030   /* Go get next type. */


        moveit2 /* BY TYPE- Kill TO, toss FROM to TO, kill FROM (optnl)*/
            plowkey2$ = str(to$) & type$ & hex(0000)
            call "DELETE" (#1, plowkey2$, 9%)      /* Delete TO        */

            plowkey2$ = str(from$) & type$ & hex(0000)
L20150:     call "PLOWNEXT" (#1, plowkey2$, 9%, f1%(1))
            if f1%(1) = 0% then L20250
                get #1 using L20180, lines%
L20180:              FMT XX(14), BI(2)
                if lines% = 0% then L20250          /* Implies delete   */
                put #1 using L20210, str(to$,,8)    /* File ID, User    */
L20210:              FMT POS(1), CH(8)
                write #1
                goto L20150

L20250:     if delete% = 0% then return
            plowkey2$ = str(from$) & type$ & hex(0000)
            call "DELETE" (#1, plowkey2$, 9%)      /* Delete FROM      */
            return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            textid$ = text$
            end
