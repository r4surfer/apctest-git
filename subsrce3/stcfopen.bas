        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   FFFFF   OOO   PPPP   EEEEE  N   N   *~
            *  S        T    C   C  F      O   O  P   P  E      NN  N   *~
            *   SSS     T    C      FFFF   O   O  PPPP   EEEE   N N N   *~
            *      S    T    C   C  F      O   O  P      E      N  NN   *~
            *   SSS     T     CCC   F       OOO   P      EEEEE  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCFOPEN - Opens the files which comprise the Cost Set ID *~
            *            passed in.                                     *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/25/87 ! Original                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "STCFOPEN"   (setin$,        /* Cost Set to open files for */~
                                         /*   (blank = current set)    */~
                          modein$,       /* See Below                  */~
                          #2,            /* SYSFILE2 Channel           */~
                          errormsg$,     /* Returned w/ msg if open(s) */~
                                         /*   unsuccessful             */~
                          #10,           /* STCHNY   Channel           */~
                          #11,           /* STCDETAL Channel           */~
                          #12,           /* STCWCACT Channel           */~
                          #13,           /* STCLABOR Channel           */~
                          #14,           /* STCMAPNG Channel           */~
                          #15 )          /* STCCHNGS Channel           */

*        The program will first close any open files on channels
*          #10 - #15.  If the MODEIN$ is passed = 'C' then we are all
*          done.
*        Next the Internal ID for the cost set requested is determined.
*          If SET$ is passed in as blanks then the current cost set is
*          derived and used.  An error occurs if there is no current
*          cost set defined.
*        The files for the cost set are then opened per the requests in
*          MODEIN$.  An error msg is issued if any can not be opened.
*          MODEIN$ consists of six characters which correspond to the
*          file channels #10 - 15 and contain the file open mode for
*          the file: 'S' = Shared, R = Input, U = I/O, ' ' = don't open.
*          If just = 'C' then the files are just closed.
*        Note: SYSFILE2 must be opened by the caller!!!

        dim                                                              ~
            errormsg$79,                 /* Error message              */~
            mode$5,                      /* OPENFILE Mode              */~
            modein$6,                    /* Request Mode for opens     */~
            prname$8,                    /* Physical File Name         */~
            rslt$20,                     /* Text from file opening     */~
            set$8,                       /* The set to open files for  */~
            setid$4,                     /* Internal ID of SET$        */~
            setin$8,                     /* Set ID requested           */~
            suffixes$6                   /* File name suffixes         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * #10 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
            * #11 ! STCDETAL ! Standard Cost Details                    *~
            * #12 ! STCWCACT ! Standard Cost Work Center / Activities   *~
            * #13 ! STCLABOR ! Standard Costing Labor Standards         *~
            * #14 ! STCMAPNG ! Standard Cost Set Mappings               *~
            * #15 ! STCCHNGS ! Tickler file of changed components       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *************************************************************

            call "GETUFBS1" addr(#2, f2%)
            if f2% = 1% then L09130
                errormsg$ = "SYSFILE2 not open (program error)."
                end

L09130:     set$        = setin$
            errormsg$   = " "
            suffixes$   = "HDWLMC"

        REM *************************************************************~
            *        S U B R O U T I N E   L O G I C                    *~
            *************************************************************

            gosub close_files
            if modein$ = "C" then end

*        Determine Internal ID of set requested.
            f1% = 1%
            call "STCSETID" (f1%, #2, set$, setid$)
            if f1% > 0% then L10140
                errormsg$ = "Set not on file."
                end

L10140
*        Now set the File names and attempt to open the files.
            for f% = 10% to 15%
              if pos("SRU" = str(modein$,f%-9%,1)) = 0% then L10190
                prname$ = "STC" & str(setid$) & str(suffixes$,f%-9%,1)
                call "PUTPRNAM" addr(#f%, prname$)
L10190:     next f%

            for f% = 10% to 15%
              if pos("SRU" = str(modein$,f%-9%,1)) = 0% then L10320
                if str(modein$,f%-9%,1) = "S" then mode$ = "SHARE"
                if str(modein$,f%-9%,1) = "R" then mode$ = "INPUT"
                if str(modein$,f%-9%,1) = "U" then mode$ = "IO   "
                f2% = 1%
                call "OPENFILE" (#f%, mode$, f2%, rslt$, " ")
                if f2% = 0% then L10320
                     errormsg$ = "UNABLE TO OPEN COST SET. ("& rslt$ &")"
                     gosub close_files
                     end
L10320:     next f%
            end


        close_files
            for f% = 10% to 15%
                call "GETUFBS1" addr(#f%, f2%)
                if f2% = 1% then close #f%
            next f%
            return
