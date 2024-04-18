        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  TTTTT  X   X  TTTTT  PPPP   RRRR   IIIII  N   N  TTTTT   *~
            *    T     X X     T    P   P  R   R    I    NN  N    T     *~
            *    T      X      T    PPPP   RRRR     I    N N N    T     *~
            *    T     X X     T    P      R   R    I    N  NN    T     *~
            *    T    X   X    T    P      R   R  IIIII  N   N    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TXTPRINT - Routine to aid in the printing of text.        *~
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
            * 09/18/85 ! ORIGINAL                                 ! ERN *~
            * 10/21/86 ! Redid schema of determining what prints  ! ERN *~
            * 06/09/87 ! Fixed Line count logic, added TYPEHDR$=T ! HES *~
            * 09/06/89 ! Added return to status codes.            ! JDH *~
            * 03/23/92 ! Rehost.  Text file may allow 1 - 28 lines! KAB *~
            *          !   of text per record.  Monitors record   !     *~
            *          !   length to determine how many.          !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "TXTPRINT"   (#1, f201%,     /* TXTFILE channel and status */~
                          prt%,          /* Printer Line Width         */~
                          textid$,       /* Text Id to print           */~
                          flags$,        /* Report ID or blank         */~
                          horiz%,        /* Pos to start print at      */~
                          line%,         /* Current Line Number        */~
                          maxlines%,     /* Max Lines per page         */~
                          typehdr$,      /* Print Type Descr (Y/N)     */~
                          linemask$,     /* Mask for Print Overlay     */~
                          status%   )    /* Incomming & Return Status  */~

*        TEXTID$ indicates which Text is to be printed.  Must be
*          dimensioned as CH(4).
*
*        FLAGS$ is the Report ID (specifies which types to print).
*          If left blank, all types will be printed.
*
*        HORIZ% indicates the print position to start printing the
*          text.
*
*        LINE% is the line counter.
*
*        MAXLINES% indicates the number of lines that may be printed
*          on a page before a new page is required.
*
*        TYPEHDR$ is set to 'Y' if the Type Description should be
*          printed as a header to each type of text printed.
*          set to 'T' to remove forced blank line in front of text
*
*        STATUS%- IN   0%- First call for this text.
*                      1%- Continuation call for this text.
*                      2%- Continuation call for this text.
*                      3%- Continuation call for this text.
*
*                 OUT  0%- Text printing completed.
*                      1%- Page Break required.
*                      2%- Page Break required.
*                      3%- Page Break required.
*
*               1% - 3%  Denote different departure and return positions

        dim                                                              ~
            filler64$64,                 /* Record Header Area         */~
            flags$,                      /* Print Flags to Print       */~
            linemask$132,                /* Mask for Print Line Format */~
            printline$132,               /* Print Line Itself          */~
            plowkey1$20,                 /* Plow Key                   */~
            plowkey2$20,                 /* Plow Key                   */~
            readkey$20,                  /* Read Key                   */~
            rptid$6,                     /* Last Rpt ID read in        */~
            s%(1),                       /* Receiver for SEARCH        */~
            srce$3,                      /* Source of text             */~
            srces$30,                    /* Sources Defined for Report */~
            text$(28)70,                 /* Text                       */~
            textid$4,                    /* Text ID of text to print   */~
            type$1,                      /* Text Type                  */~
            types$(10)10,                /* Types to print on report   */~
            typedescr$25,                /* Text Type Description      */~
            typehdr$1                    /* Print Type Descriptions Y/N*/

        dim f1%(01)                      /* = 1 if READ was successful */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.04 05/19/92 UNIX Compatibility Changes      "
        REM *************************************************************


                     /* The variables F2%() and AXD$() should not be   */
                     /* modified.   They are an intrinsic part of the  */
                     /* file opening routines.                         */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! TXTFILE  ! System Text File                         *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

*        Open TXTFILE.

          if openstatus% = 0% then                                       ~
             call "OPENCHCK" (#1, openstatus%, f201%, 0%, " ")
          if openstatus% < 0% then L65000
             call "GETUFBRS" addr(#1, filler64$)
             textsize% = val(str(filler64$,,2),2)
             textsize% = textsize% - 64%
             textincr% = textsize% / 70%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

*        Set printing parameters and test caller's arguments regarding
            printline% = min(132%, prt% - 2%)
            linemax%   = min(70%, printline% - horiz%)
            headmax%   = min(28%, printline% - horiz%)
               if linemax%   < 1% then L65000
               if headmax%   < 1% then L65000
               if printline% < 1% then L65000

*        See if we're in the middle of some text already
            if status% = 1% then return_from_caller_1
            if status% = 2% then return_from_caller_2
            if status% = 3% then return_from_caller_3
                select printer(prt%)


        REM *************************************************************~
            * M A I N   D R I V E R   L O G I C                         *~
            * --------------------------------------------------------- *~
            * Loop through text by type and print the text types that   *~
            * are specified for the report.                             *~
            *************************************************************

*        Initialize to start plow at the first Type for this text
            plowkey1$ = "M   " & str(textid$) & hex(0000)
            goto L10140

*        Loop- Get next type for text and see if we should print it.
        next_type
            str(plowkey1$,10,2) = hex(ffff)
L10140:     call "PLOWNEXT" (#1, plowkey1$, 8%, f1%(1))
            if f1%(1) = 0% then L65200    /* Done Done Done   */

            get #1 using L10180, type$, srce$, textlines%
L10180:         FMT XX(8), CH(1), XX(2), CH(3), BI(2)
            if srce$ = "CPY" then type$ = "1"

          /* Print this Type???                              */
            if textlines% = 0% then next_type
            gosub test_type : if textlines% = 0% then next_type


          /* Print it- but first a moment from our sponsor   */
            if srce$ <> "CPY" then L10290
                typedescr$ = "COPY ELEMENT TEXT"
                goto L10350
L10290:     readkey$ = "T" & str(srce$) & type$   /* Get Type Descr */
            typedescr$ = "TYPE NO LONGER ON-FILE"
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 1% then get #1 using L10330, typedescr$
L10330:         FMT POS(12), CH(25)

L10350
*        This routine prints the text
            plowkey2$ = str(plowkey1$,,9) & hex(0000)
            textline% = 1%

            if typehdr$ = "T" then print_loop
            if line% >= maxlines% then L65260
                               /* Exit to caller to do page heading    */
        return_from_caller_3
            str(printline$,1%,printline%) = linemask$
            print str(printline$,1%,printline%)
            line% = line% + 1%

            if typehdr$ <> "Y" then print_loop
            if line% >= maxlines% then L65240
                               /* Exit to caller to do page heading    */
        return_from_caller_2
                str(printline$,1%,printline%) = linemask$
                str(printline$,horiz%, headmax%) = "** " & typedescr$
                print str(printline$,1%,printline%)
                line% = line% + 1%

        print_loop
            call "PLOWNEXT" (#1, plowkey2$, 9%, f1%(1))
            if f1%(1) = 0% then next_type
            get #1, str(filler64$), str(text$(),,textsize%)
                FMT XX(64), 28*CH(70)
            elem% = 1%
L10550:     if line% >= maxlines% then L65220
                               /* Exit to caller to do page heading    */
        return_from_caller_1
            str(printline$,1%,printline%) = linemask$
            str(printline$,horiz%, linemax%) = text$(elem%)
            print str(printline$,1%,printline%)

            line%     = line%     + 1%
            textline% = textline% + 1%
            elem%     = elem%     + 1%
            if textline% > textlines% then next_type
            if elem%     > textincr%  then print_loop else L10550


*       ****************************************************************

        test_type  /* See if this type is to print on this report      */
                   /* If it doesn't, we set # of lines to zero         */
            if flags$      = " " then return  /* Print All Types */
            if len(flags$) <> 6% then return

            if rptid$ = flags$ then L11310
                readkey$ = "X" & str(flags$,,6) & hex(00000000)
                rptid$ = str(flags$,,6)
                call "PLOWNEXT" (#1, readkey$, 7%, f1%(1))
                if f1%(1) = 1% then L11140
                     rptid$ = " "        /* Print if not on file       */
                     return
L11140:         if str(readkey$,10,1) = hex(00) then L11200
                   /* OLD FORMAT */
                get #1 using L11170, srces$, str(types$(),,100)
L11170:             FMT POS(42), CH(30), CH(100)
                goto L11310

L11200:         if textsize% < 130% then L11250
                get #1 using L11220, srces$, str(types$(),,100)
L11220:             FMT POS(65), CH(30), CH(100)
                goto L11310

L11250:         get #1 using L11260, str(srces$,,15), str(types$(),,50)
L11260:             FMT POS(65), CH(15), CH(50)
                init (" ") str(srces$,16), str(types$(),51)
                call "PLOWNEXT" (#1, readkey$, 7%, f1%(1))
                   if f1%(1) = 0% then L11310
                get #1 using L11260, str(srces$,16,15), str(types$(),51,50)
L11310:     search srces$ = str(srce$,,3) to s%() step 3
            if s%(1) = 0% then dont_print
            s% = (s%(1) + 2%) / 3%
            if pos(str(types$(s%)) = type$) <> 0% then return

          dont_print
            textlines% = 0%
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

L65200:     status% = 0%  :  end         /* Printing Completed         */

L65220:     status% = 1%  :  end         /* Not done Yet               */

L65240:     status% = 2%  :  end         /* Not done Yet               */

L65260:     status% = 3%  :  end         /* Not done Yet               */

