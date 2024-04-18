        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  EEEEE  N   N   AAA   BBBB   L       SSS   U   U  BBBB    *~
            *  E      NN  N  A   A  B   B  L      S      U   U  B   B   *~
            *  EEEE   N N N  AAAAA  BBBB   L       SSS   U   U  BBBB    *~
            *  E      N  NN  A   A  B   B  L          S  U   U  B   B   *~
            *  EEEEE  N   N  A   A  BBBB   LLLLL   SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ENABLSUB - This routine performs most of the tasks        *~
            *            related to setting and resetting 'soft coded'  *~
            *            Default/Enable switches.                       *~
            *            NOTE- Calling program should ensure that the   *~
            *             user attempting to modify settings is either  *~
            *             a module or data base administrator.          *~
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
            * 09/26/85 ! ORIGINAL                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ZENABLSU"   (function$,     /* Task to execute-           */~
                                         /* 'INIT', 'SET', 'MODIFY'    */~
                          program$,      /* Calling Program            */~
                          xref%(),       /* Screen/Field to Settings   */~
                          set%(),        /* Default/Enable settings    */~
                          scrn%,         /* Screen Number              */~
                          field%,        /* Field Number               */~
                          mode%,         /* 1- Input; 2- Edit          */~
                          enabled% )     /* Returned ENABLED% setting  */


*        Notes regarding program setting of enables
*
*         ! VALUE ! INPUT ! EDIT !
*         +-------+-------+------+
*         !   0   !   N   !   N  !   Add 10 to disallow user
*         !   1   !   N   !   Y  !   modification of setting
*         !   2   !   Y   !   Y  !
*         !   3   !   Y   !   N  !
*         +-------+-------+------+

        dim                                                              ~
            def%(255),                   /* Pgm Default Settings       */~
            first$2,                     /* First time in subrtn?      */~
            function$6,                  /* Function to execute        */~
            program$8,                   /* Calling program name       */~
            readkey$25,                  /* Read key Variable          */~
            set%(255),                   /* DEF/ENA settings           */~
            temp$12,                     /* Temporary string           */~
            text$60,                     /* Text for Modify            */~
            val$1,                       /* Value entered for switch   */~
            xref%(5,15)                  /* Field to Setting XREF      */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************
        if first$ = " " then                                             ~
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* It is an intrinsic part of the                 */
                     /* file opening routines.                         */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ENABLES  ! System Enables Settings                  *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "ENABLES",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =     1, keylen =  20


        if f2%(1) = 0% then L09000
            call "OPENCHCK" (#1, 0%, f2%(1), 100%, rslt$(1))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            first$ = "NO"

        REM *************************************************************~
            *        F U N C T I O N   D R I V E R S                    *~
            * --------------------------------------------------------- *~
            * Branch per function.                                      *~
            *************************************************************

            if function$ = "INIT"    then init_enables
            if function$ = "SET"     then set_enables
            if function$ = "MODIFY"  then modify_enables
                enabled% = 99%
                goto L65000


        REM *************************************************************~
            * I N I T I A L I Z E    E N A B L E    S W I T C H E S     *~
            * --------------------------------------------------------- *~
            * This function over-rides the program defaults with the    *~
            * copy found in ENABLES.   If no record is found, it is     *~
            * created here. Checking is done to see if fields have been *~
            * added or deleted.                                         *~
            *************************************************************
        init_enables
            mat def% = set%    /* DEF%() = Program settings,           */
                               /* SET%() = User settings               */
            readkey$ = "ENABLES:" & str(program$,,8)
            call "READ101" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then L11200
                get #1 using L11150, set%()
L11150:              FMT XX(20), 255*BI(1)
                for x% = 1% to 255%      /* Add and delete fields      */
                     if set%(x%) = 99% then set%(x%) = def%(x%)  /*Add */
                     if def%(x%) = 99% then set%(x%) = 99%       /*Del */
                next x%
L11200:     gosub write_enables
            goto L65000


        REM *************************************************************~
            * S E T   E N A B L E D   V A R I A B L E                   *~
            * --------------------------------------------------------- *~
            * This function sets the variable ENABLED% to either 0 if   *~
            * the field is disabled for input -or- 1 if the field is    *~
            * enabled.                                                  *~
            *************************************************************
        set_enables

            enabled% = 0%
                if scrn% <= 0% or field% <= 0% then L65000
                if mode% <  1% or mode%  >  2% then L65000
            val% = set%(xref%(scrn%, field%))   /* Field setting    */
            if val% >= 10% then val%  = val% - 10%
            if val% <   0% or   val%  > 3% then L65000 else L12150
L12150:     enabled% = 0%
            if mode% = 1% and (val% = 2% or val% = 3%) then enabled% = 1%
            if mode% = 2% and (val% = 1% or val% = 2%) then enabled% = 1%
            goto L65000


        REM *************************************************************~
            * M O D I F Y   D E F / E N A   S E T T I N G               *~
            * --------------------------------------------------------- *~
            * This function allows the User to redefine a 'soft'        *~
            * default/enable setting.  The User must be a Security      *~
            * Administrator to execute this function.                   *~
            *************************************************************
        modify_enables
            enabled%  =  99%
            if scrn% <=   0% or field% <= 0% then L65000
            enabled%  =   0%

            val% = set%(xref%(scrn%, field%))
            if val% >= 10% then L65000   /* Not User settable.          */
            convert val% to str(temp$,,1), pic(0)
            text$ = hex(84) &                                            ~
                "Settings (Input/Edit): 0-NN 1-NY 2-YY. Now = " &        ~
                str(temp$,,1)
L13180:     print at(24,2,79); text$;
            val$ = str(temp$,,1)
            input val$
            if val$ = " " then L65000
            if pos("012" = val$) = 0 then L13180
            convert str(val$) to val%
            set%(xref%(scrn%, field%)) = val%
            gosub write_enables
            goto L65000


        REM *************************************************************~
            * W R I T E  -  E N A B L E S                               *~
            * --------------------------------------------------------- *~
            * Write/Rewrite Default/Enable array to file ENABLES        *~
            *************************************************************
        write_enables

            readkey$ = "ENABLES:" & str(program$,,8)
            call "READ101" (#1, readkey$, f1%(1))
            put #1 using L18100, readkey$, set%(), " "
L18100:         FMT CH(20), 255*BI(1), CH(225)
            if f1%(1) = 0% then write #1  else  rewrite #1
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

            close ws

            end
