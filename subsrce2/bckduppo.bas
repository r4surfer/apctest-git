        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  BBBB    CCC   K   K  DDDD   U   U  PPPP   PPPP    OOO    *~
            *  B   B  C   C  K  K   D   D  U   U  P   P  P   P  O   O   *~
            *  BBBB   C      KKK    D   D  U   U  PPPP   PPPP   O   O   *~
            *  B   B  C   C  K  K   D   D  U   U  P      P      O   O   *~
            *  BBBB    CCC   K   K  DDDD    UUU   P      P       OOO    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKDUPPO - LITTLE ROUTINE TO CHECK FOR DUPLICATE PO       *~
            *            NUMBERS FOR THE SAME CUSTOMER.                 *~
            *            BCKMASTR OPENED HERE TO AVOID POINTER          *~
            *            CONFLICTS.                                     *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/03/93 ! ORIGINAL                                 ! JDH *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "BCKDUPPO" (cuscode$, so$, po$, errormsg$)

        dim cuscode$9,                   /* Customer Code - Passed in  */~
            errormsg$79,                 /* Error Message - Passed out */~
            po$16,                       /* Purchase Order- Passed in  */~
            so$16,                       /* Sales Order   - Passed in  */~
            temp_cuscode$9,              /* Customer Code - Comparison */~
            temp_po$16,                  /* Purchase Order- Comparison */~
            temp_so$16                   /* Sales Order   - Comparison */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #05 ! BCKMASTR ! Backlog master file                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L02102
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs "
L02102: REM *************************************************************
            select #05, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            if been_here_before% = 1% then L10000

                been_here_before% = 1%
                call "OPENCHCK" (#05, 0%, f2%, 0%, " ")
                f2% = f2%

L10000: REM *************************************************************~
            * Here's where what little happens happens.                 *~
            *************************************************************

            errormsg$ = " "
            call "REDALT0" (#05, po$, 1%, po_found%) /* Prime the pump */
            goto L10050
L10040:     call "READNEXT" (#05, po_found%)        /* Keep it pumpin' */
L10050:     if po_found% = 0% then L65000
                get #05 using L10070, temp_cuscode$, temp_so$, temp_po$
L10070:              FMT CH(9), CH(16), CH(16)
                if temp_po$ <> po$ then L65000           /* All done.   */
                if temp_cuscode$ <> cuscode$ then L10040 /* No problem, */
                                                        /* Diff Cust.  */
                if temp_so$ = so$ then L10040   /* No problem, same SO. */
L10100:              u3% = 2%
                     call "ASKUSER" (u3%, "DUPLICATE PO"," This custome"&~
                               "r specifies the same PO# on", "Sales "  &~
                               "Order " & temp_so$, "Press RETURN "     &~
                               "to Continue; Press PF1 to Re-enter.")
                     if u3% =  0% then L65000
                     if u3% <> 1% then L10100
                          errormsg$ = "Duplicate PO Number, Please " &   ~
                                      "Re-enter!"
                          goto L65000

        REM *************************************************************~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *************************************************************

L65000: REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            end
