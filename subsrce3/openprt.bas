        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   OOO   PPPP   EEEEE  N   N  PPPP   RRRR   TTTTT          *~
            *  O   O  P   P  E      NN  N  P   P  R   R    T            *~
            *  O   O  PPPP   EEEE   N N N  PPPP   RRRR     T            *~
            *  O   O  P      E      N  NN  P      R   R    T            *~
            *   OOO   P      EEEEE  N   N  P      R   R    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * OPENPRT  - This Subroutine Opens a Print File on the      *~
            *            supplied file channel using the current user   *~
            *            default values.  The File name, Library, &     *~
            *            Volume are all passed back to the calling      *~
            *            program. (for Display, Scratch, etc).          *~
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
            * 07/31/84 ! ORIGINAL                                 ! LDJ *~
            * 03/04/86 ! Print File Now Uses Current Program Name ! LDJ *~
            *          !   (First 4 bytes anyway).                !     *~
            * 08/13/90 ! Basic 4.3 & SSL Compatibility            ! KAB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "OPENPRT"  (#1,              /* FILE CHANNEL OF PRINT FILE */~
                        space%,          /* NUMBER OF RECORDS TO ALLOC */~
                        file$,           /* PRINT FILE NAME RETURNED   */~
                        library$,        /* PRINT FILE LIBRARY RETURNED*/~
                        volume$,         /* PRINT FILE VOLUME RETURNED */~
                        f2%)             /* STATUS, 0=OPEN,1=NOT OPENED*/

        dim                                                              ~
            file$8,                      /* FILE NAME ASSIGNED TO PRT  */~
            library$8, plibrary$8,       /* SPOOLIB OR CONSTRUCT       */~
            program$8,                   /* CURRENT PROGRAM NAME       */~
            volume$6, pvolume$6          /* SPOOLVOL OR SYSVOL         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            call "EXTRACT" addr("CF", program$)
            program$ = "#" & str(program$,,4%): pvolume$, plibrary$ = " "
            f2% = 1
            open nodisplay #1, output, file = program$,                  ~
                                       library = plibrary$,              ~
                                       volume = pvolume$, space = space%
            call "GETNAMES" addr(#1, file$, library$, volume$)
            f2% = 0
            end
