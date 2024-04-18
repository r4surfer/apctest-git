        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  M   M   AAA   N   N  U   U   AAA   L                     *~
            *  MM MM  A   A  NN  N  U   U  A   A  L                     *~
            *  M M M  AAAAA  N N N  U   U  AAAAA  L                     *~
            *  M   M  A   A  N  NN  U   U  A   A  L                     *~
            *  M   M  A   A  N   N   UUU   A   A  LLLLL                 *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * MANUAL   - The subroutine is passed a name of a program or*~
            *            subroutine. The program name is then passed    *~
            *            via dynamic link to ONLINEWP to convert &      *~
            *            display the document.                          *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/05/82 ! ORIGINAL                                 ! ECR *~
            * 05/04/83 ! ADDED ACCESS = PRINT TO DISPLAY GETPARM  ! ECR *~
            * 05/12/83 ! CHANGED "READFDR" TO "FIND" DOCFILE VOL. ! ECR *~
            * 11/22/83 ! THIS VERSION FOR NON-WP CUSTOMERS        ! ECR *~
            * 09/13/84 ! CHANGED TO POINT TO NEW S.E.S LIBRARY    ! LDJ *~
            *          !   (SESDB) AND ACCOMODATE NEW INTDOC01.   !     *~
            * 02/08/85 ! CORRECTED WORKVOL EXTRACT - IF NOT THERE ! LDJ *~
            *          !   WILL USE SYSTEM VOLUME.  ALSO CORRECTED!     *~
            *          !   BUG IN TESTING WHETHER INTDOC01 NEEDS  !     *~
            *          !   TO BE CLOSED OR NOT.                   !     *~
            * 10/16/85 ! Modified to no longer have a hard-coded  ! LDJ *~
            *          !   SES library - now extracted from OUTLIB!     *~
            * 01/21/86 ! Stripped to nothing more than caller to  ! LDJ *~
            *          !   ONLINEWP.                              !     *~
            * 02/20/91 ! Converted to stub for ZMANUAL            ! MJB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "MANUAL" (arg1)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            close ws

            call "ZMANUAL" (arg1)

            end
