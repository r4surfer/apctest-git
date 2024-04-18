        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   H   H   OOO    SSS   TTTTT    A    TTTTT          *~
            *  S      H   H  O   O  S        T     A A     T            *~
            *   SSS   HHHHH  O   O   SSS     T    AAAAA    T            *~
            *      S  H   H  O   O      S    T    A   A    T            *~
            *   SSS   H   H   OOO    SSS     T    A   A    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHOSTAT  - General routine for display of a message up to *~
            *            80 characters in length.  Used throughout the  *~
            *            CMS System.  Calling syntax is;                *~
            *            CALL "SHOSTAT" ("abc....") or ( String Var )   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/13/82 ! ORIGINAL                                 ! GLW *~
            * 08/15/85 ! Modified to Give a "Windowing" Effect    ! LDJ *~
            *          !   using WSXIO,  Message length lengthened!     *~
            *          !   from 79 characters to 80 characters.   !     *~
            * 10/22/85 ! Simplified, no FAC limit                 ! KAB *~
            * 06/29/87 ! Complicated again to properly handle     ! LDJ *~
            *          !   hidden FACs.  Shows program name.      !     *~
            *          !   Also now shows date & time also.       !     *~
            * 04/19/89 ! O.S. 7.20/30 Comapatability (WSXIO).     ! KAB *~
            * 02/20/91 ! Converted to stub for ZSHOSTAT           ! MJB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


           sub "SHOSTAT" (arg1)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            close ws

            call "ZSHOSTAT" (arg1)

            end
