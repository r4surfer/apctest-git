        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *    A     SSS   K  K   U   U   SSS   EEEEE  RRRR           *~
            *   A A   S      K K    U   U  S      E      R   R          *~
            *  AAAAA   SSS   KK     U   U   SSS   EEEE   RRRR           *~
            *  A   A      S  K K    U   U      S  E      R   R          *~
            *  A   A   SSS   K  K    UUU    SSS   EEEEE  R   R          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ASKUSER  - General routine for requesting Yes/No reply    *~
            *            from User.  Overlays message window onto       *~
            *            current screen & waits for user response (PF   *~
            *            Key(s) designated in text from calling program.*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/05/85 ! Original                                 ! LDJ *~
            * 09/29/86 ! Change for O.S. 7.10 incompatibility.    ! LDJ *~
            *          !   Call to SCREEN no longer supports      !     *~
            *          !   passing the Workstation UFB directly.  !     *~
            * 04/19/89 ! O.S. 7.20/30 Comapatability (WSXIO).     ! KAB *~
            * 02/20/91 ! Converted to stub for ZASKUSER           ! MJB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


            sub "ASKUSER" (arg1, arg2, arg3, arg4, arg5)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            close ws

            call "ZASKUSER" (arg1, arg2, arg3, arg4, arg5)

            end
