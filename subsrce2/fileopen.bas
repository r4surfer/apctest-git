        REM *************************************************************~
            *                                                           *~
            *  FFFFF  IIIII  L      EEEEE   OOO   PPPP   EEEEE  N   N   *~
            *  F        I    L      E      O   O  P   P  E      NN  N   *~
            *  FFFF     I    L      EEEE   O   O  PPPP   EEEE   N N N   *~
            *  F        I    L      E      O   O  P      E      N  NN   *~
            *  F      IIIII  LLLLL  EEEEE   OOO   P      EEEEE  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FILEOPEN - STANDARD SUBROUTINE THAT OPENS ALL FILES.      *~
            *            WHEN A FILE IS OPENED IN OUTPUT MODE, A 100    *~
            *            RECORD FILE WILL BE CREATED.  THE SELECTION    *~
            *            OF DATA LIBRARY IS BY USEAGE CONSTANT, OR BY   *~
            *            SET INLIB IN A PROCEDURE.                      *~
            *            THIS VERSION INCORPORATES A INTERNAL CALL TO   *~
            *            "SHOWMSG" TO INFORM THE USER THAT FILES ARE NOW*~
            *            BEING OPENED.                                  *~
            *    CALL 'FILEOPEN' (#--, 'MODE ', RSLT$(--), AXD$(--))    *~
            *    VALID MODES ARE  'SHARE', 'OUTPT', ETC (5 CHAR)        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/27/79 ! ORIGINAL SUBROUTINE                      ! BCW *~
            * 12/28/79 ! UFB BUG FIXES & GETPRNAM SUBROUTINE      ! BCW *~
            * 03/19/80 ! FILELIST ON SYSTEM VOLUME. FIX UFBALTPTR ! BCW *~
            * 11/10/80 ! MESSAGE WHILE OPENING FILE               ! TEM *~
            * 08/31/81 ! VOLUME, LIBRARY FROM USAGE CONSTANTS     ! TEM *~
            * 10/22/81 ! REM 10455,10460,10470                    ! ECR *~
            * 12/22/81 ! ADDED FILEOP10 & FILEOP20                ! GLW *~
            * 02/25/83 ! ADD BLOCKS = 10 TO SHARE MODE            ! GLW *~
            * 02/25/83 ! ADD DPACK, IPACK = 50 TO OUTPT MODE      ! GLW *~
            * 05/12/83 ! ADDED "FIND" FOR VOLUME, REMVED BLOCKS=10! ECR *~
            * 10/17/84 ! REPLACED WITH CLONE OF NEW OPENFILE SUB, ! LDJ *~
            *          !   TO INCORPORATE FASTER FIND LOGIC -     !     *~
            *          !   FUNCTIONAL CHARACTERISTICS REMAIN THE  !     *~
            *          !   SAME EXCEPT SHOWMSG GENERATED ONCE &   !     *~
            *          !   ONCE ONLY - NOT FOR EACH FILE ANYLONGER!     *~
            * 01/03/85 ! COPIED BODY OF OPENFILE                  ! KAB *~
            * 01/21/86 ! CALLS OPENFILE                           ! KAB *~
            *************************************************************~
                                                                         ~
           **************************************************************~
           * NOTE- EFFICIENT USE OF DISK SPACE AND PROGRAM EXECUTION    *~
           *         SPEED UNDER VARIOUS SYSTEM LOADS REQUIRES A PROPER *~
           *         SETTING DPACK/IPACK VARAIBLES IN OUTPUT MODE.      *~
           * (*) DPACK/IPACK - WHEN FILES ARE FIRST CREATED THESE TWO   *~
           *         VARIABLES DETERMINE HOW TIGHTLY TO PACK EACH BLOCK *~
           *         THE TIGHTER BLOCKS ARE PACKED, THE MORE SPLATTERING*~
           *         OF RECORDS OVER THE DISK MUST BE DONE.  THE LESS   *~
           *         PACKING, THE MORE SPACE WILL BE USED.  IF YOU RUN  *~
           *         SHORT OF DISK MOVE THESE TWO VALUES TOWARDS 100.   *~
           **************************************************************

        sub "FILEOPEN" (#1, mode$, f2%, returncode$, axd$)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat     "
        REM *************************************************************

            if first% <> 0% then L01062

            call "SHOSTAT" ("Opening Files.  One Moment Please.")
            first% = 1%

L01062:     call "OPENFILE" (#1, mode$, f2%, returncode$, axd$)
            end
