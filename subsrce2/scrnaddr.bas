        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS    CCC   RRRR   N   N   AAA   DDDD   DDDD   RRRR    *~
            *  S      C   C  R   R  NN  N  A   A  D   D  D   D  R   R   *~
            *   SSS   C      RRRR   N N N  AAAAA  D   D  D   D  RRRR    *~
            *      S  C   C  R   R  N  NN  A   A  D   D  D   D  R   R   *~
            *   SSS    CCC   R   R  N   N  A   A  DDDD   DDDD   R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SCRNADDR - This subroutine allows direct address placement*~
            *            on the screen of strings from 1 to 79 character*~
            *            in length.  Calling arguments are;             *~
            *             OPERATION$1 ;  "O" = Open workstation for this*~
            *                                  subroutines subsequent   *~
            *                                  use.                     *~
            *                            "C" = Close workstation - this *~
            *                                  must be done before the  *~
            *                                  calling program can      *~
            *                                  execute any WS I-O if a  *~
            *                                  previous 'O'pen call was *~
            *                                  made to this subroutine. *~
            *                            "E" = Erase the screen begining*~
            *                                  at the specified row # to*~
            *                                  the end of the screen.   *~
            *                                  (A previous 'O'pen call  *~
            *                                  must have been made).    *~
            *                            "W" = Write a string at the    *~
            *                                  specified row & column   *~
            *                                  position on the screen   *~
            *                                  (a previous 'O'pen call  *~
            *                                  must have been made).    *~
            *                            "U" = Scroll screen contents   *~
            *                                  from line 24 up to       *~
            *                                  specified row and then   *~
            *                                  write string to row 24.  *~
            *                                  Prior contents of        *~
            *                                  specified row will be    *~
            *                                  lost.                    *~
            *                            "D" = Scroll screen down       *~
            *                                  beginning at specified   *~
            *                                  row to bottom of screen  *~
            *                                  (row 24) and then write  *~
            *                                  string to specified row. *~
            *                                  Prior contents of row 24 *~
            *                                  will be lost.            *~
            *             ROW%        ;  The row number in full integer *~
            *                            format. Must be > 0; < 25.     *~
            *                            (not used by 'O' or 'C').      *~
            *             COLUMN%     ;  The column number in full      *~
            *                            integer format. Must be >0; <79*~
            *                            (not used by 'O', 'C', or 'E') *~
            *             PASS$79     ;  The string variable containing *~
            *                            the characters to be displayed *~
            *                            on the screen at the specified *~
            *                            row & column. May be blank.    *~
            *                            (Not used by 'O', 'C', or 'E') *~
            *             LEN%        ;  The length or number of bytes  *~
            *                            in pass$ to display. LEN% +    *~
            *                            COLUMN% cannot be greater than *~
            *                            80.                            *~
            *                            (Not used by 'O', 'C', or 'E') *~
            *             INTENSITY%  ;  Controls whether PASS$ will be *~
            *                            displayed as bright or dim on  *~
            *                            the screen.                    *~
            *                              0 = Dim display              *~
            *                              1 = Bright display           *~
            *                              2 = Bright display with BELL *~
            *                            (Not used by 'O', 'C', or 'E') *~
            *             RETURN%     ;  The return code signifying the *~
            *                            success or failure of the      *~
            *                            requested operation.           *~
            *                            Returned values may be;        *~
            *                              0 = Operation successful     *~
            *                              1 = Invalid operation code   *~
            *                              2 = Invalid row, column, or  *~
            *                                  length combination       *~
            *                              8 = 'O'pen operation not     *~
            *                                   successful, calling     *~
            *                                   program may have failed *~
            *                                   to close crt prior to   *~
            *                                   this call or this       *~
            *                                   subroutine may already  *~
            *                                   have the crt open (two  *~
            *                                   successive calls with no*~
            *                                   intervening close).     *~
            *                           100+ = The pfkey number hit plus*~
            *                                  100.                     *~
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
            * 01/26/84 ! ORIGINAL                                 ! LDJ *~
            * 08/15/84 ! ADDED SCROLL UP/DOWN CAPABILITIES        ! LDJ *~
            * 11/08/85 ! Modified CLOSE to lock the keyboard      ! LDJ *~
            *          !   before closing the workstation.        !     *~
            * 09/18/86 ! Now sensitive to whether program running ! LDJ *~
            *          !   in foregorund or background.           !     *~
            * 11/02/88 ! Added Option to Ring the Workstation Bell! LDJ *~
            * 10/23/89 ! O.S. 7.20/30 Comapatability (WSXIO).     ! MJB *~
            * 02/20/91 ! Converted to stub for ZSCRNADD           ! MJB *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

            sub "SCRNADDR" (arg1, arg2, arg3, arg4, arg5, arg6, arg7)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.04 02/23/91 BASIC 4.03.01 & SSL Support     "
        REM *************************************************************

            close ws

            call "ZSCRNADD" (arg1, arg2, arg3, arg4, arg5, arg6, arg7)

            end
