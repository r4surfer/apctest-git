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
            *                            * 0 = Dim display              *~
            *                            *  +1 = Bright display         *~
            *                            *  +2 = with BELL              *~
            *                            * EVEN = Dim display           *~
            *                            * ODD  = Bright display        *~
            *                            * [Divide by 2]                *~
            *                            * EVEN = No Bell               *~
            *                            * ODD  = Bell                  *~
            *                            *  (Not used by 'O', 'C', 'E') *~
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
            * 02/25/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 07/09/93 ! More Minor Mods for CDAVSCOM Unix Supp.  ! KAB *~
            * 09/13/93 ! Soft Support for scrolling (Unix n/Avail)! KAB *~
            *          ! Programmer's Note - Embedded here is     !     *~
            *          !   'self test' to enable stand-alone      !     *~
            *          !   testing. (*) the sub statement,        !     *~
            *          !   activate the code at 9000 and change   !     *~
            *          !   certain easy to find END statemets to  !     *~
            *          !   RETURN.  Compile, LINK (!) and Run.    !     *~
            *          !   Much easier than concocting drivers.   !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

            sub "ZSCRNADD" (operation$, row%, column%, pass$, len%,      ~
                            intensity%, return%)

        dim                                                              ~
            operation$1,                                                 ~
            function$1,                                                  ~
            command$1,                                                   ~
            order$4,                                                     ~
            screen$81,                                                   ~
            tasktype$1,                  /* Foregound or Background ?  */~
            version$6,                   /* Test Platform              */~
            pass$79,                                                     ~
            fac$1,                                                       ~
            iosw_receiver$8

        dim order$(1)4,                                                  ~
            screen$(24)80

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.01 07/28/94 CMS Patch Release R6.03.01      "
        REM *************************************************************

            goto scrnaddr      /* Hold Encryption at this location */

        /*  OPERATION$ = "O"
            ROW%       =  0%
            COLUMN%    =  2%
            PASS$      = " "
            LEN%       =  0%
            INTENSITY% =  0%
            RETURN%    =  0%
            GOSUB SCRNADDR


            OPERATION$ = "E"
            ROW%       =  1%
            COLUMN%    =  2%
            PASS$      = " "
            LEN%       =  0%
            INTENSITY% =  0%
            RETURN%    =  0%
            GOSUB SCRNADDR

            FOR Q% = 1% TO 24%
                OPERATION$ = "W"
                ROW%       =  Q%
                COLUMN%    =  2%
                PASS$      = " " : CONVERT Q% TO STR(PASS$,2%,2%), PIC(00)
                LEN%       =  78%
                INTENSITY% =  0%
                RETURN%    =  0%
                GOSUB SCRNADDR
            NEXT Q%

            Q% = 24%
                OPERATION$ = "W"
                ROW%       =  Q%
                COLUMN%    =  2%
                PASS$      = " " : CONVERT Q% TO STR(PASS$,2%,2%), PIC(00)
                LEN%       =  78%
                INTENSITY% =  0%
                RETURN%    =  0%
                GOSUB SCRNADDR
            IF RETURN% = 116% THEN END
            GOTO 9310
        */

         scrnaddr:
            return% = 0%
            if tasktype$ = " " then call "EXTRACT" addr("TT",tasktype$,  ~
                                                        "S#",version$)
            if tasktype$ = "B" then end

*        Determine Platform for this routine
            if unix% <> 0% then L10110  /* Done only once */
            convert version$ to unix%, data goto L10100
               goto L10110
L10100:           unix% = -1%
L10110
*        Now if UNIX% >= 0% this is a VS Platform, otherwise its UNIX

            if operation$ = "W" then L14000
            if operation$ = "U" then L14000
            if operation$ = "D" then L14000
            if operation$ = "O" then L11000
            if operation$ = "C" then L12000
            if operation$ = "E" then L13000
            return% = 1%                           /* INVALID FUNCTION */
            end


L11000
*       **                    OPEN FUNCTION
            select # 1, "CRTFILE", consec, recsize = 1924
            function$ = "O"
            device_num% = 255%
            call "WSXIO" addr(function$, device_num%, "Y", #1, return%)
            end /* RETURN */

L12000
*       **                    CLOSE FUNCTION
            REM *** first lock the keyboard ***
            call "WSXIO" addr("X", #1, hex(80), hex(01000000), hex(00),  ~
                     0%, iosw_receiver$)
            function$="C"
            call "WSXIO" addr(function$, #1)
            end /* RETURN */

L13000
*       **                    ERASE SCREEN FUNCTION
            if row% < 1% or row% > 24% then return% = 2%
            if return% > 0% then end
            function$ = "X"
            command$ = hex(80)
            order$ = bin(row%,1) & hex(020000)
            call "WSXIO" addr(function$, #1, command$, order$, screen$,  ~
                 0%, iosw_receiver$)
            end /* RETURN */

L14000
*       **                    WRITE / SCROLL FUNCTION
            if row% < 1% or row% > 24% or column% < 2% or column% +      ~
                     len% > 80% then return% = 2%
            if return% > 0% then end

            bright% = mod(intensity%, 2%)
            bell%   = intensity% / 2%
            bell%   = mod(bell%, 2%)

*       *
*          HEXUNPACK IOSW_RECEIVER$ TO STR(PASS$, 8%,16%)
*          STR(PASS$,44%,16%) = TIME
*       *

*       *** Platform Sensitive Code
            if unix% < 0% then L14180
            call "WSXIO" addr("W", #1, 1%, iosw_receiver$)
              goto L14190
L14180:     call "WSXIO" addr("A", #1, str(iosw_receiver$,3%,1%))
L14190
*       *** Platform Sensitive Code

*       *
*          HEXUNPACK IOSW_RECEIVER$ TO STR(PASS$,26%,16%)
*          STR(PASS$,62%,16%) = TIME
*       *
L14250:        if str(iosw_receiver$,3%,1%) <  hex(40) then L14500
                  return% = val(str(iosw_receiver$,3%,1%),1) + 36%
               if str(iosw_receiver$,3%,1%) <= hex(50) then L14500
                  return% = val(str(iosw_receiver$,3%,1%),1) + 20%
               if str(iosw_receiver$,3%,1%) <> hex(6f) then L14500

            call "WSXIO" addr("X", #1, hex(80), hex(01800101), hex(00),  ~
                     0%, iosw_receiver$)
*       *
*          HEXUNPACK IOSW_RECEIVER$ TO STR(PASS$,44%,16%)
*       *
            call "WSXIO" addr("W", #1, 360000%, iosw_receiver$)
*       *
*          HEXUNPACK IOSW_RECEIVER$ TO STR(PASS$,62%,16%)
*       *
               goto L14250

L14500:     if bright% = 0% then fac$ = hex(8c) else fac$ = hex(84)

            function$ = "X"
            len1% = 81%
            if row% = 24% then len1% = 80%
            order$=bin(row%,1) & hex(800000)

            if operation$ <> "D" then L14620
               if row%  = 24% then L15000
               if unix% <  0% then L16000
                  str(order$,2%,1%) = hex(90)
                  goto L15000
L14620:     if operation$ <> "U" then L15000
               if row%  = 24% then L15000
               len1% = 80%
               if unix% <  0% then L16000
                  str(order$,2%,1%) = hex(88)
                  goto L15000

L15000:     if bell% = 1% then str(order$,2%,1%) = or hex(40)
            command$=hex(80)
            str(screen$,column%-1%,len%+2%) = fac$ & str(pass$,1%,len%) &~
                     hex(84)
            call "WSXIO" addr(function$, #1, command$, order$,           ~
                str(screen$,1%,len1%), len1%, iosw_receiver$)
            str(screen$,column%-1%, len%+2%) = " "
            end /* RETURN */

L16000
*       *** UNIX Scroll Emulation

            call "WSXIO" addr("X", #1, hex(80), hex(01000101), hex(00),  ~
                     0%, iosw_receiver$)

            function$ = "X"
            len1% = (25% - row%) * 80%
            order$(1) = bin(row%,1) & hex(000000)
            command$ = hex(40)
            call "WSXIO" addr(function$, #1, command$, order$(1),        ~
                str(screen$(),1%,len1%), len1%, iosw_receiver$)
            if operation$ = "D" then L16230

*       * UP Emulation

            for i% = 1% to 24% - row%
                screen$(i%) = screen$(i% + 1%)
            next i%
            screen$(25% - row%) = " "
            str(screen$(25% - row%),column%-1%,len%+2%) =                ~
                                      fac$ & str(pass$,1%,len%) & hex(84)
            goto L16320

L16230
*       * DOWN Emulation

            for i% = 25% - row% to 2% step -1%
                screen$(i%) = screen$(i% - 1%)
            next i%
            screen$(1%) = " "
            str(screen$(1%),column%-1%,len%+2%) =                        ~
                                      fac$ & str(pass$,1%,len%) & hex(84)

L16320
*       * Rewrite the affected portion of the screen

            function$ = "X"
            len1% = (25% - row%) * 80%
            order$(1) = bin(row%,1) & hex(800000)
            command$ = hex(80)
            if bell% = 1% then str(order$,2%,1%) = or hex(40)
            call "WSXIO" addr(function$, #1, command$, order$(1),        ~
                str(screen$(),1%,len1%), len1%, iosw_receiver$)
            end /* RETURN */

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************
