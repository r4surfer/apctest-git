        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLB58 - Subroutine                *~
            *  Creation Date     - 12/08/98                             *~
            *  Last Modified Date- 12/08/98                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Verify Process Routine, Yes or No    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/08/98 ! New Program for (EWD) - Last Mod Date    ! RHH *~
            *************************************************************

        sub "EWDPLB58" (switch% )        /* o% = Pass, 1% = Fail       */

        dim                                                              ~
            yes$3,                       /* Input Verify               */~
            blankline$79                 /* Cursor location for edit   */

        REM *************************************************************~
            *            S A F E G U A R D   S C R E E N   1            *~
            *                                                           *~
            * SAFEGUARDS THE ACCIDENTAL DELETING OF DATA BY ASKING IF   *~
            * HE/SHE REALLY WANTS TO DELETE IT.                                 *~
            *************************************************************

            gosub L10000
            goto exit_sub
 
L10000: accept                                                           ~
               at (08,23), "***************************************",    ~
               at (09,23), "* YOU ARE ABOUT TO DELETE INFORMATION *",    ~
               at (10,23), "*       DO YOU WISH TO PROCEED?       *",    ~
               at (11,23), "*                                     *",    ~
               at (12,23), "*                                     *",    ~
               at (13,23), "*                                     *",    ~
               at (14,23), "*     RESPONSES OTHER THAN 'YES'      *",    ~
               at (15,23), "*       WILL ABORT THE PROCESS        *",    ~
               at (16,23), "***************************************",    ~
                                                                         ~
               at (12,40), fac(hex(81)), yes$                   , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
                                                                         ~
                                                                         ~
               keys(hex(0001)), key (keyhit%)

               switch% = 0%
               if yes$ = "YES" then switch% = 1%
               if keyhit% <> 0% then goto L10000
        return

        exit_sub
        end
