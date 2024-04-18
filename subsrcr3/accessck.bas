        REM *** ACCESSCK - Check User/Program Access Rights to a File ***~
*       ****************************************************************
        sub "ACCESSCK" addr(file$,       /* In:  File Name to Check (8)*/~
                            library$,    /*      File Library Name  (8)*/~
                            volume$,     /*      File Volume  Name  (6)*/~
                            read$,       /* Out: Read  Access? Y/N  (1)*/~
                            write$,      /*      Write Access? Y/N  (1)*/~
                            return%)     /*      Return Code from      */~
                                         /*      READFDR - If not 0    */~
                                         /*      then File not found.  */

        REM *************************************************************~
            *             D E C L A R E   V A R I A B L E S             *~
            *************************************************************
        dim bitmask$32,                  /* Bit Packing String         */~
            file$8,                      /* File Name to Check         */~
            file_class$1,                /* File's Security Class      */~
            filemask$4,                  /* File's Read/Write Mask(s)  */~
            library$8,                   /* File Library Name          */~
            ownerid$3,                   /* File's Owner ID            */~
            progrmask$4,                 /* Program's  Read Access Mask*/~
            progwmask$4,                 /* Program's Write Access Mask*/~
            read$1,                      /* Read Privileges Allowed?   */~
            userid$3,                    /* Current User's ID          */~
            userrmask$4,                 /* User's  Read Access Mask   */~
            userwmask$4,                 /* User's Write Access Mask   */~
            volume$6,                    /* File Volume Name           */~
            write$1                      /* Write Privileges Allowed?  */

        REM *************************************************************~
            *                  I N I T I A L I Z A T I O N              *~
            *************************************************************
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto    L00322
            cms2v$ = "05.01.00 07/01/88 General Release R5.01.00        "
L00322: REM *************************************************************
            if userid$ <> " " then get_file_class  /* Already Extracted*/
            call "EXTRACT" addr("UR", userrmask$, "UW", userwmask$,      ~
                                "MR", progrmask$, "MW", progwmask$,      ~
                                "ID", userid$)
            userrmask$ = userrmask$ or progrmask$ /* User + Prog Rights*/
            userwmask$ = userwmask$ or progwmask$ /* User + Prog Rights*/


        REM *************************************************************~
            *   G E T   &   C H E C K   F I L E   C L A S S   M A S K   *~
            *************************************************************
        get_file_class
            read$, write$ = "N"          /* Default = NO               */
            call "READFDR" addr(file$, library$, volume$, 0%,            ~
                                "FC", file_class$, "ID", ownerid$,return%)
            if return% <> 0% then exit_routine     /* File Not Found   */
            REM *** Is the Current User the Owner or a Security Admin? ***
            if userid$ <> ownerid$ and file_class$ <> " " and            ~
               str(userrmask$,,1%) < hex(80) then check_read_privileges
            read$, write$ = "Y"
            goto exit_routine

        REM *************************************************************~
            *  Check File Class against User/Program's Read Privileges  *~
            *************************************************************
        check_read_privileges
            if file_class$ = "#" or file_class$ = "@" then exit_routine
            if file_class$ = "$" then check_write_privileges
            bitmask$ = all("0")
            str(bitmask$,val(file_class$,1)-63%,1%) = "1"
            call "BITPACK" addr(bitmask$, filemask$, 32%)
            filemask$ = filemask$ and userrmask$
            if filemask$ = hex(00000000) then exit_routine /* No Access */

        REM *************************************************************~
            *  Check File Class against User/Program's Write Privileges *~
            *************************************************************
        check_write_privileges
            read$ = "Y"
            if file_class$ = "$" then exit_routine
            call "BITPACK" addr(bitmask$, filemask$, 32%)
            filemask$ = filemask$ and userwmask$
            if filemask$ = hex(00000000) then exit_routine /* No Access */
            write$ = "Y"

        exit_routine
            end
