        REM *************************************************************~
            *                                                           *~
            *  DDDD   EEEEE   SSS    CCC   RRRR   IIIII  BBBB   EEEEE   *~
            *  D   D  E      S      C   C  R   R    I    B   B  E       *~
            *  D   D  EEEE    SSS   C      RRRR     I    BBBB   EEEE    *~
            *  D   D  E          S  C   C  R   R    I    B   B  E       *~
            *  DDDD   EEEEE   SSS    CCC   R   R  IIIII  BBBB   EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DESCRIBE - SUBROUTINE TO DESCRIBE ARBITRARY ITEM.  DESCR  *~
            *            MUST BE IN THE 30 (32 FOR "HNYMASTR") BYTES    *~
            *            IMMEDIATELY AFTER THE KEY OF FILE.  ALLOWS     *~
            *            OPTIONAL PARENTHESES.                          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/17/80 ! ORIGINAL                                 ! BCW *~
            * 09/21/80 ! "GETUFBWA" INSTEAD OF BASIC "GET"        ! BCW *~
            * 05/14/81 ! INCREASE KEY LENGTH FOR HNYMASTR         ! TEM *~
            *************************************************************

            sub "DESCRIBE" (#1, key$, descr$, flag%, f1%)

            dim text$50,                 /* DESCRIPTION WORK AREA.     */~
                descr$50,                /* RETURNED DESCRIPTION       */~
                temp$100,                /* RECORD TEXT                */~
                prname$8,                /* PARAMETER REFERENCE NAME   */~
                ufbkl$1,                 /* KEY LENGTH THIS FILE       */~
                ufbkd$2,                 /* KEY DISPLACEMENT REL TO 0  */~
                key$50                   /* READ KEY                   */~

            text$ = " "
            f1% = 0
            descr$ = " "
            if key$ = " " then end
            call "READ100" (#1, key$, f1%)
                 if f1% = 0 then end
            call "GETUFBWA" addr(#1, temp$)        /* RECORD WORK AREA */
            call "GETUFBKL" addr(#1, ufbkl$)       /* KEY LENGTH (BIN) */
            call "GETUFBDK" addr(#1, ufbkd$)       /* KEY DISPL. (BIN) */
            call "GETPRNAM" addr(#1, prname$)

            REM DESCRIPTION = KEY LENGTH + 3, FOR 30 >>INVENTORY KLUGE<<
                disp% = 1+val(ufbkl$)+val(ufbkd$,2)
                if disp% > 100% then end
                text$ = str(temp$, disp%)
                if prname$ = "HNYMASTR"                                  ~
                   then str(text$, 33) = " "                             ~
                   else str(text$, 31) = " "

            REM NOW PUT PARENTHESES AROUND THIS THING.
                if text$ <> " " and flag% <> 0                           ~
                   then text$ = "(" & text$ & ")"
                descr$ = text$
                end

        REM *************************************************************~
            *                   E X P L A N A T I O N                   *~
            *                                                           *~
            * THIS ROUTINE USES A FEW TRICKS THAT ARE WORTH KNOWING     *~
            * ABOUT.  ONE IS THE KLUGE WITH CHCEKING TO SEE IF THE      *~
            * PRNAME IS THAT OF THE INVENTORY MAIN FILE.  THIS IS       *~
            * BECAUSE THE INVENTORY MAIN FILE HAS A 32-CHARACTER        *~
            * DESCRIPTION, WHILE ALL THE REST ARE 30-CHAR (THAT'S A     *~
            * MISTAKE, BUT THAT'S THE WAY IT IS...                      *~
            *     ALSO, THE "GETUFBWA" GETS THE WORK AREA FROM THE UFB  *~
            * AND RETURNS THE FIRST 100 BYTES OF IT TO THE VARIABLE     *~
            * SPECIFIED.  THAT MEANS THAT IT'S A GOOD IDEA TO HAVE A    *~
            * 100-BYTE MINIMUM RECEIVER.  NOTE ALSO THAT IF THE RECORD  *~
            * BEING DESCRIBED IS LESS THAN 100 CHARACTERS LONG, THEN    *~
            * WHAT YOU WILL GET IS GARBAGE.  IN THE UNLIKELY EVENT THAT *~
            * YOU HIT A RECORD THAT HAS A DESCRIPTION DISPLACEMENT OF   *~
            * MORE THAN 70 BYTES, THE SUBROUTINE WILL NEED MODIFICATION *~
            * BUT IT WILL RETURN THE CORRECT F1%  AND A BLANK OR FRAG-  *~
            * MENTED DESCRIPTION.                                       *~
            *************************************************************

