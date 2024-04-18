        REM *************************************************************~
            *                                                           *~
            *  FFFFF  M   M  TTTTT  K   K  EEEEE  Y   Y   CCC   K   K   *~
            *  F      MM MM    T    K  K   E      Y   Y  C   C  K  K    *~
            *  FFFF   M M M    T    KKK    EEEE    YYY   C      KKK     *~
            *  F      M   M    T    K  K   E        Y    C   C  K  K    *~
            *  F      M   M    T    K   K  EEEEE    Y     CCC   K   K   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FMTKEY   - FORMATS A BUFFER KEY FOR A STANDARD DOCUMENT.  *~
            *            RETURNS NEXT BUFFER KEY IN KEY$, AND THE       *~
            *            REVERSE KEY IN REVERSEKEY$.  NEEDS TO HAVE A   *~
            *            FILE NUMBER ALSO, TO KNOW WHERE TO LOOK FOR    *~
            *            NEXT CONSECUTIVE SEQUENCE NUMBER.              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/15/80 ! ORIGINAL                                 ! BW  *~
            * 03/15/83 ! CHECK SEQUENCE NUMBER FOR CHKBUFFR       ! KEN *~
            *************************************************************

            sub "FMTKEYCK" (#1, key$, reversekey$)

            dim key$7,                   /* FORWARD KEY--USERID + SEQ  */~
                reversekey$7,            /* REVERSE KEY--XOR WITH HEXFF*/~
                userid$7,                /* USERID OF CURRENT USER.    */~
                tempuserid$3             /* USERID FROM DISK FOR TESTS */~

            REM FIND LAST KEY IN BUFFER.
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10092
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10092: REM *************************************************************
                bufferseqnr% = 0
                call "EXTRACT"  addr("ID", userid$)
                str(reversekey$,1,7)=hex(cfcfcfc6c6c6c6)
                gosub L11000
                      if f1% = 0 then L10180
                         get #1, using L10140, tempuserid$, temp$
L10140:                          FMT CH(3), CH(4)
                REM      IF TEMPUSERID$ <> USERID$ THEN 10180
                         convert temp$ to bufferseqnr%

L10180:         REM CONVERT WHATEVER WE THOUGHT WAS IT TO THE KEY.
                    key$ = userid$
                    convert bufferseqnr% + 1 to str(key$, 4, 4), pic(0000)
                    str(reversekey$,4,4)=str(key$,4,4) bool6 all(hex(ff))
                    str(reversekey$,1,3)=hex(cfcfcf)
                    end

L11000: REM *************************************************************~
            *            R E A D A L T 2   S U B R O U T I N E          *~
            *                                                           *~
            * THIS IS A COPY OF THE SOURCE CODE FOR THE READALT2 SUB,   *~
            * WHICH NEEDS TO BE INCLUDED IN THIS PARTICULAR PROGRAM IN  *~
            * ORDER TO CIRCUMVENT AN APPARENT COMPILER BUG WHICH MAKES  *~
            * THE KEY$ ARGUMENT GET LOST.  HERE, HOWEVER, WE WIRE THE   *~
            * KEY NUMBER TO CONSTANT 1, RATHER THAN SETTING UP A        *~
            * VARIABLE FOR IT.                                          *~
            *************************************************************

            call "GETUFBS1" addr (#1, f1%)
            if f1% = 0% then return
            f1% = 0
            read #1, key 1 > reversekey$,                                ~
                     eod goto L11180
            f1% = 1
L11180:     return
