        REM *************************************************************~
            *                                                           *~
            *   SSS   EEEEE  TTTTT   SSS   EEEEE  PPPP                  *~
            *  S      E        T    S      E      P   P                 *~
            *   SSS   EEEE     T     SSS   EEEE   PPPP                  *~
            *      S  E        T        S  E      P                     *~
            *   SSS   EEEEE    T     SSS   EEEEE  P                     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SETSEP   - SET SEPARATOR LINES FOR INPUT.  INPUTS TO      *~
            *            ROUTINE INCLUDE THE SEPARATOR LINE ARRAY, THE  *~
            *            STARTING LINE NUMBER TO NUMBER THEM WITH, AND  *~
            *            THE NUMBER OF LINES TO DO.                     *~
            *-----------------------------------------------------------*~
            *                   M O D I F I C A T I O N S               *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 05/15/80 ! ORIGINAL                                 ! BCW *~
            *************************************************************

            sub "SETSEP" (separator$(), start%, number%)
                dim separator$(10)79
                init("-") separator$()
                if number% = 0 then end
                for temp% = 1 to number%
                    junk$ = "LINE"
                    convert start% + temp% to str(junk$, len(junk$)+2),  ~
                                         pic(###)
                    str(separator$(temp%),                               ~
                            (len(separator$(temp%))-len(junk$))/2,       ~
                            len (junk$)) = junk$
                    next temp%
                    end
