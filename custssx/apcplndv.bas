*       ****************************************************************~
*                             ( As of 11/14/97 - RHH )                 *~
*        APCPLNDV - Under Development Message                          *~
*                   Check for R6.04.03                                 *~
*       ****************************************************************

        sub "APCPLNDV"

        dim hdr$40,                      /* ASKUSER Header            */ ~
            msg$(3)79                    /* ASKUSER Messages          */

            init(" ") hdr$, msg$()

            comp% = 2%
            hdr$ = "*** Under Development ***"
            msg$(1) = "(Specified Selection) Has not been Implemented"
            msg$(3) = "Press any Key to Exit "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
            goto exit_program

        exit_program
        end


