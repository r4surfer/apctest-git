        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDOPEN                              *~
            *  Creation Date     - 08/17/99                             *~
            *  Last Modified Date- 09/06/99                             *~
            *  Written By        - Royal H.Hoffman                      *~
            *  Modifications By  - Royal H. Hoffman                     *~ 
            *                                                           *~
            *  Description       - EWD Open File Assume Shared and      *~
            *                      that "File Must Exists.              *~
            *                                                           *~
            *         Note       - Will not create file                 *~
            *                                                           *~ 
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/17/99 ! Original -                               ! RHH *~
            *************************************************************

        sub "EWDOPEN"  (#1,              /* File Channel to open       */~
                        filename$,       /* File Name                  */~
                        error%)          /* Return Code 0% = ok        */

        dim                                                              ~
            filename$8,                  /* Filename                   */~
            library$8,                   /* Library                    */~
            invol$6,                     /* Default input volume name  */~
            outvol$6,                    /* Default output volume name */~
            fgbg$1,                      /* Task Type Fore/Back        */~ 
            volume$6, vtoc$22            /* Volume                     */



            call "EXTRACT" addr("IL", library$, "IV",invol$,"OV",outvol$,~
                            "TT",fgbg$)  /* Extract data library name. */

            volume$ = "?"              
            x% = 1%       
            vtoc$ = " "                  /* Find Volume Name           */
            call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
            if vtoc$ = " " then goto open_error      /* File Not found */
                volume$ = vtoc$
                open nogetparm #1,                                       ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$
        error% = 0%
        end

        open_error
           error% = 1%
        end


