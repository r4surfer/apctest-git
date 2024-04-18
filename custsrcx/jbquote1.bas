        REM *************************************************************~
            *                                                           *~
            *  Program Name      - JBQUOTE1                             *~
            *  Creation Date     - 05/14/97                             *~
            *  Last Modified Date- 11/17/97                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - New Utility Program to bring up      *~
            *                      the APC Price Quote background task. *~
            *                      (JBPOST1)                            *~
            *                                                           *~
            *  Subroutine Used   - TASKUP                               *~
            *                                                           *~
            *  Spec. Comments    - Run from Menu                        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/14/97 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/17/97 ! Mof for Upgrade to R6.04.03              ! RHH *~
            *************************************************************

            return% = 0%
            call "TASKUP" ("J1", return% )
        end

