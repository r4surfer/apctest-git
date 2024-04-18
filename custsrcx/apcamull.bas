      REM ***************************************************************~
          *                                                             *~
          *  Subroutine Name   - APCAMULL                               *~
          *  Creation Date     - 03/08/2019                             *~
          *  Written By        - Ricky Beane                            *~
          *                                                             *~
          *  Description       - Check for mulling                      *~
          *                                                             *~
          *-------------------------------------------------------------*~
          *          - Paramters                                        *~
          *             Input - Part number                             *~
          *             Output - mull interger => flag 0 or 1           *~
          *                    - desc character 30                      *~
          *                    - wood_scrpt   character 2               *~ 
          *          - INPUT Files                                      *~
          *                     GENCODES                                *~
          *          - ERROR% = 0% - Ok                                 *~
          *                    99% - Error                              *~
          *-------------------------------------------------------------*~
          *                  M O D I F I C A T I O N S                  *~
          *---WHEN----+----------------WHAT----------------------+-WHO--*~
          * 03/08/2019! New subroutine                           ! RDB  *~
          * 03/21/2019! Add wood$ to return values               ! RDB  *~
          ***************************************************************

        sub "APCAMULL" (part$,           /* Part number                */~
                        mull%,           /* Mull flag                  */~
                        wood$,           /* Wood mull of part          */~
                        desc$,           /* Mull description           */~
                        wood_scrpt$,     /* Wood script code           */~    
                        #1,              /* GENCODES                   */~
                        error%)          /* Return Code                */
                        
        dim                                                              ~
            schema$8,                    /* Schema Switch              */~
            readkey$50                   /* GENCODES GENERIC KEY       */
        
        dim                                                              ~
            part$25,                     /* Part number                */~       
            mull%,                       /* Mull flag                  */~       
            wood$3,                      /* Wood size of part          */~       
            desc$30,                     /* Mull description           */~       
            wood_scrpt$2,                /* Wood script code           */~       
            error%                       /* Return Code                */            
            
            
                        
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************                       
            err%  = 0%
            mull% = 0%
            init(" ") wood_scrpt$, wood$
            call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE      */~
                           schema%,      /* Schema                     */~
                           #1,           /* GENCODES                   */~
                           err% )        /* error                      */                        
                        
        REM *************************************************************~
            *       M A I N   P R O G R A M                             *~
            *************************************************************    
            
            if str(part$,1%,1%) = "9" then goto exit_sub
            if len(part$) < 22 then exit_sub
            if len(part$) = 22 then wood$ = str(part$,20%,3%)      ~
                               else wood$ = str(part$,23%,3%)
            convert wood$ to wood%, data goto L00010    /* Sample */

            if wood% > 1% and wood% < 81% then exit_sub
                                                     
L00010:     if wood$ = "000" then exit_sub
               str(readkey$,1%,9%) = "APC WOOD "
               str(readkey$,10%,3%) = wood$
               read #1,key = readkey$, using L02500, desc$,  eod goto exit_sub
L02500:     FMT POS(24), CH(30)   

               wood_scrpt$ = str(wood$,1%,1%) & "0"  /* 0 = Unit Code */

               mull% = 1%                         /* Set Mull Flag     */

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_sub

        end