        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN16                             *~
            *  Creation Date     - 06/24/04                             *~
            *  Last Modified Date- 03/21/2018                           *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  - Christie M. Gregory                  *~
            *                                                           *~
            *  Description       - New Program to split to GLSGED file  *~
            *                      into batches.                        *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Spec. Comm (Screen 1) -                                  *~
            *                         PF(10) Create Delimited File.     *~
            *  **Default Run Prompts -> Enter - Press Return to Continue*~
            *                           PF1   - Glass                   *~
            *                           PF10  - Merge DS                *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/24/04 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            * 03/15/05 ! (AWD001) mod for pre-cut file            ! CMG *~
            * 09/28/05 ! (AWD002) Add Question for DS Batches     ! CMG *~
            *11/17/2009! (AWD003) changes for Ultra Batches       ! CMG *~
            *04/05/2013! (AWD004) add schema                      ! CMG *~
            *03/21/2018! (CR800) mod for DS and error batches     ! CMN *~
            *************************************************************

        dim                              /*                            */~
            hdr$40,                      /* Askuser Header             */~
            msg$(3%)79,                  /* Askuser Messages           */~
            filename$8,                  /* File Name for Open         */~
            errormsg$79                  /* Error Msg                  */

/* (AWD004) */
        dim schema$8                     /* Database schema            */



        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! @GLSGED@ ! Glass Batch File for GED Glass System    *~
            * #2  ! GENCODES ! Master Code Tables File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "@GLSGED@", consec, recsize = 384  


            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24


REM  SELECT #3, "@GLSPRE@", CONSEC, RECSIZE = 384  /* (CR800) */

/* (AWD003) */
REM  SELECT #4, "@GLSULA@", CONSEC, RECSIZE = 384  /* (CR800) */


            gosub run
            if comp% = 16%  then goto exit_program

REM GOSUB RUN_WHICH
REM FF% = 1%
REM IF PRE_COMP% = 10% THEN FF% = 3%
REM IF PRE_COMP% = 20% THEN FF% = 4%         /* (AWD003) */

REM PRE% = 0%
REM IF FF% = 3% THEN PRE% = 1%
REM IF FF% = 4% THEN PRE% = 2%      /* (AWD003) MEANS ULTRA TABLES */



REM IF PRE_COMP% <> 1% THEN GOTO NOT_GLSGED
            ff% = 1%
            filename$ = "@GLSGED@" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error

REM NOT_GLSGED
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error


REM IF PRE_COMP% <> 10% THEN GOTO NOT_GLSPRE
REM FILENAME$ = "@GLSPRE@" 
REM CALL "EWDOPEN" (#3, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR         /* (AWD001)  */
REM NOT_GLSPRE

REM IF PRE_COMP% <> 20% THEN GOTO NOT_GLSULA
REM FILENAME$ = "@GLSULA@" 
REM CALL "EWDOPEN" (#4, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR         /* (AWD003)  */
REM NOT_GLSULA
/* (AWD002) */
/* ds_merge% =  1% for split 10% for merge */
REM GOSUB RUN_DS
REM GOTO CREATE_C
/* (AWD004) */

            schema%, s_err% = 0%
            ds_merge% = 10%             /*(CR800)*/
            call "SCHEMA" (schema$, schema%, #2, s_err%)
/* (\AWD004) */
            call "SHOSTAT" (" Writing Glass Data A")

            call "AWDPLD16" (#ff%, #2, pre% , ds_merge%, schema%  )
            close #1
        
REM IF PRE_COMP% = 1% THEN CLOSE #1
REM IF PRE_COMP% = 10% THEN CLOSE #3
REM IF PRE_COMP% = 20% THEN CLOSE #4             /* (AWD003) */

            goto exit_program

REM IF PRE_COMP% <> 1% THEN GOTO NOT_GLSGED_B
REM FILENAME$ = "@GLSGED@"  : CALL "EWDOPEN" (#1, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR
REM NOT_GLSGED_B
REM IF PRE_COMP% <> 10% THEN GOTO NOT_GLSPRE_B
REM FILENAME$ = "@GLSPRE@"
REM CALL "EWDOPEN" (#3, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR
NOT_GLSPRE_B            
/* (AWD003)*/
REM IF PRE_COMP% <> 20% THEN GOTO NOT_GLSULA_B
REM FILENAME$ = "@GLSULA@"
REM CALL "EWDOPEN" (#4, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR
REM NOT_GLSULA_B
REM FF% = 1%
REM IF PRE_COMP% = 10% THEN FF% = 3%
REM IF PRE_COMP% = 20% THEN FF% = 4%                /* (AWD003)*/
REM PRE% = 0%
REM IF FF% = 3% THEN PRE% = 1%
REM IF FF% = 4% THEN PRE% = 2%                      /* (AWD003)*/
REM CREATE_B
REM CALL "SHOSTAT" (" WRITING GLASS DATA B") 
REM STOP
REM CALL "AWDPLB16" (#FF%, #2, PRE%, DS_MERGE%, SCHEMA%   )
REM CLOSE #1
           
REM IF PRE_COMP% = 1% THEN CLOSE #1
REM IF PRE_COMP% = 10% THEN CLOSE #3
REM IF PRE_COMP% = 20% THEN CLOSE #4             /* (AWD003) */
REM GOTO EXIT_PROGRAM
REM IF PRE_COMP% = 10% THEN GOTO EXIT_PROGRAM
REM IF DS_MERGE% = 1% THEN GOTO EXIT_PROGRAM
REM IF PRE_COMP% <> 1% THEN GOTO NOT_GLSGED_C
REM FILENAME$ = "@GLSGED@"   CALL "EWDOPEN" (#1, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR
REM NOT_GLSGED_C
REM IF PRE_COMP% <> 10% THEN GOTO NOT_GLSPRE_C
REM FILENAME$ = "@GLSPRE@"
REM CALL "EWDOPEN" (#3, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR
REM NOT_GLSPRE_C
/* (AWD003)*/
REM IF PRE_COMP% <> 20% THEN GOTO NOT_GLSULA_C
REM FILENAME$ = "@GLSULA@"
REM CALL "EWDOPEN" (#4, FILENAME$, ERR%)
REM IF ERR% <> 0% THEN GOSUB OPEN_ERROR
REM NOT_GLSULA_C

REM FF% = 1%
REM IF PRE_COMP% = 10% THEN FF% = 3%
REM IF PRE_COMP% = 20% THEN FF% = 4%                /* (AWD003)*/
REM PRE% = 0%
REM IF FF% = 3% THEN PRE% = 1%
REM IF FF% = 4% THEN PRE% = 2%                      /* (AWD003)*/
REM CREATE_C
REM CALL "SHOSTAT" (" WRITING GLASS DATA C")  
REM CALL "AWDPLC16" (#FF%, #2, PRE%, SCHEMA%   )
REM GOTO EXIT_PROGRAM




        open_error                                   
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
            err% = 0%
        return

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        run
            comp% = 2%
            hdr$ = "****  Create Glass Batch Files???  ***"
                                                 /* msg$(1%) Pre-Set   */
            msg$(2) = "       G L A S S   B A T C H   F I L E S         "
            msg$(3) = "Press <RETURN> To Continue, or PF(16) to Exit. "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return
/* (AWD003) */
        run_which
            pre_comp% = 2%
            hdr$ = "****  Glass, Pre-Cut or Ultra Files???  ***"
                                                 /* msg$(1%) Pre-Set   */
            msg$(2) = "       G L A S S   B A T C H   F I L E S         "
            msg$(3) = "Press PF<1> Glass PF<10> Pre-Cut PF<20> Ultra. "
            call "ASKUSER" (pre_comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return
/*(AWD002) - BEG */
        run_ds
            ds_merge% = 2%
            hdr$ = "****  Merge DS Glass Batches ???   ***"
                                                 /* msg$(1%) Pre-Set   */
            msg$(2) = "   D S    G L A S S   B A T C H   F I L E S  "   
            msg$(3) = "Press PF<1> for Split or PF<10> for Merge. "
            call "ASKUSER" (ds_merge%, hdr$, msg$(1), msg$(2), msg$(3))
        return
/*(AWD002) - END */


        exit_program

             end


