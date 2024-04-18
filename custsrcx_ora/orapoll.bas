        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - ORAPOLL                              *~
            *  Creation Date     - 06/17/2011                           *~
            *  Last Modified Date- 01/25/2018                           *~
            *  Written By        - David Speight                        *~
            *                                                           *~
            *  Description       - Poll Oracle transaction table &      *~
            *                      update Caelus.                       *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *06/17/2011! Original                                 ! DES *~
            *01/25/20181 (CR1289) mod to add SLMMASTR             ! CMN *~
            *************************************************************
  

        dim                              /*                            */~
            errormsg$79,                 /* Error message              */~
            hdr$40, msg$(3%)79,          /* Shostat Error Messages     */~
            server$25,                   /* Connection String          */~
            user$25,                     /* User Name to Connect       */~
            pass$25,                     /* Password to Connect        */~
            error$256,                   /* Error String               */~
            fields$(100)64,              /* Query Return Field         */~
            field$64,                    /* Query Return Field         */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250                    /* Second Query String        */
 


        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            rslt$(40%)20                 /* Text from file opening     */
            
        dim action$1,                                  ~
            file$256,                                  ~
            keyval$256,                                ~
            colname$256,                               ~
            colval$256

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            
        REM *************************************************************

            mat f2% = con      

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! CUSTOMER !                                          *~
            * #2  ! GENCODES !                                          *~
            * #3  ! AMTBOMCD !                                          *~
            * #4  ! AMTBOMIF !                                          *~
            * #5  ! SLMMASTR ! Salesman master file (CR1289)            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
                  
            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos = 1,    keylen = 24

            select #3,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #4, "AMTBOMIF",                                       ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32
                        
            select #6 , "SLMMASTR",          /* (CR1289) */              ~
                        varc, indexed,                                   ~
                        recsize = 600,                                   ~
                        keypos =    1,  keylen = 4
                        
            select #9,   "SYSFILE2",                                     ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20
                        
            select #10, "VINYLDT",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen = 57,          ~
                            key  2, keypos =   53, keylen = 51,          ~
                            key  3, keypos =    1, keylen = 23, dup,     ~
                            key  4, keypos =   96, keylen =  8, dup                        

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),0%, rslt$(4%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),0%, rslt$(6%)) /*(CR1289)*/
            call "OPENCHCK" (#9, fs%(9%), f2%(9%),0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),100%, rslt$(10%))            

            mat f1% = zer

            error% = 0%
            gosub oracle_connect
            pass% = 0%
           
try_again:
            init(" ") stmt1$, stmt2$
REM           ....+....1....+....2....+....3....+....4....+....5....+....6....
            str(stmt1$,1,54)  =             ~ 
             "SELECT * FROM MSSQL.OE_CAELUS_UPDT ORDER BY TRANSNBR  "   
            gosub select_data
fetch_next:
            gosub fetch_data
REM            IF OCI_ERR% <> 0% AND OCI_ERR% <> 100% THEN ORA_TEST_DONE  
               if oci_err% <> 0% and oci_err% <> 100% then FINI                 
               if oci_err% = 100% then FINI            

            init(" ") action$, file$, keyval$, colname$, colval$ 
            pos% = 1%     
     
            for field_num% = 1%  to no_fields%  
              gosub oracle_getfield
              if field_num% = 1%     then trans$     = field$
              if field_num% = 3%     then action$    = field$
              if field_num% = 4%     then file$      = field$
              l% = field_num% - 4%
              if (l% > 0%) then fields$(l%) = field$
           /*     STR(PRINTMSG$,POS%, FIELD_LEN%) = FIELD$
                  IF FIELD_NUM% = 1%  THEN KEY$ = FIELD$         
                  FIELDS$(FIELD_NUM%) = FIELD$
                  POS% = POS% + FIELD_LEN% */
            next field_num%
           
            err% = 0%
            if file$ = "CUSTOMER" then                       ~ 
               call "ORACUST" (#1, trans$, action$, file$,   ~
                 fields$(), (no_fields% - 3%), err%) 
                 
REM IF FILE$ = "APCPLNDT" THEN                       ~
REM CALL "ORAVINYL" (#10, TRANS$, ACTION$, FILE$, ~
REM FIELDS$(), (NO_FIELDS% - 3%), ERR%) 
                 
            if file$ = "SLSMMASTR" then      /* (CR1289) */   ~ 
               call "ORASLMM" (#6, trans$, action$, file$,   ~
                 fields$(), (no_fields% - 3%), err%)                  
                 
REM IF FILE$ = "AMTBOMCD" THEN                       ~
REM CALL "ORABOMCD" (#3, TRANS$, ACTION$, FILE$,  ~
REM FIELDS$(), (NO_FIELDS% - 3%), ERR%)

            if err% <> 0% then goto try_again       


            init(" ") stmt1$, stmt2$
REM                   ....+....1....+....2....+....3....+....4....+....5....+....6....
 
            stmt1$ = "DELETE FROM MSSQL.OE_CAELUS_UPDT WHERE TRANSNBR = '" ~
                    & trans$ & "'"
            gosub oracle_exec

            stmt1$  = "COMMIT " 
            gosub oracle_exec

             goto try_again       
REM      GOTO FETCH_NEXT      
              
        oracle_connect
            init(" ") user$, pass$, server$
REM USER$   = "MSSQL"
REM PASS$   = "MSSQL"
REM SERVER$ = " "
            call "READ100" (#9, "ORACLE PASSWORD", f1%(9%))   /* SYSFILE2 */
            if f1%(9%) <> 0% then get #9 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

REM         USER$   = "MSSQL"
REM         PASS$   = "MSSQL"
            oci_err% = 0%
            call "CONNECT" (user$, pass$, server$, oci_err%)

            if oci_err% >= 0% then goto exit_con
            errormsg$ = "YOU ARE NOT CONNECTED TO ORACLE, CONTACT SYSTEMS!!!!"
            gosub error_prompt
exit_con: return 

        error_prompt
           comp% = 2%
           hdr$ = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           print errormsg$
REM        CALL "ASKUSER" (COMP%, HDR$, MSG$(1%), MSG$(2%), MSG$(3%))
           goto FINI              
        return
 
        select_data
            gosub oracle_flush
            gosub oracle_query                             
        return

        fetch_data
            gosub oracle_fetch

        return

        oracle_query
            oci_err% = 0%
            call "QUERY" (stmt1$, stmt2$, oci_err%)
        return

        oracle_exec
            gosub oracle_flush
            oci_err% = 0%
            call "EXEC" (stmt1$, stmt2$, oci_err%)
        return

        oracle_flush
            oci_err% = 0%
            call "FLUSH" (oci_err%)
        return

        oracle_fetch
            oci_err% = 0%
            no_fields% = 0%
            call "FETCH" (no_fields%, oci_err%)
 
        return
            if oci_err% = 0% then return
REM CALL "ERROR" (ERROR$)
REM CALL "SHOSTAT" ("ERROR ERROR RETURN VALUE --> " & ERROR$)
REM STOP               
            goto FINI
        return

        oracle_getfield
            oci_err% = 0%
            field_len% = 0%
            init(" ") field$, name$
            call "FIELDINF" (field_num%, field$, name$, field_len%, oci_err%)

        return
            if oci_err% = 0% then return
REM CALL "ERROR" (ERROR$)
REM CALL "SHOSTAT" ("ERROR ERROR RETURN VALUE --> " & ERROR$)
REM STOP 
            goto FINI
        return

REM  ORA_TEST_DONE
        end
           sleep% = 30%
           call "EWDSLEEP" (sleep%, er%)
           goto try_again

    FINI:
          end

