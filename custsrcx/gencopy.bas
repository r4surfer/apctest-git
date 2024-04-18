        dim filename$8,                  /* Used by EWDOPEN            */~
            errormsg$79,                 /* Error message              */~
            hdr$40, msg$(3%)79,          /* Ask User Prompt            */~
            tables$(50)9,                /* Tables To Delete From      */~
            genkey$24,                   /* Gencodes Key               */~
            genrec$128,                  /* Gencodes Record            */~
            cpykey$24,                   /* Gencopy Key                */~
            cpyrec$128                   /* Gencopy Record             */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCOPY  ! Master Code Table File to Copy From      *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "GENCOPY",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            filename$ = "GENCOPY" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

        tableMax% = 0%
        init(" ") tables$()
        gosub load_tables
        if tableMax% = 0% then goto exit_program
          for i% = 1% to tableMax%

              init(" ") genkey$, cpykey$ 
              str(genkey$,1,9) = tables$(i%)
              str(cpykey$,1,9) = tables$(i%)
              gosub delete_data
              init(" ") genkey$, genrec$
              gosub copy_data
          next i%

        goto exit_program


        load_tables
          table% = 0%
          init(" ") genkey$
          genkey$ = "TABLECOPY"
        load_table_next  
          read #1, key > genkey$, using gen_fmt, genrec$,              ~
                                  eod goto load_table_done
                                   
            genkey$ = str(genrec$,1,24)
            if str(genkey$,1,9) <> "TABLECOPY" then goto load_table_done
            
            table% = table% + 1%
            tables$(table%) = str(genkey$,10,9)
            goto load_table_next
        load_table_done
          tableMax% = table%
        return                           

        delete_data
           read #2, hold, key > genkey$, using gen_fmt, genrec$,          ~
                                 eod goto deletedatadone

gen_fmt:          FMT CH(128)

            genkey$ = str(genrec$,1,24)

            if tables$(i%) <> str(genkey$,1,9) then goto deletedatadone

              delete #2
     
              goto delete_data
        deletedatadone
        return


        copy_data
           read #1, key > cpykey$, using gen_fmt, cpyrec$, ~
                                eod goto copydatadone

           cpykey$ = str(cpyrec$,1,24)
           if str(cpykey$,1,9) <> tables$(i%) then goto copydatadone

           genrec$ = cpyrec$

           put #2, using gen_fmt, genrec$

           write #2, eod goto copyerror
              goto copy_data
        copydatadone
        return
        copyerror
            errormsg$ = "(Copy Error) - Key = " & cpykey$
            gosub error_prompt
        return

        open_error
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return

        error_prompt
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           comp% = 2%
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return



        exit_program

            end



