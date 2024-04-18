        REM **************************************************************~
            * subroutine to determine users schema                       *~ 
            *------------------------------------------------------------*~
            * 03/01/06 ! original                                  - CMG *~
            **************************************************************

        sub "SCHEMA" (schema$,          /* What switch 1-NC 2-NE      */~
                       schema%,          /* Schema                     */~
                       #1,               /* GENCODES                   */~
                       err% )            /* error                      */


        dim schema$8,                    /* Schema switch              */~
            schema$(10)8,                /* Database schemas           */~
            schema%(10),                 /* Schema Values              */~
            database$8,                  /* Users database setting     */~
            jobname$8,                   /* Job Name                   */~
            gen_key$50,                  /* Generic Readkey            */~
            gen_desc$100                 /* Generic Description        */



* Get database ie apcdata or nedata and jobname ie program name
        err% = 99%          /* Assume error */
        call "EXTRACT" addr("IL", database$, "JN", jobname$ ) /* (AWD017) */
        max_schema% = 10%
        gosub load_schema                                     /* (AWD017) */

        call "UPPERCASE" (database$, err%)


        for i% = 1% to max_schema%
            if schema$(i%) = database$ then goto schema_found
        next i%

* Exit Subroutine
               end


        schema_found
            schema% = schema%(i%)
            schema$ = schema$(i%)
            err% = 0%

        end



        load_schema
              init(" ") gen_key$, gen_desc$ ,schema$() 
              cnt% = 1%
              mat schema% = zer
              str(gen_key$,1%,9%) = "SCHEMA"


        schema_nxt
              read #1, key > gen_key$, using LSCHEMA, gen_key$, gen_desc$,  ~
                                                      eod goto schema_done
LSCHEMA:                FMT CH(24), CH(30)
                      if str(gen_key$,1%,6%) <> "SCHEMA" then goto schema_done

                      schema$(cnt%) = str(gen_key$,10%,8%)
                     
                      convert str(gen_desc$,1%,1%) to schema%(cnt%),        ~
                                                        data goto schema_nxt


                          cnt% = cnt% + 1%
                          if cnt% > max_schema% then cnt% = max_schema%

                          goto schema_nxt

        schema_done
        return



