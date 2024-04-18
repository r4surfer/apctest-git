        REM              AS OF 04/21/97       ( TXTSHOW)
        REM    PASS (1)  DISPLAY THE TYPE "N" RECORD POINTER VALUE
        REM

        dim txt$11,  rslt$20, textid$4
        dim rec$30, rhh$12

            select #1,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            call "OPENCHCK" (#1, x%, y%,  0%, rslt$)

         display                                                         ~
            at(01,17), "Display the Current Record Counter in (TXTFILE)"

        REM - SETTING OF 1ST SEVEN (7) CHARACTERS OF KEY WORKS
              x%, y% = 0%
              txt% = 0%
              textid$ = all(hex(ff))
              call "TXTFUTIL" (#1, y%, "INTL", textid$)  /* LOAD TEXT */
        REM NEXT_REC
              txt$ = all(hex(00))
              str(txt$,1%,1%) = "N"
              read #1,hold,key > txt$, using L00260, txt$, eod goto L00330
L00260:           FMT CH(11)

              get #1,using L00290  , rec$
L00290:          FMT CH(30)
              get str(rec$,12%,4%), using L00310  , txt%
L00310:          FMT BI(4)
              convert txt% to rhh$, pic(##########)
L00330:       call "SHOSTAT" ("CURRENT VALUE = " & rhh$)
              stop
        end

