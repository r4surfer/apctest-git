        REM *************************************************************~
            *                                                           *~
            *  RRRR   JJJJJ  U   U   SSS   TTTTT  IIIII  FFFFF  Y   Y   *~
            *  R   R    J    U   U  S        T      I    F      Y   Y   *~
            *  RRRR     J    U   U   SSS     T      I    FFFF    YYY    *~
            *  R   R  J J    U   U      S    T      I    F        Y     *~
            *  R   R   J      UUU    SSS     T    IIIII  F        Y     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RJUSTIFY - Takes a text string and right-justifies it,    *~
            *            returning the result in the input string.      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/12/80 ! ORIGINAL                                 ! BCW *~
            * 11/27/83 ! LENGTHENED STRINGS TO 100 CH             ! GLW *~
            * 01/22/86 ! Changed in the interest of efficiency.   ! LDJ *~
            *************************************************************

            sub "RJUSTIFY" (input$)
                dim input$100,           /* INPUT ARGUMENT             */~
                    output$100           /* OUTPUT STRING.             */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10042
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10042: REM *************************************************************
                output$ = input$
                len% = len(str(input$))
                input$ = " "
                str(input$, 1% + len% - len(output$)) = output$
                end

