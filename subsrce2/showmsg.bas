        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   H   H   OOO   W   W  M   M   SSS    GGGG          *~
            *  S      H   H  O   O  W   W  MM MM  S      G              *~
            *   SSS   HHHHH  O   O  W W W  M M M   SSS   G GGG          *~
            *      S  H   H  O   O  WW WW  M   M      S  G    G         *~
            *   SSS   H   H   OOO   W   W  M   M   SSS    GGGG          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHOWMSG  - General routine for display of a message up to *~
            *            80 characters in length.  Used throughout the  *~
            *            CMS System.  Calling syntax is;                *~
            *            CALL "SHOWMSG" ("abc....") or ( String Var )   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/13/82 ! ORIGINAL                                 ! GLW *~
            * 08/15/85 ! Modified to Give a "Windowing" Effect    ! LDJ *~
            *          !   using WSXIO,  Message length lengthened!     *~
            *          !   from 79 characters to 80 characters.   !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


           sub "SHOWMSG" (msg$)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.15.01 08/20/85 General Ledger & Purchasing    "
        REM *************************************************************
           call "SHOSTAT" (msg$)

           end
