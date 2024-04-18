        REM *************************************************************~
            *                                                           *~
            *   N   N  U   U  M   M  PPPP   RRRR   IIIII  N   N  TTTTT  *~
            *   NN  N  U   U  MM MM  P   P  R   R    I    NN  N    T    *~
            *   N N N  U   U  M M M  PPPP   RRRR     I    N N N    T    *~
            *   N  NN  U   U  M   M  P      R   R    I    N  NN    T    *~
            *   N   N   UUU   M   M  P      R   R  IIIII  N   N    T    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * NUMPRINT - FORMATS A NUMBER RIGHT JUSTIFIED IN PRINT FMT  *~
            *            10 - CHARACTER STRING.  NOTE THAT IMAGES WILL  *~
            *            GIVE YOU 2 PLACES ARBITRARILY BUT THIS WILL    *~
            *            GIVE AT LEAST 2, AND AS MANY AS POSSIBLE.      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 03/21/80 ! ORIGINAL (FROM NUMSMASH)                 ! BCW *~
            * 09/26/80 ! EXPANSION TO HANDLE 2 DECIMAL PLACES CASE! TEM *~
            * 06/11/81 ! DONT MESS WITH FLOATING POINT            ! TEM *~
            * 08/26/82 ! REWRITTEN WITH CALLS TO NUMFMT           ! ECR *~
            * 01/22/86 ! REWRITTEN WITH CALLS TO CONVERT!         ! HES *~
            *************************************************************

            sub "NUMPRINT" (u8,u4,arg$)

            dim number$10

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10042
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10042: REM *************************************************************
            number$ = arg$

        REM U4 <> 2  GIVES A RIGHT JUSTIFIED NUMBER WITH AT LEAST 2      ~
            DECIMAL PLACES BUT AS MANY AS POSSIBLE, WITH ROUNDING.......

        REM U4  = 2  GIVES A RIGHT JUSTIFIED NUMBER ROUNDED TO ONLY 2    ~
            DECIMAL PLACES (FOR $$$$ AND THE LIKE)......................

            if u4 <> 2 then call "CONVERT" (u8, 2.6, number$) else       ~
                            call "CONVERT" (u8, 2.2, number$)

            arg$ = number$
            end
