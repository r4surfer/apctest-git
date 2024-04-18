        REM *************************************************************~
            *                                                           *~
            *   N   N  U   U  M   M   SSS   M   M   AAA    SSS   H   H  *~
            *   NN  N  U   U  MM MM  S      MM MM  A   A  S      H   H  *~
            *   N N N  U   U  M M M   SSS   M M M  AAAAA   SSS   HHHHH  *~
            *   N  NN  U   U  M   M      S  M   M  A   A      S  H   H  *~
            *   N   N   UUU   M   M   SSS   M   M  A   A   SSS   H   H  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * NUMSMASH - TAKES A STRING (UP TO 16 BYTES) AND RETURNS AN *~
            *            INTEGER RESULT CODE.  ALSO MODIFIES THE STRING *~
            *            SO THAT THE NUMBER WILL FIT NICELY INTO 10     *~
            *            BYTES UPON ITS RETURN.                         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 01/21/80 ! ORIGINAL                                 ! BCW *~
            * 06/03/80 ! EXPANSION TO HANDLE 2-DIGIT CASE         ! BCW *~
            * 06/11/81 ! DONT MESS WITH FLOATING POINT            ! TEM *~
            * 08/26/82 ! REWRITTEN WITH CALLS TO NUMFMT           ! ECR *~
            * 01/22/86 ! REWRITTEN WITH CALLS TO CONVERT          ! HES *~
            *************************************************************

            sub "NUMSMASH" (u8,u4,arg$)

            dim number$10

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10022
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10022: REM *************************************************************
            number$ = arg$

        REM  IF U4 IS AN INTEGER, THEN U3 = 0
             u3 = abs(sgn(u4-int(u4)))   /* TEST FOR INTEGER U4        */
             if u3 = 0 then L10210

        REM  U4 IS NOT AN INTEGER (E.G. U4 = .5) THEN NUMBER IS LEFT     ~
             JUSTIFIED WITH A MINIMUM OF TWO DECIMAL PLACES, BUT WITH AS ~
             MANY AS POSSIBLE............................................

              call "CONVERT" (u8, -2.6, number$)
              goto L10230

        REM  IF U4 = 2 THEN NUMBER IS LEFT JUSTIFIED WITH TWO DECIMAL    ~
             PLACES.                                                     ~
                                                                         ~
             IF U4 IS AN INTEGER <> 2 THEN ROUND THE NUMBER TO AN        ~
             INTEGER (ZERO DECIMAL PLACES) AND LEFT JUSTIFY..............

L10210:    if u4 = 2  then call "CONVERT" (u8, -2.2, number$)            ~
                      else call "CONVERT" (u8, -0.001, number$)

L10230:    arg$ = number$
           end
