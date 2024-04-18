        REM *************************************************************~
            *                                                           *~
            *   N   N  U   U  M   M  V   V   AAA   L     IIIII  DDDD    *~
            *   NN  N  U   U  MM MM  V   V  A   A  L       I    D   D   *~
            *   N N N  U   U  M M M  V   V  AAAAA  L       I    D   D   *~
            *   N  NN  U   U  M   M   V V   A   A  L       I    D   D   *~
            *   N   N   UUU   M   M    V    A   A  LLLLL IIIII  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * NUMVALID - TAKES A STRING OF UP TO 10 BYTES, VERIFIES THAT*~
            *            IT IS A VALID NUMBER, CONVERTS IT AND REFORMATS*~
            *            IT USING CODE RIPPED OFF FROM NUMSMASH TO      *~
            *            REWRITE IN A NICE NEW FORMAT.                  *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+------------------WHAT--------------------+-WHO-*~
            * 01/21/80 ! ORIGINAL (SWIPED FROM NUMSMASH)          ! BCW *~
            * 08/24/82 ! REWRITTEN WITH CALLS TO NUMFMT           ! ECR *~
            * 04/16/84 ! CORRECT BUGS  10270, 10290 (ARG$ -> Z1$) ! ECR *~
            * 01/22/86 ! REWRITTEN WITH CALLS TO CONVERT          ! HES *~
            *************************************************************

            sub "NUMVALID" (z1$, error%, u4)

            dim number$10

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10022
            cms2v$ = "04.15.09 01/27/86 Object code size and dfloat    "
L10022: REM *************************************************************
            number$ = z1$
            error% = 1

            convert number$ to number, data goto L10070
            goto L10090
L10070:     end

L10090: REM  IF U4 IS AN INTEGER, THEN U3 = 0
             u3 = abs(sgn(u4-int(u4)))   /* TEST FOR INTEGER U4        */
             if u3 = 0 then L10270

        REM  U4 IS NOT AN INTEGER (E.G. U4 = .5) THEN NUMBER IS LEFT     ~
             JUSTIFIED WITH A MINIMUM OF TWO DECIMAL PLACES, BUT WITH AS ~
             MANY AS POSSIBLE............................................

              call "CONVERT" (number, 2.6, number$)
              if pos(number$="#") = 0 then L10310
              end

        REM  IF U4 = 2 THEN NUMBER IS LEFT JUSTIFIED WITH TWO DECIMAL    ~
             PLACES.                                                     ~
                                                                         ~
             IF U4 IS AN INTEGER <> 2 THEN ROUND THE NUMBER TO AN        ~
             INTEGER (ZERO DECIMAL PLACES) AND LEFT JUSTIFY..............

L10270:    if u4 = 2  then call "CONVERT" (number, 2.2, number$)         ~
                      else call "CONVERT" (number, 0.0, number$)
           if pos(number$="#") <> 0 then end

L10310:    arg$ = number$
           if arg$ = number$ then error% = 0  /* Incase length is diff */
           end
