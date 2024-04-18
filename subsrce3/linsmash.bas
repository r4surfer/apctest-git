        REM *************************************************************~
            *                                                           *~
            *  L      IIIII  N   N   SSS   M   M   AAA    SSS   H   H   *~
            *  L        I    NN  N  S      MM MM  A   A  S      H   H   *~
            *  L        I    N N N   SSS   M M M  AAAAA   SSS   HHHHH   *~
            *  L        I    N  NN      S  M   M  A   A      S  H   H   *~
            *  LLLLL  IIIII  N   N   SSS   M   M  A   A   SSS   H   H   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * LINSMASH - Shoves non-blank lines in an array to the top  *~
            *            of the array.  Example of use is to crunch     *~
            *            addresses.                                     *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/08/86 ! ORIGINAL                                 ! ERN *~
            * 01/20/87 ! Changed for better performance with large! LDJ *~
            *          !   arrays.                                !     *~
            *************************************************************

            sub "LINSMASH" (lines$())

            dim lines$(1)1               /* Input-Output String        */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.19.00 03/13/87 Serial number, Lot tracking     "
        REM *************************************************************
            lines% = dim(lines$(), 1)
            len%   = len(str(lines$(1)))
            if lines% = 1% then end

            x% = 1%
L10141:     if str(lines$(),(x%-1%)*len%+1%) = " " then L10210
L10150:     if lines$(x%) <> " " then L10190
            copy str(lines$(),x%*len%+1%) to str(lines$(),(x%-1%)*len%+1%)
            goto L10150
L10190:     x% = x% + 1%
            if x% < lines% then L10141

L10210:     end


