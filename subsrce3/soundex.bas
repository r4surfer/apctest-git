        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS    OOO   U   U  N   N  DDDD   EEEEE  X   X          *~
            *  S      O   O  U   U  NN  N  D   D  E       X X           *~
            *   SSS   O   O  U   U  N N N  D   D  EEEE     X            *~
            *      S  O   O  U   U  N  NN  D   D  E       X X           *~
            *   SSS    OOO    UUU   N   N  DDDD   EEEEE  X   X          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SOUNDEX  - This routine is used to create a Soundex code, *~
            *            which is a numeric code that represents the    *~
            *            sound of a name.  In can be used for in any    *~
            *            system / file in which peoples names or the    *~
            *            the descriptive name of other objects are      *~
            *            stored.  The Soundex code may be used to       *~
            *            search a file for the 'proper' sound  of a     *~
            *            user supplied name/description, no matter how  *~
            *            the name is spelled, or if the user isn't sure *~
            *            how it should be spelled (or even if the user  *~
            *            has the wrong spelling).                       *~
            *            To obtain the desired results, the routine     *~
            *            should first be used to create the Soundex code*~
            *            for the name to find/search for.  Then read    *~
            *            the file sequentially, obtain the name from the*~
            *            record area, use this routine to create another*~
            *            Soundex code, and then compare the two to see  *~
            *            if they're equal.  Continue with this process  *~
            *            until you have enough matches to fill the      *~
            *            screen or the end-of-file is reached.          *~
            *            Hint;  your search program should use this     *~
            *            in combination with a partial comparision      *~
            *            match - i.e. if the first part of the record   *~
            *            name field = the partial name keyed in by the  *~
            *            user then that is also a match.                *~
            *                                                           *~
            *            This subroutine is called using two arguments; *~
            *                 NAME$       CH(XX)                        *~
            *                        and                                *~
            *                 CODE$       CH(04)                        *~
            *                                                           *~
            *            The calling syntax from a BASIC program is     *~
            *                 CALL "SOUNDEX" (NAME$, CODE$)             *~
            *            Where NAME$ is the name to be encoded and      *~
            *            CODE$ is the number computed by this routine   *~
            *            and returned to the caller (NAME$ is           *~
            *            unchanged).                                    *~
            *            The code is calculated using the following     *~
            *            rules;                                         *~
            *              1) All occurences of non-alphabetic          *~
            *                 characters are ignored.                   *~
            *              2) The case (upper or lower) is ignored.  All*~
            *                 characters are translated to Upper Case   *~
            *                 for processing.                           *~
            *              3) The following numbers are assigned to each*~
            *                 letter;                                   *~
            *                 1 = B,F,P,V,PH                            *~
            *                 2 = C,G,J,K,Q,S,X,Z                       *~
            *                 3 = D,T                                   *~
            *                 4 = L                                     *~
            *                 5 = M,N                                   *~
            *                 6 = R                                     *~
            *                 9 = A,E,H,I,O,U,W,Y                       *~
            *              4) Consecutive occurences of a digit are     *~
            *                 compressed to one digit.                  *~
            *              5) All 9's are removed (keeping only         *~
            *                 consonants).                              *~
            *              6) If the calculated code is longer than four*~
            *                 digits (a long name) it is truncated at   *~
            *                 the end.  If it is shorter (a short name),*~
            *                 trailing zeroes are added.                *~
            *                                                           *~
            *            Note;  Adventurous souls may want to experiment*~
            *                   with codes longer than 4 digits.        *~
            *                   Theoretically this should allow for more*~
            *                   exact matches and fewer hits.           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/25/86 ! Original (translated from Public Domain  ! LDJ *~
            *          !   code published by Software Business    !     *~
            *          !   Applications of Chicago).              !     *~
            * 05/05/87 ! Added support for "PH" sounds like "F".  ! LDJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SOUNDEX" (name$, code$)

        dim                                                              ~
            char$1,            /* Temporary Work Variable              */~
            char2$1,           /* Temporary Work Variable              */~
            code$4,            /* Returned SOUNDEX Code                */~
            name$30            /* Passed in Name or Description        */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "
        REM *************************************************************


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            code$ = "0000"
            dup%, x%, b% = 0%

        REM *************************************************************~
            *             M A I N   P R O C E S S I N G                 *~
            *-----------------------------------------------------------*~
            * Main Processing begins here.                              *~
            *************************************************************
        start_loop
            x% = x% + 1%
            if x% > len(name$) then exit_loop
            char$ = str(name$,x%,1%)
            char$ = and hex(df)           /* Translate lower to Upper   */
            char2$ = str(name$,max(x%-1%,1%),1%) and hex(df)
            if char$ < "A" or char$ > "Z" then start_loop
            if char$ = "H" and char2$ = "P" then char$ = "A"
            tran(char$,"9A1B2C3D9E1F2G9H9I2J2K4L5M5N9O1P2Q6R2S3T9U1V9W2X9~
        ~Y2Z")replacing                   /* Translate to Special Codes */
            if char$ = "9" then start_loop
            if dup% = 0% then L10160
               if str(code$,b%,1%) = char$ then start_loop
L10160:     if b% = 0% or char$ <> "9" then L10190
                  dup% = 0%
                  goto start_loop
L10190:     b% = b% + 1%
            str(code$,b%,1%) = char$
            dup% = 1%
            if b% < 4% then start_loop

        exit_loop
            end
