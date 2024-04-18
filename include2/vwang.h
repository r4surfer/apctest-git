#define TREE_MAX 1000

struct {
        unsigned long eq;               /* Pointer to Child   */
        unsigned long gt;               /*  " Greater Than    */
        unsigned long lt;               /*  " Less Than       */
        unsigned long order;            /* Char Sequence Num. */
        unsigned long prev;             /* Pointer to Parent  */
        unsigned long key_value;        /* Extended Character */
        char value;                     /* Char in sequence   */
} key_tree[TREE_MAX+1];

void add_rest_of_sequence ( char *, unsigned int *, unsigned int , unsigned int  );
void build_tree ( char *, unsigned int );
void Init_Key_Tree();
int  MatchExtendedChar ( int * );
int  positionCursorTab ( int , int , char * );
void pushchar ( int );
void vwang_timer ( S32 );


/* These are the function definitions of what the macros are supposed to do */

#ifdef NO_FAC_MACROS

void readScreen(char *, char * );

int isfac      ( char );    /* 0 if not FAC         */
int isfacline  ( char );    /* Underline            */
int isfacbright( char );    /* Bright               */

int isfacdim   ( char );    /* Dim                  */
int isfacblink ( char );    /* Blinks               */
int isfacblank ( char );    /* Blank                */
int isfacall   ( char );    /* Up/Low/Number ...    */
int isfacupr   ( char );    /* Upper Only           */
int isfacnum   ( char );    /* Number Only          */
int isfacmodify( char );
int isfacprotect(char );

void readScreen(char *wsb, char *wsbCopy){
        memmove( &wsb[4], wsbCopy, 1920); }
int isfac( char c ) {
        if( c & 0x80 && ((c & 0x03) < 0x03)) return(1); else return(0);}
int isfacline  ( char c ) {
        if( isfac(c) && (c & 0x20) ) return(1); else return(0);}
int isfacbright( char c ) {
        if( isfac(c) && ((c & 0x18) == 0x00))return(1); else return(0);}
int isfacdim   ( char c ) {
        if(isfac(c) && ((c & 0x18) == 0x08)) return(1); else return(0);}
int isfacblink ( char c ) {
        if( isfac(c) && ((c & 0x18) == 0x10))return(1); else return(0);}
int isfacblank ( char c ) {
        if( isfac(c) && ((c & 0x18) == 0x18)) return(1); else return(0);}
int isfacmodify( char c ) {
        if( isfac(c) && ((c & 0x04) == 0) ) return(1); else return(0);}
int isfacprotect( char c ) {
        if( isfac(c) && ((c & 0x04) == 4) ) return(1); else return(0);}
int isfacall ( char c ) {
        if( isfac(c) && ((c & 0x03) == 0) ) return(1); else return(0);}
int isfacupr ( char c ) {
        if( isfac(c) && ((c & 0x03) == 1) ) return(1); else return(0);}
int isfacnum ( char c ) {
        if( isfac(c) && ((c & 0x03) == 2) ) return(1); else return(0);}

#else

#define readScreen(wsb, wsbCopy) memmove( &wsb[4], wsbCopy, 1920);
#define isfac( c )       ((c & 0x80) && ((c & 0x03) < 0x03))
#define isfacline( c )   (isfac(c) && (c & 0x20) )
#define isfacbright( c ) (isfac(c) && ((c & 0x18) == 0x00))
#define isfacdim( c )    (isfac(c) && ((c & 0x18) == 0x08))
#define isfacblink( c )  (isfac(c) && ((c & 0x18) == 0x10))
#define isfacblank( c )  (isfac(c) && ((c & 0x18) == 0x18))
#define isfacmodify( c ) (isfac(c) && ((c & 0x04) == 0) )
#define isfacprotect( c )(isfac(c) && ((c & 0x04)) )
#define isfacall( c )    (isfac(c) && ((c & 0x03) == 0) )
#define isfacupr( c )    (isfac(c) && ((c & 0x03) == 1) )
#define isfacnum( c )    (isfac(c) && ((c & 0x03) == 2) )
#define isfactab( c )    (isfac(c) && ((c & 0x06) == 6) )
/*
#define pushchar( d )          ( (sprintf(                                   \
                                      &vwang_type_ahead_buffer[strlen(vwang_type_ahead_buffer)], \
                                      "%c", d )) )
*/
#define v_out_c( c )    write( STDOUT_FILENO, &c, 1 )
#define v_out_s( c )    if( isgoodaddr(c) ) write( STDOUT_FILENO, c, strlen(c) )
#define v_out_l( c, d ) write( STDOUT_FILENO, c, d )
#endif

#ifdef ALPHA
#define isgoodaddr( c ) ( (c != NULL) )
#else
#define isgoodaddr( c ) ( (c != NULL) && !((long)c & 0x80000000) )
#endif

/********************************************************************************************************************************/
/*						VWANG definitions								*/
/********************************************************************************************************************************/

#define WS_MAX_LINES_PER_SCREEN     24							/* Maximum lines per screen.		*/
#define WS_MAX_COLUMNS_ON_A_LINE   132							/* Maximum characters per line.		*/
#define WS_DEFAULT_COLUMNS_PER_LINE 80							/* Default characters per line.		*/
/*					VWANG function definitions								*/
#define DISPLAY			   0							/* COBOL DISPLAY verb.			*/
#define WRITE_ALL		   1							/* Write to screen.			*/
#define READ_ALL		   2							/* Read from screen.			*/
#define DISPLAY_AND_READ	   3							/* Write then read screen.		*/
#define CLOSE_WORK_STATION	   4							/* Close the workstation.		*/
#define WRITE_SELECTED		   5							/* Write selected fields.		*/
#define READ_ALTERED		   6							/* Read altered fields.			*/
#define READ_MODIFIABLE           10							/* Read modifiable fields.		*/
#define DISPLAY_AND_READ_ALTERED   7							/* Write then read altered.		*/
#define LOAD_SUB_TABLE		   8							/* Load user defined substitution table.*/
#define INIT_DEFAULT_TABLE	   9							/* Init the sub table to default values.*/
#define WRITE_ALL_PB		  11							/* Write to screen. With PsuedoBlanks   */
#define READ_ALL_PB		  12
#define DO_NOTHING                13							/* Read from screen. With PsuedoBlanks  */
/*					WCC (Write Control Character) definitions						*/
#define SOUND_ALARM		0100							/* Ring the bell.			*/
#define UNLOCK_KEYBOARD		0200							/* Unlock the keyboard.			*/
#define ROLL_UP			0010							/* Scroll screen up.			*/
#define ROLL_DOWN	    	0020							/* Scroll screen down.			*/
#define POSITION_CURSOR		0040							/* Position cursor.			*/
#define ERASE_AND_PROTECT	0002							/* Erase and protect.			*/
#define ERASE_FIELDS		0004							/* Output pseudoblanks.			*/
/*					Indeces for VWANG screen Order Area							*/
#define ROW_NUMBER_BYTE		0							/* Starting row to display.		*/
#define WCC_BYTE		1							/* Hold WCC for screen.			*/
#define CURSOR_COL_BYTE		2							/* Position cursor column.		*/
#define CURSOR_ROW_BYTE		3							/* Position cursor row.			*/

#define FAC_CHARACTER		0200							/* FAC ID bit.				*/
#define FAC_ALTERED		0100							/* Altered read bit.			*/
#define FAC_SELECTED		0100							/* Selected write bit.			*/
#define FAC_UNDERSCORE  	0040							/* Underscore bit.			*/
#define FAC_BLINKING		0020							/* Blinking bit.			*/
#define FAC_LOW_INTENSITY	0010							/* Low intensity bit.			*/
#define FAC_PROTECT		0004							/* Protected char bit.			*/
#define FAC_NUMERIC_ONLY	0002							/* Numeric only bit.			*/
#define FAC_UPPERCASE_ONLY	0001							/* Uppercase only bit.			*/

/********************************************************************************************************************************/
/*					DEC substitution characters.								*/
/********************************************************************************************************************************/
#define WANG_MENU_PICK		0x05							/* Input from Wang for menu pick item.	*/
#define DEC_MENU_PICK		0x5F							/* Underscore for menu pick.		*/
#ifdef VMS 
#define CURRENT_MENU_PICK	0xBB							/* Double right arrow.			*/
#endif
#ifdef unix
#define CURRENT_MENU_PICK	0x2A							/* Asterisk.				*/
#endif
#define DEC_GRAPHIC_DIAMOND	0x60							/* Graphic diamond.			*/
#define DEC_GRAPHIC_BOX		0x61							/* Graphic box.				*/
#define DEC_GRAPHIC_DEGREE	0x66							/* Graphic degree symbol.		*/
#define DEC_PLUS_MINUS		0x67							/* Graphic plus/minus sign.		*/
#define DEC_BOTM_RIGHT_BOX	0x6A							/* Graphic lower right corner of box.	*/
#define DEC_TOP_RIGHT_BOX	0x6B							/* Graphic top right corner of box.	*/
#define DEC_TOP_LEFT_BOX	0x6C							/* Graphic top left corner of box.	*/
#define DEC_BOTM_LEFT_BOX	0x6D							/* Graphic lower left corner of box.	*/
#define DEC_FOUR_CORNER		0x6E							/* Graphic (plus) four corners meet.	*/
#define DEC_GRAPHIC_HORIZ	0x71							/* Graphic horizontal.			*/
#define DEC_VERT_RHORIZ		0x74							/* Graphic |- .				*/
#define DEC_VERT_LHORIZ		0x75							/* Graphic -| .				*/
#define DEC_HORIZ_TVERT		0x76							/* Graphic horiz. with top vertical.	*/
#define DEC_HORIZ_BVERT		0x77							/* Graphic horiz. with bottom vertical.	*/
#define DEC_GRAPHIC_VERT	0x78							/* Graphic | .				*/
#define DEC_GRAPHIC_PI		0x7B							/* Graphic pi.				*/
#define DEC_VERTICAL_BAR	0x7C							/* DEC vertical bar or not equal sign.	*/
#define DEC_GRAPHIC_UK_POUND	0x7D							/* Graphic U.K. - pound sign.		*/
#define DEC_CENTERED_PERIOD	0x7E							/* Graphic centered period.		*/
#define LPN_FAC			0x8E							/* Low Protect Numeric FAC.		*/
#define DEC_EXCLAMATION		0xA1							/* Sp.graphic upside dowm exclamation.	*/
#define DEC_CENTS_SIGN		0xA2							/* Special graphic cents sign.		*/
#define DEC_GRAPHIC_SQUARE	0xA8							/* Special graphic square box.		*/
#define DEC_DBL_LEFT_ARROW	0xAB							/* Special graphic double left arrow.	*/
#define DEC_SUPSCPT_2		0xB2							/* Special graphic superscript 2.	*/
#define DEC_SUPSCPT_3		0xB3							/* Special graphic superscript 3.	*/
#define DEC_PARAGRAPH		0xB6							/* Special graphic paragraph symbol.	*/
#define DEC_DBL_RIGHT_ARROW	0xBB							/* Special graphic double right arrow.	*/
#define DEC_ONE_FOURTH		0xBC							/* Special graphic One fourth symbol.	*/
#define DEC_ONE_HALF		0xBD							/* Special graphic One half symbol.	*/
#define DEC_QUESTION_MARK	0xBF							/* Sp.graphic upside down question mark.*/
