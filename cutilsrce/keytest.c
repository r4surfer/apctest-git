#include<string.h>
#include<unistd.h>
#include<stdlib.h>
#include<termios.h>
#include<curses.h>
#include<term.h>

#ifdef TRUE
#undef TRUE
#endif

#ifdef FALSE
#undef FALSE
#endif

#include "wvsb0.h"

char *      cstring( char *key, int h) {
static char s[50];
int         i;

        sprintf( s, "Key F%i = ", h );

        if( key != NULL )
        if(*key != 0x00 ) {
                for( i = 0; i < strlen(key); i++ ) {
                        strcat( s, "%.2x " );
                }
        }

        return(s);
}

void hex_key( char *work, char *format, char *string ) {
	sprintf( work, format,
	         string[0],         string[1],
	         string[2],         string[3],
	         string[4],         string[5],
	         string[6],         string[7],
	         string[8],         string[9],
	         string[10],        string[11],
	         string[12],        string[13],
	         string[14],        string[15],
	         string[16],        string[17],
	         string[18],        string[19],
	         string[20]
	         );
}
 
int main(char *argv, int argc ){
        char c[2], key[16][200];
        int stat;

        initscr();

        nodelay(stdscr,TRUE);
        while(getch()!=ERR){}
        nodelay(stdscr,FALSE);
        keypad(stdscr,TRUE);
        putp(keypad_local);

        hex_key( key[0],  cstring( key_f1, 1), key_f1 );
        hex_key( key[1],  cstring( key_f2, 2), key_f2 );
        hex_key( key[2],  cstring( key_f3, 3), key_f3 );
        hex_key( key[3],  cstring( key_f4, 4), key_f4 );
        hex_key( key[4],  cstring( key_f5, 5), key_f5 );
        hex_key( key[5],  cstring( key_f6, 6), key_f6 );
        hex_key( key[6],  cstring( key_f7, 7), key_f7 );
        hex_key( key[7],  cstring( key_f8, 8), key_f8 );
        hex_key( key[8],  cstring( key_f9, 9), key_f9 );
        hex_key( key[9],  cstring( key_f10,10), key_f10);
        hex_key( key[10], cstring( key_f11,11), key_f11);
        hex_key( key[11], cstring( key_f12,12), key_f12);
        hex_key( key[12], cstring( key_f13,13), key_f13);
        hex_key( key[13], cstring( key_f14,14), key_f14);
        hex_key( key[14], cstring( key_f15,15), key_f15);
        hex_key( key[15], cstring( key_f16,16), key_f16);

        endwin();

        printf( "\n\n" );

        for( stat = 0; stat < 16 ; stat++ ) {
                printf( "%s\n", key[stat] );
        }

        printf( "\n\n" );
	return( 0x6a );
}

