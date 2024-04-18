main()
{
 char reset[5];

 reset[0] = 0x1b;   /* ESC */
 reset[1] = 0x5b;   /* [ */
 reset[2] = 0x21;   /* ! */
 reset[3] = 0x70;   /* p */
 reset[4] = 0x0;    /* null */
 printf("%s",reset);
}
