#include <stdio.h>
#include <stdlib.h>
main(int argc, char* argv[]) 
{
/*      +------------------------------------------------------------+
        |       Rack labels    A12-34-5                              |
        |                    Row-Rack-Bin                            |
        +------------------------------------------------------------+ */

	FILE *input, *output;
	int x, y, z, dash, offSet, offSet2, odd, len;
	int rc, offSet3;  
	char rack[8],arrow[2],bin[2], point[5];
	char rec[257], tmp[257],filler[257];
	offSet = atoi(argv[1]);
	printf("offset is %d\n",offSet);
	input=fopen("racklbl.csv","r");
	output=fopen("racklbl.txt","w");
	fgets(rec,257,input);             
	odd=1;
	while(! feof(input)) {
		for(x=0;x<256;x++) if (rec[x] < ' ') rec[x]=' ';
		y=0;
		z=1;
		for(x=0;x<257;x++) {
		    if (rec[x] == '\"') z = z * -1;
		    if (rec[x] == ',' && z == -1) rec[x] = '/';
		    if (rec[x] != '\"') tmp[y++]=rec[x];
		    }
		strcpy(rack,"");
		y=0;
                for(x=0;x<256&&tmp[x]!=',';x++) 
                  rack[y++]=tmp[x]; 
                rack[y++]='\0';   
		printf("rack = %s\n",rack);
		len=strlen(rack);
		printf("len  = %d\n",len);             
		x++;
		y=0;
		strcpy(arrow,"");
                for(;x<256&&tmp[x]!=',';x++) 
                  arrow[y++]=tmp[x]; 
                arrow[y++]='\0';   
		printf("arrow = %s\n",arrow);
		x++;
		y=0;
		/* find bin # */
		strcpy(bin,"");
		dash=0;
                for(x=0;x<9;x++) { 
                  if (rack[x]=='-') dash++;
                  if (dash==2) {
                      bin[0]=rack[++x];
		      bin[1]='\0';
		      x=9;
                  }
                }
		printf("bin = %s\n",bin);
                if (odd == 1) {
		    fprintf(output,"^JO^XA^EG^XZ^XA^PMN^MNY^MMT^MD0\n");
		    fprintf(output,"^LH0,0^LL600^PR4^JMA\n");
		}
		if (odd==1) 
		    offSet3 = 030;
                else
		    offSet3 = 570;
		offSet2=offSet+40; 
		z=offSet2+20;
		fprintf(output,"^FO%d,%d^GB545,480,20^FS\n",z,offSet3);
		z=offSet2+240;
		offSet3=offSet3+30; 
		if (bin[0]>' ')
		    fprintf(output,"^FO%d,%d^CI0^A0N,195,205^FR^FD%s^FS\n",z,offSet3,bin);
		z=offSet2+180;
		offSet3=offSet3+170;
		fprintf(output,"^FO%d,%d^CI0^A0N,75,45^FR^FD%s^FS\n",z,offSet3,rack);
		z=offSet2+100;
		offSet3=offSet3+70;
		if (len == 6) z+=40;
		if (len == 7) z+=20;
                fprintf(output,"^FO%d,%d^BY3,4.0,200^BCN,90,N,N,N^FR^FD>:%s^FS\n",z,offSet3,rack);
		z=offSet2+160;
		strcpy(point,"    ");
		if (arrow[0]=='R')
         		strcpy(point,"--->");
		if (arrow[0]=='L')
	        	strcpy(point,"<---");
		offSet3=offSet3+90; 
		if (point[0]!=' ')
		    fprintf(output,"^FO%d,%d^CI0^A0N,80,70^FR^FD%s^FS\n",z,offSet3,point);
		z=offSet2+176;
		offSet3=offSet3+30; 
		if (point[0]!=' ')
		     fprintf(output,"^FO%d,%d^GB228,0,12^FS\n",z,offSet3);
                if (odd == -1) 
        	     fprintf(output,"^PQ1^XZ\n");
                odd*=-1;
		for(x=0;x<257;x++) rec[x]='\0';
	        fgets(rec,256,input);             
	}
        if (odd = -1) 
             fprintf(output,"^PQ1^XZ\n");
        fclose(input);
        fclose(output);
}
