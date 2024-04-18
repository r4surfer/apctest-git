import java.io.*;
import java.lang.*;
import java.util.*;
public class DesLbl extends Object
{
     public static void main(String[] args)
     {
      try
      {
           BufferedReader input = new BufferedReader(new FileReader("label.csv"));
           PrintWriter output = new PrintWriter(new FileWriter("label.zpl"));
          
	int x, y, z, maxPage, offSet, sf;
	int rc, argv, cards, lp;  
	String rec = new String("");
	String pages,pour,qtyUM,part,desc,subInv, tmp, filler, qtyAmt;
	String [] strArray = new String[16];
	pages = "1 OF 10";
	cards = 0;
	pour = "BMDH";
	qtyUM = "EA";
	part = "";
	desc = "";
	subInv = "100";
	qtyAmt = "";
	offSet = 0;	
	argv = args.length;
	if (argv > 0) offSet = Integer.parseInt(args[0].trim());
	System.out.println("offset is " + offSet);
	
	while((rec = input.readLine()) != null)
	{
           System.out.println(rec);
	   y=0;
	   x=0;
	   sf = 1;
	   z = rec.length();
	   strArray[0] = ""; 
           while(x < z) {
	       if (rec.charAt(x) == '\"') sf = sf * -1; 
	       if (sf == 1 && (x >= z || rec.charAt(x) == ',')) {
                   y++;
                   //x++;
		   strArray[y]="";
               }
	       else
	        if (x < z && rec.charAt(x) != '\"' && (rec.charAt(x) != ',' || sf == -1)) strArray[y] = strArray[y] + rec.charAt(x);
		x++;
           }
	   part = strArray[0].trim();
	   desc = strArray[1].trim();
	   cards = Integer.parseInt(strArray[2].trim());
	   qtyAmt = strArray[3].trim();
	   qtyUM = strArray[4].trim();
	   lp = 1;
	   while(lp<=cards) {
	        pages = lp + " OF " + cards;
        	output.println("^JO^XA^EG^XZ^XA^PMN^MNY^MMT^MD0");
		output.println("^LH0,0^LL600^PR4^JMA");
                z=offSet+5;
		output.println("^FO830," + z + "^CI0^A0N,95,45^FR^FD" + pages + "^FS");
		output.println("^FO305," + z + "^CI0^A0N,50,85^FR^FD" + part + "^FS");
		z=offSet+55;
		output.println("^FO305," + z + "^BY3,2.0,100^BCN,100,N,N,N^FR^FD>:" + part + "^FS");
		z=offSet+167;
		output.println("^FO375," + z + "^CI0^A0N,50,20^FR^FD" + desc + "^FS");
		z=offSet+385;
		output.println("^FO60," + z + "^CI0^A0N,75,45^FR^FDSUB INV^FS");
		z=offSet+445;
		output.println("^FO45," + z + "^BY3,2.0,80^BCN,80,Y,N,N^FR^FD>:" + subInv + "^FS");
		z=offSet+250;
		output.println("^FO450," + z + "^CI0^A0N,75,30^FR^FD" + qtyAmt + "  " + qtyUM + "^FS");
		z=offSet+330;
		output.println("^FO375," + z + "^BY3,2.0,80^BCN,80,N,N,N^FR^FD>:" + qtyAmt + "^FS");
		z=offSet+380;
		output.println("^FO812," + z + "^CI0^A0N,75,25^FR^FDPOUR^FS");
		z=offSet+455;
		output.println("^FO765," + z + "^CI0^A0N,125,60^FR^FD" + pour + "^FS");
		output.println("^PQ1^XZ");
		lp++;
	   }
         }



           input.close();
           output.close();
          
           System.out.println("DesLbl finished.");
      }
      catch(IOException e) {
           System.err.println("File not found.");
      }
     }
}
 
