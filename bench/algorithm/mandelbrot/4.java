/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
   
   modified by Henco Appel
*/

import java.io.*;
import java.util.concurrent.atomic.*;
import java.util.stream.*;

public final class app {

   static final byte getByte(final double[] Crb, final double CibY, final int x){
      int res=0;
      for(int i=0;i<8;i+=2){
         double Zr1=Crb[x+i];
         double Zi1=CibY;

         double Zr2=Crb[x+i+1];
         double Zi2=CibY;

         int b=0;
         int j=49;do{
            double nZr1=Zr1*Zr1-Zi1*Zi1+Crb[x+i];
            Zi1=Zr1*Zi1+Zr1*Zi1+CibY;
            Zr1=nZr1;

            double nZr2=Zr2*Zr2-Zi2*Zi2+Crb[x+i+1];
            Zi2=Zr2*Zi2+Zr2*Zi2+CibY;
            Zr2=nZr2;

            if(Zr1*Zr1+Zi1*Zi1>4){b|=2;if(b==3)break;}
            if(Zr2*Zr2+Zi2*Zi2>4){b|=1;if(b==3)break;}
         }while(--j>0);
         res=(res<<2)+b;
      }
      return (byte)(res^-1);
   }

   public static void main(String[] args) throws Exception {
      int N=6000;
      if (args.length>=1)
		  N=Integer.parseInt(args[0]);

      double[] Crb=new double[N+7];
      double invN=2.0/N;
	  for(int i=0;i<N;i++){  Crb[i]=i*invN-1.5; }
	  int lineLen = (N-1)/8 + 1;
	  byte[] data = new byte[N*lineLen];
	  IntStream.range(0,N).parallel().forEach(y -> {
		  double Ciby = y*invN-1.0;
		  int offset = y*lineLen;
		  for(int x=0; x<lineLen; x++)
			  data[offset+x] = getByte(Crb, Ciby, x*8);
	  });

      OutputStream stream = new BufferedOutputStream(System.out);
      stream.write(("P4\n"+N+" "+N+"\n").getBytes());
      stream.write(data);
      stream.close();
   }
}