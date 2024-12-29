//tranlated from Zig
//
//gcc -O3 -mno-fma -march=native -Wall main.c
//clang -O3 -Wno-deprecated -mno-fma -mllvm -polly -mllvm -polly-parallel -lgomp -mllvm -polly-vectorizer=stripmine -lm -o main main.c -fopenmp=libomp -march=native -lcrypto

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <openssl/md5.h>

#define VEC_SIZE 8

typedef struct Vec
{
    double x[VEC_SIZE];
} Vec;

void fill(Vec* in,double val)
{
    for(int i=0;i<VEC_SIZE;++i)
    {
        in->x[i]=val;
    }
        
}

void mul(Vec* r,Vec* a,Vec* b)
  {
      for(int i=0;i<VEC_SIZE;++i)
      {
          r->x[i]=a->x[i]*b->x[i];
      }
  }

void minus(Vec* r,Vec* a,Vec* b)
  {
      for(int i=0;i<VEC_SIZE;++i)
      {
          r->x[i]=a->x[i]-b->x[i];
      }
  
  }

void add(Vec* r,Vec* a,Vec* b)
{
    for(int i=0;i<VEC_SIZE;++i)
    {
        r->x[i]=a->x[i]+b->x[i];
    }
}

unsigned char mbrot8(Vec* cr, double civ)
{
    Vec ci,zr,zi,tr,ti,absz;
    fill(&ci,civ);
    fill(&zr,0.0);
    fill(&zi,0.0);
    fill(&tr,0.0);
    fill(&ti,0.0);
    fill(&absz,0.0);
    Vec tmp;

    for(int i=0;i<10;++i)
    {
       for(int j=0;j<5;++j)
       {
           add(&tmp, &zr, &zr);
		   mul(&tmp, &tmp, &zi);
		   add(&zi, &tmp, &ci);

		   minus(&tmp, &tr, &ti);
           add(&zr, &tmp, cr);
           mul(&tr, &zr, &zr);
		   mul(&ti, &zi, &zi);
       }

       add(&absz,&tr,&ti);
       unsigned char terminate = true;
       for(int k=0;k<VEC_SIZE;++k)
       {
           if(absz.x[k]<=4.0)
           {
               terminate=false;//0
               break;
           }
       }
       if(terminate)
       {
           return 0;
       }

    }

    char accu=0;
    for(int k=0;k<VEC_SIZE;++k)
    {
        if(absz.x[k]<=4.0)
             {
                 const unsigned char lhs = 0x80;
                 accu |= (lhs >> k);
             }

    }

return accu;
}

double init_xloc(int i,double inv)
{
    return ((double)i)*inv-1.5;

}


int main(int argc, char **argv) 
{
    int n = (argc > 1) ? atoi(argv[1]) : 200;
    int size = (n + VEC_SIZE - 1)/VEC_SIZE*VEC_SIZE;
    int chunk_size=size/VEC_SIZE;
    double inv = 2.0 / ((double)size);
	
    Vec *xloc=(Vec*) malloc(chunk_size*sizeof(Vec));

    for(int i=0;i<chunk_size;++i)
    {
        int offset = i*VEC_SIZE;
        for(int j=0;j<VEC_SIZE;++j)
        {
            xloc[i].x[j]=init_xloc(offset+j, inv);
        }
    }
    printf("P4\n %d %d\n",size,size);

    unsigned char* pixels = (unsigned char*)malloc(size*chunk_size*sizeof(unsigned char));


    for(int y=0;y<size;++y)
    {
        double ci = ((double)y)*inv-1.0;
        for(int x=0;x<chunk_size;++x)
        {
            pixels[y*chunk_size+x]=mbrot8(&xloc[x],ci);
        }
    }

/*    printf("{ ");
    for(int x=0;x<size*chunk_size;++x)
       {
          printf("%d, ",pixels[x]);
       }// */
       printf("\n");
    unsigned char *output = MD5(pixels, size*chunk_size,NULL);
    for(int i=0;i<16;++i)
    {
	    printf("%02x", (unsigned char)output[i]);
    }
    printf("\n");	
    //free(output);
    return 0;
}
