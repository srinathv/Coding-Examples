#include<stdio.h>
#include<stdlib.h>
#include<arm_sve.h>
#include<complex.h>

#define cmul(a, b, c, i) c[i]   = a[i]*b[i]   - a[i+1]*b[i+1]; c[i+1] = a[i]*b[i+1] + a[i+1]*b[i];


void mulVecComplex1(const int n, const float* a, const float *b, float *res)
{
        int i;

	for(i=0;i<n;i++)
        {
          cmul(a,b,res,2*i)
        }

}

void mulVecComplex2(const int n, const float* a, const float *b, float *res)
{
        int i;

	svbool_t predReg = svptrue_b32();

	svfloat32_t aReg    = svld1(predReg, a);
	svfloat32_t bReg    = svld1(predReg, b);
	svfloat32_t tempReg = svdup_f32(0);
	svfloat32_t resReg  = svdup_f32(0);

	resReg = svcmla_f32_x( predReg, tempReg, aReg, bReg, 90);
	resReg = svcmla_f32_x( predReg,  resReg, aReg, bReg,  0);

	svst1( predReg, res, resReg);

	}


}

void mulVecComplex3(const int n, const float* a, const float *b, float *res)
{
        int i;

	svbool_t predReg = svptrue_b32();

	svfloat32_t aReg    ;
	svfloat32_t bReg    ;
	svfloat32_t tempReg ;
	svfloat32_t resReg  ;

	for(i=0;i<2*n;i=i+svcntw())
	{

  	  aReg    = svld1(predReg, a+i);
  	  bReg    = svld1(predReg, b+i);
  	  tempReg = svdup_f32(0);
  	  resReg  = svdup_f32(0);
  
  	  resReg = svcmla_f32_x( predReg, tempReg, aReg, bReg, 90);
  	  resReg = svcmla_f32_x( predReg,  resReg, aReg, bReg,  0);

  	  svst1( predReg, res+i, resReg);
	}


}


int main()
{
	float *a;
	float *b;
	float *res1;
	float *res2;
	float *res3;
	_Complex float *ref;
	int i;
	int n;
	int test;

	n=20;

	a    = malloc(sizeof(float)*2*n);
	b    = malloc(sizeof(float)*2*n);
	res1 = malloc(sizeof(float)*2*n);
	res2 = malloc(sizeof(float)*2*n);
	res2 = malloc(sizeof(float)*3*n);
	ref  = malloc(sizeof(_Complex float)*n);

	for(i=0;i<2*n;i=i+2)
	{
	  a[i]   = ( i + 1)*1.0;
	  a[i+1] = (-i + 2)*1.0;
	  b[i]   = ( i + 3)*2.0;
	  b[i+1] = (-i + 4)*3.0;
	  ref[i] = ( a[i] + _Complex_I * a[i+1] ) * ( b[i] + _Complex_I * b[i+1] ) ;
	}

	mulVecComplex1(n, a, b, res1);
	mulVecComplex2(n, a, b, res2);
	mulVecComplex3(n, a, b, res3);
	
	test=0;
	for(i=0;i<2*n;i=i+2)
	{
	  if( (ref[i] != ref2[i]) || (ref[i+1] != res2[i+1])) test=1;
	}
	
	if(test==1) printf("Error !\n");

	for(i=0;i<2*n;i=i+2)
	{
	  printf("-------------------------------------------\n");
	  printf("   a = %f + i x %f\n",   a[i],   a[i+1]);
	  printf("   b = %f + i x %f\n",   b[i],   b[i+1]);
	  printf("res1 = %f + i x %f\n",res1[i],res1[i+1]);
	  printf("res2 = %f + i x %f\n",res2[i],res2[i+1]);
	  printf("res3 = %f + i x %f\n",res3[i],res3[i+1]);
	  printf("ref  = %f + i x %f\n",creal(ref[i]),cimag(ref[i]));
	  printf("-------------------------------------------\n");
	}


	return 0;
}
