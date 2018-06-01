#include<stdlib.h>
#include<stdio.h>
#include<arm_acle.h>
#include<arm_neon.h>

int main()
{
  float    A[16];
  float A_tr[16];
  int i,j;

  for(i=0;i<16;i++)
  {
       A[i] = i * 1.0;
    A_tr[i] = 0.0;
  }

  printf("Before transpose\n");
  for(i=0;i<4;i++)
  {
    for(j=0;j<4;j++)
    {
      printf("%f ",A[i*4+j]);
    }
    printf("\n");
  }
  printf("\n");

  printf("Here is A_tr to be loaded (ie. full of zeros) \n");
  for(i=0;i<4;i++)
  {
    for(j=0;j<4;j++)
    {
      printf("%f ",A_tr[i*4+j]);
    }
    printf("\n");
  }
  printf("\n");


  vst4q_f32(A_tr,vld1q_f32_x4(A));

  printf("Here is the original A unchanged \n");
  for(i=0;i<4;i++)
  {
    for(j=0;j<4;j++)
    {
      printf("%f ",A[i*4+j]);
    }
    printf("\n");
  }

  printf(" Here is A_tr. using a x4 spread load instruction over 4 sections of the array \n");
  printf("\n");
  for(i=0;i<4;i++)
  {
    for(j=0;j<4;j++)
    {
      printf("%f ",A_tr[i*4+j]);
    }
    printf("\n");
  }

  return 0;
}
