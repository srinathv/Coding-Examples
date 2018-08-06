/* neon_example.c - Neon intrinsics example program */
#include <stdint.h>
#include <stdio.h>
#include <assert.h>
#include <arm_neon.h>
/* fill array with increasing integers beginning with 0 */
void fill_array(int16_t *array, int size)
{    int i;
    for (i = 0; i < size; i++)
    {
         array[i] = i;
    }
}
/* return the sum of all elements in an array. This works by calculating 4 totals (one for each lane) and adding those at the end to get the final total */
int sum_array(int16_t *array, int size)
{
     /* initialize the accumulator vector to zero */
     int16x4_t acc = vdup_n_s16(0);
     int32x2_t acc1;
     int64x1_t acc2;
     /* this implementation assumes the size of the array is a multiple of 4 */
     assert((size % 4) == 0);
     /* counting backwards gives better code */
     for (; size != 0; size -= 4)
     {
          int16x4_t vec;
          /* load 4 values in parallel from the array */
          vec = vld1_s16(array);
          /* increment the array pointer to the next element */
          array += 4;
          /* add the vector to the accumulator vector */
          acc = vadd_s16(acc, vec);
      }
      /* calculate the total */
      acc1 = vpaddl_s16(acc);
      acc2 = vpaddl_s32(acc1);
      /* return the total as an integer */
      return (int)vget_lane_s64(acc2, 0);
}
/* main function */
int main()
{
/*      int16_t my_array[100];
      fill_array(my_array, 100);
      printf("Sum was %d\n", sum_array(my_array, 100));
      return 0;
*/
#ifdef __ARM_NEON
	printf("__ARM_NEON true \n");
#else
	printf("__ARM_NEON false \n");
#endif

#ifdef __ARM_NEON_FP
        printf("__ARM_NEON_FP true \n");
#else
        printf("__ARM_NEON_FP false \n");
#endif

#ifdef __ARM_FEATURE_FMA
        printf("__ARM_FEATURE_FMA true, but FMA enabled only if __ARM_NEON_FP also true \n");
#else
        printf("__ARM_FEATURE_FMA false \n");
#endif

#ifdef __ARM_TEST_IFDEF
        printf("__ARM_TEST_IFDEF true \n");
#else
        printf("__ARM_TEST_IFDEF false \n");
#endif

#ifdef __ARM_ARCH
        printf("__ARM_ARCH true with value %d \n", __ARM_ARCH );
#else
        printf("__ARM_ARCH false \n");
#endif
}

