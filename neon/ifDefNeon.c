/* neon_example.c - Neon intrinsics example program */
#include <stdint.h>
#include <stdio.h>
#include <arm_neon.h>

/* main function */
int main()
{

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

