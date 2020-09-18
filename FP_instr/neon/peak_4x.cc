#include <iostream>
#include <cstdint>
#include <chrono>
#include <cstdlib>

extern "C" void kernel(std::uint64_t, void *);

// unroll 4x
asm(R"(
	.global kernel
kernel:
	stp x29, x30, [sp, -256]!
	stp x19, x20, [sp,  16]
	stp x21, x22, [sp,  32]
	stp x23, x24, [sp,  48]
	stp x25, x26, [sp,  64]
	stp x27, x28, [sp,  80]
	stp d8,  d9,  [sp,  96]
	stp d10, d11, [sp, 112]
	stp d12, d13, [sp, 128]
	stp d14, d15, [sp, 144]

	mov x20, xzr
  
loop:
fmla v0.2d, v30.2d, v31.2d
fmla v1.2d, v30.2d, v31.2d
fmla v2.2d, v30.2d, v31.2d
fmla v3.2d, v30.2d, v31.2d
fmla v4.2d, v30.2d, v31.2d
fmla v5.2d, v30.2d, v31.2d
fmla v6.2d, v30.2d, v31.2d
fmla v7.2d, v30.2d, v31.2d
fmla v8.2d, v30.2d, v31.2d
fmla v9.2d, v30.2d, v31.2d
fmla v10.2d, v30.2d, v31.2d
fmla v11.2d, v30.2d, v31.2d
fmla v12.2d, v30.2d, v31.2d
fmla v13.2d, v30.2d, v31.2d
fmla v14.2d, v30.2d, v31.2d
fmla v15.2d, v30.2d, v31.2d
	add x20, x20, 1
fmla v0.2d, v30.2d, v31.2d
fmla v1.2d, v30.2d, v31.2d
fmla v2.2d, v30.2d, v31.2d
fmla v3.2d, v30.2d, v31.2d
fmla v4.2d, v30.2d, v31.2d
fmla v5.2d, v30.2d, v31.2d
fmla v6.2d, v30.2d, v31.2d
fmla v7.2d, v30.2d, v31.2d
fmla v8.2d, v30.2d, v31.2d
fmla v9.2d, v30.2d, v31.2d
fmla v10.2d, v30.2d, v31.2d
fmla v11.2d, v30.2d, v31.2d
fmla v12.2d, v30.2d, v31.2d
fmla v13.2d, v30.2d, v31.2d
fmla v14.2d, v30.2d, v31.2d
fmla v15.2d, v30.2d, v31.2d
	cmp x0, x20
fmla v0.2d, v30.2d, v31.2d
fmla v1.2d, v30.2d, v31.2d
fmla v2.2d, v30.2d, v31.2d
fmla v3.2d, v30.2d, v31.2d
fmla v4.2d, v30.2d, v31.2d
fmla v5.2d, v30.2d, v31.2d
fmla v6.2d, v30.2d, v31.2d
fmla v7.2d, v30.2d, v31.2d
fmla v8.2d, v30.2d, v31.2d
fmla v9.2d, v30.2d, v31.2d
fmla v10.2d, v30.2d, v31.2d
fmla v11.2d, v30.2d, v31.2d
fmla v12.2d, v30.2d, v31.2d
fmla v13.2d, v30.2d, v31.2d
fmla v14.2d, v30.2d, v31.2d
fmla v15.2d, v30.2d, v31.2d
fmla v0.2d, v30.2d, v31.2d
fmla v1.2d, v30.2d, v31.2d
fmla v2.2d, v30.2d, v31.2d
fmla v3.2d, v30.2d, v31.2d
fmla v4.2d, v30.2d, v31.2d
fmla v5.2d, v30.2d, v31.2d
fmla v6.2d, v30.2d, v31.2d
fmla v7.2d, v30.2d, v31.2d
fmla v8.2d, v30.2d, v31.2d
fmla v9.2d, v30.2d, v31.2d
fmla v10.2d, v30.2d, v31.2d
fmla v11.2d, v30.2d, v31.2d
fmla v12.2d, v30.2d, v31.2d
fmla v13.2d, v30.2d, v31.2d
fmla v14.2d, v30.2d, v31.2d
fmla v15.2d, v30.2d, v31.2d
	bne loop

end:
	ldp x19, x20, [sp, #16]
	ldp x21, x22, [sp, #32]
	ldp x23, x24, [sp, #48]
	ldp x25, x26, [sp, #64]
	ldp x27, x28, [sp, #80]
	ldp d8,  d9,  [sp,  96]
	ldp d10, d11, [sp, 112]
	ldp d12, d13, [sp, 128]
	ldp d14, d15, [sp, 144]
	ldp x29, x30, [sp], 256
	ret
)");

int main(int argc, char ** argv) 
{
    const std::uint64_t iters = 1000000ul;

    void * buf = std::malloc(sizeof(double)*125*iters);
    buf += 64 - (uint64_t(buf)%64);

    const std::uint64_t lanes = 2;  // 2 for double, 4 for float
    const std::uint64_t flops = 32; // 16 fmla = 32 flops
    const std::uint64_t unroll = 4; // match kernel asm
    const std::uint64_t kernel_flops = lanes * flops * unroll * iters;

    auto start = std::chrono::high_resolution_clock::now();
    kernel(iters, buf);
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> diff = end - start;
    double seconds = diff.count();

    double gflops = kernel_flops / (seconds * 1e9);

    std::cout << kernel_flops << " Flops in " << seconds << " seconds" << std::endl;
    std::cout << gflops << " GFlops" << std::endl;

    return 0;
}
