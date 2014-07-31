         .file   "rdtsc.s" 
         .text 
.globl rdtsc_ 
         .type   rdtsc_, @function 
rdtsc_: 
         rdtsc 
         movl %eax,%ecx 
         movl %edx,%eax 
         shlq $32,%rax 
         addq %rcx,%rax 
         ret 
         .size   rdtsc_, .-rdtsc_ 
<<<<<<< HEAD
=======

>>>>>>> b8bf7aa809d731542480932529bd28e9f1a30499
