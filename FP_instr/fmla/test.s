.global _start
_start:
  ldr x0, =1000    // x0 = 100
1:
  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v29.4s // v30 += v1*v2
  mov v29.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v28.4s // v30 += v1*v2
  mov v28.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v27.4s // v30 += v1*v2
  mov v27.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v26.4s // v30 += v1*v2
  mov v26.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v25.4s // v30 += v1*v2
  mov v25.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v24.4s // v30 += v1*v2
  mov v24.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v23.4s // v30 += v1*v2
  mov v23.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v22.4s // v30 += v1*v2
  mov v22.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v21.4s // v30 += v1*v2
  mov v21.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v20.4s // v30 += v1*v2
  mov v20.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v19.4s // v30 += v1*v2
  mov v19.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v18.4s // v30 += v1*v2
  mov v18.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v17.4s // v30 += v1*v2
  mov v17.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v16.4s // v30 += v1*v2
  mov v16.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v15.4s // v30 += v1*v2
  mov v15.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v14.4s // v30 += v1*v2
  mov v14.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v13.4s // v30 += v1*v2
  mov v13.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v12.4s // v30 += v1*v2
  mov v12.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v11.4s // v30 += v1*v2
  mov v11.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v10.4s // v30 += v1*v2
  mov v10.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v9.4s // v30 += v1*v2
  mov v9.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v8.4s // v30 += v1*v2
  mov v8.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v7.4s // v30 += v1*v2
  mov v7.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v6.4s // v30 += v1*v2
  mov v6.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v5.4s // v30 += v1*v2
  mov v5.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v4.4s // v30 += v1*v2
  mov v4.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v3.4s // v30 += v1*v2
  mov v3.16b, v30.16b       // v1 = v30

  mov v30.16b, v0.16b       // v30 = v0
  fmla v30.4s, v1.4s, v2.4s // v30 += v1*v2
  mov v2.16b, v30.16b       // v1 = v30

  sub x0, x0, #1
  cbnz x0, 1b

  br xzr
