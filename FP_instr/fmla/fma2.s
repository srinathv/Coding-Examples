.global _start
_start:
  ldr x0, =1000    // x0 = 100
1:
  fmul v2.4s, v1.4s, v2.4s // v31 = v1*v31
  fadd v2.4s, v2.4s, v0.4s // v31 = v31+v0

  fmul v3.4s, v1.4s, v3.4s // v31 = v1*v31
  fadd v3.4s, v3.4s, v0.4s // v31 = v31+v0

  fmul v4.4s, v1.4s, v4.4s // v31 = v1*v31
  fadd v4.4s, v4.4s, v0.4s // v31 = v31+v0

  fmul v5.4s, v1.4s, v5.4s // v31 = v1*v31
  fadd v5.4s, v5.4s, v0.4s // v31 = v31+v0

  fmul v6.4s, v1.4s, v6.4s // v31 = v1*v31
  fadd v6.4s, v6.4s, v0.4s // v31 = v31+v0

  fmul v7.4s, v1.4s, v7.4s // v31 = v1*v31
  fadd v7.4s, v7.4s, v0.4s // v31 = v31+v0

  fmul v8.4s, v1.4s, v8.4s // v31 = v1*v31
  fadd v8.4s, v8.4s, v0.4s // v31 = v31+v0

  fmul v9.4s, v1.4s, v9.4s // v31 = v1*v31
  fadd v9.4s, v9.4s, v0.4s // v31 = v31+v0

  fmul v10.4s, v1.4s, v10.4s // v31 = v1*v31
  fadd v10.4s, v10.4s, v0.4s // v31 = v31+v0

  fmul v11.4s, v1.4s, v11.4s // v31 = v1*v31
  fadd v11.4s, v11.4s, v0.4s // v31 = v31+v0

  fmul v12.4s, v1.4s, v12.4s // v31 = v1*v31
  fadd v12.4s, v12.4s, v0.4s // v31 = v31+v0

  fmul v13.4s, v1.4s, v13.4s // v31 = v1*v31
  fadd v13.4s, v13.4s, v0.4s // v31 = v31+v0

  fmul v14.4s, v1.4s, v14.4s // v31 = v1*v31
  fadd v14.4s, v14.4s, v0.4s // v31 = v31+v0

  fmul v15.4s, v1.4s, v15.4s // v31 = v1*v31
  fadd v15.4s, v15.4s, v0.4s // v31 = v31+v0

  fmul v16.4s, v1.4s, v16.4s // v31 = v1*v31
  fadd v16.4s, v16.4s, v0.4s // v31 = v31+v0

  fmul v17.4s, v1.4s, v17.4s // v31 = v1*v31
  fadd v17.4s, v17.4s, v0.4s // v31 = v31+v0

  fmul v18.4s, v1.4s, v18.4s // v31 = v1*v31
  fadd v18.4s, v18.4s, v0.4s // v31 = v31+v0

  fmul v19.4s, v1.4s, v19.4s // v31 = v1*v31
  fadd v19.4s, v19.4s, v0.4s // v31 = v31+v0

  fmul v20.4s, v1.4s, v20.4s // v31 = v1*v31
  fadd v20.4s, v20.4s, v0.4s // v31 = v31+v0

  fmul v21.4s, v1.4s, v21.4s // v31 = v1*v31
  fadd v21.4s, v21.4s, v0.4s // v31 = v31+v0

  fmul v22.4s, v1.4s, v22.4s // v31 = v1*v31
  fadd v22.4s, v22.4s, v0.4s // v31 = v31+v0

  fmul v23.4s, v1.4s, v23.4s // v31 = v1*v31
  fadd v23.4s, v23.4s, v0.4s // v31 = v31+v0

  fmul v24.4s, v1.4s, v24.4s // v31 = v1*v31
  fadd v24.4s, v24.4s, v0.4s // v31 = v31+v0

  fmul v25.4s, v1.4s, v25.4s // v31 = v1*v31
  fadd v25.4s, v25.4s, v0.4s // v31 = v31+v0

  fmul v26.4s, v1.4s, v26.4s // v31 = v1*v31
  fadd v26.4s, v26.4s, v0.4s // v31 = v31+v0

  fmul v27.4s, v1.4s, v27.4s // v31 = v1*v31
  fadd v27.4s, v27.4s, v0.4s // v31 = v31+v0

  fmul v28.4s, v1.4s, v28.4s // v31 = v1*v31
  fadd v28.4s, v28.4s, v0.4s // v31 = v31+v0

  fmul v29.4s, v1.4s, v29.4s // v31 = v1*v31
  fadd v29.4s, v29.4s, v0.4s // v31 = v31+v0

  fmul v30.4s, v1.4s, v30.4s // v31 = v1*v31
  fadd v30.4s, v30.4s, v0.4s // v31 = v31+v0

  fmul v31.4s, v1.4s, v31.4s // v31 = v1*v31
  fadd v31.4s, v31.4s, v0.4s // v31 = v31+v0

  sub x0, x0, #1
  cbnz x0, 1b

  br xzr
