#pragma OPENCL EXTENSION cl_khr_fp64 : enable

#define PTCL_SIZE 12

void load_ptcl(__global const double *ptcls, int n,
  __private double ptcl[PTCL_SIZE]);
void store_ptcl(__global double *ptcls, int n,
  __private double const ptcl[PTCL_SIZE]);

__kernel void calc_radii_kernel(__global double *ptcls, int num_ptcls,
  double k) {

  int tid = get_global_id(0);
  if (tid < num_ptcls) {

    double ptcl1[PTCL_SIZE];
    load_ptcl(ptcls, tid, ptcl1);
    ptcl1[9] = 0.0;
    ptcl1[10] = 0.0;
    ptcl1[11] = 0.0;

    double ptcl2[PTCL_SIZE];
    for (int i = 0; i < tid; ++i) {
      load_ptcl(ptcls, i, ptcl2);
      double invR = 1.0 / sqrt(
          (ptcl1[0] - ptcl2[0]) * (ptcl1[0] - ptcl2[0]) +
          (ptcl1[1] - ptcl2[1]) * (ptcl1[1] - ptcl2[1]) +
          (ptcl1[2] - ptcl2[2]) * (ptcl1[2] - ptcl2[2]));
      double prefactor = ptcl1[6] * ptcl2[6] * k * invR * invR * invR;
      ptcl1[9] += prefactor * (ptcl1[0] - ptcl2[0]);
      ptcl1[10] += prefactor * (ptcl1[1] - ptcl2[1]);
      ptcl1[11] += prefactor * (ptcl1[2] - ptcl2[2]);
    }
    for (int i = tid + 1; i < num_ptcls; ++i) {
      load_ptcl(ptcls, i, ptcl2);
      double invR = 1.0 / sqrt(
          (ptcl1[0] - ptcl2[0]) * (ptcl1[0] - ptcl2[0]) +
          (ptcl1[1] - ptcl2[1]) * (ptcl1[1] - ptcl2[1]) +
          (ptcl1[2] - ptcl2[2]) * (ptcl1[2] - ptcl2[2]));
      double prefactor = ptcl1[6] * ptcl2[6] * k * invR * invR * invR;
      ptcl1[9] += prefactor * (ptcl1[0] - ptcl2[0]);
      ptcl1[10] += prefactor * (ptcl1[1] - ptcl2[1]);
      ptcl1[11] += prefactor * (ptcl1[2] - ptcl2[2]);
    }
    store_ptcl(ptcls, tid, ptcl1);
  }
}

void load_ptcl(__global const double *ptcls, int n,
    __private double ptcl[PTCL_SIZE]) {
  for (int i = 0; i < PTCL_SIZE; ++i) {
    ptcl[i] = ptcls[n * PTCL_SIZE + i];
  }
}

void store_ptcl(__global double *ptcls, int n,
    __private double const ptcl[PTCL_SIZE]) {
  for (int i = 0; i < PTCL_SIZE; ++i) {
    ptcls[n * PTCL_SIZE + i] = ptcl[i];
  }
}               
