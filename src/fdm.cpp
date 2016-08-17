#include "fdm.hpp"
#include "types.hpp"

// Zero Dirichlet BC diffusion
Field fdm(const Field& inp, const Params& p) {
  Field out = inp;
  Field wrk(inp.size());

  Real d = p.dt/p.dx/p.dx;
  
  using std::swap;
  for (int t = 0; t < p.timeSteps; ++t) {
    for (int j = 1; j < p.N-1; ++j) {
//#pragma omp simd
      for (int i = 1; i < p.N-1; ++i) {
	         wrk[p.N * j + i] = out[p.N * j + i] 
                              + d*(-4.*out[p.N * j + i]
	         	                         + out[p.N * (j+1) + i] 
                                     + out[p.N * j + (i+1)]
                                     + out[p.N * (j-1) + i] 
                                     + out[p.N * j + (i-1)]
                                     );
      }
    }
    swap(out, wrk);
  }

  return out;
}
