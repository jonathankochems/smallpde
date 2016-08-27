#include "types.hpp"
#include "fdm.hpp"
#include <iostream>

int main() {
  // set up params
  Params p;
  //p.N  = 256;
  p.N  = 16;
  //p.timeSteps = 1024*5;
  p.timeSteps = 16;
  p.dx = 1./(p.N-1);
  p.dt = p.dx*p.dx/4 / 2; // stable/2  for explicit Euler
  Field f(p.N*p.N);
  f[p.N/2 * p.N + p.N/2] = 1; // initial condition

  // calc
  Field fnew = fdm(f, p);

  // print
  for (int j = 0; j < p.N; ++j) {
    for (int i = 0; i < p.N; ++i)
      std::cout << fnew[p.N * j + i] << ", ";
    std::cout << std::endl;
  }
}
