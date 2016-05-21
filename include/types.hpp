#pragma once
#include <vector>
#include <array>

using Real  = float;
using Field = std::vector<Real>;

struct Params {
  int N;
  int timeSteps;
  Real dx;
  Real dt;
};
