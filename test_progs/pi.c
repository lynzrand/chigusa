double abs(double x) {
  if (x < 0)
    return -x;
  else
    return x;
}

double invx2factorial(int n) {
  double invfac = 2;
  int i = 1, j = 1;
  while (i < n) {
    invfac = invfac * i;
    i = i + 1;
  }
  while (j <= 2 * n - 1) {
    invfac = invfac / j;
    j = j + 2;
  }
  return invfac;
}

int main() {
  int iter = 1;
  double halfpi = 0, lasthalfpi, epsilon;
  scan(epsilon);
  while (true) {
    lasthalfpi = halfpi;
    halfpi = halfpi + invx2factorial(iter);  // test
    iter = iter + 1;
    print(iter - 1, halfpi);
    if (iter > 2)
      if (abs(halfpi - lasthalfpi) < epsilon)
        break;
  }
  print(iter - 1, halfpi);
  return 0;
}
