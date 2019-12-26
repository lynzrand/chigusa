/**
 * mod
 * @param x
 * @param y
 */
int mod(int x, int y) {
  /*
  ...
  */
  return x - x  //
                 / y * y;
}

int main() {
  print(mod(3, 2));
  return 0;
}
