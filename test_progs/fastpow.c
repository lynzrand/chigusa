int main() {
  // bb0
  int x, y;
  int result = 1;
  scan(x);
  scan(y);
  while (x > 0) {
    // bb1
    if ((x / 2 * 2) != x)
      // bb3
      result = result * y;
    // bb4
    y = y * y;
    x = x / 2;
  }
  // bb2
  print(result);
  return 0;
}
