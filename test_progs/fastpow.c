int main() {
  // bb0
  int x, y;
  int result = 1;
  scan(x);
  scan(y);
  while (x > 0) {
    // bb1
    if ((x / 2 * 2) != x)
      // bb2
      result = result * y;
    // bb3
    y = y * y;
    x = x / 2;
  }
  // bb4
  print(result);
  return 0;
}
