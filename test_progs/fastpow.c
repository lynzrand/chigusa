int main() {
  int x, y;
  int result = 1;
  scan(x);
  scan(y);
  while (x > 0) {
    if ((x / 2 * 2) != x)
      result = result * y;
    y = y * y;
    x = x / 2;
  }
  print(result);
  return 0;
}
