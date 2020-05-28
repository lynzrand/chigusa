void myprint(int x, int y) {
  int z = x * y;
  if (z > 12)
    print(1);
  if (x > y)
    print(2);
  if (x <= y)
    print(3);
  if (x >= y)
    print(4);
  if (x == y)
    print(5);
  if (x != y)
    print(6);
}

void main() {
  int x = 4;
  int y = 9;

  myprint(x, y);
}
