void myprint(int x, int y) {
  if (x < y)
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
  int x = 1;
  int y = 2;

  myprint(x, y);
}
