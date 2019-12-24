int fib(int a) {
  if (a <= 1) {
    return 1;
  } else {
    return fib(a - 1) + fib(a - 2);
  }
}

int main() {
  int a = 0;
  scan(a);
  while (a < 15) {
    print(fib(a));
    a = a + 1;
    if (a == 10)
      break;
  }
  return 0;
}
