double pi = 3.1415926;
int N = 0xbabe;
int max;

int fib(int n) {
  if (n <= 0)
    return 0;
  else if (n == 1)
    return 1;
  else
    return fib(n - 2) + fib(n - 1);
}

int main() {
  int a = 0;
  int max;
  scan(max);
  while (a < max) {
    print(fib(a));
    a = a + 1;
  }
  return 0;
}
