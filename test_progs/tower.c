void a(int a, int b, int c) {}
int main() {
  int i = 0, n;
  scan(n);
  while (i < n) {
    int j = 0;
    while (j < i) {
      print(' ');
      j = j + 1;
    }
    while (j < n) {
      print('\\');
      print('/');
      j = j + 1;
    }
    print('\n');
    i = i + 1;
  }
  return 0;
}
