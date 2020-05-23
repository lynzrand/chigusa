#include <stdio.h>

int __std_scan_char() {
  return getchar();
}

int __std_scan_int() {
  int res;
  scanf("%d", &res);
  return res;
}

double __std_scan_double() {
  double res;
  scanf("%lf", &res);
  return res;
}

void __std_put_str(char* str, int len) {
  for (int i = 0; i < len; i++) {
    putchar(str[i]);
  }
}

void __std_put_char(int ch) {
  putchar(ch);
}

void __std_put_int(int i) {
  printf("%d", i);
}

void __std_put_double(double i) {
  printf("%lf", i);
}
