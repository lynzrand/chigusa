int func1(){}
int func2(int p1){}
void func3(int p1, int p2){}

void testStatement(int p1, int p2) {
  int a1;
  {
    int a1 = p1;
  }
  if (p1 == p2)
    a1 = p1;
  
  if (p1 <= p2) {
    int a2 = p1;
  }
  
  if (p1 < p2)
    a1 = p1;
  else {
    a1 = p2;
  }

  while (p1 > 1)
    p1 = p1 - 1;
  
  while (p1 >= 1) {
    int a2 = 3;
    p1 = p1 - a2;
  }

  return;

  print(p1);
  print(p1, p1 + p2);
  print(1, p2 + 2 * 3, "hello");
  print("hello2");

  scan(p1);

  p1 = func1();
  func2(p1);
  func3(p1, p2);
}


void testExpression() {
  int a, b = 5;
  a = (1 + 2 - 3);
  a = b;
  a = 12345;
  // a = testVariableA();
  a = -(1 + 2 - 3);
  a = -b;
  a = +12345;
  a = -12345;

  a = b * -12345;
  a = 1 * (2 + 3);

  a + b * c + d;

  a = a + b + 1;
  a = -(a + b + 1);
  a = b * -12345 + (2 + 3);
}

int testVariableA(int p1) {
  int a1, a2 = 1;
  const int b1 = 0x12;
  int testVariableA;
  a1 = 2;
}

int testVariableB(int p) {
  void a1 = 1;
  const int b1;
  const void b2 = 1;
  
  const int a2;
  a2 = 3;
  scan(a2);
  
}
