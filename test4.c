// unreachable functions
int uf2(int x) {
  if (x == 0) return 0;
  if (x < 0 ) x = -x;
  return x+ uf2(x-1);
}

int uf3(int x) {
  return 3 + uf2(1);
}

int uf1(int x) {
  return uf2(x-1) + uf3(x-2);
}

// Target of function pointers
///////////////////////////////////////////////////////////////////////////////
int ft2(int x) {
  if (x == 0) return 0;
  if (x < 0 ) x = -x;
  return x+ ft2(x-1);
}

int ft3(int x) {
  return 3 + ft2(1);
}

int ft1(int x) {
  return ft2(x-1) + ft3(x-2);
}

///////////////////////////////////////////////////////////////////////////////
int f2(int x) {
  if (x == 0) return 0;
  if (x < 0 ) x = -x;
  return x+ f2(x-1);
}

int f3(int x) {
  return 3 + f2(1) + f2(x-1);
}

int f1(int x) {
  return f2(x-1) + f3(x-2);
}

int main(int argc) {
  int (*pf)(int);
  if (argc <=1)
    pf = ft1;
  else
    pf = ft3;
  
  return f1(10) + (*pf)(9);
}
