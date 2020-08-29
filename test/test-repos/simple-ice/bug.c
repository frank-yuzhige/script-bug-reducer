struct a {
  signed b
} d;
c, e;
static f() {
  __transaction_atomic {
    for (; 0; c++);
  }
}
static g(h) {
  e = 4;
  for (; e; e--) {
    struct a *i;
    *i = d;
  }
}
int buggyFun() {
  __transaction_atomic { g(f()); }
}

