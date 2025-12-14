#include <cstdarg>

namespace ns {
int func0() { return 1; }
int func1() { return 101; }
int func3() { return 103; }

struct Base {
  Base(int m) : member(m) {}
  virtual ~Base() = default;
  int member;
  static int func2() { return 200; }
  virtual int method() { return 100 + member; }
  virtual int method(int i) { return 100 + i; }
};
struct Derived : Base {
  Derived(int m) : Base(m) {}
  int method() override { return 200 + member; }
  int method(int i) override { return 200 + i; }
};
} // namespace ns

namespace ns1 {
int func3() { return 203; }
} // namespace ns1

struct Base {
  int member = 99;
  int get_member() { return member; }
  static const int array[];
  static int func2() { return 201; }
  static int func2(float f) { return f + 202; }
  static int func2(double d) { return d + 203; }
  int method() { return 300 + member; }
  int method(int i) { return 300 + i + member; }
  float method(float f) { return member + f + 1.25f; }
  double method(float f, int i, double d) { return member + f + i + d; }
  int method(int a, int b, int c, int d) { return member + a + b + c + d; }
  int ambiguous(float f) { return 10; }
  int ambiguous(double d) { return 20; }
  int member_add(int N, ...) {
    va_list args;
    va_start(args, N);
    int sum = 0;
    for (auto i = 0; i < N; i++)
      sum += va_arg(args, int);
    va_end(args);
    member += sum;
    return member;
  }
};
const int Base::array[] = {10};
const int *arr_ptr = Base::array;
// Function with the same name as an existing variable:
int array() { return Base::array[0] + 1; }

union Union {
  int i = 1;
  char c[4];
  int method() { return i; }
};

int func0() { return 0; }
int func0(int i) { return i + 100; }
float func0(float f) { return f + 100.25f; }
double func0(float f, int i, double d) { return f + i + d; }
int func0(int a, int b, int c, int d) { return a + b + c + d; }
int ambiguous(float f) { return 1; }
int ambiguous(double d) { return 2; }
double dsum(int N, ...) {
  va_list args;
  va_start(args, N);
  double sum = 0;
  for (auto i = 0; i < N; i++)
    sum += va_arg(args, double);
  va_end(args);
  return sum;
}

int debase(ns::Base *nsbase, int i) { return nsbase->member + i; }

void stop() {}

int main(int argc, char **argv) {
  ns::Base nsbase = ns::Derived(10);
  auto &r_nsderived = (ns::Derived &)nsbase;
  ns::Base *p_nsbase = new ns::Derived(20);
  auto *p_nsderived = static_cast<ns::Derived *>(p_nsbase);
  Base base = Base();
  Base *p_base = &base;
  Union uni = Union();
  int r0 = Base::func2();
  int r1 = ns::Base::func2();
  int r2 = base.method();
  int r3 = nsbase.method();
  int r4 = uni.method();
  int r6 = base.method(2);
  auto r7 = base.method(2.0f);
  auto r8 = base.method(2.0f, 2, 3.0);
  auto r9 = base.method(2, 10, 100, 1000);
  auto r10 = base.member_add(1, 0);
  auto r11 = base.get_member();

  stop(); // Set a breakpoint here
  double r100 = func0(1.0f, 2, 4.0);
  double r101 = dsum(3, 128.125, 2.0, 4.0);
  return 0;
}
