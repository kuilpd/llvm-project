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
};
struct Derived : Base {
  Derived(int m) : Base(m) {}
  int method() override { return 200 + member; }
};
} // namespace ns

namespace ns1 {
int func3() { return 203; }
} // namespace ns1

struct Base {
  int member = 99;
  static const int array[];
  static int func2() { return 201; }
  int method() { return 300 + member; }
  int ambiguous(float f) { return 10; }
  int ambiguous(double d) { return 20; }
};
const int Base::array[] = {10};
// Function with the same name as an existing variable:
int array() { return Base::array[0] + 1; }

int func0() { return 0; }
int func0(int i) { return i + 1; }
int ambiguous(float f) { return 1; }
int ambiguous(double d) { return 2; }

void stop() {}

int main(int argc, char **argv) {
  ns::Base nsbase = ns::Derived(10);
  auto &r_nsderived = (ns::Derived &)nsbase;
  ns::Base *p_nsbase = new ns::Derived(20);
  auto *p_nsderived = static_cast<ns::Derived *>(p_nsbase);
  Base base = Base();
  int r0 = Base::func2();
  int r1 = ns::Base::func2();
  int r2 = base.method();
  int r3 = nsbase.method();

  stop(); // Set a breakpoint here
  return 0;
}
