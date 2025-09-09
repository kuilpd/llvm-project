#include <cstdint>
#include <limits>

int main(int argc, char **argv) {
  short s = 10;
  unsigned short us = 1;
  long l = 5;
  float f = 1.0f;
  double d = 2.5;

  int x = 2;
  int &r = x;
  int *p = &x;
  typedef int &myr;
  myr my_r = x;

  int array[] = {1};
  enum Enum { kZero, kOne } enum_one = kOne;
  wchar_t wchar = 1;
  char16_t char16 = 2;
  char32_t char32 = 3;

  struct BitFieldStruct {
    char a : 4;
    int b : 32;
    unsigned int c : 32;
    uint64_t d : 48;
  };
  BitFieldStruct bitfield = {1, 2, 3, 4};

  int int_max = std::numeric_limits<int>::max();
  unsigned int uint_max = std::numeric_limits<unsigned int>::max();
  long long ll_max = std::numeric_limits<long long>::max();
  unsigned long long ull_max = std::numeric_limits<unsigned long long>::max();

  return 0; // Set a breakpoint here
}
