#include <stddef.h>
#include <stdint.h>
typedef struct {
} __unitty;
unsigned char _allocation_15[67] = {72,  101, 108, 108, 111, 44, 32, 119,
                                    111, 114, 108, 100, 33,  10, 0};
struct __slice_0 {
  unsigned char *ptr;
  size_t len;
};
int main(int _1, char **_2);
int main(int _1, char **_2) {
  int _0;
  int32_t _3;
  int32_t _4;
  char *_5;
  char *_6;
  unsigned char *_7;
  struct __slice_0 _8;
  struct __slice_0 _9;
  int _10;
  int32_t _11;
  struct __slice_0 _12;
  int _13;
bb0 : {
  _11 = 1;
  _3 = 1;
  goto bb4;
}
bb1 : {
  _9 = (struct __slice_0){.ptr = _allocation_15, .len = 15};
  _8 = _9;
  _12 = _8;
  _7 = _12.ptr;
  _6 = ((char *)_7);
  _5 = _6;
  _4 = printf(_5);
  goto bb2;
}
bb2 : { goto bb3; }
bb3 : {
  _13 = 123;
  _10 = 123;
  goto bb5;
}
bb4 : {
  switch (_3) {
  case 1:
    goto bb1;
  default:
    goto bb3;
  }
}
bb5 : {
  _0 = 4567 * _10;
  return _0;
}
}
