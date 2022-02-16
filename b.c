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
unsigned char *
_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$6as_ptr17h9c1f26f51c603ab6E(
    struct __slice_0 _1);
int _ZN4core4hint9black_box17hbd40d8fad24b92e9E(int _1);
int main(int _1, char **_2) {
  int _0;
  int32_t _3;
  char *_4;
  char *_5;
  unsigned char *_6;
  struct __slice_0 _7;
  struct __slice_0 _8;
  int _9;
bb0 : {
  _8 = (struct __slice_0){.ptr = _allocation_15, .len = 15};
  _7 = _8;
  _6 = _ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$6as_ptr17h9c1f26f51c603ab6E(
      _7);
  goto bb1;
}
bb1 : {
  _5 = ((char *)_6);
  _4 = _5;
  _3 = printf(_4);
  goto bb2;
}
bb2 : {
  _9 = _ZN4core4hint9black_box17hbd40d8fad24b92e9E(123);
  goto bb3;
}
bb3 : {
  _0 = 4567 * _9;
  return _0;
}
}
unsigned char *
_ZN4core5slice29_$LT$impl$u20$$u5b$T$u5d$$GT$6as_ptr17h9c1f26f51c603ab6E(
    struct __slice_0 _1) {
  unsigned char *_0;
  struct __slice_0 _2;
bb0 : {
  _2 = _1;
  _0 = _2.ptr;
  return _0;
}
}
int _ZN4core4hint9black_box17hbd40d8fad24b92e9E(int _1) {
  int _0;
  int _2;
bb0 : {
  _2 = _1;
  _0 = _2;
  goto bb1;
}
bb1 : { return _0; }
}
