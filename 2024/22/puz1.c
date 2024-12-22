#include <stdint.h>
#include <stdio.h>

static const uint64_t prune_by = 0x1000000;

uint64_t secret(uint64_t sec) {
  uint64_t a = sec << 6;
  sec ^= a;
  sec %= prune_by;

  a = sec >> 5;
  sec ^= a;
  sec %= prune_by;

  a = sec << 11;
  sec ^= a;
  sec %= prune_by;

  return sec;
}

int main() {
  uint64_t sec;
  uint64_t sum = 0;
  while (EOF != scanf("%lu", &sec)) {
    for (int i = 0; i < 2000; i++) {
      sec = secret(sec);
    }
    printf("%lu\n", sec);
    sum += sec;
  }
  printf("\nsum = %lu\n", sum);
}
