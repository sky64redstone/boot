#define SCREEN_MEM ((volatile char*)0xB8000)
#define SCREEN_W 80
#define SCREEN_H 25

void kput(char c, int offset) {
  offset *= 2;
  SCREEN_MEM[offset] = c;
  SCREEN_MEM[offset + 1] = 0x0F;
}

void kprint(const char* str, int offset) {
  while (*str != 0) {
    kput(*str, offset);
    str++;
    offset++;
  }
}

void kmain() {
  kprint("Hello World!", SCREEN_W);
}
