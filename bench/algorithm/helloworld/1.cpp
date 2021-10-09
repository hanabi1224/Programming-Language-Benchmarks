#include <cstdio>

int main(int argc, char *argv[]) {
  if (argc < 2) {
    printf("Hello world!");
  } else {
    printf("Hello world %s!", argv[1]);
  }
  return 0;
}
