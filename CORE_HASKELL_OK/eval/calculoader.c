#include <stdio.h>
#include <dlfcn.h>

#define INPUT_LEN 500

int main(void)
{
  int r;
  char input[INPUT_LEN];
  void *libEval;
  int *(*eval)(const char *, int *);

  if ( (libEval = dlopen("libEval.so", RTLD_LAZY)) == NULL ||
       (eval = dlsym(libEval, "eval")) == NULL ) {
    printf("calculoader says: %s\n", dlerror());
    return 1;
  }

  for (;;) {
    printf("> ");
    if (fgets(input, INPUT_LEN, stdin) == NULL) break;
    if (eval(input, &r) != NULL) {
      printf("%d\n", r);
    } else {
      printf("syntax error\n");
    }
  }
  printf("\n");

  dlclose(libEval);
  return 0;
}
