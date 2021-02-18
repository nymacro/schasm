/* Copyright (c) 2020, Aaron Marks */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

#include <sys/mman.h> /* for mmap */

#include <scheme.h>
#include <equates.h>

/*
 * return value: %rax
 */
typedef int (*JMP_TO)(void);

void test_func(void) {
  write(1, NULL, 0);
  return;
}

int main(int argc, char *argv[]) {
  void *mm = NULL;
  size_t sz = 0;

  /* run scheme */
  Sscheme_init(0);
  Sregister_boot_file(CHEZ_LIBHOME "/petite.boot");
  // Sregister_boot_file(CHEZ_LIBHOME "/scheme.boot");
  Sbuild_heap(NULL, NULL);
  Sscheme_script("test.ss", 0, NULL);

  ptr *fn = Stop_level_value(Sstring_to_symbol("run"));
  if (fn == Snil) {
    fprintf(stderr, "expected function\n");
    goto done;
  }
  ptr *bv = Scall0(fn);
  if (!Sbytevectorp(bv)) {
    fprintf(stderr, "expected bytevector\n");
    goto done;
  }
  sz = Sbytevector_length(bv);
  octet *bv_data = Sbytevector_data(bv);

  printf("Len: %zu\n", sz);

  /* map memory so we can jump into it */
  mm = mmap(NULL, sz, PROT_WRITE, MAP_SHARED|MAP_ANON, -1, 0);
  if (mm == MAP_FAILED) {
    fprintf(stderr, "Failed to mmap region\n");
    goto done;
  }

  memcpy(mm, bv_data, sz);

  printf("Executing:\n");
  if (mprotect(mm, sz, PROT_READ|PROT_EXEC) == -1) {
    fprintf(stderr, "failed to map exec\n");
    goto done;
  }

  /* jump to loaded code */
  JMP_TO jmp = (JMP_TO)mm;
  int ret = jmp();
  printf("Returned: %i\n", ret);

 done:
  if (mm) munmap(mm, sz);
  Sscheme_deinit();

  return 0;
}
