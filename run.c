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

void print_hex(unsigned char *buf, size_t buf_len) {
  for (size_t i = 0; i < buf_len; i += 8) {
    for (size_t j = i; j < i+8; ++j) {
      if (j >= buf_len)
        printf("  ");
      else
        printf("%02x", buf[j]);
    }
    printf(" : ");
    for (size_t j = i; j < i+8; ++j) {
      if (j >= buf_len) {
        printf(" ");
      } else {
        if (isalnum(buf[i])) {
          printf("%c", buf[j]);
        } else {
          printf(".");
        }
      }
    }
    printf("\n");
  }
  printf("\n");
}

unsigned char _from_hex(unsigned char c) {
  switch (tolower(c)) {
  case '0': return 0;
  case '1': return 1;
  case '2': return 2;
  case '3': return 3;
  case '4': return 4;
  case '5': return 5;
  case '6': return 6;
  case '7': return 7;
  case '8': return 8;
  case '9': return 9;
  case 'a': return 10;
  case 'b': return 11;
  case 'c': return 12;
  case 'd': return 13;
  case 'e': return 14;
  case 'f': return 15;
  }
  return c - '0';
}
unsigned char from_hex(unsigned char c[2]) {
  return (_from_hex(c[0]) << 4) | _from_hex(c[1]);
}

#define CHEZ_LIBHOME "/home/nymacro/ChezScheme/INSTALL/lib/csv9.5.3/ta6fb"

int main(int argc, char *argv[]) {
  void *mm = NULL;
  const size_t sz = sizeof(char) * 4096;
  int len_read = 0;
  int did_read = 0;
  unsigned char buf[512] = {0};
  int old_stdout = dup(STDOUT_FILENO);

  int p[2] = {0,0};
  if (pipe2(p, O_CLOEXEC) != 0) {
    fprintf(stderr, "Failed to open pipe\n");
    goto done;
  }

  if (dup2(p[1], STDOUT_FILENO) == -1) {
    fprintf(stderr, "failed to shift stdout\n");
    goto done;
  }

  /* run scheme */
  Sscheme_init(0);
  Sregister_boot_file(CHEZ_LIBHOME "/petite.boot");
  // Sregister_boot_file(CHEZ_LIBHOME "/scheme.boot");
  Sbuild_heap(0, 0);
  const char *args[1] = {"scheme"};
  Sscheme_script("test.ss", 1, args);

  /* void *current_output_port = Stop_level_value(Sstring_to_symbol("magic-output-port")); */

  /* void *fn = Stop_level_value(Sstring_to_symbol("magic-bin")); */
  /* void *bv = Scall0(fn); */

  /* reset stdout */
  if (dup2(old_stdout, STDOUT_FILENO) == -1) {
    fprintf(stderr, "Failed to remap stdout\n");
    goto done;
  }

  /* map memory so we can jump into it */
  mm = mmap(NULL, sz, PROT_READ|PROT_WRITE, MAP_SHARED|MAP_ANON, -1, 0);
  if (mm == MAP_FAILED) {
    fprintf(stderr, "Failed to mmap region\n");
    exit(1);
  }
  printf("Region mmaped to %p\n", mm);

  do {
    did_read = read(p[0], buf+len_read, sizeof(buf)-len_read);
    if (did_read < 0) {
      fprintf(stderr, "Failed to read (%i)\n", did_read);
      goto done;
    }
    len_read += did_read;
    if (did_read == 0) break;
  } while (0);

  printf("Read %i bytes (%i hex)\n", len_read/2, len_read);

  /* hex into bin */
  for (int i = 0; i < len_read; i += 2) {
    unsigned char c = from_hex(&buf[i]);
    /* printf("%02x", c); */
    ((unsigned char*)mm)[i/2] = c;
  }
  printf("\n");

  printf("Executing\n");
  if (mprotect(mm, sz, PROT_READ|PROT_EXEC) == -1) {
    fprintf(stderr, "failed to map exec\n");
    goto done;
  }

  /* jump to loaded code */
  JMP_TO jmp = (JMP_TO)mm;
  printf("Returned: %i\n", jmp());

 done:
  close(p[0]);
  close(p[1]);
  if (mm) munmap(mm, sz);
  Sscheme_deinit();

  return 0;
}
