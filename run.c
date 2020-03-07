#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

#include <sys/mman.h> /* for mmap */

typedef void (*JMP_TO)(void);

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

int main(int argc, char *argv[]) {
  void *mm = NULL;

  const size_t sz = sizeof(char) * 4096;
  mm = mmap(NULL, sz, PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
  if (mm == MAP_FAILED) {
    fprintf(stderr, "Failed to mmap region\n");
    exit(1);
  }

  printf("Region mmaped to %p\n", mm);

  printf("Loading binary\n");
  int p[2] = {0,0};
  if (pipe2(p, O_CLOEXEC) != 0) {
    fprintf(stderr, "Failed to open pipe\n");
    goto done;
  }

  pid_t pid = fork();
  if (pid == 0) {
    if (dup3(p[1], STDOUT_FILENO, 0) == -1) {
      fprintf(stderr, "failed to shift stdout\n");
    }
    /* close(p[1]); */
    /* close(p[0]); */

    char *args[] = { "/usr/bin/env", "-v", "/home/nymacro/ChezScheme/INSTALL/bin/scheme", "-q", "--script", "test.ss", NULL };
    char pwd[256];
    getcwd(pwd, sizeof(pwd));
    char *env[] = { pwd, NULL };
    fprintf(stderr, "Pwd: %s\n", pwd);

    int fd = execve("/usr/bin/env", args, env);
    if (fd == -1) {
      fprintf(stderr, "failed to exec subprocess: %s\n", strerror(errno));
    }

    exit(1);
  }

  int fd = p[0];
  int len_read = 0;
  int did_read = 0;
  unsigned char buf[4096] = {0};

  do {
    did_read = read(fd, buf, sizeof(buf));
    if (did_read < 0) {
      fprintf(stderr, "Failed to read (%i)\n", did_read);
      goto done;
    }
    if (did_read == 0) {
      sleep(1);
      continue;
    }
  } while (0);

  len_read += did_read;
  fprintf(stderr, "read %i bytes (%i hex)\n", len_read/2, len_read);

  /* memcpy(mm, buf, len_read); */
  /* hex into bin */
  for (int i = 0; i < len_read; i += 2) {
    unsigned char c = from_hex(&buf[i]);
    printf("%02x", c);
    ((unsigned char*)mm)[i/2] = c;
  }
  printf("\n");

  /* print_hex(mm, len_read/2); */

  printf("Executing\n");
  if (mprotect(mm, sz, PROT_EXEC) == -1) {
    fprintf(stderr, "failed to map exec\n");
  }
  /* jump to loaded code */
  JMP_TO jmp = (JMP_TO)mm;
  jmp();

 done:
  close(p[0]);
  close(p[1]);
  if (mm) munmap(mm, sz);

  return 0;
}
