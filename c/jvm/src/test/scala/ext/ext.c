#include <all.h>
#include <unistd.h>

void Console_read(IS_541E35 result, StackFrame caller) {
  ssize_t n = read(STDIN_FILENO, &result->value, MaxIS_541E35);
  result->size = (IS_541E35SizeT) n;
}