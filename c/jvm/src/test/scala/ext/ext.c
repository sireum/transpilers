#include <all.h>
#include <unistd.h>

void top_Console_read(STACK_FRAME IS_541E35 result) {
  intmax_t n = read(STDIN_FILENO, &result->value, MaxIS_541E35);
  result->size = (IS_541E35SizeT) n;
}