/* Compile as: gcc -ggdb segfault.c -o segfault && ./segfault */
int main()
{
  int* x = 0;
  return *x; /* segfault */
}
