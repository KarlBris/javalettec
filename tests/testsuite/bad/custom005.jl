// Test argument shadowing

int main() {
  return test(5);
}

int test(int foo) {
  int foo = 2;
  return foo;
}