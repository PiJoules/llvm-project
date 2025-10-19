#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct S {
  int a, b;
};

[[clang::optnone]]
S TestStack() {
  S s1, s2;
  printf("s1 %p s2 %p\n", &s1, &s2);

  s1.a = s1.b = 2;
  s2 = s1;

  return s1;
}

[[clang::optnone]]
S *TestMalloc() {
  S *s1, *s2;
  s1 = (S *)malloc(sizeof(S));
  s2 = (S *)malloc(sizeof(S));
  printf("s1 %p s2 %p\n", s1, s2);

  s1->a = s1->b = 2;
  *s2 = *s1;

  free(s2);

  return s1;
}

[[clang::optnone]]
void TestRealloc() {
  S *s1 = (S *)malloc(sizeof(S));
  printf("s1 %p\n", s1);

  s1->a = s1->b = 2;
  printf("a %d b %d\n", s1->a, s1->b);

  // Fine since the initial allocation was read from before reallocing.
  // The reallocation was also never written to which is fine.
  s1 = (S *)realloc(s1, sizeof(S));

  free(s1);
}

[[clang::optnone]]
void TestRealloc2() {
  S *s1 = (S *)malloc(sizeof(S));

  // Fine since the initial allocation was never written to.
  // The reallocation is written to then read.
  s1 = (S *)realloc(s1, sizeof(S));

  printf("s1 %p\n", s1);
  s1->a = s1->b = 2;
  printf("a %d b %d\n", s1->a, s1->b);

  free(s1);
}

struct MyString {
  ~MyString() { free(data); }

  void CopyImpl(const char *other) {
    size_t len = strlen(other);
    data = (char *)malloc(len + 1);

    // TODO: Intercept memcpy/strncmp.
    for (size_t i = 0; i < len; ++i) {
      data[i] = other[i];
    }
    data[len] = 0;
    this->len = len;
  }

  MyString() : len(0), data((char *)malloc(1)) { data[0] = 0; }

  MyString(const MyString &other) { CopyImpl(other.data); }

  MyString(MyString &&other) {
    len = other.len;
    data = other.data;

    other.len = 0;
    other.data = nullptr;
  }

  MyString &operator=(const char *other) {
    CopyImpl(other);
    return *this;
  }

  MyString &operator+=(const MyString &other) {
    size_t newlen = len + other.len;

    char *newdata = (char *)malloc(newlen + 1);
    // TODO: Intercept memcpy/strncmp.
    for (size_t i = 0; i < len; ++i) {
      newdata[i] = data[i];
    }
    for (size_t i = 0; i < other.len; ++i) {
      newdata[i + len] = other.data[i];
    }
    newdata[newlen] = 0;

    free(data);

    this->len = newlen;
    this->data = newdata;
    return *this;
  }

  const char *c_str() const { return data; }

  size_t len;
  char *data;
};

using string = MyString;

[[clang::optnone]]
void ForceLoad(const void *p) {
  char c = *(const char *)p;
  (void)c;
}

string TestString(bool read_s1) {
  string s1, s2;
  printf("s1 %p, s2 %p\n", &s1, &s2);
  s2 = "Hellooooo!";
  printf("s2.c_str() %s\n", s2.c_str());
  ForceLoad(s2.c_str());
  // s1 += s2;
  printf("s1.c_str() %p\n", s1.c_str());

  if (read_s1)
    printf("s1.c_str() %s\n", s1.c_str());

  return s1;
}

int main() {
  {
    printf("TestStack: Expecting one error\n");
    auto s = TestStack();
    printf("s %p\n", &s);
    printf("s.a %d s.b %d\n", s.a, s.b);
    printf("\n");
  }

  {
    printf("TestMalloc: Expecting one error\n");
    auto *s = TestMalloc();
    printf("s %p\n", s);
    printf("s->a %d s->b %d\n", s->a, s->b);
    free(s);
    printf("\n");
  }

  {
    printf("TestRealloc: Expecint no errors\n");
    TestRealloc();
    printf("\n");

    printf("TestRealloc2: Expecting no errors\n");
    TestRealloc2();
    printf("\n");
  }

  {
    printf("TestString: Expecting 1 error from not reasing s1\n");
    auto s = TestString(/*read_s1=*/false);
    // FalsePositive: The underlying c_str is not marked as read because it's
    // behind printf.
    printf("s %p, c_str %p, '%s'\n", &s, s.c_str(), s.c_str());
    printf("\n");
    // Uncommenting below suppresses the FP since it forces a read.
    char c = s.c_str()[0];
    (void)c;
  }
}
