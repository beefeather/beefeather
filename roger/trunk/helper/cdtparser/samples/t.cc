
typedef A::B C::E<int, (2>3)>::* T;

class A {
public:
  class B {
  public:
    T* t;
  };
};