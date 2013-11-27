#ifndef CLANG_BASIC_ROGER_CONSTR_H
#define CLANG_BASIC_ROGER_CONSTR_H

namespace clang {

template<typename T>
class RogerConstr {
  char buffer[sizeof (T)];
public:
  void *getBuffer() {
    return &buffer;
  }
  T &get() {
    return *reinterpret_cast<T*>(getBuffer());
  }
  void del() {
    get().~T();
  }
};

}

#endif
