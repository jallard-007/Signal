#pragma once

#include <cstdint>
#include <cstdlib>

template<typename T>
struct MemPool {
  uint32_t j;
  uint32_t n;
  uint32_t mainListSize;
  const uint32_t listSize;
  struct Obj {
    union {
      T val;
      Obj *next;
    };
  };
  Obj *freeObj;
  Obj **mem;

  MemPool(uint32_t listSize = 50): j{0}, n{0}, mainListSize{10}, listSize{listSize} {
    mem = (Obj**)malloc(sizeof (Obj*) * mainListSize);
    mem[0] = (Obj*)malloc(sizeof (Obj) * (listSize));
    initializeList(mem[0]);
    freeObj = mem[0];
  }

  MemPool(const MemPool &) = delete;
  MemPool(MemPool&&) = delete;

  ~MemPool() {
    for (uint32_t i = 0; i <= n; ++i) {
      free(mem[i]);
    }
    free(mem);
  }

  void reset() {
    initializeList(mem[0]);
    freeObj = mem[0];
    j = 0;
  }

  void addList() {
    if (++j > n) {
      if (++n >= mainListSize) {
        mainListSize += mainListSize;
        mem = (Obj**)realloc(mem, sizeof (Obj*) * (mainListSize));
      }
      mem[n] = (Obj*)malloc(sizeof (Obj) * (listSize));
    }
    initializeList(mem[j]);
  }


  void initializeList(Obj* list) {
    for (uint32_t i = 0; i < listSize - 1; ++i) {
      list[i].next = &list[i + 1];
    }
    list[listSize - 1].next = nullptr;
  }

  void release(T *obj) {
    ((Obj*)obj)->next = freeObj;
    freeObj = (Obj *)obj;
  }

  T *get(T&& t) {
    if (!freeObj->next) {
      addList();
      freeObj->next = mem[n];
    }

    T *curr = &freeObj->val;
    freeObj = freeObj->next;
    new (curr) T{std::move(t)};
    return curr;
  }
  
};