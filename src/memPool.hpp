#pragma once

#include <cstdint>
#include <cstdlib>

/**
 * Template memory pool. Allocations are not reallocated, so the memory address of data is permanent.
 * Memory is not freed when reset; only on destruction.
 * Not thread safe
*/
template<typename T>
struct MemPool {
  uint32_t j; // index of last array that was used
  uint32_t n; // how many arrays have been allocated
  uint32_t mainArraySize; // number of slots available for arrays to be allocated
  const uint32_t arraySize; // number of units available per array
  struct Obj {
    union {
      T val;
      Obj *next;
    };
  };
  Obj *freeObj;
  Obj **mem;

  MemPool(uint32_t arraySize = 500): j{0}, n{0}, mainArraySize{10}, arraySize{arraySize} {
    mem = (Obj**)malloc(sizeof (Obj*) * mainArraySize);
    mem[0] = (Obj*)malloc(sizeof (Obj) * (arraySize));
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
      if (++n >= mainArraySize) {
        mainArraySize += mainArraySize;
        mem = (Obj**)realloc(mem, sizeof (Obj*) * (mainArraySize));
      }
      mem[n] = (Obj*)malloc(sizeof (Obj) * (arraySize));
    }
    initializeList(mem[j]);
  }

  void initializeList(Obj* list) {
    for (uint32_t i = 0; i < arraySize - 1; ++i) {
      list[i].next = &list[i + 1];
    }
    list[arraySize - 1].next = nullptr;
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

  T *get(const T& t) {
    if (!freeObj->next) {
      addList();
      freeObj->next = mem[n];
    }

    T *curr = &freeObj->val;
    freeObj = freeObj->next;
    new (curr) T{t};
    return curr;
  }

  T *get() {
    if (!freeObj->next) {
      addList();
      freeObj->next = mem[n];
    }

    T *curr = &freeObj->val;
    freeObj = freeObj->next;
    new (curr) T{};
    return curr;
  }
  
};
