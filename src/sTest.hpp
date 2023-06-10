#pragma once

#include "./nodes.hpp"
#include <iostream>
#include <cstdint>
#include <cstdlib>

struct Obj {
  union {
    Statement val;
    Obj *next;
  };
};

struct MemPoolS {
  uint32_t n;
  uint32_t mainListSize;
  const uint32_t listSize;
  Obj *freeObj;
  Obj **mem;

  MemPoolS(uint32_t listSize = 50): n{0}, mainListSize{1}, listSize{listSize} {
    mem = (Obj**)malloc(sizeof (Obj*) * mainListSize);
    mem[0] = (Obj*)malloc(sizeof (Obj) * (listSize));
    initializeList(mem[0]);
    freeObj = mem[0];
  }

  MemPoolS(const MemPoolS &) = delete;
  MemPoolS(MemPoolS&&) = delete;

  ~MemPoolS() {
    for (uint32_t i = 0; i <= n; ++i) {
      free(mem[i]);
    }
    free(mem);
  }

  void addList() {
    if (++n >= mainListSize) {
      mainListSize += mainListSize;
      mem = (Obj**)realloc(mem, sizeof (Obj*) * (mainListSize));
    }
    mem[n] = (Obj*)malloc(sizeof (Obj) * (listSize));
    initializeList(mem[n]);
  }

  void initializeList(Obj* list) {
    for (uint32_t i = 0; i < listSize - 1; ++i) {
      list[i].next = &list[i + 1];
    }
    list[listSize - 1].next = nullptr;
  }

  // void release(Obj *const obj) {
  //   obj->next = freeObj;
  //   freeObj = obj;
  // }

  Statement *get(Statement&& t) {
    if (!freeObj->next) {
      addList();
      freeObj->next = mem[n];
    }

    Statement *curr = &freeObj->val;
    freeObj = freeObj->next;
    new (curr) Statement{std::move(t)};
    return curr;
  }
  
};
