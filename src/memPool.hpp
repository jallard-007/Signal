// #pragma once

// #include <iostream>
// #include <cstdint>
// #include <cstdlib>

// template <typename T>
// struct MemPool {
//   uint32_t n;
//   uint32_t mainListSize;
//   const uint32_t listSize;
//   struct Obj {
//     union {
//       T val;
//       Obj *next;
//     };
//   };
//   Obj *free;
//   Obj **mem;

//   MemPool(uint32_t listSize = 50): n{0}, mainListSize{10}, listSize{listSize} {
//     mem = (Obj**)malloc(sizeof (Obj*) * mainListSize);
//     addList();
//     free = mem[0];
//   }

//   MemPool(const MemPool &) = delete;
//   MemPool(MemPool&&) = delete;

//   ~MemPool() {
//     for (uint32_t i = 0; i < n; ++i) {
//       ::free(mem[i]);
//     }
//     ::free(mem);
//   }

//   void addList() {
//     if (++n >= mainListSize) {
//       mainListSize += mainListSize;
//       mem = (Obj**)realloc(mem, sizeof (Obj*) * (mainListSize));
//     }
//     mem[n] = (Obj*)malloc(sizeof (Obj) * (listSize));
//     initializeList(mem[n]);
//   }

//   void initializeList(Obj* list) {
//     for (uint32_t i = 0; i < listSize - 1; ++i, ++list) {
//       list[i].next = list + i + 1;
//     }
//     list[listSize - 1].next = nullptr;
//   }

//   void release(Obj *const obj) {
//     obj->next = free;
//     free = obj;
//   }

//   T *get(T&& t) {
//     if (!free->next) {
//       addList();
//       free->next = mem[n];
//     }

//     T *curr = &free->val;
//     new (curr) T{std::move(t)};
//     free = free->next;
//     return curr;
//   }
  
// };
