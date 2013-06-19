// RUN: %clang_cc1 %s -fsyntax-only -fcxx-attributes -verify
// expected-no-diagnostics

[[noreturn]]
void foo(void);
