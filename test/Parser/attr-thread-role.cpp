// RUN: %clang_cc1 %s -std=c++11 -fsyntax-only -thread-role-analysis -verify
// expected-no-diagnostics

[[cert::thread_role_incompatible("GUI, Worker")]];
[[cert::thread_role_unique("GUI")]];

[[cert::thread_role_decl("GUI, Worker")]]
[[cert::thread_role("GUI | Worker")]]
void func() {
  [[cert::thread_role_revoke("GUI")]] [[cert::thread_role_grant("Worker")]] {
  }
}

[[cert::thread_role("GUI | Worker")]]
void func2() {
  if (true) [[cert::thread_role_revoke("GUI")]] [[cert::thread_role_grant("Worker")]] {
  } else [[cert::thread_role_revoke("Worker")]] [[cert::thread_role_grant("GUI")]] {
  }
}
