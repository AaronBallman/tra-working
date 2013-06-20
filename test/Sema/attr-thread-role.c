// RUN: %clang_cc1 %s -fsyntax-only -thread-role-analysis -fcxx-attributes -verify

// Unique and incompatible thread roles attach to null statements
[[cert::thread_role_incompatible("GUI | Compute")]];
[[cert::thread_role_unique("GUI")]];

[[cert::thread_role_decl("GUI, Compute")]] int a;
[[cert::thread_role_decl(12)]] int c;	// expected-error {{'cert::thread_role_decl' attribute requires parameter 1 to be a string}}
[[cert::thread_role_decl("")]] int d;     // expected-error {{'cert::thread_role_decl' requires a non-empty list of thread roles}}
[[cert::thread_role_decl("GUI, Compute, GUI, Compute")]] int e; // expected-error {{duplicate name 'GUI' found in 'cert::thread_role_decl'}} expected-error {{duplicate name 'Compute' found in 'cert::thread_role_decl'}}
[[cert::thread_role_decl(" , ")]] int g; // expected-error {{Malformed thread role name '' found in 'cert::thread_role_decl'}} expected-error {{Malformed thread role name '' found in 'cert::thread_role_decl'}}

void func(void) {
  if (1)
    [[cert::thread_role_grant("GUI")]];  // expected-error {{'cert::thread_role_grant' attribute is allowed only on compound statements}}

  if (1)
    [[cert::thread_role_revoke("Compute")]];  // expected-error {{'cert::thread_role_revoke' attribute is allowed only on compound statements}}

  [[cert::thread_role_grant(1)]]{} // expected-error {{'cert::thread_role_grant' attribute requires parameter 1 to be a string}}
  [[cert::thread_role_revoke(1)]]{} // expected-error {{'cert::thread_role_revoke' attribute requires parameter 1 to be a string}}

  [[cert::thread_role_unique("GUI")]]  // expected-error {{'cert::thread_role_unique' attribute is allowed only on null statements}}
  (void)12;

  [[cert::thread_role_incompatible("GUI | Compute")]] // expected-error {{'cert::thread_role_incompatible' attribute is allowed only on null statements}}
  (void)12;
}
