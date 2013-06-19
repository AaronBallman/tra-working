// RUN: %clang_cc1 -std=c++11 -thread-role-analysis -ast-dump %s | FileCheck -strict-whitespace %s

void func2() {
  if (true) [[cert::thread_role_grant("Worker")]] {
  // CHECK:       AttributedStmt{{.*}}
  // CHECK-NEXT:  ThreadRoleGrantAttr{{.*}} "Worker"
  // CHECK-NEXT:  CompoundStmt{{.*}}
  } else [[cert::thread_role_grant("GUI")]] {
  // CHECK:       AttributedStmt{{.*}}
  // CHECK-NEXT:  ThreadRoleGrantAttr{{.*}} "GUI"
  // CHECK-NEXT:  CompoundStmt{{.*}}
  }
}
