target triple = "x86_64-pc-linux-gnu"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
declare i32 @printf(ptr nocapture, ...) nounwind
@.empty_string = constant [1 x i8] c"\00"
declare i64 @getline(ptr nocapture nonnull noundef, ptr nocapture nonnull noundef, ptr nocapture nonnull noundef)
declare ptr @fdopen(i32, ptr nocapture nonnull noundef)
@fdopen_mode = constant [2 x i8] c"r\00"


define ptr @readString ()  { 
  %stdin = call ptr @fdopen(i32 0, ptr @fdopen_mode)
  %string_pointer = alloca ptr
  store ptr null, ptr %string_pointer
  %string_length = alloca i64
  store i64 0, ptr %string_length
  %return_code = 
      call i64 @getline(ptr %string_pointer, ptr %string_length, ptr %stdin)
  %cond = icmp eq i64 %return_code, -1
  br i1 %cond, label %IfError, label %IfSuccess
IfSuccess:
  %string_value = load ptr, ptr %string_pointer
  ret ptr %string_value
IfError:
  ret ptr @.empty_string
}

define i32 @readInt () {
  ret i32 0
}

define double @readDouble () {
  ret double 0.0
}

@.printInt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
define void @printInt (i32 %arg) {
  call i32 (...) @printf(ptr @.printInt, i32 %arg)
  ret void
}

@.printDouble = private unnamed_addr constant [4 x i8] c"%g\0A\00"
define void @printDouble (double %arg) {
  call i32 (...) @printf(ptr @.printDouble, double %arg)
  ret void
}

@.printString = private unnamed_addr constant [4 x i8] c"%s\0A\00"
define void @printString (ptr %arg) {
  call i32 (...) @printf(ptr @.printString, ptr %arg)
  ret void
}


