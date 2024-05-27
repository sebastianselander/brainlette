{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Runtime (runtime) where

import Data.String.Interpolate
import Data.Text (Text)

runtime :: Text
runtime =
  [i|

target triple = "x86_64-pc-linux-gnu"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
declare i32 @printf(ptr, ...)
@.empty_string = constant [1 x i8] c"\\00"
@out_of_bounds$Internal = constant [23 x i8] c"index out of bounds: \\0A\\00"
declare i32 @exit(i32)
declare i64 @getline(ptr, ptr, ptr)
declare ptr @fdopen(i32, ptr)
declare i64 @strlen(ptr)
declare i32 @puts(ptr)
declare i32 @scanf(ptr, ...)
declare ptr @malloc(i64)
@fdopen_mode = constant [2 x i8] c"r\\00"

%Array$Internal = type { ptr, i64 }

define i64 @checkBound$Internal(i64 %index, i64 %upper_bound) {
    %is_lower = icmp slt i64 %index, %upper_bound
    br i1 %is_lower, label %ok, label %bad
ok:
    %is_larger = icmp sge i64 %index, 0
    br i1 %is_larger, label %good, label %bad
bad:
    ; WARNING: This should not print to stdout
    call void @printString(ptr @out_of_bounds$Internal)
    call void @printInt(i64 %index)
    call i32 @exit(i32 1)
    unreachable
good:
    ret i64 %index
}

@printInt = constant {ptr, ptr} {ptr @printInt$og, ptr null}
@dnl = internal constant [4 x i8] c"%d\\0A\\00"
define void @printInt$og(ptr %unused ,i32 %x) {
entry: %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
	call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
	ret void
}

@printDouble = constant {ptr, ptr} { ptr @printDouble$og, ptr null}
@fnl = internal constant [6 x i8] c"%.1f\\0A\\00"
define void @printDouble$og(ptr %unused, double %x) {
entry: %t0 = getelementptr [6 x i8], [6 x i8]* @fnl, i32 0, i32 0
	call i32 (i8*, ...) @printf(i8* %t0, double %x)
	ret void
}

@printString = constant {ptr, ptr} { ptr @printString$og, ptr null}

define void @printString$og(ptr %unused, i8* %s) {
entry:  call i32 @puts(i8* %s)
	ret void
}

@readInt = constant {ptr, ptr} { ptr @readInt$og, ptr null}
@d = internal constant [3 x i8] c"%d\\00"
define i32 @readInt$og(ptr %unused) {
entry:	%res = alloca i32
        %t1 = getelementptr [3 x i8], [3 x i8]* @d, i32 0, i32 0
	call i32 (i8*, ...) @scanf(i8* %t1, i32* %res)
	%t2 = load i32, i32* %res
	ret i32 %t2
}

@readDouble = constant {ptr, ptr} { ptr @readDouble$og, ptr null}
@lf = internal constant [4 x i8] c"%lf\\00"
define double @readDouble$og(ptr %unused) {
entry:	%res = alloca double
        %t1 = getelementptr [4 x i8], [4 x i8]* @lf, i32 0, i32 0
	call i32 (i8*, ...) @scanf(i8* %t1, double* %res)
	%t2 = load double, double* %res
	ret double %t2
}

@readString = constant {ptr, ptr} { ptr @readString$og, ptr null}

define ptr @readString$og (ptr %unused) {
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
  %1 = load ptr, ptr %string_pointer
  %2 = call i64 @strlen(ptr %1)
  %3 = add i64 %2, -1
  %4 = getelementptr i8, ptr %string_pointer, i64 %3
  store i8 0, ptr %4
  %string_value = load ptr, ptr %string_pointer
  ret ptr %string_value
IfError:
  ret ptr @.empty_string
}

|]