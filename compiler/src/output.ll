define i32 @add (i32 %x$0.arg, i32 %y$1.arg)  { 
  %x$0 = alloca i32
  store i32 %x$0.arg, ptr %x$0
  %y$1 = alloca i32
  store i32 %y$1.arg, ptr %y$1
  %1 = load i32, ptr %x$0
  %2 = load i32, ptr %y$1
  %3 = add i32  %1,  %2
  ret i32 %3 
}

define void @main ()  { 
  %x$2 = alloca i32
  %1 = alloca i32
  store i32 1, ptr %1
  %2 = load i32, ptr %1
  store i32 %2, ptr %x$2
  %y$3 = alloca i32
  %3 = alloca i32
  store i32 2, ptr %3
  %4 = load i32, ptr %3
  store i32 %4, ptr %y$3
  %res$4 = alloca i32
  %5 = load i32, ptr %x$2
  %6 = load i32, ptr %y$3
  %7 = call i32 @add(i32 %5, i32 %6)
  store i32 %7, ptr %res$4
  %8 = load i32, ptr %res$4
  %9 = call void @printInt(i32 %8) 
}

define void @printInt (i32 %a$5.arg)  { 
  %a$5 = alloca i32
  store i32 %a$5.arg, ptr %a$5 
}

define void @printDouble (double %a$6.arg)  { 
  %a$6 = alloca double
  store double %a$6.arg, ptr %a$6 
}

define void @printString (ptr %a$7.arg)  { 
  %a$7 = alloca ptr
  store ptr %a$7.arg, ptr %a$7 
}

define double @readDouble ()  { 
  %1 = alloca double
  store double 0.0, ptr %1
  %2 = load double, ptr %1
  ret double %2 
}

define i32 @readInt ()  { 
  %1 = alloca i32
  store i32 0, ptr %1
  %2 = load i32, ptr %1
  ret i32 %2 
}