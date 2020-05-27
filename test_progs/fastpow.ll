; ModuleID = 'fastpow.c'
source_filename = "fastpow.c"
target datalayout = "e-m:e-p:32:32-Fi8-i64:64-v128:64:128-a:0:32-n32-S64"
target triple = "armv7-unknown-linux-gnueabi"

; Function Attrs: noinline nounwind optnone sspstrong
define dso_local i32 @main() #0 !dbg !10 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  call void @llvm.dbg.declare(metadata i32* %2, metadata !14, metadata !DIExpression()), !dbg !15
  call void @llvm.dbg.declare(metadata i32* %3, metadata !16, metadata !DIExpression()), !dbg !17
  call void @llvm.dbg.declare(metadata i32* %4, metadata !18, metadata !DIExpression()), !dbg !19
  store i32 1, i32* %4, align 4, !dbg !19
  %5 = load i32, i32* %2, align 4, !dbg !20
  %6 = call i32 bitcast (i32 (...)* @scan to i32 (i32)*)(i32 %5), !dbg !21
  %7 = load i32, i32* %3, align 4, !dbg !22
  %8 = call i32 bitcast (i32 (...)* @scan to i32 (i32)*)(i32 %7), !dbg !23
  br label %9, !dbg !24

9:                                                ; preds = %22, %0
  %10 = load i32, i32* %2, align 4, !dbg !25
  %11 = icmp sgt i32 %10, 0, !dbg !26
  br i1 %11, label %12, label %28, !dbg !24

12:                                               ; preds = %9
  %13 = load i32, i32* %2, align 4, !dbg !27
  %14 = sdiv i32 %13, 2, !dbg !30
  %15 = mul nsw i32 %14, 2, !dbg !31
  %16 = load i32, i32* %2, align 4, !dbg !32
  %17 = icmp ne i32 %15, %16, !dbg !33
  br i1 %17, label %18, label %22, !dbg !34

18:                                               ; preds = %12
  %19 = load i32, i32* %4, align 4, !dbg !35
  %20 = load i32, i32* %3, align 4, !dbg !36
  %21 = mul nsw i32 %19, %20, !dbg !37
  store i32 %21, i32* %4, align 4, !dbg !38
  br label %22, !dbg !39

22:                                               ; preds = %18, %12
  %23 = load i32, i32* %3, align 4, !dbg !40
  %24 = load i32, i32* %3, align 4, !dbg !41
  %25 = mul nsw i32 %23, %24, !dbg !42
  store i32 %25, i32* %3, align 4, !dbg !43
  %26 = load i32, i32* %2, align 4, !dbg !44
  %27 = sdiv i32 %26, 2, !dbg !45
  store i32 %27, i32* %2, align 4, !dbg !46
  br label %9, !dbg !24, !llvm.loop !47

28:                                               ; preds = %9
  %29 = load i32, i32* %4, align 4, !dbg !49
  %30 = call i32 bitcast (i32 (...)* @print to i32 (i32)*)(i32 %29), !dbg !50
  ret i32 0, !dbg !51
}

; Function Attrs: nounwind readnone speculatable willreturn
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

declare i32 @scan(...) #2

declare i32 @print(...) #2

attributes #0 = { noinline nounwind optnone sspstrong "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+armv7-a,+d32,+dsp,+fp64,+neon,+vfp2,+vfp2sp,+vfp3,+vfp3d16,+vfp3d16sp,+vfp3sp,-thumb-mode" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable willreturn }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "frame-pointer"="all" "less-precise-fpmad"="false" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+armv7-a,+d32,+dsp,+fp64,+neon,+vfp2,+vfp2sp,+vfp3,+vfp3d16,+vfp3d16sp,+vfp3sp,-thumb-mode" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3, !4, !5, !6, !7, !8}
!llvm.ident = !{!9}

!0 = distinct !DICompileUnit(language: DW_LANG_C99, file: !1, producer: "clang version 10.0.0 ", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "fastpow.c", directory: "/mnt/d/Projects/chigusa/test_progs")
!2 = !{}
!3 = !{i32 7, !"Dwarf Version", i32 4}
!4 = !{i32 2, !"Debug Info Version", i32 3}
!5 = !{i32 1, !"wchar_size", i32 4}
!6 = !{i32 1, !"min_enum_size", i32 4}
!7 = !{i32 7, !"PIC Level", i32 2}
!8 = !{i32 7, !"PIE Level", i32 2}
!9 = !{!"clang version 10.0.0 "}
!10 = distinct !DISubprogram(name: "main", scope: !1, file: !1, line: 10, type: !11, scopeLine: 10, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !2)
!11 = !DISubroutineType(types: !12)
!12 = !{!13}
!13 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!14 = !DILocalVariable(name: "x", scope: !10, file: !1, line: 12, type: !13)
!15 = !DILocation(line: 12, column: 7, scope: !10)
!16 = !DILocalVariable(name: "y", scope: !10, file: !1, line: 12, type: !13)
!17 = !DILocation(line: 12, column: 10, scope: !10)
!18 = !DILocalVariable(name: "result", scope: !10, file: !1, line: 12, type: !13)
!19 = !DILocation(line: 12, column: 13, scope: !10)
!20 = !DILocation(line: 13, column: 8, scope: !10)
!21 = !DILocation(line: 13, column: 3, scope: !10)
!22 = !DILocation(line: 14, column: 8, scope: !10)
!23 = !DILocation(line: 14, column: 3, scope: !10)
!24 = !DILocation(line: 15, column: 3, scope: !10)
!25 = !DILocation(line: 15, column: 10, scope: !10)
!26 = !DILocation(line: 15, column: 12, scope: !10)
!27 = !DILocation(line: 17, column: 10, scope: !28)
!28 = distinct !DILexicalBlock(scope: !29, file: !1, line: 17, column: 9)
!29 = distinct !DILexicalBlock(scope: !10, file: !1, line: 15, column: 17)
!30 = !DILocation(line: 17, column: 12, scope: !28)
!31 = !DILocation(line: 17, column: 16, scope: !28)
!32 = !DILocation(line: 17, column: 24, scope: !28)
!33 = !DILocation(line: 17, column: 21, scope: !28)
!34 = !DILocation(line: 17, column: 9, scope: !29)
!35 = !DILocation(line: 19, column: 16, scope: !28)
!36 = !DILocation(line: 19, column: 25, scope: !28)
!37 = !DILocation(line: 19, column: 23, scope: !28)
!38 = !DILocation(line: 19, column: 14, scope: !28)
!39 = !DILocation(line: 19, column: 7, scope: !28)
!40 = !DILocation(line: 21, column: 9, scope: !29)
!41 = !DILocation(line: 21, column: 13, scope: !29)
!42 = !DILocation(line: 21, column: 11, scope: !29)
!43 = !DILocation(line: 21, column: 7, scope: !29)
!44 = !DILocation(line: 22, column: 9, scope: !29)
!45 = !DILocation(line: 22, column: 11, scope: !29)
!46 = !DILocation(line: 22, column: 7, scope: !29)
!47 = distinct !{!47, !24, !48}
!48 = !DILocation(line: 23, column: 3, scope: !10)
!49 = !DILocation(line: 25, column: 9, scope: !10)
!50 = !DILocation(line: 25, column: 3, scope: !10)
!51 = !DILocation(line: 26, column: 3, scope: !10)
