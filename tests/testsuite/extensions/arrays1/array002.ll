
declare void @printInt(i32)
declare void @printDouble(double)
declare void @printString(i8*)
declare i32 @readInt()
declare double @readDouble()

declare noalias i8* @calloc(i32, i32)
declare noalias i8* @malloc(i32)

%__Arr_i32 = type {
  i32,
  i32*
}
  
define %__Arr_i32 @doubleArray(%__Arr_i32 %var0) {
entry:
  %var1 = alloca %__Arr_i32
  store %__Arr_i32 %var0, %__Arr_i32* %var1
  br label %lbl0
lbl0:
  %var4 = load %__Arr_i32* %var1
  %var5 = alloca %__Arr_i32
  store %__Arr_i32 %var4, %__Arr_i32* %var5
  %var6 = getelementptr %__Arr_i32* %var5, i32 0, i32 0
  %var3 = load i32* %var6
  %var7 = alloca %__Arr_i32
  %var8 = getelementptr %__Arr_i32* %var7, i32 0, i32 0
  store i32 %var3, i32* %var8
  %var9 = call i8* @calloc(i32 ptrtoint (%__Arr_i32* getelementptr (%__Arr_i32* null, i32 1) to i32), i32 %var3)
  %var10 = bitcast i8* %var9 to i32*
  %var11 = getelementptr %__Arr_i32* %var7, i32 0, i32 1
  store i32* %var10, i32** %var11
  %var2 = load %__Arr_i32* %var7
  %var12 = alloca %__Arr_i32
  store %__Arr_i32 %var2, %__Arr_i32* %var12
  %var14 = alloca i32
  store i32 0, i32* %var14
  %var13 = load i32* %var14
  %var15 = alloca i32
  store i32 %var13, i32* %var15
  %var16 = load %__Arr_i32* %var1
  %var17 = alloca %__Arr_i32
  store %__Arr_i32 %var16, %__Arr_i32* %var17
  %var18 = getelementptr %__Arr_i32* %var17, i32 0, i32 0
  %var19 = load i32* %var18
  %var20 = getelementptr %__Arr_i32* %var17, i32 0, i32 1
  %var21 = load i32** %var20
  %var22 = alloca i32
  store i32 0, i32* %var22
  br label %lbl1
lbl1:
  %var23 = load i32* %var22
  %var24 = icmp slt i32 %var23, %var19
  %var25 = add i32 0, %var23
  store i32 %var23, i32* %var22
  br i1 %var24, label %lbl2, label %lbl3
lbl2:
  %var26 = getelementptr i32* %var21, i32 %var23
  %var27 = load i32* %var15
  %var28 = getelementptr %__Arr_i32* %var12, i32 0, i32 1
  %var29 = load i32** %var28
  %var30 = getelementptr i32* %var29, i32 %var27
  %var34 = alloca i32
  store i32 2, i32* %var34
  %var32 = load i32* %var34
  %var33 = load i32* %var26
  %var31 = mul i32 %var32, %var33
  store i32 %var31, i32* %var30
  %var36 = load i32* %var15
  %var35 = add i32 %var36, 1
  store i32 %var35, i32* %var15
  br label %lbl1
lbl3:
  %var37 = load %__Arr_i32* %var12
  ret %__Arr_i32 %var37
}
  
define void @shiftLeft(%__Arr_i32 %var38) {
entry:
  %var39 = alloca %__Arr_i32
  store %__Arr_i32 %var38, %__Arr_i32* %var39
  br label %lbl4
lbl4:
  %var42 = alloca i32
  store i32 0, i32* %var42
  %var41 = load i32* %var42
  %var43 = getelementptr %__Arr_i32* %var39, i32 0, i32 1
  %var44 = load i32** %var43
  %var45 = getelementptr i32* %var44, i32 %var41
  %var40 = load i32* %var45
  %var46 = alloca i32
  store i32 %var40, i32* %var46
  %var48 = alloca i32
  store i32 0, i32* %var48
  %var47 = load i32* %var48
  %var49 = alloca i32
  store i32 %var47, i32* %var49
  br label %lbl5
lbl5:
  %var51 = load i32* %var49
  %var55 = load %__Arr_i32* %var39
  %var56 = alloca %__Arr_i32
  store %__Arr_i32 %var55, %__Arr_i32* %var56
  %var57 = getelementptr %__Arr_i32* %var56, i32 0, i32 0
  %var53 = load i32* %var57
  %var58 = alloca i32
  store i32 1, i32* %var58
  %var54 = load i32* %var58
  %var52 = sub i32 %var53, %var54
  %var50 = icmp slt i32 %var51, %var52
  br i1 %var50, label %lbl6, label %lbl7
lbl6:
  %var59 = load i32* %var49
  %var60 = getelementptr %__Arr_i32* %var39, i32 0, i32 1
  %var61 = load i32** %var60
  %var62 = getelementptr i32* %var61, i32 %var59
  %var65 = load i32* %var49
  %var67 = alloca i32
  store i32 1, i32* %var67
  %var66 = load i32* %var67
  %var64 = add i32 %var65, %var66
  %var68 = getelementptr %__Arr_i32* %var39, i32 0, i32 1
  %var69 = load i32** %var68
  %var70 = getelementptr i32* %var69, i32 %var64
  %var63 = load i32* %var70
  store i32 %var63, i32* %var62
  %var72 = load i32* %var49
  %var71 = add i32 %var72, 1
  store i32 %var71, i32* %var49
  br label %lbl5
lbl7:
  %var76 = load %__Arr_i32* %var39
  %var77 = alloca %__Arr_i32
  store %__Arr_i32 %var76, %__Arr_i32* %var77
  %var78 = getelementptr %__Arr_i32* %var77, i32 0, i32 0
  %var74 = load i32* %var78
  %var79 = alloca i32
  store i32 1, i32* %var79
  %var75 = load i32* %var79
  %var73 = sub i32 %var74, %var75
  %var80 = getelementptr %__Arr_i32* %var39, i32 0, i32 1
  %var81 = load i32** %var80
  %var82 = getelementptr i32* %var81, i32 %var73
  %var83 = load i32* %var46
  store i32 %var83, i32* %var82
  ret void
  ret void
}
  
define i32 @scalProd(%__Arr_i32 %var84, %__Arr_i32 %var85) {
entry:
  %var86 = alloca %__Arr_i32
  store %__Arr_i32 %var84, %__Arr_i32* %var86
  %var87 = alloca %__Arr_i32
  store %__Arr_i32 %var85, %__Arr_i32* %var87
  br label %lbl8
lbl8:
  %var89 = alloca i32
  store i32 0, i32* %var89
  %var88 = load i32* %var89
  %var90 = alloca i32
  store i32 %var88, i32* %var90
  %var92 = alloca i32
  store i32 0, i32* %var92
  %var91 = load i32* %var92
  %var93 = alloca i32
  store i32 %var91, i32* %var93
  br label %lbl9
lbl9:
  %var95 = load i32* %var93
  %var97 = load %__Arr_i32* %var86
  %var98 = alloca %__Arr_i32
  store %__Arr_i32 %var97, %__Arr_i32* %var98
  %var99 = getelementptr %__Arr_i32* %var98, i32 0, i32 0
  %var96 = load i32* %var99
  %var94 = icmp slt i32 %var95, %var96
  br i1 %var94, label %lbl10, label %lbl11
lbl10:
  %var101 = load i32* %var90
  %var105 = load i32* %var93
  %var106 = getelementptr %__Arr_i32* %var86, i32 0, i32 1
  %var107 = load i32** %var106
  %var108 = getelementptr i32* %var107, i32 %var105
  %var103 = load i32* %var108
  %var109 = load i32* %var93
  %var110 = getelementptr %__Arr_i32* %var87, i32 0, i32 1
  %var111 = load i32** %var110
  %var112 = getelementptr i32* %var111, i32 %var109
  %var104 = load i32* %var112
  %var102 = mul i32 %var103, %var104
  %var100 = add i32 %var101, %var102
  store i32 %var100, i32* %var90
  %var114 = load i32* %var93
  %var113 = add i32 %var114, 1
  store i32 %var113, i32* %var93
  br label %lbl9
lbl11:
  %var115 = load i32* %var90
  ret i32 %var115
}
  
define i32 @main() {
entry:
  br label %lbl12
lbl12:
  %var118 = alloca i32
  store i32 5, i32* %var118
  %var117 = load i32* %var118
  %var119 = alloca %__Arr_i32
  %var120 = getelementptr %__Arr_i32* %var119, i32 0, i32 0
  store i32 %var117, i32* %var120
  %var121 = call i8* @calloc(i32 ptrtoint (%__Arr_i32* getelementptr (%__Arr_i32* null, i32 1) to i32), i32 %var117)
  %var122 = bitcast i8* %var121 to i32*
  %var123 = getelementptr %__Arr_i32* %var119, i32 0, i32 1
  store i32* %var122, i32** %var123
  %var116 = load %__Arr_i32* %var119
  %var124 = alloca %__Arr_i32
  store %__Arr_i32 %var116, %__Arr_i32* %var124
  %var126 = alloca i32
  store i32 0, i32* %var126
  %var125 = load i32* %var126
  %var127 = alloca i32
  store i32 %var125, i32* %var127
  br label %lbl13
lbl13:
  %var129 = load i32* %var127
  %var131 = load %__Arr_i32* %var124
  %var132 = alloca %__Arr_i32
  store %__Arr_i32 %var131, %__Arr_i32* %var132
  %var133 = getelementptr %__Arr_i32* %var132, i32 0, i32 0
  %var130 = load i32* %var133
  %var128 = icmp slt i32 %var129, %var130
  br i1 %var128, label %lbl14, label %lbl15
lbl14:
  %var134 = load i32* %var127
  %var135 = getelementptr %__Arr_i32* %var124, i32 0, i32 1
  %var136 = load i32** %var135
  %var137 = getelementptr i32* %var136, i32 %var134
  %var138 = load i32* %var127
  store i32 %var138, i32* %var137
  %var140 = load i32* %var127
  %var139 = add i32 %var140, 1
  store i32 %var139, i32* %var127
  br label %lbl13
lbl15:
  %var142 = load %__Arr_i32* %var124
  call void @shiftLeft(%__Arr_i32 %var142)
  %var144 = load %__Arr_i32* %var124
  %var143 = call %__Arr_i32 @doubleArray(%__Arr_i32 %var144)
  %var145 = alloca %__Arr_i32
  store %__Arr_i32 %var143, %__Arr_i32* %var145
  %var146 = load %__Arr_i32* %var124
  %var147 = alloca %__Arr_i32
  store %__Arr_i32 %var146, %__Arr_i32* %var147
  %var148 = getelementptr %__Arr_i32* %var147, i32 0, i32 0
  %var149 = load i32* %var148
  %var150 = getelementptr %__Arr_i32* %var147, i32 0, i32 1
  %var151 = load i32** %var150
  %var152 = alloca i32
  store i32 0, i32* %var152
  br label %lbl16
lbl16:
  %var153 = load i32* %var152
  %var154 = icmp slt i32 %var153, %var149
  %var155 = add i32 0, %var153
  store i32 %var153, i32* %var152
  br i1 %var154, label %lbl17, label %lbl18
lbl17:
  %var156 = getelementptr i32* %var151, i32 %var153
  %var158 = load i32* %var156
  call void @printInt(i32 %var158)
  br label %lbl16
lbl18:
  %var159 = load %__Arr_i32* %var145
  %var160 = alloca %__Arr_i32
  store %__Arr_i32 %var159, %__Arr_i32* %var160
  %var161 = getelementptr %__Arr_i32* %var160, i32 0, i32 0
  %var162 = load i32* %var161
  %var163 = getelementptr %__Arr_i32* %var160, i32 0, i32 1
  %var164 = load i32** %var163
  %var165 = alloca i32
  store i32 0, i32* %var165
  br label %lbl19
lbl19:
  %var166 = load i32* %var165
  %var167 = icmp slt i32 %var166, %var162
  %var168 = add i32 0, %var166
  store i32 %var166, i32* %var165
  br i1 %var167, label %lbl20, label %lbl21
lbl20:
  %var169 = getelementptr i32* %var164, i32 %var166
  %var171 = load i32* %var169
  call void @printInt(i32 %var171)
  br label %lbl19
lbl21:
  %var174 = load %__Arr_i32* %var124
  %var175 = load %__Arr_i32* %var145
  %var173 = call i32 @scalProd(%__Arr_i32 %var174, %__Arr_i32 %var175)
  call void @printInt(i32 %var173)
  %var177 = alloca i32
  store i32 0, i32* %var177
  %var176 = load i32* %var177
  ret i32 %var176
}
