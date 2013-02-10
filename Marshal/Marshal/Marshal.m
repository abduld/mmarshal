
BeginPackage["Marshal`", {"OpenCLLink`"}]

Transpose0100 ;
Transpose100 ;
Transpose010BS ;
Transpose100IPT
TransposeArrayInPlace
FullTranspose ;


Begin["`Private`"]



(*********************************************************************)
(*********************************************************************)
(*********************************************************************)

Marshal`Developer`$ThisFile = $InputFileName
Marshal`Developer`$ThisDirectory = DirectoryName[Marshal`Developer`$ThisFile]
Marshal`Developer`$SystemResourcesDirectory = FileNameJoin[{Marshal`Developer`$ThisDirectory, "SystemResources"}]
Marshal`Developer`$OpenCLProgram = FileNameJoin[{Marshal`Developer`$SystemResourcesDirectory, "cl_aos_asta.cl"}]


(*********************************************************************)
(*********************************************************************)
(*********************************************************************)


initializedQ := initializedQ =
	(
		transpose0100[b_Integer, opts___] := transpose0100[b, opts] = OpenCLFunctionLoad[
			{Marshal`Developer`$OpenCLProgram},
			"transpose_0100",
			{
				{"Float"},
				"Integer32",
				"Integer32",
				"Integer32",
				{"Integer32"}
			},
			{b, Floor[256/b]},
			"ShellOutputFunction" -> Print,
			opts
		];
		mymemset := mymemset = OpenCLFunctionLoad[
			{Marshal`Developer`$OpenCLProgram},
			"mymemset",
			{
				"Integer32"
			},
			256
		]; 
		transpose100[b_Integer, opts___] := transpose0100[b, opts] = OpenCLFunctionLoad[
			{Marshal`Developer`$OpenCLProgram},
			"transpose_100",
			{
				{"Float"},
				"Integer32",
				"Integer32",
				{"Integer32"}
			},
			b,
			"ShellOutputFunction" -> Print,
			opts
		];
		transpose010BS[opts___] := transpose0100[opts] = OpenCLFunctionLoad[
			{Marshal`Developer`$OpenCLProgram},
			"BS_marshal",
			{
				{"Float"},
				"Integer32",
				"Integer32",
				{"Shared", "Float"}
			},
			256,
			"ShellOutputFunction" -> Print,
			opts
		];
		True
	)

(*********************************************************************)
(*********************************************************************)
(*********************************************************************)


Transpose0100[buf_OpenCLMemory, A_Integer, a_Integer, B_Integer, b_Integer] :=
	Module[{ker},
		ker = transpose0100[b];
		ker[buf, A, a, B, b, {a*B*b, Floor[256 / b]}]
	] /; initializedQ

Transpose0100[data_List, args___] :=
	Module[{buf, res},
		buf = OpenCLMemoryLoad[data];
		res = Transpose0100[buf, args];
		res = OpenCLMemoryGet /@ res;
		OpenCLMemoryUnload[buf];
		res
	]
	
Transpose0100[args___] /; (Message[Transpose0100::args, {args}]; False) := $Failed

(*********************************************************************)
(*********************************************************************)
(*********************************************************************)


Transpose100[buf_OpenCLMemory, A_Integer, B_Integer, b_Integer] :=
	Module[{ker, finished, res},
		ker = transpose100[b];
		finished = OpenCLMemoryAllocate["Float", A * B * 4];
		res = ker[buf, A, B, b, finished, Min[A*B*b, b*1024]];
		OpenCLMemoryUnload[finished];
		res
	] /; initializedQ

Transpose100[data_List, args___] :=
	Module[{buf, res},
		buf = OpenCLMemoryLoad[data];
		res = Transpose0100[buf, args];
		res = OpenCLMemoryGet /@ res;
		OpenCLMemoryUnload[buf];
		res
	]
	
Transpose100[args___] /; (Message[Transpose100::args, {args}]; False) := $Failed

(*********************************************************************)
(*********************************************************************)
(*********************************************************************)

Transpose100IPT[buf_, rest___] :=
	Transpose0100[buf, 1, rest]

(*********************************************************************)
(*********************************************************************)
(*********************************************************************)


Transpose010BS[buf_OpenCLMemory, A_Integer, a_Integer, B_Integer] :=
	Module[{ker},
		ker = transpose010BS[];
		ker[buf, A, a, B, A * 256]
	] /; initializedQ

Transpose010BS[data_List, args___] :=
	Module[{buf, res},
		buf = OpenCLMemoryLoad[data];
		res = Transpose010BS[buf, args];
		res = OpenCLMemoryGet /@ res;
		OpenCLMemoryUnload[buf];
		res
	]
	
Transpose010BS[args___] /; (Message[Transpose010BS::args, {args}]; False) := $Failed

(*********************************************************************)
(*********************************************************************)
(*********************************************************************)


FullTranspose[buf_OpenCLMemory, A_Integer, a_Integer, B_Integer, b_Integer] :=
	Module[{method2a, method2b, method2c},
		method2a = Transpose100;
		method2b = Transpose010BS;
		method2c = Transpose0100;
		method2a[buf, A*a, B, b];
		method2b[buf, B*A, a, b];
		method2c[buf, B, A, b, a]
	] /; initializedQ

FullTranspose[data_List, args___] :=
	Module[{buf, res},
		buf = OpenCLMemoryLoad[data];
		res = FullTranspose[buf, args];
		res = OpenCLMemoryGet /@ res;
		OpenCLMemoryUnload[buf];
		res
	]
FullTranspose[args___] /; (Message[FullTranspose::args, {args}]; False) := $Failed
	
(*********************************************************************)
(*********************************************************************)
(*********************************************************************)

TransposeArrayInPlace[data_List, a_Integer:1, b_Integer:1] :=
	Module[{dims, res},
		dims = Dimensions[data];
		res = FullTranspose[data, dims[[1]]/a, a, dims[[2]]/b, b];
		If[ListQ[res] && res =!= {},
			res = Flatten[First[res]];
			Internal`Deflatten[res, Reverse[dims]],
			$Failed
		]
	] 

TransposeArrayInPlace[args___] /; (Message[TransposeArrayInPlace::args, {args}]; False) := $Failed

(*********************************************************************)
(*********************************************************************)
(*********************************************************************)

End[]

EndPackage[]

