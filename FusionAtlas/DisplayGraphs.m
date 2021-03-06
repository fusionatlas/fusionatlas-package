(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



BeginPackage["FusionAtlas`DisplayGraphs`",{"FusionAtlas`","JLink`","FusionAtlas`Java`","FusionAtlas`Bigraphs`","FusionAtlas`GraphPairs`","FusionAtlas`TensorSolver`","FusionAtlas`ExtractGraphs`","FusionAtlas`FormalCodegrees`"}];


DisplayFusionGraph;DisplayBigraph;DisplayGraph;GraphHash;DeleteGraph;


Begin["`Private`"];


TikZInclusion[{},x0_,\[CapitalDelta]x_,\[CapitalDelta]y_,oriented_:False]:=TikZInclusion[{{}},x0,\[CapitalDelta]x,\[CapitalDelta]y,oriented];
TikZInclusion[m_?MatrixQ,x0_,\[CapitalDelta]x_,\[CapitalDelta]y_,oriented_:False]:=
Module[{rows, columns,matrixEntries,source,target,draw,edges,nodes,noedges},
rows=Length[m];
columns=Length[m[[1]]];
matrixEntries=Flatten[Outer[{#1,#2,m[[#1,#2]]}&,Range[rows],Range[columns]],1];
noedges=Union[matrixEntries[[All,3]]]=={0};
source[r_,c_]:="("<>ToString[N[x0]]<>","<>ToString[N[(c-(columns+1)/2) \[CapitalDelta]y]]<>")";
target[r_,c_]:="("<>ToString[N[x0+\[CapitalDelta]x]]<>","<>ToString[N[(r-(rows+1)/2) \[CapitalDelta]y]]<>")";
draw[{r_,c_,0}]:="";
draw[{r_,c_,1}]:="\\draw"<>If[oriented,"[shorten >=0.1cm, shorten <=0.1cm, ->]",""]<>" "<>source[r,c]<>" -- "<>target[r,c]<>";\n";
draw[{r_,c_,k_}]:="\\draw"<>If[oriented,"[shorten >=0.1cm, shorten <=0.1cm, ->]",""]<>" "<>source[r,c]<>" -- node[above] {$"<>ToString[k]<>"$} "<>target[r,c]<>";\n";
edges=StringJoin@@(draw/@matrixEntries);
nodes=StringJoin@@(If[Length[matrixEntries]==0,
"\\draw[fill=white] "<>target[#,1]<>" circle (0.1);\n",
"\\draw[fill] "<>target[#,1]<>" circle (0.05);\n"]&/@Range[rows]);
edges<>nodes
]


TikZSameDepth[m_?MatrixQ,x0_,\[CapitalDelta]x_,\[CapitalDelta]y_,oriented_:False]:=
Module[{size,matrixEntries,draw,source,target,control1, control2},
size=Length[m];
matrixEntries=Flatten[Table[{i,j,m[[i,j]]},{i,1,size},{j,If[oriented,1,i],size}],1];
source[r_,c_]:="("<>ToString[N[x0]]<>","<>ToString[N[(c-(size+1)/2) \[CapitalDelta]y]]<>")";
target[r_,c_]:="("<>ToString[N[x0]]<>","<>ToString[N[(r-(size+1)/2) \[CapitalDelta]y]]<>")";
control1[r_,r_]:="("<>ToString[N[x0-\[CapitalDelta]x/2]]<>","<>ToString[N[(1/2+r-(size+1)/2) \[CapitalDelta]y]]<>")";
control2[r_,r_]:="("<>ToString[N[x0+\[CapitalDelta]x/2]]<>","<>ToString[N[(1/2+r-(size+1)/2) \[CapitalDelta]y]]<>")";
control1[r_,c_]:="("<>ToString[N[x0+\[CapitalDelta]x/2]]<>","<>ToString[N[(-Sign[c-r]/2+c-(size+1)/2) \[CapitalDelta]y]]<>")";
control2[r_,c_]:="("<>ToString[N[x0+\[CapitalDelta]x/2]]<>","<>ToString[N[(Sign[c-r]/2+r-(size+1)/2) \[CapitalDelta]y]]<>")";
control1[r_,c_]/;oriented\[And]Abs[r-c]==1:="("<>ToString[N[x0]]<>","<>ToString[N[(Sign[r-c]/4+c-(size+1)/2) \[CapitalDelta]y]]<>")";
control2[r_,c_]/;oriented\[And]Abs[r-c]==1:="("<>ToString[N[x0]]<>","<>ToString[N[(Sign[c-r]/4+r-(size+1)/2) \[CapitalDelta]y]]<>")";
draw[{r_,c_,0}]:="";
draw[{r_,c_,1}]:="\\draw"<>If[oriented,"[shorten >=0.1cm, shorten <=0.1cm, ->]",""]<>" "<>source[r,c]<> " .. controls "<>control1[r,c]<>" and "<>control2[r,c]<>" .. "<>target[r,c]<>";\n";
draw[{r_,c_,k_}]:="\\draw"<>If[oriented,"[shorten >=0.1cm, shorten <=0.1cm, ->]",""]<>" "<>source[r,c]<> " .. controls "<>control1[r,c]<>" and "<>control2[r,c]<>" .. node[above] {$"<>ToString[k]<>"$} "<>target[r,c]<>";\n";
StringJoin@@(draw/@matrixEntries)
]


(*TikZDualDataFusion[dualPairs_,total_,x0_,\[CapitalDelta]y_]:=Module[{coordinate,draw},
coordinate[k_]:="("<>ToString[N[x0]]<>","<>ToString[N[(k-(total+1)/2)\[CapitalDelta]y]]<>")";
draw[k_]:="\\draw[red, thick] "<>coordinate[2k-1]<>" to[out=135,in=-135] "<>coordinate[2k]<>";\n";
StringJoin@@(draw/@Range[dualPairs])
]*)


Clear[TikZDualDataFusion];
TikZDualDataFusion[l:{___Integer},x0_,\[CapitalDelta]y_]:=Module[{coordinate,draw,total},
total=Length[l];
coordinate[k_Integer]:="("<>ToString[N[x0]]<>","<>ToString[N[(k-(total+1)/2)\[CapitalDelta]y]]<>")";
draw[k_Integer]:="\\draw[red] "<>coordinate[k]<>" to[out=135,in=-135] "<>coordinate[l[[k]]]<>";\n";
StringJoin@@(draw/@Cases[Transpose[{Range[total],l}],{i_,j_}/;i<j:>i])
]


TikZDualDataPrincipal[dualPerm_,x0_,\[CapitalDelta]y_]:=Module[{coordinate,draw,total},
total=Length[dualPerm];
coordinate[k_]:="("<>ToString[N[x0]]<>","<>ToString[N[(k-(total+1)/2)\[CapitalDelta]y]]<>")";
draw[k_]:=Switch[k-dualPerm[[k]],
0, "\\draw[red, thick] "<>coordinate[k]<>" -- +(0,"<>ToString[N[1/3 \[CapitalDelta]y]]<>") ;\n",
c_/;c<0,
"\\draw[red, thick] "<>coordinate[k]<>" to[out=135,in=-135] "<>coordinate[dualPerm[[k]]]<>";\n",
_,""
];
StringJoin@@(draw/@Range[total])
]


TikZFusionGraph[fg_,\[CapitalDelta]x_,\[CapitalDelta]y_]:=Module[{i,d,s,r0},
i=Reverse[UnpackJavaLists[fg@inclusions[]]];
d=Reverse[UnpackJavaLists[fg@dualData[]]]+1;
s=Reverse[UnpackJavaLists[fg@sameDepth[]]];
r0=fg@rankAtDepth[0];
(StringJoin@@Table["\\draw[fill] (0,"<>ToString[N[(k-(r0+1)/2) \[CapitalDelta]y]]<>") circle (0.05);\n",{k,1,r0}])<>
StringJoin@@Table[TikZInclusion[UnpackJavaLists[i[[j]]@asArray[]],(j-1)\[CapitalDelta]x,\[CapitalDelta]x,\[CapitalDelta]y],{j,1,Length[i]}]<>
StringJoin@@Table[TikZDualDataFusion[d[[j]],(*fg@rankAtDepth[j-1],*)(j-1)\[CapitalDelta]x,\[CapitalDelta]y],{j,1,Length[d]}]<>
StringJoin@@Table[TikZSameDepth[UnpackJavaLists[s[[j]]@asArray[]],(j-1)\[CapitalDelta]x,\[CapitalDelta]x,\[CapitalDelta]y],{j,1,Length[s]}]
]


TikZDirectedFusionGraph[dfg_,\[CapitalDelta]x_,\[CapitalDelta]y_]:=Module[{last,next,same,dd,r0},
last=Reverse[UnpackJavaLists[dfg@lastDepth[]]];
next=Reverse[UnpackJavaLists[dfg@nextDepth[]]];
same=Reverse[UnpackJavaLists[dfg@sameDepth[]]];
dd=Reverse[UnpackJavaLists[dfg@dualData[]]]+1;
r0=dfg@rankAtDepth[0];
StringJoin@@Table["\\draw[fill] (0,"<>ToString[N[(k-(r0+1)/2) \[CapitalDelta]y]]<>") circle (0.05);\n",{k,1,r0}]<>
StringJoin@@Table[TikZInclusion[UnpackJavaLists[next[[j]]@asArray[]],(j-1)\[CapitalDelta]x,\[CapitalDelta]x,\[CapitalDelta]y,True],{j,1,Length[next]}]<>
StringJoin@@Table[TikZInclusion[UnpackJavaLists[last[[j]]@asArray[]],(j)\[CapitalDelta]x,-\[CapitalDelta]x,\[CapitalDelta]y,True],{j,1,Length[last]}]<>
StringJoin@@Table[TikZDualDataFusion[dd[[j]],(*dfg@rankAtDepth[j-1],*)(j-1)\[CapitalDelta]x,\[CapitalDelta]y],{j,1,Length[same]}]<>
StringJoin@@Table[TikZSameDepth[UnpackJavaLists[same[[j]]@asArray[]],(j-1)\[CapitalDelta]x,\[CapitalDelta]x,\[CapitalDelta]y,True],{j,1,Length[same]}]
]


TikZPrincipalGraph[pg_GradedBigraph,\[CapitalDelta]x_,\[CapitalDelta]y_]:=Module[{},
(*"\\draw[draw=none] (0,"<>ToString[N[\[CapitalDelta]y/2]]<>") -- (0,"<>ToString[N[-\[CapitalDelta]y/2]]<>");\n"<>*)
StringJoin@@Table["\\draw[fill] (0,"<>ToString[N[(k-(Length[pg[[1,1]]]+1)/2) \[CapitalDelta]y]]<>") circle (0.05);\n",{k,1,Length[pg[[1,1]]]}]<>
StringJoin@@Table[TikZInclusion[pg[[j]],(j-1)\[CapitalDelta]x,\[CapitalDelta]x,\[CapitalDelta]y],{j,1,Length[pg]}]
]


TikZPrincipalGraph[pg_BigraphWithDuals,\[CapitalDelta]x_,\[CapitalDelta]y_]:=
TikZPrincipalGraph[pg[[1]],\[CapitalDelta]x,\[CapitalDelta]y]<>
StringJoin@@Table[TikZDualDataPrincipal[pg[[2]][[j]],2(j-1)\[CapitalDelta]x,\[CapitalDelta]y],{j,1,Length[pg[[2]]]}]


GraphToString[fg_?JavaObjectQ/;InstanceOf[fg,"org.fusionatlas.graphs.FusionGraph"]]:=fg@toString[]
GraphToString[dfg_?JavaObjectQ/;InstanceOf[dfg,"org.fusionatlas.graphs.DirectedFusionGraph"]]:=dfg@toString[]
GraphFromString[s_String/;StringTake[s,2]=="fg"]:=org`fusionatlas`graphs`FusionGraph$`MODULE$@apply[s]
GraphFromString[s_String/;StringTake[s,3]=="dfg"]:=org`fusionatlas`graphs`DirectedFusionGraph$`MODULE$@apply[s]


DisplayFusionGraph=DisplayGraph;


DisplayBigraph=DisplayGraph;


Clear[DisplayGraph]


DisplayGraph[list:{__BigraphWithDuals}]:=Grid[{Magnify[DisplayGraph[#],0.7]&/@list}]
DisplayGraph[list:{__String}]:=Grid[{Magnify[DisplayGraph[#],0.7]&/@list}]


DisplayGraph[g_GraphWithDuals]:=DisplayGraph[GraphToString[g]]


DeleteGraph[g_]:=DeleteFile[FileNameJoin[{FusionAtlasDirectory[],"graphs",GraphHash[GraphToString[g]]<>".pdf"}]]


DisplayGraph[g_]:=DisplayGraph[g]=Module[{file,import},
file=FileNameJoin[{FusionAtlasDirectory[],"graphs",GraphHash[GraphToString[g]]<>".pdf"}];
If[FileExistsQ[file],
import=Import[file];
If[Length[import]!=1,
Print["Import failed: ", file];DeleteFile[file];CreateGraph[g];DisplayGraph[g],
import[[1]]
],
CreateGraph[g]
]
]


DisplayGraph[s_String]:=Module[{g},
g=GraphFromString[s];
If[g===$Failed,$Failed,
DisplayGraph[g]
]
]


findExecutable[cmd_]:=StringTrim[If[$OperatingSystem==="Windows",ReadString["!where "<>cmd],ReadString["!which "<>cmd]]/.EndOfFile->""]


reportNotFound[cmd_,{path_,___}]:=If[$OperatingSystem==="Windows","\""<>path<>"\"",path]
reportNotFound[cmd_,{}]:=(Print["Can't find the executable "<>cmd<>". It is needed for DisplayGraph. Please install it and try again. If you've already installed it, and this still isn't working, look at DisplayGraphs.nb or complain to Scott."];cmd)


pdflatexPath=reportNotFound["pdflatex",FileNames[{"/usr/texbin/pdflatex","/Library/TeX/texbin/pdflatex",findExecutable["pdflatex"]}]];
pdftexPath=reportNotFound["pdftex",FileNames[{"/usr/texbin/pdftex","/Library/TeX/texbin/pdftex",findExecutable["pdftex"]}]];
pdfcropPath=reportNotFound["pdfcrop",FileNames[{"/usr/texbin/pdfcrop","/Library/TeX/texbin/pdfcrop",findExecutable["pdfcrop"]}]];
gsPath=reportNotFound["gs",FileNames[{"/usr/bin/gs","/usr/local/bin/gs","C:\\Program Files\\gs\\gs9.20\\bin\\gswin64c.exe",findExecutable["gs"]}]];
perlPath=reportNotFound["perl",FileNames[{"/opt/local/bin/perl",findExecutable["perl"]}]];


CreateFiles[g_,function_,filename_]:=
Module[{tikz,result,cmd,cat,rm,semicolon},
{cat,rm,semicolon}=If[$OperatingSystem==="Windows",{"type","del"," & "},{"cat","rm"," ; "}];
tikz="$$\\begin{tikzpicture}\n"<>function[g]<>"\\end{tikzpicture}$$";
SetDirectory[FileNameJoin[{FusionAtlasDirectory[],"tikz-diagrams"}]];
If[Length[Streams["snippet"]]>0,Close["snippet"]];
If[FileExistsQ["snippet"],DeleteFile["snippet"]];
WriteString[OpenWrite["snippet"],tikz];
Close["snippet"];
Off[CopyFile::"filex"];
CopyFile["snippet",FileNameJoin[{FusionAtlasDirectory[],"graphs",filename<>".tex"}]];
ReadString[
cmd="!"<>
cat<>" top-template > document.tex"<>semicolon<>
cat<>" snippet >> document.tex"<>semicolon<>
cat<>" bottom-template >> document.tex"<>semicolon<>
pdflatexPath<>" document.tex > log"<>semicolon<>
pdfcropPath<>" document.pdf --gscmd "<>gsPath<>" --pdftexcmd "<>pdftexPath<>" --verbose --debug >> log"<>semicolon<>
gsPath<>" -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=finished.pdf document-crop.pdf >> log"<>semicolon<>rm<>" tmp-pdfcrop-*"<>semicolon<>
rm<>" document.*"<>semicolon<>
rm<>" snippet"<>semicolon];
If[!FileExistsQ["finished.pdf"],Print["Generating a diagram failed --- no PDF produced."];Print[cmd];Print[tikz];Abort[]];
CopyFile["finished.pdf",FileNameJoin[{FusionAtlasDirectory[],"graphs",filename<>".pdf"}]];
result=Import["finished.pdf"][[1]];
DeleteFile/@FileNames["*.pdf"];
ResetDirectory[];
result
]


(*Returns the right-most 16 digits of the hexadecimal representation of the MD5 hash of graphName, padded on the left with 0's if necessary*)
GraphHash[g_GradedBigraph]:=GraphHash[GraphToString[g]]
GraphHash[g_BigraphWithDuals]:=GraphHash[GraphToString[g]]
GraphHash[g_/;JavaObjectQ]:=GraphHash[GraphToString[g]]
GraphHash[graphName_String]:=GraphHash[graphName]=StringJoin[PadLeft[Characters[ToUpperCase[IntegerString[StringHash[graphName,"MD5"],16]]],16]]

(*Workaround to account for Mathematica hashing the quotation marks with the string*)
StringHash[string_String,type_: "MD5"]:=Module[{stream,file,hash},stream=OpenWrite[];
WriteString[stream,string];
file=Close[stream];
hash=FileHash[file,type];
DeleteFile[file];
hash]


CreateGraph[g_]:=Module[{dir,TikzFunction},
dir=FileNameJoin[{FusionAtlasDirectory[],"graphs"}];
If[!FileExistsQ[dir],CreateDirectory[dir]];
SetDirectory[dir];
WriteString[OpenAppend["lookup.tex"],"\\hashdef{" <>GraphToString[g]<>"}{" <>GraphHash[GraphToString[g]]<>"}%\n"];
Close["lookup.tex"];
ResetDirectory[];
TikzFunction=If[JavaObjectQ[g],
If[InstanceOf[g,"org.fusionatlas.graphs.FusionGraph"],
TikZFusionGraph,TikZDirectedFusionGraph],
TikZPrincipalGraph
];
CreateFiles[g,TikzFunction[#,1,HeightFactor[g]]&,GraphHash[GraphToString[g]]]
]


GraphDepth[g_?JavaObjectQ/;InstanceOf[g,"org.fusionatlas.graphs.FusionGraph"]\[Or]InstanceOf[g,"org.fusionatlas.graphs.DirectedFusionGraph"]]:=g@graphDepth[]
RankAtDepth[g_?JavaObjectQ/;InstanceOf[g,"org.fusionatlas.graphs.FusionGraph"]\[Or]InstanceOf[g,"org.fusionatlas.graphs.DirectedFusionGraph"],d_Integer]:=g@rankAtDepth[d]


HeightFactor[g_]:=Module[{h},
h=Max[RankAtDepth[g,#]&/@Range[GraphDepth[g]]];
If[h==1,1,1/(h-1)]
]


FusionRank[FusionRules[_,{{0,0,0}->m_}]]:=Length[m]


FusionGraphsTable[fr_FusionRules]:=Table[DisplayGraph[ExtractFusionGraph[fr,k]],{k,2,FusionRank[fr]}]


DisplayFusionGraphs[fr_FusionRules]:=Grid[FusionGraphsTable[fr],Frame->True]


SubfactorGraphsTable[fr_FusionRules]:=Table[DisplayGraph[ExtractPairOfBigraphsWithDuals[fr,k]],{k,2,FusionRank[fr]}]


DisplaySubfactorGraphs[fr_FusionRules]:=Grid[SubfactorGraphsTable[fr],Frame->True]


DisplayGraph[fr_FusionRules]:=Grid[Transpose[{Rest[FPDimensions[fr]],FusionGraphsTable[fr],SubfactorGraphsTable[fr]}],Frame->True]


End[];


EndPackage[]
