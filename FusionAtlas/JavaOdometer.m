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



BeginPackage["FusionAtlas`JavaOdometer`",{"FusionAtlas`","FusionAtlas`Bigraphs`","FusionAtlas`Debugging`","FusionAtlas`RemoteInterface`","FusionAtlas`Java`","JLink`"}];


FindBigraphExtensionsUpToRankJava::usage="";


FindBigraphExtensionsUpToRankAndDepthJava;FindBigraphExtensionsUpToRankAndDepthJavaIterator;


FindBigraphExtensionsMatching::usage="";


Begin["`Private`"];


FindBigraphExtensionsUpToRankJava[norm_][g_GradedBigraph,totalRank_,maximalLoops_:(-1)]:=Module[{timing,expressionStrings,expression,union,result},
If[totalRank-FusionAtlas`Bigraphs`GraphRank[g]<=0,Return[{}]];
DebugPrint["Entering java ... ",g,".findExtensionsUpToRank(",norm,",",If[totalRank==\[Infinity],-1,totalRank-FusionAtlas`Bigraphs`GraphRank[g]],",",maximalLoops,")"];
{timing,expressionStrings}=AbsoluteTiming[
Proxy`findExtensionsUpToRank[GraphToString[g],N[norm]+10.^-6,If[totalRank==\[Infinity],-1,totalRank-FusionAtlas`Bigraphs`GraphRank[g]],maximalLoops]];
DebugPrint["... expression returned from java in ",timing," seconds, with ",Length[expressionStrings], " strings."];
expression=GraphFromString/@expressionStrings;
DebugPrint["Finished interpreting, ",Length[expression], " GradedBigraphs returned."];
union=Union[FusionAtlas`Bigraphs`CanonicalizeBigraph/@expression];
DebugPrint["Reduced to ",Length[union], " GradedBigraphs after canonicalizing."];
result=Cases[union,c_/;FusionAtlas`Bigraphs`DimensionAtMostQ[norm][c]];
DebugPrint["... of which ",Length[result], " actually have small enough norm."];
result
]


FindBigraphExtensionsUpToRankAndDepthJava[norm_][g_GradedBigraph,totalRank_,depth_]:=
Module[{timing,expressionStrings,expression,union,result},
If[totalRank<=FusionAtlas`Bigraphs`GraphRank[g],Return[{}]];
If[depth<FusionAtlas`Bigraphs`GraphDepth[g],Return[{}]];
If[depth==FusionAtlas`Bigraphs`GraphDepth[g],Return[{GraphToString[g]}]];
DebugPrint["Entering java ... ",g,".findExtensionsUpToRankAndDepth(",norm,",",If[totalRank==\[Infinity],-1,totalRank],",",depth,")"];
{timing,expressionStrings}=AbsoluteTiming[GradedBigraph`iterableToStringArray[JavaNew["org.fusionatlas.odometer.GradedBigraph",List@@g]@findExtensionsUpToRankAndDepth[N[norm]+10.^-6,If[totalRank==\[Infinity],-1,totalRank],depth]]];
DebugPrint["... expression returned from java in ",timing," seconds, with ",Length[expressionStrings], " strings."];
expressionStrings
]


FindBigraphExtensionsUpToRankAndDepthJavaIterator[norm_][g_GradedBigraph,totalRank_,depth_]:=
Module[{timing,iter,expression,union,result},
If[totalRank<=FusionAtlas`Bigraphs`GraphRank[g],Return[{}]];
If[depth<FusionAtlas`Bigraphs`GraphDepth[g],Return[{}]];
If[depth==FusionAtlas`Bigraphs`GraphDepth[g],Return[{g}]];
DebugPrint["Entering java ... ",g,".findExtensionsUpToRankAndDepth(",norm,",",If[totalRank==\[Infinity],-1,totalRank],",",depth,")"];
{timing,iter}=AbsoluteTiming[(GradedBigraph`iterableToStringArrayIterable[JavaNew["org.fusionatlas.odometer.GradedBigraph",List@@g]@findExtensionsUpToRankAndDepth[N[norm]+10.^-6,If[totalRank==\[Infinity],-1,totalRank],depth],10000])@iterator[]];
DebugPrint["... iterator returned from java in ",timing," seconds."];
iter
]


FindBigraphExtensionsMatching[norm_][short_GradedBigraph,long_GradedBigraph]:=FindBigraphExtensionsMatching[norm][short,{long}][[1]]


FindBigraphExtensionsMatching[norm_][short_GradedBigraph,longs:{}]:={}


FindBigraphExtensionsMatchingServer[norm_][short_GradedBigraph,longs:List[__GradedBigraph]]:=Module[{URL, timing,expressionStrings,graphs,result},
URL="http://localhost:8182/graphs/"<>GraphToString[short]<>"/ExtensionsMatching/"<>ToString[N[norm]+10.^-6]<>"/"<>StringReplace[ToString[GraphToString/@longs],{" "->"","{"->"%7B","}"->"%7D"}];
DebugPrint["Calling server ... ",Short[URL]];
{timing,expressionStrings}=AbsoluteTiming[
ToExpression[Import[URL]]];
If[expressionStrings===$Failed,
ServerCrash={short,longs};
Print["It appears an error occurred while calling the server. Is it running?"];
];
DebugPrint["... expression returned from server in ",timing," seconds, with ",Plus@@(Length/@expressionStrings), " strings."];
graphs=Map[CachedGraphFromString,expressionStrings,{2}];
ClearCachedGraphFromString[];
DebugPrint["... finished converting strings."];
graphs
(*result=Cases[#,g_/;FusionAtlas`Bigraphs`DimensionAtMostQ[norm][g]]&/@graphs;
DebugPrint["... finished verifying dimensions (",Length[Flatten[result]],"/",Length[Flatten[graphs]],")"];
result*)
]


FindBigraphExtensionsMatchingJLink[norm_][short_GradedBigraph,longs:List[__GradedBigraph]]:=Module[{timing,expressionStrings,graphs,result},
DebugPrint["Entering java ... ",short,".findExtensionsMatching(",Short[longs],",",norm,")"];
{timing,expressionStrings}=AbsoluteTiming[
Proxy`findExtensionsMatching[GraphToString[short],GraphToString/@longs,N[norm]+10.^-6]];
If[expressionStrings===$Failed,
Print["It appears an error occurred in Java during FindBigraphExtensionsMatchingNoDuplicates. Restarting Java and trying again ."];
JavaCrash={short,longs};
RestartJava[];
Return[FindBigraphExtensionsMatchingJLink[norm][short,longs]];
];
DebugPrint["... expression returned from java in ",timing," seconds, with ",Plus@@(Length/@expressionStrings), " strings."];
graphs=Map[CachedGraphFromString,expressionStrings,{2}];
ClearCachedGraphFromString[];
DebugPrint["... finished converting strings."];
graphs
(*result=Cases[#,g_/;FusionAtlas`Bigraphs`DimensionAtMostQ[norm][g]]&/@graphs;
DebugPrint["... finished verifying dimensions (",Length[Flatten[result]],"/",Length[Flatten[graphs]],")"];
result*)
]


RLE[list_]:={First[#],Length[#]}&/@Split[list]


RLD[list_]:=Flatten[Table[#[[1]],{#[[2]]}]&/@list,1]


FindBigraphExtensionsMatching[norm_][short_GradedBigraph,longs:List[__GradedBigraph]]:=Module[{union=Union[longs],noduplicates,graphs,repeats,result},
{graphs,repeats}=Transpose[RLE[longs]];
If[Length[union]!=Length[graphs],DebugPrint["Hmm, maybe RLE isn't the right solution!"]];
DebugPrint[Length[graphs]];
noduplicates=FindBigraphExtensionsMatchingJLink[norm][short,graphs];
DebugPrint["Beginning RLD, ",Tally[repeats]];
result=RLD[Transpose[{noduplicates,repeats}]];
DebugPrint["Finished RLD"];
result
]


(*FindBigraphExtensionsMatching[norm_][short_GradedBigraph,longs:List[__GradedBigraph]]:=Module[{union=Union[longs],noduplicates},
noduplicates=FindBigraphExtensionsMatchingJLink[norm][short,union];
(* it seems this is really slow *)
Table[noduplicates[[FindFirst[union,longs[[i]]]]],{i,1,Length[longs]}]
]*)



End[];


EndPackage[];
