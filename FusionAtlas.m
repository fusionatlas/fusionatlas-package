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



DeclarePackage["FusionAtlas`Bigraphs`",{"GradedGraph","GradedBigraph","GraphRank","GraphEvenRank","GraphOddRank","GraphDepth","RankAtDepth","GraphAdjacencyMatrix","EvenPart","GraphIndex","DimensionOfGenerator","ReducedDimensionOfGenerator","NumericDimensionOfGenerator","DimensionsByDepth","NumericDimensionsByDepth","ReducedDimensionsByDepth","GlobalEvenDimension","GlobalDimension","DimensionAtMostQ","DimensionOfLowWeightSpace","StableLowWeightSpaceDimensions","AnnularTanglesSubgraphTest","AnnularTanglesTest","FindBigraphExtensions","FindBigraphExtensionsUpToRank","FindBigraphExtensionsUpToRankAndDepth","CanonicalizeBigraph","AnBigraph","DnBigraph","EnBigraph","trivalentBigraph","haagerupFamilyBigraph","dualHaagerupFamilyBigraph","HaagerupBigraph","DualHaagerupBigraph","IzumiStarBigraph","ExtendedDnBigraph","HexagonBigraph","DualHexagonBigraph","HaagerupAsaedaBigraph","DualHaagerupAsaedaBigraph","ExtendedEnBigraph","S4S5Bigraph", "A4A5Bigraph","A5A6Bigraph", "GraphToString","GraphFromString","ClearCachedGraphFromString","CachedGraphFromString","DepthOfBranchPoint","HasSimpleBranchQ","Truncate","BranchFactor"}];


DeclarePackage["FusionAtlas`DisplayGraphs`",{"DisplayFusionGraph","DisplayBigraph","DisplayGraph","GraphHash","DeleteGraph"}];


DeclarePackage["FusionAtlas`CyclotomicityBound`",{"CyclotomicityBound","CyclotomicTranslates","CyclotomicTranslatesWithWitnesses"}];


DeclarePackage["FusionAtlas`Java`",{"FusionAtlasJavaDirectory","FusionAtlasScalaDirectory","RestartJava","AsScalaSet","AsScalaList","AsScalaOption","AsScalaObject","FromScalaObject","UnpackJavaLists","BuildScalaLibraries","CleanScalaLibraries","RebuildScalaLibraries","UnpackJavaObjects","GraphToScalaObject"}];


DeclarePackage["FusionAtlas`JavaOdometer`",{"FindBigraphExtensionsUpToRankJava","FindBigraphExtensionsUpToRankAndDepthJava","FindBigraphExtensionsUpToRankAndDepthJavaIterator","FindBigraphExtensionsMatching"}];


DeclarePackage["FusionAtlas`FindAllBigraphs`",{"AlgebraicIntegerTest","PositivityTest","GHJTest","BigraphConditions","AllBigraphs","FindAllBigraphs","RequestAllBigraphs","KnownObstructions","FindAllObstructions","RequestMoreBigraphs"}]


DeclarePackage["FusionAtlas`PackageData`",
{"ValuesAsString","DefiniteValuesAsString","PackageData","MatchingValues","DefiniteValues","PackageEverything","PackageCachedRootReduce","PackageDimensions","PackageBigraphExtensions","PackageBifusionAlgebras","PackageLowWeightSpaces","PackageAlgebraicIntegerTest","PackageObstructions","PackageKnapsackTest","PackageFindGraphPartners","PackageLowestWeightEigenspaces","PackageIdempotents","PackagePairExtensions"}];


DeclarePackage["FusionAtlas`TensorSolver`",
{"FindFusionRules","FindFusionRulesTimeLimit","FlipFusionRules","EvenPartFusionRules","FusionRules"}];


DeclarePackage["FusionAtlas`ExtractGraphs`",
{"ExtractFusionGraphLabels","ExtractFusionGraph","ExtractPairOfBigraphsWithDuals"}];


DeclarePackage["FusionAtlas`FormalCodegrees`",
{"FormalCodegreesInDimensionFieldTest","FormalCodegrees","FindDimensionFunctions","FormalCodegreesInequalityTest","FPDimensions"}];


DeclarePackage["FusionAtlas`Iterators`",
{"Iterator"}];


DeclarePackage["FusionAtlas`InductionMatrix`",
{"InductionMatrices","AllDecompositions"}];


DeclarePackage["FusionAtlas`ModularData`",
{"FindModularData","ExplicitGenerators","ExecuteGAP","CharacterTable","GaloisGroup","GaloisAction","GaloisGroupGenerators","PossibleConductors","workOnRepresentations","SInRepresentation","TInRepresentation","DimensionsFromInductionMatrix","GaloisOrbitClumps","PossibleGaloisImages","PossibleGaloisTraces","RepresentationsForRank","RepresentationsForInductionMatrix","SaveRepresentationsForRank","SaveCharacterTables","SaveGenerators","SaveConjugacyClasses","SaveAllModularData","ClearSavedModularData","AllocateEigenvaluesToGaloisOrbitClumps","AllocateEigenvaluesToSimples","AllocateEigenvaluesToSimplesAndCheckFrobeniusSchurIndicators","AllocateEigenvaluesToSimplesAndCompleteGaloisActions","FindQLinearSolutions"}];


DeclarePackage["FusionAtlas`FindGraphPartners`",
{"FindGraphPartners","FindWeedPartners"}];


DeclarePackage["FusionAtlas`CyclotomicTest`",
{"CyclotomicTest"}];


DeclarePackage["FusionAtlas`KnapsackTest`",
{"KnapsackTest"}];


DeclarePackage["FusionAtlas`QuadraticTanglesTest`",
{"ParityTest","HaagerupTypeBranchTest"}];


DeclarePackage["FusionAtlas`dTest`",{"DTest"}];


DeclarePackage["FusionAtlas`Debugging`",
{"DebugEcho","DebugPrint","DebugEvaluate","SetDebugMode","SetDebugQueueInterval","SetDebugFile","DebugPrintHeld","ToStringHeld"}];


DeclarePackage["FusionAtlas`RemoteInterface`",
{"RecordObstructions","RequestObstructions","LookupObstructions","LoadObstructions","LookupCanonicalForms","ProcessObstructionsQueue","WorkOnObstructionsQueue","ClearObstructionsQueue","ObstructionsQueueLength","EstimateQueueCompletionTime","RetrieveGraphs","LookupGraphs","TakeGraphs","RecordCanonicalForms","RecordFindGraphPartners","GraphsNeedingPartners","ReapplyTestToDatabase","RecordRequestAllBigraphs","BigraphRequests","MaximalBigraphRequests","MaximalIncompleteBigraphRequests","DisplayBigraphRequests","QueueExpression"}];


DeclarePackage["FusionAtlas`GraphPairs`",{"BigraphWithDuals","GraphWithDuals","DualData","OcneanuDisplay","HaagerupWithDuals","DualHaagerupWithDuals","HaagerupAsaedaWithDuals","DualHaagerupAsaedaWithDuals","TwistBigraph","FullOcneanuTest","PartialOcneanuTest","DualDataList","CompatibleDualDataList","CheckDualDimensions","GraphPairsIsomorphicQ","ClearGraphPairsCachedValues","PermuteToReduceCrossings","GraphVertices","DirectionsFromVertices","EdgesAdjacentTo","GraphPathsFrom","GraphPathsTo","GraphPathsBetween","GraphPaths","GraphLoops","ConcatenatePaths","PathLength","ReverseLoop","Vertex","SplitPathAt","PathToLoop","TruncatesToOneOfQ","RemoveDuplicates","ScalaTriplePointObstruction","StabilitySequence","CylinderQ","CylinderObstruction"}];


DeclarePackage["FusionAtlas`PairOdometer`",{"FindBigraphPairExtensionsUpToDepth","FamiliesOfBigraphPairs","DisplayFamiliesOfBigraphPairs","ChooseIsomorphismRepresentatives"}];


DeclarePackage["FusionAtlas`AfzalyEnumerator`",{"FilteredEnumerate","FilteredEnumerateToRank","FilteredEnumerateToDepth","IgnoredWeeds"}];


DeclarePackage["FusionAtlas`Enumerator2014`",{"DescendantsTree","FindGraphPartners2"}];


(*
DeclarePackage["FusionAtlas`GraphPlanarAlgebra`",{
"GPAElement","GPAMatrix",
"NumberFieldGenerator","EnlargeNumberField",
"LopsidedDimension","EmptyGPAElement","GPACoefficients","GPAMultiply",
"RotateOneClick","RotateTwoClicks","CapTopLeft","CapAt","AddStrandOnRight", "AddCupOnTop","JonesWenzlIdempotent",
"LowestWeightEigenvectorConditions",
"LowestWeightEigenspace"
}]
*)


DeclarePackage["FusionAtlas`GraphPlanarAlgebra4`",{
"NumberFieldGenerator","EnlargeNumberField","RedefineLopsidedDimension","LopsidedDimension","SphericalDimension","CriticalPointCoefficient","GPA4Element","EmptyGPA4Element","CapTopLeft","PartialTrace","GPA4Trace","GPA4TraceEvaluation","TurnUpBottomRightCorner","TurnDownTopRightCorner","TurnUpBottomLeftCorner","TurnDownTopLeftCorner","RotateOneClick","RotateTwoClicks","GPAFourierTransform","GPAFourierTransformTwice","AddStrandOnRight","AddStrandOnLeft","AddStrandsOnLeft","GPAMultiply","GPAConjugate","GPACoefficients","GPACoefficientsAtStar","VariableGPA4Element","GPA4Matrix","GPATensor","GPAInverse","Rotate\[Pi]Clockwise","Rotate\[Pi]Counterclockwise","GPAMultiplyWithOffset","PivotalStructure","ConnectionGrid","StrandCrossingAbove","StrandCrossingBelow","TwoStrandFlatness","OneStrandFlatness","FusionRulesFromConnection","LowestWeightEigenvectorConditions","LowestWeightConditions","CollectGPA4Matrix","CollectGPA4Element","LowestWeightEigenspace","LowestWeightSpace","LoadLowestWeightEigenspaces","ChangePivotalStructure","NumberFieldGauge","FindEquationsForFlatGenerators","FindFlatGenerators","FindFlatLowestWeightVectors","qInteger\[Delta]","JonesWenzlIdempotent","LoadIdempotents","S2Equation","RowReducedS2Equation","S2Solutions","IdentityTL","OneCupTL","OneCupJonesWenzl","SomeOneCupJonesWenzl","AnnularConsequences","GPACirc","GPAStar","GaugeTransform", "GaugeAction", "VariableGaugeElement", "FindGaugeElementRelating","WenzlRecursion"
}]


DeclarePackage["FusionAtlas`Connections`",{"ClearConnection","ConnectionEquations","SimplifyConnectionEquations","BranchMatrix","UUt","ConnectionNormEquations","ConnectionPhaseEquations","SolveConnectionNormEquations","SolveConnectionNormEquationsForExtraDimensions","SolveConnectionPhaseEquations","ConnectionNormConditions","ReducedConnectionNormConditions","ConnectionTriangleInequalities","ConnectionPolygonInequalities","IndexRangeFromConnectionNorms","IndexRangeFromTriangleInequalities","ConnectionTrianglesNormsAndArgs","complicatedOnes","newComplicatedOne","PhaseSolver","LawOfCosinesAlternatives","norm","arg"}];


DeclarePackage["FusionAtlas`RelativeDimensions`",{"RelativeDimensionsByDepthEquations","SolveRelativeDimensionsByDepthEquations","RelativeBranchFactor","RelativeDimensionsByDepth","RelativeDimensionsConditions","PositiveOmegaInequality"}];


DeclarePackage["FusionAtlas`TriplePointTest`",{"TriplePointObstruction","EvenTriplePointObstruction","OddTriplePointObstruction"}];


DeclarePackage["FusionAtlas`Utilities`IntersectSubspaces`",{"IntersectSubspaces"}];


Print[
"Loading FusionAtlas` version 0\n",
"Read more at http://tqft.net/wiki/Atlas_of_subfactors"
]


If[$VersionNumber<6,
BeginPackage["FusionAtlas`",{"LinearAlgebra`MatrixManipulation`"}],
BeginPackage["FusionAtlas`"]
]


FusionAtlasDirectory::usage="FusionAtlasDirectory[] should hopefully return the location the FusionAtlas` package was loaded from.";
FusionAtlasDataDirectory::usage="FusionAtlasDataDirectory[] specifies were the FusionAtlas` package should look for, and save, precomputed data.";


FusionAtlas::loading="Loading precomputed data in `1`.";


cachedRootReduce::usage="";


UnsortedUnion::usage="UnsortedUnion[list] a list of all the unique elements in list, in the order that they first appear.";


FindFirst::usage="";


RealMax::usage="";
RealMaxPosition::usage="";


NaturalQ::usage="NaturalNumberQ[n] tests if n is a non-negative integer.";


qInteger::usage="qInteger[n][q] computes the quantum integer n with the variable q; in our conventions qInteger[2][q]==q+\!\(\*SuperscriptBox[\(q\), \(-1\)]\)";
{qFactorial,qBinomial};


If[$VersionNumber<7,
SplitBy[x_,f_]:=Split[x,f[#1]===f[#2]&]
]


If[$VersionNumber<6,
UnitVector::usage="UnitVector[n,i] returns the i-th n-dimensional unit vector, if i is an integer between 1 and n, and the n-dimensional zero vector otherwise.";
]


If[$VersionNumber>=6,
BlockMatrix=ArrayFlatten;
ZeroMatrix[n_]:=ZeroMatrix[n,n];
ZeroMatrix[n_,m_]:=ConstantArray[0,{n,m}]
]


ZeroVector::usage="ZeroVector[n] returns the n-dimensional 0 vector.";


UnitVectorQ::usage="UnitVectorQ[v] tests if v is a unit coordinate vector.";


ZeroVectorQ::usage="ZeroVectorQ[v] tests if v is the zero vector.";


ZeroMatrixQ::usage=""


qFromd::usage="Computes q given d=q+q^(-1)";


$parallelMode::usage="'$parallelMode = True' allows the FusionAtlas to use multiple kernels.";
parallelRootReduce;
progressiveRootReduce;


Begin["`Private`"];


FusionAtlasDirectory[]:=FusionAtlasDirectory[]=StringDrop[(File/.Flatten[FileInformation[ToFileName[#,"FusionAtlas"]]&/@(Reverse[$Path]/."."->Directory[])]),-12]


(*might be dangerous if FusionAtlasDirectory[] is somehow incorrect!*)
If[!MemberQ[$Path,FusionAtlasDirectory[]],AppendTo[$Path,FusionAtlasDirectory[]]]


(*If[StringTake[FusionAtlasDirectory[],-7]=="package",
FusionAtlasDataDirectory[]:=StringDrop[FusionAtlasDirectory[],-7]<>"data";
Print["Found precomputed data in ",FusionAtlasDataDirectory[]];
If[!MemberQ[$Path,FusionAtlasDataDirectory[]],AppendTo[$Path,FusionAtlasDataDirectory[]]],
Print["Remember to set FusionAtlasDataDirectory[] to the appropriate path, if you've downloaded precomputed data."]
];*)


UnsortedUnion[x_]:=Module[{f},f[y_]:=(f[y]=Sequence[];y);f/@x]


cachedRootReduceCount=0;


cachedRootReduce[x_Root]:=x
cachedRootReduce[x_Integer]:=x
cachedRootReduce[x_Complex]:=x
cachedRootReduce[x_]:=(++cachedRootReduceCount;cachedRootReduce[x]=RootReduce[x])
cachedRootReduce[x_List]:=cachedRootReduce/@x


progressiveRootReduce[x_Plus]:=cachedRootReduce[progressiveRootReduce/@x]
progressiveRootReduce[x_Times]:=cachedRootReduce[progressiveRootReduce/@x]
progressiveRootReduce[x_Root]:=x
progressiveRootReduce[x_]:=cachedRootReduce[x]
progressiveRootReduce[x_List] :=progressiveRootReduce/@x
progressiveRootReduce[x_->y_]:=x->progressiveRootReduce[y]


parallelModeHook:=
($parallelMode/:Set[$parallelMode,x_]:=(UpValues[$parallelMode]={};$parallelMode=x;setupParallelMode;parallelModeHook;x))


parallelModeHook


setupParallelMode:=Module[{},
If[$parallelMode,
CloseKernels[];
LaunchKernels[];
With[{path=FusionAtlasDirectory[]},
ParallelEvaluate[
$Path=$Path~Join~{path};
]];
ParallelNeeds["FusionAtlas`"];
]
]


$parallelMode=False;


conditionalParallelMap[X___]:=If[$parallelMode===True,ParallelMap[X],Map[X]]


Clear[parallelRootReduce]
parallelRootReduce[x_Plus]:=progressiveRootReduce[conditionalParallelMap[progressiveRootReduce,x]]
parallelRootReduce[x_Times]:=progressiveRootReduce[conditionalParallelMap[progressiveRootReduce,x]]
parallelRootReduce[x_]:=progressiveRootReduce[x]
unflatten[e_,{d__?((IntegerQ[#]&&Positive[#])&)}]:= Fold[Partition,e,Take[{d},{-1,2,-1}]] /;(Length[e]===Times[d])
parallelRootReduce[x_?ArrayQ]:=unflatten[With[{flat=Flatten[x]},FusionAtlas`Private`conditionalParallelMap[progressiveRootReduce,flat]],Dimensions[x]]
parallelRootReduce[x_List] :=conditionalParallelMap[progressiveRootReduce,x]
parallelRootReduce[x_->y_]:=x->parallelRootReduce[y]


NaturalQ[n_]:=NonNegative[n]&&IntegerQ[n]


FindFirst[a_,b_]:=With[{p=Position[a,b,{1},1,Heads->False]},If[Length[p]==0,-1,p[[1,1]]]]


RealMax[a_] := Max[Cases[a,x_/;Im[x]==0]]


RealMaxPosition[a_] := FindFirst[a,RealMax[a]]


If[$VersionNumber>=6.,
Unprotect[IdentityMatrix];
IdentityMatrix[0]={};
Protect[IdentityMatrix];
]


ZeroVector[n_]:=Table[0,{n}]


If[$VersionNumber<6,
(UnitVector[n_,i_Integer]/;(1<=i<=n):=Module[{z=Table[0,{n}]},z[[i]]=1;z]);
(UnitVector[n_,i_Integer]:=Table[0,{n}]),
Unprotect[UnitVector];
(UnitVector[n_,i_Integer]/;(i<1\[Or]i>n):=(Message[UnitVector::nokun,n,i];ZeroVector[n]));
Protect[UnitVector];
]


UnitVectorQ[v_?VectorQ]:=Complement[v,{0,1}]=={}\[And]Count[v,1]==1


ZeroVectorQ[v_?VectorQ]:=Union[v]==={0}\[Or]v=={}


qFromd[d_]:= cachedRootReduce[(d+Sqrt[d^2-4])/2]


qInteger[n_][q_]:=RootReduce[Sum[q^i,{i,-n+1,n-1,2}]]


End[];


EndPackage[];
