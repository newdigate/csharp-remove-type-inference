using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
namespace type_deinference;

public class TypeDeInference : ITypeDeInference
{
    public string? RemoveTypeInference(string source, IEnumerable<MetadataReference> references)
    {
        IDictionary<string, string> result = RemoveTypeInference(new [] { string.Empty }, references, s => source);
        if (result.ContainsKey(string.Empty))
            return result[string.Empty];
        return null;
    }

    public IDictionary<string, string> RemoveTypeInference(
        IEnumerable<string> sourceIdentifiers, 
        IEnumerable<MetadataReference> references,
        Func<string, string> getSourceFromIdentifier)
    {
        Dictionary<string, string> sourceCodeByIdentifier = new Dictionary<string, string>();
        foreach (string sourceIdentifier in sourceIdentifiers) {
            sourceCodeByIdentifier.Add( sourceIdentifier, getSourceFromIdentifier(sourceIdentifier));
        }

        CSharpCompilation compilation = 
            CompileCSharp(
                sourceCodeByIdentifier.Values, 
                references,
                out IDictionary<SyntaxTree, CompilationUnitSyntax> trees);

        Func<SyntaxTree, string> fnKeyForSyntaxTree = 
            tree => 
                sourceCodeByIdentifier
                    .FirstOrDefault(kvp => kvp.Value == tree.ToString())
                    .Key;

        return Process(trees, compilation, fnKeyForSyntaxTree);
    }

    public IDictionary<string, string> Process(IDictionary<SyntaxTree, CompilationUnitSyntax> trees, Compilation compilation, Func<SyntaxTree, string> fnGetKeyForSyntaxTree){
        Dictionary<string, string> result = new Dictionary<string, string>();
        foreach (KeyValuePair<SyntaxTree, CompilationUnitSyntax> tree in trees) {
            IEnumerable<VariableDeclarationSyntax> typeInferenceVariableDeclarations = GetVariableDeclarationSyntaxUsingTypeInference(tree.Key);
            IEnumerable<ForEachStatementSyntax> typeInferencesForEachStatements = GetForEachSyntaxUsingTypeInference(tree.Key);
            IEnumerable<CSharpSyntaxNode> typeInferenceSyntaxes =                         
                typeInferenceVariableDeclarations
                    .Cast<CSharpSyntaxNode>()
                    .Union(typeInferencesForEachStatements);
            if (typeInferenceSyntaxes.Count() == 0)
                continue;

            SemanticModel model = compilation.GetSemanticModel(tree.Key);
            CompilationUnitSyntax newSyntax =
                tree.Value
                    .ReplaceNodes(
                        typeInferenceSyntaxes, 
                            (a,b) => {
                                if (a is VariableDeclarationSyntax variableDeclarationSyntax)
                                    return ReplaceVariableDeclarationSyntaxWithDeinferedType(model, variableDeclarationSyntax)?? a;
                                if (a is ForEachStatementSyntax forEachStatementSyntax)
                                    return ReplaceForeachSyntaxWithDeinferedType(model, forEachStatementSyntax)?? a;
                                return a;
                            });

            string newSyntaxString = newSyntax.ToString();
            if (tree.Value.ToString() == newSyntaxString) continue;
            string? key = fnGetKeyForSyntaxTree(tree.Key);
            if (key == null) continue;
            result.Add(key, newSyntaxString);
        }
        return result;
    }

    private CSharpCompilation CompileCSharp(
        IEnumerable<string> sourceCodes, 
        IEnumerable<MetadataReference> references,
        out IDictionary<SyntaxTree, CompilationUnitSyntax> roots) 
    {
        Dictionary<SyntaxTree, CompilationUnitSyntax> syntaxTrees = new Dictionary<SyntaxTree, CompilationUnitSyntax>();
        roots = syntaxTrees;

        foreach (string sourceCode in sourceCodes) {
            SyntaxTree tree = CSharpSyntaxTree.ParseText(sourceCode);
            CompilationUnitSyntax root = tree.GetCompilationUnitRoot();
            syntaxTrees[tree] = root;
        }
        CSharpCompilationOptions cSharpCompilationOptions = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary);

        CSharpCompilation compilation = 
            CSharpCompilation
                .Create(
                    "assemblyName",
                    syntaxTrees.Keys,
                    references,
                    cSharpCompilationOptions
                   );
        foreach (var d in compilation.GetDiagnostics())
        {
            Console.WriteLine(CSharpDiagnosticFormatter.Instance.Format(d));
        }
        return compilation;
    }

    private SyntaxNode? ReplaceVariableDeclarationSyntaxWithDeinferedType(SemanticModel model, VariableDeclarationSyntax a) {
        SyntaxNode? expression = a.Variables.FirstOrDefault()?.Initializer?.Value;
        if (expression == null)
            return null;

        Microsoft.CodeAnalysis.TypeInfo typeInfo = model.GetTypeInfo(expression);
        if (typeInfo.ConvertedType == null)
            return null;

        string replacementTypeName = typeInfo.ConvertedType.ToMinimalDisplayString(model, NullableFlowState.None, a.SpanStart);
        TypeSyntax replacement = 
            SyntaxFactory
                .IdentifierName(
                    SyntaxFactory.Identifier(replacementTypeName))
                .WithTriviaFrom(a.Type);

        return a.WithType(replacement);
    }

    private ITypeSymbol? GetEnumerationType(ITypeSymbol typeSymbol) {
        if (typeSymbol is INamedTypeSymbol returnTypeNamedTypeSymbol){
            if (returnTypeNamedTypeSymbol.ConstructedFrom.ToDisplayString() == "System.Collections.Generic.IEnumerable<T>")
            {
                return returnTypeNamedTypeSymbol.TypeArguments[0];
            }
        }
        INamedTypeSymbol? ienumerable = typeSymbol.AllInterfaces.FirstOrDefault( i => i.ConstructedFrom?.ToDisplayString() == "System.Collections.Generic.IEnumerable<T>");
        if (ienumerable != null) {
            return ienumerable.TypeArguments[0];
        }
       
        return null;
    }

    private SyntaxNode? ReplaceForeachSyntaxWithDeinferedType(SemanticModel model, ForEachStatementSyntax a) {
        Microsoft.CodeAnalysis.TypeInfo typeInfo = model.GetTypeInfo(a.Expression);

        string? returnTypeEnumerable = null;
        if (a.Expression is InvocationExpressionSyntax invocationExpressionSyntax) {
            if (invocationExpressionSyntax.Expression is MemberAccessExpressionSyntax memberAccessExpressionSyntax) {  
                string toLookup = memberAccessExpressionSyntax.Expression.ToFullString();
                string methodName = memberAccessExpressionSyntax.Name.ToFullString();
                ISymbol? classSymbol = model
                    .LookupSymbols(a.SpanStart, null, toLookup, true)
                    .FirstOrDefault();
                if (classSymbol != null) {
                    if (classSymbol is INamedTypeSymbol namedTypeSymbol1) {
                        ISymbol? methodSymbol = namedTypeSymbol1.GetMembers().FirstOrDefault( m => m.Name == methodName);
                        if (methodSymbol is IMethodSymbol methodSymbol1) {
                            ITypeSymbol returnTypeSymbol = methodSymbol1.ReturnType;
                            ISymbol? enumerableTypeSymbol = GetEnumerationType(returnTypeSymbol);
                            if (enumerableTypeSymbol != null)
                                returnTypeEnumerable = enumerableTypeSymbol.ToMinimalDisplayString(model, a.SpanStart);
                        } 
                    }
                }
            }
        } else if (a.Expression is IdentifierNameSyntax identifierNameSyntax)  {
            ISymbol? classSymbol = model
                    .LookupSymbols(a.SpanStart, null, identifierNameSyntax.Identifier.ToFullString(), true)
                    .FirstOrDefault();
            if (classSymbol != null && classSymbol is ILocalSymbol localSymbol) {
                ISymbol? enumerableTypeSymbol = GetEnumerationType(localSymbol.Type);
                if (enumerableTypeSymbol != null)
                    returnTypeEnumerable = enumerableTypeSymbol.ToMinimalDisplayString(model, a.SpanStart);
            }

        }
        if (returnTypeEnumerable == null)
            return a;

        TypeSyntax replacement = 
            SyntaxFactory
                .IdentifierName(
                    SyntaxFactory.Identifier( returnTypeEnumerable))
                .WithTriviaFrom(a.Type);

        return a.WithType(replacement);
    }

    private IEnumerable<VariableDeclarationSyntax> GetVariableDeclarationSyntaxUsingTypeInference(SyntaxTree tree) {
        return 
            tree
                .GetRoot()
                .DescendantNodes()
                .OfType<VariableDeclarationSyntax>()
                .Cast<VariableDeclarationSyntax>()
                .Where( variableDeclarationSyntax => variableDeclarationSyntax.Type.IsVar );
    }

    private IEnumerable<ForEachStatementSyntax> GetForEachSyntaxUsingTypeInference(SyntaxTree tree) {
        return 
            tree
                .GetRoot()
                .DescendantNodes()
                .OfType<ForEachStatementSyntax>()
                .Cast<ForEachStatementSyntax>()
                .Where( variableDeclarationSyntax => variableDeclarationSyntax.Type.IsVar );
    }
}
