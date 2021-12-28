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

        TypeSyntax replacement;
        if (typeInfo.ConvertedType is IArrayTypeSymbol arrayTypeSymbol) {
            replacement = 
                SyntaxFactory
                    .IdentifierName(
                        SyntaxFactory.Identifier(arrayTypeSymbol.ToDisplayString()))
                    .WithTriviaFrom(a.Type);
        } else {
            string replacementTypeName = typeInfo.ConvertedType.ToMinimalDisplayString(model, NullableFlowState.None, a.SpanStart);
            replacement = 
                SyntaxFactory
                    .IdentifierName(
                        SyntaxFactory.Identifier(replacementTypeName))
                    .WithTriviaFrom(a.Type);
        }

        return a.WithType(replacement);
    }
    private SyntaxNode? ReplaceForeachSyntaxWithDeinferedType(SemanticModel model, ForEachStatementSyntax a) {
        Microsoft.CodeAnalysis.TypeInfo typeInfo = model.GetTypeInfo(a.Expression);
        INamedTypeSymbol? namedTypeSymbol = typeInfo.Type as INamedTypeSymbol;
        if (namedTypeSymbol == null)
            return a;

        string? convertedType = typeInfo.ConvertedType?.ToDisplayString();
        if (convertedType == null)
            return a;

        string? nameSpace = namedTypeSymbol.ContainingNamespace?.ToString();

        string typeArgsString = String.Join(",", namedTypeSymbol.TypeArguments.OfType<INamedTypeSymbol>().Cast<INamedTypeSymbol>().Select( tp => tp.ContainingNamespace + "." +tp.Name));
        string translatedTypeName = $"{nameSpace}.{typeInfo.ConvertedType?.OriginalDefinition?.Name}`{namedTypeSymbol.TypeArguments.Count()}[{typeArgsString}]";
        Type? t = Type.GetType(translatedTypeName);
        if (t == null) {
            Assembly? ass = Assembly.LoadWithPartialName(typeInfo.ConvertedType.ContainingAssembly.Name);
            t = ass?.GetType(translatedTypeName);
        }
        if (t == null) return a;

        MethodInfo? getEnumerator = t.GetMethod("GetEnumerator");
        if (getEnumerator == null) return a;

        Type? typeOfEnumerable = getEnumerator?.ReturnType.GetGenericArguments()?.FirstOrDefault();
        if (typeOfEnumerable == null)
            return a;
        
        TypeSyntax replacement = 
            SyntaxFactory
                .IdentifierName(
                    SyntaxFactory.Identifier( typeOfEnumerable.Name))
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
