using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
namespace type_deinference;

public interface ITypeDeInference {
    string? RemoveTypeInference(string source);
    IDictionary<string, string> RemoveTypeInference(
        IEnumerable<string> sourceIdentifier,         
        Func<string, string> getSourceFromIdentifier);
    
    IDictionary<string, string> Process(IDictionary<SyntaxTree, CompilationUnitSyntax> trees, Compilation compilation, Func<SyntaxTree, string> fnGetKeyForSyntaxTree);
}
