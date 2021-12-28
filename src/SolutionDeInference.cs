using Microsoft.Build.Locator;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.MSBuild;
namespace type_deinference;

public class SolutionDeInference {
    private readonly ITypeDeInference _typeDeInference;

    public SolutionDeInference(ITypeDeInference typeDeInference)
    {
        _typeDeInference = typeDeInference;
    }

    public async Task<IDictionary<string, string>> RemoveTypeInferenceFromSolution(string solutionPath) {
        EnsureMsBuildRegistration();
        using (MSBuildWorkspace workspace = MSBuildWorkspace.Create())
        {
            workspace.WorkspaceFailed += (sender, workspaceFailedArgs) => Console.WriteLine(workspaceFailedArgs.Diagnostic.Message);
            Dictionary<string, string> result = new Dictionary<string, string>();

            Solution solution = await workspace.OpenSolutionAsync(solutionPath);
            foreach (ProjectId projectId in workspace.CurrentSolution.GetProjectDependencyGraph().GetTopologicallySortedProjects()) {
                Project? project = workspace.CurrentSolution.GetProject(projectId);
                if (project == null) continue;
                IDictionary<string, string> substitutions = await RemoveTypeInferenceFromProject(project);
                result = result.Union(substitutions).ToDictionary(k => k.Key, v => v.Value);
            }

            return result;
        }    
    }

    public async Task<IDictionary<string, string>> RemoveTypeInferenceFromProject(string projectPath) {
        EnsureMsBuildRegistration();
        using(MSBuildWorkspace workspace = MSBuildWorkspace.Create()) 
        {
            workspace.WorkspaceFailed += (sender, workspaceFailedArgs) => Console.WriteLine(workspaceFailedArgs.Diagnostic.Message);

            Project project = await workspace.OpenProjectAsync(projectPath);
            return await RemoveTypeInferenceFromProject(project);
        }
    }
    public async Task<IDictionary<string, string>> RemoveTypeInferenceFromProject(Project project) {
        Dictionary<string, string> empty = new Dictionary<string, string>();
        
        Compilation? compilation = await project.GetCompilationAsync();
               
        if (compilation == null) return empty;
 
        foreach (var d in compilation.GetDiagnostics())
        {
            Console.WriteLine(CSharpDiagnosticFormatter.Instance.Format(d));
        }
        IDictionary<SyntaxTree, CompilationUnitSyntax> roots = new Dictionary<SyntaxTree, CompilationUnitSyntax>();
        foreach (SyntaxTree tree in compilation?.SyntaxTrees) {
            CompilationUnitSyntax root = tree.GetCompilationUnitRoot();
            roots[tree] = root;
        }

        return
            _typeDeInference
                .Process(roots, compilation, tree => tree.FilePath);
    }

    private void EnsureMsBuildRegistration() {
        if (!MSBuildLocator.IsRegistered)
            MSBuildLocator.RegisterDefaults();
    }
}
