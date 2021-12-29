using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Xunit;
namespace type_deinference;

public class TestTypeDeInference {

    private readonly ITypeDeInference _typeDeInference;
    
    private readonly IEnumerable<MetadataReference> defaultReferences;

    public TestTypeDeInference() {
        string assemlyLoc = typeof(Enumerable).GetTypeInfo().Assembly.Location;
        DirectoryInfo? coreDir = Directory.GetParent(assemlyLoc);
        if (coreDir == null)
            throw new ApplicationException("Unable to locate core framework directory...");

        defaultReferences = 
            new[] { 
                MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(System.Linq.Enumerable).Assembly.Location),
                MetadataReference.CreateFromFile(typeof(System.Console).Assembly.Location),
                MetadataReference.CreateFromFile(coreDir.FullName + Path.DirectorySeparatorChar + "System.Runtime.dll"),
            };
        _typeDeInference = new TypeDeInference(defaultReferences);
    }

    [Fact]
    public void TestDeInferVariableDeclarationStatement() {
        const string source = @"using System;
public class NumberWang { 
    public void Wang() {
        var x = 1;
        var z = (x == 1)? 10 : 100;
        var t = GetType();
        var t2 = new int[] {0, 1, 2, 3, 4, 5};
    }
}";
        const string expectedResult = @"using System;
public class NumberWang { 
    public void Wang() {
        int x = 1;
        int z = (x == 1)? 10 : 100;
        Type t = GetType();
        int[] t2 = new int[] {0, 1, 2, 3, 4, 5};
    }
}";

        string? deinferedSource = _typeDeInference.RemoveTypeInference(source);
        Assert.Equal(expectedResult, deinferedSource);
    }

    [Fact]
    public void TestDeInferAsyncVariableDeclarationStatement() {
        const string source = @"using System;
using System.Threading.Tasks;
public class NumberWang { 
    public async Task Wang() {
        var x = await Task.FromResult<int>(5);
        var z = Task.FromResult<int>(5);
    }
}";
        const string expectedResult = @"using System;
using System.Threading.Tasks;
public class NumberWang { 
    public async Task Wang() {
        int x = await Task.FromResult<int>(5);
        Task<int> z = Task.FromResult<int>(5);
    }
}";

        string? deinferedSource = _typeDeInference.RemoveTypeInference(source);
        Assert.Equal(expectedResult, deinferedSource);
    }


    [Fact]
    public void TestDeInferForEachStatementWithMemberAccessExpression() {
        const string source = @"using System.Linq;
public class T {
    public void M() {
        foreach(var s in Enumerable.Range(1, 10)) {
            System.Console.WriteLine(s);
        }
    }
}";
        const string expectedResult = @"using System.Linq;
public class T {
    public void M() {
        foreach(int s in Enumerable.Range(1, 10)) {
            System.Console.WriteLine(s);
        }
    }
}";

        string? deinferedSource = _typeDeInference.RemoveTypeInference(source);
        Assert.Equal(expectedResult, deinferedSource);
    }


    [Fact]
    public void TestDeInferForEachStatementWithLocalArrayExpression() {
        const string source = @"using System;
public class T {
    public void M() {
        int[] intArray = new [] {1,2,3,4};
        foreach(var s in intArray) {
            Console.WriteLine(s);
        }
    }
}";
        const string expectedResult = @"using System;
public class T {
    public void M() {
        int[] intArray = new [] {1,2,3,4};
        foreach(int s in intArray) {
            Console.WriteLine(s);
        }
    }
}";

        string? deinferedSource = _typeDeInference.RemoveTypeInference(source);
        Assert.Equal(expectedResult, deinferedSource);
    }
    
    [Fact]
    public void TestDeInferForEachStatementWithNonLocalArrayExpression() {
        const string source = @"using System;
public class T {
    private int[] intArray = new [] {1,2,3,4};
    public void M() {
        foreach(var s in intArray) {
            Console.WriteLine(s);
        }
    }
}";
        const string expectedResult = @"using System;
public class T {
    private int[] intArray = new [] {1,2,3,4};
    public void M() {
        foreach(int s in intArray) {
            Console.WriteLine(s);
        }
    }
}";

        string? deinferedSource = _typeDeInference.RemoveTypeInference(source);
        Assert.Equal(expectedResult, deinferedSource);
    }

    [Fact]
    public void TestDeinferMultipleCodeFiles() {
        const string source1 = @"using System;
public class NumberWang { 
    public void Wang() {
        var x = 1;
        var z = (x == 1)? 10 : 100;
        var t = GetType();
        var t2 = new int[] {0, 1, 2, 3, 4, 5};
    }
}"; 
        const string source2 = @"using System;
public class NumberWong { 
    public void Wong() {
        var x = 1;
        var z = (x == 1)? 10 : 100;
        var t = GetType();
        var t2 = new int[] {0, 1, 2, 3, 4, 5};
    }
}"; 

        const string expectedSource1Result = @"using System;
public class NumberWang { 
    public void Wang() {
        int x = 1;
        int z = (x == 1)? 10 : 100;
        Type t = GetType();
        int[] t2 = new int[] {0, 1, 2, 3, 4, 5};
    }
}"; 
        const string expectedSource2Result = @"using System;
public class NumberWong { 
    public void Wong() {
        int x = 1;
        int z = (x == 1)? 10 : 100;
        Type t = GetType();
        int[] t2 = new int[] {0, 1, 2, 3, 4, 5};
    }
}"; 

        Dictionary<string, string> sourceByIdentifier = new Dictionary<string, string>() {{"a",source1}, {"b", source2}};
        IDictionary<string, string> results = _typeDeInference.RemoveTypeInference(sourceByIdentifier.Keys, ident => sourceByIdentifier[ident]);
        Assert.Equal(expectedSource1Result, results["a"]);
        Assert.Equal(expectedSource2Result, results["b"]);
    }

    [Fact]
    public async Task TestDeinferProject() {
        const string projectPath = "../csharp-remove-type-inference/src/type-deinference.csproj";
        SolutionDeInference solutionDeInference = new SolutionDeInference(_typeDeInference);
        try {
            IDictionary<string, string> results = await solutionDeInference.RemoveTypeInferenceFromProject(projectPath);
            foreach(KeyValuePair<string,string> kvp in results) {
                await File.WriteAllTextAsync(kvp.Key, kvp.Value);
            }
        }
        catch (Exception exc) {
            Console.Write(exc.Message);
        }

    }

    [Fact]
    public async Task TestDeinferSolution() {
        const string solutionPath = "../../Architecture/source/Architecture.sln";
        SolutionDeInference solutionDeInference = new SolutionDeInference(_typeDeInference);
        try {
            IDictionary<string, string> results = await solutionDeInference.RemoveTypeInferenceFromSolution(solutionPath);
            foreach(KeyValuePair<string,string> kvp in results){
                await File.WriteAllTextAsync(kvp.Key, kvp.Value);
            }
        }
        catch (Exception exc) {
            Console.WriteLine(exc.Message);
        }
    }
}


