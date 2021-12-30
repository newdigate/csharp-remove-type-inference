# c# remove type inference
- remove type inference in c# files, projects and solutions
- uses roslyn to parse, query and replace instances of `ForEachStatementSyntax` and `VariableDeclarationSyntax` which are using type-inference, `var`
- uses msbuild to load solutions and projects

# TL/DR
  - tranforms this:
``` c#
using System;
public class T {
    public int[] intArray { get; set; } = new [] {1,2,3,4};
    public void M() {
        foreach(var s in intArray) {
            Console.WriteLine(s);
        }
        var x = 1;
        var z = (x == 1)? 10 : 100;
        var t = GetType();
        var t2 = new int[] {0, 1, 2, 3, 4, 5};
    }
}
``` 
 - to this:
``` c#
using System;
public class T {
    public int[] intArray { get; set; } = new [] {1,2,3,4};
    public void M() {
        foreach(int s in intArray) {
            Console.WriteLine(s);
        }
        int x = 1;
        int z = (x == 1)? 10 : 100;
        Type t = GetType();
        int[] t2 = new int[] {0, 1, 2, 3, 4, 5};
    }
}
```
