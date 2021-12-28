# csharp-remove-type-inference
remove type inference in c# files, projects and solutions

* uses roslyn to parse, query and replace instances of `ForEachStatementSyntax` and `VariableDeclarationSyntax` which are using type-inference, `var`
* uses msbuild to load solutions and projects
