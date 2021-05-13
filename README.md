# cgp
## Environment Setup
In order to import the extension in NetLogo, it is necessary to create `*.jar` file in the directory. The following step shows how to create this `*.jar` file:
```
$ sbt
sbt:cgp> compile
sbt:cgp> package
```
Note that warnings marked with `[warn]` is acceptable while errors are not. Resolving the error is the first step to do to generate `*.jar`.
