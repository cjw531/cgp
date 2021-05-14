# cgp
## Environment Setup
0. Pull `NetLogo` github.
1. Locate `cgp/` folder under the `NetLogo/extensions/` folder.
2. Cretae `*.jar` file in the `cgp` directory:
```
~NetLogo/extensions/cgp$ sbt
sbt:cgp> compile
sbt:cgp> package
```
Note that warnings marked with `[warn]` is acceptable while errors are not. Resolving the error is the first step to do to generate `*.jar`.
3. Launch NetLogo:
```
~NetLogo/$ sbt
sbt:root> netlogo/run
```
and open `cgp.nlogo` inside `cgp/` folder.
