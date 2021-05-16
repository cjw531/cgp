# cgp
## Environment Setup
0. Refer to [NetLogo Wiki](https://github.com/NetLogo/NetLogo/wiki/Building?fbclid=IwAR1Lo5sAUh_W6tPvBhMmL0tBrdsE1Nyl3vBTHhkBoQR2jDTJd2cBUsifiU0), install dependencies first. Also, pull [`NetLogo`](https://github.com/NetLogo/NetLogo) GitHub.
1. Locate `cgp/` folder under the `NetLogo/extensions/` folder, which can be found from [`NetLogo`](https://github.com/NetLogo/NetLogo) which we just pulled from GitHub.
2. Cretae `*.jar` file in the `cgp` directory:
```
~NetLogo/extensions/cgp$ sbt
sbt:cgp> compile
sbt:cgp> package
```
Note that warnings marked with `[warn]` is acceptable while `[errors]` are not. Resolving the error is the first step to do to generate `*.jar`. After executing both `compile` and `package` command, you should be able to see `[success]`.

3. Launch NetLogo:
```
~NetLogo/$ sbt
sbt:root> netlogo/run
```
and open `cgp.nlogo` inside `cgp/` folder.
