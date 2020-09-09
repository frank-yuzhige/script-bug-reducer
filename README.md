# projReducer

### Build, Install and Execute
1. Install stack on your machine: 
  https://docs.haskellstack.org/en/stable/README/
2. `cd` to the project directory.
3. Run `stack install`. The name of the executable will be printed out to the console once the install is finished. You might want to add the path of the executable to your `PATH`.
4. To run the project reducer on your own project, you will need to prepare 2 bash scripts, namely `build.sh` and `itest.sh`. `build.sh` should builds your project with the buggy compiler, and `itest.sh` should fail with the buggy compiler, while succeed with another compiler is used to build the project.
5. To find out which file is the one that triggers the compiler bug, first `cd` into the project directory, then run:  
```bash
projReducer-exe -path=./build.sh -ipath=./itest.sh -cvar=$BUGGY_COMPILER -xcvar=$GOOD_COMPILER
```
The reducer will then automatically find the file which triggers the compiler bug. It would also create a copy of the original build script `build.sh.orig` (You can also designate different paths for your build/test script by changing the relevant argument).

### Arguments
##### -path=<build_script>
The path to the build script. The reducer will modify and run this file continuously during its execution.
##### -ipath=<test_script>
The path to the build script. In each iteration the reducer will run the test script after the build to determine its next step.
##### -cvar=<buggy_compiler>
The 'buggy' compiler which triggers a miscompilation when building the project.
##### -xcvar=<good_compiler>
The compiler which does not trigger the miscompilation.
##### -recover
Recover the original build script after execution. 
##### -cache
_Experimental!!_ Cache and avoid executing some of the instructions between iterations where possible.
##### -debug
Toggle DEBUG mode. Prints __a lot__ of extra information.