# ProSyT: Property-based Symbolic Testing

This branch includes the sources of ProSyT

- [Installation](#installation)
- [Usage](#usage)

### NOTEs

In order to reproduce the experiments of *Property-Based Test Case Generators for Free*, run ProSyT as follows:

```shell
maper$   ./prosyt.sh ./examples/$prog prop_tbt \
    --inf -10000 --sup 10000 --min-size 10 --max-size 100 \
    --tests 100000 --force-spec --verbose --generation-timeout 300
```
where ```$prog``` is the program under test.
The benchmark suite of the paper is included in ```$maper/examples```.

### INSTALLATION

#### Requirements
* SWI Prolog: http://www.swi-prolog.org/
* Erlang: http://www.erlang.org/
* PropEr: https://proper-testing.github.io/

1. Install PropEr into the top level folder `maper`, which should look as follows:

```
maper
|-- ebin
|-- examples
|-- include
|-- Makefile
|-- proper
|-- prosyt.sh
|-- scripts
|-- src
```

2. Run `make` to build the project.

#### Notes

ProSyT has been tested using SWI Prolog 7.6.4 and Erlang/OTP 20 on Ubuntu 18.04.2 LTS

### USAGE

To run ProSyT

```shell
maper$ ./prosyt [OPTION]... SOURCE PROPERTY
```
where `SOURCE` includes the program under test and the definition of `PROPERTY`.
The ProSyT options are listed below.

```
 --tests N
     specifies the number of tests to be run (Default: 100)
 --min-size M
     specifies the minimum size of terms generated by typeof (Default: 0)
 --max-size M
     specifies the maximum size of terms generated by typeof (Default: 42)
 --inf I
     specifies the minimum value for the random number generator range (Default: -1000)
 --sup S
     specifies the maximum value for the random number generator range (Default: 1000)
 --force-spec
     adds some constraints on the output expressions of functions as specified by their contracts
 --symbolic_generation-and-filter
     executes the test cases generation process that runs the (symbolic) type generator first, followed by the filter
 --generation-timeout T
     sets a timeout T in seconds for the generation process
 --skeleton-instances I
     sets the maximum number of test cases generated from each symbolic answer of the filter expression
 --verbose
     prints statistics about timings
      - 'erl2clp': time required for translating from Erlang to CLP
      - 'tests generation': time required for generating the test cases
      - 'testing': time required for running the tests
 -h, --help
     display this help and exit
```
