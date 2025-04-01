Puppetmaster
============
A hardware architecture for task parallelism.

Requirements
------------
- Python 3.5 or newer
- [bsc](https://github.com/B-Lang-org/bsc)
- [meson](https://mesonbuild.com/)
- [ninja](https://ninja-build.org/)
- [connectal](http://www.connectal.org/) (for FPGA synthesis)
- GNU make (for FPGA synthesis)
- GNU compiler collection a.k.a. gcc (for FPGA synthesis)
- [bsc-contrib](https://github.com/B-Lang-org/bsc-contrib) (for FPGA synthesis)
- [verilator](https://www.veripool.org/wiki/verilator)  (for FPGA simulation)

Configuration
-------------
Copy `DefaultPmConfig.bsv` to `bsv/PmConfig.bsv` and change the values as desired.

Compiling and running tests in Bluesim
--------------------------------------
```
make test
./mkSchedulerTestbench.exe
```
(or other test benches generated)

Compiling and running FPGA design in Verilator
----------------------------------------------
For the commands below to work, two environment variables need to be set:
- `CONNECTALDIR` to point to the root of the Connectal installation directory
- `BLUESPECDIR` to point to the `lib` subdirectory in the Bluespec installation directory

```
$ make build.verilator
$ make run.verilator
```
