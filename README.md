To compile and run Puppetmaster in Bluesim:
- install Python 3.5 or newer
- install [meson](https://mesonbuild.com/)
- install [ninja](https://ninja-build.org/)
- install [bsc](https://github.com/B-Lang-org/bsc)
- `meson setup build` (you can replace `build` with anything, adjust commands accordingly)
- `cd build && meson compile`
  or `meson compile -C build`
  or `ninja -C build`
- (inside `build`) `./mkPuppetmasterTestbench`

To compile and run Connectal wrapper (in Verilator):
- install [bsc-contrib](https://github.com/B-Lang-org/bsc-contrib)
- install [verilator](https://www.veripool.org/wiki/verilator)
- set `BLUESPECDIR` to point to the root of the Bluespec installation directory
- (optional) set `PROJECTDIR` and use that as the build directory instead of `verilator`
- `make gen.verilator && cd verilator && make`
  or `make gen.verilator && make -C verilator`
  or `make build.verilator`
- copy `mem.vmh` from `build` to `verilator`
- `make run.verilator`
  or `make -C verilator run`
  or (inside `verilator`) `make run`
