project('pmhw')

bsc = find_program('bsc')
prog_python = import('python').find_installation('python3')
builddir = meson.current_build_dir()

pm_ifc_obj = custom_target('pm_ifc_obj',
    input: 'PmIfc.bsv',
    output: ['PmIfc.bo'],
    command: [
        bsc,
        '-bdir', builddir,
        '-aggressive-conditions',
        '-sim',
        '@INPUT@'])

pm_core_obj = custom_target('pm_core_obj',
    input: 'PmCore.bsv',
    output: ['PmCore.bo'],
    command: [
        bsc,
        '-bdir', builddir,
        '-aggressive-conditions',
        '-sim',
        '@INPUT@'],
    depends: [pm_ifc_obj])

scheduler_obj = custom_target('scheduler_obj',
    input: 'Scheduler.bsv',
    output: ['Scheduler.bo', 'mkSchedulerTestbench.ba'],
    command: [
        bsc,
        '-bdir', builddir,
        '-aggressive-conditions',
        '-sim',
        '-g', 'mkSchedulerTestbench',
        '@INPUT@'],
    depends: [pm_core_obj, pm_ifc_obj])

scheduler = custom_target('scheduler',
    input: scheduler_obj,
    output: 'mkSchedulerTestbench',
    command: [
        bsc,
        '-bdir', builddir,
        '-simdir', builddir,
        '-sim',
        '-e', '@OUTPUT@',
        '-o', '@OUTPUT@'])

memfile = custom_target('memfile',
    input: ['make_memfile.py', 'PmCore.bsv', 'PmIfc.bsv', 'Shard.bsv'],
    output: 'mem.vmh',
    command: [
        prog_python,
        '@INPUT0@',
        '@INPUT1@',
        '@INPUT2@',
        '@INPUT3@',
        '@OUTPUT@'])

shard_obj = custom_target('shard_obj',
    input: 'Shard.bsv',
    output: ['Shard.bo', 'mkShardTestbench.ba'],
    command: [
        bsc,
        '-bdir', builddir,
        '-aggressive-conditions',
        '-sim',
        '-g', 'mkShardTestbench',
        '@INPUT@'],
    depends: [pm_core_obj, pm_ifc_obj, memfile])

shard = custom_target('shard',
    input: shard_obj,
    output: 'mkShardTestbench',
    command: [
        bsc,
        '-bdir', builddir,
        '-simdir', builddir,
        '-sim',
        '-e', '@OUTPUT@',
        '-o', '@OUTPUT@'])

renamer_obj = custom_target('renamer_obj',
    input: 'Renamer.bsv',
    output: ['Renamer.bo', 'mkRenamerTestbench.ba'],
    command: [
        bsc,
        '-bdir', builddir,
        '-aggressive-conditions',
        '-sim',
        '-g', 'mkRenamerTestbench',
        '@INPUT@'],
    depends: [pm_core_obj, pm_ifc_obj, shard_obj])

renamer = custom_target('renamer',
    input: renamer_obj,
    output: 'mkRenamerTestbench',
    command: [
        bsc,
        '-bdir', builddir,
        '-simdir', builddir,
        '-sim',
        '-e', '@OUTPUT@',
        '-o', '@OUTPUT@'])

puppets_obj = custom_target('puppets_obj',
    input: 'Puppets.bsv',
    output: 'Puppets.bo',
    command: [
        bsc,
        '-bdir', builddir,
        '-aggressive-conditions',
        '-sim',
        '@INPUT@'],
    depends: [pm_core_obj, pm_ifc_obj])

pm_obj = custom_target('pm_obj',
    input: 'Puppetmaster.bsv',
    output: ['Puppetmaster.bo', 'mkPuppetmasterTestbench.ba'],
    command: [
        bsc,
        '-bdir', builddir,
        '-aggressive-conditions',
        '-sim',
        '-g', 'mkPuppetmasterTestbench',
        '@INPUT@'],
    depends: [pm_core_obj, pm_ifc_obj, puppets_obj, renamer_obj, scheduler_obj, shard_obj])

pm = custom_target('pm',
    input: pm_obj,
    output: 'mkPuppetmasterTestbench',
    command: [
        bsc,
        '-bdir', builddir,
        '-simdir', builddir,
        '-sim',
        '-e', '@OUTPUT@',
        '-o', '@OUTPUT@'],
    build_by_default: true)

pm_top_obj = custom_target('pm_top_obj',
    input: 'PmTop.bsv',
    output: ['PmTop.bo'],
    command: [
        bsc,
        '-bdir', builddir,
        '-aggressive-conditions',
        '-sim',
        '@INPUT@'],
    depends: [pm_core_obj, pm_ifc_obj, pm_obj])
