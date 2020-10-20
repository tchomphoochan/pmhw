project('pmhw')

bsc = find_program('bsc')
prog_python = import('python').find_installation('python3')
builddir = meson.current_build_dir()

pm_types_obj = custom_target('pm_types_obj',
    input: 'PmTypes.bsv',
    output: 'PmTypes.bo',
    command: [
        bsc,
        '-bdir', builddir,
        '-sim',
        '@INPUT@'])

sched_obj = custom_target('sched_obj',
    input: 'Scheduler.bsv',
    output: ['Scheduler.bo', 'mkSchedulerTestbench.ba'],
    command: [
        bsc,
        '-bdir', builddir,
        '-sim',
        '-g', 'mkSchedulerTestbench',
        '@INPUT@'],
    depends: pm_types_obj)

sched = custom_target('sched',
    input: sched_obj,
    output: 'mkSchedulerTestbench',
    command: [
        bsc,
        '-bdir', builddir,
        '-simdir', builddir,
        '-sim',
        '-e', '@OUTPUT@',
        '-o', '@OUTPUT@'])

memfile = custom_target('memfile',
    input: ['make_memfile.py', 'PmTypes.bsv'],
    output: 'mem.vmh',
    command: [
        prog_python,
        '@INPUT0@',
        '@INPUT1@',
        '@OUTPUT@'])

shard_obj = custom_target('shard_obj',
    input: 'Shard.bsv',
    output: ['Shard.bo', 'mkShardTestbench.ba'],
    command: [
        bsc,
        '-bdir', builddir,
        '-sim',
        '-g', 'mkShardTestbench',
        '@INPUT@'],
    depends: pm_types_obj)

shard = custom_target('shard',
    input: shard_obj,
    output: 'mkShardTestbench',
    command: [
        bsc,
        '-bdir', builddir,
        '-simdir', builddir,
        '-sim',
        '-e', '@OUTPUT@',
        '-o', '@OUTPUT@'],
    depends: memfile)
