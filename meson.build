project('pmhw')

bsc = find_program('bsc')
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
    output: 'sched.bin',
    command: [
        bsc,
        '-bdir', builddir,
        '-simdir', builddir,
        '-sim',
        '-e', 'mkSchedulerTestbench',
        '-o', '@OUTPUT@'])

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
    output: 'shard.bin',
    command: [
        bsc,
        '-bdir', builddir,
        '-simdir', builddir,
        '-sim',
        '-e', 'mkShardTestbench',
        '-o', '@OUTPUT@'])