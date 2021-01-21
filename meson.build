# Build file for the Puppetmaster project's Bluespec design files.
project('pmhw')

# Find external programs needed.
bsc = find_program('bsc')
prog_python = import('python').find_installation('python3')

# List of modules in the project.
modules = [
    'PmIfc',
    'PmCore',
    'Shard',
    'Renamer',
    'Scheduler',
    'Puppet',
    'Puppetmaster',
    'PmTop',
]

# Modules with testbenches.
has_tests = [
    'Shard',
    'Renamer',
    'Scheduler',
    'Puppetmaster',
    'PmTop',
]

# Bluespec module dependencies. Values are arrays of indices into the `modules` array.
# This workaround is needed because Meson dictionaries are immutable.
dep_indices = {
    'PmCore': [0],
    'Shard': [0, 1],
    'Renamer': [0, 1, 2],
    'Scheduler': [0, 1],
    'Puppet': [0, 1],
    'Puppetmaster': [0, 1, 2, 3, 4, 5],
    'PmTop': [0, 1, 6],
}

# Non-Bluespec build targets.
memfile = custom_target('memfile',
    input: [
        'make_memfile.py',
        'PmCore.bsv',
        'PmIfc.bsv',
        'Shard.bsv'
    ],
    output: 'mem.vmh',
    command: [
        prog_python,
        '@INPUT0@',
        '@INPUT1@',
        '@INPUT2@',
        '@INPUT3@',
        '@OUTPUT@',
    ],
)

# Bluespec build targets.
obj_targets = []
foreach module : modules
    # Determine target names.
    target_name = module.to_lower()
    obj_target_name = target_name + '_obj'

    # Generate output file names and compiler options.
    obj_outputs = ['@0@.bo'.format(module)]
    obj_bsc_opts = [
        '-quiet',
        '-bdir', '@OUTDIR@',
        '-aggressive-conditions',
        '-sim',
    ]
    if module in has_tests
        obj_outputs += ['mk@0@Testbench.ba'.format(module)]
        obj_bsc_opts += ['-g', 'mk@0@Testbench'.format(module)]
    endif
    if get_option('debug')
        obj_bsc_opts += ['-D', 'DEBUG=1']
    endif

    # Compute dependencies.
    deps = []
    foreach dep_index : dep_indices.get(module, [])
        deps += obj_targets[dep_index]
    endforeach

    # Build target for generating Bluesim object file.
    obj_target = custom_target(obj_target_name,
        input: '@0@.bsv'.format(module),
        output: obj_outputs,
        command: [bsc] + obj_bsc_opts + ['@INPUT@'],
        depends: deps,
    )
    obj_targets += [obj_target]

    # Build target for Bluesim executable.
    if module in has_tests
        custom_target(target_name,
            input: obj_target,
            output: 'mk@0@Testbench'.format(module),
            command: [
                bsc,
                '-quiet',
                '-simdir', '@OUTDIR@',
                '-sim',
                '-e', '@OUTPUT@',
                '-o', '@OUTPUT@',
            ],
            build_by_default: true,
        )
    endif
endforeach
