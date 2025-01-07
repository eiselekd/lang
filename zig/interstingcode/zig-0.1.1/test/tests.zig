const std = @import("std");
const debug = std.debug;
const build = std.build;
const os = std.os;
const StdIo = os.ChildProcess.StdIo;
const Term = os.ChildProcess.Term;
const Buffer = std.Buffer;
const io = std.io;
const mem = std.mem;
const fmt = std.fmt;
const ArrayList = std.ArrayList;
const builtin = @import("builtin");
const Mode = builtin.Mode;

const compare_output = @import("compare_output.zig");
const build_examples = @import("build_examples.zig");
const compile_errors = @import("compile_errors.zig");
const assemble_and_link = @import("assemble_and_link.zig");
const debug_safety = @import("debug_safety.zig");
const parsec = @import("parsec.zig");

const TestTarget = struct {
    os: builtin.Os,
    arch: builtin.Arch,
    environ: builtin.Environ,
};

const test_targets = []TestTarget {
    TestTarget {
        .os = builtin.Os.linux,
        .arch = builtin.Arch.x86_64,
        .environ = builtin.Environ.gnu,
    },
    TestTarget {
        .os = builtin.Os.darwin,
        .arch = builtin.Arch.x86_64,
        .environ = builtin.Environ.unknown,
    },
    TestTarget {
        .os = builtin.Os.windows,
        .arch = builtin.Arch.x86_64,
        .environ = builtin.Environ.msvc,
    },
    TestTarget {
        .os = builtin.Os.windows,
        .arch = builtin.Arch.i386,
        .environ = builtin.Environ.msvc,
    },
};

error TestFailed;

pub fn addCompareOutputTests(b: &build.Builder, test_filter: ?[]const u8) -> &build.Step {
    const cases = %%b.allocator.create(CompareOutputContext);
    *cases = CompareOutputContext {
        .b = b,
        .step = b.step("test-compare-output", "Run the compare output tests"),
        .test_index = 0,
        .test_filter = test_filter,
    };

    compare_output.addCases(cases);

    return cases.step;
}

pub fn addDebugSafetyTests(b: &build.Builder, test_filter: ?[]const u8) -> &build.Step {
    const cases = %%b.allocator.create(CompareOutputContext);
    *cases = CompareOutputContext {
        .b = b,
        .step = b.step("test-debug-safety", "Run the debug safety tests"),
        .test_index = 0,
        .test_filter = test_filter,
    };

    debug_safety.addCases(cases);

    return cases.step;
}

pub fn addCompileErrorTests(b: &build.Builder, test_filter: ?[]const u8) -> &build.Step {
    const cases = %%b.allocator.create(CompileErrorContext);
    *cases = CompileErrorContext {
        .b = b,
        .step = b.step("test-compile-errors", "Run the compile error tests"),
        .test_index = 0,
        .test_filter = test_filter,
    };

    compile_errors.addCases(cases);

    return cases.step;
}

pub fn addBuildExampleTests(b: &build.Builder, test_filter: ?[]const u8) -> &build.Step {
    const cases = %%b.allocator.create(BuildExamplesContext);
    *cases = BuildExamplesContext {
        .b = b,
        .step = b.step("test-build-examples", "Build the examples"),
        .test_index = 0,
        .test_filter = test_filter,
    };

    build_examples.addCases(cases);

    return cases.step;
}

pub fn addAssembleAndLinkTests(b: &build.Builder, test_filter: ?[]const u8) -> &build.Step {
    const cases = %%b.allocator.create(CompareOutputContext);
    *cases = CompareOutputContext {
        .b = b,
        .step = b.step("test-asm-link", "Run the assemble and link tests"),
        .test_index = 0,
        .test_filter = test_filter,
    };

    assemble_and_link.addCases(cases);

    return cases.step;
}

pub fn addParseCTests(b: &build.Builder, test_filter: ?[]const u8) -> &build.Step {
    const cases = %%b.allocator.create(ParseCContext);
    *cases = ParseCContext {
        .b = b,
        .step = b.step("test-parsec", "Run the C header file parsing tests"),
        .test_index = 0,
        .test_filter = test_filter,
    };

    parsec.addCases(cases);

    return cases.step;
}

pub fn addPkgTests(b: &build.Builder, test_filter: ?[]const u8, root_src: []const u8,
    name:[] const u8, desc: []const u8, with_lldb: bool) -> &build.Step
{
    const step = b.step(b.fmt("test-{}", name), desc);
    for (test_targets) |test_target| {
        const is_native = (test_target.os == builtin.os and test_target.arch == builtin.arch);
        for ([]Mode{Mode.Debug, Mode.ReleaseSafe, Mode.ReleaseFast}) |mode| {
            for ([]bool{false, true}) |link_libc| {
                if (link_libc and !is_native) {
                    // don't assume we have a cross-compiling libc set up
                    continue;
                }
                const these_tests = b.addTest(root_src);
                these_tests.setNamePrefix(b.fmt("{}-{}-{}-{}-{} ", name, @enumTagName(test_target.os),
                    @enumTagName(test_target.arch), @enumTagName(mode), if (link_libc) "c" else "bare"));
                these_tests.setFilter(test_filter);
                these_tests.setBuildMode(mode);
                if (!is_native) {
                    these_tests.setTarget(test_target.arch, test_target.os, test_target.environ);
                }
                if (link_libc) {
                    these_tests.linkSystemLibrary("c");
                }
                if (with_lldb) {
                    these_tests.setExecCmd([]?[]const u8{
                        "lldb", null, "-o", "run", "-o", "bt", "-o", "exit"});
                }
                step.dependOn(&these_tests.step);
            }
        }
    }
    return step;
}

pub const CompareOutputContext = struct {
    b: &build.Builder,
    step: &build.Step,
    test_index: usize,
    test_filter: ?[]const u8,

    const Special = enum {
        None,
        Asm,
        DebugSafety,
    };

    const TestCase = struct {
        name: []const u8,
        sources: ArrayList(SourceFile),
        expected_output: []const u8,
        link_libc: bool,
        special: Special,

        const SourceFile = struct {
            filename: []const u8,
            source: []const u8,
        };

        pub fn addSourceFile(self: &TestCase, filename: []const u8, source: []const u8) {
            %%self.sources.append(SourceFile {
                .filename = filename,
                .source = source,
            });
        }
    };

    const RunCompareOutputStep = struct {
        step: build.Step,
        context: &CompareOutputContext,
        exe_path: []const u8,
        name: []const u8,
        expected_output: []const u8,
        test_index: usize,

        pub fn create(context: &CompareOutputContext, exe_path: []const u8,
            name: []const u8, expected_output: []const u8) -> &RunCompareOutputStep
        {
            const allocator = context.b.allocator;
            const ptr = %%allocator.create(RunCompareOutputStep);
            *ptr = RunCompareOutputStep {
                .context = context,
                .exe_path = exe_path,
                .name = name,
                .expected_output = expected_output,
                .test_index = context.test_index,
                .step = build.Step.init("RunCompareOutput", allocator, make),
            };
            context.test_index += 1;
            return ptr;
        }

        fn make(step: &build.Step) -> %void {
            const self = @fieldParentPtr(RunCompareOutputStep, "step", step);
            const b = self.context.b;

            const full_exe_path = b.pathFromRoot(self.exe_path);

            %%io.stderr.printf("Test {}/{} {}...", self.test_index+1, self.context.test_index, self.name);

            const child = %%os.ChildProcess.init([][]u8{full_exe_path}, b.allocator);
            defer child.deinit();

            child.stdin_behavior = StdIo.Ignore;
            child.stdout_behavior = StdIo.Pipe;
            child.stderr_behavior = StdIo.Pipe;
            child.env_map = &b.env_map;

            child.spawn() %% |err| debug.panic("Unable to spawn {}: {}\n", full_exe_path, @errorName(err));

            var stdout = Buffer.initNull(b.allocator);
            var stderr = Buffer.initNull(b.allocator);

            %%(??child.stdout).readAll(&stdout);
            %%(??child.stderr).readAll(&stderr);

            const term = child.wait() %% |err| {
                debug.panic("Unable to spawn {}: {}\n", full_exe_path, @errorName(err));
            };
            switch (term) {
                Term.Exited => |code| {
                    if (code != 0) {
                        %%io.stderr.printf("Process {} exited with error code {}\n", full_exe_path, code);
                        return error.TestFailed;
                    }
                },
                else => {
                    %%io.stderr.printf("Process {} terminated unexpectedly\n", full_exe_path);
                    return error.TestFailed;
                },
            };


            if (!mem.eql(u8, self.expected_output, stdout.toSliceConst())) {
                %%io.stderr.printf(
                    \\
                    \\========= Expected this output: =========
                    \\{}
                    \\================================================
                    \\{}
                    \\
                , self.expected_output, stdout.toSliceConst());
                return error.TestFailed;
            }
            %%io.stderr.printf("OK\n");
        }
    };

    const DebugSafetyRunStep = struct {
        step: build.Step,
        context: &CompareOutputContext,
        exe_path: []const u8,
        name: []const u8,
        test_index: usize,

        pub fn create(context: &CompareOutputContext, exe_path: []const u8,
            name: []const u8) -> &DebugSafetyRunStep
        {
            const allocator = context.b.allocator;
            const ptr = %%allocator.create(DebugSafetyRunStep);
            *ptr = DebugSafetyRunStep {
                .context = context,
                .exe_path = exe_path,
                .name = name,
                .test_index = context.test_index,
                .step = build.Step.init("DebugSafetyRun", allocator, make),
            };
            context.test_index += 1;
            return ptr;
        }

        fn make(step: &build.Step) -> %void {
            const self = @fieldParentPtr(DebugSafetyRunStep, "step", step);
            const b = self.context.b;

            const full_exe_path = b.pathFromRoot(self.exe_path);

            %%io.stderr.printf("Test {}/{} {}...", self.test_index+1, self.context.test_index, self.name);

            const child = %%os.ChildProcess.init([][]u8{full_exe_path}, b.allocator);
            defer child.deinit();

            child.env_map = &b.env_map;
            child.stdin_behavior = StdIo.Ignore;
            child.stdout_behavior = StdIo.Ignore;
            child.stderr_behavior = StdIo.Ignore;

            const term = child.spawnAndWait() %% |err| {
                debug.panic("Unable to spawn {}: {}\n", full_exe_path, @errorName(err));
            };

            const expected_exit_code: i32 = 126;
            switch (term) {
                Term.Exited => |code| {
                    if (code != expected_exit_code) {
                        %%io.stderr.printf("\nProgram expected to exit with code {} " ++
                            "but exited with code {}\n", expected_exit_code, code);
                        return error.TestFailed;
                    }
                },
                Term.Signal => |sig| {
                    %%io.stderr.printf("\nProgram expected to exit with code {} " ++
                        "but instead signaled {}\n", expected_exit_code, sig);
                    return error.TestFailed;
                },
                else => {
                    %%io.stderr.printf("\nProgram expected to exit with code {}" ++
                        " but exited in an unexpected way\n", expected_exit_code);
                    return error.TestFailed;
                },
            }

            %%io.stderr.printf("OK\n");
        }
    };

    pub fn createExtra(self: &CompareOutputContext, name: []const u8, source: []const u8,
        expected_output: []const u8, special: Special) -> TestCase
    {
        var tc = TestCase {
            .name = name,
            .sources = ArrayList(TestCase.SourceFile).init(self.b.allocator),
            .expected_output = expected_output,
            .link_libc = false,
            .special = special,
        };
        const root_src_name = if (special == Special.Asm) "source.s" else "source.zig";
        tc.addSourceFile(root_src_name, source);
        return tc;
    }

    pub fn create(self: &CompareOutputContext, name: []const u8, source: []const u8,
        expected_output: []const u8) -> TestCase
    {
        return createExtra(self, name, source, expected_output, Special.None);
    }

    pub fn addC(self: &CompareOutputContext, name: []const u8, source: []const u8, expected_output: []const u8) {
        var tc = self.create(name, source, expected_output);
        tc.link_libc = true;
        self.addCase(tc);
    }

    pub fn add(self: &CompareOutputContext, name: []const u8, source: []const u8, expected_output: []const u8) {
        const tc = self.create(name, source, expected_output);
        self.addCase(tc);
    }

    pub fn addAsm(self: &CompareOutputContext, name: []const u8, source: []const u8, expected_output: []const u8) {
        const tc = self.createExtra(name, source, expected_output, Special.Asm);
        self.addCase(tc);
    }

    pub fn addDebugSafety(self: &CompareOutputContext, name: []const u8, source: []const u8) {
        const tc = self.createExtra(name, source, undefined, Special.DebugSafety);
        self.addCase(tc);
    }

    pub fn addCase(self: &CompareOutputContext, case: &const TestCase) {
        const b = self.b;

        const root_src = %%os.path.join(b.allocator, b.cache_root, case.sources.items[0].filename);

        switch (case.special) {
            Special.Asm => {
                const annotated_case_name = %%fmt.allocPrint(self.b.allocator, "assemble-and-link {}", case.name);
                if (self.test_filter) |filter| {
                    if (mem.indexOf(u8, annotated_case_name, filter) == null)
                        return;
                }

                const exe = b.addExecutable("test", null);
                exe.addAssemblyFile(root_src);

                for (case.sources.toSliceConst()) |src_file| {
                    const expanded_src_path = %%os.path.join(b.allocator, b.cache_root, src_file.filename);
                    const write_src = b.addWriteFile(expanded_src_path, src_file.source);
                    exe.step.dependOn(&write_src.step);
                }

                const run_and_cmp_output = RunCompareOutputStep.create(self, exe.getOutputPath(), annotated_case_name,
                    case.expected_output);
                run_and_cmp_output.step.dependOn(&exe.step);

                self.step.dependOn(&run_and_cmp_output.step);
            },
            Special.None => {
                for ([]Mode{Mode.Debug, Mode.ReleaseSafe, Mode.ReleaseFast}) |mode| {
                    const annotated_case_name = %%fmt.allocPrint(self.b.allocator, "{} {} ({})",
                        "compare-output", case.name, @enumTagName(mode));
                    if (self.test_filter) |filter| {
                        if (mem.indexOf(u8, annotated_case_name, filter) == null)
                            continue;
                    }

                    const exe = b.addExecutable("test", root_src);
                    exe.setBuildMode(mode);
                    if (case.link_libc) {
                        exe.linkSystemLibrary("c");
                    }

                    for (case.sources.toSliceConst()) |src_file| {
                        const expanded_src_path = %%os.path.join(b.allocator, b.cache_root, src_file.filename);
                        const write_src = b.addWriteFile(expanded_src_path, src_file.source);
                        exe.step.dependOn(&write_src.step);
                    }

                    const run_and_cmp_output = RunCompareOutputStep.create(self, exe.getOutputPath(),
                        annotated_case_name, case.expected_output);
                    run_and_cmp_output.step.dependOn(&exe.step);

                    self.step.dependOn(&run_and_cmp_output.step);
                }
            },
            Special.DebugSafety => {
                const annotated_case_name = %%fmt.allocPrint(self.b.allocator, "safety {}", case.name);
                if (self.test_filter) |filter| {
                    if (mem.indexOf(u8, annotated_case_name, filter) == null)
                        return;
                }

                const exe = b.addExecutable("test", root_src);
                if (case.link_libc) {
                    exe.linkSystemLibrary("c");
                }

                for (case.sources.toSliceConst()) |src_file| {
                    const expanded_src_path = %%os.path.join(b.allocator, b.cache_root, src_file.filename);
                    const write_src = b.addWriteFile(expanded_src_path, src_file.source);
                    exe.step.dependOn(&write_src.step);
                }

                const run_and_cmp_output = DebugSafetyRunStep.create(self, exe.getOutputPath(), annotated_case_name);
                run_and_cmp_output.step.dependOn(&exe.step);

                self.step.dependOn(&run_and_cmp_output.step);
            },
        }
    }
};

pub const CompileErrorContext = struct {
    b: &build.Builder,
    step: &build.Step,
    test_index: usize,
    test_filter: ?[]const u8,

    const TestCase = struct {
        name: []const u8,
        sources: ArrayList(SourceFile),
        expected_errors: ArrayList([]const u8),
        link_libc: bool,
        is_exe: bool,

        const SourceFile = struct {
            filename: []const u8,
            source: []const u8,
        };

        pub fn addSourceFile(self: &TestCase, filename: []const u8, source: []const u8) {
            %%self.sources.append(SourceFile {
                .filename = filename,
                .source = source,
            });
        }

        pub fn addExpectedError(self: &TestCase, text: []const u8) {
            %%self.expected_errors.append(text);
        }
    };

    const CompileCmpOutputStep = struct {
        step: build.Step,
        context: &CompileErrorContext,
        name: []const u8,
        test_index: usize,
        case: &const TestCase,
        build_mode: Mode,

        pub fn create(context: &CompileErrorContext, name: []const u8,
            case: &const TestCase, build_mode: Mode) -> &CompileCmpOutputStep
        {
            const allocator = context.b.allocator;
            const ptr = %%allocator.create(CompileCmpOutputStep);
            *ptr = CompileCmpOutputStep {
                .step = build.Step.init("CompileCmpOutput", allocator, make),
                .context = context,
                .name = name,
                .test_index = context.test_index,
                .case = case,
                .build_mode = build_mode,
            };
            context.test_index += 1;
            return ptr;
        }

        fn make(step: &build.Step) -> %void {
            const self = @fieldParentPtr(CompileCmpOutputStep, "step", step);
            const b = self.context.b;

            const root_src = %%os.path.join(b.allocator, b.cache_root, self.case.sources.items[0].filename);
            const obj_path = %%os.path.join(b.allocator, b.cache_root, "test.o");

            var zig_args = ArrayList([]const u8).init(b.allocator);
            %%zig_args.append(b.zig_exe);

            %%zig_args.append(if (self.case.is_exe) "build-exe" else "build-obj");
            %%zig_args.append(b.pathFromRoot(root_src));

            %%zig_args.append("--name");
            %%zig_args.append("test");

            %%zig_args.append("--output");
            %%zig_args.append(b.pathFromRoot(obj_path));

            switch (self.build_mode) {
                Mode.Debug => {},
                Mode.ReleaseSafe => %%zig_args.append("--release-safe"),
                Mode.ReleaseFast => %%zig_args.append("--release-fast"),
            }

            %%io.stderr.printf("Test {}/{} {}...", self.test_index+1, self.context.test_index, self.name);

            if (b.verbose) {
                printInvocation(zig_args.toSliceConst());
            }

            const child = %%os.ChildProcess.init(zig_args.toSliceConst(), b.allocator);
            defer child.deinit();

            child.env_map = &b.env_map;
            child.stdin_behavior = StdIo.Ignore;
            child.stdout_behavior = StdIo.Pipe;
            child.stderr_behavior = StdIo.Pipe;

            child.spawn() %% |err| debug.panic("Unable to spawn {}: {}\n", zig_args.items[0], @errorName(err));

            var stdout_buf = Buffer.initNull(b.allocator);
            var stderr_buf = Buffer.initNull(b.allocator);

            %%(??child.stdout).readAll(&stdout_buf);
            %%(??child.stderr).readAll(&stderr_buf);

            const term = child.wait() %% |err| {
                debug.panic("Unable to spawn {}: {}\n", zig_args.items[0], @errorName(err));
            };
            switch (term) {
                Term.Exited => |code| {
                    if (code == 0) {
                        %%io.stderr.printf("Compilation incorrectly succeeded\n");
                        return error.TestFailed;
                    }
                },
                else => {
                    %%io.stderr.printf("Process {} terminated unexpectedly\n", b.zig_exe);
                    return error.TestFailed;
                },
            };


            const stdout = stdout_buf.toSliceConst();
            const stderr = stderr_buf.toSliceConst();

            if (stdout.len != 0) {
                %%io.stderr.printf(
                    \\
                    \\Expected empty stdout, instead found:
                    \\================================================
                    \\{}
                    \\================================================
                    \\
                , stdout);
                return error.TestFailed;
            }

            for (self.case.expected_errors.toSliceConst()) |expected_error| {
                if (mem.indexOf(u8, stderr, expected_error) == null) {
                    %%io.stderr.printf(
                        \\
                        \\========= Expected this compile error: =========
                        \\{}
                        \\================================================
                        \\{}
                        \\
                    , expected_error, stderr);
                    return error.TestFailed;
                }
            }
            %%io.stderr.printf("OK\n");
        }
    };

    fn printInvocation(args: []const []const u8) {
        for (args) |arg| {
            %%io.stderr.printf("{} ", arg);
        }
        %%io.stderr.printf("\n");
    }

    pub fn create(self: &CompileErrorContext, name: []const u8, source: []const u8,
        expected_lines: ...) -> &TestCase
    {
        const tc = %%self.b.allocator.create(TestCase);
        *tc = TestCase {
            .name = name,
            .sources = ArrayList(TestCase.SourceFile).init(self.b.allocator),
            .expected_errors = ArrayList([]const u8).init(self.b.allocator),
            .link_libc = false,
            .is_exe = false,
        };
        tc.addSourceFile(".tmp_source.zig", source);
        comptime var arg_i = 0;
        inline while (arg_i < expected_lines.len) : (arg_i += 1) {
            tc.addExpectedError(expected_lines[arg_i]);
        }
        return tc;
    }

    pub fn addC(self: &CompileErrorContext, name: []const u8, source: []const u8, expected_lines: ...) {
        var tc = self.create(name, source, expected_lines);
        tc.link_libc = true;
        self.addCase(tc);
    }

    pub fn addExe(self: &CompileErrorContext, name: []const u8, source: []const u8, expected_lines: ...) {
        var tc = self.create(name, source, expected_lines);
        tc.is_exe = true;
        self.addCase(tc);
    }

    pub fn add(self: &CompileErrorContext, name: []const u8, source: []const u8, expected_lines: ...) {
        const tc = self.create(name, source, expected_lines);
        self.addCase(tc);
    }

    pub fn addCase(self: &CompileErrorContext, case: &const TestCase) {
        const b = self.b;

        for ([]Mode{Mode.Debug, Mode.ReleaseSafe, Mode.ReleaseFast}) |mode| {
            const annotated_case_name = %%fmt.allocPrint(self.b.allocator, "compile-error {} ({})",
                case.name, @enumTagName(mode));
            if (self.test_filter) |filter| {
                if (mem.indexOf(u8, annotated_case_name, filter) == null)
                    continue;
            }

            const compile_and_cmp_errors = CompileCmpOutputStep.create(self, annotated_case_name, case, mode);
            self.step.dependOn(&compile_and_cmp_errors.step);

            for (case.sources.toSliceConst()) |src_file| {
                const expanded_src_path = %%os.path.join(b.allocator, b.cache_root, src_file.filename);
                const write_src = b.addWriteFile(expanded_src_path, src_file.source);
                compile_and_cmp_errors.step.dependOn(&write_src.step);
            }
        }
    }
};

pub const BuildExamplesContext = struct {
    b: &build.Builder,
    step: &build.Step,
    test_index: usize,
    test_filter: ?[]const u8,

    pub fn addC(self: &BuildExamplesContext, root_src: []const u8) {
        self.addAllArgs(root_src, true);
    }

    pub fn add(self: &BuildExamplesContext, root_src: []const u8) {
        self.addAllArgs(root_src, false);
    }

    pub fn addBuildFile(self: &BuildExamplesContext, build_file: []const u8) {
        const b = self.b;

        const annotated_case_name = b.fmt("build {} (Debug)", build_file);
        if (self.test_filter) |filter| {
            if (mem.indexOf(u8, annotated_case_name, filter) == null)
                return;
        }

        var zig_args = ArrayList([]const u8).init(b.allocator);
        %%zig_args.append(b.zig_exe);
        %%zig_args.append("build");

        %%zig_args.append("--build-file");
        %%zig_args.append(b.pathFromRoot(build_file));

        %%zig_args.append("test");

        if (b.verbose) {
            %%zig_args.append("--verbose");
        }

        const run_cmd = b.addCommand(null, b.env_map, zig_args.toSliceConst());

        const log_step = b.addLog("PASS {}\n", annotated_case_name);
        log_step.step.dependOn(&run_cmd.step);

        self.step.dependOn(&log_step.step);
    }

    pub fn addAllArgs(self: &BuildExamplesContext, root_src: []const u8, link_libc: bool) {
        const b = self.b;

        for ([]Mode{Mode.Debug, Mode.ReleaseSafe, Mode.ReleaseFast}) |mode| {
            const annotated_case_name = %%fmt.allocPrint(self.b.allocator, "build {} ({})",
                root_src, @enumTagName(mode));
            if (self.test_filter) |filter| {
                if (mem.indexOf(u8, annotated_case_name, filter) == null)
                    continue;
            }

            const exe = b.addExecutable("test", root_src);
            exe.setBuildMode(mode);
            if (link_libc) {
                exe.linkSystemLibrary("c");
            }

            const log_step = b.addLog("PASS {}\n", annotated_case_name);
            log_step.step.dependOn(&exe.step);

            self.step.dependOn(&log_step.step);
        }
    }
};

pub const ParseCContext = struct {
    b: &build.Builder,
    step: &build.Step,
    test_index: usize,
    test_filter: ?[]const u8,

    const TestCase = struct {
        name: []const u8,
        sources: ArrayList(SourceFile),
        expected_lines: ArrayList([]const u8),
        allow_warnings: bool,

        const SourceFile = struct {
            filename: []const u8,
            source: []const u8,
        };

        pub fn addSourceFile(self: &TestCase, filename: []const u8, source: []const u8) {
            %%self.sources.append(SourceFile {
                .filename = filename,
                .source = source,
            });
        }

        pub fn addExpectedLine(self: &TestCase, text: []const u8) {
            %%self.expected_lines.append(text);
        }
    };

    const ParseCCmpOutputStep = struct {
        step: build.Step,
        context: &ParseCContext,
        name: []const u8,
        test_index: usize,
        case: &const TestCase,

        pub fn create(context: &ParseCContext, name: []const u8, case: &const TestCase) -> &ParseCCmpOutputStep {
            const allocator = context.b.allocator;
            const ptr = %%allocator.create(ParseCCmpOutputStep);
            *ptr = ParseCCmpOutputStep {
                .step = build.Step.init("ParseCCmpOutput", allocator, make),
                .context = context,
                .name = name,
                .test_index = context.test_index,
                .case = case,
            };
            context.test_index += 1;
            return ptr;
        }

        fn make(step: &build.Step) -> %void {
            const self = @fieldParentPtr(ParseCCmpOutputStep, "step", step);
            const b = self.context.b;

            const root_src = %%os.path.join(b.allocator, b.cache_root, self.case.sources.items[0].filename);

            var zig_args = ArrayList([]const u8).init(b.allocator);
            %%zig_args.append(b.zig_exe);

            %%zig_args.append("parsec");
            %%zig_args.append(b.pathFromRoot(root_src));

            %%io.stderr.printf("Test {}/{} {}...", self.test_index+1, self.context.test_index, self.name);

            if (b.verbose) {
                printInvocation(zig_args.toSliceConst());
            }

            const child = %%os.ChildProcess.init(zig_args.toSliceConst(), b.allocator);
            defer child.deinit();

            child.env_map = &b.env_map;
            child.stdin_behavior = StdIo.Ignore;
            child.stdout_behavior = StdIo.Pipe;
            child.stderr_behavior = StdIo.Pipe;

            child.spawn() %% |err| debug.panic("Unable to spawn {}: {}\n", zig_args.toSliceConst()[0], @errorName(err));

            var stdout_buf = Buffer.initNull(b.allocator);
            var stderr_buf = Buffer.initNull(b.allocator);

            %%(??child.stdout).readAll(&stdout_buf);
            %%(??child.stderr).readAll(&stderr_buf);

            const term = child.wait() %% |err| {
                debug.panic("Unable to spawn {}: {}\n", zig_args.toSliceConst()[0], @errorName(err));
            };
            switch (term) {
                Term.Exited => |code| {
                    if (code != 0) {
                        %%io.stderr.printf("Compilation failed with exit code {}\n", code);
                        return error.TestFailed;
                    }
                },
                Term.Signal => |code| {
                    %%io.stderr.printf("Compilation failed with signal {}\n", code);
                    return error.TestFailed;
                },
                else => {
                    %%io.stderr.printf("Compilation terminated unexpectedly\n");
                    return error.TestFailed;
                },
            };

            const stdout = stdout_buf.toSliceConst();
            const stderr = stderr_buf.toSliceConst();

            if (stderr.len != 0 and !self.case.allow_warnings) {
                %%io.stderr.printf(
                    \\====== parsec emitted warnings: ============
                    \\{}
                    \\============================================
                    \\
                , stderr);
                return error.TestFailed;
            }

            for (self.case.expected_lines.toSliceConst()) |expected_line| {
                if (mem.indexOf(u8, stdout, expected_line) == null) {
                    %%io.stderr.printf(
                        \\
                        \\========= Expected this output: ================
                        \\{}
                        \\================================================
                        \\{}
                        \\
                    , expected_line, stdout);
                    return error.TestFailed;
                }
            }
            %%io.stderr.printf("OK\n");
        }
    };

    fn printInvocation(args: []const []const u8) {
        for (args) |arg| {
            %%io.stderr.printf("{} ", arg);
        }
        %%io.stderr.printf("\n");
    }

    pub fn create(self: &ParseCContext, allow_warnings: bool, filename: []const u8, name: []const u8,
        source: []const u8, expected_lines: ...) -> &TestCase
    {
        const tc = %%self.b.allocator.create(TestCase);
        *tc = TestCase {
            .name = name,
            .sources = ArrayList(TestCase.SourceFile).init(self.b.allocator),
            .expected_lines = ArrayList([]const u8).init(self.b.allocator),
            .allow_warnings = allow_warnings,
        };
        tc.addSourceFile(filename, source);
        comptime var arg_i = 0;
        inline while (arg_i < expected_lines.len) : (arg_i += 1) {
            tc.addExpectedLine(expected_lines[arg_i]);
        }
        return tc;
    }

    pub fn add(self: &ParseCContext, name: []const u8, source: []const u8, expected_lines: ...) {
        const tc = self.create(false, "source.h", name, source, expected_lines);
        self.addCase(tc);
    }

    pub fn addC(self: &ParseCContext, name: []const u8, source: []const u8, expected_lines: ...) {
        const tc = self.create(false, "source.c", name, source, expected_lines);
        self.addCase(tc);
    }

    pub fn addAllowWarnings(self: &ParseCContext, name: []const u8, source: []const u8, expected_lines: ...) {
        const tc = self.create(true, "source.h", name, source, expected_lines);
        self.addCase(tc);
    }

    pub fn addCase(self: &ParseCContext, case: &const TestCase) {
        const b = self.b;

        const annotated_case_name = %%fmt.allocPrint(self.b.allocator, "parsec {}", case.name);
        if (self.test_filter) |filter| {
            if (mem.indexOf(u8, annotated_case_name, filter) == null)
                return;
        }

        const parsec_and_cmp = ParseCCmpOutputStep.create(self, annotated_case_name, case);
        self.step.dependOn(&parsec_and_cmp.step);

        for (case.sources.toSliceConst()) |src_file| {
            const expanded_src_path = %%os.path.join(b.allocator, b.cache_root, src_file.filename);
            const write_src = b.addWriteFile(expanded_src_path, src_file.source);
            parsec_and_cmp.step.dependOn(&write_src.step);
        }
    }
};
