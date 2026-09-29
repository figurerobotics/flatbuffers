"""Build rules for flatbuffers-generated nanobind code."""

load("@nanobind_bazel//:build_defs.bzl", "nanobind_extension")
load("@rules_python//python:defs.bzl", "py_library")
load("//:new_build_defs.bzl", "flatc_generated_files")

DEFAULT_COPTS = [
    "-std=c++17",
    "-Wno-unused-local-typedef",
]

DEFAULT_FILENAME_SUFFIX = "_generated"

def flatbuffer_nanobind_module_names(srcs, filename_suffix = None):
    """Returns the python module names generated for the given .fbs files.

    Each schema `<schema>.fbs` is generated as the module (and extension) `<schema><filename_suffix>`.

    Args:
        srcs: Source .fbs files.
        filename_suffix: The filename suffix for generated files (defaults to "_generated").

    Returns:
        The list of module names, in the same order as `srcs`.
    """
    suffix = filename_suffix if filename_suffix != None else DEFAULT_FILENAME_SUFFIX
    return [src.split(":")[-1].split("/")[-1].removesuffix(".fbs") + suffix for src in srcs]

def flatbuffer_nanobind_library(
        name,
        srcs,
        cc_deps = [],
        py_deps = [],
        flatc_data = [],
        copts = DEFAULT_COPTS,
        filename_suffix = None,
        flatc_args = None,
        flatc = None,
        nanobind_link_mode = "auto",
        **kwargs):
    """A py_library which generates and compiles flatbuffers nanobind code.

    Each schema `<schema>.fbs` in `srcs` is compiled as its own extension module
    `<schema><filename_suffix>` (see `flatbuffer_nanobind_module_names`), and `name` is a
    py_library of all of them.

    NOTE: nanobind_bazel's `nanobind_extension` creates an alias for each module, in which case
    `name` must differ from the module names.

    Args:
        name: Rule name.
        srcs: Source .fbs files.
        cc_deps: `flatbuffer_cc_library` targets which correspond to the flatbuffer files in srcs.
        py_deps: Python dependencies for generated nanobind code. This can include other
            `flatbuffer_nanobind_library` targets whose flatbuffer files are imported by `srcs`.
        flatc_data: Additional files to make visible to flatc when generating code. (e.g. files
            specified by `--cpp-include`).
        copts: C++ compiler options for compiling the nanobind extensions.
        filename_suffix: Overrides the default filename suffix ("_generated") for generated files.
        flatc_args: Overrides the arguments to pass to flatc.
        flatc: Overrides the flatc executable.
        nanobind_link_mode: Passed to `nanobind_extension` for each module ("static", "shared" or
            "auto"). Use the same linkage as other nanobind extensions in the build.
        **kwargs: Additional arguments to pass to `py_library`.
    """
    module_names = flatbuffer_nanobind_module_names(srcs, filename_suffix)
    for src, module_name in zip(srcs, module_names):
        gen_target_name = "%s_srcs" % module_name
        flatc_generated_files(
            name = gen_target_name,
            srcs = [src],
            language = "nanobind",
            deps = cc_deps + py_deps + flatc_data,
            filename_suffix = filename_suffix,
            flatc_args = flatc_args,
            flatc = flatc,
        )
        nanobind_extension(
            name = module_name,  # This creates a {module_name}.so target.
            srcs = [":%s" % gen_target_name],
            copts = copts,
            deps = [
                "@flatbuffers//nanobind",
            ] + cc_deps,
            nanobind_link_mode = nanobind_link_mode,
            testonly = kwargs.get("testonly"),
            visibility = kwargs.get("visibility"),
        )
    py_library(
        name = name,
        data = [":%s.so" % module_name for module_name in module_names],
        deps = py_deps,
        **kwargs
    )
