package(default_visibility = ["//visibility:public"])
load("@npm//@bazel/typescript:index.bzl", "ts_project")
load("@build_bazel_rules_nodejs//:index.bzl", "nodejs_binary")

ts_project(
    name = "lib",
    srcs = glob(["**/*.ts"]),
    tsconfig = {
        "compilerOptions": {
            "downlevelIteration": True,
        }
    },
    deps = [
        "@npm//@types/node"
    ],
)

nodejs_binary(
    name = "day21",
    data = [
        ":lib",
        "input.txt",
    ],
    entry_point = ":main.ts",
)
