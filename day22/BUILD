package(default_visibility = ["//visibility:public"])

load("@io_bazel_rules_go//go:def.bzl", "go_binary")
load("@io_bazel_rules_rust//rust:rust.bzl", "rust_binary")

go_binary(
    name = "day22",
    data = ["input.txt"],
    srcs = ["main.go"],
)

rust_binary(
  name = "day22rs",
  data = ["input.txt"],
  srcs = ["main.rs"],
)
