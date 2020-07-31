---
layout: post
author: Timmy Jose
title: "A simple Arithmetic Expression evaluator in Zig"
date: 2020-05-29
---

It has been a while since I learnt Zig (well, at least the basics), and I thought no better time than now to refresh my memory by implementing a simple Arithmetic Expression
evaluator (as done in the previous post in Ada) in Zig.

Much like Ada, Zig continues to delight me with its excellent readability and useful verbosity, as I like to call it. 

Without further ado, here is the code (with some help from the Zig IRC channel in fixing the return types and some errors with recursion of functions returning error sets). Thanks, ifreund!


```zig
const std = @import("std");
const warn = std.debug.warn;

const Expr = union(enum) {
    Val: i32,
    Add: Payload,
    Div: Payload,
    Sub: Payload,
    Mul: Payload,
};

const Payload = struct {
    left: *const Expr,
    right: *const Expr,
};

fn show_helper(expr: *const Payload, expr_name: []const u8, stdout: *const @TypeOf(std.io.getStdOut().outStream())) anyerror!void {
    try stdout.print("{}", .{expr_name});
    try show(expr.left, stdout);
    try stdout.print(", ", .{});
    try show(expr.right, stdout);
    try stdout.print(")", .{});
}

fn show(e: *const Expr, stdout: *const @TypeOf(std.io.getStdOut().outStream())) anyerror!void {
    switch (e.*) {
        Expr.Val => |n| try stdout.print("Val {}", .{n}),
        Expr.Add => |a| try show_helper(&a, "Add (", stdout),
        Expr.Sub => |s| try show_helper(&s, "Sub (", stdout),
        Expr.Mul => |m| try show_helper(&m, "Mul (", stdout),
        Expr.Div => |d| try show_helper(&d, "Div (", stdout),
        else => unreachable,
    }
}

fn eval(e: *const Expr) i32 {
    return switch (e.*) {
        Expr.Val => |v| v,
        Expr.Add => |a| eval(a.left) + eval(a.right),
        Expr.Sub => |s| eval(s.left) - eval(s.right),
        Expr.Mul => |m| eval(m.left) * eval(m.right),
        Expr.Div => |d| return if (eval(d.right) == 0) eval(d.left) else @divTrunc(eval(d.left), eval(d.right)),
        else => unreachable,
    };
}

pub fn main() !void {
    const stdout = std.io.getStdOut().outStream();

    const e1 = Expr{ .Val = 100 };
    try show(&e1, &stdout);
    try stdout.print(" = {}\n", .{eval(&e1)});

    const e2 = Expr{ .Div = .{ .left = &Expr{ .Val = 10 }, .right = &Expr{ .Val = 2 } } };
    try show(&e2, &stdout);
    try stdout.print(" = {}\n", .{eval(&e2)});

    const e3 = Expr{
        .Div = .{
            .left = &Expr{
                .Mul = .{
                    .left = &Expr{ .Val = 5 },
                    .right = &Expr{ .Val = 4 },
                },
            },
            .right = &Expr{ .Val = 2 },
        },
    };
    try show(&e3, &stdout);
    try stdout.print(" = {}\n", .{eval(&e3)});

    const e4 = Expr{
        .Add = .{
            .left = &Expr{
                .Mul = .{
                    .left = &Expr{ .Val = 5 },
                    .right = &Expr{ .Val = 4 },
                },
            },
            .right = &Expr{
                .Sub = .{
                    .left = &Expr{ .Val = 100 },
                    .right = &Expr{
                        .Div = .{
                            .left = &Expr{ .Val = 12 },
                            .right = &Expr{ .Val = 4 },
                        },
                    },
                },
            },
        },
    };

    try show(&e4, &stdout);
    try stdout.print(" = {}\n", .{eval(&e4)});

    const e5 = Expr{ .Div = .{ .left = &Expr{ .Val = 100 }, .right = &Expr{ .Val = 0 } } };
    try show(&e5, &stdout);
    try stdout.print(" = {}\n", .{eval(&e5)});
}
```

Sample run:

```bash
~/dev/playground/zig$ zig build-exe eval.zig --release-fast && ./eval
Val 100 = 100
Div (Val 10, Val 2) = 5
Div (Mul (Val 5, Val 4), Val 2) = 10
Add (Mul (Val 5, Val 4), Sub (Val 100, Div (Val 12, Val 4))) = 117
Div (Val 100, Val 0) = 100
```

[<](2020-05-20-expr-eval-in-ada)
[Home](/index.html)
[>](2020-07-31-coin-change-ats-rust-zig)
