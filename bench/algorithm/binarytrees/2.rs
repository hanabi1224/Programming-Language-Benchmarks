// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by the Rust Project Developers
// contributed by TeXitoi
// contributed by Cristi Cobzarenco
// contributed by Matt Brubeck

extern crate typed_arena;
extern crate rayon;

use typed_arena::Arena;
use rayon::prelude::*;

struct Tree<'a> {
    children: Option<(&'a Tree<'a>, &'a Tree<'a>)>,
}

fn item_check(tree: &Tree) -> i32 {
    if let Some((left, right)) = tree.children {
        1 + item_check(right) + item_check(left)
    } else {
        1
    }
}

fn bottom_up_tree<'r>(arena: &'r Arena<Tree<'r>>, depth: i32)
                  -> &'r Tree<'r> {
    let mut tree = arena.alloc(Tree { children: None });
    if depth > 0 {
        let right = bottom_up_tree(arena, depth - 1);
        let left = bottom_up_tree(arena, depth - 1);
        tree.children = Some((left, right))
    }
    tree
}

fn inner(depth: i32, iterations: i32) -> String {
    let chk: i32 = (0 .. iterations).into_par_iter().map(|_| {
        let arena = Arena::new();
        let a = bottom_up_tree(&arena, depth);
        item_check(a)
    }).sum();
    format!("{}\t trees of depth {}\t check: {}", iterations, depth, chk)
}

fn main() {
    let n = std::env::args().nth(1)
        .and_then(|n| n.parse().ok())
        .unwrap_or(10);
    let min_depth = 4;
    let max_depth = if min_depth + 2 > n { min_depth + 2 } else { n };

    {
        let arena = Arena::new();
        let depth = max_depth + 1;
        let tree = bottom_up_tree(&arena, depth);
        println!("stretch tree of depth {}\t check: {}", depth, item_check(tree));
    }

    let long_lived_arena = Arena::new();
    let long_lived_tree = bottom_up_tree(&long_lived_arena, max_depth);

    let messages = (min_depth/2..max_depth/2 + 1).into_par_iter().map(|half_depth| {
            let depth = half_depth * 2;
            let iterations = 1 << ((max_depth - depth + min_depth) as u32);
            inner(depth, iterations)
        }).collect::<Vec<_>>();

    for message in messages {
        println!("{}", message);
    }

    println!("long lived tree of depth {}\t check: {}", max_depth, item_check(long_lived_tree));
}
