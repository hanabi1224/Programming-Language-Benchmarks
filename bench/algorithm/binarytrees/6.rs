use std::cmp::max;

#[global_allocator]
static ALLOC: mimalloc::MiMalloc = mimalloc::MiMalloc;

struct TreeNode {
    l: Option<Box<TreeNode>>,
    r: Option<Box<TreeNode>>,
}

impl TreeNode {
    fn check(self) -> i32 {
        let mut ret = 1;
        if let Some(l) = self.l {
            ret += l.check();
        }
        if let Some(r) = self.r {
            ret += r.check();
        }
        ret
    }

    fn create(depth: i32) -> Box<Self> {
        let tree = if depth > 0 {
            TreeNode {
                l: Some(Self::create(depth - 1)),
                r: Some(Self::create(depth - 1)),
            }
        } else {
            TreeNode { l: None, r: None }
        };
        Box::new(tree)
    }
}

const MIN_DEPTH: i32 = 4;

fn main() {
    let n = std::env::args()
        .nth(1)
        .and_then(|n| n.parse().ok())
        .unwrap_or(10);
    let max_depth = max(MIN_DEPTH + 2, n);

    {
        let stretch_depth = max_depth + 1;
        let stretch_tree = TreeNode::create(stretch_depth);

        println!(
            "stretch tree of depth {}\t check: {}",
            stretch_depth,
            stretch_tree.check()
        );
    }

    let long_lived_tree = TreeNode::create(max_depth);

    for d in (MIN_DEPTH..=max_depth).step_by(2) {
        let iterations = 1 << ((max_depth - d + MIN_DEPTH) as u32);
        let mut chk = 0;
        for _ in 0..iterations {
            let a = TreeNode::create(d);
            chk += a.check();
        }
        println!("{}\t trees of depth {}\t check: {}", iterations, d, chk)
    }

    println!(
        "long lived tree of depth {}\t check: {}",
        max_depth,
        long_lived_tree.check()
    );
}
