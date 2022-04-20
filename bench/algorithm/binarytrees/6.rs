use std::cmp::max;

use bumpalo::Bump;

struct TreeNode<'r> {
    l: Option<&'r TreeNode<'r>>,
    r: Option<&'r TreeNode<'r>>,
}

impl<'r> TreeNode<'r> {
    fn check(&self) -> i32 {
        let mut ret = 1;
        if let Some(l) = &self.l {
            ret += l.check();
        }
        if let Some(r) = &self.r {
            ret += r.check();
        }
        ret
    }

    fn create(bump: &Bump, depth: i32) -> &TreeNode<'_> {
        let tree = if depth > 0 {
            TreeNode {
                l: Some(Self::create(bump, depth - 1)),
                r: Some(Self::create(bump, depth - 1)),
            }
        } else {
            TreeNode { l: None, r: None }
        };
        bump.alloc(tree)
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
        let depth = max_depth + 1;
        let bump = Bump::new();
        let tree = TreeNode::create(&bump, depth);

        println!("stretch tree of depth {}\t check: {}", depth, tree.check());
    }

    let long_lived_bump = Bump::new();
    let long_lived_tree = TreeNode::create(&long_lived_bump, max_depth);

    let mut bump = Bump::new();
    for d in (MIN_DEPTH..=max_depth).step_by(2) {
        let iterations = 1 << ((max_depth - d + MIN_DEPTH) as u32);
        let mut chk = 0;
        for _ in 0..iterations {
            let a = TreeNode::create(&bump, d);
            chk += a.check();
            bump.reset();
        }
        println!("{}\t trees of depth {}\t check: {}", iterations, d, chk)
    }

    println!(
        "long lived tree of depth {}\t check: {}",
        max_depth,
        long_lived_tree.check()
    );
}
