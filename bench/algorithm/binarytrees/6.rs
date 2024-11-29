use std::cmp::max;

enum TreeNode {
    Leaf,
    Node(Box<TreeNode>, Box<TreeNode>),
}

impl TreeNode {
    fn check(&self) -> usize {
        match &self {
            TreeNode::Leaf => 1,
            TreeNode::Node(left, right) => 1 + left.check() + right.check(),
        }
    }

    fn create(depth: usize) -> Box<TreeNode> {
        if depth > 0 {
            let next_depth = depth - 1;
            Box::new(TreeNode::Node(Self::create(next_depth), Self::create(next_depth)))
        } else {
            Box::new(TreeNode::Leaf)
        }
    }
}

const MIN_DEPTH: usize = 4;

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(10);

    let max_depth = max(MIN_DEPTH + 2, n);
    let stretch_depth = max_depth + 1;

    println!("stretch tree of depth {}\t check: {}", stretch_depth, TreeNode::create(stretch_depth).check());

    let long_lived_tree = TreeNode::create(max_depth);

    for iteration_depth in (MIN_DEPTH..stretch_depth).step_by(2) {
        let iterations = 1 << (max_depth - iteration_depth + MIN_DEPTH);
        let mut nodes = 0;
        for _ in 0..iterations {
            nodes += TreeNode::create(iteration_depth).check();
        }
        println!("{iterations}\t trees of depth {iteration_depth}\t check: {nodes}")
    }

    let nodes = long_lived_tree.check();
    println!("long lived tree of depth {max_depth}\t check: {nodes}");
}
