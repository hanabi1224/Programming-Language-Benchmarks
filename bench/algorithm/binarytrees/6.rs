pub type TreeIndex = usize;

#[derive(Clone, Copy)]
struct TreeNode {
    l: TreeIndex,
    r: TreeIndex,
}

struct Tree {
    nodes: Vec<TreeNode>,
}

impl Tree {
    fn check(&self) -> i32 {
        self.check_inner(self.nodes.len() - 1)
    }

    fn check_inner(&self, ix: usize) -> i32 {
        let mut ret = 1;
        let TreeNode { l, r } = self.nodes[ix];
        if l > 0 {
            ret += self.check_inner(l);
        }
        if r > 0 {
            ret += self.check_inner(r);
        }
        ret
    }

    fn create(depth: usize) -> Self {
        let mut tree = Tree {
            nodes: Vec::with_capacity(1 << (depth + 1)),
        };
        tree.nodes.push(TreeNode { l: 0, r: 0 });
        let root = TreeNode::new(depth, &mut tree);
        assert_eq!(root + 1, tree.nodes.len());
        tree
    }
}

impl TreeNode {
    fn new(depth: usize, tree: &mut Tree) -> TreeIndex {
        if depth > 0 {
            let n = TreeNode {
                l: Self::new(depth - 1, tree),
                r: Self::new(depth - 1, tree),
            };
            tree.nodes.push(n);
            tree.nodes.len() - 1
        } else {
            tree.nodes.push(TreeNode { l: 0, r: 0 });
            tree.nodes.len() - 1
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

    let max_depth = if MIN_DEPTH + 2 > n { MIN_DEPTH + 2 } else { n };

    {
        let depth = max_depth + 1;
        let tree = Tree::create(max_depth + 1);

        println!("stretch tree of depth {}\t check: {}", depth, tree.check());
    }

    let long_lived_tree = Tree::create(max_depth);

    for d in (MIN_DEPTH..max_depth + 1).step_by(2) {
        let iterations = 1 << ((max_depth - d + MIN_DEPTH) as u32);
        let mut chk = 0;
        for _i in 0..iterations {
            let a = Tree::create(d);
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

