type Full<T> = static_rc::StaticRc<T, 2, 2>;
// type Half<T> = static_rc::StaticRc<T, 1, 2>;

struct TreeNode {
    l: Option<Full<Self>>,
    r: Option<Full<Self>>,
}

impl TreeNode {
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

    fn create<'r>(depth: i32) -> Full<Self> {
        Full::new(Self::_create_inner(depth))
    }

    fn _create_inner<'r>(depth: i32) -> Self {
        if depth > 0 {
            // let (l, _) = Full::split::<1, 1>(Full::new(Self::_create_inner(depth - 1)));
            // let (_, r) = Full::split::<1, 1>(Full::new(Self::_create_inner(depth - 1)));
            let l = Full::new(Self::_create_inner(depth - 1));
            let r = Full::new(Self::_create_inner(depth - 1));
            Self {
                l: Some(l),
                r: Some(r),
            }
        } else {
            Self { l: None, r: None }
        }
    }
}

const MIN_DEPTH: i32 = 4;

fn main() {
    let n = std::env::args_os()
        .nth(1)
        .and_then(|s| s.into_string().ok())
        .and_then(|n| n.parse().ok())
        .unwrap_or(10);

    let max_depth = if MIN_DEPTH + 2 > n { MIN_DEPTH + 2 } else { n };
    {
        let depth = max_depth + 1;
        let tree = TreeNode::create(max_depth + 1);

        println!("stretch tree of depth {}\t check: {}", depth, tree.check());
    }

    let long_lived_tree = TreeNode::create(max_depth);

    for d in (MIN_DEPTH..max_depth + 1).step_by(2) {
        let iterations = 1 << ((max_depth - d + MIN_DEPTH) as u32);
        let mut chk = 0;
        for _i in 0..iterations {
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
