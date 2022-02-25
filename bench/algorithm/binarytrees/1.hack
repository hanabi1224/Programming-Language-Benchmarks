<<__EntryPoint>>

function main(): void {
  $n = (int)(vec(\HH\global_get('argv') as Container<_>)[1] ?? 6);
  $minDepth = 4;
  $maxDepth = max($minDepth + 2, $n);
  $stretchDepth = $maxDepth + 1;
  $strechedTree = make($stretchDepth);
  echo
    "stretch tree of depth $stretchDepth\t check: ",
    $strechedTree->check(),
    PHP_EOL;
  $longLivedTree = make($maxDepth);

  $iterations = 1 << ($maxDepth);
  do {
    $check = 0;
    for ($i = 1; $i <= $iterations; ++$i) {
      $t = make($minDepth);
      $check += $t->check();
    }

    printf(
      "%d\t trees of depth %d\t check: %d\n",
      $iterations,
      $minDepth,
      $check,
    );

    $minDepth += 2;
    $iterations >>= 2;
  } while ($minDepth <= $maxDepth);

  echo
    "long lived tree of depth $maxDepth\t check: ",
    $longLivedTree->check(),
    PHP_EOL;
}

class Node {
  public ?Node $left = null;
  public ?Node $right = null;

  public function check(): int {
    $v = 1;
    $left = $this->left;
    $right = $this->right;
    if ($left != null && $right != null) {
      $v += $left->check() + $right->check();
    }
    return $v;
  }
}

function make(int $depth): Node {
  $node = new Node();
  if ($depth > 0) {
    $depth -= 1;
    $node->left = make($depth);
    $node->right = make($depth);
  }
  return $node;
}
