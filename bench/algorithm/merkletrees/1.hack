<<__EntryPoint>>

function main(): void {
  $n = (int)(vec(\HH\global_get('argv') as Container<_>)[1] ?? 6);
  $minDepth = 4;
  $maxDepth = max($minDepth + 2, $n);
  $stretchDepth = $maxDepth + 1;
  $strechedTree = make($stretchDepth);
  $strechedTree->calHash();
  echo
    "stretch tree of depth $stretchDepth\t root hash: ",
    $strechedTree->getHash(),
    " check: ",
    $strechedTree->checkStr(),
    PHP_EOL;
  $longLivedTree = make($maxDepth);

  $iterations = 1 << ($maxDepth);
  do {
    $sum = 0;
    for ($i = 1; $i <= $iterations; ++$i) {
      $t = make($minDepth);
      $t->calHash();
      $sum += $t->getHash();
    }

    echo "$iterations\t trees of depth $minDepth\t root hash sum: $sum\n";

    $minDepth += 2;
    $iterations >>= 2;
  } while ($minDepth <= $maxDepth);

  $longLivedTree->calHash();
  echo
    "long lived tree of depth $maxDepth\t root hash: ",
    $longLivedTree->getHash(),
    " check: ",
    $longLivedTree->checkStr(),
    PHP_EOL;
}

class Node {
  public ?Node $left = null;
  public ?Node $right = null;
  public ?int $value = null;
  public ?int $hash = null;

  public function getHash(): int {
    if ($this->hash is null) {
      return 0;
    }
    return $this->hash;
  }

  public function checkStr(): string {
    return $this->check() ? "true" : "false";
  }

  public function check(): bool {
    if ($this->hash == null) {
      return false;
    } else if ($this->value != null) {
      return true;
    } else {
      $left = $this->left;
      $right = $this->right;
      if ($left != null && $right != null) {
        return $left->check() && $right->check();
      }
    }
    return false;
  }

  public function calHash(): void {
    if ($this->hash == null) {
      if ($this->value != null) {
        $this->hash = $this->value;
      } else {
        $left = $this->left;
        $right = $this->right;
        if ($left != null && $right != null) {
          $left->calHash();
          $right->calHash();
          $leftHash = $left->hash;
          $rightHash = $right->hash;
          if ($leftHash != null && $rightHash != null) {
            $this->hash = $leftHash + $rightHash;
          }
        }
      }
    }
  }
}

function make(int $depth): Node {
  $node = new Node();
  if ($depth > 0) {
    $depth -= 1;
    $node->left = make($depth);
    $node->right = make($depth);
  } else {
    $node->value = 1;
  }
  return $node;
}
