<?php
class Node
{
    public $l;
    public $r;
    public $value;
    public $hash;

    public function check()
    {
        if ($this->hash == null) {
            return false;
        }
        return $this->value != null || ($this->l->check() && $this->r->check());
    }

    public function checkStr()
    {
        return $this->check() ? "true" : "false";
    }

    public function calHash()
    {
        if ($this->hash == null) {
            if ($this->value != null) {
                $this->hash = $this->value;
            } else {
                $this->l->calHash();
                $this->r->calHash();
                $this->hash = $this->l->hash + $this->r->hash;
            }
        }
    }
}

function make($depth)
{
    $node = new Node();
    if ($depth < 1) {
        $node->value = 1;
    } else {
        $depth -= 1;
        $node->l = make($depth);
        $node->r = make($depth);
    }
    return $node;
}

$minDepth = 4;

$n = ($argc == 2) ? $argv[1] : 1;
$maxDepth = max($minDepth + 2, $n);
$stretchDepth = $maxDepth + 1;

$stretchTree = make($stretchDepth);
$stretchTree->calHash();
printf("stretch tree of depth %d\t root hash: %d check: %s\n", $stretchDepth, $stretchTree->hash, $stretchTree->checkStr());
unset($stretchTree);

$longLivedTree = make($maxDepth);

$iterations = 1 << ($maxDepth);
do {
    $sum = 0;
    for ($i = 1; $i <= $iterations; ++$i) {
        $t = make($minDepth);
        $t->calHash();
        $sum += $t->hash;
        unset($t);
    }

    printf("%d\t trees of depth %d\t root hash sum: %d\n", $iterations, $minDepth, $sum);

    $minDepth += 2;
    $iterations >>= 2;
} while ($minDepth <= $maxDepth);

$longLivedTree->calHash();
printf(
    "long lived tree of depth %d\t root hash: %d check: %s\n",
    $maxDepth,
    $longLivedTree->hash,
    $longLivedTree->checkStr()
);
unset($longLivedTree);
