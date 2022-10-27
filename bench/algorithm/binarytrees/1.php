<?php
/* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Peter Baltruschat
   modified by Levi Cameron
   modified by Stéphane Demonchaux
   *reset*
   
   must run with
   
   opcache.jit=1255
   opcache.enable_cli=1
   opcache.jit_buffer_size=100M
   memory_limit=400M
*/

function checksum(array|null $node):int {
    return $node === null ? 1 : 1 + checksum($node[0]) + checksum($node[1]);
}

function createTree(int $depth): array|null {
    return $depth-- > 0 ? [createTree($depth), createTree($depth)] : null;
}

$maxDepth = max(6, ($argc == 2) ? $argv[1] : 1);
$stretchDepth = $maxDepth + 1;
$stretchTree = createTree($stretchDepth);

echo sprintf("stretch tree of depth %s\t check: %s\n", $stretchDepth,  checksum($stretchTree));

$longLivedTree = createTree($maxDepth);

for ($depth = 4; $depth <= $maxDepth; $depth += 2) {
    $iterations = 1 << $maxDepth - $depth + 4;
    $sum = 0;
    for ($i = 0; $i < $iterations; $i++) {
        $sum += checksum(createTree($depth));
    }

    echo sprintf("%s\t trees of depth %s\t check: %s\n", $i, $depth, $sum);
}

echo sprintf("long lived tree of depth %s\t check: %s\n", $maxDepth, checksum($longLivedTree));
