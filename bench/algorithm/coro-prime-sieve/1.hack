async function generate(): AsyncIterator<int> {
  for ($i = 2; ; $i++) {
    yield $i;
  }
}

async function filter(
  AsyncIterator<int> $iterator,
  int $prime,
): AsyncIterator<int> {
  foreach ($iterator await as $i) {
    if ($i % $prime != 0) {
      yield $i;
    }
  }
}

async function run(int $n): Awaitable<void> {
  $it = generate();
  for ($i = 0; $i < $n; $i++) {
    $tuple = await $it->next();
    if ($tuple is nonnull) {
      $prime = (int)($tuple[1]);
      echo $prime."\n";
      $it = filter($it, $prime);
    }
  }
}

<<__EntryPoint>>
function main(): void {
  $n = (int)(vec(\HH\global_get('argv') as Container<_>)[1] ?? 10);
  \HH\Asio\join(run($n));
}
