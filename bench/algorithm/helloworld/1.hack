<<__EntryPoint>>
function main(): void {
  $n = (string)(vec(\HH\global_get('argv') as Container<_>)[1] ?? "");
  echo "Hello world $n!\n";
}
