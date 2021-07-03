defmodule App do
  def main(args) do
    n = String.to_integer(Enum.at(args,0,"27"), 10)
    generate(n)
  end

  def generate(n) do
    mainPid = self()
    pid = spawn_link(fn -> filter(mainPid, n) end)
    generateLoop(pid, 2)
  end

  def generateLoop(pid, n) do
    send(pid, {:n, n})
    receive do
      :gen -> generateLoop(pid, n + 1)
    end
  end

  def filter(mainPid, nLeft) do
    receive do
      {:n, n} -> filterInner(mainPid, n, nLeft)
    end
  end

  def filterInner(mainPid, prime, nLeft) do
    send(mainPid, :gen)
    IO.puts("#{prime}")
    if nLeft > 1 do
      pid = spawn_link(fn -> filter(mainPid, nLeft-1) end)
      recieveAndSendToFilter(mainPid, self(), pid, prime)
    else
      System.halt(0)
    end
  end

  def recieveAndSendToFilter(mainPid, rxPid, txPid, prime) do
    receive do
      {:n, n} -> recieveAndSendToFilterInner(mainPid, rxPid, txPid, prime, n)
    end
  end
  def recieveAndSendToFilterInner(mainPid, rxPid, txPid, prime, n) do
    if Integer.mod(n, prime) != 0 do
      send(txPid, {:n, n})
    else
      send(mainPid, :gen)
    end
    recieveAndSendToFilter(mainPid, rxPid, txPid, prime)
  end
end
