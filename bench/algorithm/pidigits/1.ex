import Math

defmodule App do
  def main(args) do
    n = String.to_integer(Enum.at(args,0,"27"), 10)
    loop(1,4,3,1,1,n)
  end

  defp loop(k, n1, n2, d, digit, to_print) do
    u = div(n1, d)
    v = div(n2, d)
    if u == v do
      ended = digit >= to_print
      IO.write(u)
      digit_mod_ten = Integer.mod(digit, 10)
      if digit_mod_ten == 0 do
        IO.puts("\t:#{digit}")
      else
        if ended do
          padding = gen_padding_whitespace(10-digit_mod_ten)
          IO.puts("#{padding}\t:#{digit}")
        end
      end
      if !ended do
        to_minus = u * 10 * d
        n1 = n1 * 10 - to_minus
        n2 = n2 * 10 - to_minus
        loop(k, n1, n2, d, digit + 1, to_print)
      end
    else
      k2 = k * 2
      u = n1 * (k2 - 1)
      v = n2 * 2
      w = n1 * (k - 1)
      n1 = u + v
      u = n2 * (k + 2)
      n2 = w + u
      d = d * (k2 + 1)
      k = k + 1
      loop(k, n1, n2, d, digit, to_print)
    end
  end

  defp gen_padding_whitespace(n) do
    if n == 0 do
      ""
    else
      if n == 1 do
      " "
      else
        " " <> gen_padding_whitespace(n-1)
      end
    end
  end
end
