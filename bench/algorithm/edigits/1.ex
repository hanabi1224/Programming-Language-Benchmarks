import Math

defmodule App do
  def main(args) do
    n = String.to_integer(Enum.at(args,0,"27"), 10)
    k = binary_search(n)
    {p, q} = sum_terms(0, k-1)
    p = p + q
    a = pow(10, n - 1)
    answer = div(p*a,q)
    s = Integer.to_string(answer)
    print(s, 0, n)
  end

  defp print(s, start, n) do
    end_index=start + 10
    if end_index <= n do
      sub_str = String.slice(s, start..end_index-1)
      IO.puts("#{sub_str}\t:#{end_index}")
      if end_index < n do
        print(s, end_index, n)
      end
    else
      end_index = n
      sub_str = String.slice(s, start..n) <> gen_padding_whitespace(start+10-n)
      IO.puts("#{sub_str}\t:#{end_index}")
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

  defp binary_search(n) do
    {a,b}=binary_search_test_k_1(n,0,1)
    {a,b}=binary_search_test_k_2(n,a,b)
    b
  end

  defp binary_search_test_k_1(n, a, b) do
    if !test_k(n, b) do
      binary_search_test_k_1(n, b, b*2)
    else
      {a,b}
    end
  end

  defp binary_search_test_k_2(n, a, b) do
    if b-a>1 do
      m = div(a+b,2)
      if test_k(n,m) do
        binary_search_test_k_2(n, a, m)
      else
        binary_search_test_k_2(n, m, b)
      end
    else
      {a,b}
    end
  end

  defp test_k(n, k) do
    if k < 0 do
      false
    else
      ln_k_factorial = k * (Math.log(k) - 1) + 0.5 * Math.log(Math.tau)
      log_10_k_factorial = ln_k_factorial / Math.log(10)
      log_10_k_factorial >= n + 50
    end
  end

  defp sum_terms(a, b) do
    if b == a + 1 do
      {1,b}
    else
      mid = div(a+b,2)
      {p_left, q_left} = sum_terms(a, mid)
      {p_right, q_right} = sum_terms(mid, b)
      {p_left*q_right+p_right, q_left*q_right}
    end
  end
end
