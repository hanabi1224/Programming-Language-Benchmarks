import Math

defmodule App do
  def main(args) do
    n = String.to_integer(Enum.at(args,0,"10"), 10)
    u = create_list(1.0, n)
    v = create_list(1.0, n)
    tuple = a_times_transp_loop(u,v,10, n)
    u = elem(tuple,0)
    v = elem(tuple,1)
    vbv = product(u,v)
    vv = product(v,v)
    ans = sqrt(vbv / vv)
    IO.puts("#{:erlang.float_to_binary(ans, [decimals: 9])}")
  end

  defp product(u,v) do
    result = hd(u) * hd(v)
    if (length u) > 1 do
      result + product(tl(u), tl(v))
    else
      result
    end
  end

  defp a_times_transp_loop(u,v,times, n) do
    if times > 0 do
      v = a_times_transp(v,u,n)
      u = a_times_transp(u,v,n)
      a_times_transp_loop(u,v,times-1,n)
    else
      {u, v}
    end
  end

  defp a_times_transp(v, u, n) do
    x = create_list(0.0, n)
    x=times(x, u, n, false)
    times(v, x, n, true)
  end

  defp create_list(value, size) do
    create_list_loop(value, size)
  end

  defp create_list_loop(value, n) do
    if n>1 do
      [value] ++ create_list_loop(value, n-1)
    else
      [value]
    end
  end

  defp evala(i, j) do
    sum = i + j
    sum * (sum + 1) / 2 + i + 1
  end

  defp times(v, u, n, reverse) do
    times_loop(v, u, n, reverse, 0)
  end

  defp times_loop(v, u, n, reverse, i) do
    if i < n do
      head = times_sum(u, reverse, n, i)
      [head] ++ times_loop(tl(v), u, n, reverse, i+1)
    else
      []
    end
  end

  defp times_sum(u, reverse, n, i) do
    times_sum_loop(u, reverse, n, i, 0)
  end

  defp times_sum_loop(u, reverse, n, i, j) do
    if j < n do
      value = hd(u)
      if reverse do
        value / evala(j, i) + times_sum_loop(tl(u), reverse, n, i, j+1)
      else
        value / evala(i, j) + times_sum_loop(tl(u), reverse, n, i, j+1)
      end
    else
      0.0
    end
  end
end
