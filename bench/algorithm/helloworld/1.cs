using System;

public class Program
{
    public static void Main(String[] args)
    {
            var name = args.Length > 0 ? args[0] : string.Empty;
            Console.WriteLine($"Hello world {name}!");
    }
}
