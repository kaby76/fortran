using System.IO;
using System.Text.RegularExpressions;

public class RuleExtraction
{
    static void Main(string[] args)
    {
        string line;
        Regex rs = new Regex(@"^R\d");
        Regex re = new Regex(@"^[ ]");
        bool do_print = false;
        while ((line = Console.ReadLine()) != null)
        {
            if (rs.IsMatch(line))
            {
                do_print = true;
                Console.WriteLine(line);
            }
            else if (do_print && re.IsMatch(line))
            {
                Console.WriteLine(line);
            }
            else
            {
                do_print = false;
            }
        }
    }
}