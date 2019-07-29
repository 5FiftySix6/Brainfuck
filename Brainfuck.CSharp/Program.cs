using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Brainfuck
{
    internal static class Program
    {
        private static void Main(string[] args)
        {
            if (args.Length != 1)
            {
                Console.WriteLine(@"Supply a file parameter containing valid brainfuck code.");
                Console.WriteLine("If wanted supply a second file parameter to replace human standard io.");
                return;
            }

            string file = args[0];

            if (!File.Exists(file))
            {
                Console.WriteLine($"File {file} does not exist!\n Exiting.");
                return;
            }

            string text = File.ReadAllText(file);

            Interpret(text);
        }

        private static unsafe void Interpret(string text)
        {
            if (!CheckBrackets(text)) return;

            // 30,000 is the typical size for brainfuck.
            char* mem = stackalloc char[30000];

            Stack<int> jmp = new Stack<int>();

            for (int i = 0; i < text.Length; i++)
            {
                char c = text[i];

                switch (c)
                {
                    case '>':
                        mem++;
                        break;
                    case '<':
                        mem--;
                        break;
                    case '+':
                        (*mem)++;
                        break;
                    case '-':
                        (*mem)--;
                        break;
                    case ',':
                        *mem = Console.ReadKey().KeyChar;
                        break;
                    case '.':
                        Console.Write(*mem);
                        break;
                    case '[' when *mem != 0:
                        jmp.Push(i);
                        break;
                    case '[' when *mem == 0:
                        int brackets = 1;

                        while (brackets > 0)
                        {
                            i++;

                            if (text[i] == '[')
                                brackets++;
                            else if (text[i] == ']')
                                brackets--;
                        }

                        break;
                    case ']' when *mem != 0:
                        i = jmp.Peek();
                        break;
                    case ']' when *mem == 0:
                        jmp.TryPop(out int _);
                        break;
                }
            }
        }

        private static bool CheckBrackets(string text)
        {
            void HandleBracket(char c, int ind)
            {
                (int lineNum, string line, int pos) = GetLinePosOfChar(text, ind);

                char op = c == '[' ? ']' : '[';

                Console.WriteLine($"'{c}' at position {pos} on line {lineNum} is missing matching '{op}':");
                Console.WriteLine("> " + line);
                Console.WriteLine(new string(' ', pos + 2) + "^");
            }

            IEnumerable<(int, char)> brackets = text.Select((x, ind) => (ind, x)).Where(t => t.Item2 == ']' || t.Item2 == '[');
            
            int open = 0;
            int? openInd = null;

            foreach ((int ind, char c) in brackets)
            {
                switch (c)
                {
                    case ']' when open == 0:
                        HandleBracket(']', ind);
                        return false;
                    case '[':
                        open++;
                        openInd = ind;
                        break;
                    case ']':
                        open--;
                        break;
                }
            }

            if (open == 0) return true;

            HandleBracket('[', openInd ?? throw new Exception("openInd was not set."));

            return false;
        }

        private static (int, string, int) GetLinePosOfChar(string text, int ind)
        {
            int lineStart = text.LastIndexOf('\n', ind) + 1;

            int lineEnd = text.IndexOf('\n', lineStart);

            int length = lineEnd == -1
                ? text.Length - lineStart
                : lineEnd - lineStart;

            int lineNum = text.Substring(0, lineStart).Count(x => x == '\n') + 1;

            return (lineNum, text.Substring(lineStart, length), ind - lineStart);
        }
    }
}