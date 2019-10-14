using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Reflection.Emit;

namespace BrainfuckSharp
{
    public class Program
    {
        private static readonly MethodInfo WriteChar = typeof(Console).GetMethod(nameof(Console.Write),
            BindingFlags.Public | BindingFlags.Static, null, new[] {typeof(char)}, null);

        private static readonly MethodInfo ReadChar = typeof(Console).GetMethod(nameof(Console.ReadKey),
            BindingFlags.Public | BindingFlags.Static, null, new[] {typeof(bool)}, null);

        private static readonly MethodInfo KeyChar = typeof(ConsoleKeyInfo)
            .GetProperty(nameof(ConsoleKeyInfo.KeyChar), BindingFlags.Public | BindingFlags.Instance)?.GetGetMethod();

        public static void Main(string[] args)
        {
            if (args.Length != 1)
            {
                Console.WriteLine(@"Supply a file parameter containing valid brainfuck code.");
                return;
            }

            string file = args[0];

            if (!File.Exists(file))
            {
                Console.WriteLine($"File {file} does not exist!\n Exiting.");
                return;
            }

            Token[] tokens = Tokenize(File.ReadAllText(file));
            Compile(tokens);
        }

        private static Token[] Tokenize(string bf)
        {
            List<Token> tokens = new List<Token>();
            Stack<int> brackets = new Stack<int>();

            bool ptrMove = false;
            bool cellAdd = false;
            int count = 0;

            foreach (char c in bf)
            {
                // Check for end of pointer move string
                if (ptrMove && count != 0 && c != '>' && c != '<')
                {
                    TokenType type = count > 0
                        ? TokenType.MovePointerRight
                        : TokenType.MovePointerLeft;
                    count = Math.Abs(count);

                    tokens.Add(new Token(type, count));

                    ptrMove = false;
                    count = 0;
                }

                // Check for end of cell arithmetic string
                if (cellAdd && count != 0 && c != '+' && c != '-')
                {
                    TokenType type = count > 0
                        ? TokenType.AddToCell
                        : TokenType.SubtractFromCell;
                    count = Math.Abs(count);

                    tokens.Add(new Token(type, count));

                    cellAdd = false;
                    count = 0;
                }

                switch (c)
                {
                    case '>':
                        ptrMove = true;
                        count++;
                        break;
                    case '<':
                        ptrMove = true;
                        count--;
                        break;
                    case '+':
                        cellAdd = true;
                        count++;
                        break;
                    case '-':
                        cellAdd = true;
                        count--;
                        break;
                    case '.':
                        tokens.Add(new Token(TokenType.OutputChar));
                        break;
                    case ',':
                        tokens.Add(new Token(TokenType.InputChar));
                        break;
                    case '[':
                        brackets.Push(tokens.Count);
                        tokens.Add(new Token(TokenType.LBracket));
                        break;
                    case ']':
                        if (brackets.Count == 0)
                        {
                            Console.WriteLine($"Closing bracket at position {tokens.Count} does not have a matching opening bracket");
                            Environment.Exit(-1);
                        }

                        int openBrack = brackets.Pop();
                        if (tokens[openBrack].Type != TokenType.LBracket)
                        {
                            Console.WriteLine("Closing bracket matched to something that wasn't an opening bracket. This shouldn't happen!");
                            Environment.Exit(-1);
                        }

                        tokens[openBrack].Value = tokens.Count;
                        tokens.Add(new Token(TokenType.RBracket, openBrack));

                        break;
                }
            }

            if (brackets.Count != 0)
            {
                Console.WriteLine($"Opening bracket at position {brackets.Peek()} does not have a matching closing bracket");
                Environment.Exit(-1);
            }

            return tokens.ToArray();
        }

        private static void Compile(Token[] bf)
        {
            // Setup various builders for the assembly
            AssemblyBuilder asmBuilder =
                AppDomain.CurrentDomain.DefineDynamicAssembly(new AssemblyName("Brainfuck"),
                    AssemblyBuilderAccess.Save);
            ModuleBuilder modBuilder = asmBuilder.DefineDynamicModule("Brainfuck", "Brainfuck.exe", false);
            TypeBuilder program = modBuilder.DefineType("Program", TypeAttributes.Public);
            MethodBuilder main = program.DefineMethod("Main",
                MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig, typeof(void),
                new Type[0]);
            ILGenerator il = main.GetILGenerator();
            LocalBuilder mem = il.DeclareLocal(typeof(byte*));
            LocalBuilder keyTemp = il.DeclareLocal(typeof(ConsoleKeyInfo));

            // Initialize memory to 30k bytes
            il.Emit(OpCodes.Ldc_I4, 30000);
            il.Emit(OpCodes.Conv_U);
            il.Emit(OpCodes.Localloc);
            il.Emit(OpCodes.Stloc, mem);

            // Create labels
            foreach (Token token in bf)
            {
                token.LabelBefore = il.DefineLabel();
                token.LabelAfter = il.DefineLabel();
            }

            // Compilation of tokens
            foreach (Token token in bf)
            {
                il.MarkLabel(token.LabelBefore);
                switch (token.Type)
                {
                    case TokenType.MovePointerLeft:
                    case TokenType.MovePointerRight:
                        il.Emit(OpCodes.Ldloc, mem);
                        il.Emit(OpCodes.Ldc_I4, token.Value);
                        il.Emit(token.Type == TokenType.MovePointerLeft ? OpCodes.Sub : OpCodes.Add);
                        il.Emit(OpCodes.Stloc, mem);
                        break;
                    case TokenType.AddToCell:
                    case TokenType.SubtractFromCell:
                        il.Emit(OpCodes.Ldloc, mem);
                        il.Emit(OpCodes.Dup);
                        il.Emit(OpCodes.Ldind_U1);
                        il.Emit(OpCodes.Ldc_I4, token.Value);
                        il.Emit(token.Type == TokenType.SubtractFromCell ? OpCodes.Sub : OpCodes.Add);
                        il.Emit(OpCodes.Stind_I1);
                        break;
                    case TokenType.OutputChar:
                        il.Emit(OpCodes.Ldloc, mem);
                        il.Emit(OpCodes.Ldind_U1);
                        il.Emit(OpCodes.Call, WriteChar);
                        break;
                    case TokenType.InputChar:
                        il.Emit(OpCodes.Ldloc, mem);
                        il.Emit(OpCodes.Ldc_I4_1);
                        il.Emit(OpCodes.Call, ReadChar);
                        il.Emit(OpCodes.Stloc, keyTemp);
                        il.Emit(OpCodes.Ldloca, keyTemp);
                        il.Emit(OpCodes.Call, KeyChar);
                        il.Emit(OpCodes.Stind_I1);
                        break;
                    case TokenType.LBracket:
                    case TokenType.RBracket:
                        il.Emit(OpCodes.Ldloc, mem);
                        il.Emit(OpCodes.Ldind_U1);
                        il.Emit(token.Type == TokenType.RBracket ? OpCodes.Brtrue : OpCodes.Brfalse,
                            bf[token.Value].LabelAfter);
                        break;
                    default:
                        throw new ArgumentOutOfRangeException($"{nameof(token)}.{nameof(token.Type)}", token.Type, null);
                }

                il.MarkLabel(token.LabelAfter);
            }

            il.Emit(OpCodes.Ret);

            program.CreateType();

            asmBuilder.SetEntryPoint(main);
            asmBuilder.Save("Brainfuck.exe", PortableExecutableKinds.Preferred32Bit, ImageFileMachine.I386);
        }
    }
}
