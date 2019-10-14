using System.Reflection.Emit;

namespace BrainfuckSharp
{
    internal class Token
    {
        public readonly TokenType Type;
        public Label LabelBefore;
        public Label LabelAfter;
        public int Value;

        public Token(TokenType type, int value)
        {
            Type = type;
            Value = value;
        }

        public Token(TokenType type) : this(type, 0)
        {
        }
    }

    internal enum TokenType
    {
        MovePointerLeft,
        MovePointerRight,
        AddToCell,
        SubtractFromCell,
        OutputChar,
        InputChar,
        LBracket,
        RBracket
    }
}
