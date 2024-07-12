using Antlr4.Runtime;
using System.IO;

public abstract class EBNFParserBase : Parser {
	private readonly ITokenStream _input;

	protected EBNFParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
			: base(new ChannelCommonTokenStream(input), output, errorOutput)
	{
		_input = input;
	}

	public bool IsNotWS()
	{
		var c = (this.InputStream as ChannelCommonTokenStream).LT(-1, 1);
        return c.Type != EBNFParser.WS && c.Type != EBNFParser.NL;
	}
}