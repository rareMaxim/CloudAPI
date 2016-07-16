unit SM.Console;

interface

Type
  TConsoleColor = (Black, Blue, Cyan, DarkBlue, DarkCyan, DarkGray, DarkGreen,
    DarkMagenta, DarkRed, DarkYellow, Gray, Green, Magenta, Red, White, Yellow);

  TConsole = Class
  private
    FBackgroundColor: TConsoleColor;
    FForegroundColor: TConsoleColor;
    procedure SetTitle(const Value: String);
    function GetTitle: String;
  published
    property BackgroundColor: TConsoleColor read FBackgroundColor
      write FBackgroundColor;
    property ForegroundColor: TConsoleColor read FForegroundColor
      write FForegroundColor;
    property Title: String read GetTitle write SetTitle;
  End;

implementation

uses
  WinAPI.Windows;

{ TConsole }

function TConsole.GetTitle: String;
var
  lSize: Cardinal;
begin
  GetConsoleTitle(PWideChar(Result), SizeOf(PWideChar(Result)));
end;

procedure TConsole.SetTitle(const Value: String);
begin
  SetConsoleTitle(PWideChar(Value));
end;

end.
