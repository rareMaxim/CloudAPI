unit TelegAPi.Games.Quitz;

interface

uses
  System.Classes,
  TelegAPi.Bot,
  TelegAPi.Types,
  uVictorine;

Type

  TTeleGameQuitz = Class(TComponent)
  private
    FVictorine: TVictorine;
    FTelegramBot: TTelegramBot;
    FChatID: Int64;
    FQuitz: String;
    FAnswers: TArray<string>;
  public
    procedure ShowQuitz;
    procedure ParseAnswer(Const Value: String);
    Function HideQuitz(Const Quitz: String): String;
  published
    property Quitz: String read FQuitz write FQuitz;
    property ChatID: Int64 read FChatID write FChatID;
    property TelegramBot: TTelegramBot read FTelegramBot write FTelegramBot;
    property Victorine: TVictorine read FVictorine write FVictorine;
  End;

implementation

uses
  System.SysUtils;
{ TTeleGameQuitz }

function TTeleGameQuitz.HideQuitz(const Quitz: String): String;
var
  Chr: Char;
begin
  Result := '';
  for Chr in Quitz do
  begin

    if Chr <> ' ' then
      Result := Result + '🔺'
    else
      Result := Result + ' ';
  end;
end;

procedure TTeleGameQuitz.ParseAnswer(const Value: String);
var
  Answer: String;
begin
  for Answer in FAnswers do
  begin
    if Value.ToLower.Contains(Answer.ToLower) then
      TelegramBot.sendTextMessage(ChatID, 'Правильно!');
  end;
end;

procedure TTeleGameQuitz.ShowQuitz;
begin
  Quitz := Victorine.RandomQuitz;
  FAnswers := Victorine.Answer(Quitz);
  TelegramBot.sendTextMessage(ChatID, Quitz + #13#10 + HideQuitz(FAnswers[0]));
end;

end.
