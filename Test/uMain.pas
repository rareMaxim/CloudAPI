unit uMain;

interface

uses uVictorine,
  LoggerPro, TelegAPI.Bot, TelegAPI.Types,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  System.Generics.Collections;

type
  TForm2 = class(TForm)
    TelegramBot1: TTelegramBot;
    Memo1: TMemo;

    procedure FormCreate(Sender: TObject);
    procedure TelegramBot1Error(const Sender: TObject; const Code: Integer; const Message: string);
    procedure TelegramBot1Update(const Sender: TObject; const Update: TTelegaUpdate);
  private
    { Private declarations }
    FLog: ILogWriter;
    FQuitz: TVictorine;
    EnableAnswer: Boolean;
  public
    { Public declarations }
    function Log: ILogWriter;
  end;

var
  Form2: TForm2;

implementation

uses

  XSuperObject,
  LoggerPro.FMXMemoAppender;
{$R *.fmx}
{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  FLog := BuildLogWriter([TFMXMemoLogAppender.Create(Memo1)]);
  TelegramBot1.Token := {$I telegaToken.inc};
  TelegramBot1.IsReceiving := True;
  EnableAnswer := False;
  FQuitz := TVictorine.Create(nil);
  FQuitz.LoadFromFile('D:\VersionControl\DELPHI\uasoft-atari\Game Files\Викторина.txt');
end;

function TForm2.Log: ILogWriter;
begin
  Result := FLog;
end;

procedure TForm2.TelegramBot1Error(const Sender: TObject; const Code: Integer;
  const Message: string);
begin
  Log.Error(Message, Code.ToString);
end;

procedure TForm2.TelegramBot1Update(const Sender: TObject; const Update: TTelegaUpdate);
var
  Msg: String;
  Reply: String;
  ReplyMark: TTelegaReplyKeyboardMarkup;
  Quitz: TVictorine;
begin
  Log.Info(Update.Message.Text, Update.Message.From.Username);

  Msg := Update.Message.Text.ToLower;
  if Msg.Contains('format') then
  Begin
    Reply := '<b>bold</b>, <strong>bold</strong>' + #13#10 + '<i>italic</i>, <em>italic</em>' +
      #13#10 + '<a href="codmasters.ru">inline URL</a>' + #13#10 +
      '<code>inline fixed-width code</code>' + #13#10 +
      '<pre>pre-formatted fixed-width code block</pre>';
    TelegramBot1.sendTextMessage(Update.Message.Chat.ID, Reply, TTelegaParseMode.Html)
  End;
  if Msg.Contains('card') then
  Begin
    ReplyMark := TTelegaReplyKeyboardMarkup.Create;
    try
      ReplyMark.KeyBoard := [[TTelegaKeyboardButton.Create('♥ Валет'),
        TTelegaKeyboardButton.Create('♠️Туз')], [TTelegaKeyboardButton.Create('♥ 8'),
        TTelegaKeyboardButton.Create('♣️Джокер'), TTelegaKeyboardButton.Create('♦️2')]];
      Reply := 'Тест клавиатуры';
      TelegramBot1.sendTextMessage(Update.Message.From.ID, Reply, TTelegaParseMode.Html, False,
        False, 0, ReplyMark);
    finally
      ReplyMark.Free;
    end;

  End;
  if Msg.Contains('XO') then
  Begin
    ReplyMark := TTelegaReplyKeyboardMarkup.Create;
    try
      ReplyMark.KeyBoard := [
      [TTelegaKeyboardButton.Create()]
      ];
      Reply := 'Тест клавиатуры';
      TelegramBot1.sendTextMessage(Update.Message.From.ID, Reply, TTelegaParseMode.Html, False,
        False, 0, ReplyMark);
    finally
      ReplyMark.Free;
    end;

  End;
  if Msg.Contains('включить ответы') then
    EnableAnswer := True;
  if Msg.Contains('выключить ответы') then
    EnableAnswer := False;
  if EnableAnswer then
  begin
    Reply := (string.Join(' ', FQuitz.Answer(Msg)));
    if NOT Reply.Trim.IsEmpty then
      TelegramBot1.sendTextMessage(Update.Message.Chat.ID, Reply);
  end;
end;

end.
