unit EchoBot.Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  TelegAPI.Bot,
  TelegAPI.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TMain = class(TForm)
    tgBot: TTelegramBot;
    mmo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure tgBotInlineResultChosen(ASender: TObject;
      AChosenInlineResult: TtgChosenInlineResult);
    procedure tgBotInlineQuery(ASender: TObject; AInlineQuery: TtgInlineQuery);
    procedure tgBotMessage(ASender: TObject; AMessage: TtgMessage);
    procedure tgBotCallbackQuery(ASender: TObject;
      ACallbackQuery: TtgCallbackQuery);
  private
    { Private declarations }
    procedure WriteLine(Const AValue: String);
    procedure SendInline(Msg: TtgMessage);
    procedure SendKeyboard(Msg: TtgMessage);
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

uses
  {..} TelegAPI.Helpers
  {} , TelegAPI.Types.Enums
  {} , TelegAPI.Types.InlineQueryResults
  {} , TelegAPI.Types.InputMessageContents
  {} , TelegAPI.Types.ReplyMarkups
  {.};
{$R *.fmx}

procedure TMain.FormCreate(Sender: TObject);
begin
  tgBot.Token := {$I ..\token.inc};
  if not tgBot.IsValidToken then
    raise ELoginCredentialError.Create('invalid token format');
end;

procedure TMain.SendInline;
var
  keyboard: TtgInlineKeyboardMarkup;
begin
  tgBot.SendChatAction(Msg.Chat.Id, TtgSendChatAction.Typing);
  keyboard := TtgInlineKeyboardMarkup.Create([
    { first row }
    [TtgInlineKeyboardButton.Create('1.1'),
    TtgInlineKeyboardButton.Create('1.2')],
    { second row }
    [TtgInlineKeyboardButton.Create('2.1'),
    TtgInlineKeyboardButton.Create('2.2')]]);

  Sleep(500); // simulate longer running task

  tgBot.SendMessage(Msg.Chat.Id, 'Choose', TtgParseMode.default, False, False,
    0, keyboard);
end;

procedure TMain.SendKeyboard(Msg: TtgMessage);
var
  keyboard: TtgReplyKeyboardMarkup;
begin
  keyboard := TtgReplyKeyboardMarkup.Create
    ([[TtgKeyboardButton.Create('1.1'), TtgKeyboardButton.Create('1.2')],
    [TtgKeyboardButton.Create('2.1'), TtgKeyboardButton.Create('2.2')]]);

  tgBot.SendMessage(Msg.Chat.Id, 'Choose', TtgParseMode.default, False, False,
    0, keyboard);
end;

procedure TMain.tgBotCallbackQuery(ASender: TObject;
  ACallbackQuery: TtgCallbackQuery);
begin
  tgBot.AnswerCallbackQuery(ACallbackQuery.Id,
    'Received ' + ACallbackQuery.Data);
end;

procedure TMain.tgBotInlineQuery(ASender: TObject;
  AInlineQuery: TtgInlineQuery);
var
  results: TArray<TtgInlineQueryResult>;
begin
  results := [TtgInlineQueryResultLocation.Create,
    TtgInlineQueryResultLocation.Create];
  with TtgInlineQueryResultLocation(results[0]) do
  begin
    Id := '1';
    Latitude := 40.7058316; // displayed result
    Longitude := -74.2581888;
    Title := 'New York';
    InputMessageContent := TtgInputLocationMessageContent.Create;
    // message if result is selected
    TtgInputLocationMessageContent(InputMessageContent).Latitude := 40.7058316;
    TtgInputLocationMessageContent(InputMessageContent).Longitude :=
      -74.2581888;
  end;
  with TtgInlineQueryResultLocation(results[1]) do
  begin
    Id := '2';
    Latitude := 52.507629; // displayed result
    Longitude := 13.1449577;
    Title := 'Berlin';
    InputMessageContent := TtgInputLocationMessageContent.Create;
    // message if result is selected
    TtgInputLocationMessageContent(InputMessageContent).Latitude := 52.507629;
    TtgInputLocationMessageContent(InputMessageContent).Longitude := 13.1449577;
  end;
  tgBot.AnswerInlineQuery(AInlineQuery.Id, results, 0, true);
end;

procedure TMain.tgBotInlineResultChosen(ASender: TObject;
  AChosenInlineResult: TtgChosenInlineResult);
begin
  WriteLine('Received choosen inline result: ' + AChosenInlineResult.ResultId);
end;

procedure TMain.tgBotMessage(ASender: TObject; AMessage: TtgMessage);
begin
  if not Assigned(AMessage) then
    Exit;
  if AMessage.Text.StartsWith('/inline') then // send inline keyboard
  begin
    SendInline(AMessage);
  end
  else if (AMessage.Text.StartsWith('/keyboard')) then // send custom keyboard
  begin
    SendKeyboard(AMessage);
  end;
end;

procedure TMain.WriteLine(const AValue: String);
begin
  mmo1.Lines.Insert(0, AValue);
end;

end.
