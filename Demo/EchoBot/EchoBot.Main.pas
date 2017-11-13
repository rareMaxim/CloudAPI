unit EchoBot.Main;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  FMX.Types,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Memo,
  TelegAPI.Bot,
  TelegAPI.Types,
  TelegAPI.Types.Intf,
  TelegAPI.Exceptions,
  FMX.Edit,
  FMX.StdCtrls,
  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Layouts,
  TelegAPI.Bot.Recesiver.UI,
  TelegAPI.Base;

type
  TMain = class(TForm)
    mmoLog: TMemo;
    Layout1: TLayout;
    lblToken: TLabel;
    edtToken: TEdit;
    swtchToken: TSwitch;
    tgBot: TTelegramBot;
    TgBotRecesiverUI1: TTgBotRecesiverUI;
    procedure tgBotReceiveError(ASender: TObject; AApiRequestException: EApiRequestException);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tgBotReceiveGeneralError(ASender: TObject; AException: Exception);
    procedure swtchTokenSwitch(Sender: TObject);
    procedure TgBotAsync1CallbackQuery(ASender: TObject; ACallbackQuery: TtgCallbackQuery);
    procedure TgBotAsync1Connect(Sender: TObject);
    procedure TgBotAsync1Disconnect(Sender: TObject);
    procedure TgBotAsync1InlineQuery(ASender: TObject; AInlineQuery: TtgInlineQuery);
    procedure TgBotAsync1Message(ASender: TObject; AMessage: ITgMessage);
    procedure TgBotAsync1InlineResultChosen(ASender: TObject; AChosenInlineResult: TtgChosenInlineResult);
  private
    { Private declarations }
    procedure WriteLine(const AValue: string);
    procedure SendInline(Msg: ITgMessage);
    procedure SendKeyboard(Msg: ITgMessage);
    procedure SendPhoto(Msg: ITgMessage);
    procedure SendRequest(Msg: ITgMessage);
    procedure SendQuest(Msg: ITgMessage);
    // parsing
    procedure ParseTextMessage(Msg: ITgMessage);
    procedure ParsePhotoMessage(Msg: ITgMessage);
    procedure ParseLocationMessage(Msg: ITgMessage);
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

uses
  System.IOUtils,
  TelegAPI.Helpers,
  TelegAPI.Types.Enums,
  TelegAPI.Types.ReplyMarkups,
  TelegAPI.Types.InlineQueryResults,
  TelegAPI.Types.InputMessageContents;
{$R *.fmx}

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TgBotRecesiverUI1.IsReceiving := False;
end;

procedure TMain.ParseLocationMessage(Msg: ITgMessage);
begin
  WriteLine('Location: ' + Msg.Location.Longitude.ToString + ' ' + Msg.Location.Latitude.ToString);
end;

procedure TMain.ParsePhotoMessage(Msg: ITgMessage);
begin
  if Msg.Photo[High(Msg.Photo)].CanDownload then
    WriteLine(Msg.Photo[High(Msg.Photo)].GetFileUrl(tgBot.Token))
  else
  begin
    WriteLine(tgBot.GetFile(Msg.Photo[High(Msg.Photo)].FileId).GetFileUrl(tgBot.Token));
  end;
end;

procedure TMain.ParseTextMessage(Msg: ITgMessage);
var
  usage: string;
begin
  WriteLine(Msg.From.Username + ': ' + Msg.Text);
  if Msg.IsCommand('/inline') then // send inline keyboard
  begin
    SendInline(Msg);
  end
  else if Msg.IsCommand('/keyboard') then // send custom keyboard
  begin
    SendKeyboard(Msg);
  end
  else if Msg.IsCommand('/photo') then // send custom keyboard
  begin
    SendPhoto(Msg);
  end
  else if Msg.IsCommand('/request') then // send custom keyboard
  begin
    SendRequest(Msg);
  end
  else if Msg.IsCommand('/quest') then // send custom keyboard
  begin
    SendQuest(Msg);
  end
  else if Msg.IsCommand('/help') then // send
  begin
    usage := 'Usage:' + #13#10 + //
      '/inline   - send inline keyboard' + #13#10 + //
      '/keyboard - send custom keyboard' + #13#10 + //
      '/photo    - send a photo' + #13#10 + //
      '/request  - request location or contact';
    tgBot.SendMessage(Msg.Chat.Id, usage, TtgParseMode.default, False, False, 0, TtgReplyKeyboardRemove.Create);
  end;
end;

procedure TMain.SendRequest(Msg: ITgMessage);
var
  kb: IReplyMarkup;
begin
  kb := TtgReplyKeyboardMarkup.Create([[
    { } TtgKeyboardButton.Create('Location', False, True),
    { } TtgKeyboardButton.Create('Contact', True, False)]]);
  tgBot.SendMessage(Msg.Chat.Id, 'Who or Where are you?', TtgParseMode.default, False, False, 0, kb);
end;

procedure TMain.swtchTokenSwitch(Sender: TObject);
begin
  tgBot.Token := edtToken.Text;
  if not tgBot.IsValidToken then
    raise ELoginCredentialError.Create('invalid token format');
  TgBotRecesiverUI1.IsReceiving := swtchToken.IsChecked;
end;

procedure TMain.TgBotAsync1CallbackQuery(ASender: TObject; ACallbackQuery: TtgCallbackQuery);
begin
  tgBot.AnswerCallbackQuery(ACallbackQuery.Id, 'Received ' + ACallbackQuery.Data);
end;

procedure TMain.TgBotAsync1Connect(Sender: TObject);
begin
  WriteLine('Bot connected');
 Caption := tgBot.GetMe.Username;
end;

procedure TMain.TgBotAsync1Disconnect(Sender: TObject);
begin
  WriteLine('Bot Disconnected');
end;

procedure TMain.TgBotAsync1InlineQuery(ASender: TObject; AInlineQuery: TtgInlineQuery);
var
  results: TArray<TtgInlineQueryResult>;
begin
  results := [TtgInlineQueryResultLocation.Create, TtgInlineQueryResultLocation.Create];
  with TtgInlineQueryResultLocation(results[0]) do
  begin
    ID := '1';
    Latitude := 40.7058316; // displayed result
    Longitude := -74.2581888;
    Title := 'New York';
    InputMessageContent := TtgInputLocationMessageContent.Create(40.7058316, -74.2581888);  // message if result is selected
  end;
  with TtgInlineQueryResultLocation(results[1]) do
  begin
    ID := '2';
    Latitude := 52.507629; // displayed result
    Longitude := 13.1449577;
    Title := 'Berlin';
    InputMessageContent := TtgInputLocationMessageContent.Create(52.507629, 13.1449577); // message if result is selected
  end;
  tgBot.AnswerInlineQuery(AInlineQuery.Id, results, 0, True);
end;

procedure TMain.TgBotAsync1InlineResultChosen(ASender: TObject; AChosenInlineResult: TtgChosenInlineResult);
begin
  WriteLine('Received choosen inline result: ' + AChosenInlineResult.ResultId);
end;

procedure TMain.TgBotAsync1Message(ASender: TObject; AMessage: ITgMessage);
begin
  case AMessage.&Type of
    TtgMessageType.TextMessage:
      ParseTextMessage(AMessage);
    TtgMessageType.PhotoMessage:
      ParsePhotoMessage(AMessage);
    TtgMessageType.LocationMessage:
      ParseLocationMessage(AMessage);
  end;
end;

procedure TMain.SendPhoto(Msg: ITgMessage);
const
  PATH_PHOTO = 'C:\Users\Public\Pictures\Sample Pictures\Tulips.jpg';
var
  LFile: TtgFileToSend;
begin
  tgBot.SendChatAction(Msg.Chat.Id, TtgSendChatAction.UploadPhoto);
  if not TFile.Exists(PATH_PHOTO) then
    WriteLine('Change path to photo in metod: TMain.SendPhoto');
  LFile := TtgFileToSend.Create(PATH_PHOTO);
  try
    tgBot.SendPhoto(Msg.Chat.Id, LFile, 'Nice Picture');
  finally
    LFile.Free;
  end;
end;

procedure TMain.SendInline;
var
  keyboard: TtgInlineKeyboardMarkup;
begin
  tgBot.SendChatAction(Msg.Chat.Id, TtgSendChatAction.Typing);
  keyboard := TtgInlineKeyboardMarkup.Create([
    { first row }
    [TtgInlineKeyboardButton.Create('1.1', '1'), TtgInlineKeyboardButton.Create('1.2', '2')],
    { second row }
    [TtgInlineKeyboardButton.Create('2.1', '3'), TtgInlineKeyboardButton.Create('2.2', '4')]]);
  Sleep(500); // simulate longer running task
  tgBot.SendMessage(Msg.Chat.Id, 'Choose', TtgParseMode.default, False, False, 0, keyboard);
end;

procedure TMain.SendKeyboard(Msg: ITgMessage);
var
  keyboard: IReplyMarkup;
begin
  keyboard := TtgReplyKeyboardMarkup.Create(False, True);
  with keyboard as TtgReplyKeyboardMarkup do
  begin
    { first row }
    AddRow([TtgKeyboardButton.Create('1.1'), TtgKeyboardButton.Create('1.2')]);
    { second row }
    AddRow([TtgKeyboardButton.Create('2.1'), TtgKeyboardButton.Create('2.2')]);
    AddRow([TtgKeyboardButton.Create('Contact', True, False), TtgKeyboardButton.Create('Location', False, True)]);
  end;
  tgBot.SendMessage(Msg.Chat.Id, 'Choose', TtgParseMode.default, False, False, 0, keyboard);
end;

procedure TMain.SendQuest(Msg: TTgMessage);
var
  keyboard: IReplyMarkup;
begin
  keyboard := TtgReplyKeyboardMarkup.Create([
    { first row }
    [TtgKeyboardButton.Create('1.1'), TtgKeyboardButton.Create('1.2')],
    { second row }
    [TtgKeyboardButton.Create('2.1'), TtgKeyboardButton.Create('2.2')]], False);
  tgBot.SendMessage(Msg.Chat.Id, 'Выбери:', TtgParseMode.default, False, False, 0, keyboard);
end;

procedure TMain.tgBotReceiveError(ASender: TObject; AApiRequestException: EApiRequestException);
begin
  WriteLine(AApiRequestException.ToString);
end;

procedure TMain.tgBotReceiveGeneralError(ASender: TObject; AException: Exception);
begin
  WriteLine(AException.ToString);
end;

procedure TMain.WriteLine(const AValue: string);
begin
  mmoLog.Lines.Add(AValue);
  mmoLog.ScrollBy(0, mmoLog.ContentBounds.Bottom, False);
end;

initialization
  ReportMemoryLeaksOnShutdown := True;

finalization
  if ReportMemoryLeaksOnShutdown then
    CheckSynchronize();

end.

