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
  TelegAPI.Exceptions,
  FMX.Edit,
  FMX.StdCtrls,
  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Layouts;

type
  TMain = class(TForm)
    tgBot: TTelegramBot;
    mmoLog: TMemo;
    Layout1: TLayout;
    lblToken: TLabel;
    edtToken: TEdit;
    swtchToken: TSwitch;
    procedure tgBotInlineResultChosen(ASender: TObject; AChosenInlineResult: TtgChosenInlineResult);
    procedure tgBotInlineQuery(ASender: TObject; AInlineQuery: TtgInlineQuery);
    procedure tgBotMessage(ASender: TObject; AMessage: TTgMessage);
    procedure tgBotCallbackQuery(ASender: TObject; ACallbackQuery: TtgCallbackQuery);
    procedure tgBotReceiveError(ASender: TObject; AApiRequestException: EApiRequestException);
    procedure tgBotConnect(Sender: TObject);
    procedure tgBotDisconnect(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tgBotReceiveGeneralError(ASender: TObject; AException: Exception);
    procedure swtchTokenSwitch(Sender: TObject);
  private
    { Private declarations }
    procedure WriteLine(const AValue: string);
    procedure SendInline(Msg: TTgMessage);
    procedure SendKeyboard(Msg: TTgMessage);
    procedure SendPhoto(Msg: TTgMessage);
    procedure SendRequest(Msg: TTgMessage);
    procedure SendQuest(Msg: TTgMessage);
    // parsing
    procedure ParseTextMessage(Msg: TTgMessage);
    procedure ParsePhotoMessage(Msg: TTgMessage);
    procedure ParseLocationMessage(Msg: TTgMessage);
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
  tgBot.IsReceiving := False;
end;

procedure TMain.ParseLocationMessage(Msg: TTgMessage);
begin
  WriteLine('Location: ' + Msg.Location.Longitude.ToString + ' ' + Msg.Location.Latitude.ToString);
end;

procedure TMain.ParsePhotoMessage(Msg: TTgMessage);
var
  LFile: TtgFile;
begin
  if Msg.Photo.Last.CanDownload then
    WriteLine(Msg.Photo.Last.GetFileUrl(tgBot.Token))
  else
  begin
    LFile := tgBot.GetFile(Msg.Photo.Last.FileId);
    WriteLine(LFile.GetFileUrl(tgBot.Token));
    LFile.Free;
  end;
end;

procedure TMain.ParseTextMessage(Msg: TTgMessage);
var
  usage: string;
begin
  WriteLine(Msg.From.Username + ': ' + Msg.Text);
  if Msg.Text.StartsWith('/inline') then // send inline keyboard
  begin
    SendInline(Msg);
  end
  else if Msg.Text.StartsWith('/keyboard') then // send custom keyboard
  begin
    SendKeyboard(Msg);
  end
  else if Msg.Text.StartsWith('/photo') then // send custom keyboard
  begin
    SendPhoto(Msg);
  end
  else if Msg.Text.StartsWith('/request') then // send custom keyboard
  begin
    SendRequest(Msg);
  end
  else if Msg.Text.StartsWith('/quest') then // send custom keyboard
  begin
    SendQuest(Msg);
  end
  else if Msg.Text.StartsWith('/help') then // send
  begin
    usage := 'Usage:' + #13#10 +    //
      '/inline   - send inline keyboard' + #13#10 +    //
      '/keyboard - send custom keyboard' + #13#10 +   //
      '/photo    - send a photo' + #13#10 +       //
      '/request  - request location or contact';
    tgBot.SendMessage(Msg.Chat.Id, usage, TtgParseMode.default, False, False, 0, TtgReplyKeyboardRemove.Create).Free;
  end;
end;

procedure TMain.SendRequest(Msg: TTgMessage);
var
  kb: IReplyMarkup;
begin
  kb := TtgReplyKeyboardMarkup.Create([[
  {} TtgKeyboardButton.Create('Location', False, True),
  {} TtgKeyboardButton.Create('Contact', True, False)]]);
  tgBot.SendMessage(Msg.Chat.Id, 'Who or Where are you?', TtgParseMode.default, False, False, 0, kb).Free;
end;

procedure TMain.swtchTokenSwitch(Sender: TObject);
begin
  tgBot.Token := edtToken.Text;
  if not tgBot.IsValidToken then
    raise ELoginCredentialError.Create('invalid token format');
  tgBot.IsReceiving := swtchToken.IsChecked;
end;

procedure TMain.SendPhoto(Msg: TTgMessage);
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
    tgBot.SendPhoto(Msg.Chat.ID, LFile, 'Nice Picture').Free;
  finally
    LFile.free;
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
  tgBot.SendMessage(Msg.Chat.Id, 'Choose', TtgParseMode.default, False, False, 0, keyboard).Free;
end;

procedure TMain.SendKeyboard(Msg: TTgMessage);
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
  tgBot.SendMessage(Msg.Chat.Id, 'Choose', TtgParseMode.default, False, False, 0, keyboard).Free;
end;

procedure TMain.tgBotCallbackQuery(ASender: TObject; ACallbackQuery: TtgCallbackQuery);
begin
  tgBot.AnswerCallbackQuery(ACallbackQuery.Id, 'Received ' + ACallbackQuery.Data);
end;

procedure TMain.tgBotConnect(Sender: TObject);
var
  LMe: TtgUser;
begin
  WriteLine('Bot connected');
  try
    LMe := tgBot.GetMe;
    if Assigned(LMe) then
    begin
      Caption := LMe.Username;
    end;
  finally
    FreeAndNil(LMe);
  end;
end;

procedure TMain.tgBotDisconnect(Sender: TObject);
begin
  WriteLine('Bot Disconnected');
end;

procedure TMain.tgBotInlineQuery(ASender: TObject; AInlineQuery: TtgInlineQuery);
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
    InputMessageContent := TtgInputLocationMessageContent.Create;
    // message if result is selected
    TtgInputLocationMessageContent(InputMessageContent).Latitude := 40.7058316;
    TtgInputLocationMessageContent(InputMessageContent).Longitude := -74.2581888;
  end;
  with TtgInlineQueryResultLocation(results[1]) do
  begin
    ID := '2';
    Latitude := 52.507629; // displayed result
    Longitude := 13.1449577;
    Title := 'Berlin';
    InputMessageContent := TtgInputLocationMessageContent.Create;
    // message if result is selected
    TtgInputLocationMessageContent(InputMessageContent).Latitude := 52.507629;
    TtgInputLocationMessageContent(InputMessageContent).Longitude := 13.1449577;
  end;
  tgBot.AnswerInlineQuery(AInlineQuery.Id, results, 0, True);
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
  tgBot.SendMessage(Msg.Chat.Id, 'Выбери:', TtgParseMode.default, False, False, 0, keyboard).Free;
end;

procedure TMain.tgBotInlineResultChosen(ASender: TObject; AChosenInlineResult: TtgChosenInlineResult);
begin
  WriteLine('Received choosen inline result: ' + AChosenInlineResult.ResultId);
end;

procedure TMain.tgBotMessage(ASender: TObject; AMessage: TTgMessage);
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

