unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, TelegAPI.Receiver.UI,
  TelegAPI.UpdateParser, TelegAPI.Receiver.Base, TelegAPI.Receiver.Service,
  CloudAPI.BaseComponent, TelegAPI.Bot, Telegapi.types, Vcl.Samples.Spin,
  Vcl.StdCtrls, CloudAPI.Exception,  CloudAPI.Utils.Json, Data.DB, MemDS,
  DBAccess, Uni, UniProvider, MySQLUniProvider;

type
  TForm1 = class(TForm)
    TelegramBot1: TTelegramBot;
    tgReceiverUI1: TtgReceiverUI;
    Memo1: TMemo;
    btnStart: TButton;
    btnStop: TButton;
    spinInterval: TSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    edChatId: TEdit;
    edChatUserName: TEdit;
    edChatMsgText: TEdit;
    btnChatSendMsg: TButton;
    edUserId: TEdit;
    edUserMsgText: TEdit;
    btnUserSend: TButton;
    UniConnection1: TUniConnection;
    MySQLUniProvider1: TMySQLUniProvider;
    qMed: TUniQuery;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure tgReceiverUI1Message(ASender: TObject; AMessage: ITgMessage);
    procedure tgReceiverUI1Update(ASender: TObject; AUpdate: ItgUpdate);
    procedure tgReceiverUI1Updates(ASender: TObject;
      AUpdates: TArray<TelegAPi.Types.ItgUpdate>);
    procedure TelegramBot1Error(ASender: TObject;
      const Exception: ECloudApiException);
    procedure tgReceiverUI1ChannelPost(ASender: TObject; AMessage: ITgMessage);
    procedure btnChatSendMsgClick(Sender: TObject);
    procedure TelegramBot1SendData(ASender: TObject; const AUrl, AData: string);
    procedure btnUserSendClick(Sender: TObject);
    procedure TelegramBot1ReceiveRawData(ASender: TObject; const AData: string);
    procedure spinIntervalChange(Sender: TObject);
  private
    const
      constCmdStart = '/start';
      constCmdMe = '/me';
      constCmdBuy = '/купить';
      constBuy = 'купить';
      constCheckPone = 'тел';

    procedure ReceivePhoto(Photos: TArray<TelegAPi.Types.ItgPhotoSize>);
    procedure ChatSendMsg(UserId: int64; const Msg: string);
    procedure ReceiveText(AUpdate: ItgUpdate);
    function  ParseCmdBuy(const sText: string): string;
    function ParseBuy(const sText: string): string;
    function GetMedNameByTel(const Tel: string): string;
    function OnlyDec(const Inp: string): string;
    function GetConnectString: string;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  TelegaPi.Types.Enums, System.StrUtils, System.IniFiles, uParseDIRegEx;

{$R *.dfm}

procedure TForm1.ChatSendMsg(UserId: int64; const Msg: string);
Var
 UserLink: TtgUserLink;
begin
  if Msg.IsEmpty then exit;


  if UserID = 0 then
  begin
    Memo1.Lines.Add('SendMsg: ' + edChatId.Text + ' is not valid integer value');
    exit;
  end;

  UserLink.ID := UserID;

  TelegramBot1.SendMessage(UserLink, Msg);
end;


procedure TForm1.btnChatSendMsgClick(Sender: TObject);
Var
 UserLink: TtgUserLink;
begin
  UserLink.ID := StrToInt64Def(edChatId.Text, 0);
  UserLink.Username := edChatUserName.Text;

  if UserLink.ID = 0 then
  begin
    Memo1.Lines.Add('SendMsg: ' + edChatId.Text + ' is not valid integer value');
    exit;
  end;

  TelegramBot1.SendMessage(UserLink, edChatMsgText.Text);
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  tgReceiverUI1.Start;
  tgReceiverUI1.PollingInterval := spinInterval.Value;

  Memo1.Lines.Add('СТАРТ');
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  tgReceiverUI1.Stop;
  Memo1.Lines.Add('СТОП');
end;

procedure TForm1.btnUserSendClick(Sender: TObject);
Var
 UserLink: TtgUserLink;
begin
  UserLink.ID := StrToInt64Def(edUserId.Text, 0);

  if UserLink.ID = 0 then
  begin
    Memo1.Lines.Add('UserSendMsg: ' + edUserId.Text + ' is not valid integer value');
    exit;
  end;

  TelegramBot1.SendMessage(UserLink, edUserMsgText.Text);
end;

procedure TForm1.spinIntervalChange(Sender: TObject);
begin
  tgReceiverUI1.PollingInterval := spinInterval.Value;
end;

procedure TForm1.TelegramBot1Error(ASender: TObject;
  const Exception: ECloudApiException);
begin
  Memo1.Lines.Add('TelegramBot OnError, Exception: ' + Exception.ToString);
  Memo1.Lines.Add('TelegramBot OnError ------------------');
end;

procedure TForm1.TelegramBot1ReceiveRawData(ASender: TObject;
  const AData: string);
begin
//  Memo1.Lines.Add('TelegramBot OnReceiveRawData, ' + AData);
//  Memo1.Lines.Add('TelegramBot OnReceiveRawData ------------------');
end;

procedure TForm1.TelegramBot1SendData(ASender: TObject; const AUrl,
  AData: string);
begin
//  Memo1.Lines.Add('OnSendData, AUrl:' +  AUrl);
//  Memo1.Lines.Add('OnSendData, AData:' +  AData);
//  Memo1.Lines.Add('TelegramBot OnSendData ------------------');
end;

procedure TForm1.tgReceiverUI1ChannelPost(ASender: TObject; AMessage: ITgMessage);
begin
  Memo1.Lines.Add('OnChannelPost RAW:' + (AMessage as TBaseJson).AsJson);
  Memo1.Lines.Add('');

  Memo1.Lines.Add('OnChannelPost, Text:' + AMessage.Text);
  Memo1.Lines.Add('');

  if Assigned(AMessage.Chat) then
  begin
    Memo1.Lines.Add('OnChannelPost, From:' + AMessage.Chat.Title);
    edChatId.Text       := AMessage.Chat.ID.ToString;
    //edChatUserName.Text := AMessage.Chat.Username;
  end
  else
    Memo1.Lines.Add('OnChannelPost, From is not assigned');

// новый пользователь
  if AMessage.Text.ToLowerInvariant.Contains(constCmdStart) then
    ChatSendMsg(AMessage.Chat.ID, 'Приветствуем тебя в нашем чате');


  Memo1.Lines.Add('');
  Memo1.Lines.Add('OnChannelPost -----------------');
end;


procedure TForm1.tgReceiverUI1Message(ASender: TObject; AMessage: ITgMessage);
begin
  Memo1.Lines.Add('OnMessage, ' + AMessage.Text );
  Memo1.Lines.Add('');

  Memo1.Lines.Add('OnMessage, UserID' + AMessage.From.ID.ToString);
  edUserId.Text := AMessage.From.ID.ToString;
  //edUserName.Text := AMessage.From.Username;

  Memo1.Lines.Add('OnMessage --------------------');
end;

procedure TForm1.tgReceiverUI1Update(ASender: TObject; AUpdate: ItgUpdate);
begin

  Memo1.Lines.Add('OnUpdate, AUpdate.ID: ' + AUpdate.ID.ToString );
  Memo1.Lines.Add('');

  if not Assigned(AUpdate.message) then
  begin
    Memo1.Lines.Add('OnUpdate, AUpdate.message not assigned')
  end
  else
  begin
    Memo1.Lines.Add('OnUpdate: ' + AUpdate.message.Text);

    case AUpdate.message.&Type of
      TtgMessageType.Photo: ReceivePhoto(AUpdate.message.Photo);
      TtgMessageType.Text : ReceiveText(AUpdate);

    end;


  end;

  Memo1.Lines.Add('');
  Memo1.Lines.Add('OnUpdate RAW:' + (AUpdate as TBaseJson).AsJson);
  Memo1.Lines.Add('');
  Memo1.Lines.Add('OnUpdate ------------------');
end;


procedure TForm1.ReceiveText(AUpdate: ItgUpdate);
Var
 s: string;

begin
// мои данные
  if not Assigned(AUpdate) then exit;
  if not Assigned(AUpdate.message) then exit;

  if Assigned(AUpdate.Message.From) then
  begin

    if AUpdate.Message.Text.ToLowerInvariant.Contains(constCmdMe) then
      ChatSendMsg(AUpdate.Message.From.ID,
                 'ID: ' + AUpdate.Message.From.ID.ToString + sLineBreak +
                 'Имя: ' + AUpdate.Message.From.FirstName + sLineBreak +
                 'Фамилия: ' + AUpdate.Message.From.LastName + sLineBreak +
                 'Логин: ' + AUpdate.Message.From.Username + sLineBreak +
                 'Бот: ' + ifthen(AUpdate.Message.From.IsBot, 'Да', 'Нет')
                  );

    if AUpdate.Message.Text.StartsWith(constCmdStart) then
    begin
      ChatSendMsg(AUpdate.Message.From.ID, 'Привет, ' + AUpdate.Message.From.FirstName + ' введи запрос...');
      ChatSendMsg(AUpdate.Message.From.ID, 'например, купить квартиру в харькове алексеевка 1к цена 12-15 этаж 3-6 608мр');
    end;

    if AUpdate.Message.Text.StartsWith(constCmdBuy) then
      ChatSendMsg(AUpdate.Message.From.ID, ParseCmdBuy(AUpdate.Message.Text));

    if AUpdate.Message.Text.ToLowerInvariant.Contains(constBuy) then
      ChatSendMsg(AUpdate.Message.From.ID, ParseBuy(AUpdate.Message.Text));

    s := AUpdate.Message.Text;
    s := uParseDIRegEx.GetLastMatch(s, '\d *?[\d-) ]{5,15}').Match;
    if not s.IsEmpty then
      ChatSendMsg(AUpdate.Message.From.ID, GetMedNameByTel(s));



  end;
end;

function TForm1.ParseCmdBuy(const sText: string): string;
Var
  s: string;
begin
  s := Copy(sText, constCmdBuy.Length + 1, sText.Length - constCmdBuy.Length);

  if s.IsEmpty then
    Exit('Вы не ввели запрос');

  result := 'Вы ввели запрос: ' + s;
end;

function TForm1.ParseBuy(const sText: string): string;
Var
  s: string;
begin
  s := sText;

  if s.StartsWith(constCmdBuy) then exit;// для ParseCmdBuy


  if s.IsEmpty then
    Exit('Вы не ввели запрос');

  result := 'Вы ввели запрос: ' + s;
end;

procedure TForm1.ReceivePhoto(Photos: TArray<TelegAPi.Types.ItgPhotoSize>);
Var
  PhotoSize: TelegAPi.Types.ItgPhotoSize;
  tgFile: ItgFile;
  FileUrl: string;
begin
  Memo1.Lines.Add('ReceivePhoto');

  for PhotoSize in Photos do
  begin
    tgFile := TelegramBot1.GetFile(PhotoSize.FileId);
    if tgFile.CanDownload then
    begin
      FileUrl := tgFile.GetFileUrl(TelegramBot1.Token);
      Memo1.Lines.Add('ReceivePhoto, FileUrl: ' + FileUrl);


    end;

  end;

  Memo1.Lines.Add('ReceivePhoto ------------------');
end;


procedure TForm1.tgReceiverUI1Updates(ASender: TObject;
  AUpdates: TArray<TelegAPi.Types.ItgUpdate>);
begin
  Memo1.Lines.Add('OnUpdates');
  Memo1.Lines.Add('OnUpdates ------------------');
end;


function TForm1.OnlyDec(const Inp: string): string;
Var
 k:integer;
begin
  Result := '';
  for k := 1 to Length(Inp) do
   if CharInSet(Inp[k], ['0'..'9']) then
     Result := Result + Inp[k];
end;

function TForm1.GetConnectString: string;
Var
 sAppPath2: string;
begin
  sAppPath2 := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  with tinifile.Create( sAppPath2 + 'connect.ini') do
  begin
    result := ReadString('DB', 'ConnectString', 'Provider Name=MySQL;User ID=kvartal_ekr2;Password=Kvartal-000;Use Unicode=True;Data Source=kvartal.ua;Database=kvartal_ekr;Port=3306;Compress=True;Login Prompt=False');
    free;
  end;

end;

function TForm1.GetMedNameByTel(const Tel: string): string;
Var
  sTel: string;
begin
  qMed.close;
  if Tel.isempty then exit;

  sTel := OnlyDec(Tel);
  UniConnection1.Disconnect;
  UniConnection1.ConnectString := GetConnectString;
  UniConnection1.Connect;


  if not UniConnection1.Connected then
    exit('Ошибка. Нет подключения к базе.');


//если номер в локальном форме без 38
  if copy(sTel, 1, 2) <> '38' then
  begin
    //передаем номер в двух форматах
    qMed.ParamByName('num_phone').AsString  := '38' + sTel;
    qMed.ParamByName('num_phone2').AsString := sTel;
  end;

//если номер в международном формате
  if copy(sTel, 1, 2) = '38' then
  begin
    //передаем номер в двух форматах
    qMed.ParamByName('num_phone').AsString := sTel;
    qMed.ParamByName('num_phone2').AsString := copy(sTel, 3, 10);// 380687379266
  end;

  try
    try
      qMed.Open;
    except
      on e:exception do
      begin
        result := 'Ошибка: ' + e.Message;
      end;

    end;//try

    if qMed.RecordCount > 0 then
      result := qMed.FieldByName('name').AsString
    else
      result := 'не посредник';


  finally
    qMed.close;
    UniConnection1.Disconnect;
  end;

end;
end.
