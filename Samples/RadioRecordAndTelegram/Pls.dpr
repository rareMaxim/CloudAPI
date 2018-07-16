 program Pls;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  RadioRecord,
  RadioRecord.Types,
  System.Classes,
  CloudAPI.Request,
  CloudAPI.Exception,
  System.SysUtils,
  System.Zip,
  TelegAPI.Receiver.Console,
  TelegAPI.Bot,
  TelegAPI.Types.ReplyMarkups,
  TelegAPI.Types,
  TelegAPI.Types.Enums,
  TelegAPI.Bot.Impl;

const
  Token = '606359138:AAGUqIymgeLstmDafIjculG9p5zjSg3s_qk';

function CreatePls(const ABitrate: Integer; var ACount: Integer): string;
const
  M3U_ITEM = '#EXTINF:-1,%s'#13#10'%s';
var
  RR: TRadioRecord;
  Station: IrrStation;
  Plt: TStringList;
  LStream: string;
begin
  ACount := 0;
  RR := TRadioRecord.Create(nil);
  Plt := TStringList.Create;
  try
    Plt.Add('#EXTM3U');
    for Station in RR.GetStations do
    begin
      Inc(ACount);
      case ABitrate of
        32:
          LStream := Station.stream_32;
        64:
          LStream := Station.stream_64;
        128:
          LStream := Station.stream_128;
        320:
          LStream := Station.stream_320;
      else
        raise Exception.Create('Unknown bitrate');
      end;
      Plt.Add(Format(M3U_ITEM, [Station.title, LStream]));
    end;
    Result := ExtractFilePath(ParamStr(0));
    Result := Result + 'radiorecord' + ABitrate.ToString + '.m3u';
    Plt.SaveToFile(Result);
  finally
    RR.Free;
    Plt.Free;
  end;
end;

function CreateZip(var ACount: Integer): string;
var
  LZip: TZipFile;
begin
  LZip := TZipFile.Create;
  try
    Result := ExtractFilePath(ParamStr(0)) + 'by @radio_record_bot.zip';
    LZip.Open(Result, TZipMode.zmWrite);
    LZip.Add(CreatePls(32, ACount));
    LZip.Add(CreatePls(64, ACount));
    LZip.Add(CreatePls(128, ACount));
    LZip.Add(CreatePls(320, ACount));
    LZip.Close;
  finally
    LZip.Free;
  end;
end;

function CreateFile(const AData: string; var ACount: Integer): string;
begin
  if AData = 'zip' then
    Result := CreateZip(ACount)
  else
    Result := CreatePls(AData.ToInteger, ACount);
end;

function GetKbBitrate: IReplyMarkup;
var
  Kb: TtgInlineKeyboardMarkup;
begin
  Kb := TtgInlineKeyboardMarkup.Create;
  try
    Kb.AddRow([TtgInlineKeyboardButton.Create('3️⃣2️⃣0️⃣', '320'),
      TtgInlineKeyboardButton.Create('1️⃣2️⃣8️⃣', '128'), TtgInlineKeyboardButton.Create
      ('6️⃣4️⃣ ', '64'), TtgInlineKeyboardButton.Create('3️⃣2️⃣ ', '32')]);
    Kb.AddRow([TtgInlineKeyboardButton.Create('🗂 ZIP', 'zip')]);
    Result := Kb;
  finally
    // Kb.Free;
  end;
end;

procedure Main;
var
  LBot: TTelegramBot;
  LReceiver: TtgReceiverConsole;
  LStop: string;
begin
  LBot := TTelegramBot.Create(Token);
  LReceiver := TtgReceiverConsole.Create(LBot);
  try
    LBot.OnError :=
      procedure(Sender: TObject; E: ECloudApiException)
      begin
        if Assigned(E) then
          Writeln(E.ToString);
      end;
    LReceiver.OnStart :=
      procedure
      begin
        Writeln('started');
      end;
    LReceiver.OnStop :=
      procedure
      begin
        Writeln('stoped');
      end;
    LReceiver.OnCallbackQuery :=
      procedure(Qr: ItgCallbackQuery)
      var
        LCount: Integer;
      begin
        Write('Создаю плейлист с качеством потока: ' + Qr.Data + '... ');
        LBot.AnswerCallbackQuery(Qr.ID, 'Секундочку...');
        LBot.SendChatAction(Qr.From.ID, TtgSendChatAction.Upload_document);
        Writeln(Qr.Data);
        LBot.SendDocument(Qr.From.ID, CreateFile(Qr.Data, LCount), 'Станций: ' + LCount.ToString);
        LBot.AnswerCallbackQuery(Qr.ID);
        Writeln('Готово');
      end;
    LReceiver.OnMessage :=
      procedure(AMessage: ITgMessage)
      begin
        if AMessage.Text = '/start' then
        begin
          LBot.SendMessage(AMessage.From.ID, 'Выбери качество плейлиста: ',
            TtgParseMode.Markdown, False, False, 0, GetKbBitrate);
        end;
        Writeln(AMessage.From.ID, ': ', AMessage.Text);
        LBot.SendMessage(AMessage.From.ID, AMessage.Text);
      end;
    Writeln('Bot nick: ', LBot.GetMe.Username);
    LReceiver.IsActive := True;
    while LStop.ToLower.Trim <> 'exit' do
    begin
      Readln(LStop);
      if LStop.ToLower.Trim = 'stop' then
        LReceiver.IsActive := False
      else if LStop.ToLower.Trim = 'start' then
        LReceiver.IsActive := True;
    end;
  finally
    LBot.Free;
    LReceiver.Free;
  end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    ReportMemoryLeaksOnShutdown := True;
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.message);
  end;

end.

