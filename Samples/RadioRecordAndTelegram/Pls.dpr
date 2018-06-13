program Pls;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  RadioRecord,
  RadioRecord.Types,
  System.Classes,
  CloudAPI.Logger,
  CloudAPI.Request,
  System.SysUtils,
  TelegAPI.Receiver.Console,
  TelegAPI.Bot,
  TelegAPI.Types.ReplyMarkups,
  TelegAPI.Types,
  TelegAPI.Types.Enums,
  TelegAPI.Bot.Impl,
  TelegAPI.Logger.Old;

const
  Token = '606359138:AAGUqIymgeLstmDafIjculG9p5zjSg3s_qk';

function CreatePls(const ABitrate: Integer): string;
const
  M3U_ITEM = '#EXTINF:-1,%s'#13#10'%s';
var
  RR: TRadioRecord;
  Station: IrrStation;
  Plt: TStringList;
  LStream: string;
begin
  Write('Создаю плейлист с качеством потока: ' + ABitrate.ToString + '... ');
  RR := TRadioRecord.Create(nil);
  Plt := TStringList.Create;
  try
    Plt.Add('#EXTM3U');
    for Station in RR.GetStations do
    begin
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
    Writeln('Готово');
  finally
    RR.Free;
    Plt.Free;
  end;
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
    Result := Kb;
  finally
  //  Kb.Free;
  end;
end;

procedure Main;
var
  LBot: ITelegramBot;
  LReceiver: TtgReceiverConsole;
  LExcp: TtgExceptionManagerConsole;
  LStop: string;
begin
  LBot := TTelegramBot.Create(Token);
  LReceiver := TtgReceiverConsole.Create(LBot);
  (LBot as TTelegramBot).Logger := TtgExceptionManagerConsole.Create(nil);
  try
    LExcp := (LBot as TTelegramBot).Logger as TtgExceptionManagerConsole;
    LExcp.OnLog :=
      procedure(level: TLogLevel; msg: string; e: Exception)
      begin
        if level >= TLogLevel.Error then
        begin
          if Assigned(e) then
            Writeln('[' + e.ToString + '] ' + msg)
          else
            Writeln(msg);
        end;
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
       begin
        LBot.AnswerCallbackQuery(Qr.ID, 'Секундочку...');
        LBot.SendChatAction(Qr.From.ID, TtgSendChatAction.Upload_document);
        Writeln(Qr.Data);
        LBot.SendDocument(Qr.From.ID, TFileToSend.FromFile(CreatePls(Qr.Data.ToInteger)));
        LBot.AnswerCallbackQuery(Qr.ID);
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
    LReceiver.Free;
  end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Main;
  except
    on e: Exception do
      Writeln(e.ClassName, ': ', e.message);
  end;

end.

