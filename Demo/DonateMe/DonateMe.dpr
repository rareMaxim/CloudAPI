program DonateMe;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  TelegAPI.Bot.Console,
  TelegAPI.Types,
  TelegAPI.Helpers,
  System.SysUtils;

procedure Main;
var
  LBot: TTelegramBotConsole;
  LStop, LToken: string;
begin
  LBot := TTelegramBotConsole.Create('');

  repeat
    Write('Paste valid token from @botFather: ');
    Readln(LToken);
    LBot.Token := LToken;
  until (LBot.IsValidToken);

  try
    LBot.OnConnect :=
      procedure
      begin
        Writeln('Connected');
      end;
    LBot.OnMessage :=
      procedure(AMessage: TTgMessage)
      begin
        Writeln(AMessage.From.Username, ': ', AMessage.Text);
        if AMessage.IsCommand('/start') then
          LBot.SendInvoice(
            AMessage.Chat.ID, 
            'Title', 
            'Description', 
            'Payload', 
            '381764678:TEST:1848', 
            'start_parameter', 
            'RUB', 
            [
              TtgLabeledPrice.Create('RUB', 5839)
            ], 
            'http://minionomaniya.ru/wp-content/uploads/2016/01/%D0%9A%D0%B5%D0%B2%D0%B8%D0%BD.jpg'
)
      end;
    with LBot.GetMe do
    begin
      Writeln('Bot nick: ', Username);
      Free;
    end;
    LBot.IsReceiving := True;
    while LStop.ToLower.Trim <> 'exit' do
      Readln(LStop);
  finally
    LBot.Free;
  end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

