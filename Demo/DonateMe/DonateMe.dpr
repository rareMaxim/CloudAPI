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
        if AMessage.IsCommand('/donate') then
          LBot.SendInvoice(AMessage.Chat.ID, 'Поддержать автора', 'Кинуть автору на мяско', 'Payload', '381764678:TEST:1848', 'start_parameter', 'RUB', [TtgLabeledPrice.Create('label', 6000)], 'http://assets.fridge.menu/i/good/photo/55/03/5c/21/52/75/52/18/d7/cd/24/00/full_551a83d152755296f1696900.jpg', 120, 120, 120, True);
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

