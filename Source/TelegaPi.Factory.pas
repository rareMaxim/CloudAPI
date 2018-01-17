unit TelegaPi.Factory;
{$I config.inc}

interface

uses
  TelegaPi.Bot;

type
  TtgFactory = class
    class function CreateTelegram(const AToken: string = ''): ITelegramBot;
  end;

implementation

uses
  TelegaPi.Bot.Impl;

{ TtgFactory }

class function TtgFactory.CreateTelegram(const AToken: string): ITelegramBot;
begin
  Result := TTelegramBot.Create(nil);
  Result.Token := AToken;
end;

end.

