unit TelegaPi.Factory;

interface

uses
  CoreAPI,
  TelegaPi.Bot;

type
  TtgFactory = class
    class function CreateTelegram(const AToken: string = ''): ITelegramBot;
    class function RequestAPI: ItgRequestAPI;
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

class function TtgFactory.RequestAPI: ItgRequestAPI;
begin
  Result := TtgRequestAPI.Create;
end;

end.

