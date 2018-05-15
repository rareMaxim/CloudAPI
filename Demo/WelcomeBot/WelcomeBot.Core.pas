unit WelcomeBot.Core;

interface

uses
  CrossUrl.HttpClient,
  TelegAPI.Bot,
  TelegAPI.Receiver.Console,
  TelegAPI.Exceptions,
  WelcomeBot.Config;

type
  TWelcomeBot = class
  private
    FConfig: TwbConfig;
    FConfigPath: string;
    FHttpCore: IcuHttpClient;
    FExcept: ItgExceptionHandler;
    FBot: ITelegramBot;
    FBotRec: TtgReceiverConsole;
  public
    constructor Create(const APathConfig: string);
    destructor Destroy; override;
    function Start: Boolean;
  published
    property Config: TwbConfig read FConfig write FConfig;
  end;

implementation

uses
  CrossUrl.SystemNet.HttpClient,
  TelegAPI.Bot.Impl,
  TelegAPI.Utils.Json;

{ TWelcomeBot }

destructor TWelcomeBot.Destroy;
begin

  TJsonUtils.ObjectToFile(FConfig, FConfigPath);
  FConfig.Free;
  inherited;
end;

function TWelcomeBot.Start: Boolean;
begin

end;

{ TWelcomeBot }

constructor TWelcomeBot.Create(const APathConfig: string);
begin
  FConfigPath := APathConfig;
  FConfig := TJsonUtils.FileToObject<TwbConfig>(FConfigPath);
  FHttpCore := TcuHttpClientSysNet.Create(nil);
  FExcept := TtgExceptionManagerConsole.Create(nil);
  FBot := TTelegramBot.Create(FConfig.Token);
  FBotRec := TtgReceiverConsole.Create(FBot);
end;

end.

