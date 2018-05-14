unit WelcomeBot.Config;

interface

type
  TwbConfig = class
  private
    FToken: string;
    FWelcomeText: string;
  public
    property WelcomeText: string read FWelcomeText write FWelcomeText;
    property Token: string read FToken write FToken;
  end;

var
  BOT_CONF: TwbConfig;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  TelegAPI.Utils.Json;

function ConfigPath: string;
const
  CONFIG_NAME = 'config.json';
begin
  Result := ExtractFilePath(ParamStr(0)) + CONFIG_NAME;
end;

initialization

BOT_CONF := TJsonUtils.FileToObject<TwbConfig>(ConfigPath);

finalization

TJsonUtils.ObjectToFile(BOT_CONF, ConfigPath);

end.
