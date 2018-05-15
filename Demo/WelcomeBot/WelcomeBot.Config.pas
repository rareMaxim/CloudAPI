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

implementation

uses
  System.IOUtils,
  System.SysUtils;

function ConfigPath: string;
const
  CONFIG_NAME = 'config.json';
begin
  Result := ExtractFilePath(ParamStr(0)) + CONFIG_NAME;
end;

end.

