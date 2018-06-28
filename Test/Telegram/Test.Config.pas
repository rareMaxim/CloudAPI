unit Test.Config;

interface

type
  TTestConfig = class
  private
    FToken: string;
    FUserId: Int64;
    FChannel: string;
  public
    property Token: string read Ftoken write Ftoken;
    property UserId: Int64 read FUserId write FUserId;
    property Channel: string read FChannel write FChannel;
  end;

function TestConfig: TTestConfig;

implementation

uses
  CloudAPI.Utils.Json;

resourcestring
  StrConfigtestjson = 'config.test.json';

var
  __TestConfig: TTestConfig;

function TestConfig: TTestConfig;
begin
  Result := __TestConfig;
end;

initialization
  __TestConfig := TJsonUtils.FileToObject<TTestConfig>(StrConfigtestjson);

finalization
  __TestConfig.Free;

end.

